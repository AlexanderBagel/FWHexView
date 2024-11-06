////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.MappedView.pas
//  * Purpose   : Implementation of advanced HexView editor with data map support
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 2.0.15
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/FWHexView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/FWHexView
//  ****************************************************************************
//

{
Licence:
  FWHexView is dual-licensed. You may choose to use it under the restrictions of the GPL v3 licence at no cost to you,
  or you may purchase a commercial licence. A commercial licence grants you the right to use FWHexView in your own
  applications, royalty free, and without any requirement to disclose your source code nor any modifications to FWHexView
  to any other party. A commercial licence lasts into perpetuity, and entitles you to all future updates, free of
  charge. A commercial licence is sold per developer developing applications that use FWHexView, as follows:
    1 developer = $49
    2 developers = $89
    3 developers = $139
    4 developers = $169
    5 developers = $199
    >5 developers = $199 + $25 per developer from the 6th onwards
    site licence = $499 (unlimited number of developers affiliated with the owner of the licence, i.e. employees, co-workers, interns and contractors)

  Please send an e-mail to rouse79@yandex.ru to request an invoice before or after payment is made. Payment may be
  made via bank transfer. Bank details will be provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence. Enhancement requests submitted by users with a
  commercial licence will be prioritized.
}

unit FWHexView.MappedView;

{$UNDEF EXTENDED_RTL}
{$IFDEF FPC}
  {$I FWHexViewConfig.inc}
{$ELSE}
  {$DEFINE EXTENDED_RTL}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType,
  {$ELSE}
  Windows,
  Messages,
  UITypes,
  Actions,
  {$ENDIF}
  SysUtils,
  Classes,
  Controls,
  Graphics,
  Math,
  Generics.Defaults,
  Generics.Collections,
  Themes,
  Types,
  ActnList,
  Menus,
  {$IFDEF USE_PROFILER}
  uni_profiler,
  {$ENDIF}
  FWHexView,
  FWHexView.Common;

type
  TRowStyle = (
    rsNone,
    rsSeparator,
    rsLine,
    rsUnbrokenLine, // unbroken line between lines, not passed to RawData
    rsRaw,
    rsRawWithExDescription,
    rsAsm,
    rsMask,
    rsMaskCheck,
    rsMaskRadio,
    rsMaskSeparator,
    rsLineComment,
    rsBlockComment);

  THintType = (htNone, htDescriptionLine, htHiperlink);

  TRowData = record
    RowIndex: Int64;
    Style: TRowStyle;
    Address: Int64;
    DataOffset: Int64;
    RawLength: Int64;
    Description: string;
    Comment: string;
    Color: TColor;
    DrawRowSmallSeparator: Boolean;
    HintType: THintType;
    Hint: string;
    case Integer of
      0: (
        // for Style = rsMask
        Expanded: Boolean;
        MapRowIndex: Int64); // Index in TMappedRawData to switch Expanded
      1: (
        // for Style = rsRawWithExDescription, rsAsm
        JmpToAddr: Int64;
        // link parameters in the Description column
        LinkStart: Integer;   // index of the character from which the hyperlink starts
        LinkLength: Integer;  // hyperlink length
        Linked: Boolean);     // the tag means there's an incoming link
      2: (
        // for Style = rsMaskCheck, rsMaskRadio
        MaskRowIndex: Int64;  // Index in TRowData for row with mask
        CharIndex: Byte;
        Checked: Boolean);
  end;

  TCustomMappedHexView = class;

  TMappedRawData = class(TRawData)
  strict private
    FRows: TListEx<TRowData>;
    FCount: Int64;
    procedure CheckDataMap;
    function GetRowAtIndex(ARowIndex: Int64): TMappedRawData;
    function GetItem: TRowData;
    function GetRawIndexByAddr(AAddr: Int64): Integer;
    function GetRawIndexByRowIndex(RowIndex: Int64): Integer;
    function InternalGetItem (ARowIndex: Int64): TRowData; inline;
  protected
    procedure SetLinked(Index: Int64);
    function Owner: TCustomMappedHexView;
  public
    constructor Create(AOwner: TFWCustomHexView); override;
    destructor Destroy; override;
    function AddressToRowIndex(Value: Int64): Int64; override;
    function Style: TRowStyle;
    function Address: Int64; override;
    function Color: TColor; override;
    function DataOffset: Int64; override;
    function DrawRowSmallSeparator: Boolean; override;
    function RawLength: Int64; override;
    function Description: string;
    function Comment: string; override;
    function HintType: THintType;
    function Hint: string;
    // for Style = rsMask
    function Expanded: Boolean;
    function MapRowIndex: Int64;
    // for Style = rsRawWithExDescription, rsAsm
    function JmpToAddr: Int64;
    // link parameters in the Description column
    function LinkStart: Integer;    // index of the character from which the hyperlink starts
    function LinkLength: Integer;   // hyperlink length
    function Linked: Boolean;       // the tag means there's an incoming link
    // для Style = rsMaskCheck, rsMaskRadio
    function MaskRowIndex: Int64;
    function CharIndex: Byte;
    function Checked: Boolean;
    function Count: Int64; override;
    procedure Clear; override;
    procedure Update; override;
    property Row[ARowIndex: Int64]: TMappedRawData read GetRowAtIndex; default;
  end;

  TMapRow = record
    Index: Integer;
    Style: TRowStyle;
    Address: Int64;
    RawLength: Int64;
    Description: string;
    Comment: string;
    Color: TColor;
    case Integer of
      0: (
        Expanded: Boolean);
      1: (
        JmpToAddr: Int64;
        LinkStart: Integer;
        LinkLength: Integer);
      2: (
        CharIndex: Byte;
        Checked: Boolean);
      3: (
        BlockCommentStart: Boolean);
  end;

  TAddrCheck = (
    acStartOutOfPool,
    acEndOutOfPool,
    acStartOutOfSpace,
    acEndOutOfSpace,
    acIntersect,
    acChecked);

  TDataMap = class
  strict private
    FOwner: TCustomMappedHexView;
    FData: TListEx<TMapRow>;
    FRawIndex: TDictionary<Int64, Int64>;
    FMaskPresent: Boolean;
    FUpdateCount: Integer;
    FCurrentAddr, FSavedCurrentAddr: Int64;
    function AddMapLine(Value: TMapRow): Integer;
    procedure DataChange(Sender: TObject;
      {$IFDEF EXTENDED_RTL}const{$ELSE}constref{$ENDIF} {%H-}Item: TMapRow;
      {%H-}Action: TCollectionNotification);
    procedure RebuildDataMap;
  protected
    procedure InternalClear(NewStartAddress: Int64);
    property CurrentAddr: Int64 read FCurrentAddr write FCurrentAddr;
  public
    constructor Create(AOwner: TCustomMappedHexView);
    destructor Destroy; override;

    procedure Clear;
    function CheckAddr(Address, DataLength: Int64): TAddrCheck;

    function AddMask(Address: Int64; DataLength: Byte;
      const Description, Comment: string; Expanded: Boolean;
      Color: TColor = clDefault): Integer; overload;
    function AddMask(DataLength: Byte; const Description, Comment: string;
      Expanded: Boolean; Color: TColor = clDefault): Integer; overload;
    function AddMask(DataLength: Byte; const Description: string;
      Expanded: Boolean; Color: TColor = clDefault): Integer; overload;

    function AddMaskCheck(ByteIndex: Byte;
      const Description, Comment: string; Checked: Boolean): Integer;
    function AddMaskRadio(ByteIndex: Byte;
      const Description, Comment: string; Checked: Boolean): Integer;
    function AddMaskSeparator: Integer;

    function AddRaw(DataLength: Int64): Integer; overload;
    function AddRaw(Address, DataLength: Int64): Integer; overload;

    function AddSeparator(const Description: string): Integer; overload;
    function AddSeparator(Address: Int64; const Description: string): Integer; overload;

    function AddExDescription(Address: Int64; DataLength: Byte;
      const Description, Comment: string; JmpToAddr: Int64;
      LinkStart, LinkLength: Integer; Color: TColor = clDefault): Integer; overload;
    function AddExDescription(DataLength: Byte;
      const Description, Comment: string; JmpToAddr: Int64;
      LinkStart, LinkLength: Integer; Color: TColor = clDefault): Integer; overload;
    function AddExDescription(DataLength: Byte;
      const Description, Comment: string; Color: TColor = clDefault): Integer; overload;
    function AddExDescription(DataLength: Byte;
      const Description: string; Color: TColor = clDefault): Integer; overload;

    function AddAsm(Address: Int64; DataLength: Byte;
      const Description, Comment: string; JmpToAddr: Int64;
      LinkStart, LinkLength: Integer; Color: TColor = clDefault): Integer; overload;
    function AddAsm(DataLength: Byte;
      const Description, Comment: string; JmpToAddr: Int64;
      LinkStart, LinkLength: Integer; Color: TColor = clDefault): Integer; overload;
    function AddAsm(DataLength: Byte;
      const Description, Comment: string; Color: TColor = clDefault): Integer; overload;
    function AddAsm(DataLength: Byte;
      const Description: string; Color: TColor = clDefault): Integer; overload;

    function AddComment(const Description: string): Integer; overload;
    function AddComment(Address: Int64; const Description: string): Integer; overload;

    function AddBlockComment(StartAddress, EndAddress: Int64;
      const Description: string): Integer; overload;
    function AddBlockComment(EndAddress: Int64;
      const Description: string): Integer; overload;

    function AddLine(Address: Int64; UnBroken: Boolean = False): Integer; overload;
    function AddLine(UnBroken: Boolean = False): Integer; overload;

    function AddNone(Address: Int64): Integer; overload;
    function AddNone: Integer; overload;

    procedure Assign({%H-}Value: TDataMap);
    procedure BeginUpdate;
    procedure EndUpdate;
    property Data: TListEx<TMapRow> read FData;

    function GetCurrentAddr: Int64;
    procedure SaveCurrentAddr;
    procedure RestoreCurrentAddr;
  end;

  TMapViewColors = class(THexViewColorMap)
  strict private
    FArrowDownSelectedColor: TColor;
    FArrowUpSelectedColor: TColor;
    FArrowDownColor: TColor;
    FArrowUpColor: TColor;
    FJmpMarkColor: TColor;
    FJmpMarkTextColor: TColor;
    FSeparatorBackgroundColor: TColor;
    FSeparatorBorderColor: TColor;
    FSeparatorTextColor: TColor;
    procedure SetArrowDownColor(const Value: TColor);
    procedure SetArrowDownSelectedColor(const Value: TColor);
    procedure SetArrowUpColor(const Value: TColor);
    procedure SetArrowUpSelectedColor(const Value: TColor);
    procedure SetJmpMarkColor(const Value: TColor);
    procedure SetJmpMarkTextColor(const Value: TColor);
    procedure SetSeparatorBackgroundColor(const Value: TColor);
    procedure SetSeparatorBorderColor(const Value: TColor);
    procedure SetSeparatorTextColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InitLightMode; override;
    procedure InitDarkMode; override;
  published
    property ArrowDownColor: TColor read FArrowDownColor write SetArrowDownColor stored IsColorStored;
    property ArrowDownSelectedColor: TColor read FArrowDownSelectedColor write SetArrowDownSelectedColor stored IsColorStored;
    property ArrowUpColor: TColor read FArrowUpColor write SetArrowUpColor stored IsColorStored;
    property ArrowUpSelectedColor: TColor read FArrowUpSelectedColor write SetArrowUpSelectedColor stored IsColorStored;
    property JmpMarkColor: TColor read FJmpMarkColor write SetJmpMarkColor stored IsColorStored;
    property JmpMarkTextColor: TColor read FJmpMarkTextColor write SetJmpMarkTextColor stored IsColorStored;
    property SeparatorBackgroundColor: TColor read FSeparatorBackgroundColor write SetSeparatorBackgroundColor stored IsColorStored;
    property SeparatorBorderColor: TColor read FSeparatorBorderColor write SetSeparatorBorderColor stored IsColorStored;
    property SeparatorTextColor: TColor read FSeparatorTextColor write SetSeparatorTextColor stored IsColorStored;
  end;

  TFixedHexByteTextMetric = class(TDefaultTextMetric)
  protected
    function ByteViewMode: TByteViewMode; override;
  end;

  TPrimaryMappedRowPainter = class(TRowHexPainter)
  protected
    function CalcColumnLengthForCopy(Column: TColumnType): Integer; override;
  end;

  TRowWithExDescription = class(TPrimaryMappedRowPainter)
  strict private
    FTextMetric: TAbstractTextMetric;
    function GetLineJmpMarkRect(const ARect: TRect): TRect;
  protected
    function ByteViewMode: TByteViewMode; override;
    function ColumnAsString(AColumn: TColumnType): string; override;
    procedure DrawColorGroup({%H-}ACanvas: TCanvas; var {%H-}ARect: TRect); override;
    procedure DrawDataPart(ACanvas: TCanvas; var ARect: TRect); override;
    function FormatRowColumn(AColumn: TColumnType;
      const Value: string): string; override;
    procedure GetHitInfo(var AMouseHitInfo: TMouseHitInfo;
      XPos, YPos: Int64); override;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
    function TextMetric: TAbstractTextMetric; override;
  public
    constructor Create(AOwner: TFWCustomHexView); override;
    function AcceptEdit(AColumn: TColumnType): Boolean; override;
    function CharCount(Column: TColumnType): Integer; override;
  end;

  TRowMask = class(TRowWithExDescription)
  protected
    procedure DrawHexPart(ACanvas: TCanvas; var ARect: TRect); override;
    procedure GetHitInfo(var AMouseHitInfo: TMouseHitInfo;
      XPos, YPos: Int64); override;
  end;

  TRowAssembler = class(TRowWithExDescription)
  public
    function AcceptEdit(AColumn: TColumnType): Boolean; override;
  end;

  TSecondaryMappedRowPainter = class(TSecondaryRowPainter)
  protected
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
  public
    constructor Create(AOwner: TFWCustomHexView); override;
  end;

  TRowSeparator = class(TSecondaryMappedRowPainter)
  protected
    procedure CopyRowAsString(Builder: TSimplyStringBuilder); override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
  end;

  TRowLineSeparator = class(TSecondaryMappedRowPainter)
  protected
    procedure CopyRowAsString(Builder: TSimplyStringBuilder); override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
  end;

  TRowComment = class(TSecondaryMappedRowPainter)
  protected
    procedure CopyRowAsString(Builder: TSimplyStringBuilder); override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
  end;

  TRowCheckRadioMask = class(TSecondaryRowPainter)
  strict private
    FTextMetric: TAbstractTextMetric;
    procedure DrawCheckPart(ACanvas: TCanvas; const ARect: TRect);
    procedure DrawRadioPart(ACanvas: TCanvas; const ARect: TRect);
    procedure DrawCommentPart(ACanvas: TCanvas; const ARect: TRect);
  protected
    function AcceptSelection: Boolean; override;
    function CalcColumnLengthForCopy(Column: TColumnType): Integer; override;
    function ColumnAsString(AColumn: TColumnType): string; override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
    function TextMetric: TAbstractTextMetric; override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
  public
    constructor Create(AOwner: TFWCustomHexView); override;
  end;

  TCommonMapViewPostPainter = class(TLinesPostPainter)
  protected
    function IsNeedOffsetRow(ARowIndex: Int64; FirstRow: Boolean): Boolean; override;
    function LineColorPresent(Selected: Boolean; const Param: TDrawLineParam;
      out LineColor: TColor): Boolean; override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
    function Owner: TCustomMappedHexView;
  public
    constructor Create(AOwner: TFWCustomHexView); override;
  end;

  TJumpLinesPostPainter = class(TCommonMapViewPostPainter)
  public
    procedure PostPaint(ACanvas: TCanvas; StartRow, EndRow: Int64;
      var Offset: TPoint); override;
  end;

  TCheckRadioRowPostPainter = class(TCommonMapViewPostPainter)
  protected
    procedure DrawArrowPart(ACanvas: TCanvas; RowIndex: Int64;
      var Offset: TPoint; DrawOnlySelectedArrow: Boolean);
    procedure DrawRow(ACanvas: TCanvas; RowIndex: Int64;
      var Offset: TPoint; DrawOnlySelectedArrow: Boolean;
      DrawRadio: Boolean);
  public
    procedure PostPaint(ACanvas: TCanvas; StartRow, EndRow: Int64;
      var Offset: TPoint); override;
  end;

  TVirtualPage = class
  strict private
    FOwner: TCustomMappedHexView;
    FCaption: string;
    FShowCaption: Boolean;
    FVirtualAddress: Int64;
    FSize: DWORD;
  private
    procedure DoChange;
    procedure SetCaption(const Value: string);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetSize(const Value: DWORD);
    procedure SetVirtualAddress(const Value: Int64);
  protected
    property Owner: TCustomMappedHexView read FOwner write FOwner;
  public
    property Caption: string read FCaption write SetCaption;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property VirtualAddress: Int64 read FVirtualAddress write SetVirtualAddress;
    property Size: DWORD read FSize write SetSize;
  end;

  TPageIndexResult = (pirOutOfBounds, pirNotPaged, pirPagePresent);

  TVirtualPages = class
  strict private
    FOwner: TCustomMappedHexView;
    FPages: TObjectList<TVirtualPage>;
  private
    procedure DoChange;
    function GetItem(Index: Integer): TVirtualPage;
  public
    constructor Create(AOwner: TCustomMappedHexView);
    destructor Destroy; override;
    function AddPage(const Caption: string; VirtualAddress: Int64;
      Size: DWORD): Integer;
    function Count: Integer;
    procedure Delete(Index: Integer);
    function GetPageIndex(VirtualAddress: Int64;
      out Index: Integer): TPageIndexResult;
    function CheckAddrInPages(VirtualAddress: Int64): Boolean;
    function MaxAddrAware: Int64;
    property Items[Index: Integer]: TVirtualPage read GetItem; default;
  end;

  TViewShortCut = class(TPersistent)
  private
    FDefault: TShortCut;
    FSecondaryShortCut: TShortCutList;
    FShortCut: TShortCut;
    function IsSecondaryStored: Boolean;
    function IsShortSutStored: Boolean;
    procedure SetSecondaryShortCut(const Value: TShortCutList);
    function GetSecondaryShortCut: TShortCutList;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function IsCustomViewShortCutStored: Boolean;
  public
    constructor Create(ADefault: TShortCut);
    destructor Destroy; override;
    function IsShortCut(Key: Word; Shift: TShiftState): Boolean;
  published
    property ShortCut: TShortCut read FShortCut write FShortCut stored IsShortSutStored;
    property SecondaryShortCut: TShortCutList read GetSecondaryShortCut write SetSecondaryShortCut stored IsSecondaryStored;
  end;

  TViewShortCuts = class(TPersistent)
  private
    FJmpBack: TViewShortCut;
    FJmpTo: TViewShortCut;
    procedure SetJmpBack(const Value: TViewShortCut);
    procedure SetJmpTo(const Value: TViewShortCut);
    function IsJmpBackStored: Boolean;
    function IsJmpToStored: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function IsShortCutsStored: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property JmpBack: TViewShortCut read FJmpBack write SetJmpBack stored IsJmpBackStored;
    property JmpTo: TViewShortCut read FJmpTo write SetJmpTo stored IsJmpToStored;
  end;

  TViewShortCutsClass = class of TViewShortCuts;

  TJmpData = record
    JmpFrom, JmpTo: Int64;
  end;

  TAddressToRowIndexMode = (armFindFirstRaw, armFindFirstAny);
  TJmpState = (jsPushToUndo, jsPopFromUndo, jsRestorePopFromUndo, jsJmpDone);
  TJmpToEvent = procedure(Sender: TObject; const AJmpAddr: Int64;
    AJmpState: TJmpState; var Handled: Boolean) of object;

  { TCustomMappedHexView }

  TCustomMappedHexView = class(TFWCustomHexView)
  strict private
    FAddressToRowIndexMode: TAddressToRowIndexMode;
    FCursorOnJmpMark: Boolean;
    FDataMap: TDataMap;
    FDrawIncomingJmp: Boolean;
    FJmpInitList: TList<Int64>;
    FJmpData: TObjectDictionary<Int64, TList<Int64>>;
    FPages: TVirtualPages;
    FPreviosJmp: TList<Int64>;
    FPreviosJmpIdx: Integer;
    FShortCuts: TViewShortCuts;
    FJmpToEvent: TJmpToEvent;
    function GetColorMap: TMapViewColors;
    procedure SetColorMap(const Value: TMapViewColors);
    procedure SetDrawIncomingJmp(const Value: Boolean);
    procedure SetShortCuts(const Value: TViewShortCuts);
    procedure UpdateJumpList;
  protected
    function CalculateJmpToRow(JmpFromRow: Int64): Int64; virtual;
    procedure DoCaretKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoInvalidateRange(AStartRow, AEndRow: Int64); override;
    procedure DoJmpTo(ARowIndex: Int64; AJmpState: TJmpState);
    function DoLButtonDown(Shift: TShiftState): Boolean; override;
    function GetColorMapClass: THexViewColorMapClass; override;
    function GetDefaultPainterClass: TPrimaryRowPainterClass; override;
    function GetRawDataClass: TRawDataClass; override;
    function GetShortCutsClass: TViewShortCutsClass; virtual;
    procedure InitPainters; override;
    function InternalGetRowPainter(ARowIndex: Int64): TAbstractPrimaryRowPainter; override;
    function IsShortCutsStored: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
    procedure UpdateCursor(const HitTest: TMouseHitInfo); override;
    procedure UpdateDataMap; override;
    {$IFNDEF FPC}
    procedure WMXButtonDown(var Msg: TWMMouse); message WM_XBUTTONDOWN;
    {$ENDIF}
  protected
    property CursorOnJmpMark: Boolean read FCursorOnJmpMark write FCursorOnJmpMark;
    property JmpData: TObjectDictionary<Int64, TList<Int64>> read FJmpData;
    property JmpInitList: TList<Int64> read FJmpInitList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearDataMap;
    procedure FitColumnToBestSize(Value: TColumnType); override;
    function RowStyle(ARowIndex: Int64): TRowStyle;
    property DataMap: TDataMap read FDataMap;
    property Pages: TVirtualPages read FPages;
  protected
    property AddressToRowIndexMode: TAddressToRowIndexMode read FAddressToRowIndexMode write FAddressToRowIndexMode default armFindFirstRaw;
    property ColorMap: TMapViewColors read GetColorMap write SetColorMap stored IsColorMapStored;
    property DrawIncomingJmp: Boolean read FDrawIncomingJmp write SetDrawIncomingJmp default False;
    property ShortCuts: TViewShortCuts read FShortCuts write SetShortCuts stored IsShortCutsStored;
    property OnJmpTo: TJmpToEvent read FJmpToEvent write FJmpToEvent;
  end;

  TMappedHexView = class(TCustomMappedHexView)
  published
    property AddressMode;
    property AddressToRowIndexMode;
    property AddressView;
    property AddressViewAutoToggle;
    property Align;
    property Anchors;
    property AutoSize;
    //property BevelEdges;
    //property BevelInner;
    //property BevelKind default bkNone;
    //property BevelOuter;
    //property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BytesInColorGroup;
    property BytesInGroup;
    property BytesInRow;
    property ByteViewMode;
    property Color;
    property ColorMap;
    property Constraints;
    //property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawIncomingJmp;
    property Enabled;
    property Encoder;
    property Font;
    property Header;
    property HideSelection;
    //property ImeMode;
    //property ImeName;
    property NoDataText;
    property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property SelectOnMouseDown;
    property SeparateGroupByColor;
    property ShortCuts;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property WheelMultiplyer;
    property OnCaretPosChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawColumnBackground;
    property OnDrawToken;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnJmpTo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    //property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnQueryComment;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

{$IFDEF FPC}

  { StyleServices }

  StyleServices = class(TThemeServices)
    class function Enabled: Boolean;
    class function {%H-}DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil; {%H-}DPI: Integer = 0): Boolean;
    class function GetElementDetails(Detail: TThemedButton): TThemedElementDetails; overload;
  end;

{$ENDIF}

implementation

const
  SInvalidOwnerClass =
    'To use %s, the %s must be a descendant of TCustomMappedHexView';

{$IFDEF FPC}

  { StyleServices }

class function StyleServices.Enabled: Boolean;
begin
  Result := ThemeServices.ThemesAvailable;
end;

class function StyleServices.{%H-}DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect; DPI: Integer): Boolean;
begin
  ThemeServices.DrawElement(DC, Details, R, ClipRect);
end;

class function StyleServices.GetElementDetails(Detail: TThemedButton
  ): TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

{$ENDIF}

{ TMappedRawData }

function TMappedRawData.Address: Int64;
begin
  Result := GetItem.Address;
end;

function TMappedRawData.AddressToRowIndex(Value: Int64): Int64;
var
  RawRowIndex: Integer;
  LinesBetween: Int64;
  Offset: Int64;
begin
  if Count = 0 then Exit(-1);
  RawRowIndex := GetRawIndexByAddr(Value);

  if FRows.List[RawRowIndex].Address = Value then
  begin

    // если попали на второстепенную строку отображающую маску
    // значения, то ищем само значение, находящееся выше

    // If we get to the second line displaying the value mask,
    // we look for the value above it.

    while (FRows.List[RawRowIndex].Style in [rsMaskCheck..rsMaskSeparator]) do
    begin
      Dec(RawRowIndex);
      if (RawRowIndex < 0) or (FRows.List[RawRowIndex].Address < Value) then
      begin
        Result := -1;
        Exit;
      end;
    end;

    // Если требуется найти самое первое вхождение адреса из нескольких подряд
    // идущих, то крутим цикл вверх

    // If you need to find the first occurrence of an address out of several
    // consecutive ones, run the loop upward

    if Owner.AddressToRowIndexMode = armFindFirstAny then
    begin
      while (RawRowIndex > 0) and (FRows.List[RawRowIndex - 1].Address = Value) do
        Dec(RawRowIndex);
    end
    else

      // в противном случае пропускаем все второстепенные строки,
      // наподобие разделителей и коментариев

      // Otherwise, skip all minor lines like delimiters and comments.

      while not (FRows.List[RawRowIndex].Style in [rsRaw..rsMask]) do
      begin
        Inc(RawRowIndex);
        if (FRows.Count <= RawRowIndex) or (FRows.List[RawRowIndex].Address > Value) then
        begin
          Result := -1;
          Exit;
        end;
      end;

    Result := FRows.List[RawRowIndex].RowIndex
  end
  else
  begin

    // здесь окажемся в случае попадания в блок не размапленных данных,
    // в этом случае надо просто рассчитать строку по формуле

    // here we will find ourselves in the case of non-unmapped data getting
    // into the block, in this case we just need to calculate the row by the formula

    if Value < FRows.List[RawRowIndex].Address then
    begin
      Result := -1;
      Exit;
    end;
    Offset := Value - FRows.List[RawRowIndex].Address;
    LinesBetween := Offset div Owner.BytesInRow;
    Result := FRows.List[RawRowIndex].RowIndex + LinesBetween;
  end;
end;

function TMappedRawData.CharIndex: Byte;
begin
  Result := GetItem.CharIndex;
end;

procedure TMappedRawData.CheckDataMap;
var
  I: Integer;
  LastAddr, NextAddr: Int64;
  LastRowIndex: Integer;
  Data: TListEx<TMapRow>;
  ARow: TMapRow;
begin
  if Owner.DataMap.Data.Count = 0 then Exit;
  LastAddr := 0;
  NextAddr := 0;
  LastRowIndex := 0;
  Data := Owner.DataMap.Data;
  Data.Sort;
  for I := 0 to Data.Count - 1 do
  begin
    ARow := Data[I];
    if (NextAddr <> 0) and (ARow.Address < NextAddr) then
      raise Exception.CreateFmt(
        'Data map address %x (%d) intersects with the block at %x (%d)',
        [ARow.Address, ARow.Index, LastAddr, LastRowIndex]);
    LastAddr := ARow.Address;
    LastRowIndex := ARow.Index;
    NextAddr := LastAddr + ARow.RawLength;
  end;
end;

function TMappedRawData.Checked: Boolean;
begin
  Result := GetItem.Checked;
end;

procedure TMappedRawData.Clear;
begin
  FRows.Clear;
  FCount := 0;
end;

function TMappedRawData.Color: TColor;
begin
  Result := GetItem.Color;
end;

function TMappedRawData.Comment: string;
begin
  Result := GetItem.Comment;
end;

function TMappedRawData.Count: Int64;
begin
  Result := FCount;
end;

constructor TMappedRawData.Create(AOwner: TFWCustomHexView);
begin
  inherited Create(AOwner);
  FRows := TListEx<TRowData>.Create;
end;

function TMappedRawData.DataOffset: Int64;
begin
  Result := GetItem.DataOffset;
end;

function TMappedRawData.Description: string;
begin
  Result := GetItem.Description;
end;

destructor TMappedRawData.Destroy;
begin
  FRows.Free;
  inherited;
end;

function TMappedRawData.DrawRowSmallSeparator: Boolean;
begin
  Result := GetItem.DrawRowSmallSeparator;
end;

function TMappedRawData.Expanded: Boolean;
begin
  Result := GetItem.Expanded;
end;

function TMappedRawData.GetItem: TRowData;
begin
  Result := InternalGetItem(RowIndex);
  if Result.RawLength < 0 then
    raise Exception.CreateFmt('Unexpected RawLength (%d)', [Result.RawLength]);
end;

function TMappedRawData.GetRawIndexByAddr(AAddr: Int64): Integer;
var
  FLeft, FRight, FCurrent: Integer;
begin
  if FCount = 0 then
  begin
    Result := 0;
    Exit;
  end;
  FLeft := 0;
  FRight := FRows.Count - 1;
  FCurrent := (FRight + FLeft) div 2;
  if FRows.List[FLeft].Address > AAddr then
  begin
    Result := 0;
    Exit;
  end;
  if FRows.List[FRight].Address < AAddr then
  begin
    Result := FRight;
    Exit;
  end;
  repeat
    if FRows.List[FCurrent].Address = AAddr then
    begin
      Result := FCurrent;
      Exit;
    end;
    if FRows.List[FCurrent].Address < AAddr then
      FLeft := FCurrent
    else
      FRight := FCurrent;
    FCurrent := (FRight + FLeft) div 2;
  until FLeft = FCurrent;
  if FRows.List[FCurrent].Address < AAddr then
    if FRows.List[FCurrent + 1].Address = AAddr then
      Inc(FCurrent);
  Result := FCurrent;
end;

function TMappedRawData.GetRawIndexByRowIndex(RowIndex: Int64): Integer;
var
  FLeft, FRight, FCurrent: Integer;
begin
  if FCount = 0 then
  begin
    Result := 0;
    Exit;
  end;
  FLeft := 0;
  FRight := FRows.Count - 1;
  FCurrent := (FRight + FLeft) div 2;
  if FRows.List[FLeft].RowIndex > RowIndex then
  begin
    Result := 0;
    Exit;
  end;
  if FRows.List[FRight].RowIndex < RowIndex then
  begin
    Result := FRight;
    Exit;
  end;
  repeat
    if FRows.List[FCurrent].RowIndex = RowIndex then
    begin
      Result := FCurrent;
      Exit;
    end;
    if FRows.List[FCurrent].RowIndex < RowIndex then
      FLeft := FCurrent
    else
      FRight := FCurrent;
    FCurrent := (FRight + FLeft) div 2;
  until FLeft = FCurrent;
  if FRows.List[FCurrent].RowIndex < RowIndex then
    if FRows.List[FCurrent + 1].RowIndex = RowIndex then
      Inc(FCurrent);
  Result := FCurrent;
end;

function TMappedRawData.GetRowAtIndex(ARowIndex: Int64): TMappedRawData;
begin
  Result := TMappedRawData(inherited Data[ARowIndex]);
end;

function TMappedRawData.Hint: string;
begin
  Result := GetItem.Hint;
end;

function TMappedRawData.HintType: THintType;
begin
  Result := GetItem.HintType;
end;

function TMappedRawData.InternalGetItem(ARowIndex: Int64): TRowData;
var
  RawRowIndex: Integer;
  Offset, LinesBetween: Int64;
begin
  RawRowIndex := GetRawIndexByRowIndex(ARowIndex);
  if RawRowIndex >= FRows.Count then Exit(Default(TRowData));
  if FRows.List[RawRowIndex].RowIndex = ARowIndex then
  begin
    Result := FRows[RawRowIndex];
    Result.RawLength := Min(Owner.BytesInRow, Result.RawLength);
  end
  else
  begin
    Result := FRows[RawRowIndex];
    LinesBetween := RowIndex - Result.RowIndex;
    Offset := LinesBetween * Owner.BytesInRow;
    Inc(Result.RowIndex, LinesBetween);
    Inc(Result.Address, Offset);
    Inc(Result.DataOffset, Offset);
    Dec(Result.RawLength, Offset);
    Result.RawLength := Min(Owner.BytesInRow, Result.RawLength);
  end;
end;

function TMappedRawData.JmpToAddr: Int64;
begin
  Result := GetItem.JmpToAddr;
end;

function TMappedRawData.Linked: Boolean;
begin
  Result := GetItem.Linked;
end;

function TMappedRawData.LinkLength: Integer;
begin
  Result := GetItem.LinkLength;
end;

function TMappedRawData.LinkStart: Integer;
begin
  Result := GetItem.LinkStart;
end;

function TMappedRawData.MapRowIndex: Int64;
begin
  Result := GetItem.MapRowIndex;
end;

function TMappedRawData.MaskRowIndex: Int64;
begin
  Result := GetItem.MaskRowIndex;
end;

function TMappedRawData.Owner: TCustomMappedHexView;
begin
  Result := TCustomMappedHexView(inherited Owner);
end;

function TMappedRawData.RawLength: Int64;
begin
  Result := GetItem.RawLength;
end;

procedure TMappedRawData.SetLinked(Index: Int64);
var
  RawRowIndex: Integer;
begin
  RawRowIndex := GetRawIndexByRowIndex(Index);
  if RawRowIndex >= 0 then
    FRows.List[RawRowIndex].Linked := True;
end;

function TMappedRawData.Style: TRowStyle;
begin
  Result := GetItem.Style;
end;

procedure TMappedRawData.Update;
var
  MapIndex: Integer;
  StreamOffset, DataStreamSize: Int64;
  Line: TRowData;
  RawLength: Integer;
  PageIndex: Integer;
  PageAddress: Int64;
  PageSize, PageOffset: Int64;
  BytesInRow: Integer;
  Data: TListEx<TMapRow>;
  LastPageIsRaw: Boolean;
  LastMaskIndex: Int64;
  AMapRow: TMapRow;

  procedure AddLine;
  begin
    if LastPageIsRaw and (Line.Style = rsRaw) then
      Inc(FRows.List[FRows.Count - 1].RawLength, Line.RawLength)
    else
      FRows.Add(Line);
    LastPageIsRaw := (Line.Style = rsRaw) and (Line.RawLength = BytesInRow);
    Inc(FCount);
    if (Line.Style = rsRaw) and (Line.RawLength > BytesInRow) then
    begin
      Inc(FCount, Line.RawLength div BytesInRow);
      if Line.RawLength mod BytesInRow = 0 then
        Dec(FCount);
    end;
    Line.RowIndex := FCount;
  end;

  procedure AddPageDelimiter;
  begin

    // разделитель между страницами

    // page separator

    if Owner.Pages[PageIndex].ShowCaption then
    begin
      Line.Style := rsNone;
      Line.Address := PageAddress;
      Line.DataOffset := StreamOffset;
      Line.RawLength := 0;
      Line.Description := '';
      Line.Comment := '';
      Line.Color := Owner.ColorMap.TextColor;
      Line.DrawRowSmallSeparator := False;
      Line.HintType := THintType.htNone;
      Line.Hint := '';
      Line.JmpToAddr := 0;
      Line.LinkStart := 0;
      Line.LinkLength := 0;
      Line.Linked := False;
      if FCount > 0 then
        AddLine;

      Line.Style := rsSeparator;
      Line.Description := Owner.Pages[PageIndex].Caption;
      AddLine;

      Line.Style := rsNone;
      Line.Description := '';
      AddLine;
    end;
  end;

  procedure UpdateLocalMapRow;
  begin
    if MapIndex < Data.Count then
      AMapRow := Data[MapIndex]
    else
      AMapRow := Default(TMapRow);
  end;

begin
  FCount := 0;
  StreamOffset := 0;
  FillChar(Line, SizeOf(Line), 0);
  FRows.Clear;
  Owner.JmpInitList.Clear;

  CheckDataMap;

  PageIndex := -1;
  PageAddress := Owner.StartAddress;
  if Owner.Pages.Count = 0 then
    PageSize := Owner.GetDataStreamSize
  else
  begin
    if Owner.Pages[0].VirtualAddress > Owner.StartAddress then
      PageSize := Owner.Pages[0].VirtualAddress - Owner.StartAddress
    else
    begin
      PageIndex := 0;
      PageSize := Owner.Pages[0].Size;
      AddPageDelimiter;
    end;
  end;

  PageOffset := 0;
  DataStreamSize := Owner.GetDataStreamSize;
  MapIndex := 0;
  BytesInRow := Owner.BytesInRow;
  Data := Owner.DataMap.Data;
  LastPageIsRaw := False;
  LastMaskIndex := -1;
  AMapRow := Default(TMapRow);
  while PageIndex < Owner.Pages.Count do
  begin

    while (StreamOffset < DataStreamSize) and (PageOffset < PageSize) do
    begin

      Line.Address := PageAddress + PageOffset;
      Line.DataOffset := StreamOffset;
      if MapIndex < Data.Count then
      begin
        UpdateLocalMapRow;
        Line.RawLength := AMapRow.Address - Line.Address;
        Line.RawLength := Min(PageSize - PageOffset, Line.RawLength);
      end
      else
        Line.RawLength := PageSize - PageOffset;

      if Line.RawLength > 0 then
      begin
        Line.Style := rsRaw;
        Line.Comment := '';
        Line.Color := Owner.ColorMap.TextColor;
        Line.Description := '';
        Line.DrawRowSmallSeparator := False;
        Line.HintType := THintType.htNone;
        Line.Hint := EmptyStr;
        Line.JmpToAddr := 0;
        Line.LinkStart := 0;
        Line.LinkLength := 0;
        Line.Linked := False;
      end;

      if MapIndex < Data.Count then
      begin
        if (AMapRow.Address = Line.Address) or
          (AMapRow.Style in [rsMaskCheck..rsMaskSeparator]) then
        begin

          // rsUnbrokenLine - виртуальный флаг не представленый ввиде
          // отдельного пайнтера. Обрабатывается при отрисовке самим HexView,
          // отрисовывая штрих между двумя линиями

          // rsUnbrokenLine - virtual flag not represented as a separate pinter.
          // It is processed during drawing by HexView itself,
          // drawing a stroke between two lines

          if AMapRow.Style = rsUnbrokenLine then
          begin
            Inc(MapIndex);
            UpdateLocalMapRow;
            if FRows.Count = 0 then
              Continue;
            FRows.List[FRows.Count - 1].DrawRowSmallSeparator := True;
            Continue;
          end;

          RawLength := Min(AMapRow.RawLength,
            PageSize - PageOffset);
          Line.Description := AMapRow.Description;
          Line.Comment := AMapRow.Comment;
          Line.Style := AMapRow.Style;
          Line.Color := AMapRow.Color;

          case Line.Style of
            rsSeparator, rsRaw, rsLineComment, rsBlockComment:
              LastMaskIndex := -1;
            rsRawWithExDescription, rsAsm:
            begin
              LastMaskIndex := -1;
              Line.JmpToAddr := AMapRow.JmpToAddr;
              Line.LinkStart := AMapRow.LinkStart;
              Line.LinkLength := AMapRow.LinkLength;
              if Line.JmpToAddr > 0 then
                Owner.JmpInitList.Add(FCount);
            end;
            rsMask:
            begin
              Line.Expanded := AMapRow.Expanded;
              Line.MapRowIndex := MapIndex;
              LastMaskIndex := FRows.Count;
            end;
            rsMaskCheck..rsMaskSeparator:
            begin
              if (LastMaskIndex < 0) or not FRows.List[LastMaskIndex].Expanded then
              begin
                Inc(MapIndex);
                UpdateLocalMapRow;
                Continue;
              end;
              Line.MaskRowIndex := FRows.List[LastMaskIndex].RowIndex;
              Line.Address := FRows.List[LastMaskIndex].Address;
              Line.CharIndex := AMapRow.CharIndex;
              Line.Checked := AMapRow.Checked;
            end;
          end;

          Inc(MapIndex);
          while RawLength > BytesInRow do
          begin
            Line.RawLength := BytesInRow;
            AddLine;
            Line.Description := '';
            Line.Comment := '';
            Line.JmpToAddr := 0;
            Line.LinkStart := 0;
            Line.LinkLength := 0;
            Dec(RawLength, BytesInRow);
            Inc(StreamOffset, Line.RawLength);
            Inc(PageOffset, Line.RawLength);
            Line.Address := PageAddress + PageOffset;
            Line.DataOffset := StreamOffset;
          end;
          Line.RawLength := RawLength;
        end
        else
          if Line.Address + Int64(Line.RawLength) > AMapRow.Address then
            Line.RawLength := AMapRow.Address - Line.Address;
      end;

      AddLine;
      Inc(StreamOffset, Line.RawLength);
      Inc(PageOffset, Line.RawLength);
    end;
    Inc(PageIndex);
    if PageIndex < Owner.Pages.Count then
    begin
      PageAddress := Owner.Pages[PageIndex].VirtualAddress;
      AddPageDelimiter;

      // вместе с последней страницей выводим все что в неё не вошло

      // along with the last page, we output everything not included in it.

      if PageIndex = Owner.Pages.Count - 1 then
      begin
        if DataStreamSize > StreamOffset then
          PageSize := DataStreamSize - StreamOffset
        else
          raise Exception.CreateFmt(
            'Unexpected StreamOffset (%d) with total size %d',
            [StreamOffset, DataStreamSize]);
      end
      else
        PageSize := Owner.Pages[PageIndex].Size;
      PageOffset := 0;
    end;
  end;

  // контроль что последняя строчка добавлена верно
  // и выдает неверные данные по своему размеру

  // control that the last line is added correctly
  // and gives incorrect data for its size

  {$ifdef dbg_check_last_row}
  if FCount > 0 then
  begin
    RawLength := InternalGetItem(FCount).RawLength;
    Assert(RawLength <= 0);
    Assert(RawLength > -BytesInRow);
  end;
  {$endif}
end;

{ TDataMap }

function TDataMap.AddAsm(DataLength: Byte; const Description: string;
  Color: TColor): Integer;
begin
  Result := AddAsm(CurrentAddr, DataLength, Description, '', 0, 0, 0, Color);
end;

function TDataMap.AddAsm(DataLength: Byte; const Description, Comment: string;
  Color: TColor): Integer;
begin
  Result := AddAsm(CurrentAddr, DataLength, Description, Comment, 0, 0, 0, Color);
end;

function TDataMap.AddAsm(DataLength: Byte; const Description, Comment: string;
  JmpToAddr: Int64; LinkStart, LinkLength: Integer;
  Color: TColor): Integer;
begin
  Result := AddAsm(CurrentAddr, DataLength, Description, Comment,
    JmpToAddr, LinkStart, LinkLength, Color);
end;

function TDataMap.AddAsm(Address: Int64; DataLength: Byte;
  const Description, Comment: string; JmpToAddr: Int64; LinkStart,
  LinkLength: Integer; Color: TColor): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsAsm;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.RawLength := DataLength;
  LineData.Color := Color;
  LineData.JmpToAddr := JmpToAddr;
  LineData.LinkStart := LinkStart;
  LineData.LinkLength := LinkLength;
  Result := AddMapLine(LineData);
  CurrentAddr := Address + DataLength;
end;

function TDataMap.AddBlockComment(EndAddress: Int64;
  const Description: string): Integer;
begin
  Result := AddBlockComment(CurrentAddr, EndAddress, Description);
end;

function TDataMap.AddBlockComment(StartAddress, EndAddress: Int64;
  const Description: string): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsBlockComment;
  LineData.Address := StartAddress;
  LineData.Description := Description + ' start';
  LineData.BlockCommentStart := True;
  Result := AddMapLine(LineData);

  // если концовка попадает на границу региона, ну просто не добавляем её

  // If the ending falls on a region boundary, well, just don't add it.

  if FOwner.Pages.CheckAddrInPages(EndAddress) then
  begin
    LineData.Index := FData.Count;
    LineData.Address := EndAddress;
    LineData.Description := Description + ' end';
    LineData.BlockCommentStart := False;
    AddMapLine(LineData);
  end;
  CurrentAddr := StartAddress;
end;

function TDataMap.AddComment(const Description: string): Integer;
begin
  Result := AddComment(CurrentAddr, Description);
end;

function TDataMap.AddComment(Address: Int64;
  const Description: string): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsLineComment;
  LineData.Address := Address;
  LineData.Description := Description;
  CurrentAddr := Address;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddExDescription(DataLength: Byte; const Description,
  Comment: string; Color: TColor): Integer;
begin
  Result := AddExDescription(
    CurrentAddr, DataLength, Description, Comment, 0, 0, 0, Color);
end;

function TDataMap.AddExDescription(DataLength: Byte; const Description: string;
  Color: TColor): Integer;
begin
  Result := AddExDescription(CurrentAddr,
    DataLength, Description, '', 0, 0, 0, Color);
end;

function TDataMap.AddExDescription(DataLength: Byte; const Description,
  Comment: string; JmpToAddr: Int64; LinkStart, LinkLength: Integer;
  Color: TColor): Integer;
begin
  Result := AddExDescription(CurrentAddr,
    DataLength, Description, Comment, JmpToAddr, LinkStart, LinkLength, Color);
end;

function TDataMap.AddExDescription(Address: Int64; DataLength: Byte;
  const Description, Comment: string; JmpToAddr: Int64; LinkStart,
  LinkLength: Integer; Color: TColor): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsRawWithExDescription;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.RawLength := DataLength;
  LineData.Color := Color;
  LineData.JmpToAddr := JmpToAddr;
  LineData.LinkStart := LinkStart;
  LineData.LinkLength := LinkLength;
  Result := AddMapLine(LineData);
  CurrentAddr := Address + DataLength;
end;

function TDataMap.AddLine(UnBroken: Boolean): Integer;
begin
  Result := AddLine(CurrentAddr, UnBroken);
end;

function TDataMap.AddLine(Address: Int64; UnBroken: Boolean): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  if UnBroken then
    LineData.Style := rsUnbrokenLine
  else
    LineData.Style := rsLine;
  LineData.Address := Address;
  Result := AddMapLine(LineData);
  CurrentAddr := Address;
end;

function TDataMap.AddMapLine(Value: TMapRow): Integer;
begin
  case CheckAddr(Value.Address, Value.RawLength) of
    acStartOutOfPool:
      raise Exception.CreateFmt('Line start address %x, out of pages pool',
        [Value.Address]);
    acEndOutOfPool:
      raise Exception.CreateFmt('Line end address %x, out of pages pool',
        [Value.Address + Value.RawLength]);
    acStartOutOfSpace:
      raise Exception.CreateFmt('Line start address %x, out of space %x',
        [Value.Address, FOwner.StartAddress]);
    acEndOutOfSpace:
      raise Exception.CreateFmt('Line end address %x, out of space %x',
        [Value.Address + Value.RawLength, FOwner.Pages.MaxAddrAware]);
    acIntersect:
      raise Exception.CreateFmt('DataMap address %x already contains data',
        [Value.Address]);
  end;

  case Value.Style of
    rsMask:
      FMaskPresent := True;
    rsMaskCheck,
    rsMaskRadio,
    rsMaskSeparator:
      if not FMaskPresent then
        raise Exception.Create('Cannot add a mask element when the mask is missing.');
  else
    FMaskPresent := False;
  end;

  if Value.RawLength > 0 then
    FRawIndex.AddOrSetValue(Value.Address, Value.RawLength);
  Result := FData.Add(Value);
end;

function TDataMap.AddMask(Address: Int64; DataLength: Byte; const Description,
  Comment: string; Expanded: Boolean; Color: TColor): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsMask;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.RawLength := DataLength;
  LineData.Color := Color;
  LineData.Expanded := Expanded;
  Result := AddMapLine(LineData);
  CurrentAddr := Address + DataLength;
end;

function TDataMap.AddMask(DataLength: Byte; const Description, Comment: string;
  Expanded: Boolean; Color: TColor): Integer;
begin
  Result := AddMask(CurrentAddr, DataLength, Description, Comment, Expanded, Color);
end;

function TDataMap.AddMask(DataLength: Byte; const Description: string;
  Expanded: Boolean; Color: TColor): Integer;
begin
  Result := AddMask(CurrentAddr, DataLength, Description, '', Expanded, Color);
end;

function TDataMap.AddMaskCheck(ByteIndex: Byte; const Description,
  Comment: string; Checked: Boolean): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsMaskCheck;
  LineData.Address := CurrentAddr;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.CharIndex := ByteIndex;
  LineData.Checked := Checked;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddMaskRadio(ByteIndex: Byte; const Description,
  Comment: string; Checked: Boolean): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsMaskRadio;
  LineData.Address := CurrentAddr;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.CharIndex := ByteIndex;
  LineData.Checked := Checked;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddMaskSeparator: Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsMaskSeparator;
  LineData.Address := CurrentAddr;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddNone(Address: Int64): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsNone;
  LineData.Address := Address;
  Result := AddMapLine(LineData);
  CurrentAddr := Address;
end;

function TDataMap.AddNone: Integer;
begin
  Result := AddNone(CurrentAddr);
end;

function TDataMap.AddRaw(DataLength: Int64): Integer;
begin
  Result := AddRaw(CurrentAddr, DataLength);
end;

function TDataMap.AddRaw(Address, DataLength: Int64): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsRaw;
  LineData.Address := Address;
  LineData.RawLength := DataLength;
  CurrentAddr := Address + DataLength;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddSeparator(const Description: string): Integer;
begin
  Result := AddSeparator(CurrentAddr, Description);
end;

function TDataMap.AddSeparator(Address: Int64;
  const Description: string): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsSeparator;
  LineData.Address := Address;
  LineData.Description := Description;
  CurrentAddr := Address;
  Result := AddMapLine(LineData);
end;

procedure TDataMap.Assign(Value: TDataMap);
begin
  FData.Clear;
  FData.AddRange(FData.ToArray);
  RebuildDataMap;
end;

procedure TDataMap.BeginUpdate;
begin
  if FUpdateCount = 0 then
    FCurrentAddr := FOwner.StartAddress;
  Inc(FUpdateCount);
end;

function TDataMap.CheckAddr(Address, DataLength: Int64): TAddrCheck;
begin
  if FOwner.Pages.Count > 0 then
  begin
    if not FOwner.Pages.CheckAddrInPages(Address) then
      Exit(acStartOutOfPool);
    if not FOwner.Pages.CheckAddrInPages(Address + DataLength - 1) then
      Exit(acEndOutOfPool);
  end
  else
  begin
    if Address < FOwner.StartAddress then
      Exit(acStartOutOfSpace);
    if Address + DataLength - 1 > FOwner.Pages.MaxAddrAware then
      Exit(acEndOutOfSpace);
  end;

  if DataLength > 0 then
    if FRawIndex.TryGetValue(Address, DataLength) then
      Exit(acIntersect);

  Result := acChecked;
end;

procedure TDataMap.Clear;
begin
  BeginUpdate;
  try
    InternalClear(0);
  finally
    EndUpdate;
  end;
end;

function DefaultMapRowComparer({$IFDEF EXTENDED_RTL}const{$ELSE}constref{$ENDIF} A, B: TMapRow): Integer;
var
  LongResult: Int64;
begin

  // порядок строго по возрастанию адресов

  // The order is strictly in ascending order of addresses

  LongResult := A.Address - B.Address;

  // все строки не содержащие данных ОБЯЗАТЕЛЬНО
  // идут перед блоками с данными!!!

  // all non-data lines MUST come before the data blocks!!!

  if LongResult = 0 then
    LongResult := A.RawLength - B.RawLength;

  // отдельная обработка парных блоков коментариев

  // separate processing of paired comment blocks

  if LongResult = 0 then
  begin
    if A.Style = rsBlockComment then
    begin
      if A.Index = B.Index then
        Exit(0);
      if B.Style = rsBlockComment then
        LongResult := Integer(A.BlockCommentStart) -
          Integer(B.BlockCommentStart)
      else
        if A.BlockCommentStart then
          LongResult := 1
        else
          LongResult := -1;
    end
    else
      if B.Style = rsBlockComment then
      begin
        if A.Index = B.Index then
          Exit(0);
        if A.Style = rsBlockComment then
          LongResult := Integer(A.BlockCommentStart) -
            Integer(B.BlockCommentStart)
        else
          if B.BlockCommentStart then
            LongResult := -1
          else
            LongResult := 1;
      end
  end;

  if LongResult = 0 then
    LongResult := A.Index - B.Index;

  if LongResult < 0 then
    Result := -1
  else
    if LongResult = 0 then
      Result := 0
    else
      Result := 1;
end;

constructor TDataMap.Create(AOwner: TCustomMappedHexView);
begin
  FOwner := AOwner;
  FData := TListEx<TMapRow>.Create(TComparer<TMapRow>.Construct(DefaultMapRowComparer));
  FData.OnNotify := DataChange;
  FRawIndex := TDictionary<Int64, Int64>.Create;
end;

procedure TDataMap.DataChange(Sender: TObject;
  {$IFDEF EXTENDED_RTL}const{$ELSE}constref{$ENDIF} Item: TMapRow;
  Action: TCollectionNotification);
begin
  RebuildDataMap;
end;

destructor TDataMap.Destroy;
begin
  FRawIndex.Free;
  FData.OnNotify := nil;
  FData.Free;
  inherited;
end;

procedure TDataMap.EndUpdate;
begin
  Dec(FUpdateCount);
  RebuildDataMap;
end;

function TDataMap.GetCurrentAddr: Int64;
begin
  Result := CurrentAddr;
end;

procedure TDataMap.InternalClear(NewStartAddress: Int64);
begin
  FData.Clear;
  FRawIndex.Clear;
  FCurrentAddr := NewStartAddress;
end;

procedure TDataMap.RebuildDataMap;
begin
  if FUpdateCount = 0 then
    FOwner.RebuildData;
end;

procedure TDataMap.RestoreCurrentAddr;
begin
  if FSavedCurrentAddr <> 0 then
  begin
    FCurrentAddr := FSavedCurrentAddr;
    FSavedCurrentAddr := 0;
  end;
end;

procedure TDataMap.SaveCurrentAddr;
begin
  FSavedCurrentAddr := CurrentAddr;
end;

{ TMapViewColors }

procedure TMapViewColors.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TMapViewColors then
  begin
    TMapViewColors(Dest).FArrowDownColor := FArrowDownColor;
    TMapViewColors(Dest).FArrowDownSelectedColor := FArrowDownSelectedColor;
    TMapViewColors(Dest).FArrowUpColor := FArrowUpColor;
    TMapViewColors(Dest).FArrowUpSelectedColor := FArrowUpSelectedColor;
    TMapViewColors(Dest).FJmpMarkColor := FJmpMarkColor;
    TMapViewColors(Dest).FJmpMarkTextColor := FJmpMarkTextColor;
    TMapViewColors(Dest).FSeparatorBackgroundColor := FSeparatorBackgroundColor;
    TMapViewColors(Dest).FSeparatorBorderColor := FSeparatorBorderColor;
    TMapViewColors(Dest).FSeparatorTextColor := FSeparatorTextColor;
  end;
end;

procedure TMapViewColors.InitDarkMode;
begin
  inherited;
  FArrowDownColor := $B1B1B1;
  FArrowDownSelectedColor := $B87149;
  FArrowUpColor := $D9D4FF;
  FArrowUpSelectedColor := $8B6EFF;
  FJmpMarkColor := clDefault;
  FJmpMarkTextColor := $B87149;
  FSeparatorBackgroundColor := $5D5D5D;
  FSeparatorBorderColor := clGrayText;
  FSeparatorTextColor := TextColor;
end;

procedure TMapViewColors.InitLightMode;
begin
  inherited;
  FArrowDownColor := $FFBD9C;
  FArrowDownSelectedColor := $FF8138;
  FArrowUpColor := $C4CCFF;
  FArrowUpSelectedColor := $424CFF;
  FJmpMarkColor := clDefault;
  FJmpMarkTextColor := RGB(73, 113, 184);
  FSeparatorBackgroundColor := clBtnFace;
  FSeparatorBorderColor := clGrayText;
  FSeparatorTextColor := clWindowText;
end;

procedure TMapViewColors.SetArrowDownColor(const Value: TColor);
begin
  if FArrowDownColor <> Value then
  begin
    FArrowDownColor := Value;
    DoChange;
  end;
end;

procedure TMapViewColors.SetArrowDownSelectedColor(const Value: TColor);
begin
  if FArrowDownSelectedColor <> Value then
  begin
    FArrowDownSelectedColor := Value;
    DoChange;
  end;
end;

procedure TMapViewColors.SetArrowUpColor(const Value: TColor);
begin
  if FArrowUpColor <> Value then
  begin
    FArrowUpColor := Value;
    DoChange;
  end;
end;

procedure TMapViewColors.SetArrowUpSelectedColor(const Value: TColor);
begin
  if FArrowUpSelectedColor <> Value then
  begin
    FArrowUpSelectedColor := Value;
    DoChange;
  end;
end;

procedure TMapViewColors.SetJmpMarkColor(const Value: TColor);
begin
  if JmpMarkColor <> Value then
  begin
    FJmpMarkColor := Value;
    DoChange;
  end;
end;

procedure TMapViewColors.SetJmpMarkTextColor(const Value: TColor);
begin
  if JmpMarkTextColor <> Value then
  begin
    FJmpMarkTextColor := Value;
    DoChange;
  end;
end;

procedure TMapViewColors.SetSeparatorBackgroundColor(const Value: TColor);
begin
  if FSeparatorBackgroundColor <> Value then
  begin
    FSeparatorBackgroundColor := Value;
    DoChange;
  end;
end;

procedure TMapViewColors.SetSeparatorBorderColor(const Value: TColor);
begin
  if FSeparatorBorderColor <> Value then
  begin
   FSeparatorBorderColor := Value;
   DoChange;
  end;
end;

procedure TMapViewColors.SetSeparatorTextColor(const Value: TColor);
begin
  if FSeparatorTextColor <> Value then
  begin
   FSeparatorTextColor := Value;
   DoChange;
  end;
end;

{ TFixedHexByteTextMetric }

function TFixedHexByteTextMetric.ByteViewMode: TByteViewMode;
begin
  Result := bvmHex8;
end;

{ TPrimaryMappedRowPainter }

function TPrimaryMappedRowPainter.CalcColumnLengthForCopy(
  Column: TColumnType): Integer;
begin
  if Column = ctDescription then
    Result := 32
  else
    Result := inherited;
end;

{ TRowWithExDescription }

function TRowWithExDescription.AcceptEdit(AColumn: TColumnType): Boolean;
begin
  Result := AColumn = ctOpcode;
end;

function TRowWithExDescription.ByteViewMode: TByteViewMode;
begin
  Result := bvmHex8;
end;

function TRowWithExDescription.CharCount(Column: TColumnType): Integer;
begin
  if Column = ctDescription then
    Result := 0
  else
    Result := inherited;
end;

function TRowWithExDescription.ColumnAsString(AColumn: TColumnType): string;
begin
  if AColumn = ctDescription then
    Result := RawData[RowIndex].Description
  else
    Result := inherited;
end;

constructor TRowWithExDescription.Create(AOwner: TFWCustomHexView);
begin
  if AOwner is TCustomMappedHexView then
    inherited
  else
    raise Exception.CreateFmt(SInvalidOwnerClass, [ClassName, AOwner.ClassName]);

  // дестроить не нужно - метрика зарегистрируется в общем списке

  // no destroying is necessary - the metric will be registered in the common list

  FTextMetric := GetTextMetricClass.Create(AOwner);
end;

procedure TRowWithExDescription.DrawColorGroup(ACanvas: TCanvas;
  var ARect: TRect);
begin
  // для размапленных данных отрисовка цветовых групп избыточна

  // color group rendering is redundant for unmapped data
end;

procedure TRowWithExDescription.DrawDataPart(ACanvas: TCanvas;
  var ARect: TRect);
var
  R: TRect;
  DataString: string;
begin
  DataString := RawData[RowIndex].Description;

  if not DrawRowColumnBackground(ACanvas, ctDescription, ARect) then
  begin
    R := ARect;
    R.Width := CharWidth * Length(DataString);
    InflateRect(R, ToDpi(2), 0);
    R.Right := Min(R.Right, ARect.Right);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := SelectedColor(SelData.SelectStyle);
    ACanvas.FillRect(R);
  end;

  ACanvas.Brush.Style := bsClear;
  DrawAlignedTextPart(ACanvas, ctDescription, DataString, ARect);

  if RawData[RowIndex].LinkLength > 0 then
  begin
    R := GetLineJmpMarkRect(ARect);
    if TMapViewColors(ColorMap).JmpMarkColor <> clDefault then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := TMapViewColors(ColorMap).JmpMarkColor;
    end;
    ACanvas.Font.Color := TMapViewColors(ColorMap).JmpMarkTextColor;
    ACanvas.Font.Style := [TFontStyle.fsUnderline];
    DataString := Copy(DataString, RawData[RowIndex].LinkStart + 1,
      RawData[RowIndex].LinkLength);
    DrawAlignedTextPart(ACanvas, ctDescription, DataString, R);
    ACanvas.Font.Style := [];
  end;
end;

function TRowWithExDescription.FormatRowColumn(AColumn: TColumnType;
  const Value: string): string;

  function AlignStr(const Value: string; CharLength: Integer): string;
  begin
    Result := Value;
    if Length(Result) < CharLength then
      Result := Result +
        StringOfChar(' ', CharLength - Length(Result));
  end;

begin
  if AColumn = ctDescription then
    Result := AlignStr(Value, CalcColumnLengthForCopy(AColumn))
  else
    Result := inherited;
end;

procedure TRowWithExDescription.GetHitInfo(var AMouseHitInfo: TMouseHitInfo;
  XPos, YPos: Int64);
var
  LeftOffset: Integer;
begin
  if AMouseHitInfo.SelectPoint.Column = ctDescription then
  begin
    if RawData[AMouseHitInfo.SelectPoint.RowIndex].LinkLength > 0 then
    begin
      LeftOffset := AMouseHitInfo.ColumnStart;
      if XPos > LeftOffset + AMouseHitInfo.ColumnWidth - TextMargin then Exit;
      Inc(LeftOffset, RawData[AMouseHitInfo.SelectPoint.RowIndex].LinkStart * CharWidth);
      Inc(LeftOffset, TextMargin);
      if XPos >= LeftOffset then
        TCustomMappedHexView(Owner).CursorOnJmpMark := XPos < (LeftOffset +
          RawData[AMouseHitInfo.SelectPoint.RowIndex].LinkLength * CharWidth);
    end;
  end
  else
    inherited;
end;

function TRowWithExDescription.GetLineJmpMarkRect(const ARect: TRect): TRect;
var
  LeftOffset, MarkWidth: Integer;
begin
  LeftOffset := RawData[RowIndex].LinkStart * CharWidth;
  MarkWidth := RawData[RowIndex].LinkLength * CharWidth;
  if LeftOffset + MarkWidth + DblSize(TextMargin) > ColumnWidth[ctDescription] then
    MarkWidth := ColumnWidth[ctDescription] - DblSize(TextMargin) - LeftOffset;
  Result := MakeSelectRect(ARect.Left + LeftOffset, ARect.Top, MarkWidth);
end;

function TRowWithExDescription.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TFixedHexByteTextMetric;
end;

function TRowWithExDescription.RawData: TMappedRawData;
begin
  Result := TMappedRawData(inherited RawData);
end;

function TRowWithExDescription.TextMetric: TAbstractTextMetric;
begin
  Result := FTextMetric;
end;

{ TRowMask }

procedure TRowMask.DrawHexPart(ACanvas: TCanvas; var ARect: TRect);
var
  AWidth, ACenteredOffset: Integer;
  P: TPoint;
  R: TRect;
  Data: TMappedRawData;
begin
  inherited;
  Data := RawData[RowIndex];
  ACanvas.Pen.Color := ColorMap.TextColor;
  AWidth := ToDpi(3);
  if Data.Expanded then
  begin
    ACenteredOffset := (RowHeight - AWidth) div 2;
    R := Bounds(ACenteredOffset, ACenteredOffset, AWidth, AWidth);
    OffsetRect(R,
      ARect.Left + TextMetric.SelectionLength(ctOpcode, 1, Data.RawLength) - MulDiv(AWidth, 3, 2),
      ARect.Top);
    if R.Right >= ARect.Right then Exit;
    P := R.TopLeft;
    Dec(P.Y, AWidth div 2);
    DrawArrow(ACanvas, sdDown, P, AWidth);
  end
  else
  begin
    ACenteredOffset := (RowHeight - AWidth * 2) div 2;
    R := Bounds(ACenteredOffset, ACenteredOffset, AWidth, AWidth);
    OffsetRect(R,
      ARect.Left + TextMetric.SelectionLength(ctOpcode, 1, Data.RawLength) - AWidth,
      ARect.Top);
    if R.Right >= ARect.Right then Exit;
    P := R.TopLeft;
    Dec(P.Y, AWidth div 2);
    DrawArrow(ACanvas, sdRight, P, AWidth);
  end;
end;

procedure TRowMask.GetHitInfo(var AMouseHitInfo: TMouseHitInfo; XPos,
  YPos: Int64);
var
  AWidth, LeftOffset: Integer;
begin
  if AMouseHitInfo.SelectPoint.Column = ctOpcode then
  begin
    AWidth := ToDpi(3);
    LeftOffset := AMouseHitInfo.ColumnStart +
      TextMetric.SelectionLength(ctOpcode, 1, RawData[RowIndex].RawLength) + AWidth;
    TCustomMappedHexView(Owner).CursorOnJmpMark :=
      (XPos >= LeftOffset) and (XPos <= LeftOffset + RowHeight + AWidth)
  end
  else
    inherited;
end;

{ TRowAssembler }

function TRowAssembler.AcceptEdit(AColumn: TColumnType): Boolean;
begin
  Result := False;
end;

{ TSecondaryMappedRowPainter }

constructor TSecondaryMappedRowPainter.Create(AOwner: TFWCustomHexView);
begin
  if AOwner is TCustomMappedHexView then
    inherited
  else
    raise Exception.CreateFmt(SInvalidOwnerClass, [ClassName, AOwner.ClassName]);
end;

function TSecondaryMappedRowPainter.RawData: TMappedRawData;
begin
  Result := TMappedRawData(inherited RawData);
end;

{ TRowSeparator }

procedure TRowSeparator.CopyRowAsString(Builder: TSimplyStringBuilder);

  function AlignStr(const Value: string; CharLength: Integer): string;
  begin
    Result := Value;
    if Length(Result) < CharLength then
      Result :=
        StringOfChar(' ', (CharLength - Length(Result)) div 2) + Result;
  end;

begin
  Builder.Append(StringOfChar('-', 120) + sLineBreak);
  Builder.Append(AlignStr(RawData[RowIndex].Description, 120) + sLineBreak);
  Builder.Append(StringOfChar('-', 120) + sLineBreak);
end;

procedure TRowSeparator.DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
  var ARect: TRect);
begin
  if AColumn <> ctNone then Exit;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := TMapViewColors(ColorMap).SeparatorBackgroundColor;
  ACanvas.Pen.Color := TMapViewColors(ColorMap).SeparatorBorderColor;
  Inc(ARect.Left, GetLeftNCWidth);
  Inc(ARect.Right);
  Dec(ARect.Top);
  ACanvas.Rectangle(ARect);
  Inc(ARect.Top);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := TMapViewColors(ColorMap).SeparatorTextColor;
  CorrectCanvasFont(ACanvas, AColumn);
  DrawText(ACanvas, PChar(RawData[RowIndex].Description),
    -1, ARect, DT_CENTER);
end;

{ TRowLineSeparator }

procedure TRowLineSeparator.CopyRowAsString(Builder: TSimplyStringBuilder);
begin
  Builder.Append(StringOfChar('-', 120) + sLineBreak);
end;

procedure TRowLineSeparator.DrawColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
begin
  if AColumn <> ctNone then Exit;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Color := ColorMap.RowSeparatorColor;
  Inc(ARect.Left, GetLeftNCWidth);
  ACanvas.MoveTo(ARect.Left, ARect.CenterPoint.Y);
  ACanvas.LineTo(ARect.Right, ARect.CenterPoint.Y);
end;

{ TRowComment }

procedure TRowComment.CopyRowAsString(Builder: TSimplyStringBuilder);
begin
  Builder.Append(RawData[RowIndex].Description + sLineBreak);
end;

procedure TRowComment.DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
  var ARect: TRect);
var
  ADescription: string;
begin
  if AColumn <> ctNone then Exit;
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('TRowComment.DrawColumn');{$ENDIF}
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := SelectedColor(SelData.SelectStyle);
  Inc(ARect.Left, GetLeftNCWidth);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := TMapViewColors(ColorMap).TextCommentColor;
  ADescription := RawData[RowIndex].Description;
  DrawText(ACanvas, PChar(ADescription),
    Length(ADescription), ARect, DT_CALCRECT);
  ACanvas.FillRect(ARect);
  CorrectCanvasFont(ACanvas, AColumn);
  DrawText(ACanvas, PChar(ADescription),
    Length(ADescription), ARect, DT_LEFT);
  ACanvas.Font.Style := [];
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
end;

{ TRowCheckRadioMask }

function TRowCheckRadioMask.AcceptSelection: Boolean;
begin
  Result := True;
end;

function TRowCheckRadioMask.CalcColumnLengthForCopy(
  Column: TColumnType): Integer;
begin
  if Column = ctDescription then
    Result := 32
  else
    Result := inherited;
end;

function TRowCheckRadioMask.ColumnAsString(AColumn: TColumnType): string;
begin
  if AColumn = ctDescription then
    Result := RawData[RowIndex].Description
  else
    Result := inherited;
end;

constructor TRowCheckRadioMask.Create(AOwner: TFWCustomHexView);
begin
  inherited;
  FTextMetric := GetTextMetricClass.Create(AOwner);
end;

procedure TRowCheckRadioMask.DrawCheckPart(ACanvas: TCanvas;
  const ARect: TRect);
var
  R, CheckRect: TRect;
  Details: TThemedElementDetails;
begin
  CheckRect := Rect(ARect.Left, ARect.Top, ARect.Left +
    Min(RowHeight, ToDpi(13)), ARect.Top + Min(RowHeight, ToDpi(13)));
  OffsetRect(CheckRect, 0, (RowHeight - CheckRect.Height) div 2);
  if StyleServices.Enabled then
  begin
    if RawData[RowIndex].Checked then
      Details := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal)
    else
      Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    StyleServices.DrawElement(ACanvas.Handle, Details, CheckRect, nil, CurrentPPI);
  end
  else
  begin
    if RawData[RowIndex].Checked then
      DrawFrameControl(ACanvas.Handle, CheckRect, DFC_BUTTON, DFCS_CHECKED)
    else
      DrawFrameControl(ACanvas.Handle, CheckRect, DFC_BUTTON, 0);
  end;
  ACanvas.Brush.Color := SelectedColor(GetSelectData(RowIndex).SelectStyle);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := ColorMap.TextColor;
  R := ARect;
  R.Left := CheckRect.Right + ToDpi(4);
  CorrectCanvasFont(ACanvas, ctDescription);
  DrawText(ACanvas, PChar(RawData[RowIndex].Description), -1, R, 0);
end;

procedure TRowCheckRadioMask.DrawColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
begin
  case AColumn of
    ctDescription:
      if RawData[RowIndex].Style = rsMaskRadio then
        DrawRadioPart(ACanvas, ARect)
      else
        DrawCheckPart(ACanvas, ARect);
    ctComment:
      DrawCommentPart(ACanvas, ARect);
  end;
end;

procedure TRowCheckRadioMask.DrawCommentPart(ACanvas: TCanvas;
  const ARect: TRect);
var
  DataString: string;
begin
  DataString := RawData[RowIndex].Comment;
  if DataString = '' then Exit;

  if not DrawRowColumnBackground(ACanvas, ctComment, ARect) then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := SelectedColor(GetSelectData(RowIndex).SelectStyle);
  end;

  ACanvas.Font.Style := [];
  ACanvas.Font.Color := ColorMap.TextCommentColor;
  DrawAlignedTextPart(ACanvas, ctComment, DataString, ARect);
end;

procedure TRowCheckRadioMask.DrawRadioPart(ACanvas: TCanvas;
  const ARect: TRect);
var
  R, CheckRect: TRect;
  Details: TThemedElementDetails;
begin
  CheckRect := Rect(ARect.Left, ARect.Top, ARect.Left +
    Min(RowHeight, ToDpi(13)), ARect.Top + Min(RowHeight, ToDpi(13)));
  OffsetRect(CheckRect, 0, (RowHeight - CheckRect.Height) div 2);
  if StyleServices.Enabled then
  begin
    if RawData[RowIndex].Checked then
      Details := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal)
    else
      Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
    StyleServices.DrawElement(ACanvas.Handle, Details, CheckRect, nil, CurrentPPI);
  end
  else
  begin
    if RawData[RowIndex].Checked then
      DrawFrameControl(ACanvas.Handle, CheckRect, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_CHECKED)
    else
      DrawFrameControl(ACanvas.Handle, CheckRect, DFC_BUTTON, DFCS_BUTTONRADIO);
  end;
  ACanvas.Brush.Color := SelectedColor(GetSelectData(RowIndex).SelectStyle);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := ColorMap.TextColor;
  R := ARect;
  R.Left := CheckRect.Right + ToDpi(4);
  CorrectCanvasFont(ACanvas, ctDescription);
  DrawText(ACanvas, PChar(RawData[RowIndex].Description), -1, R, 0);
end;

function TRowCheckRadioMask.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TFixedHexByteTextMetric;
end;

function TRowCheckRadioMask.RawData: TMappedRawData;
begin
  Result := TMappedRawData(inherited RawData);
end;

function TRowCheckRadioMask.TextMetric: TAbstractTextMetric;
begin
  Result := FTextMetric;
end;

{ TCommonMapViewPostPainter }

constructor TCommonMapViewPostPainter.Create(AOwner: TFWCustomHexView);
begin
  if AOwner is TCustomMappedHexView then
    inherited
  else
    raise Exception.CreateFmt(SInvalidOwnerClass, [ClassName, AOwner.ClassName]);
end;

function TCommonMapViewPostPainter.IsNeedOffsetRow(ARowIndex: Int64;
  FirstRow: Boolean): Boolean;
begin
  if FirstRow then
    Result := RawData[ARowIndex].Linked
  else
    Result := RawData[ARowIndex].JmpToAddr <> 0;
end;

function TCommonMapViewPostPainter.LineColorPresent(Selected: Boolean;
  const Param: TDrawLineParam; out LineColor: TColor): Boolean;
begin
  Result := True;
  if Param.LineColor <> clDefault then
    LineColor := Param.LineColor
  else
    if Selected then
    begin
      if Param.DirectionDown then
        LineColor := TMapViewColors(ColorMap).ArrowDownSelectedColor
      else
        LineColor := TMapViewColors(ColorMap).ArrowUpSelectedColor;
    end
    else
      if not Param.DrawOnlySelectedArrow then
      begin
        if Param.DirectionDown then
          LineColor := TMapViewColors(ColorMap).ArrowDownColor
        else
          LineColor := TMapViewColors(ColorMap).ArrowUpColor;
      end
      else
        Result := False;
end;

function TCommonMapViewPostPainter.Owner: TCustomMappedHexView;
begin
  Result := TCustomMappedHexView(inherited Owner);
end;

function TCommonMapViewPostPainter.RawData: TMappedRawData;
begin
  Result := TMappedRawData(inherited RawData);
end;

{ TJumpLinesPostPainter }

procedure TJumpLinesPostPainter.PostPaint(ACanvas: TCanvas;
  StartRow, EndRow: Int64; var Offset: TPoint);

  procedure Process(DrawOnlySelectedArrow: Boolean;
    JmpData: TObjectDictionary<Int64, TList<Int64>>);
  var
    I, JmpLine: Int64;
    A: Integer;
    Param: TDrawLineParam;
    IncomingJmps: TList<Int64>;
  begin
    PaintedLinesCount := 1;
    Param.DrawArrow := True;
    Param.LineIndent := ToDpi(4);
    Param.LineVerticalMargin := DblSize(SplitMargin);
    Param.LineWidth := 1;
    Param.LineColor := clDefault;
    Param.DrawOnlySelectedArrow := DrawOnlySelectedArrow;
    Param.Offset := Offset;
    for I := StartRow to EndRow do
    begin

      // последняя строка может быть невидимой (часть затерта скролом)

      // the last line can be invisible (part of it hide by scroll-ctrl)

      if (I = EndRow) and not RowVisible(I) then Exit;

      if RawData[I].Style in [rsRawWithExDescription, rsAsm] then
        if RawData[I].JmpToAddr <> 0 then
        begin
          JmpLine := Owner.CalculateJmpToRow(I);
          if JmpLine >= 0 then
          begin
            Param.DirectionDown := JmpLine > I;
            Param.RowFrom := I;
            Param.RowTo := JmpLine;
            Param.SecondDraw := False;
            DrawLine(ACanvas, Param);
            Inc(PaintedLinesCount);
          end;
        end;

      if not Owner.DrawIncomingJmp then
        Continue;

      if RawData[I].Linked and JmpData.TryGetValue(I, IncomingJmps) then
        for A := 0 to IncomingJmps.Count - 1 do
        begin
          Param.RowFrom := IncomingJmps[A];
          if RowVisible(Param.RowFrom) then Continue;
          Param.RowTo := I;
          Param.DirectionDown := Param.RowFrom < I;
          Param.SecondDraw := True;
          DrawLine(ACanvas, Param);
          Inc(PaintedLinesCount);
        end;
    end;
  end;

begin
  if ctJmpLine in Columns then
  begin
    Process(False, Owner.JmpData);
    Process(True, Owner.JmpData);
  end;
end;

{ TCheckRadioRowPostPainter }

procedure TCheckRadioRowPostPainter.DrawArrowPart(ACanvas: TCanvas;
  RowIndex: Int64; var Offset: TPoint; DrawOnlySelectedArrow: Boolean);
var
  StartPoint, EndPoint: TPoint;
  Selected: Boolean;
  ArrowSize: Integer;
begin
  Selected := GetSelectData(RowIndex).SelectStyle <> ssNone;

  if not Selected and DrawOnlySelectedArrow then Exit;

  if Selected then
    ACanvas.Pen.Color := TMapViewColors(ColorMap).ArrowDownSelectedColor
  else
    ACanvas.Pen.Color := TMapViewColors(ColorMap).ArrowDownColor;

  StartPoint.X :=
    Offset.X +
    TextMargin +
    CharWidth shr 1 + // центрирование по символу
                      // character centering
    TextMetric.CharLength(ctOpcode, 0, RawData[RowIndex].CharIndex - 1);

  StartPoint.Y := Offset.Y;

  // оффсет нужен на линию, на которую указываем

  // Offset is needed on the line we're pointing at.

  Inc(StartPoint.Y, RowHeight);
  Dec(StartPoint.Y, (RowIndex - RawData[RowIndex].MaskRowIndex) * RowHeight);

  EndPoint.X := Offset.X + ColumnWidth[ctOpcode] - TextMargin;
  EndPoint.Y := Offset.Y + RowHeight shr 1;

  ACanvas.MoveTo(StartPoint.X, StartPoint.Y + ToDpi(5));
  ACanvas.LineTo(StartPoint.X, EndPoint.Y);
  ACanvas.LineTo(EndPoint.X, EndPoint.Y);
  Dec(StartPoint.Y, ToDpi(1));
  ArrowSize := ToDpi(2);
  DrawArrowWithOffset(ACanvas, sdDown, StartPoint, ArrowSize, False);
  DrawArrowWithOffset(ACanvas, sdRight, EndPoint, ArrowSize, False);

  Inc(Offset.X, ColumnWidth[ctOpcode]);
end;

procedure TCheckRadioRowPostPainter.DrawRow(ACanvas: TCanvas; RowIndex: Int64;
  var Offset: TPoint; DrawOnlySelectedArrow, DrawRadio: Boolean);
var
  LeftOffset: Integer;
  I: TColumnType;
begin
  LeftOffset := Offset.X;
  try
    for I := ctWorkSpace to ctOpcode do
      if I in Columns then
      begin
        if I = ctOpcode then
          DrawArrowPart(ACanvas, RowIndex, Offset, DrawOnlySelectedArrow)
        else
          Inc(Offset.X, ColumnWidth[I]);
      end;
  finally
    Offset.X := LeftOffset;
  end;
  Inc(Offset.Y, RowHeight);
end;

procedure TCheckRadioRowPostPainter.PostPaint(ACanvas: TCanvas;
  StartRow, EndRow: Int64; var Offset: TPoint);
var
  I, TopOffset, StartRowWithMask, EndRowWithMask: Integer;
begin
  TopOffset := Offset.Y;

  // Быстрая проверка валидности данных

  // Quick data validity check

  if not IsRowVisible(StartRow) then Exit;

  // Определяем где начнутся и закончатся расширенные чеки и радиокнопки

  // Determine where extended checks and radio buttons begin and end

  StartRowWithMask := StartRow;
  while RawData[StartRowWithMask].Style in [rsMaskCheck..rsMaskSeparator, rsNone, rsLine] do
  begin
    if StartRowWithMask = 0 then Break;
    Dec(StartRowWithMask);
    Dec(TopOffset, RowHeight);
  end;

  EndRowWithMask := EndRow;
  while RawData[EndRowWithMask].Style in [rsMaskCheck..rsMaskSeparator, rsNone, rsLine] do
  begin
    if EndRowWithMask = RawData.Count - 1 then Break;
    Inc(EndRowWithMask);
  end;

  // рисуем стрелки в два прохода, лениво заморачиваться с отсечкой
  // по вертикали чтобы не подсвеченные линии не перезатирали подсвеченные

  // we draw arrows in two passes, it is lazy to bother with vertical cutoff
  // so that not illuminated lines do not overwrite the illuminated ones.

  Offset.Y := TopOffset;
  for I := StartRowWithMask to EndRowWithMask do
    if RawData[I].Style in [rsMaskCheck, rsMaskRadio] then
      DrawRow(ACanvas, I, Offset, False, RawData[I].Style = rsMaskRadio)
    else
      Inc(Offset.Y, RowHeight);

  Offset.Y := TopOffset;
  for I := StartRowWithMask to EndRowWithMask do
    if RawData[I].Style in [rsMaskCheck, rsMaskRadio] then
      DrawRow(ACanvas, I, Offset, True, RawData[I].Style = rsMaskRadio)
    else
      Inc(Offset.Y, RowHeight);
end;

{ TVirtualPage }

procedure TVirtualPage.DoChange;
begin
  if Assigned(FOwner) then
    FOwner.RebuildData;
end;

procedure TVirtualPage.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    FCaption := Value;
    DoChange;
  end;
end;

procedure TVirtualPage.SetShowCaption(const Value: Boolean);
begin
  if ShowCaption <> Value then
  begin
    FShowCaption := Value;
    DoChange;
  end;
end;

procedure TVirtualPage.SetSize(const Value: DWORD);
begin
  if Size <> Value then
  begin
    FSize := Value;
    DoChange;
  end;
end;

procedure TVirtualPage.SetVirtualAddress(const Value: Int64);
begin
  if VirtualAddress <> Value then
  begin
    FVirtualAddress := Value;
    DoChange;
  end;
end;

{ TVirtualPages }

function TVirtualPages.AddPage(const Caption: string;
  VirtualAddress: Int64; Size: DWORD): Integer;
var
  NewPage: TVirtualPage;
  Index: Integer;
begin
  if Size <= 0 then
    raise Exception.CreateFmt('Invalid page size %d', [Size]);
  if VirtualAddress < FOwner.StartAddress then
    raise Exception.CreateFmt(
      'New Page hase lower address 0x%x than initial 0x%x.',
      [VirtualAddress, FOwner.StartAddress]);
  if GetPageIndex(VirtualAddress, Index) = pirPagePresent then
    raise Exception.CreateFmt(
      'New Page region intersect with page at address 0x%x.',
      [FPages[Index].VirtualAddress]);
  if GetPageIndex(VirtualAddress + Size, Index) = pirPagePresent then
    raise Exception.CreateFmt(
      'New Page region intersect with page at address 0x%x.',
      [FPages[Index].VirtualAddress]);

  NewPage := TVirtualPage.Create;
  NewPage.Caption := Caption;
  NewPage.ShowCaption := True;
  NewPage.VirtualAddress := VirtualAddress;
  NewPage.Size := Size;
  NewPage.Owner := FOwner;
  FPages.Add(NewPage);
  FPages.Sort;
  GetPageIndex(VirtualAddress, Result);
  DoChange;
end;

function TVirtualPages.CheckAddrInPages(VirtualAddress: Int64): Boolean;
var
  PageIndex: TPageIndexResult;
  Tmp: Integer;
  StartAddr, EndAddr: Int64;
begin
  Result := False;
  PageIndex := GetPageIndex(VirtualAddress, Tmp);
  if PageIndex = pirPagePresent then
    Result := True
  else

    // отдельно проверяем самую последнюю страницу которая может в ключать в себя
    // оверлей не описанный в секциях

    // separately check the most recent page, which may include an overlay
    // not described in the sections.

    if Count > 0 then
    begin
      StartAddr := FPages[Count - 1].VirtualAddress;
      EndAddr := MaxAddrAware;
      Result := (StartAddr <= VirtualAddress) and (EndAddr > VirtualAddress);
    end;
end;

function TVirtualPages.Count: Integer;
begin
  Result := FPages.Count;
end;

function VirtualPagesComparer({$IFDEF EXTENDED_RTL}const{$ELSE}constref{$ENDIF} A, B: TVirtualPage): Integer;
begin
  if Int64(A.VirtualAddress) < Int64(B.VirtualAddress) then
    Result := -1
  else
    if Int64(A.VirtualAddress) = Int64(B.VirtualAddress) then
      Result := 0
    else
      Result := 1;
end;

constructor TVirtualPages.Create(AOwner: TCustomMappedHexView);
begin
  FOwner := AOwner;
  FPages := TObjectList<TVirtualPage>.Create(
    TComparer<TVirtualPage>.Construct(VirtualPagesComparer));
end;

procedure TVirtualPages.Delete(Index: Integer);
begin
  FPages.Delete(Index);
  DoChange;
end;

destructor TVirtualPages.Destroy;
begin
  FPages.Free;
  inherited;
end;

procedure TVirtualPages.DoChange;
begin
  FOwner.RebuildData;
end;

function TVirtualPages.GetItem(Index: Integer): TVirtualPage;
begin
  Result := FPages[Index];
end;

function TVirtualPages.GetPageIndex(VirtualAddress: Int64;
  out Index: Integer): TPageIndexResult;
var
  I: Integer;
  APage: TVirtualPage;
begin
  Index := -1;
  Result := pirOutOfBounds;
  if VirtualAddress < FOwner.StartAddress then
    Exit;
  Result := pirNotPaged;
  for I := 0 to Count - 1 do
  begin
    APage := FPages[I];
    if (APage.VirtualAddress <= VirtualAddress) and
      (APage.VirtualAddress + APage.Size > VirtualAddress) then
    begin
      Index := I;
      Result := pirPagePresent;
      Break;
    end;
  end;
end;

function TVirtualPages.MaxAddrAware: Int64;
var
  I: Integer;
begin
  if Count = 0 then
    Result := FOwner.StartAddress + CheckNegative(FOwner.GetDataStreamSize)
  else
  begin
    Result := FPages[0].VirtualAddress - FOwner.StartAddress;
    for I := 0 to Count - 1 do
      Inc(Result, FPages[I].Size);
    Result := FOwner.StartAddress + FPages[Count - 1].VirtualAddress +
      CheckNegative(FOwner.GetDataStreamSize) - Result;
  end;
end;

{ TViewShortCut }

procedure TViewShortCut.AssignTo(Dest: TPersistent);
begin
  if Dest is TViewShortCut then
  begin
    if Assigned(TViewShortCut(Dest).SecondaryShortCut) then
    begin
      if Assigned(FSecondaryShortCut) then
        TViewShortCut(Dest).SecondaryShortCut := SecondaryShortCut
      else
        TViewShortCut(Dest).SecondaryShortCut.Clear;
    end;
    TViewShortCut(Dest).FShortCut := FShortCut;
  end
  else
    inherited;
end;

constructor TViewShortCut.Create(ADefault: TShortCut);
begin
  FDefault := ADefault;
  FShortCut := ADefault;
end;

destructor TViewShortCut.Destroy;
begin
  FSecondaryShortCut.Free;
  inherited;
end;

function TViewShortCut.GetSecondaryShortCut: TShortCutList;
begin
  if FSecondaryShortCut = nil then
    FSecondaryShortCut := TShortCutList.Create;
  Result := FSecondaryShortCut;
end;

function TViewShortCut.IsCustomViewShortCutStored: Boolean;
begin
  Result := IsSecondaryStored or IsShortSutStored;
end;

function TViewShortCut.IsSecondaryStored: Boolean;
begin
  Result := Assigned(FSecondaryShortCut) and (FSecondaryShortCut.Count > 0);
end;

function TViewShortCut.IsShortCut(Key: Word; Shift: TShiftState): Boolean;
var
  AShortCut: TShortCut;
  I: Integer;
begin
  Result := False;
  AShortCut := Menus.ShortCut(Key, Shift);
  if ShortCut = AShortCut then Exit(True);
  if FSecondaryShortCut = nil then Exit;
  for I := 0 to FSecondaryShortCut.Count - 1 do
    if FSecondaryShortCut.ShortCuts[I] = AShortCut then
      Exit(True);
end;

function TViewShortCut.IsShortSutStored: Boolean;
begin
  Result := ShortCut <> FDefault;
end;

procedure TViewShortCut.SetSecondaryShortCut(const Value: TShortCutList);
begin
  SecondaryShortCut.Assign(Value);
end;

{ TViewShortCuts }

procedure TViewShortCuts.AssignTo(Dest: TPersistent);
begin
  if Dest is TViewShortCuts then
  begin
    TViewShortCuts(Dest).JmpBack := JmpBack;
    TViewShortCuts(Dest).JmpTo := JmpTo;
  end
  else
    inherited;
end;

constructor TViewShortCuts.Create;
begin
  FJmpBack := TViewShortCut.Create(VK_BACK);
  FJmpTo := TViewShortCut.Create(VK_RETURN);
end;

destructor TViewShortCuts.Destroy;
begin
  FJmpBack.Free;
  FJmpTo.Free;
  inherited;
end;

function TViewShortCuts.IsJmpBackStored: Boolean;
begin
  Result := JmpBack.IsCustomViewShortCutStored
end;

function TViewShortCuts.IsJmpToStored: Boolean;
begin
  Result := JmpTo.IsCustomViewShortCutStored;
end;

function TViewShortCuts.IsShortCutsStored: Boolean;
begin
  Result := IsJmpBackStored or IsJmpToStored;
end;

procedure TViewShortCuts.SetJmpBack(const Value: TViewShortCut);
begin
  FJmpBack.Assign(Value);
end;

procedure TViewShortCuts.SetJmpTo(const Value: TViewShortCut);
begin
  FJmpTo.Assign(Value);
end;

{ TCustomMappedHexView }

function TCustomMappedHexView.CalculateJmpToRow(JmpFromRow: Int64): Int64;
var
  OldAddressToRowMode: TAddressToRowIndexMode;
begin
  OldAddressToRowMode := AddressToRowIndexMode;
  AddressToRowIndexMode := armFindFirstRaw;
  Result := RawData.AddressToRowIndex(RawData[JmpFromRow].JmpToAddr);
  AddressToRowIndexMode := OldAddressToRowMode;
end;

procedure TCustomMappedHexView.DoCaretKeyDown(var Key: Word; Shift: TShiftState);
var
  RowIndex: Int64;
begin
  if ShortCuts.JmpTo.IsShortCut(Key, Shift) then
  begin
    RowIndex := SelectedRowIndex;
    if RowIndex < 0 then Exit;
    if RawData[RowIndex].JmpToAddr = 0 then Exit;
    DoJmpTo(RowIndex, jsPushToUndo);
  end;
  if ShortCuts.JmpBack.IsShortCut(Key, Shift) then
    DoJmpTo(0, jsPopFromUndo);
end;

procedure TCustomMappedHexView.ClearDataMap;
begin
  SetDataStream(nil, 0);
  RebuildData;
end;

constructor TCustomMappedHexView.Create(AOwner: TComponent);
begin
  inherited;
  FDataMap := TDataMap.Create(Self);
  FPages := TVirtualPages.Create(Self);
  FPreviosJmp := TList<Int64>.Create;
  FJmpInitList := TList<Int64>.Create;
  FJmpData := TObjectDictionary<Int64, TList<Int64>>.Create([doOwnsValues]);
  FShortCuts := GetShortCutsClass.Create;
end;

destructor TCustomMappedHexView.Destroy;
begin
  FShortCuts.Free;
  FJmpData.Free;
  FJmpInitList.Free;
  FPreviosJmp.Free;
  FPages.Free;
  FDataMap.Free;
  inherited;
end;

procedure TCustomMappedHexView.DoInvalidateRange(AStartRow, AEndRow: Int64);
var
  I: Int64;
begin

  // если отображается колонка с линиями прыжков
  // и в селекшен попадает одна из стрелок, нужно перерисовать вьюху целиком

  // If a column with jump lines is displayed and one of the arrows falls
  // into the selection, you need to redraw the entire view.

  if ctJmpLine in Header.Columns then
    for I := AStartRow to AEndRow do
      with RawData[I] do
        if (JmpToAddr <> 0) or Linked then
        begin
          Invalidate;
          Exit;
        end;

  inherited;
end;

procedure TCustomMappedHexView.DoJmpTo(ARowIndex: Int64; AJmpState: TJmpState);
var
  Handled: Boolean;
  JmpAddr: Int64;
  NewRowIndex: Int64;
begin
  Handled := False;

  if RawData[ARowIndex].Style = rsMask then
  begin
    DataMap.Data.List[RawData.MapRowIndex].Expanded := not RawData.Expanded;
    ClearSelection;
    UpdateDataMap;
    UpdateTextBoundary;
    UpdateScrollPos;
    Invalidate;
    Exit;
  end;

  if AJmpState = jsPushToUndo then
    JmpAddr := RawData[ARowIndex].JmpToAddr
  else
    JmpAddr := -1;
  if Assigned(FJmpToEvent) then
    FJmpToEvent(Self, JmpAddr, AJmpState, Handled);
  if Handled then Exit;

  case AJmpState of
    jsPushToUndo:
    begin
      // прыжки делются в два шага для восстановления
      // состояния экрана на откате

      // jumps are divided in two steps to restore the screen state on rollback

      FPreviosJmp.Count := FPreviosJmpIdx;
      FPreviosJmp.Add(ARowIndex);
      FPreviosJmp.Add(CurrentVisibleRow);
      Inc(FPreviosJmpIdx, 2);
      FocusOnAddress(RawData[ARowIndex].JmpToAddr, ccmSelectRow);
    end;
    jsPopFromUndo:
    begin
      if FPreviosJmpIdx = 0 then Exit;
      NewRowIndex := FPreviosJmp[FPreviosJmpIdx - 1];
      FocusOnAddress(RawData[NewRowIndex].Address, ccmNone);
      NewRowIndex := FPreviosJmp[FPreviosJmpIdx - 2];
      Dec(FPreviosJmpIdx, 2);
      FocusOnAddress(RawData[NewRowIndex].Address, ccmSelectRow);
    end;
    jsRestorePopFromUndo:
    begin
      if FPreviosJmpIdx >= FPreviosJmp.Count then Exit;
      Inc(FPreviosJmpIdx, 2);
      NewRowIndex := FPreviosJmp[FPreviosJmpIdx - 2];
      JmpAddr := RawData[NewRowIndex].JmpToAddr;
      FocusOnAddress(JmpAddr, ccmSelectRow);
    end;
  end;

  if Assigned(FJmpToEvent) then
    FJmpToEvent(Self, JmpAddr, jsJmpDone, Handled);
end;

function TCustomMappedHexView.DoLButtonDown(Shift: TShiftState): Boolean;
begin
  Result := CursorOnJmpMark;
  if Result then
    DoJmpTo(MousePressedHitInfo.SelectPoint.RowIndex, jsPushToUndo);
end;

procedure TCustomMappedHexView.FitColumnToBestSize(Value: TColumnType);
begin
  case Value of
    ctWorkSpace: Header.ColumnWidth[ctWorkSpace] := ToDpi(32);
    ctJmpLine: Header.ColumnWidth[ctJmpLine] := ToDpi(82);
    ctOpcode: Header.ColumnWidth[ctOpcode] := ToDpi(235);
    ctDescription: Header.ColumnWidth[ctDescription] := ToDpi(377);
    ctComment: Header.ColumnWidth[ctComment] := ToDpi(440);
  else
    inherited;
  end;
end;

function TCustomMappedHexView.GetColorMap: TMapViewColors;
begin
  Result := TMapViewColors(inherited ColorMap);
end;

function TCustomMappedHexView.GetColorMapClass: THexViewColorMapClass;
begin
  Result := TMapViewColors;
end;

function TCustomMappedHexView.GetDefaultPainterClass: TPrimaryRowPainterClass;
begin
  Result := TPrimaryMappedRowPainter;
end;

function TCustomMappedHexView.GetRawDataClass: TRawDataClass;
begin
  Result := TMappedRawData;
end;

function TCustomMappedHexView.GetShortCutsClass: TViewShortCutsClass;
begin
  Result := TViewShortCuts;
end;

procedure TCustomMappedHexView.InitPainters;
begin
  inherited;
  Painters.Add(GetOverloadPainterClass(TRowWithExDescription).Create(Self));
  Painters.Add(GetOverloadPainterClass(TRowAssembler).Create(Self));
  Painters.Add(GetOverloadPainterClass(TRowSeparator).Create(Self));
  Painters.Add(GetOverloadPainterClass(TRowLineSeparator).Create(Self));
  Painters.Add(GetOverloadPainterClass(TRowComment).Create(Self));
  Painters.Add(GetOverloadPainterClass(TRowMask).Create(Self));
  Painters.Add(GetOverloadPainterClass(TRowCheckRadioMask).Create(Self));

  PostPainters.Add(TJumpLinesPostPainter.Create(Self));
  PostPainters.Add(TCheckRadioRowPostPainter.Create(Self));
end;

function TCustomMappedHexView.InternalGetRowPainter(
  ARowIndex: Int64): TAbstractPrimaryRowPainter;
begin
  if (ARowIndex < 0) or (ARowIndex >= RawData.Count) then
    Exit(nil);
  case RawData[ARowIndex].Style of
    rsRaw: Result := Painters[0];
    rsRawWithExDescription: Result := Painters[1];
    rsAsm: Result := Painters[2];
    rsSeparator: Result := Painters[3];
    rsLine: Result := Painters[4];
    rsLineComment, rsBlockComment: Result := Painters[5];
    rsMask: Result := Painters[6];
    rsMaskCheck, rsMaskRadio: Result := Painters[7];
  else
    Result := nil;
  end;
end;

function TCustomMappedHexView.IsShortCutsStored: Boolean;
begin
  Result := ShortCuts.IsShortCutsStored;
end;

procedure TCustomMappedHexView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  {$IFDEF FPC}
  case Button of
    mbExtra1: DoJmpTo(0, jsPopFromUndo);
    mbExtra2: DoJmpTo(0, jsRestorePopFromUndo);
  end;
  {$ENDIF}
end;

procedure TCustomMappedHexView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  CursorOnJmpMark := False;
end;

function TCustomMappedHexView.RawData: TMappedRawData;
begin
  Result := TMappedRawData(inherited RawData);
end;

function TCustomMappedHexView.RowStyle(ARowIndex: Int64): TRowStyle;
begin
  Result := RawData[ARowIndex].Style;
end;

procedure TCustomMappedHexView.SetColorMap(const Value: TMapViewColors);
begin
  ColorMap.Assign(Value);
end;

procedure TCustomMappedHexView.SetDrawIncomingJmp(const Value: Boolean);
begin
  if DrawIncomingJmp <> Value then
  begin
    FDrawIncomingJmp := Value;
    UpdateJumpList;
    Invalidate;
  end;
end;

procedure TCustomMappedHexView.SetShortCuts(const Value: TViewShortCuts);
begin
  FShortCuts.Assign(Value);
end;

procedure TCustomMappedHexView.UpdateCursor(const HitTest: TMouseHitInfo);
begin
  if CursorOnJmpMark then
    Cursor := crHandPoint
  else
    inherited;
end;

procedure TCustomMappedHexView.UpdateDataMap;
begin
  FJmpInitList.Clear;
  inherited;

  // заполнение массива известных прыжков

  // filling the array of known jumps

  UpdateJumpList;
end;

procedure TCustomMappedHexView.UpdateJumpList;
var
  I: Integer;
  JmpFrom, JmpTo: Int64;
  IncomingJmps: TList<Int64>;
begin
  if not DrawIncomingJmp then
    Exit;
  FJmpData.Clear;
  for I := 0 to FJmpInitList.Count - 1 do
  begin
    JmpFrom := FJmpInitList[I];
    JmpTo := CalculateJmpToRow(JmpFrom);
    if JmpTo >= 0 then
    begin
      RawData[JmpFrom].SetLinked(JmpTo);
      if not JmpData.TryGetValue(JmpTo, IncomingJmps) then
      begin
        IncomingJmps := TList<Int64>.Create;
        JmpData.Add(JmpTo, IncomingJmps);
      end;
      IncomingJmps.Add(JmpFrom);
    end;
  end;
end;

{$IFNDEF FPC}
procedure TCustomMappedHexView.WMXButtonDown(var Msg: TWMMouse);
const
  MK_XBUTTON1 = $20;
  MK_XBUTTON2 = $40;
begin
  case Word(Msg.Keys) of
    MK_XBUTTON1: DoJmpTo(0, jsPopFromUndo);
    MK_XBUTTON2: DoJmpTo(0, jsRestorePopFromUndo);
  end;
end;
{$ENDIF}

end.
