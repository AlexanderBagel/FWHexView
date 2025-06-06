﻿////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.MappedView.pas
//  * Purpose   : Implementation of advanced HexView editor with data map support
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2025.
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

  Please send an e-mail to hexview_sale@rousehome.ru to request an invoice before or after payment is made. Payment may be
  made via bank transfer. Bank details will be provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence. Enhancement requests submitted by users with a
  commercial licence will be prioritized.
}

unit FWHexView.MappedView;

{$IFDEF FPC}
  {$I FWHexViewConfig.inc}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType,
  {$ELSE}
  Windows,
  Messages,
  UITypes,
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
  {$IFDEF USE_PROFILER}
  uni_profiler,
  {$ENDIF}
  FWHexView,
  FWHexView.Common;

const
  FLAG_RADIOCHECK = 1;

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
        BitIndex: Byte;
        Checked: Boolean);
  end;

  TCustomMappedHexView = class;

  { TMappedRawData }

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
    function View: TCustomMappedHexView;
  protected
    procedure SetLinked(Index: Int64);
    property PresentRows: TListEx<TRowData> read FRows;
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
    function BitIndex: Byte;
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
        BitIndex: Byte;
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
      {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} {%H-}Item: TMapRow;
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

    function AddMaskCheck(BitIndex: Byte;
      const Description, Comment: string; Checked: Boolean): Integer;
    function AddMaskRadio(BitIndex: Byte;
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
    function CalcColumnLengthForCopy(AColumn: TColumnType): Integer; override;
    function CharCount(AColumn: TColumnType): Integer; override;
    function ColumnAsString(AColumn: TColumnType): string; override;
    procedure DrawColorGroup({%H-}ACanvas: TCanvas; var {%H-}ARect: TRect); override;
    procedure DrawDataPart(ACanvas: TCanvas; var ARect: TRect); override;
    function FormatRowColumn(AColumn: TColumnType;
      const Value: string): string; override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
  public
    constructor Create(AOwner: TFWCustomHexView); override;
  end;

  TRowWithExDescription = class(TPrimaryMappedRowPainter)
  strict private
    FTextMetric: TAbstractTextMetric;
    function GetLineJmpMarkRect(const ARect: TRect): TRect;
  protected
    function ByteViewMode: TByteViewMode; override;
    function CaretEditMode(AColumn: TColumnType): TCaretEditMode; override;
    procedure DrawDataPart(ACanvas: TCanvas; var ARect: TRect); override;
    procedure GetHitInfo(var AHitInfo: TMouseHitInfo); override;
    function GetLinkBoundaries(out ABounds: TBoundaries): Boolean;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
    function TextMetric: TAbstractTextMetric; override;
  public
    constructor Create(AOwner: TFWCustomHexView); override;
  end;

  TMaskTextMetric = class(TDefaultTextMetric)
  strict private
    FByteViewMode: TByteViewMode;
    FRawLength: Integer;
  private
    procedure SetRawLength(const Value: Integer);
  protected
    function ByteViewMode: TByteViewMode; override;
    property RawLength: Integer read FRawLength write SetRawLength;
  public
    constructor Create(AOwner: TFWCustomHexView); override;
  end;

  TRowMask = class(TPrimaryMappedRowPainter)
  protected
    function ByteViewMode: TByteViewMode; override;
    procedure DrawHexPart(ACanvas: TCanvas; var ARect: TRect); override;
    procedure GetHitInfo(var AHitInfo: TMouseHitInfo); override;
    procedure RowChanged; override;
    function TextMetric: TAbstractTextMetric; override;
  end;

  TRowAssembler = class(TRowWithExDescription)
  protected
    function CaretEditMode(AColumn: TColumnType): TCaretEditMode; override;
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
    function GetCheckRect(const ARect: TRect): TRect;
  protected
    function AcceptSelection: Boolean; override;
    function CalcColumnLengthForCopy(AColumn: TColumnType): Integer; override;
    function ColumnAsString(AColumn: TColumnType): string; override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    procedure GetHitInfo(var AMouseHitInfo: TMouseHitInfo); override;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
    function TextMetric: TAbstractTextMetric; override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
  public
    constructor Create(AOwner: TFWCustomHexView); override;
  end;

  { TCommonMapViewPostPainter }

  TCommonMapViewPostPainter = class(TLinesPostPainter)
  protected
    function IsNeedOffsetRow(ARowIndex: Int64; FirstRow: Boolean): Boolean; override;
    function LineColorPresent(Selected: Boolean; const Param: TDrawLineParam;
      out LineColor: TColor): Boolean; override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
    function View: TCustomMappedHexView;
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
    function TextMetric: TAbstractTextMetric; override;
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

  TJmpData = record
    JmpFrom, JmpTo: Int64;
  end;

  TAddressToRowIndexMode = (armFindFirstRaw, armFindFirstAny);

  TRadioCheckClickEvent = procedure(Sender: TObject; ARowIndex: Int64) of object;

  { TCustomMappedHexView }

  TCustomMappedHexView = class(TFWCustomHexView)
  strict private
    FAddressToRowIndexMode: TAddressToRowIndexMode;
    FDataMap: TDataMap;
    FDrawIncomingJmp: Boolean;
    FJmpInitList: TList<Int64>;
    FJmpData: TObjectDictionary<Int64, TList<Int64>>;
    FLastInvalidAddrRect: TRect;
    FMaskTextMetric: TMaskTextMetric;
    FPages: TVirtualPages;
    FRadioCheckClick: TRadioCheckClickEvent;
    FShowMaskAsValue: Boolean;
    function GetColorMap: TMapViewColors;
    procedure SetColorMap(const Value: TMapViewColors);
    procedure SetDrawIncomingJmp(const Value: Boolean);
    procedure SetShowMaskAsValue(const Value: Boolean);
    procedure UpdateJumpList;
  protected
    function CalculateColumnBestSize(Value: TColumnType): Integer; override;
    function CalculateJmpToRow(JmpFromRow: Int64): Int64; virtual;
    procedure DoCaretKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoGetHint(var AHintParam: THintParam; var AHint: string); override;
    procedure DoInvalidateRange(AStartRow, AEndRow: Int64); override;
    function DoLButtonDown(const AHitInfo: TMouseHitInfo): Boolean; override;
    function DoRadioCheckClick(ARowIndex: Int64): Boolean;
    function GetColorMapClass: THexViewColorMapClass; override;
    function GetRawDataClass: TRawDataClass; override;
    procedure HandleUserInputJump(ARowIndex: Int64);
    procedure InitPainters; override;
    function IsJumpValid(AJmpToAddr: Int64): Boolean; virtual;
    function InternalGetRowPainter(ARowIndex: Int64): TAbstractPrimaryRowPainter; override;
    function RawData: TMappedRawData; {$ifndef fpc} inline; {$endif}
    procedure UpdateDataMap; override;
  protected
    property JmpData: TObjectDictionary<Int64, TList<Int64>> read FJmpData;
    property JmpInitList: TList<Int64> read FJmpInitList;
    property MaskTextMetric: TMaskTextMetric read FMaskTextMetric;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearDataMap;
    function RowStyle(ARowIndex: Int64): TRowStyle;
    property DataMap: TDataMap read FDataMap;
    property Pages: TVirtualPages read FPages;
  protected
    property AddressToRowIndexMode: TAddressToRowIndexMode read FAddressToRowIndexMode write FAddressToRowIndexMode default armFindFirstRaw;
    property ColorMap: TMapViewColors read GetColorMap write SetColorMap stored IsColorMapStored;
    property DrawIncomingJmp: Boolean read FDrawIncomingJmp write SetDrawIncomingJmp default False;
    property ShowMaskAsValue: Boolean read FShowMaskAsValue write SetShowMaskAsValue default False;
    property OnRadioCheckClick: TRadioCheckClickEvent read FRadioCheckClick write FRadioCheckClick;
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
    property ShowMaskAsValue;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property WheelMultiplier;
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
    property OnHint;
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
    property OnRadioCheckClick;
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

    if View.AddressToRowIndexMode = armFindFirstAny then
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
    LinesBetween := Offset div View.BytesInRow;
    Result := FRows.List[RawRowIndex].RowIndex + LinesBetween;
  end;
end;

function TMappedRawData.BitIndex: Byte;
begin
  Result := GetItem.BitIndex;
end;

procedure TMappedRawData.CheckDataMap;
var
  I: Integer;
  LastAddr, NextAddr: Int64;
  LastRowIndex: Integer;
  Data: TListEx<TMapRow>;
  ARow: TMapRow;
begin
  if View.DataMap.Data.Count = 0 then Exit;
  LastAddr := 0;
  NextAddr := 0;
  LastRowIndex := 0;
  Data := View.DataMap.Data;
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
    Result.RawLength := Min(View.BytesInRow, Result.RawLength);
  end
  else
  begin
    Result := FRows[RawRowIndex];
    LinesBetween := RowIndex - Result.RowIndex;
    Offset := LinesBetween * View.BytesInRow;
    Inc(Result.RowIndex, LinesBetween);
    Inc(Result.Address, Offset);
    Inc(Result.DataOffset, Offset);
    Dec(Result.RawLength, Offset);
    Result.RawLength := Min(View.BytesInRow, Result.RawLength);
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

function TMappedRawData.View: TCustomMappedHexView;
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
  ServiceLineProcessing: Boolean;

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

    if View.Pages[PageIndex].ShowCaption then
    begin
      Line.Style := rsNone;
      Line.Address := PageAddress;
      Line.DataOffset := StreamOffset;
      Line.RawLength := 0;
      Line.Description := '';
      Line.Comment := '';
      Line.Color := View.ColorMap.TextColor;
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
      Line.Description := View.Pages[PageIndex].Caption;
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

  procedure UpdateServiceLineProcessing;
  begin
    if (StreamOffset = DataStreamSize) or (PageOffset = PageSize) then
    begin
      if MapIndex < Data.Count then
      begin
        UpdateLocalMapRow;
        ServiceLineProcessing := AMapRow.Address = (PageAddress + PageOffset);
      end
      else
        ServiceLineProcessing := False;
    end;
  end;

begin
  FCount := 0;
  StreamOffset := 0;
  FillChar(Line, SizeOf(Line), 0);
  FRows.Clear;
  View.JmpInitList.Clear;

  CheckDataMap;

  PageIndex := -1;
  PageAddress := View.StartAddress;
  if View.Pages.Count = 0 then
    PageSize := View.GetDataStreamSize
  else
  begin
    if View.Pages[0].VirtualAddress > View.StartAddress then
      PageSize := View.Pages[0].VirtualAddress - View.StartAddress
    else
    begin
      PageIndex := 0;
      PageSize := View.Pages[0].Size;
      AddPageDelimiter;
    end;
  end;

  PageOffset := 0;
  DataStreamSize := View.GetDataStreamSize;
  MapIndex := 0;
  BytesInRow := View.BytesInRow;
  Data := View.DataMap.Data;
  LastPageIsRaw := False;
  LastMaskIndex := -1;
  AMapRow := Default(TMapRow);
  ServiceLineProcessing := False;
  while PageIndex < View.Pages.Count do
  begin

    while ((StreamOffset < DataStreamSize) and (PageOffset < PageSize)) or
      ServiceLineProcessing do
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
        Line.Color := View.ColorMap.TextColor;
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
            UpdateServiceLineProcessing;
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
                View.JmpInitList.Add(FCount);
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
                UpdateServiceLineProcessing;
                Continue;
              end;
              Line.MaskRowIndex := FRows.List[LastMaskIndex].RowIndex;
              Line.Address := FRows.List[LastMaskIndex].Address;
              Line.BitIndex := AMapRow.BitIndex;
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
      UpdateServiceLineProcessing;
    end;
    Inc(PageIndex);
    if PageIndex < View.Pages.Count then
    begin
      PageAddress := View.Pages[PageIndex].VirtualAddress;
      AddPageDelimiter;

      // вместе с последней страницей выводим все что в неё не вошло

      // along with the last page, we output everything not included in it.

      if PageIndex = View.Pages.Count - 1 then
      begin
        if DataStreamSize > StreamOffset then
          PageSize := DataStreamSize - StreamOffset
        else
          raise Exception.CreateFmt(
            'Unexpected StreamOffset (%d) with total size %d',
            [StreamOffset, DataStreamSize]);
      end
      else
        PageSize := View.Pages[PageIndex].Size;
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

function TDataMap.AddMaskCheck(BitIndex: Byte; const Description,
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
  LineData.BitIndex := BitIndex;
  LineData.Checked := Checked;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddMaskRadio(BitIndex: Byte; const Description,
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
  LineData.BitIndex := BitIndex;
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
    if (DataLength > 0) and not FOwner.Pages.CheckAddrInPages(Address) then
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

function DefaultMapRowComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} A, B: TMapRow): Integer;
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
  {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Item: TMapRow;
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
  AColumn: TColumnType): Integer;
begin
  if AColumn = ctDescription then
    Result := 32
  else
    Result := inherited;
end;

function TPrimaryMappedRowPainter.CharCount(AColumn: TColumnType): Integer;
begin
  if AColumn = ctDescription then
    Result := 0
  else
    Result := inherited;
end;

function TPrimaryMappedRowPainter.ColumnAsString(AColumn: TColumnType): string;
begin
  if AColumn = ctDescription then
    Result := RawData[RowIndex].Description
  else
    Result := inherited;
end;

constructor TPrimaryMappedRowPainter.Create(AOwner: TFWCustomHexView);
begin
  if AOwner is TCustomMappedHexView then
    inherited
  else
    raise Exception.CreateFmt(SInvalidOwnerClass, [ClassName, AOwner.ClassName]);
end;

procedure TPrimaryMappedRowPainter.DrawColorGroup(ACanvas: TCanvas;
  var ARect: TRect);
begin
  // для размапленных данных отрисовка цветовых групп избыточна

  // color group rendering is redundant for unmapped data
end;

procedure TPrimaryMappedRowPainter.DrawDataPart(ACanvas: TCanvas;
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
end;

function TPrimaryMappedRowPainter.FormatRowColumn(AColumn: TColumnType;
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

function TPrimaryMappedRowPainter.RawData: TMappedRawData;
begin
  Result := TMappedRawData(inherited RawData);
end;

{ TRowWithExDescription }

function TRowWithExDescription.ByteViewMode: TByteViewMode;
begin
  Result := bvmHex8;
end;

function TRowWithExDescription.CaretEditMode(
  AColumn: TColumnType): TCaretEditMode;
begin
  if AColumn = ctOpcode then
    Result := inherited
  else
    Result := cemDisabled;
end;

constructor TRowWithExDescription.Create(AOwner: TFWCustomHexView);
begin
  inherited;

  // дестроить не нужно - метрика зарегистрируется в общем списке

  // no destroying is necessary - the metric will be registered in the common list

  FTextMetric := GetTextMetricClass.Create(AOwner);
end;

procedure TRowWithExDescription.DrawDataPart(ACanvas: TCanvas;
  var ARect: TRect);
var
  R: TRect;
  DataString: string;
begin
  inherited;

  if RawData[RowIndex].LinkLength > 0 then
  begin
    DataString := RawData[RowIndex].Description;
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

procedure TRowWithExDescription.GetHitInfo(var AHitInfo: TMouseHitInfo);
var
  ABounds: TBoundaries;
begin
  if AHitInfo.SelectPoint.Column = ctDescription then
  begin
    if GetLinkBoundaries(ABounds) then
    begin
      Inc(ABounds.LeftOffset, AHitInfo.ColumnStart + TextMargin);
      if AHitInfo.ScrolledCursorPos.X >= ABounds.LeftOffset then
        if AHitInfo.ScrolledCursorPos.X < (ABounds.LeftOffset + ABounds.Width) then
          AHitInfo.Cursor := crHandPoint;
    end;
  end
  else
    inherited;
end;

function TRowWithExDescription.GetLineJmpMarkRect(const ARect: TRect): TRect;
var
  ABounds: TBoundaries;
begin
  if GetLinkBoundaries(ABounds) then
    Result := MakeSelectRect(ARect.Left + ABounds.LeftOffset, ARect.Top, ABounds.Width)
  else
    Result := TRect.Empty;
end;

function TRowWithExDescription.GetLinkBoundaries(out ABounds: TBoundaries): Boolean;
begin
  ABounds.LeftOffset := RawData[RowIndex].LinkStart * CharWidth;
  ABounds.Width := RawData[RowIndex].LinkLength * CharWidth;
  if ABounds.LeftOffset + ABounds.Width + DblSize(TextMargin) > ColumnWidth[ctDescription] then
    ABounds.Width := ColumnWidth[ctDescription] - DblSize(TextMargin) - ABounds.LeftOffset;
  Result := ABounds.Width > 0;
end;

function TRowWithExDescription.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TFixedHexByteTextMetric;
end;

function TRowWithExDescription.TextMetric: TAbstractTextMetric;
begin
  Result := FTextMetric;
end;

{ TMaskTextMetric }

function TMaskTextMetric.ByteViewMode: TByteViewMode;
begin
  if TCustomMappedHexView(Owner).ShowMaskAsValue then
    Result := FByteViewMode
  else
    Result := bvmHex8;
end;

constructor TMaskTextMetric.Create(AOwner: TFWCustomHexView);
begin
  if AOwner is TCustomMappedHexView then
    inherited
  else
    raise Exception.CreateFmt(SInvalidOwnerClass, [ClassName, AOwner.ClassName]);
end;

procedure TMaskTextMetric.SetRawLength(const Value: Integer);
var
  NewByteViewMode: TByteViewMode;
begin
  FRawLength := Value;
  case Value of
    1: NewByteViewMode := bvmHex8;
    2: NewByteViewMode := bvmHex16;
    3, 4: NewByteViewMode := bvmHex32;
  else
    NewByteViewMode := bvmHex64;
  end;
  if NewByteViewMode <> FByteViewMode then
  begin
    FByteViewMode := NewByteViewMode;
    Update;
  end;
end;

{ TRowMask }

function TRowMask.ByteViewMode: TByteViewMode;
begin
  Result := TMaskTextMetric(TextMetric).ByteViewMode;
end;

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
      ARect.Left + TextMetric.SelectionLength(ctOpcode, 0, Data.RawLength - 1) - MulDiv(AWidth, 3, 2) + AWidth,
      ARect.Top + 1);
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
      ARect.Left + TextMetric.SelectionLength(ctOpcode, 0, Data.RawLength - 1),
      ARect.Top + 1);
    if R.Right >= ARect.Right then Exit;
    P := R.TopLeft;
    Dec(P.Y, AWidth div 2);
    DrawArrow(ACanvas, sdRight, P, AWidth);
  end;
end;

procedure TRowMask.GetHitInfo(var AHitInfo: TMouseHitInfo);
var
  AWidth, LeftOffset: Integer;
begin
  inherited;
  if AHitInfo.SelectPoint.Column = ctOpcode then
  begin
    AWidth := ToDpi(3);
    LeftOffset := AHitInfo.ColumnStart +
      TextMetric.SelectionLength(ctOpcode, 0, RawData[RowIndex].RawLength - 1) + AWidth;
    if (AHitInfo.ScrolledCursorPos.X >= LeftOffset) and
      (AHitInfo.ScrolledCursorPos.X <= LeftOffset + RowHeight + AWidth) then
      AHitInfo.Cursor := crHandPoint;
  end;
end;

procedure TRowMask.RowChanged;
begin
  TMaskTextMetric(TextMetric).RawLength := RawData[RowIndex].RawLength;
end;

function TRowMask.TextMetric: TAbstractTextMetric;
begin
  Result := TCustomMappedHexView(Owner).MaskTextMetric;
end;

{ TRowAssembler }

function TRowAssembler.CaretEditMode(AColumn: TColumnType): TCaretEditMode;
begin
  Result := cemDisabled;
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
  DrawText(ACanvas, ADescription,
    Length(ADescription), ARect, DT_CALCRECT);
  ACanvas.FillRect(ARect);
  CorrectCanvasFont(ACanvas, AColumn);
  DrawText(ACanvas, ADescription,
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
  AColumn: TColumnType): Integer;
begin
  if AColumn = ctDescription then
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
  CheckRect := GetCheckRect(ARect);
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
  CheckRect := GetCheckRect(ARect);
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

function TRowCheckRadioMask.GetCheckRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, ARect.Left +
    Min(RowHeight, ToDpi(13)), ARect.Top + Min(RowHeight, ToDpi(13)));
  OffsetRect(Result, 0, (RowHeight - Result.Height) div 2);
end;

procedure TRowCheckRadioMask.GetHitInfo(var AMouseHitInfo: TMouseHitInfo);
var
  R: TRect;
begin
  inherited;
  if AMouseHitInfo.SelectPoint.Column = ctDescription then
  begin
    R := GetColumnRect(ctDescription, RowIndex);
    InflateRect(R, -TextMargin, 0);
    if R.IsEmpty then Exit;
    R := GetCheckRect(R);
    if PtInRect(R, AMouseHitInfo.CursorPos) then
      AMouseHitInfo.Flags := AMouseHitInfo.Flags or FLAG_RADIOCHECK;
  end;
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

function TCommonMapViewPostPainter.View: TCustomMappedHexView;
begin
  Result := TCustomMappedHexView(inherited Owner);
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
    I, JmpLine, JmpToAddr: Int64;
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
    I := StartRow;
    while I <= EndRow do
    begin

      // последняя строка может быть невидимой (часть затерта скролом)

      // the last line can be invisible (part of it hide by scroll-ctrl)

      if (I = EndRow) and not RowVisible(I) then Exit;

      if RawData[I].Style in [rsRawWithExDescription, rsAsm] then
      begin
        JmpToAddr := RawData[I].JmpToAddr;
        if JmpToAddr <> 0 then
        begin
          JmpLine := View.CalculateJmpToRow(I);
          if (JmpLine >= 0) or View.IsJumpValid(JmpToAddr) then
          begin
            Param.DirectionDown := JmpLine > I;
            Param.RowFrom := I;
            Param.RowTo := JmpLine;
            Param.SecondDraw := False;
            DrawLine(ACanvas, Param);
            Inc(PaintedLinesCount);
          end;
        end;
      end;

      if not View.DrawIncomingJmp then
      begin
        Inc(I);
        Continue;
      end;

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

      Inc(I);
    end;
  end;

begin
  if ctJmpLine in Columns then
  begin
    Process(False, View.JmpData);
    Process(True, View.JmpData);
  end;
end;

{ TCheckRadioRowPostPainter }

procedure TCheckRadioRowPostPainter.DrawArrowPart(ACanvas: TCanvas;
  RowIndex: Int64; var Offset: TPoint; DrawOnlySelectedArrow: Boolean);
var
  StartPoint, EndPoint: TPoint;
  Selected: Boolean;
  ArrowSize, AMaskIndex, ABitIndex, ACharIndex: Integer;
begin
  Selected := GetSelectData(RowIndex).SelectStyle <> ssNone;

  if not Selected and DrawOnlySelectedArrow then Exit;

  if Selected then
    ACanvas.Pen.Color := TMapViewColors(ColorMap).ArrowDownSelectedColor
  else
    ACanvas.Pen.Color := TMapViewColors(ColorMap).ArrowDownColor;

  ABitIndex := RawData[RowIndex].BitIndex;
  ACharIndex := ABitIndex shr 2;
  AMaskIndex := RawData.MaskRowIndex;
  if View.ShowMaskAsValue then
    ACharIndex := RawData[AMaskIndex].RawLength shl 1 - ACharIndex - 1
  else
    ACharIndex := ACharIndex xor 1;

  StartPoint.X :=
    Offset.X +
    TextMargin +
    CharWidth shr 1 + // центрирование по символу
                      // character centering
    TextMetric.CharLength(ctOpcode, 0, ACharIndex - 1);

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

function TCheckRadioRowPostPainter.TextMetric: TAbstractTextMetric;
begin
  Result := TCustomMappedHexView(Owner).MaskTextMetric;
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

function VirtualPagesComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} A, B: TVirtualPage): Integer;
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

{ TCustomMappedHexView }

function TCustomMappedHexView.CalculateColumnBestSize(
  Value: TColumnType): Integer;

  function GetRowCustomString: string;
  begin
    if Value = ctDescription then
      Result := RawData.Description
    else
      Result := RawData.Comment;
  end;

var
  I, ARowIndex: Integer;
  Painter: TAbstractPrimaryRowPainter;
  ATextMetric: TAbstractTextMetric;
begin
  case Value of
    ctWorkSpace: Result := ToDpi(32);
    ctJmpLine: Result := ToDpi(82);
    ctOpcode:
    begin
      Result := ToDpi(255);
      for I := 0 to RawData.PresentRows.Count - 1 do
      begin
        ARowIndex := RawData.PresentRows.List[I].RowIndex;
        Painter := GetRowPainter(ARowIndex);
        if Painter <> nil then
        begin
          ATextMetric := TPrimaryMappedRowPainter(Painter).TextMetric;
          Result := Max(Result, ATextMetric.SelectionLength(Value, 0,
            RawData[ARowIndex].RawLength - 1));
        end;
      end;
      Inc(Result, TextMargin shl 1);
    end;
    ctDescription, ctComment:
    begin
      Result := inherited;
      for I := 0 to RawData.PresentRows.Count - 1 do
      begin
        ARowIndex := RawData.PresentRows.List[I].RowIndex;
        if RawData[ARowIndex].RawLength > 0 then
          Result := Max(Result, Length(GetRowCustomString) * CharWidth + TextMargin shl 1);
      end;
    end;
  else
    Result := inherited;
  end;
end;

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
    if (RowIndex >= 0) and (RawData[RowIndex].JmpToAddr > 0) then
    begin
      HandleUserInputJump(RowIndex);
      Exit;
    end;
  end;
  inherited;
end;

procedure TCustomMappedHexView.DoGetHint(var AHintParam: THintParam;
  var AHint: string);
var
  AddrVA: Int64;
  Painter: TAbstractPrimaryRowPainter;
  ABounds: TBoundaries;
begin
  if AHintParam.MouseHitInfo.SelectPoint.Column <> ctDescription then Exit;
  if PtInRect(FLastInvalidAddrRect, AHintParam.MouseHitInfo.CursorPos) then Exit;
  AddrVA := RawData[AHintParam.MouseHitInfo.SelectPoint.RowIndex].JmpToAddr;
  if AddrVA = 0 then
  begin
    FLastInvalidAddrRect := AHintParam.HintInfo.CursorRect;
    Exit;
  end;
  Painter := GetRowPainter(AHintParam.MouseHitInfo.SelectPoint.RowIndex);
  if Assigned(Painter) and (Painter is TRowWithExDescription) then
  begin
    AHintParam.AddrVA := AddrVA;
    if TRowWithExDescription(Painter).GetLinkBoundaries(ABounds) then
    begin
      AHintParam.HintInfo.CursorRect.Left :=
        AHintParam.MouseHitInfo.ColumnStart + TextMargin + ABounds.LeftOffset;
      AHintParam.HintInfo.CursorRect.Width := ABounds.Width;
      if not PtInRect(AHintParam.HintInfo.CursorRect, AHintParam.MouseHitInfo.CursorPos) then Exit;
      FLastInvalidAddrRect := TRect.Empty;
      inherited;
    end;
  end;
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
  FJmpInitList := TList<Int64>.Create;
  FJmpData := TObjectDictionary<Int64, TList<Int64>>.Create([doOwnsValues]);
end;

destructor TCustomMappedHexView.Destroy;
begin
  FJmpData.Free;
  FJmpInitList.Free;
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
  begin
    I := AStartRow;
    while I <= AEndRow do
    begin
      with RawData[I] do
        if (JmpToAddr <> 0) or Linked then
        begin
          Invalidate;
          Exit;
        end;
      Inc(I);
    end;
  end;

  inherited;
end;

function TCustomMappedHexView.DoLButtonDown(const AHitInfo: TMouseHitInfo): Boolean;
begin
  Result := AHitInfo.Cursor = crHandPoint;
  if Result then
    HandleUserInputJump(MousePressedHitInfo.SelectPoint.RowIndex)
  else
    if MousePressedHitInfo.Flags and FLAG_RADIOCHECK <> 0 then
      Result := DoRadioCheckClick(AHitInfo.SelectPoint.RowIndex);
end;

function TCustomMappedHexView.DoRadioCheckClick(ARowIndex: Int64): Boolean;
begin
  Result := Assigned(FRadioCheckClick);
  if Result then
    FRadioCheckClick(Self, ARowIndex);
end;

function TCustomMappedHexView.GetColorMap: TMapViewColors;
begin
  Result := TMapViewColors(inherited ColorMap);
end;

function TCustomMappedHexView.GetColorMapClass: THexViewColorMapClass;
begin
  Result := TMapViewColors;
end;

function TCustomMappedHexView.GetRawDataClass: TRawDataClass;
begin
  Result := TMappedRawData;
end;

procedure TCustomMappedHexView.HandleUserInputJump(ARowIndex: Int64);
begin
  if RawData[ARowIndex].Style = rsMask then
  begin
    DataMap.Data.List[RawData.MapRowIndex].Expanded := not RawData.Expanded;
    ClearSelection;
    UpdateDataMap;
    UpdateTextBoundary;
    UpdateScrollPos;
    Invalidate;
  end
  else
    JumpToAddress(RawData[ARowIndex].JmpToAddr);
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

  FMaskTextMetric := TMaskTextMetric.Create(Self);
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

function TCustomMappedHexView.IsJumpValid(AJmpToAddr: Int64): Boolean;
begin
  Result := AddressToRowIndex(AJmpToAddr) >= 0;
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

procedure TCustomMappedHexView.SetShowMaskAsValue(const Value: Boolean);
begin
  if ShowMaskAsValue <> Value then
  begin
    FShowMaskAsValue := Value;
    DoChange(cmByteViewMode);
  end;
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

end.
