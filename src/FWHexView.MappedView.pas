////////////////////////////////////////////////////////////////////////////////
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
    rsRaw,
    rsRawCustom,
    rsRawWithExDescription,
    rsAsm,
    rsMask,
    rsMaskCheck,
    rsMaskRadio,
    rsMaskSeparator,
    rsLineComment,
    rsRegion);

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
      3: (
        // for Style = rsRegion
        RegionStart: Boolean;
        RegionSizeOrAddr: Int64;
        RegionIndex: Integer;
      );
  end;

  TRegion = class;
  TCustomMappedHexView = class;

  { TMappedRawData }

  TMappedRawData = class(TRawData)
  strict private
    FRows: TListEx<TRowData>;
    FRowWithData: TList<Int64>;
    FCount: Int64;
    FMaxRegionLevel: Integer;
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
    // for Style = rsRegion
    function Region: TRegion;
    function RegionIndex: Integer;
    function RegionSizeOrAddr: Int64;
    function RegionStart: Boolean;

    function Count: Int64; override;
    procedure Clear; override;
    procedure Update; override;

    property MaxRegionLevel: Integer read FMaxRegionLevel;
    property Row[ARowIndex: Int64]: TMappedRawData read GetRowAtIndex; default;
  end;

  TMapRow = record
    Index: Integer;
    Style: TRowStyle;
    Address: Int64;
    RawLength: Int64;
    Description: string;
    DrawRowSmallSeparator: Boolean;
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
        RegionStart: Boolean;
        RegionSizeOrAddr: Int64;
        RegionIndex: Integer;
      );
  end;

  TRegionDrawStyle = (rdsComment, rdsSeparator);

  TAddrCheck = (
    acStartOutOfPool,
    acEndOutOfPool,
    acStartOutOfSpace,
    acEndOutOfSpace,
    acIntersect,
    acChecked);

  TDataMap = class
  private type
    TRegionCompareResult = (rcrError, rcrNotMatching, rcrPartialIntersectTop,
      rcrPartialIntersectBottom, rcrSame, rcrNested, rcrFullNested, rcrExternal);
  strict private
    FOwner: TCustomMappedHexView;
    FData: TListEx<TMapRow>;
    FRawIndex: TDictionary<Int64, Int64>;
    FRegions: TObjectList<TRegion>;
    FHiddenFooter: TObjectDictionary<Int64, TList<TRegion>>;
    FMaskPresent: Boolean;
    FUpdateCount: Integer;
    FCurrentAddr, FSavedCurrentAddr: Int64;
    function AddMapLine(Value: TMapRow): Integer;
    procedure DataChange(Sender: TObject;
      {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} {%H-}Item: TMapRow;
      {%H-}Action: TCollectionNotification);
  protected type
    TRegionChangeType = (rctCollapse, rctExpand, rctToggle);
  protected
    function CompareRegion(AAddress, ASize: Int64; ARegion: TRegion): TRegionCompareResult;
    procedure InternalClear(NewStartAddress: Int64);
    procedure InternalRegionChange(ARegion: TRegion; AChangeType: TRegionChangeType);
    procedure InternalRegionChangeByAddress(Address: Int64; AChangeType: TRegionChangeType);
    function FindRegionLevel(Address, RegionSize: Int64): Integer;
    procedure RebuildDataMap;
    property CurrentAddr: Int64 read FCurrentAddr write FCurrentAddr;
    property Data: TListEx<TMapRow> read FData;
    property Regions: TObjectList<TRegion> read FRegions;
    property HiddenFooter: TObjectDictionary<Int64, TList<TRegion>> read FHiddenFooter;
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

    function AddRaw(const Comment: string): Integer; overload;
    function AddRaw(DataLength: Int64): Integer; overload;
    function AddRaw(DataLength: Int64; const Comment: string): Integer; overload;
    function AddRaw(Address, DataLength: Int64): Integer; overload;
    function AddRaw(Address, DataLength: Int64; const Comment: string): Integer; overload;

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

    function AddRegion(Address, RegionSize: Int64;
      const Description: string; Expanded: Boolean = True): TRegion; overload;
    function AddRegion(RegionSize: Int64;
      const Description: string; Expanded: Boolean = True): TRegion; overload;
    function AddRegion(Address, RegionSize: Int64; const Description: string;
      DrawStyle: TRegionDrawStyle; Expanded: Boolean = True): TRegion; overload;
    function AddRegion(RegionSize: Int64; const Description: string;
      DrawStyle: TRegionDrawStyle; Expanded: Boolean = True): TRegion; overload;

    function AddLine(Address: Int64): Integer; overload;
    function AddLine: Integer; overload;

    function AddNone(Address: Int64): Integer; overload;
    function AddNone: Integer; overload;

    procedure Assign({%H-}Value: TDataMap);
    procedure BeginUpdate;
    procedure EndUpdate;

    function GetCurrentAddr: Int64;
    procedure SaveCurrentAddr;
    procedure RestoreCurrentAddr;

    procedure CollapseAllRegions;
    procedure CollapseRegion(Address: Int64);
    procedure ExpandAllRegions;
    procedure ExpandRegion(Address: Int64);
    procedure ToggleRegion(Address: Int64);

    // modification of existing elements

    function LastDataMapIndex: Integer;
    procedure SetRowColor(ADataMapIndex: Integer; AValue: TColor);
    procedure SetRowComment(ADataMapIndex: Integer; const AValue: string);
    procedure SetRowDescription(ADataMapIndex: Integer; const AValue: string);
    procedure SetRowLineSeparated(ADataMapIndex: Integer; AValue: Boolean);
  end;

  TRegionPart = class
  strict private
    FOwner: TRegion;
    FAlignment: TAlignment;
    FBackgroundColor: TColor;
    FDataMapIndex, FRowIndex: Int64;
    FDrawStyle: TRegionDrawStyle;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetComment(const Value: string);
    procedure SetDrawStyle(const Value: TRegionDrawStyle);
    procedure SetText(const Value: string);
    procedure SetTextColor(const Value: TColor);
    function GetComment: string;
    function GetText: string;
    function GetTextColor: TColor;
  protected
    procedure Assign(AValue: TRegionPart);
    property DataMapIndex: Int64 read FDataMapIndex write FDataMapIndex;
    property RowIndex: Int64 read FRowIndex write FRowIndex;
  public
    constructor Create(AOwner: TRegion; ADataMapIndex: Int64);
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property DrawStyle: TRegionDrawStyle read FDrawStyle write SetDrawStyle;
    property Comment: string read GetComment write SetComment;
    property Text: string read GetText write SetText;
    property TextColor: TColor read GetTextColor write SetTextColor;
  end;

  TRegion = class
  strict private
    FOwner: TDataMap;
    FAddress, FSize: Int64;
    FExpanded, FFooterVisible: Boolean;
    FHeader, FFooter: TRegionPart;
    FLevel: Integer;
    FLineColor: TColor;
    FLineStyle: TPenStyle;
    FLineVisible: Boolean;
    FLineWidth: Integer;
    procedure SetExpanded(const Value: Boolean);
    procedure SetFooterVisible(const Value: Boolean);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineStyle(const Value: TPenStyle);
    procedure SetLineVisible(const Value: Boolean);
    procedure SetLineWidth(const Value: Integer);
  protected
    procedure Assign(AValue: TRegion);
    procedure DoChange;
    property DataMap: TDataMap read FOwner;
    property Level: Integer read FLevel write FLevel;
  public
    constructor Create(AOwner: TDataMap; AHeaderIndex, AAddress, ASize: Int64);
    destructor Destroy; override;
    function AddressInRegion(AAddress: Int64): Boolean;
    function FirstRawDataRowIndex: Integer;
    function LastRawDataRowIndex: Integer;
    property Address: Int64 read FAddress;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Header: TRegionPart read FHeader;
    property Footer: TRegionPart read FFooter;
    property FooterVisible: Boolean read FFooterVisible write SetFooterVisible;
    property LineColor: TColor read FLineColor write SetLineColor;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle;
    property LineVisible: Boolean read FLineVisible write SetLineVisible;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
    property Size: Int64 read FSize;
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

  TRowRegion = class(TSecondaryMappedRowPainter)
  protected
    procedure CopyRowAsString(Builder: TSimplyStringBuilder); override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    procedure GetHitInfo(var AHitInfo: TMouseHitInfo); override;
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

  TRegionPostPainter = class(TCommonMapViewPostPainter)
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
    FSize: Integer;
  private
    procedure DoChange;
    procedure SetCaption(const Value: string);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetSize(const Value: Integer);
    procedure SetVirtualAddress(const Value: Int64);
  protected
    property Owner: TCustomMappedHexView read FOwner write FOwner;
  public
    property Caption: string read FCaption write SetCaption;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property VirtualAddress: Int64 read FVirtualAddress write SetVirtualAddress;
    property Size: Integer read FSize write SetSize;
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
    function CheckAddrInPages(VirtualAddress: Int64; EmptyData: Boolean): Boolean;
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
    FCoveringRegion: TList<TRegion>;
    function GetColorMap: TMapViewColors;
    procedure SetColorMap(const Value: TMapViewColors);
    procedure SetDrawIncomingJmp(const Value: Boolean);
    procedure SetShowMaskAsValue(const Value: Boolean);
    procedure UpdateJumpList;
  protected
    function CalculateColumnBestSize(Value: TColumnType): Integer; override;
    function CalculateJmpToRow(JmpFromRow: Int64): Int64; virtual;
    procedure DblClick; override;
    procedure DoBeforePostPaint(const ADiapason: TVisibleRowDiapason); override;
    procedure DoCaretKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoColumnWidthChange(AColumnType: TColumnType; var AWidth: Integer); override;
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
    property CoveringRegion: TList<TRegion> read FCoveringRegion;
    property JmpData: TObjectDictionary<Int64, TList<Int64>> read FJmpData;
    property JmpInitList: TList<Int64> read FJmpInitList;
    property MaskTextMetric: TMaskTextMetric read FMaskTextMetric;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearDataMap;
    procedure FocusOnAddress(Address: Int64; ACaretChangeMode: TCaretChangeMode); override;
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
    property AutoScrollType;
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
    property OnEdit;
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
  Reg: TRegion;
  MaxRegionLevel: Integer;
begin
  Result := -1;
  if Count = 0 then Exit;
  RawRowIndex := GetRawIndexByAddr(Value);
  try
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

      if Value < FRows.List[RawRowIndex].Address then
      begin
        Result := -1;
        Exit;
      end;

      // здесь окажемся в случае попадания в блок не размапленных данных,
      // в этом случае надо просто рассчитать строку по формуле

      // here we will find ourselves in the case of non-unmapped data getting
      // into the block, in this case we just need to calculate the row by the formula

      Offset := Value - FRows.List[RawRowIndex].Address;
      LinesBetween := Offset div View.BytesInRow;
      Result := FRows.List[RawRowIndex].RowIndex + LinesBetween;
    end;
  finally
    if Result < 0 then
    begin
      MaxRegionLevel := 0;
      for Reg in View.DataMap.Regions do
        if not Reg.Expanded and Reg.AddressInRegion(Value) then
        begin
          if MaxRegionLevel < Reg.Level then
          begin
            MaxRegionLevel := Reg.Level;
            Result := Reg.Header.RowIndex;
          end;
        end;
    end;
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
  Region: TRegion;
begin
  if View.DataMap.Data.Count = 0 then Exit;
  LastAddr := 0;
  NextAddr := 0;
  LastRowIndex := 0;
  FMaxRegionLevel := 0;
  Data := View.DataMap.Data;
  Data.Sort;
  for I := 0 to Data.Count - 1 do
  begin
    ARow := Data[I];
    if (NextAddr <> 0) and (ARow.Address < NextAddr) then
      raise Exception.CreateFmt(
        'Data map address 0x%x (%d) intersects with the block at 0x%x (%d)',
        [ARow.Address, ARow.Index, LastAddr, LastRowIndex]);
    if ARow.Style = rsRegion then
    begin
      Region := View.DataMap.Regions[ARow.RegionIndex];
      Region.Level := View.DataMap.FindRegionLevel(Region.Address, Region.Size);
      FMaxRegionLevel := Max(FMaxRegionLevel, Region.Level);
      if ARow.RegionStart then
        Region.Header.DataMapIndex := I
      else
        Region.Footer.DataMapIndex := I;
    end;
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
  FRowWithData := TList<Int64>.Create;
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
  FRowWithData.Free;
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
  FRight := FRowWithData.Count - 1;
  FCurrent := (FRight + FLeft) div 2;
  if FRows.List[FRowWithData[FLeft]].Address > AAddr then
  begin
    Result := FRowWithData[FLeft];
    Exit;
  end;
  if FRows.List[FRowWithData[FRight]].Address < AAddr then
  begin
    Result := FRowWithData[FRight];
    Exit;
  end;
  repeat
    if FRows.List[FRowWithData[FCurrent]].Address = AAddr then
    begin
      Result := FRowWithData[FCurrent];
      Exit;
    end;
    if FRows.List[FRowWithData[FCurrent]].Address < AAddr then
      FLeft := FCurrent
    else
      FRight := FCurrent;
    FCurrent := (FRight + FLeft) div 2;
  until FLeft = FCurrent;
  if FRows.List[FRowWithData[FCurrent]].Address < AAddr then
    if FCurrent < FRowWithData.Count - 1 then
      if FRows.List[FRowWithData[FCurrent + 1]].Address = AAddr then
        Inc(FCurrent);
  Result := FRowWithData[FCurrent];
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
    if FCurrent < FRows.Count - 1 then
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

function TMappedRawData.Region: TRegion;
var
  ARowData: TRowData;
begin
  ARowData := GetItem;
  if ARowData.Style <> rsRegion then Exit(nil);
  Result := View.DataMap.Regions[ARowData.RegionIndex];
end;

function TMappedRawData.RegionIndex: Integer;
begin
  Result := GetItem.RegionIndex;
end;

function TMappedRawData.RegionSizeOrAddr: Int64;
begin
  Result := GetItem.RegionSizeOrAddr;
end;

function TMappedRawData.RegionStart: Boolean;
begin
  Result := GetItem.RegionStart;
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
  Region: TRegion;
  Regions: TObjectList<TRegion>;
  HiddenFooters: TObjectDictionary<Int64, TList<TRegion>>;
  HiddenFootersList: TList<TRegion>;
  LastPageIsRaw: Boolean;
  LastMaskIndex, LastRegionIndex: Int64;
  NestedRegionCount: Integer;
  AMapRow: TMapRow;
  ServiceLineProcessing: Boolean;

  procedure AddLine;
  var
    Idx: Integer;
  begin
    if LastPageIsRaw and (Line.Style = rsRaw) then
      Inc(FRows.List[FRows.Count - 1].RawLength, Line.RawLength)
    else
    begin
      Idx := FRows.Add(Line);
      if Line.RawLength > 0 then
        FRowWithData.Add(Idx);
    end;
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
        if (AMapRow.Style = rsRegion) and AMapRow.RegionStart then
          ServiceLineProcessing := False;
      end
      else
        ServiceLineProcessing := False;
    end;
  end;

begin
  FCount := 0;
  StreamOffset := 0;
  FillChar(Line, SizeOf(Line), 0);
  FRowWithData.Clear;
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
  Regions := View.DataMap.Regions;
  HiddenFooters := View.DataMap.HiddenFooter;
  HiddenFooters.Clear;
  LastPageIsRaw := False;
  LastMaskIndex := -1;
  LastRegionIndex := -1;
  NestedRegionCount := 0;
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

        // обработка свернутых регионов

        // processing of collapsed regions

        if LastRegionIndex >= 0 then
        begin
          if AMapRow.Style = rsRegion then
          begin
            if AMapRow.RegionStart then
              Inc(NestedRegionCount)
            else
            begin
              if NestedRegionCount = 0 then
                LastRegionIndex := -1
              else
                Dec(NestedRegionCount);
            end;
          end;
          Inc(StreamOffset, Line.RawLength);
          Inc(PageOffset, Line.RawLength);
          Inc(MapIndex);
          UpdateLocalMapRow;
          UpdateServiceLineProcessing;
          Continue;
        end;

        if (AMapRow.Address = Line.Address) or
          (AMapRow.Style in [rsMaskCheck..rsMaskSeparator]) then
        begin

          RawLength := Min(AMapRow.RawLength,
            PageSize - PageOffset);
          Line.Description := AMapRow.Description;
          Line.Comment := AMapRow.Comment;
          Line.Style := AMapRow.Style;
          Line.Color := AMapRow.Color;
          Line.DrawRowSmallSeparator := AMapRow.DrawRowSmallSeparator;

          case Line.Style of
            rsRaw, rsRawCustom, rsSeparator, rsLineComment:
              LastMaskIndex := -1;
            rsRegion:
            begin
              LastMaskIndex := -1;
              Line.RegionIndex := AMapRow.RegionIndex;
              Line.RegionSizeOrAddr := AMapRow.RegionSizeOrAddr;
              Line.RegionStart := AMapRow.RegionStart;
              Region := Regions[Line.RegionIndex];
              if Line.RegionStart then
              begin
                if not Region.Expanded then
                  LastRegionIndex := FRows.Count;
                Region.Header.RowIndex := Line.RowIndex;
              end
              else
              begin
                Region.Footer.RowIndex := Line.RowIndex;
                if not Region.FooterVisible then
                begin
                  if not HiddenFooters.TryGetValue(Line.RowIndex, HiddenFootersList) then
                  begin
                    HiddenFootersList := TList<TRegion>.Create;
                    HiddenFooters.Add(Line.RowIndex, HiddenFootersList);
                  end;
                  HiddenFootersList.Add(Region);
                  Inc(MapIndex);
                  UpdateLocalMapRow;
                  UpdateServiceLineProcessing;
                  Continue;
                end;
              end;
            end;
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

function TDataMap.AddLine: Integer;
begin
  Result := AddLine(CurrentAddr);
end;

function TDataMap.AddLine(Address: Int64): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsLine;
  LineData.Address := Address;
  Result := AddMapLine(LineData);
  CurrentAddr := Address;
end;

function TDataMap.AddMapLine(Value: TMapRow): Integer;
begin
  case CheckAddr(Value.Address, Value.RawLength) of
    acStartOutOfPool:
      raise Exception.CreateFmt('Line start address 0x%x, out of pages pool',
        [Value.Address]);
    acEndOutOfPool:
      raise Exception.CreateFmt('Line end address 0x%x, out of pages pool',
        [Value.Address + Value.RawLength]);
    acStartOutOfSpace:
      raise Exception.CreateFmt('Line start address 0x%x, out of space 0x%x',
        [Value.Address, FOwner.StartAddress]);
    acEndOutOfSpace:
      raise Exception.CreateFmt('Line end address 0x%x, out of space 0x%x',
        [Value.Address + Value.RawLength, FOwner.Pages.MaxAddrAware]);
    acIntersect:
      raise Exception.CreateFmt('DataMap address 0x%x already contains data',
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

function TDataMap.AddRaw(const Comment: string): Integer;
begin
  Result := AddRaw(CurrentAddr, FOwner.BytesInRow, Comment);
end;

function TDataMap.AddRaw(DataLength: Int64): Integer;
begin
  Result := AddRaw(CurrentAddr, DataLength);
end;

function TDataMap.AddRaw(DataLength: Int64; const Comment: string): Integer;
begin
  Result := AddRaw(CurrentAddr, DataLength, Comment);
end;

function TDataMap.AddRaw(Address, DataLength: Int64): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsRawCustom;
  LineData.Address := Address;
  LineData.RawLength := DataLength;
  LineData.Color := clDefault;
  CurrentAddr := Address + DataLength;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddRaw(Address, DataLength: Int64;
  const Comment: string): Integer;
var
  LineData: TMapRow;
begin
  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsRawCustom;
  LineData.Address := Address;
  LineData.RawLength := DataLength;
  LineData.Comment := Comment;
  CurrentAddr := Address + DataLength;
  Result := AddMapLine(LineData);
end;

function TDataMap.AddRegion(RegionSize: Int64;
  const Description: string; Expanded: Boolean): TRegion;
begin
  Result := AddRegion(CurrentAddr, RegionSize, Description, Expanded);
end;

function TDataMap.AddRegion(Address, RegionSize: Int64;
  const Description: string; Expanded: Boolean): TRegion;
begin
  Result := AddRegion(Address, RegionSize, Description, rdsComment, Expanded);
end;

function TDataMap.AddRegion(RegionSize: Int64; const Description: string;
  DrawStyle: TRegionDrawStyle; Expanded: Boolean): TRegion;
begin
  Result := AddRegion(CurrentAddr, RegionSize, Description, DrawStyle, Expanded);
end;

function TDataMap.AddRegion(Address, RegionSize: Int64;
  const Description: string; DrawStyle: TRegionDrawStyle;
  Expanded: Boolean): TRegion;
var
  LineData: TMapRow;
  LastRegionAddrVa: Int64;
  I, RegionPageIndex: Integer;
  RegionPage: TVirtualPage;
begin
  if RegionSize <= 0 then
    raise Exception.CreateFmt('Invalid region size (%d)', [RegionSize]);

  // регион не должен находится на разных страницах

  // the region cannot be located on different pages

  LastRegionAddrVa := Address + RegionSize;
  if FOwner.Pages.Count > 0 then
  begin
    if FOwner.Pages.GetPageIndex(Address, RegionPageIndex) = pirPagePresent then
    begin
      RegionPage := FOwner.Pages.GetItem(RegionPageIndex);
      if LastRegionAddrVa > RegionPage.VirtualAddress + RegionPage.Size then
        raise Exception.CreateFmt('The lower boundary of the new region (0x%x) is' +
          ' located outside the page boundaries [%d](0x%x-0x%x).',
        [
          LastRegionAddrVa,
          RegionPageIndex,
          RegionPage.VirtualAddress,
          RegionPage.VirtualAddress + RegionPage.Size
        ]);
    end;
  end;

  // регион не может пересекать границы других регионов.
  // он может либо полностью входить в существующий регион,
  // либо охватывать целиком уже существующий регион.

  // a region cannot cross the borders of other regions.
  // It can either be completely included in an existing region
  // or cover an entire existing region.

  for I := 0 to FRegions.Count - 1 do
  begin
    case CompareRegion(Address, RegionSize, FRegions[I]) of
      rcrNotMatching:;
      rcrNested:;
      rcrFullNested:;
      rcrExternal:;
      rcrPartialIntersectTop:
      begin
        raise Exception.CreateFmt('The lower boundary of the new region (0x%x) ' +
          'intersects with the existing region [%d](0x%x-0x%x).',
        [
          LastRegionAddrVa,
          I,
          FRegions[I].Address,
          FRegions[I].Address + FRegions[I].Size
        ]);
      end;
      rcrPartialIntersectBottom:
      begin
        raise Exception.CreateFmt('The upper  boundary of the new region (0x%x) ' +
          'intersects with the existing region [%d](0x%x-0x%x).',
        [
          Address,
          I,
          FRegions[I].Address,
          FRegions[I].Address + FRegions[I].Size
        ]);
      end;
      rcrSame:
      begin
        raise Exception.CreateFmt('The region duplicates the existing one [%d](0x%x-0x%x).',
        [
          I,
          FRegions[I].Address,
          FRegions[I].Address + FRegions[I].Size
        ]);
      end;
      rcrError:
      begin
        raise Exception.CreateFmt('Unexpected error when adding a region (0x%x-0x%x).',
        [
          Address, Address + RegionSize
        ]);
      end;
    end;
  end;

  LineData := Default(TMapRow);
  LineData.Index := FData.Count;
  LineData.Style := rsRegion;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.RegionStart := True;
  LineData.RegionSizeOrAddr := RegionSize;

  Result := TRegion.Create(Self, FData.Count, Address, RegionSize);
  Result.Expanded := Expanded;
  Result.Header.DrawStyle := DrawStyle;
  Result.Footer.DrawStyle := DrawStyle;

  LineData.RegionIndex := FRegions.Add(Result);
  AddMapLine(LineData);

  LineData.Index := FData.Count;
  LineData.Address := LastRegionAddrVa;
  LineData.Description := Description;
  LineData.RegionStart := False;
  LineData.RegionSizeOrAddr := Address;
  AddMapLine(LineData);

  CurrentAddr := Address;
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
var
  Item: TPair<Int64, Int64>;
  I, R: TRegion;
begin
  FData.Clear;
  FData.AddRange(Value.FData.ToArray);
  FRawIndex.Clear;
  for Item in Value.FRawIndex do
    FRawIndex.AddOrSetValue(Item.Key, Item.Value);
  FRegions.Clear;
  for I in Value.Regions do
  begin
    R := TRegion.Create(Self, 0, 0, 0);
    R.Assign(I);
    FRegions.Add(R);
  end;
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
    if not FOwner.Pages.CheckAddrInPages(Address, DataLength = 0) then
      Exit(acStartOutOfPool);
    if (DataLength > 0) and not FOwner.Pages.CheckAddrInPages(Address + DataLength - 1, True) then
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

procedure TDataMap.CollapseAllRegions;
var
  RegionAddr: TRegion;
begin
  BeginUpdate;
  try
    for RegionAddr in FRegions do
      InternalRegionChangeByAddress(RegionAddr.Address, rctCollapse);
  finally
    EndUpdate;
  end;
end;

procedure TDataMap.CollapseRegion(Address: Int64);
begin
  InternalRegionChangeByAddress(Address, rctCollapse);
end;

function TDataMap.CompareRegion(AAddress, ASize: Int64; ARegion: TRegion): TRegionCompareResult;
begin
  // Region A is above region ARegion.
  if AAddress + ASize <= ARegion.Address then Exit(rcrNotMatching);
  // Region A is below region ARegion.
  if AAddress >= ARegion.Address + ARegion.Size then Exit(rcrNotMatching);
  // Region A matched region ARegion.
  if (AAddress = ARegion.Address) and (ASize = ARegion.Size) then Exit(rcrSame);
  // Region A is partially higher than region ARegion.
  if (AAddress < ARegion.Address) and (AAddress + ASize > ARegion.Address) and
    (AAddress + ASize < ARegion.Address + ARegion.Size) then Exit(rcrPartialIntersectTop);
  // Region A is partially lower than region ARegion.
  if (AAddress > ARegion.Address) and (AAddress < ARegion.Address + ARegion.Size) and
    (AAddress + ASize > ARegion.Address + ARegion.Size) then Exit(rcrPartialIntersectBottom);
  // Region A is included in region ARegion and does not border any boundaries.
  if (AAddress > ARegion.Address) and (AAddress + ASize < ARegion.Address + ARegion.Size) then Exit(rcrFullNested);
  // Region A is included in region ARegion.
  if (AAddress >= ARegion.Address) and (AAddress + ASize <= ARegion.Address + ARegion.Size) then Exit(rcrNested);
  // Region A covers region ARegion.
  if (AAddress <= ARegion.Address) and (AAddress + ASize >= ARegion.Address + ARegion.Size) then Exit(rcrExternal);
  Result := rcrError;
end;

function DefaultMapRowComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} A, B: TMapRow): Integer;
var
  LongResult: Int64;
  LeftIsMask, RighIsMask: Boolean;
begin

  // порядок строго по возрастанию адресов

  // The order is strictly in ascending order of addresses

  LongResult := A.Address - B.Address;

  // все строки не содержащие данных ОБЯЗАТЕЛЬНО
  // идут перед блоками с данными!!!

  // all non-data lines MUST come before the data blocks!!!

  if LongResult = 0 then
    LongResult := A.RawLength - B.RawLength;

  if (A.Style = rsRegion) and (B.Style = rsRegion) then
  begin
    if A.Index = B.Index then
      Exit(0);
    if A.Address = B.Address then
    begin
      if A.RegionStart and not B.RegionStart then
        Exit(1);
      if B.RegionStart and not A.RegionStart then
        Exit(-1);
      LongResult := B.RegionSizeOrAddr - A.RegionSizeOrAddr
    end;
  end;

    LeftIsMask := A.Style in [rsMaskCheck..rsMaskSeparator];
    RighIsMask := B.Style in [rsMaskCheck..rsMaskSeparator];

  if LongResult = 0 then
  begin
    if (A.Style = rsRegion) and (B.RawLength > 0) then
      LongResult := 1;
    if (B.Style = rsRegion) and (A.RawLength > 0) then
      LongResult := 1;
    if (A.Style = rsRegion) and RighIsMask then
      LongResult := 1;
    if (B.Style = rsRegion) and LeftIsMask then
      LongResult := -1;
  end;

  if LongResult = 0 then
  begin
    if LeftIsMask and (B.RawLength = 0) and not RighIsMask then
      LongResult := -1;
    if RighIsMask and (A.RawLength = 0) and not LeftIsMask then
      LongResult := 1;
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
  FRegions := TObjectList<TRegion>.Create;
  FHiddenFooter := TObjectDictionary<Int64, TList<TRegion>>.Create([doOwnsValues]);
end;

procedure TDataMap.DataChange(Sender: TObject;
  {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Item: TMapRow;
  Action: TCollectionNotification);
begin
  RebuildDataMap;
end;

destructor TDataMap.Destroy;
begin
  FHiddenFooter.Free;
  FRegions.Free;
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

procedure TDataMap.ExpandAllRegions;
var
  RegionAddr: TRegion;
begin
  BeginUpdate;
  try
    for RegionAddr in FRegions do
      InternalRegionChangeByAddress(RegionAddr.Address, rctExpand);
  finally
    EndUpdate;
  end;
end;

procedure TDataMap.ExpandRegion(Address: Int64);
begin
  InternalRegionChangeByAddress(Address, rctExpand);
end;

function TDataMap.FindRegionLevel(Address, RegionSize: Int64): Integer;
var
  I: TRegion;
begin
  Result := 1;
  for I in FRegions do
    if CompareRegion(Address, RegionSize, I) in [rcrNested, rcrFullNested] then
      Inc(Result);
end;

function TDataMap.GetCurrentAddr: Int64;
begin
  Result := CurrentAddr;
end;

procedure TDataMap.InternalClear(NewStartAddress: Int64);
begin
  FData.Clear;
  FRawIndex.Clear;
  FRegions.Clear;
  FCurrentAddr := NewStartAddress;
end;

procedure TDataMap.InternalRegionChange(ARegion: TRegion;
  AChangeType: TRegionChangeType);
begin
  if ARegion = nil then Exit;  
  case AChangeType of
    rctCollapse: ARegion.Expanded := False;
    rctExpand: ARegion.Expanded := True;
    rctToggle: ARegion.Expanded := not ARegion.Expanded;
  end;
end;

procedure TDataMap.InternalRegionChangeByAddress(Address: Int64;
  AChangeType: TRegionChangeType);
var
  Idx: Int64;
begin
  Idx := FOwner.RawData.AddressToRowIndex(Address);
  if (Idx >= 0) and (FOwner.RawData[Idx].Style = rsRegion) and FOwner.RawData.RegionStart then
    InternalRegionChange(FOwner.RawData.Region, AChangeType);
end;

function TDataMap.LastDataMapIndex: Integer;
begin
  Result := FData.Count - 1;
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

procedure TDataMap.SetRowColor(ADataMapIndex: Integer; AValue: TColor);
begin
  Data.List[ADataMapIndex].Color := AValue;
  RebuildDataMap;
end;

procedure TDataMap.SetRowComment(ADataMapIndex: Integer; const AValue: string);
begin
  Data.List[ADataMapIndex].Comment := AValue;
  RebuildDataMap;
end;

procedure TDataMap.SetRowDescription(ADataMapIndex: Integer;
  const AValue: string);
begin
  Data.List[ADataMapIndex].Description := AValue;
  RebuildDataMap;
end;

procedure TDataMap.SetRowLineSeparated(ADataMapIndex: Integer; AValue: Boolean);
begin
  Data.List[ADataMapIndex].DrawRowSmallSeparator := AValue;
  RebuildDataMap;
end;

procedure TDataMap.ToggleRegion(Address: Int64);
begin
  InternalRegionChangeByAddress(Address, rctToggle);
end;

{ TRegionPart }

procedure TRegionPart.Assign(AValue: TRegionPart);
begin
  FBackgroundColor := AValue.FBackgroundColor;
  FDataMapIndex := AValue.FDataMapIndex;
  FDrawStyle := AValue.FDrawStyle;
  FRowIndex := AValue.FRowIndex;
end;

constructor TRegionPart.Create(AOwner: TRegion; ADataMapIndex: Int64);
begin
  FOwner := AOwner;
  FDataMapIndex := ADataMapIndex;
  FBackgroundColor := clDefault;
end;

function TRegionPart.GetComment: string;
begin
  Result := FOwner.DataMap.Data.List[FDataMapIndex].Comment;
end;

function TRegionPart.GetText: string;
begin
  Result := FOwner.DataMap.Data.List[FDataMapIndex].Description;
end;

function TRegionPart.GetTextColor: TColor;
begin
  Result := FOwner.DataMap.Data.List[FDataMapIndex].Color;
end;

procedure TRegionPart.SetAlignment(const Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FAlignment := Value;
    FOwner.DoChange;
  end;
end;

procedure TRegionPart.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  FOwner.DoChange;
end;

procedure TRegionPart.SetComment(const Value: string);
begin
  FOwner.DataMap.Data.List[FDataMapIndex].Comment := Value;
  FOwner.DoChange;
end;

procedure TRegionPart.SetDrawStyle(const Value: TRegionDrawStyle);
begin
  FDrawStyle := Value;
  FOwner.DoChange;
end;

procedure TRegionPart.SetText(const Value: string);
begin
  FOwner.DataMap.Data.List[FDataMapIndex].Description := Value;
  FOwner.DoChange;
end;

procedure TRegionPart.SetTextColor(const Value: TColor);
begin
  FOwner.DataMap.Data.List[FDataMapIndex].Color := Value;
  FOwner.DoChange;
end;

{ TRegion }

function TRegion.AddressInRegion(AAddress: Int64): Boolean;
begin
  Result := (Address <= AAddress) and (AAddress < Address + Size);
end;

procedure TRegion.Assign(AValue: TRegion);
begin
  FAddress := AValue.FAddress;
  FSize := AValue.FSize;
  FExpanded := AValue.FExpanded;
  FHeader.Assign(AValue.Header);
  FFooter.Assign(AValue.Footer);
  FFooterVisible := AValue.FFooterVisible;
  FLevel := AValue.FLevel;
  FLineColor := AValue.FLineColor;
end;

constructor TRegion.Create(AOwner: TDataMap; AHeaderIndex, AAddress, ASize: Int64);
begin
  FOwner := AOwner;
  FAddress := AAddress;
  FSize := ASize;
  FHeader := TRegionPart.Create(Self, AHeaderIndex);
  FFooter := TRegionPart.Create(Self, AHeaderIndex + 1);
  FLineColor := clDefault;
  FLineVisible := True;
  FLineWidth := 2;
  FLineStyle := psSolid;
end;

destructor TRegion.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  inherited;
end;

procedure TRegion.DoChange;
begin
  DataMap.RebuildDataMap;
end;

function TRegion.FirstRawDataRowIndex: Integer;
begin
  Result := Header.RowIndex + 1;
end;

function TRegion.LastRawDataRowIndex: Integer;
begin
  Result := Footer.RowIndex - 1;
end;

procedure TRegion.SetExpanded(const Value: Boolean);
begin
  if Expanded <> Value then
  begin
    FExpanded := Value;
    DoChange;
  end;
end;

procedure TRegion.SetFooterVisible(const Value: Boolean);
begin
  if FooterVisible <> Value then
  begin
    FFooterVisible := Value;
    DoChange;
  end;
end;

procedure TRegion.SetLineColor(const Value: TColor);
begin
  if LineColor <> Value then
  begin
    FLineColor := Value;
    DoChange;
  end;
end;

procedure TRegion.SetLineWidth(const Value: Integer);
begin
  if LineWidth <> Value then
  begin
    FLineWidth := Value;
    DoChange;
  end;
end;

procedure TRegion.SetLineStyle(const Value: TPenStyle);
begin
  if LineStyle <> Value then
  begin
    FLineStyle := Value;
    DoChange;
  end;
end;

procedure TRegion.SetLineVisible(const Value: Boolean);
begin
  if LineVisible <> Value then
  begin
    FLineVisible := Value;
    DoChange;
  end;
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
    ACanvas.Font.Style := Owner.Font.Style;
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
  ACanvas.Font.Style := Owner.Font.Style;
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
  Data: TMappedRawData;
begin
  if AColumn <> ctNone then Exit;
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('TRowComment.DrawColumn');{$ENDIF}
  Data := RawData[RowIndex];
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := SelectedColor(SelData.SelectStyle);
  Inc(ARect.Left, GetLeftNCWidth + TextMargin);
  ACanvas.Font.Style := Owner.Font.Style;
  ACanvas.Font.Color := TMapViewColors(ColorMap).TextCommentColor;
  ADescription := Data.Description;
  if Data.MaxRegionLevel > 0 then
    Inc(ARect.Left, Data.MaxRegionLevel * (CharWidth shl 1));
  DrawText(ACanvas, ADescription,
    Length(ADescription), ARect, DT_CALCRECT);
  ACanvas.FillRect(ARect);
  CorrectCanvasFont(ACanvas, AColumn);
  DrawText(ACanvas, ADescription,
    Length(ADescription), ARect, DT_LEFT);
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
  ACanvas.Font.Style := Owner.Font.Style;
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

  ACanvas.Font.Style := Owner.Font.Style;
  ACanvas.Font.Color := ColorMap.TextCommentColor;
  DrawAlignedTextPart(ACanvas, ctComment, DataString, ARect);
  ACanvas.Font.Style := Owner.Font.Style;
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
  ACanvas.Font.Style := Owner.Font.Style;
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

{ TRowRegion }

procedure TRowRegion.CopyRowAsString(Builder: TSimplyStringBuilder);
begin
  Builder.Append(RawData[RowIndex].Description + sLineBreak);
end;

procedure TRowRegion.DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
  var ARect: TRect);
const
  TextFlag: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  ADescription: string;
  AWidth, ACenteredOffset, ARightOffset, RegionTextFlag: Integer;
  P: TPoint;
  R: TRect;
  Data: TMappedRawData;
  Region: TRegion;
  RegionDrawStyle: TRegionDrawStyle;
  RegionBkColor: TColor;
begin
  if AColumn <> ctNone then Exit;
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('TRowRegion.DrawColumn');{$ENDIF}
  Data := RawData[RowIndex];
  Region := Data.Region;
  if Region = nil then Exit;
  if Data.RegionStart then
  begin
    RegionDrawStyle := Region.Header.DrawStyle;
    RegionBkColor :=  Region.Header.BackgroundColor;
    RegionTextFlag := TextFlag[Region.Header.Alignment];
  end
  else
  begin
    RegionDrawStyle := Region.Footer.DrawStyle;
    RegionBkColor :=  Region.Footer.BackgroundColor;
    RegionTextFlag := TextFlag[Region.Footer.Alignment];
  end;
  ACanvas.Brush.Style := bsSolid;
  if RegionDrawStyle <> rdsComment then
  begin
    if RegionBkColor = clDefault then
      ACanvas.Brush.Color := TMapViewColors(ColorMap).SeparatorBackgroundColor
    else
      ACanvas.Brush.Color := RegionBkColor;
    ACanvas.Pen.Color := TMapViewColors(ColorMap).SeparatorBorderColor;
    R := ARect;
    Inc(R.Left, GetLeftNCWidth +
      (Region.Level - 1) * (CharWidth shl 1));
    Inc(R.Right);
    Dec(R.Top);
    ACanvas.Rectangle(R);
  end;
  ACanvas.Brush.Color := SelectedColor(SelData.SelectStyle);
  ARightOffset := ARect.Right - SplitMargin;
  Inc(ARect.Left,
    GetLeftNCWidth + TextMargin +               // text offset
    RowHeight +                                 // expander
    (Region.Level - 1) * (CharWidth shl 1)      // level offset
  );
  ACanvas.Font.Style := Owner.Font.Style;
  if Data.Color = clDefault then
    ACanvas.Font.Color := TMapViewColors(ColorMap).TextCommentColor
  else
    ACanvas.Font.Color := Data.Color;
  ADescription := Data.Description;
  DrawText(ACanvas, ADescription,
    Length(ADescription), ARect, DT_CALCRECT);
  if RegionDrawStyle = rdsComment then
    ACanvas.FillRect(ARect)
  else
    ACanvas.Brush.Style := bsClear;
  CorrectCanvasFont(ACanvas, AColumn);
  if RegionDrawStyle = rdsSeparator then
  begin
    R := ARect;
    if RegionTextFlag = DT_CENTER then
      R.Left := GetLeftNCWidth + ScrollOffset.X;
    R.Right := ARightOffset - SplitMargin;
    if Data.Comment <> '' then
      Dec(R.Right, ColumnWidth[ctComment]);
    DrawText(ACanvas, ADescription, Length(ADescription), R, RegionTextFlag);
  end
  else
    DrawText(ACanvas, ADescription, Length(ADescription), ARect, DT_LEFT);

  ACanvas.Font.Style := Owner.Font.Style;
  if Data.RegionStart then
  begin
    ARect.Width := 0;
    ARect.Left := ARect.Right - RowHeight - SplitMargin;
    ACanvas.Brush.Color := ColorMap.BackgroundColor;
    if RegionDrawStyle = rdsComment then
      ACanvas.FillRect(ARect);
    ACanvas.Pen.Color := ColorMap.TextColor;
    AWidth := ToDpi(3);
    if Region.Expanded then
    begin
      ACenteredOffset := (RowHeight - AWidth) div 2;
      R := Bounds(ACenteredOffset, ACenteredOffset, AWidth, AWidth);
      OffsetRect(R, ARect.Left - MulDiv(AWidth, 3, 2) + AWidth, ARect.Top + 1);
      if R.Right >= ARightOffset then Exit;
      P := R.TopLeft;
      Dec(P.Y, AWidth div 2);
      DrawArrow(ACanvas, sdDown, P, AWidth);
    end
    else
    begin
      ACenteredOffset := (RowHeight - AWidth * 2) div 2;
      R := Bounds(ACenteredOffset, ACenteredOffset, AWidth, AWidth);
      OffsetRect(R, ARect.Left, ARect.Top + 1);
      if R.Right >= ARightOffset then Exit;
      P := R.TopLeft;
      Dec(P.Y, AWidth div 2);
      DrawArrow(ACanvas, sdRight, P, AWidth);
    end;
  end;

  if Data.Comment <> '' then
  begin
    R := GetColumnRect(ctComment, RowIndex);
    Inc(R.Left, TextMargin);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := TMapViewColors(ColorMap).TextCommentColor;
    DrawAlignedTextPart(ACanvas, ctComment, Data.Comment, R);
  end;
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
end;

procedure TRowRegion.GetHitInfo(var AHitInfo: TMouseHitInfo);
var
  Data: TMappedRawData;
  Region: TRegion;
  ARect: TRect;
begin
  Data := RawData[RowIndex];
  Region := Data.Region;
  if Data.RegionStart and Assigned(Region) then
  begin
    ARect := Bounds(GetLeftNCWidth + TextMargin - SplitMargin +
      (Region.Level - 1) * (CharWidth shl 1), 0, RowHeight, RowHeight);
    if (AHitInfo.ScrolledCursorPos.X >= ARect.Left) and
      (AHitInfo.ScrolledCursorPos.X <= ARect.Right) then
      AHitInfo.Cursor := crHandPoint;
  end;
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
  I, TopOffset, StartRowWithMask, EndRowWithMask: Int64;
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
  I := StartRowWithMask;
  while I <= EndRowWithMask do
  begin
    if RawData[I].Style in [rsMaskCheck, rsMaskRadio] then
      DrawRow(ACanvas, I, Offset, False, RawData[I].Style = rsMaskRadio)
    else
      Inc(Offset.Y, RowHeight);
    Inc(I);
  end;

  Offset.Y := TopOffset;
  I := StartRowWithMask;
  while I <= EndRowWithMask do
  begin
    if RawData[I].Style in [rsMaskCheck, rsMaskRadio] then
      DrawRow(ACanvas, I, Offset, True, RawData[I].Style = rsMaskRadio)
    else
      Inc(Offset.Y, RowHeight);
    Inc(I);
  end;
end;

function TCheckRadioRowPostPainter.TextMetric: TAbstractTextMetric;
begin
  Result := TCustomMappedHexView(Owner).MaskTextMetric;
end;

{ TRegionPostPainter }

procedure TRegionPostPainter.PostPaint(ACanvas: TCanvas; StartRow,
  EndRow: Int64; var Offset: TPoint);
var
  Data: TMappedRawData;
  Region: TRegion;
  I, RowIdx, FooterIdx, AOffset, RegionLevel, LevelOffset, AWidth: Int64;
  AStartPoint, AEndPoint, AInternalPoint: TPoint;
  HiddenFootersList: TList<TRegion>;

  procedure CalculateRegion;
  begin
    RegionLevel := Region.Level;
    AOffset := GetLeftNCWidth +
      (TextMargin + RowHeight - SplitMargin) div 2 +
      MulDiv(AWidth, 3, 2) - AWidth +
      (RegionLevel - 1) * (CharWidth shl 1);
    LevelOffset := (View.RawData.MaxRegionLevel - RegionLevel) * (CharWidth shl 1);
  end;

  procedure ProcessFooter;
  begin
    if FooterIdx > 0 then
    begin

      if RawData[FooterIdx].Style = rsRegion then
      begin
        Region := RawData.Region;
        if Assigned(Region) and not Region.Expanded then
          Dec(LevelOffset, TextMargin +
            (Region.Level - RegionLevel) * (CharWidth shl 1) - 2);
      end;

      if Region.LineVisible then
      begin
        ACanvas.Pen.Color := Region.LineColor;
        ACanvas.Pen.Width := Region.LineWidth;
        ACanvas.Pen.Style := Region.LineStyle;
        ACanvas.MoveTo(AStartPoint.X + AOffset, AStartPoint.Y + RowHeight);
        ACanvas.LineTo(AEndPoint.X + AOffset, AEndPoint.Y + RowHeight div 2);
        ACanvas.LineTo(AEndPoint.X + AOffset + TextMargin + LevelOffset, AEndPoint.Y + RowHeight div 2);
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := psSolid;
      end;
    end;
  end;

begin
  if View.RawData.MaxRegionLevel = 0 then Exit;  
  I := StartRow;
  AWidth := ToDpi(3);
  while I <= EndRow do
  begin
    Data := RawData[I];
    FooterIdx := -1;

    if Data.Style = rsRegion then
    begin
      Region := Data.Region;
      if Region = nil then
      begin
        Inc(I);
        Continue;
      end;

      CalculateRegion;
      AStartPoint := GetRowOffsetPoint(I);

      // Если в видимой области присутствует заголовок региона,
      // рисуем линию от него вниз

      // If there is a region header in the visible area,
      // draw a line down from it.

      if RawData.RegionStart and Region.Expanded then
      begin
        RowIdx := Region.LastRawDataRowIndex;
        AEndPoint := GetRowOffsetPoint(RowIdx);
        FooterIdx := RowIdx;
      end;

      // если в видимой области только Footer региона, и заголовок скрыт,
      // то рисуем линию вверх

      // if only the Footer region is visible and the Header is hidden,
      // then draw a line upwards

      if not RawData.RegionStart then
      begin
        RowIdx := Region.FirstRawDataRowIndex;
        if RowIdx <= StartRow then
        begin
          FooterIdx := I - 1;
          AEndPoint := AStartPoint;
          Dec(AEndPoint.Y, RowHeight);
          AStartPoint.Y := 0;
        end;
      end;

      ProcessFooter;
    end;

    // скрытые Footer-ы регионов могут быть расположены на одной строке
    // поэтому за их обработку отвечает следующий код,
    // задача которого нарисовать линию вверх в случае если Footer
    // находится выше отображаемой области

    // hidden region footers may be located on the same line,
    // so the following code is responsible for processing them.
    // Its task is to draw a line upward if the footer
    // is above the displayed area

    if (I > StartRow) and View.DataMap.HiddenFooter.TryGetValue(I, HiddenFootersList) then
    begin
      AInternalPoint := GetRowOffsetPoint(I);
      for Region in HiddenFootersList do
      begin
        RowIdx := Region.FirstRawDataRowIndex;
        if RowIdx <= StartRow then
        begin
          CalculateRegion;
          FooterIdx := I - 1;
          AEndPoint := AInternalPoint;
          Dec(AEndPoint.Y, RowHeight);
          AStartPoint.X := AEndPoint.X;
          AStartPoint.Y := 0;
          ProcessFooter;
        end;
      end;
    end;

    // и последняя обрабатываемая ситуация - отображается середина региона
    // границы которого расположены вне видимой области. В этом случае
    // необходимо отрисовать вертикальную линию взяв за основу Level региона

    // and the last situation to be processed is when the middle of a region
    // whose borders are outside the visible area is displayed. In this case,
    // it is necessary to draw a vertical line based on the Level of the region

    for Region in View.CoveringRegion do
    begin
      if not Region.LineVisible then Continue;
      CalculateRegion;
      ACanvas.Pen.Color := Region.LineColor;
      ACanvas.Pen.Width := Region.LineWidth;
      ACanvas.Pen.Style := Region.LineStyle;
      ACanvas.MoveTo(AOffset, 0);
      ACanvas.LineTo(AOffset, ClientHeight);
      ACanvas.Pen.Width := 1;
      ACanvas.Pen.Style := psSolid;
    end;

    Inc(I);
  end;
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

procedure TVirtualPage.SetSize(const Value: Integer);
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

function TVirtualPages.CheckAddrInPages(VirtualAddress: Int64; EmptyData: Boolean): Boolean;
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

  // В случае если добавляется служебная строчка, наподобие коментария,
  // разделителя и т.д. разрешаем ей добавляться в первый байт за границей региона

  // If a service line is added, such as a comment, separator, etc.,
  // we allow it to be added to the first byte outside the region.

  if EmptyData and not Result then
    Result := CheckAddrInPages(VirtualAddress - 1, False);
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
  I, EndIdx, ARowIndex: Int64;
  Painter: TAbstractPrimaryRowPainter;
  ATextMetric: TAbstractTextMetric;
begin
  case Value of
    ctAddress:
    begin
      Result := inherited;
      if RawData.MaxRegionLevel > 0 then
        Inc(Result, RawData.MaxRegionLevel * (CharWidth shl 1));
    end;
    ctWorkSpace: Result := ToDpi(32);
    ctJmpLine: Result := ToDpi(82);
    ctOpcode:
    begin
      Result := ToDpi(255);
      I := 0;
      EndIdx := RawData.PresentRows.Count;
      while I < EndIdx do
      begin
        ARowIndex := RawData.PresentRows.List[I].RowIndex;
        Painter := GetRowPainter(ARowIndex);
        if Painter <> nil then
        begin
          ATextMetric := TPrimaryMappedRowPainter(Painter).TextMetric;
          Result := Max(Result, ATextMetric.SelectionLength(Value, 0,
            RawData[ARowIndex].RawLength - 1));
        end;
        Inc(I);
      end;
      Inc(Result, TextMargin shl 1);
    end;
    ctDescription, ctComment:
    begin
      Result := inherited;
      I := 0;
      EndIdx := RawData.PresentRows.Count;
      while I < EndIdx do
      begin
        ARowIndex := RawData.PresentRows.List[I].RowIndex;
        if RawData[ARowIndex].RawLength > 0 then
          Result := Max(Result, Length(GetRowCustomString) * CharWidth + TextMargin shl 1);
        Inc(I);
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

procedure TCustomMappedHexView.DoBeforePostPaint(const ADiapason: TVisibleRowDiapason);
var
  RegAddress, RegSize: Integer;
  Region: TRegion;
begin
  RegAddress := RawData.RowToAddress(ADiapason.StartRow, 0);
  RegSize := RawData.RowToAddress(ADiapason.EndRow, 0) + RawData[ADiapason.EndRow].RawLength;
  RegSize := RegSize - RegAddress;
  CoveringRegion.Clear;
  for Region in DataMap.Regions do
    case DataMap.CompareRegion(RegAddress, RegSize, Region) of
      rcrFullNested: CoveringRegion.Add(Region);
      rcrNested:
      begin

        // Регион будет определен как Nested в случае если виден один из заголовков
        // потому что у заголовков тот-же адрес что и у самого региона.
        // Поэтому делаем дополнительную проверку на то, что оба заголовка не видны

        // The region will be defined as Nested if one of the headers is visible
        // because the headers have the same address as the region itself.
        // Therefore, we perform an additional check to ensure that both headers are invisible

        if (Region.Header.RowIndex < ADiapason.StartRow) and
          (Region.Footer.RowIndex > ADiapason.EndRow) then
          CoveringRegion.Add(Region);
      end;
    end;
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

procedure TCustomMappedHexView.DoColumnWidthChange(AColumnType: TColumnType;
  var AWidth: Integer);
var
  AMinWidth: Integer;
begin
  if (AColumnType = ctAddress) and (RawData.MaxRegionLevel > 0) then
  begin
    AMinWidth := CalculateColumnBestSize(ctAddress);
    if AWidth < AMinWidth then
      AWidth := AMinWidth;
  end;
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
  FCoveringRegion := TList<TRegion>.Create;
end;

procedure TCustomMappedHexView.DblClick;
begin
  if RawData[MousePressedHitInfo.SelectPoint.RowIndex].Style = rsRegion then
  begin
    if RawData.RegionStart and (MousePressedHitInfo.Cursor <> crHandPoint) then
      DataMap.InternalRegionChange(RawData.Region, rctToggle);
  end
  else
    inherited;
end;

destructor TCustomMappedHexView.Destroy;
begin
  FCoveringRegion.Free;
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

procedure TCustomMappedHexView.FocusOnAddress(Address: Int64;
  ACaretChangeMode: TCaretChangeMode);
var
  Region: TRegion;
  RemoveUpdateLevel: Boolean;
begin
  RemoveUpdateLevel := False;
  try
    for Region in DataMap.Regions do
    begin
      if Region.AddressInRegion(Address) and not Region.Expanded then
      begin
        if not InUpdateMode then
        begin
          BeginUpdate;
          RemoveUpdateLevel := True;
        end;
        Region.Expanded := True;
      end;
    end;
  finally
    if RemoveUpdateLevel then
      EndUpdate;
  end;
  inherited;
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
  case RawData[ARowIndex].Style of
    rsMask, rsRegion:
    begin
      if RawData.Style = rsMask then
      begin
        DataMap.Data.List[RawData.MapRowIndex].Expanded := not RawData.Expanded;
        ClearSelection;
        UpdateDataMap;
        UpdateTextBoundary;
        UpdateScrollPos;
        Invalidate;
      end
      else
        DataMap.InternalRegionChange(RawData.Region, rctToggle);
    end;
  else
    JumpToAddress(RawData[ARowIndex].JmpToAddr);
  end;
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
  Painters.Add(GetOverloadPainterClass(TRowRegion).Create(Self));

  PostPainters.Add(TJumpLinesPostPainter.Create(Self));
  PostPainters.Add(TCheckRadioRowPostPainter.Create(Self));
  PostPainters.Add(TRegionPostPainter.Create(Self));

  FMaskTextMetric := TMaskTextMetric.Create(Self);
end;

function TCustomMappedHexView.InternalGetRowPainter(
  ARowIndex: Int64): TAbstractPrimaryRowPainter;
begin
  if (ARowIndex < 0) or (ARowIndex >= RawData.Count) then
    Exit(nil);
  case RawData[ARowIndex].Style of
    rsRaw, rsRawCustom: Result := Painters[0];
    rsRawWithExDescription: Result := Painters[1];
    rsAsm: Result := Painters[2];
    rsSeparator: Result := Painters[3];
    rsLine: Result := Painters[4];
    rsLineComment: Result := Painters[5];
    rsMask: Result := Painters[6];
    rsMaskCheck, rsMaskRadio: Result := Painters[7];
    rsRegion: Result := Painters[8];
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

  if RawData.MaxRegionLevel > 0 then
    FitColumnToBestSize(ctAddress);

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
