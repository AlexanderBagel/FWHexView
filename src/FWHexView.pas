////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.pas
//  * Purpose   : Implementation of a basic HexView editor
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

unit FWHexView;

{$IFDEF FPC}
  {$I FWHexViewConfig.inc}
{$ENDIF}

interface

//{$define show_dbg_lines}
//{$define dbg_check_last_row}
//{$define profile_speed}
//{$define profile_paint_speed}

uses
  {$IFDEF FPC}
  LCLIntf,
  LCLType,
  StdCtrls,
  LazUTF8,
  LMessages,
  {$ELSE}
  Windows,
  UITypes,
  UxTheme,
  Actions,
  {$ENDIF}
  Messages,
  Classes,
  Forms,
  Controls,
  Graphics,
  ClipBrd,
  SysUtils,
  Themes,
  Types,
  TypInfo,
  Math,
  {$if defined (profile_speed) or defined (profile_paint_speed)}
  Diagnostics,
  {$endif}
  Generics.Collections,
  Generics.Defaults,
  ActnList,
  Menus,
  {$IFDEF USE_PROFILER}
  uni_profiler,
  {$ENDIF}
  FWHexView.Common,
  FWHexView.Actions;

{$IFDEF FPC}
const
  WM_TIMER = LM_TIMER;
{$ENDIF}

type

{$IFDEF FPC}
  TWMTimer = TLMTimer;
{$ELSE}
  TUnicodeCharArray = TCharArray;
  TLayoutAdjustmentPolicy = (lapNone);
{$ENDIF}

  TNativeChar = {$IFDEF FPC}TUTF8Char{$ELSE}Char{$ENDIF};

  TFWCustomHexView = class;

  TRawData = class
  strict private
    FOwner: TFWCustomHexView;
    FAddress, FDataOffset, FRowAddress: Int64;
    FCount: Int64;
    FRawLength, FRowRawLength: Int64;
    FRowIndex: Int64;
    function GetDataAtRowIndex(ARowIndex: Int64): TRawData;
  protected
    property Owner: TFWCustomHexView read FOwner;
    property RowIndex: Int64 read FRowIndex;
  public
    constructor Create(AOwner: TFWCustomHexView); virtual;
    function AddressToRowIndex(Value: Int64): Int64; virtual;
    function Address: Int64; virtual;
    function Color: TColor; virtual;
    function DataOffset: Int64; virtual;
    function DrawRowSmallSeparator: Boolean; virtual;
    function RawLength: Int64; virtual;
    function Comment: string; virtual;
    function RowToAddress(ARowIndex: Int64; ValueOffset: Integer): Int64; virtual;
    function Count: Int64; virtual;
    procedure Clear; virtual;
    procedure Update; virtual;
    property Data[ARowIndex: Int64]: TRawData read GetDataAtRowIndex; default;
  end;

  TRawDataClass = class of TRawData;

  TBookMark = 0..9;

  TLargePoint = record
    X, Y: Int64;
  end;

  TDrawParam = record
    AddrVA: Int64;
    Column: TColumnType;
    RowIndex: Int64;
    CharIndex: Integer;
    ValueOffset: Integer;
  end;

  TSelectStyle = (ssNone, ssAllSelected, ssLeftSelected,
    ssCenterSelected, ssRightSelected);

  TSelectData = record
    SelectStyle: TSelectStyle;
    FirstSelectIndex: Int64;
    SecondSelectIndex: Int64;
  end;

  TSelectPoint = record
    Column: TColumnType;
    RowIndex: Int64;
    ValueOffset: Integer;
    CharIndex: Integer;

    class operator Equal(A, B: TSelectPoint): Boolean; inline;
    class operator LessThan(A, B: TSelectPoint): Boolean; inline;
    class operator LessThanOrEqual(A, B: TSelectPoint): Boolean; inline;
    class operator GreaterThan(A, B: TSelectPoint): Boolean; inline;
    class operator GreaterThanOrEqual(A, B: TSelectPoint): Boolean; inline;
    class operator NotEqual(A, B: TSelectPoint): Boolean; inline;
    procedure Erase; inline;
    function InvalidRow: Boolean; inline;
    function ValidSelectedByte: Boolean; inline;
  end;

  TSelection = record
    Tag: Integer;
    SelStart: Int64;
    SelEnd: Int64;
    Color: TColor;
  end;

  TSelections = class
  strict private
    FOwner: TFWCustomHexView;
    FItems: TListEx<TSelection>;
    FVisibleIndex: TList<Integer>;
    FUpdateCount: Integer;
    FChange: TNotifyEvent;
    procedure DoChange;
    function GetItem(Index: Integer): TSelection;
    procedure ListNotification(Sender: TObject;
      {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Item: TSelection;
      Action: TCollectionNotification);
  protected
    procedure UpdateVisibleSelections;
    property Items: TListEx<TSelection> read FItems;
    property VisibleIndex: TList<Integer> read FVisibleIndex;
    property OnChange: TNotifyEvent read FChange write FChange;
  public
    constructor Create(AOwner: TFWCustomHexView);
    destructor Destroy; override;
    function Add(ATag: Integer; ASelStart, ASelEnd: Int64; AColor: TColor): Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    function Count: Integer;
    procedure Delete(Index: Integer);
    procedure DropSelectionsAtTag(ATag: Integer);
    property Item[Index: Integer]: TSelection read GetItem; default;
  end;

  TKnownElement = (keHeader, keSplitter, keSelection, keInplaceEdit);
  TKnownElements = set of TKnownElement;

  TMouseHitInfo = record
    CursorPos: TPoint;
    ScrolledCursorPos: TPoint;
    Shift: TShiftState;
    SelectPoint: TSelectPoint;
    Elements: TKnownElements;
    ColumnStart: Integer;
    ColumnWidth: Integer;
    Cursor: TCursor;
    procedure Erase; inline;
  end;

  TCaretChangeMode = (
    ccmNone,
    ccmSetNewSelection,
    ccmContinueSelection,
    ccmSelectRow,
    ccmSelectPointer,
    ccmReset);

  /// Три режима работы с кареткой для редактирования:
  /// 1. Каретка отключена
  /// 2. Каретка позволяет производить посимвольное редактирование
  /// 3. Вместо каретки должен использоваться встроенный редактор полей

  /// <summary>
  /// Three modes of operation with the editing carriage:
  /// 1. The carriage is disabled
  /// 2. The carriage allows character-by-character editing
  /// 3. The built-in field editor must be used instead of the carriage
  /// </summary>
  TCaretEditMode = (cemDisabled, cemSingleChar, cemValueEditor);

  TCaretPosData = record
    EditMode: TCaretEditMode;
    Column: TColumnType;
    RowIndex: Int64;
    CharIndex: Integer;
    Showed: Boolean;
  end;

  ///  Класс описывает представление вывода текста
  ///  FCharPositions - размер каждого символа при выводе, можно выводить
  ///  сколько угодно символов на каждый байт
  ///  FSelectionPositions - координаты выделения символа.
  ///  Должен содержать размеры в которых будут помещаться все символы ByteStringLength * CharSize

  /// <summary>
  ///  The class describes the representation of the text output
  ///  FCharPositions - the size of each character when outputting,
  ///  you can output as many characters as you like per byte
  ///  FSelectionPositions - coordinates of the symbol selection.
  ///  Must contain the dimensions in which all characters will fit ByteStringLength * CharSize
  /// </summary>

  { TAbstractTextMetric }

  TAbstractTextMetric = class
  strict private
    FOwner: TFWCustomHexView;
    procedure InitBuff;
    function IsOpcode(AColumn: TColumnType): Boolean; {$ifndef fpc} inline; {$endif}
  protected
    FCharPositions: array [Boolean] of TIntegerDynArray;
    FSelectionPositions: array [Boolean] of TIntegerDynArray;
    procedure UpdateCharPosition(BytesInRow, CharWidth: Integer); virtual; abstract;
    procedure UpdateCharPositionDef(BytesInRow, CharWidth: Integer);
    procedure UpdateCharPositionText(BytesInRow, CharWidth: Integer);
    procedure UpdateSelection(BytesInRow, CharWidth: Integer); virtual;
    property Owner: TFWCustomHexView read FOwner;
  public
    constructor Create(AOwner: TFWCustomHexView); virtual;

    ///  CharCount - возвращает количество символов которые будут выделены
    ///  под отображение указанного количества реальных байт RawLength
    ///  с учетом текущих метрик текста

    /// <summary>
    ///  CharCount - returns the number of characters that will be allocated
    ///  to display the specified number of real RawLength bytes given
    //   the current text metrics
    /// </summary>
    function CharCount(AColumn: TColumnType; RawLength: Integer): Integer;

    function CharIndexToValueOffset(AColumn: TColumnType; CharIndex: Integer): Integer;

    ///  CharLength - возвращает размер в пикселях между AStart, AEnd

    /// <summary>
    ///  CharLength - returns the size in pixels between AStart, AEnd
    /// </summary>
    function CharLength(AColumn: TColumnType; AStart, AEnd: Integer): Integer; virtual;

    ///  CharPointer - указатель на массив кернингов символов

    /// <summary>
    ///  CharPointer - pointer to an array of character kerning
    /// </summary>
    function CharPointer(AColumn: TColumnType; AIndex: Integer): PInteger; virtual;

    ///  CharPointerLength - размер массива кернингов символов

    /// <summary>
    ///  CharPointer - character kerning array size
    /// </summary>
    function CharPointerLength(AColumn: TColumnType; AIndex: Integer): Integer; virtual;

    ///  CharWidth - возвращает размер символа в пикселях

    /// <summary>
    ///  CharWidth - returns the character size in pixels
    /// </summary>
    function CharWidth(AColumn: TColumnType; AIndex: Integer): Integer; virtual;
    function FieldCount(RawLength: Integer): Integer;
    function InitCharCount(AColumn: TColumnType): Integer;
    function SelectionLength(AColumn: TColumnType; AStart, AEnd: Integer): Integer; virtual;
    function SelectionPosition(AColumn: TColumnType; AIndex: Integer): Integer; virtual;
    procedure Update; virtual;
    function ValueOffsetToCharIndex(AColumn: TColumnType; ValueOffset: Integer): Integer;
    function ValueMetric: TValueMetric; virtual; abstract;
    function ValueWidthInChar(AColumn: TColumnType): Integer;
  end;

  TAbstractTextMetricClass = class of TAbstractTextMetric;

  TCharEncoderType = (
    cetDefault, cetAnsi, cetAscii, cetUnicode, cetBigEndianUnicode,
    cetUTF7, cetUTF8, cetCodePage, cetEncodingName
  );

  { TCharEncoder }

  TCharEncoder = class(TPersistent)
  strict private
    FOwner: TFWCustomHexView;
    FEncodingName, FEncodingDisplayName: string;
    FEncodeType: TCharEncoderType;
    FCodePage: Integer;
    FCPEncoding, FNameEncoding: TEncoding;
    {$IFDEF UNIX}
    FAnsiEncoding: TEncoding;
    {$ENDIF}
    {$IFDEF FPC}
    FLazUTF8Encoding: TEncoding;
    {$ENDIF}
    function CheckCodePage(Value: Integer): Boolean;
    function CheckEncodingName(const Value: string): Boolean;
    function GetEncoding: TEncoding;
    function GetUTF8Encoding: TEncoding;
    procedure SetEncodeType(const Value: TCharEncoderType);
    procedure SetCodePage(const Value: Integer);
    procedure SetEncodingName(const Value: string);
    procedure UpdateDisplayName;
  protected
    procedure DoChange;
  public
    constructor Create(AOwner: TFWCustomHexView);
    destructor Destroy; override;
    function EncodeBuff(const Buff: TBytes): string;
    function EncodeChar(AChar: TNativeChar): TBytes;
  published

    ///  CodePage - номер кодовой страницы кодировки текста.
    ///  Должен быть указан ДО переключения в режим cetCodePage

    /// <summary>
    ///  CodePage - number of the text encoding code page.
    ///  Must be specified BEFORE switching to cetCodePage mode
    /// </summary>
    property CodePage: Integer read FCodePage write SetCodePage default 0;

    ///  EncodeType - типы кодировки текста

    /// <summary>
    ///  EncodeType - text encoding types
    /// </summary>
    property EncodeType: TCharEncoderType read FEncodeType write SetEncodeType default cetDefault;

    ///  EncodingName - наименование кодировки текста.
    ///  Должен быть указан ДО переключения в режим cetEncodingName

    /// <summary>
    ///  EncodingName - text encoding name.
    ///  Must be specified BEFORE switching to cetEncodingName mode
    /// </summary>
    property EncodingName: string read FEncodingName write SetEncodingName;
    property DisplayName: string read FEncodingDisplayName;
  end;

  TColorMode = (cmAuto, cmLight, cmDark, cmCustom);

  { THexViewColorMap }

  THexViewColorMap = class(TPersistent)
  strict private
    FEditBackgroundColor: TColor;
    FEditBorderColor: TColor;
    FEditErrorBorderColor: TColor;
    FEditTextColor: TColor;
    FOwner: TFWCustomHexView;
    FColorMode: TColorMode;
    FBackgroundColor: TColor;
    FBookmarkBackgroundColor: TColor;
    FBookmarkBorderColor: TColor;
    FBookmarkTextColor: TColor;
    FCaretColor: TColor;
    FCaretTextColor: TColor;
    FGroupColor: TColor;
    FHeaderBackgroundColor: TColor;
    FHeaderBorder: TColor;
    FHeaderColumnSeparatorColor: TColor;
    FHeaderTextColor: TColor;
    FInfoBackgroundColor: TColor;
    FInfoBorderColor: TColor;
    FInfoTextColor: TColor;
    FRowSeparatorColor: TColor;
    FSelectColor: TColor;
    FSelectInactiveColor: TColor;
    FSelectTextContrastDarkColor: TColor;
    FSelectTextContrastLightColor: TColor;
    FTextColor: TColor;
    FTextCommentColor: TColor;
    FWorkSpaceTextColor: TColor;
    procedure InternalDoChange;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBookmarkBackgroundColor(const Value: TColor);
    procedure SetBookmarkBorderColor(const Value: TColor);
    procedure SetBookmarkTextColor(const Value: TColor);
    procedure SetCaretColor(const Value: TColor);
    procedure SetCaretTextColor(const Value: TColor);
    procedure SetColorMode(const Value: TColorMode);
    procedure SetEditBackgroundColor(AValue: TColor);
    procedure SetEditBorderColor(AValue: TColor);
    procedure SetEditErrorBorderColor(AValue: TColor);
    procedure SetEditTextColor(AValue: TColor);
    procedure SetGroupColor(const Value: TColor);
    procedure SetHeaderBackgroundColor(const Value: TColor);
    procedure SetHeaderBorder(const Value: TColor);
    procedure SetHeaderColumnSeparatorColor(const Value: TColor);
    procedure SetHeaderTextColor(const Value: TColor);
    procedure SetInfoBackgroundColor(const Value: TColor);
    procedure SetInfoBorderColor(const Value: TColor);
    procedure SetInfoTextColor(const Value: TColor);
    procedure SetRowSeparatorColor(const Value: TColor);
    procedure SetSelectColor(const Value: TColor);
    procedure SetSelectInactiveColor(const Value: TColor);
    procedure SetSelectTextContrastDarkColor(const Value: TColor);
    procedure SetSelectTextContrastLightColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    procedure SetTextCommentColor(const Value: TColor);
    procedure SetWorkSpaceTextColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChange;
    procedure InitDarkMode; virtual;
    procedure InitDefault;
    procedure InitLightMode; virtual;
    function IsColorStored: Boolean;
  public
    constructor Create(AOwner: TFWCustomHexView); virtual;
    function IsDarkMode: Boolean;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor stored IsColorStored;
    property BookmarkBackgroundColor: TColor read FBookmarkBackgroundColor write SetBookmarkBackgroundColor stored IsColorStored;
    property BookmarkBorderColor: TColor read FBookmarkBorderColor write SetBookmarkBorderColor stored IsColorStored;
    property BookmarkTextColor: TColor read FBookmarkTextColor write SetBookmarkTextColor stored IsColorStored;
    property CaretColor: TColor read FCaretColor write SetCaretColor stored IsColorStored;
    property CaretTextColor: TColor read FCaretTextColor write SetCaretTextColor stored IsColorStored;
    property ColorMode: TColorMode read FColorMode write SetColorMode default cmAuto;
    property EditBackgroundColor: TColor read FEditBackgroundColor write SetEditBackgroundColor;
    property EditBorderColor: TColor read FEditBorderColor write SetEditBorderColor;
    property EditErrorBorderColor: TColor read FEditErrorBorderColor write SetEditErrorBorderColor;
    property EditTextColor: TColor read FEditTextColor write SetEditTextColor;
    property GroupColor: TColor read FGroupColor write SetGroupColor stored IsColorStored;
    property InfoBackgroundColor: TColor read FInfoBackgroundColor write SetInfoBackgroundColor stored IsColorStored;
    property InfoBorderColor: TColor read FInfoBorderColor write SetInfoBorderColor stored IsColorStored;
    property InfoTextColor: TColor read FInfoTextColor write SetInfoTextColor stored IsColorStored;
    property HeaderBackgroundColor: TColor read FHeaderBackgroundColor write SetHeaderBackgroundColor stored IsColorStored;
    property HeaderBorderColor: TColor read FHeaderBorder write SetHeaderBorder stored IsColorStored;
    property HeaderColumnSeparatorColor: TColor read FHeaderColumnSeparatorColor write SetHeaderColumnSeparatorColor stored IsColorStored;
    property HeaderTextColor: TColor read FHeaderTextColor write SetHeaderTextColor stored IsColorStored;
    property RowSeparatorColor: TColor read FRowSeparatorColor write SetRowSeparatorColor stored IsColorStored;
    property SelectColor: TColor read FSelectColor write SetSelectColor stored IsColorStored;
    property SelectInactiveColor: TColor read FSelectInactiveColor write SetSelectInactiveColor stored IsColorStored;
    property SelectTextContrastDarkColor: TColor read FSelectTextContrastDarkColor write SetSelectTextContrastDarkColor stored IsColorStored;
    property SelectTextContrastLightColor: TColor read FSelectTextContrastLightColor write SetSelectTextContrastLightColor stored IsColorStored;
    property TextColor: TColor read FTextColor write SetTextColor stored IsColorStored;
    property TextCommentColor: TColor read FTextCommentColor write SetTextCommentColor stored IsColorStored;
    property WorkSpaceTextColor: TColor read FWorkSpaceTextColor write SetWorkSpaceTextColor stored IsColorStored;
  end;

  THexViewColorMapClass = class of THexViewColorMap;

  // класс предоставляет общий шлюз к овнеру для всех пайнтеров

  // class provides a common gateway to the owner for all pinters

  TBasePainter = class
  strict private
    FOwner: TFWCustomHexView;
    FRawData: TRawData;
    function GetColWidth(Value: TColumnType): Integer; inline;
  protected
    function AddressToRowIndex(Value: Int64): Int64; inline;
    function AddressToSelectPoint(Value: Int64): TSelectPoint; inline;
    function AddressMode: TAddressMode; inline;
    function AddressView: TAddressView; inline;
    function AddressViewOffsetBase: Int64; inline;
    function BookMark(AIndex: TBookMark): Int64; inline;
    function BytesInRow: Integer; inline;
    function CharWidth: Integer; inline;
    function ClientHeight: Integer; inline;
    function ClientWidth: Integer; inline;
    function ColorMap: THexViewColorMap; inline;
    function Columns: TFWHexViewColumnTypes; inline;
    function CurrentPPI: Integer; inline;
    procedure DoQueryComment(AddrVA: Int64; AColumn: TColumnType; var AComment: string); inline;
    function DrawColumnSeparator: Boolean; inline;
    function Encoder: TCharEncoder; inline;
    function Focused: Boolean; inline;
    function GetLeftNCWidth: Integer; inline;
    procedure GetRawBuff(ARowIndex: Int64; var Data: TBytes); inline;
    function GetRowOffset(ARowIndex: Int64): Int64; inline;
    function GetSelectData(ARowIndex: Int64): TSelectData; inline;
    function GetSelectDataWithSelection(ARowIndex: Int64; ASelStart, ASelEnd: TSelectPoint): TSelectData; inline;
    function HeaderVisible: Boolean; inline;
    function HeaderWidth: Integer; inline;
    function IsAddrVisible(AAddrVA: Int64): Boolean; inline;
    function IsRowVisible(ARowIndex: Int64): Boolean; inline;
    function MakeDrawRect(LeftOffset, TopOffset, AWidth: Integer): TRect; inline;
    function MakeSelectRect(LeftOffset, TopOffset, SelectWidth: Integer): TRect; inline;
    function NoDataText: string; inline;
    function ReadOnly: Boolean; inline;
    function RowHeight: Integer; {$ifndef fpc} inline; {$endif}
    function RowToAddress(ARowIndex: Int64; ValueOffset: Integer): Int64; inline;
    function RowVisible(ARowIndex: Int64): Boolean; inline;
    function ScrollOffset: TLargePoint; inline;
    function Selections: TSelections; inline;
    function SeparateGroupByColor: Boolean; inline;
    function SplitMargin: Integer; inline;
    function TextMargin: Integer; {$ifndef fpc} inline; {$endif}
    function TextMetric: TAbstractTextMetric; virtual;
    function ToDpi(Value: Integer): Integer; inline;
    function VisibleRowDiapason: TVisibleRowDiapason; inline;
  protected
    function ByteViewMode: TByteViewMode; virtual;
    function CharCount(AColumn: TColumnType): Integer; virtual;
    function CharIndexToValueOffset(AColumn: TColumnType; ACharIndex: Integer): Integer;
    procedure DefaultDrawHeaderColumn(ACanvas: TCanvas; var ARect: TRect;
      const ACaption: string; Flags: DWORD = DT_CENTER);
    function SelectedColor(SelectStyle: TSelectStyle): TColor;
    function ValueOffsetToCharIndex(AColumn: TColumnType; AValueOffset: Integer): Integer;
  protected
    property ColumnWidth[Value: TColumnType]: Integer read GetColWidth;
    property Owner: TFWCustomHexView read FOwner;
    property RawData: TRawData read FRawData;
  public
    constructor Create(AOwner: TFWCustomHexView); virtual;
  end;

  { TAbstractPrimaryRowPainter }

  TAbstractPrimaryRowPainter = class(TBasePainter)
  strict private
    FRowIndex: Int64;
    FSelData: TSelectData;
    procedure InternalDrawTextBlock(ACanvas: TCanvas; AColumn: TColumnType;
      const ARect: TRect; const DrawString: string; Dx: PInteger;
      CheckDxSize: Boolean);
    procedure SetRowIndex(const Value: Int64);
  protected
    function AcceptEdit(AColumn: TColumnType): Boolean;
    function AcceptSelection: Boolean; virtual;
    function CalcEditParam(ACaretPosData: TCaretPosData; var Offset: TPoint;
      out EditChar: string): Boolean; virtual;
    function CalcLeftNCWidth: Integer; virtual;

    ///  Количество символов в колонке при копировании в буффер обмена

    /// <summary>
    ///  Number of characters per column when copying to clipboard
    /// </summary>
    function CalcColumnLengthForCopy(AColumn: TColumnType): Integer; virtual;
    function CaretKeyIncrement(AColumn: TColumnType): Integer;
    function CaretEditMode(AColumn: TColumnType): TCaretEditMode; virtual;
    function CharCount(AColumn: TColumnType): Integer; override;
    function ColumnAsString(AColumn: TColumnType): string; virtual;

    ///  Указывает какие колонки заголовка пайнтер может рисовать самостоятельно

    /// <summary>
    ///  Specifies which header columns the painter can draw independently
    /// </summary>
    function ColumnsDrawSupport: TFWHexViewColumnTypes; virtual;
    function ColumnRect(const Offset: TPoint; AColumn: TColumnType): TRect;
    procedure CorrectCanvasFont({%H-}ACanvas: TCanvas; {%H-}AColumn: TColumnType); virtual;

    ///  Форматирует текущую строку для копирования в буфер

    /// <summary>
    ///  Formats the current string to be copied to the buffer
    /// </summary>
    procedure CopyRowAsString(Builder: TSimplyStringBuilder); virtual;
    procedure DoDrawToken(ACanvas: TCanvas; ATokenParam: TDrawParam;
      const ARect: TRect; AToken: PChar; var ATokenLen: Integer); virtual;
    procedure DrawAddress(ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawAlignedTextPart(ACanvas: TCanvas; AColumn: TColumnType;
      const Text: string; const ARect: TRect);
    procedure DrawBackground(ACanvas: TCanvas;
      MousePressed: Boolean; const AHitInfo: TMouseHitInfo);
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); virtual; abstract;
    procedure DrawColumnBackground(ACanvas: TCanvas;AColumn: TColumnType;
      var ARect: TRect; InResizeState: Boolean); virtual;
    procedure DrawComment(ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawHeaderColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); virtual;
    procedure DrawRow(ACanvas: TCanvas; var Offset: TPoint);
    function DrawRowColumnBackground(ACanvas: TCanvas;
      AColumn: TColumnType; const ARect: TRect): Boolean; virtual;
    procedure DrawTextBlock(ACanvas: TCanvas; AColumn: TColumnType;
      const ARect: TRect; const DrawString: string; Dx: PInteger);
    procedure DrawWorkSpace(ACanvas: TCanvas; var ARect: TRect); virtual;
    function FormatMode: TFormatMode; virtual;

    ///  Форматирует колонку в текстовое представления для копирования

    /// <summary>
    ///  Formats the column into a text representation for copying
    /// </summary>
    function FormatRowColumn(AColumn: TColumnType;
      const Value: string): string; virtual;
    function GetHeaderColumnCaption(AColumn: TColumnType): string; virtual;
    procedure GetHitInfo(var AMouseHitInfo: TMouseHitInfo); virtual;
    function GetTextMetricClass: TAbstractTextMetricClass; virtual; abstract;
  protected
    property RowIndex: Int64 read FRowIndex write SetRowIndex;
    property SelData: TSelectData read FSelData;
  end;

  TPrimaryRowPainterClass = class of TAbstractPrimaryRowPainter;

  TDefaultTextMetric = class(TAbstractTextMetric)
  strict private
    FColorGroup: TList<TRect>;
  protected
    function ByteViewMode: TByteViewMode; virtual;
    procedure UpdateCharPosition(BytesInRow, CharWidth: Integer); override;
    procedure UpdateColorGroup(BytesInRow, CharWidth: Integer); virtual;
  public
    constructor Create(AOwner: TFWCustomHexView); override;
    destructor Destroy; override;
    procedure Update; override;
    property ColorGroup: TList<TRect> read FColorGroup;
    function ValueMetric: TValueMetric; override;
  end;

  TColorMixMode = (cmmNone, cmmLight, cmmDark, cmmMix);
  TColorMixFontMode = (cmfmNone, cmfmLight, cmfmDark);

  TRowHexPainter = class(TAbstractPrimaryRowPainter)
  private
    FCanvasChanged: Boolean;
    FCanvasFontChange: TNotifyEvent;
    FColorMix: array of Longint;
    FColorMixColumn: TColumnType;
    FColorMixMode: TColorMixMode;
    FColorMixFontMode: TColorMixFontMode;
    procedure DoCanvasFontChange(Sender: TObject);
  protected
    procedure InitColorMix(AColumn: TColumnType);
    procedure AddToColorMix(SelStart, SelEnd: Integer; AColor: TColor; Primary: Boolean);
    procedure AddSelDataToColorMix(ASelData: TSelectData; AColor: TColor; Primary: Boolean);
    procedure DrawColorMix(ACanvas: TCanvas; var ARect: TRect);
  protected
    function AcceptSelection: Boolean; override;
    function CalcEditParam(ACaretPosData: TCaretPosData; var Offset: TPoint;
     out EditChar: string): Boolean; override;
    function CalcLeftNCWidth: Integer; override;
    function CaretEditMode(AColumn: TColumnType): TCaretEditMode; override;
    function ColumnsDrawSupport: TFWHexViewColumnTypes; override;
    procedure CorrectCanvasFont({%H-}ACanvas: TCanvas; {%H-}AColumn: TColumnType); override;
    procedure DoDrawToken(ACanvas: TCanvas; ATokenParam: TDrawParam;
      const ARect: TRect; AToken: PChar; var ATokenLen: Integer); override;
    procedure DrawColorGroup(ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    procedure DrawDataPart(ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawHeaderColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    procedure DrawHexPart(ACanvas: TCanvas; var ARect: TRect); virtual;
    procedure DrawSelectedBackround(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect; ASelData: TSelectData);
    function GetTextMetricClass: TAbstractTextMetricClass; override;
    function GetHeaderColumnCaption(AColumn: TColumnType): string; override;
    procedure GetHitInfo(var AHitInfo: TMouseHitInfo); override;
  end;

  TSecondaryRowPainter = class(TAbstractPrimaryRowPainter)
  protected
    function GetTextMetricClass: TAbstractTextMetricClass; override;
  end;

  TAbstractPostPainter = class(TBasePainter)
  public
    procedure PostPaint(ACanvas: TCanvas; StartRow, EndRow: Int64;
      var Offset: TPoint); virtual; abstract;
  end;

  { Описание структуры:
  TDrawLineParam = record
    DirectionDown: Boolean;   // направление линии (для выбора цвета)
    RowFrom,                  // строка с которой идет отрисовка линии
    RowTo: Int64;             // строка до которой идет отрисовка линии
    LineIndent,               // расстояние между линиями в пикселях
    LineVerticalMargin,       // вертикальные смещения для линий при выходе за границу области отрисовки
    LineWidth: Integer;       // толщина линии
    DrawArrow,                // нужно ли рисовать глиф стрелки на концах линий
    DrawAlwais,               // рисовать нужно даже если обе строки не видимы
    DrawOnlySelectedArrow,    // флаг отрисовки линий только для выделеных элементов
    SecondDraw: Boolean;      // признак повторной отрисовки
    LineColor: TColor;        // переопределенный цвет линии (clDefault по умолчанию)
    Offset: TPoint;           // текущий оффсет передаваемый в пост пайнтер
  end;
  }

  TDrawLineParam = record
    DirectionDown: Boolean;   // line direction (for color selection)
    RowFrom,                  // line from which the line is drawn
    RowTo: Int64;             // the line up to which the line is drawn
    LineIndent,               // distance between lines in pixels
    LineVerticalMargin,       // vertical offsets for lines when they go outside the drawing area boundary
    LineWidth: Integer;       // line width
    DrawArrow,                // whether to draw arrow glyphs at the ends of lines
    DrawAlwais,               // should be drawn even if both lines are not visible
    DrawOnlySelectedArrow,    // flag to draw lines only for selected elements
    SecondDraw: Boolean;      // flag of repeated drawing
    LineColor: TColor;        // redefined line color (clDefault by default)
    Offset: TPoint;           // current offset passed to the postpainter
  end;

  TLinesPostPainter = class(TAbstractPostPainter)
  protected
    PaintedLinesCount: Integer;
    procedure DrawArrowWithOffset(ACanvas: TCanvas; Direction: TScrollDirection;
      Location: TPoint; ArrowSize: Integer; StartPoint: Boolean);
    function DrawLine(ACanvas: TCanvas; const Param: TDrawLineParam): Boolean;
    function IsNeedOffsetRow(ARowIndex: Int64; FirstRow: Boolean): Boolean; virtual;
    function LineColorPresent(Selected: Boolean; const Param: TDrawLineParam;
      out LineColor: TColor): Boolean; virtual;
  end;

  TBookmarkPostPainter = class(TAbstractPostPainter)
  public
    procedure PostPaint(ACanvas: TCanvas; StartRow, EndRow: Int64;
      var Offset: TPoint); override;
  end;

  THeaderColumn = record
    Caption: string;
    MaxWidth,
    MinWidth,
    Width: Integer;
  end;

  TCustomHexViewHeader = class(TPersistent)
  strict private
    FOwner: TFWCustomHexView;
    FColumns: TFWHexViewColumnTypes;
    FColumnsData: array [TColumnType] of THeaderColumn;
    FDrawColumnSeparator: Boolean;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure DoWidthChange(AType: TColumnType);
    procedure DrawHeaderColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect);
    function GetColCaption(AType: TColumnType): string;
    function GetColMaxWidth(AType: TColumnType): Integer;
    function GetColMinWidth(AType: TColumnType): Integer;
    function GetColWidth(AType: TColumnType): Integer;
    procedure SetColCaption(AType: TColumnType; const NewCaption: string);
    procedure SetColumns(const Value: TFWHexViewColumnTypes);
    procedure SetColWidth(AType: TColumnType; AWidth: Integer);
    procedure SetColMaxWidth(AType: TColumnType; AWidth: Integer);
    procedure SetColMinWidth(AType: TColumnType; AWidth: Integer);
    procedure SetDrawColumnSeparator(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure UpdateWidth;
  protected
    procedure InitDefault; virtual;
    procedure Paint(ACanvas: TCanvas; AScrollXOffset: Integer);
  public
    constructor Create(AOwner: TFWCustomHexView);
    property ColumnCaption[AType: TColumnType]: string read GetColCaption write SetColCaption;
    property ColumnMaxWidth[AType: TColumnType]: Integer read GetColMaxWidth write SetColMaxWidth;
    property ColumnMinWidth[AType: TColumnType]: Integer read GetColMinWidth write SetColMinWidth;
    property ColumnWidth[AType: TColumnType]: Integer read GetColWidth write SetColWidth;
    property Width: Integer read FWidth;
  published
    property Columns: TFWHexViewColumnTypes read FColumns write SetColumns default [ctAddress, ctOpcode, ctDescription];
    property DrawColumnSeparator: Boolean read FDrawColumnSeparator write SetDrawColumnSeparator default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  THeaderClass = class of TCustomHexViewHeader;

  THexViewHeader = class(TCustomHexViewHeader)
  published
    property Columns;
    property DrawColumnSeparator;
    property Visible;
  end;

  TQueryStringEvent = procedure(Sender: TObject; AddrVA: Int64; AColumn: TColumnType; var AComment: string) of object;
  TDrawColumnBackgroundEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARowParam: TDrawParam; const ARect: TRect; var Handled: Boolean) of object;
  TDrawTokenEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ATokenParam: TDrawParam; const ARect: TRect;
    AToken: PChar; var ATokenLen: Integer) of object;

  TEditParam = record
    OldValue, NewValue: Int64;
    ValueSize: Integer;
    // if Size > 8 then use ExtValue
    OldExtValue, NewExtValue: array of Byte;
  end;

  TEditEvent = procedure(Sender: TObject; const ACursor: TDrawParam;
    const AData: TEditParam; var Handled: Boolean) of object;

  TEditMenuId = (emiUndo, emiCut, emiCopy, emiPaste, emiDelete, emiSelectAll, emiSeparator);

  THexViewInplaceEdit = class
  private type
    TUndoType = (utNone, utDelete, utPaste);
    TUndoSnapshot = record
      Text: string;
      SelStart, SelEnd: Integer;
      WaitUpdate: Boolean;
    end;
  private
    FActiveUndoType: TUndoType;
    FCaretPos, FCaretDownPos: Integer;
    FCharIndex: Integer;
    FData: TBytes;
    FDefaultMenu: TPopupMenu;
    FEditRect, FTextRect: TRect;
    FFormatMode: TFormatMode;
    FInvalidState: Boolean;
    FModifyed: Boolean;
    FOwner: TFWCustomHexView;
    FSelLength, FSelStart: Integer;
    FText, FInitialText: string;
    FPainter: TAbstractPrimaryRowPainter;
    FRowIndex: Int64;
    FTextMetric: TAbstractTextMetric;
    FValueOffset, FValueSize: Integer;
    FViewMode: TByteViewMode;
    FVisible: Boolean;
    FUndoSnapshot: array [Boolean] of TUndoSnapshot;
    FUndoSnapshotReverted: Boolean;
    procedure InitPopupMenu;
    procedure InternalDeleteSelected(L, R: Integer);
    procedure InternalSetText(const Value: string);
    procedure OnMenuItemClick(Sender: TObject);
    procedure SetCaretPos(Value: Integer);
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetVisible(const Value: Boolean);
  protected
    function ConvertLocalIndexToRowCharIndex(Value: Integer): Integer;
    function ConvertLocalIndexToValueIndex(Value: Integer): Integer;
    function ConvertRowCharIndexToLocalIndex(Value: Integer): Integer;
    function ConvertTextToEditParam(const AText: string;
      out AData: TEditParam): Boolean;
    function DoKeyBackspace(L, R: Integer): Boolean;
    function DoKeyDelete(L, R: Integer): Boolean;
    function DoKeyChar(L, R: Integer; AChar: TNativeChar): Boolean;
    function DoKeyCopy(L, R: Integer): Boolean;
    function DoKeyEnd(L, R: Integer; Shift: TShiftState): Boolean;
    function DoKeyHome(L, R: Integer; Shift: TShiftState): Boolean;
    function DoKeyLeft(L, R: Integer; Shift: TShiftState): Boolean;
    function DoKeyPaste: Boolean;
    function DoKeyRight(L, R: Integer; Shift: TShiftState): Boolean;
    procedure GetSelection(out L, R: Integer);
    procedure Invalidate;
    function LeftValue(Shift: TShiftState; AValue: Integer): Integer;
    function RightValue(Shift: TShiftState; AValue: Integer): Integer;
    procedure SetNewSelection(L, R: Integer; CaretAtLeft: Boolean);
    procedure TrackPopupMenuAtPos(MousePos: TPoint);
    procedure UpdateSelection(AStart, ALen, ACaret: Integer);
    function UndoSnapshotCalcDelValue(L, R: Integer): Integer;
    function UndoSnapshotNeedReset: Boolean;
    procedure UndoSnapshotMake(AUndoType: TUndoType);
    procedure UndoSnapshotRevert;
    procedure UndoSnapshotUpdate(AUndoType: TUndoType; AValue: Integer);
  protected
    function CalcCharAtPoint(X, Y: Integer): Integer; virtual;
    procedure CalcEditRect; virtual;
    function EditColumn: TColumnType; virtual;
    function GetMenuCaption(AId: TEditMenuId): string; virtual;
    procedure Hide(Apply: Boolean = True); virtual;
    function HandleChar(AChar: TNativeChar; Shift: TShiftState): Boolean; virtual;
    function HandleKeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
    function HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function HandleMouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function HandleMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure PaintTo(ACanvas: TCanvas); virtual;
    procedure Show(APainter: TAbstractPrimaryRowPainter); virtual;
    function Validate(const ANewText: string): Boolean; virtual;
  public
    constructor Create(AOwner: TFWCustomHexView); virtual;
    destructor Destroy; override;
    function CanUndo: Boolean;
    procedure Clear;
    procedure ClearSelection;
    procedure ClearUndo;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure SetSelText(const Value: string);
    procedure Undo;
    property CaretPos: Integer read FCaretPos write SetCaretPos;
    property EditRect: TRect read FEditRect;
    property Modifyed: Boolean read FModifyed write FModifyed;
    property Owner: TFWCustomHexView read FOwner;
    property SelStart: Integer read FSelStart write SetSelStart;
    property SelLength: Integer read FSelLength write SetSelLength;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  THexViewInplaceEditClass = class of THexViewInplaceEdit;

  TViewShortCut = class(TPersistent)
  private
    FDefault: TShortCut;
    FSecondaryShortCuts: TShortCutList;
    FShortCut: TShortCut;
    function IsSecondaryStored: Boolean;
    function IsShortSutStored: Boolean;
    procedure SetSecondaryShortCuts(const Value: TShortCutList);
    function GetSecondaryShortCuts: TShortCutList;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function IsCustomViewShortCutStored: Boolean;
  public
    constructor Create(ADefault: TShortCut);
    destructor Destroy; override;
    function IsShortCut(Key: Word; Shift: TShiftState): Boolean;
  published
    property ShortCut: TShortCut read FShortCut write FShortCut stored IsShortSutStored;
    property SecondaryShortCuts: TShortCutList read GetSecondaryShortCuts write SetSecondaryShortCuts stored IsSecondaryStored;
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

  TJmpState = (jsQueryJump, jsJmpPushToStack, jsJmpUndo, jsJmpRedo, jsJmpDone);
  TJmpToEvent = procedure(Sender: TObject; const AJmpAddr: Int64;
    AJmpState: TJmpState; var Handled: Boolean) of object;

  { TJmpItem }

  TJmpItem = record
    JmpAddr: Int64;
    SelLength: Integer;
    JmpFrom: Int64;
    SelStart, SelEnd: TSelectPoint;
  end;

  TJumpStack = class
  strict private
    FCurrent: TJmpItem;
    FStack: TList<TJmpItem>;
    FUndoIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AValue: TJmpItem): Integer;
    function CanRedo: Boolean;
    function CanUndo: Boolean;
    procedure Clear;
    function Redo: Boolean;
    function Undo: Boolean;
    property Current: TJmpItem read FCurrent;
    property UndoIndex: Integer read FUndoIndex;
  end;

  THintParam = record
    AddrVA: Int64;
    MouseHitInfo: TMouseHitInfo;
    HintInfo: PHintInfo;
  end;

  TGetHintEvent = procedure(Sender: TObject; const Param: THintParam;
    var Hint: string) of object;

  { TFWCustomHexView }

  TFWCustomHexView = class(TCustomControl, IHexViewCopyAction, IHexViewByteViewModeAction)
  private const
    NoDpiMultipier = 42;
    NoDpiBorderMargin = 2;
    NoDpiTextMargin = 8;
    NoDpiSplitMargin = 3;
    CHAR_STRING = 'W';
  strict private
    FAcceptSelForced: Boolean;
    FAddressMode: TAddressMode;
    FAddressView: TAddressView;
    FAddressViewAutoToggle: Boolean;
    FAddressViewOffsetBase: Int64;
    FBookMarks: array [TBookMark] of Int64;
    FBorderStyle: TBorderStyle;
    FBytesInGroup, FBytesInColorGroup, FBytesInRow: Integer;
    FByteViewMode: TByteViewMode;
    FCaretPosData: TCaretPosData;
    FCaretTimerEnabled: Boolean;
    FCharWidth: Integer;
    FColorMap: THexViewColorMap;
    FDataStream: TStream;
    FDefaultFontColorIsDark: Boolean;
    FDefaultPainter: TAbstractPrimaryRowPainter;
    FEncoder: TCharEncoder;
    FHeader: TCustomHexViewHeader;
    FHideSelection: Boolean;
    FHintHideTimeout, FHintShowPause: Integer;
    FInplaceEdit: THexViewInplaceEdit;
    FJumpStack: TJumpStack;
    FRowHeight: Integer;
    FShortCuts: TViewShortCuts;
    FLastDiapasone: TVisibleRowDiapason;
    FMeasureCanvas: TBitmap;
    FMinColumnWidth: Integer;
    FMousePressed: Boolean;
    FMousePressedHitInfo: TMouseHitInfo;
    FNeedFitBesSizes: Boolean;
    FNoDataText: string;
    FPainters: TObjectList<TAbstractPrimaryRowPainter>;
    FPostPainters: TObjectList<TAbstractPostPainter>;
    FPreviosCharWidth: Integer;
    FProcessMenu: Boolean;
    FQueryStringEvent: TQueryStringEvent;
    FRawData: TRawData;
    FReadOnly: Boolean;
    FSavedShift: TShiftState;
    FScrollBars: TScrollStyle;
    FScrollOffset, FTextBoundary: TLargePoint;
    FSelections: TSelections;
    FSelectOnMouseDown: Boolean;
    FSelectOnMouseMove: Boolean;
    FSelStart, FSelEnd: TSelectPoint;
    FSelStartAddr, FSelEndAddr: Int64;
    FSeparateGroupByColor: Boolean;
    FShiftSelectionInit: Boolean;
    FSplitMargin: Integer;
    FStartAddress: Int64;
    FStreamOwnerShip: TStreamOwnership;
    FTextMargin: Integer;
    FTextMetric: TAbstractTextMetric;
    FTextMetrics: TObjectList<TAbstractTextMetric>;
    FUpdateCount: Integer;
    FWheelMultiplier, FSystemWheelMultiplier: Integer;
    FOnCaretPosChange: TNotifyEvent;
    FOnEditContextPopup: TContextPopupEvent;
    FOnDrawColBack: TDrawColumnBackgroundEvent;
    FOnDrawToken: TDrawTokenEvent;
    FOnEdit: TEditEvent;
    FOnGetHint: TGetHintEvent;
    FOnJmpTo: TJmpToEvent;
    FOldOnFontChange: TNotifyEvent;
    FSelectionChange: TNotifyEvent;
    procedure DoChangeScale(BeforeScaleStep: Boolean);
    function GetBookMark(AIndex: TBookMark): Int64;
    function GetTextExtent: TSize;
    function SelectPoint(ARowIndex: Int64; ACharIndex: Integer;
      AColumn: TColumnType): TSelectPoint;
    function SetNewEditRowIndex(ANewRowIndex: Int64): Int64;
    procedure InternalClear;
    procedure InternalKillCaretTimer;
    procedure InvalidateSelections;
    function IsFontStored: Boolean;
    function IsShortCutsStored: Boolean;
    procedure ReleaseDataStream;
    procedure SetAddressMode(const Value: TAddressMode);
    procedure SetAddressView(const Value: TAddressView);
    procedure SetAddressViewOffsetBase(const Value: Int64);
    procedure SetBookMark(AIndex: TBookMark; const Value: Int64);
    procedure SetBytesInColorGroup(const Value: Integer);
    procedure SetBytesInGroup(const Value: Integer);
    procedure SetBytesInRow(const Value: Integer);
    procedure SetByteViewMode(Value: TByteViewMode);
    procedure SetColorMap(const Value: THexViewColorMap);
    procedure SetCtlBorderStyle(const Value: TBorderStyle);
    procedure SetDefaultPainter(const Value: TAbstractPrimaryRowPainter);
    procedure SetEncoder(const Value: TCharEncoder);
    procedure SetHeader(const Value: TCustomHexViewHeader);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetNoDataText(const Value: string);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSelEnd(const Value: Int64);
    procedure SetSelStart(const Value: Int64);
    procedure SetSeparateGroupByColor(const Value: Boolean);
    procedure SetShortCuts(const Value: TViewShortCuts);
    function Scroll32To64(Value: Integer): Int64;
    function Scroll64To32(Value: Int64): Integer;
    procedure OnSelectionsChange(Sender: TObject);
    procedure UpdateSavedShift(Shift: TShiftState);
    procedure UpdateSystemWheelMultiplier;
    procedure UpdateTextDarknessColor;
    procedure UpdateTextExtent;
    procedure UpdateTextMetrics;
  protected
    {$IFDEF FPC}
    FCurrentPPI: Integer;
    {$ENDIF}
    procedure ChangeScale(M, D: Integer{$IFNDEF FPC}; isDpiChange: Boolean{$ENDIF}); override;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMHintShowPause(var Message: TCMHintShowPause); message CM_HINTSHOWPAUSE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DestroyHandle; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UTF8KeyPress(var UTF8Key: TNativeChar); {$IFDEF FPC}override;{$ENDIF}
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    {$IFNDEF FPC}
    procedure WMXButtonDown(var Msg: TWMMouse); message WM_XBUTTONDOWN;
    {$ENDIF}
  protected
    // IHexViewCopyAction
    function CopyCommandEnabled(Value: TCopyStyle): Boolean; virtual;
    function CopyCommandHandled(Value: TCopyStyle): Boolean; virtual;
    // IHexViewByteViewModeAction
    function ByteViewModeCommandEnabled(Value: TByteViewMode; var AChecked: Boolean): Boolean; virtual;
    function ByteViewModeCommandHandled(Value: TByteViewMode): Boolean; virtual;
  protected

    // Работа с кареткой и выделением

    // Working with carret and selection

    function ColumnResetSelection(AColumn: TColumnType): Boolean; virtual;
    procedure CreateCaretTimer;
    procedure DestroyCaretTimer;
    procedure DoCaretLeft(Shift: TShiftState);
    procedure DoCaretRight(Shift: TShiftState);
    procedure DoCaretDown(Shift: TShiftState; ForcedLine: Integer = 0);
    procedure DoCaretUp(Shift: TShiftState; ForcedLine: Integer = 0);
    procedure DoCaretPageDown(Shift: TShiftState);
    procedure DoCaretPageUp(Shift: TShiftState);
    procedure DoCaretHome(Shift: TShiftState);
    procedure DoCaretEdit(AKey: TNativeChar); virtual;
    procedure DoCaretEnd(Shift: TShiftState);
    procedure DoCaretKeyDown(var Key: Word; Shift: TShiftState); virtual;
    function DoInplaceEditHandleKeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure DoInvalidateRange(AStartRow, AEndRow: Int64); virtual;
    procedure DoOnEdit(const ACursor: TDrawParam; const AData: TEditParam;
      var Handled: Boolean);
    function GetCaretChangeMode(APainter: TAbstractPrimaryRowPainter;
      AColumn: TColumnType; Shift: TShiftState): TCaretChangeMode; virtual;
    function GetCaretNextRowIndex(FromIndex: Int64; AColumn: TColumnType = ctNone): Int64; virtual;
    function GetCaretPreviosRowIndex(FromIndex: Int64; AColumn: TColumnType = ctNone): Int64; virtual;
    function GetSelectedRawBuff(ASelStartRow, ASelEndRow: Int64): TBytes;
    function GetSelectedRow(out ASelStartRow, ASelEndRow: Int64): Boolean;
    function IgnoreSelectionWhenCopyAddress: Boolean; virtual;
    procedure InvalidateCaretPosData(NewState: Boolean);
    procedure UpdateCaretColumn(AColumn: TColumnType);
    procedure UpdateCaretPosData(Value: TSelectPoint; AChangeMode: TCaretChangeMode);
    procedure UpdateCaretTimer;
    procedure UpdateCursor(var AHitInfo: TMouseHitInfo);
    procedure UpdateSelection(ANewStart, ANewEnd: TSelectPoint; AUpdateSelAddr: Boolean = True);
    procedure UpdateSelectionAddr(ANewStart, ANewEnd: Int64);

    // внутренние события

    // internal events

    procedure DoBeforePaint(const ADiapason: TVisibleRowDiapason); virtual;
    procedure DoChange(ChangeCode: Integer); virtual;
    function DoDrawRowColumnBackground(ACanvas: TCanvas;
      APainter: TAbstractPrimaryRowPainter; AColumn: TColumnType; const ARect: TRect): Boolean;
    procedure DoDrawToken(ACanvas: TCanvas; ATokenParam: TDrawParam;
      const ARect: TRect; AToken: PChar; var ATokenLen: Integer); virtual;
    procedure DoEditContextPopup(MousePos: TPoint; var Handled: Boolean); virtual;
    procedure DoEncodingChange;
    procedure DoFontChange(Sender: TObject);
    procedure DoGetHint(var AHintParam: THintParam; var AHint: string); virtual;
    procedure DoJmpTo(AAddrVA: Int64; AJmpState: TJmpState; var Handled: Boolean);
    procedure DoQueryString(AddrVA: Int64; AColumn: TColumnType; var AComment: string);
    procedure DoSelectionChage(AStartAddr, AEndAddr: Int64); virtual;
    function DoLButtonDown(const AHitInfo: TMouseHitInfo): Boolean; virtual;

    // функции получения текстовых данных для отрисовки и для копирования в буфер

    // functions for obtaining text data for drawing and for copying to the buffer

    function GetDataStreamSize: Int64; virtual;
    procedure GetRawBuff(ARowIndex: Int64; var Data: TBytes); virtual;

    // рассчеты для отрисовки

    // rendering calculations

    function CalculateColumnBestSize(Value: TColumnType): Integer; virtual;
    function GetColumnRect(AColumnType: TColumnType; ARowIndex: Int64): TRect;
    function GetLeftNCWidth: Integer;
    function GetPageHeight: Integer;
    function GetRowOffset(ARowIndex: Int64): Int64;
    function GetRowOffsetPoint(ARowIndex: Int64): TPoint;
    function GetSelectData(ARowIndex: Int64): TSelectData;
    function GetSelectDataWithSelection(ARowIndex: Int64; ASelStart, ASelEnd: TSelectPoint): TSelectData;
    function MeasureCanvas: TCanvas;
    function RowVisible(ARowIndex: Int64): Boolean;
    procedure RestoreViewParam; virtual;
    function SelectPointInSelection(const Value: TSelectPoint): Boolean;
    procedure SetScrollOffset(X, Y: Int64); virtual;
    procedure SetTopRow(ARowIndex: Int64);
    procedure SaveViewParam;

    // переопредение внутренних классов

    // redefinition of internal classes

    function GetColorMapClass: THexViewColorMapClass; virtual;
    function GetDefaultCaretChangeMode: TCaretChangeMode; virtual;
    function GetDefaultFontName: string; virtual;
    function GetDefaultFontHeight: Integer; virtual;
    function GetDefaultPainterClass: TPrimaryRowPainterClass; virtual;
    function GetHeaderClass: THeaderClass; virtual;
    function GetInplaceEditClass: THexViewInplaceEditClass; virtual;
    function GetRawDataClass: TRawDataClass; virtual;
    function GetShortCutsClass: TViewShortCutsClass; virtual;
    function GetOverloadPainterClass(Value: TPrimaryRowPainterClass): TPrimaryRowPainterClass; virtual;

    // непосредственно отрисовка

    // direct rendering

    procedure DrawEditMark(var Offset: TPoint);
    procedure DrawRows(StartRow, EndRow: Int64; var Offset: TPoint);
    procedure DrawRowSmallSeparator(var Offset: TPoint);
    function GetRowPainter(ARowIndex: Int64; TempPainter: Boolean = False): TAbstractPrimaryRowPainter;
    procedure InitDefault; virtual;
    procedure InitPainters; virtual;
    function InternalGetRowPainter(ARowIndex: Int64): TAbstractPrimaryRowPainter; virtual;
    procedure InvalidateRow(ARowIndex: Int64);
    function MakeDrawRect(LeftOffset, TopOffset, ColumnWidth: Integer): TRect;
    procedure ResetCanvas;
    procedure ResetPainters;

    // утилитарные методы под наследников и пайнтеров

    // utilitarian methods for childs and painters

    function GetHitInfo(XPos, YPos: Int64; AShift: TShiftState): TMouseHitInfo;
    function IsColorMapStored: Boolean; virtual;
    function LeftSelPoint: TSelectPoint;
    function RightSelPoint: TSelectPoint;
    procedure RebuildData;
    procedure RegisterTextMetric(Value: TAbstractTextMetric);

    // валидаторы

    // validators

    procedure UpdateDataMap; virtual;
    procedure UpdateTextBoundary;
    procedure UpdateScrollPos;
    procedure UpdateScrollY(AOffset: Int64); virtual;
    procedure UpdateVerticalScrollPos; virtual;
    procedure UpdateView; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddressToRowIndex(Value: Int64): Int64;
    function AddressToSelectPoint(Value: Int64): TSelectPoint;
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy; const AFromPPI,
      AToPPI, AOldFormWidth, ANewFormWidth: Integer); {$IFDEF FPC}override;{$ENDIF}
    procedure BeginUpdate;
    function CaretPosToAddress(const Value: TCaretPosData): Int64;
    procedure ClearSelection(ResetCaretPos: Boolean = True);
    function ColumnAsString(ARowIndex: Int64; AColumn: TColumnType): string;
    procedure CopySelected(CopyStyle: TCopyStyle); virtual;
    procedure CopySelectedToStream(AStream: TStream);
    function CurrentVisibleRow: Int64;
    function EditAtCaretPos: Boolean;
    procedure EndUpdate;
    function IsAddrVisible(AAddrVA: Int64): Boolean;
    function IsRowVisible(ARowIndex: Int64): Boolean;
    procedure FillDrawMetrics(out AValue: TDrawMetrics);
    procedure FitColumnToBestSize(Value: TColumnType);
    procedure FitColumnsToBestSize;
    function Focused: Boolean; override;

    ///  Перемещает скролл делая чтобы адрес (если строка с ним не видима)
    ///  находился вверху текущего окна отображения

    /// <summary>
    ///  Moves the scroll to make the address (if it is not visible)
    ///  at the top of the current display window.
    /// </summary>
    procedure FocusOnAddress(Address: Int64; ACaretChangeMode: TCaretChangeMode);

    ///  Перемещает скролл делая чтобы строка стала видимой, результат перемещения
    ///  не определен, если невидимая строка выше текущего окна, она появится
    ///  сверху. Если ниже - она появится снизу.

    /// <summary>
    ///  Moves the scroll to make the string visible, the result of the move
    ///  is undefined, if the invisible string is above the current window,
    ///  it will appear at the top. If it is lower, it will appear at the bottom.
    /// </summary>
    procedure FocusOnRow(ARowIndex: Int64; ACaretChangeMode: TCaretChangeMode);

    procedure JumpClear;
    function JumpToAddress(AJmpAddr: Int64; ASelLength: Integer = 0): Boolean;
    function JumpToBookmark(ABookmark: TBookMark): Boolean;
    function JumpRedo: Boolean;
    function JumpUndo: Boolean;

    function PrefferededSize: TLargePoint;

    ///  Общий метод чтения данных с начала выделения вьювера

    /// <summary>
    ///  A common method for reading data from the beginning of a viewer selection
    /// </summary>
    function ReadDataAtSelStart(var pBuffer; nSize: Integer): Integer; virtual;

    ///  Сброс настроек вьювера на значения по умолчанию

    /// <summary>
    ///  Reset viewer settings to default values
    /// </summary>
    procedure ResetViewState;
    function RowRawLength(ARowIndex: Int64): Integer;
    function RowToAddress(ARowIndex: Int64; ValueOffset: Integer): Int64;
    function SelectedColumnAsString(AColumn: TColumnType): string;
    function SelectedRawLength: Integer;
    function SelectedRowIndex: Int64;
    function SelectPointToAddress(const AValue: TSelectPoint): Int64;
    procedure SetDataStream(Value: TStream; StartAddress: Int64;
      AOwnerShip: TStreamOwnership = soReference);
    procedure SetCaretPos(AColumn: TColumnType; ARowIndex: Int64; ACharIndex: Integer);
    function ToDpi(Value: Integer): Integer;
    function VisibleRowDiapason: TVisibleRowDiapason;
    function VisibleRowCount: Integer;
    function WheelRowCount: Integer;
    procedure WndProc(var AMsg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); override;
    /// <summary>
    ///  Use this method to change the font size instead of Font.Size := XXX.
    ///  This call will automatically recalculate the size of the columns.
    ///  Example Usage:
    ///  Zoom(1) - will increase Font.Size by one
    ///  Zoom(-3) - decreases Font.Size by three
    /// </summary>
    procedure Zoom(AZoomValue: Integer);
  public
    property AddressViewOffsetBase: Int64 read FAddressViewOffsetBase write SetAddressViewOffsetBase;
    property Bookmark[AIndex: TBookMark]: Int64 read GetBookMark write SetBookMark;
    property CaretPosData: TCaretPosData read FCaretPosData;
    {$IFDEF FPC}
    property CurrentPPI: Integer read FCurrentPPI;
    {$ENDIF}
    property DataStream: TStream read FDataStream;
    property InplaceEdit: THexViewInplaceEdit read FInplaceEdit;
    property Selections: TSelections read FSelections;
    property SelEnd: Int64 read FSelEndAddr write SetSelEnd;
    property SelStart: Int64 read FSelStartAddr write SetSelStart;
  protected
    // свойства для пейнтеров и наследников

    // properties for painters and childs
    property CharWidth: Integer read FCharWidth;
    property DefaultFontColorIsDark: Boolean read FDefaultFontColorIsDark;
    property DefaultPainter: TAbstractPrimaryRowPainter read FDefaultPainter write SetDefaultPainter;
    property MinColumnWidth: Integer read FMinColumnWidth;
    property MousePressedHitInfo: TMouseHitInfo read FMousePressedHitInfo;
    property Painters: TObjectList<TAbstractPrimaryRowPainter> read FPainters;
    property PostPainters: TObjectList<TAbstractPostPainter> read FPostPainters;
    property RawData: TRawData read FRawData;
    property RowHeight: Integer read FRowHeight;
    property SavedShift: TShiftState read FSavedShift;
    property ScrollOffset: TLargePoint read FScrollOffset;
    property SplitMargin: Integer read FSplitMargin;
    property StartAddress: Int64 read FStartAddress;
    property TextBoundary: TLargePoint read FTextBoundary;
    property TextMargin: Integer read FTextMargin;
    property TextMetric: TAbstractTextMetric read FTextMetric;
  protected

    ///  Позволяет устанавливать выделение на колонки
    ///  не поддерживающие редактирование, при перемещении курсора с клавиатуры

    /// <summary>
    ///  Allows you to set a selection on columns that do not support editing
    ///  when moving the cursor from the keyboard
    /// </summary>
    property AcceptKeySelectionAllways: Boolean read FAcceptSelForced write FAcceptSelForced default True;
    property AddressMode: TAddressMode read FAddressMode write SetAddressMode default am32bit;
    property AddressView: TAddressView read FAddressView write SetAddressView default avHex;

    ///  Автоматически переключает AddressView между avHex-avOffset
    ///  при двойном клике на колонке адресов

    /// <summary>
    ///  Automatically toggles AddressView between avHex-avOffset
    ///  when double-clicking on an address column
    /// </summary>
    property AddressViewAutoToggle: Boolean read FAddressViewAutoToggle write FAddressViewAutoToggle default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetCtlBorderStyle default bsSingle;
    property BytesInColorGroup: Integer read FBytesInColorGroup write SetBytesInColorGroup default 4;
    property BytesInGroup: Integer read FBytesInGroup write SetBytesInGroup default 8;
    property BytesInRow: Integer read FBytesInRow write SetBytesInRow default 16;
    property ByteViewMode: TByteViewMode read FByteViewMode write SetByteViewMode default bvmHex8;
    property ColorMap: THexViewColorMap read FColorMap write SetColorMap stored IsColorMapStored;
    property Encoder: TCharEncoder read FEncoder write SetEncoder;
    property Font stored IsFontStored;
    property Header: TCustomHexViewHeader read FHeader write SetHeader;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default False;
    property HintHideTimeout: Integer read FHintHideTimeout write FHintHideTimeout default 7000;
    property HintShowPause: Integer read FHintShowPause write FHintShowPause default 300;
    property NoDataText: string read FNoDataText write SetNoDataText;
    property ParentFont default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default True;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default TScrollStyle.ssBoth;
    property SelectOnMouseDown: Boolean read FSelectOnMouseDown write FSelectOnMouseDown default True;
    property SelectOnMouseMove: Boolean read FSelectOnMouseMove write FSelectOnMouseMove default True;
    property SeparateGroupByColor: Boolean read FSeparateGroupByColor write SetSeparateGroupByColor default True;
    property ShortCuts: TViewShortCuts read FShortCuts write SetShortCuts stored IsShortCutsStored;
    property TabStop default True;
    property WheelMultiplier: Integer read FWheelMultiplier write FWheelMultiplier default 0;
    property OnCaretPosChange: TNotifyEvent read FOnCaretPosChange write FOnCaretPosChange;
    property OnEditContextPopup: TContextPopupEvent read FOnEditContextPopup write FOnEditContextPopup;
    property OnDrawColumnBackground: TDrawColumnBackgroundEvent read FOnDrawColBack write FOnDrawColBack;
    property OnDrawToken: TDrawTokenEvent read FOnDrawToken write FOnDrawToken;
    property OnEdit: TEditEvent read FOnEdit write FOnEdit;
    property OnHint: TGetHintEvent read FOnGetHint write FOnGetHint;
    property OnJmpTo: TJmpToEvent read FOnJmpTo write FOnJmpTo;
    property OnQueryComment: TQueryStringEvent read FQueryStringEvent write FQueryStringEvent;
    property OnSelectionChange: TNotifyEvent read FSelectionChange write FSelectionChange;
  end;

  TFWHexView = class(TFWCustomHexView)
  published
    property AddressMode;
    property AddressView;
    property AddressViewAutoToggle;
    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property BytesInColorGroup;
    property BytesInGroup;
    property BytesInRow;
    property ByteViewMode;
    property ColorMap;
    property Constraints;
    property Cursor;
    property Enabled;
    property Encoder;
    property Font;
    property Header;
    property HideSelection;
    property HintShowPause;
    property NoDataText;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
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

  function DblSize(Value: Integer): Integer; inline;
  function DblSizeDec(Value: Integer): Integer; inline;
  function CheckNegative(Value: Int64): Int64; inline;

{$IFDEF USE_PROFILER}
var
  NeedProfile: Boolean;
{$ENDIF}

implementation

const
  AddrModeOffsetSeparator = 2;
  {$IFDEF FPC}
  USER_DEFAULT_SCREEN_DPI = 96;
  GetCaretBlinkTime = 530;
  {$ENDIF}

function DblSize(Value: Integer): Integer;
begin
  Result := Value + Value;
end;

function DblSizeDec(Value: Integer): Integer;
begin
  if Value = 0 then
    Result := 0
  else
    Result := Value + Value - 1;
end;

function CheckNegative(Value: Int64): Int64;
begin
  if Value < 0 then
    Result := 0
  else
    Result := Value;
end;

{ TRawData }

function TRawData.Address: Int64;
begin
  Result := FRowAddress;
end;

function TRawData.AddressToRowIndex(Value: Int64): Int64;
begin
  if (Value < FAddress) or (Value >= FAddress + FRawLength) then
    Result := -1
  else
    Result := (Value - FAddress) div Owner.BytesInRow;
end;

procedure TRawData.Clear;
begin
  FCount := 0;
end;

function TRawData.Color: TColor;
begin
  Result := Owner.ColorMap.TextColor;
end;

function TRawData.Comment: string;
begin
  Result := '';
end;

function TRawData.Count: Int64;
begin
  Result := FCount;
end;

constructor TRawData.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
end;

function TRawData.DataOffset: Int64;
begin
  Result := FDataOffset;
end;

function TRawData.DrawRowSmallSeparator: Boolean;
begin
  Result := False;
end;

function TRawData.GetDataAtRowIndex(ARowIndex: Int64): TRawData;
begin
  FRowIndex := ARowIndex;
  FDataOffset := ARowIndex * Owner.BytesInRow;
  FRowAddress := FAddress + FDataOffset;
  FRowRawLength := FRawLength;
  Dec(FRowRawLength, FDataOffset);
  if FRowRawLength < 0 then
    FRowRawLength := 0;
  FRowRawLength := Min(FRowRawLength, Owner.BytesInRow);
  Result := Self;
end;

function TRawData.RawLength: Int64;
begin
  Result := FRowRawLength;
end;

function TRawData.RowToAddress(ARowIndex: Int64; ValueOffset: Integer): Int64;
var
  Tmp: TRawData;
begin
  Tmp := GetDataAtRowIndex(ARowIndex);
  Result := Tmp.Address;
  if Tmp.RawLength = 0 then Exit;
  if ValueOffset < 0 then
    Inc(Result, Tmp.RawLength - 1)
  else
    Inc(Result, Int64(ValueOffset));
end;

procedure TRawData.Update;
{$ifdef dbg_check_last_row}
var
  RawLength: Integer;
{$endif}
begin
  FAddress := Owner.StartAddress;
  FRawLength := Owner.GetDataStreamSize;
  FCount := 1;
  if FRawLength > Owner.BytesInRow then
  begin
    Inc(FCount, FRawLength div Owner.BytesInRow);
    if FRawLength mod Owner.BytesInRow = 0 then
      Dec(FCount);
  end;

  // контроль что последняя строчка добавлена верно
  // и выдает неверные данные по своему размеру

  // control that the last line is added correctly
  // and gives incorrect data for its size
  {$ifdef dbg_check_last_row}
  if FCount > 0 then
  begin
    RawLength := Data[FCount].RawLength;
    Assert(RawLength <= 0);
    Assert(RawLength > -Owner.BytesInRow);
    FRowIndex := 0;
  end;
  {$endif}
end;

{ TFWCustomHexView.TSelectPoint }

function TSelectPoint.InvalidRow: Boolean;
begin
  Result := RowIndex < 0;
end;

function TSelectPoint.ValidSelectedByte: Boolean;
begin
  Result := (RowIndex >= 0) and (ValueOffset >= 0) and (CharIndex >= 0);
end;

class operator TSelectPoint.Equal(A, B: TSelectPoint): Boolean;
begin
  Result :=
    (A.RowIndex = B.RowIndex) and
    (A.ValueOffset = B.ValueOffset) and
    (A.Column = B.Column) and
    (A.CharIndex = B.CharIndex);
end;

procedure TSelectPoint.Erase;
begin
  RowIndex := -1;
  ValueOffset := -1;
  CharIndex := -1;
  Column := ctNone;
end;

class operator TSelectPoint.GreaterThan(A, B: TSelectPoint): Boolean;
begin
  Result := False;
  if (A.RowIndex > B.RowIndex) then
    Exit(True);
  if A.RowIndex = B.RowIndex then
    Result := DWORD(A.ValueOffset) > DWORD(B.ValueOffset);
end;

class operator TSelectPoint.GreaterThanOrEqual(A, B: TSelectPoint): Boolean;
begin
  Result := False;
  if (A.RowIndex > B.RowIndex) then
    Exit(True);
  if A.RowIndex = B.RowIndex then
    Result := DWORD(A.ValueOffset) >= DWORD(B.ValueOffset);
end;

class operator TSelectPoint.LessThan(A,
  B: TSelectPoint): Boolean;
begin
  Result := False;
  if (A.RowIndex < B.RowIndex) then
    Exit(True);
  if A.RowIndex = B.RowIndex then
    Result := DWORD(A.ValueOffset) < DWORD(B.ValueOffset);
end;

class operator TSelectPoint.LessThanOrEqual(A, B: TSelectPoint): Boolean;
begin
  Result := False;
  if (A.RowIndex < B.RowIndex) then
    Exit(True);
  if A.RowIndex = B.RowIndex then
    Result := DWORD(A.ValueOffset) <= DWORD(B.ValueOffset);
end;

class operator TSelectPoint.NotEqual(A,
  B: TSelectPoint): Boolean;
begin
  Result :=
    (A.RowIndex <> B.RowIndex) or
    (A.ValueOffset <> B.ValueOffset) or
    (A.CharIndex <> B.CharIndex) or
    (A.Column <> B.Column);
end;

{ TSelections }

function TSelections.Add(ATag: Integer; ASelStart, ASelEnd: Int64;
  AColor: TColor): Integer;
var
  AItem: TSelection;
begin
  AItem.Tag := ATag;
  AItem.SelStart := ASelStart;
  AItem.SelEnd := ASelEnd;
  AItem.Color := AColor;
  Result := FItems.Add(AItem);
end;

procedure TSelections.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSelections.Clear;
begin
  FVisibleIndex.Clear;
  FItems.Clear;
end;

function TSelections.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TSelections.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  FItems := TListEx<TSelection>.Create;
  FItems.OnNotify := ListNotification;
  FVisibleIndex := TList<Integer>.Create;
end;

procedure TSelections.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

destructor TSelections.Destroy;
begin
  FVisibleIndex.Free;
  FItems.Free;
  inherited;
end;

procedure TSelections.DoChange;
begin
  if Assigned(FChange) then
    FChange(Self);
end;

procedure TSelections.DropSelectionsAtTag(ATag: Integer);
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    if FItems.List[I].Tag = ATag then
      FItems.Delete(I);
end;

procedure TSelections.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChange;
end;

function TSelections.GetItem(Index: Integer): TSelection;
begin
  Result := FItems[Index];
end;

procedure TSelections.ListNotification(Sender: TObject;
  {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Item: TSelection;
  Action: TCollectionNotification);
begin
  if Action in [cnAdded, cnExtracted, cnRemoved] then
    DoChange;
end;

procedure TSelections.UpdateVisibleSelections;

  function Check(const D: TVisibleRowDiapason; AddrVA: Int64): Boolean;
  begin
    AddrVA := FOwner.AddressToRowIndex(AddrVA);
    Result := (AddrVA >= D.StartRow) and (AddrVA <= D.EndRow);
  end;

var
  I: Integer;
  Diapason: TVisibleRowDiapason;
begin
  Diapason := FOwner.VisibleRowDiapason;
  FVisibleIndex.Clear;
  for I := 0 to Count - 1 do
    if Check(Diapason, FItems.List[I].SelStart) or Check(Diapason, FItems.List[I].SelEnd) then
      FVisibleIndex.Add(I);
end;

{ TFWCustomHexView.TMouseHitInfo }

procedure TMouseHitInfo.Erase;
begin
  CursorPos := TPoint.Zero;
  ScrolledCursorPos := TPoint.Zero;
  SelectPoint.Erase;
  Shift := [];
  Elements := [];
  ColumnWidth := 0;
  Cursor := crDefault;
end;

{ TAbstractTextMetric }

function TAbstractTextMetric.CharCount(AColumn: TColumnType;
  RawLength: Integer): Integer;
begin
  Result := FieldCount(RawLength) * ValueWidthInChar(AColumn);
end;

function TAbstractTextMetric.CharIndexToValueOffset(AColumn: TColumnType;
  CharIndex: Integer): Integer;
begin
  Result := CharIndex div ValueWidthInChar(AColumn) * ValueMetric.ByteCount;
end;

function TAbstractTextMetric.CharLength(AColumn: TColumnType; AStart,
  AEnd: Integer): Integer;
var
  I: Integer;
  ColIdx: Boolean;
begin
  Result := 0;
  ColIdx := IsOpcode(AColumn);
  for I := AStart to AEnd do
    Inc(Result, FCharPositions[ColIdx][I]);
end;

function TAbstractTextMetric.CharPointer(AColumn: TColumnType;
  AIndex: Integer): PInteger;
begin
  Result := @FCharPositions[IsOpcode(AColumn)][AIndex];
end;

function TAbstractTextMetric.CharPointerLength(AColumn: TColumnType;
  AIndex: Integer): Integer;
begin
  Result := Length(FCharPositions[IsOpcode(AColumn)]) - AIndex;
end;

function TAbstractTextMetric.CharWidth(AColumn: TColumnType;
  AIndex: Integer): Integer;
begin
  Result := FCharPositions[IsOpcode(AColumn)][AIndex];
end;

constructor TAbstractTextMetric.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  Owner.RegisterTextMetric(Self);
end;

function TAbstractTextMetric.FieldCount(RawLength: Integer): Integer;
begin
  Result := RawLength div ValueMetric.ByteCount;
  if RawLength mod ValueMetric.ByteCount > 0 then
    Inc(Result);
end;

procedure TAbstractTextMetric.InitBuff;

  procedure EraseArray(var Value: TIntegerDynArray);
  begin
    FillChar(Value[0], Length(Value) * SizeOf(Integer), 0);
  end;

var
  AFieldCount, Len: Integer;
begin
  // рассчет сколько элементов можно отобразить c учетом что
  // последнему может не хватить места

  // calculation of how many elements can be displayed,
  // taking into account that the last one may not have enough space.

  AFieldCount := FieldCount(Owner.BytesInRow);
  SetLength(FCharPositions[True], AFieldCount * ValueMetric.CharCount);

  // остальные данные будут иметь размер с учетом последнего элемента
  // даже если он не может поместиться целиком, причем
  // чтобы не переусложнять код, под Description всегда выделяется
  // фиксированное число символов, даже если их используется меньше

  // the rest of the data will be sized taking into account the last element
  // even if it cannot fit in its entirety, and in order not to overcomplicate
  // the code, a fixed number of characters is always allocated for Description,
  // even if fewer characters are used

  Len := AFieldCount * ValueMetric.ByteCount;
  SetLength(FCharPositions[False], Len);
  SetLength(FSelectionPositions[False], Len);
  SetLength(FSelectionPositions[True], Len);

  // Обязательная очистка

  // Mandatory cleaning

  EraseArray(FCharPositions[True]);
  EraseArray(FCharPositions[False]);
  EraseArray(FSelectionPositions[True]);
  EraseArray(FSelectionPositions[False]);
end;

function TAbstractTextMetric.InitCharCount(AColumn: TColumnType): Integer;
begin
  Result := Length(FCharPositions[IsOpcode(AColumn)]);
end;

function TAbstractTextMetric.IsOpcode(AColumn: TColumnType): Boolean;
begin
  Result := AColumn = ctOpcode;
end;

function TAbstractTextMetric.SelectionLength(AColumn: TColumnType; AStart,
  AEnd: Integer): Integer;
var
  I: Integer;
  ColIdx: Boolean;
begin
  Result := 0;
  ColIdx := IsOpcode(AColumn);
  for I := AStart to AEnd do
    Inc(Result, FSelectionPositions[ColIdx][I]);
end;

function TAbstractTextMetric.SelectionPosition(AColumn: TColumnType;
  AIndex: Integer): Integer;
begin
  Result := FSelectionPositions[IsOpcode(AColumn)][AIndex];
end;

procedure TAbstractTextMetric.Update;
var
  BytesInRow, CharWidth: Integer;
begin
  InitBuff;
  BytesInRow := Owner.BytesInRow;
  CharWidth := Owner.CharWidth;
  UpdateCharPosition(BytesInRow, CharWidth);
  UpdateSelection(BytesInRow, CharWidth);
end;

procedure TAbstractTextMetric.UpdateCharPositionDef(BytesInRow,
  CharWidth: Integer);
var
  I, ByteIndex, LastIndex: Integer;
begin
  for I := 0 to Length(FCharPositions[False]) - 1 do
    FCharPositions[False][I] := CharWidth;
  ByteIndex := 0;
  LastIndex := Length(FCharPositions[True]) - 1;
  for I := 0 to LastIndex do
  begin
    if (I + 1) mod ValueMetric.CharCount = 0 then
    begin
      FCharPositions[True][I] := DblSize(CharWidth);
      Inc(ByteIndex, ValueMetric.ByteCount);
      if ByteIndex >= Owner.BytesInGroup then
      begin
        Inc(FCharPositions[True][I], Owner.TextMargin);
        Dec(ByteIndex, Owner.BytesInGroup);
      end;
    end
    else
      FCharPositions[True][I] := CharWidth;
  end;
  FCharPositions[True][LastIndex] := CharWidth;
end;

procedure TAbstractTextMetric.UpdateCharPositionText(BytesInRow,
  CharWidth: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(FCharPositions[False]) - 1 do
    FCharPositions[False][I] := CharWidth;
  for I := 0 to Length(FCharPositions[True]) - 1 do
    FCharPositions[True][I] := CharWidth;
end;

procedure TAbstractTextMetric.UpdateSelection(BytesInRow, CharWidth: Integer);
var
  I, A, Index, DoubleWidth, LastCharIndex: Integer;
  Buff: array of Integer;
begin
  // центрируем

  // Centering

  SetLength(Buff, InitCharCount(ctOpcode));
  Buff[0] := FCharPositions[True][0];
  LastCharIndex := Length(Buff) - 1;
  for I := 1 to LastCharIndex do
  begin
    if I mod ValueMetric.CharCount = 0 then
    begin
      DoubleWidth := FCharPositions[True][I] + FCharPositions[True][I - 1];
      Buff[I - 1] := DoubleWidth shr 1;
      Buff[I] := DoubleWidth - Buff[I - 1];
    end
    else
      Buff[I] := FCharPositions[True][I];
  end;
  Buff[LastCharIndex] := FCharPositions[True][LastCharIndex];

  // раскидываем позиции начала и конца выделения для всех режимов отображения

  // spread the positions of selection start and end for all display modes

  Index := 0;
  I := 0;
  while I < Owner.BytesInRow do
  begin
    for A := 0 to ValueMetric.CharCount - 1 do
    begin
      Inc(FSelectionPositions[True][I], Buff[Index]);
      Inc(Index);
    end;
    for A := 0 to ValueMetric.ByteCount - 1 do
      Inc(FSelectionPositions[False][I], FCharPositions[False][I + A]);
    Inc(I, ValueMetric.ByteCount);
  end;
end;

function TAbstractTextMetric.ValueOffsetToCharIndex(AColumn: TColumnType;
  ValueOffset: Integer): Integer;
begin
  Result :=
    ValueOffset div ValueMetric.ByteCount * ValueWidthInChar(AColumn);
end;

function TAbstractTextMetric.ValueWidthInChar(AColumn: TColumnType): Integer;
begin
  if IsOpcode(AColumn) then
    Result := ValueMetric.CharCount
  else
    Result := ValueMetric.ByteCount;
end;

{ TCharEncoder }

function TCharEncoder.CheckCodePage(Value: Integer): Boolean;
var
  E: TEncoding;
begin
  try
    E := TEncoding.GetEncoding(Value);
    Result := E <> nil;
  except
    Result := False;
    E := nil;
  end;
  if Result then
  begin
    FCPEncoding.Free;
    FCPEncoding := E;
  end;
end;

function TCharEncoder.CheckEncodingName(const Value: string): Boolean;
var
  E: TEncoding;
begin
  try
    E := TEncoding.GetEncoding(UnicodeString(Value));
    Result := E <> nil;
  except
    Result := False;
    E := nil;
  end;
  if Result then
  begin
    FNameEncoding.Free;
    FNameEncoding := E;
  end;
end;

constructor TCharEncoder.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  UpdateDisplayName;
end;

destructor TCharEncoder.Destroy;
begin
  FCPEncoding.Free;
  FNameEncoding.Free;
  {$IFDEF UNIX}
  FAnsiEncoding.Free;
  {$ENDIF}
  {$IFDEF FPC}
  FLazUTF8Encoding.Free;
  {$ENDIF}
  inherited;
end;

procedure TCharEncoder.DoChange;
begin
  UpdateDisplayName;
  FOwner.DoEncodingChange;
end;

function TCharEncoder.EncodeBuff(const Buff: TBytes): string;

  function CharVisible(AChar: WideChar): Boolean;
  begin
    Result := (AChar >= #$20) and (AChar < #$F000);
  end;

  function GetEmpty: UnicodeString;
  begin
    Result := UnicodeString(StringOfChar('.', Length(Buff)));
  end;

var
  E: TEncoding;
  BuffLen, ByteIndex, GlyphLen, MaxLen: Integer;
  EncodedString: UnicodeString;
  EncodedChar: TUnicodeCharArray;
begin
  E := GetEncoding;
  if E = nil then
    Exit(string(GetEmpty));

  BuffLen := Length(Buff);
  if E.IsSingleByte then
  begin
    EncodedString := E.GetString(Buff, 0, BuffLen);
    for ByteIndex := 1 to BuffLen do
      if not CharVisible(EncodedString[ByteIndex]) then
        EncodedString[ByteIndex] := '.';
    Result := string(EncodedString);
    Exit;
  end;

  EncodedString := GetEmpty;
  MaxLen := E.GetMaxByteCount({$IFDEF FPC}1{$ELSE}0{$ENDIF});
  ByteIndex := 0;
  repeat
    GlyphLen := 1;
    while E.GetCharCount(Buff, ByteIndex, GlyphLen) = 0 do
    begin
      Inc(GlyphLen);
      if GlyphLen > MaxLen then
      begin
        GlyphLen := 1;
        Inc(ByteIndex);
        Continue;
      end;
      if ByteIndex + GlyphLen > BuffLen then
      begin
        Result := string(EncodedString);
        Exit;
      end;
    end;
    EncodedChar := E.GetChars(Buff, ByteIndex, GlyphLen);
    if CharVisible(EncodedChar[0]) then
      EncodedString[ByteIndex + 1] := EncodedChar[0];
    Inc(ByteIndex, GlyphLen);
  until ByteIndex >= BuffLen;
  Result := string(EncodedString);
end;

function TCharEncoder.EncodeChar(AChar: TNativeChar): TBytes;
var
  E: TEncoding;
begin
  E := GetEncoding;
  if E <> nil then
    Result := E.GetBytes(AChar{%H-});
end;

function TCharEncoder.GetEncoding: TEncoding;
begin
  case EncodeType of
    cetAnsi:
    begin
      {$IFDEF UNIX}
        if FAnsiEncoding = nil then
          FAnsiEncoding := TEncoding.GetEncoding(1251);
        Result := FAnsiEncoding;
      {$ELSE}
        Result := TEncoding.ANSI;
      {$ENDIF}
    end;
    cetAscii: Result := TEncoding.ASCII;
    cetUnicode: Result := TEncoding.Unicode;
    cetBigEndianUnicode: Result := TEncoding.BigEndianUnicode;
    cetUTF7: Result := TEncoding.UTF7;
    cetUTF8: Result := GetUTF8Encoding;
    cetCodePage: Result := FCPEncoding;
    cetEncodingName: Result := FNameEncoding;
  else
    {$IFDEF FPC}
    Result := GetUTF8Encoding;
    {$ELSE}
    Result := TEncoding.Default;
    {$ENDIF}
  end;
end;

function TCharEncoder.GetUTF8Encoding: TEncoding;
begin
  {$IFDEF FPC}
  if FLazUTF8Encoding = nil then
    FLazUTF8Encoding := TLazUTF8Encoding.Create;
  Result := FLazUTF8Encoding;
  {$ELSE}
  Result := TEncoding.UTF8;
  {$ENDIF}
end;

procedure TCharEncoder.SetCodePage(const Value: Integer);
begin
  if (CodePage <> Value) and CheckCodePage(Value) then
  begin
    FCodePage := Value;
    if EncodeType = cetCodePage then
      DoChange;
  end;
end;

procedure TCharEncoder.SetEncodeType(const Value: TCharEncoderType);
begin
  if EncodeType <> Value then
  begin
    case Value of
      cetCodePage:
        if CheckCodePage(CodePage) then
        begin
          FEncodeType := Value;
          DoChange;
        end;
      cetEncodingName:
        if CheckEncodingName(EncodingName) then
        begin
          FEncodeType := Value;
          DoChange;
        end;
    else
      FEncodeType := Value;
      DoChange;
    end;
  end;
end;

procedure TCharEncoder.SetEncodingName(const Value: string);
begin
  if (EncodingName <> Value) and CheckEncodingName(Value) then
  begin
    FEncodingName := Value;
    if EncodeType = cetEncodingName then
      DoChange;
  end;
end;

procedure TCharEncoder.UpdateDisplayName;
begin
  FEncodingDisplayName := string(GetEncoding.EncodingName);
end;

{ THexViewColorMap }

procedure THexViewColorMap.AssignTo(Dest: TPersistent);
begin
  if Dest is THexViewColorMap then
  begin
    THexViewColorMap(Dest).FColorMode := FColorMode;
    THexViewColorMap(Dest).FBackgroundColor := FBackgroundColor;
    THexViewColorMap(Dest).FBookmarkBackgroundColor := FBookmarkBackgroundColor;
    THexViewColorMap(Dest).FBookmarkBorderColor := FBookmarkBorderColor;
    THexViewColorMap(Dest).FBookmarkTextColor := FBookmarkTextColor;
    THexViewColorMap(Dest).FCaretColor := FCaretColor;
    THexViewColorMap(Dest).FCaretTextColor := FCaretTextColor;
    THexViewColorMap(Dest).FGroupColor := FGroupColor;
    THexViewColorMap(Dest).FHeaderBackgroundColor := FHeaderBackgroundColor;
    THexViewColorMap(Dest).FHeaderBorder := FHeaderBorder;
    THexViewColorMap(Dest).FHeaderColumnSeparatorColor := FHeaderColumnSeparatorColor;
    THexViewColorMap(Dest).FHeaderTextColor := FHeaderTextColor;
    THexViewColorMap(Dest).FInfoBackgroundColor := FInfoBackgroundColor;
    THexViewColorMap(Dest).FInfoBorderColor := FInfoBorderColor;
    THexViewColorMap(Dest).FInfoTextColor := FInfoTextColor;
    THexViewColorMap(Dest).FRowSeparatorColor := FRowSeparatorColor;
    THexViewColorMap(Dest).FSelectColor := FSelectColor;
    THexViewColorMap(Dest).FSelectInactiveColor := FSelectInactiveColor;
    THexViewColorMap(Dest).FSelectTextContrastDarkColor := FSelectTextContrastDarkColor;
    THexViewColorMap(Dest).FSelectTextContrastLightColor := FSelectTextContrastLightColor;
    THexViewColorMap(Dest).FTextColor := FTextColor;
    THexViewColorMap(Dest).FTextCommentColor := FTextCommentColor;
    THexViewColorMap(Dest).FWorkSpaceTextColor := FWorkSpaceTextColor;
  end
  else
    inherited;
end;

constructor THexViewColorMap.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  InitDefault;
end;

procedure THexViewColorMap.DoChange;
begin
  FColorMode := cmCustom;
  InternalDoChange;
end;

procedure THexViewColorMap.InitDarkMode;
begin
  FBackgroundColor := $212121;
  FBookmarkBackgroundColor := clHighlight;
  FBookmarkBorderColor := clHotLight;
  FBookmarkTextColor := clHighlightText;
  FCaretColor := $F6A289;
  FCaretTextColor := clWhite;
  FEditBackgroundColor := $212121;
  FEditBorderColor := $E0E0E0;
  FEditErrorBorderColor := clRed;
  FEditTextColor := $E0E0E0;
  FGroupColor := $303030;
  FSelectInactiveColor := $414141;
  FInfoBackgroundColor := clGray;
  FInfoBorderColor := clWindowFrame;
  FInfoTextColor := clWhite;
  FHeaderBorder := clGrayText;
  FHeaderBackgroundColor := $212121;
  FHeaderColumnSeparatorColor := $8C8C8C;
  FHeaderTextColor := $E0E0E0;
  FRowSeparatorColor := clGray;
  FSelectColor := $505050;
  FSelectTextContrastDarkColor := clBlack;
  FSelectTextContrastLightColor := clWhite;
  FTextColor := $E0E0E0;
  FTextCommentColor := $A0A0A0;
  FWorkSpaceTextColor := clWhite;
end;

procedure THexViewColorMap.InitDefault;
var
  DefColorMode: TColorMode;
begin
  DefColorMode := cmLight;
  if IsDarkMode then
    DefColorMode := cmDark;
  case DefColorMode of
    cmLight: InitLightMode;
    cmDark: InitDarkMode;
  end;
end;

procedure THexViewColorMap.InitLightMode;
begin
  FBackgroundColor := clWindow;
  FBookmarkBackgroundColor := clHighlight;
  FBookmarkBorderColor := clHotLight;
  FBookmarkTextColor := clHighlightText;
  FCaretColor := RGB(31, 31, 255);
  FCaretTextColor := clWhite;
  FGroupColor := RGB(239, 239, 239);
  FEditBackgroundColor := clWindow;
  FEditBorderColor := clHighlight;
  FEditErrorBorderColor := clRed;
  FEditTextColor := clWindowText;
  FSelectInactiveColor := RGB(220, 220, 220);
  FInfoBackgroundColor := clGray;
  FInfoBorderColor := clWindowFrame;
  FInfoTextColor := clWhite;
  FHeaderBorder := clGrayText;
  FHeaderBackgroundColor := clWindow;
  FHeaderColumnSeparatorColor := RGB(140, 140, 140);
  FHeaderTextColor := clWindowText;
  FRowSeparatorColor := clGray;
  FSelectColor := RGB(224, 224, 255);
  FSelectTextContrastDarkColor := clBlack;
  FSelectTextContrastLightColor := clWhite;
  FTextColor := clWindowText;
  FTextCommentColor := clGrayText;
  FWorkSpaceTextColor := clDkGray;
end;

procedure THexViewColorMap.InternalDoChange;
begin
  if Assigned(FOwner) then
    FOwner.DoChange(cmColorMap);
end;

function THexViewColorMap.IsColorStored: Boolean;
begin
  Result := ColorMode = cmCustom;
end;

function THexViewColorMap.IsDarkMode: Boolean;
begin
  case ColorMode of
    cmAuto: Result := IsColorRefDark(ColorToRGB(clWindow));
    cmDark: Result := True;
    cmCustom: Result := IsColorRefDark(ColorToRGB(BackgroundColor));
  else
    Result := False;
  end;
end;

procedure THexViewColorMap.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetBookmarkBackgroundColor(const Value: TColor);
begin
  if FBookmarkBackgroundColor <> Value then
  begin
    FBookmarkBackgroundColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetBookmarkBorderColor(const Value: TColor);
begin
  if FBookmarkBorderColor <> Value then
  begin
    FBookmarkBorderColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetBookmarkTextColor(const Value: TColor);
begin
  if FBookmarkTextColor <> Value then
  begin
    FBookmarkTextColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetCaretColor(const Value: TColor);
begin
  if FCaretColor <> Value then
  begin
    FCaretColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetCaretTextColor(const Value: TColor);
begin
  if FCaretTextColor <> Value then
  begin
    FCaretTextColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetColorMode(const Value: TColorMode);
begin
  if ColorMode <> Value then
  begin
    FColorMode := Value;
    case Value of
      cmAuto: InitDefault;
      cmLight: InitLightMode;
      cmDark: InitDarkMode;
    end;
    InternalDoChange;
  end;
end;

procedure THexViewColorMap.SetEditBackgroundColor(AValue: TColor);
begin
  if FEditBackgroundColor <> AValue then
  begin
    FEditBackgroundColor := AValue;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetEditBorderColor(AValue: TColor);
begin
  if FEditBorderColor <> AValue then
  begin
    FEditBorderColor := AValue;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetEditErrorBorderColor(AValue: TColor);
begin
  if FEditErrorBorderColor <> AValue then
  begin
    FEditErrorBorderColor := AValue;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetEditTextColor(AValue: TColor);
begin
  if FEditTextColor <> AValue then
  begin
    FEditTextColor := AValue;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetGroupColor(const Value: TColor);
begin
  if FGroupColor <> Value then
  begin
    FGroupColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetHeaderBackgroundColor(const Value: TColor);
begin
  if FHeaderBackgroundColor <> Value then
  begin
    FHeaderBackgroundColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetHeaderBorder(const Value: TColor);
begin
  if FHeaderBorder <> Value then
  begin
    FHeaderBorder := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetHeaderColumnSeparatorColor(const Value: TColor);
begin
  if FHeaderColumnSeparatorColor <> Value then
  begin
    FHeaderColumnSeparatorColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetHeaderTextColor(const Value: TColor);
begin
  if FHeaderTextColor <> Value then
  begin
    FHeaderTextColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetInfoBackgroundColor(const Value: TColor);
begin
  if FInfoBackgroundColor <> Value then
  begin
    FInfoBackgroundColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetInfoBorderColor(const Value: TColor);
begin
  if FInfoBorderColor <> Value then
  begin
    FInfoBorderColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetInfoTextColor(const Value: TColor);
begin
  if FInfoTextColor <> Value then
  begin
    FInfoTextColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetRowSeparatorColor(const Value: TColor);
begin
  if FRowSeparatorColor <> Value then
  begin
    FRowSeparatorColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetSelectColor(const Value: TColor);
begin
  if FSelectColor <> Value then
  begin
    FSelectColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetSelectInactiveColor(const Value: TColor);
begin
  if FSelectInactiveColor <> Value then
  begin
    FSelectInactiveColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetSelectTextContrastDarkColor(const Value: TColor);
begin
  if SelectTextContrastDarkColor <> Value then
  begin
    FSelectTextContrastDarkColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetSelectTextContrastLightColor(const Value: TColor);
begin
  if SelectTextContrastLightColor <> Value then
  begin
    FSelectTextContrastLightColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetTextCommentColor(const Value: TColor);
begin
  if FTextCommentColor <> Value then
  begin
    FTextCommentColor := Value;
    DoChange;
  end;
end;

procedure THexViewColorMap.SetWorkSpaceTextColor(const Value: TColor);
begin
  if WorkSpaceTextColor <> Value then
  begin
    FWorkSpaceTextColor := Value;
    DoChange;
  end;
end;

{ TBasePainter }

function TBasePainter.AddressMode: TAddressMode;
begin
  Result := FOwner.AddressMode;
end;

function TBasePainter.AddressToRowIndex(Value: Int64): Int64;
begin
  Result := RawData.AddressToRowIndex(Value);
end;

function TBasePainter.AddressToSelectPoint(Value: Int64): TSelectPoint;
begin
  Result := FOwner.AddressToSelectPoint(Value);
end;

function TBasePainter.AddressView: TAddressView;
begin
  Result := FOwner.AddressView;
end;

function TBasePainter.AddressViewOffsetBase: Int64;
begin
  Result := FOwner.AddressViewOffsetBase;
end;

function TBasePainter.BookMark(AIndex: TBookMark): Int64;
begin
  Result := FOwner.BookMark[AIndex];
end;

function TBasePainter.BytesInRow: Integer;
begin
  Result := FOwner.BytesInRow;
end;

function TBasePainter.ByteViewMode: TByteViewMode;
begin
  Result := FOwner.ByteViewMode;
end;

function TBasePainter.CharCount(AColumn: TColumnType): Integer;
begin
  Result := 0;
end;

function TBasePainter.CharIndexToValueOffset(AColumn: TColumnType;
  ACharIndex: Integer): Integer;
begin
  if AColumn in [ctOpcode, ctDescription] then
    Result := TextMetric.CharIndexToValueOffset(AColumn, ACharIndex)
  else
    Result := 0;
end;

function TBasePainter.CharWidth: Integer;
begin
  Result := FOwner.CharWidth;
end;

function TBasePainter.ClientHeight: Integer;
begin
  Result := FOwner.ClientHeight;
end;

function TBasePainter.ClientWidth: Integer;
begin
  Result := FOwner.ClientWidth;
end;

function TBasePainter.ColorMap: THexViewColorMap;
begin
  Result := FOwner.ColorMap;
end;

function TBasePainter.Columns: TFWHexViewColumnTypes;
begin
  Result := FOwner.Header.Columns;
end;

constructor TBasePainter.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  FRawData := FOwner.RawData;
end;

function TBasePainter.CurrentPPI: Integer;
begin
  Result := FOwner.FCurrentPPI;
end;

procedure TBasePainter.DefaultDrawHeaderColumn(ACanvas: TCanvas;
  var ARect: TRect; const ACaption: string; Flags: DWORD);
begin
  ACanvas.Font.Color := FOwner.ColorMap.HeaderTextColor;
  DrawText(ACanvas, PChar(ACaption), -1, ARect, Flags);
  ACanvas.Pen.Color := ColorMap.HeaderBorderColor;
  InflateRect(ARect, FOwner.TextMargin, 0);
  ACanvas.MoveTo(ARect.Right, ARect.Top);
  ACanvas.LineTo(ARect.Right, ARect.Bottom);
end;

procedure TBasePainter.DoQueryComment(AddrVA: Int64; AColumn: TColumnType;
  var AComment: string);
begin
  FOwner.DoQueryString(AddrVA, AColumn, AComment);
end;

function TBasePainter.DrawColumnSeparator: Boolean;
begin
  Result := FOwner.Header.DrawColumnSeparator;
end;

function TBasePainter.Encoder: TCharEncoder;
begin
  Result := FOwner.Encoder;
end;

function TBasePainter.Focused: Boolean;
begin
  Result := FOwner.Focused;
end;

function TBasePainter.GetColWidth(Value: TColumnType): Integer;
begin
  Result := FOwner.Header.ColumnWidth[Value];
end;

function TBasePainter.GetLeftNCWidth: Integer;
begin
  Result := FOwner.DefaultPainter.CalcLeftNCWidth;;
end;

procedure TBasePainter.GetRawBuff(ARowIndex: Int64; var Data: TBytes);
begin
  FOwner.GetRawBuff(ARowIndex, Data);
end;

function TBasePainter.GetRowOffset(ARowIndex: Int64): Int64;
begin
  Result := FOwner.GetRowOffset(ARowIndex);
end;

function TBasePainter.GetSelectData(ARowIndex: Int64): TSelectData;
begin
   Result := FOwner.GetSelectData(ARowIndex);
end;

function TBasePainter.GetSelectDataWithSelection(ARowIndex: Int64; ASelStart,
  ASelEnd: TSelectPoint): TSelectData;
begin
  Result := FOwner.GetSelectDataWithSelection(ARowIndex, ASelStart, ASelEnd);
end;

function TBasePainter.HeaderVisible: Boolean;
begin
  Result := FOwner.Header.Visible;
end;

function TBasePainter.HeaderWidth: Integer;
begin
  Result := Owner.Header.Width;
end;

function TBasePainter.IsAddrVisible(AAddrVA: Int64): Boolean;
begin
  Result := Owner.IsAddrVisible(AAddrVA);
end;

function TBasePainter.IsRowVisible(ARowIndex: Int64): Boolean;
begin
  Result := Owner.IsRowVisible(ARowIndex);
end;

function TBasePainter.MakeDrawRect(LeftOffset, TopOffset,
  AWidth: Integer): TRect;
begin
  Result := Bounds(
    LeftOffset + TextMargin,
    TopOffset,
    AWidth - TextMargin shl 1,
    RowHeight);
end;

function TBasePainter.MakeSelectRect(LeftOffset, TopOffset,
  SelectWidth: Integer): TRect;
begin
  Result := Bounds(
    LeftOffset,
    TopOffset,
    SelectWidth,
    RowHeight);
end;

function TBasePainter.NoDataText: string;
begin
  Result := FOwner.NoDataText;
end;

function TBasePainter.ReadOnly: Boolean;
begin
  Result := FOwner.ReadOnly;
end;

function TBasePainter.RowHeight: Integer;
begin
  Result := FOwner.RowHeight;
end;

function TBasePainter.RowToAddress(ARowIndex: Int64;
  ValueOffset: Integer): Int64;
begin
  Result := RawData.RowToAddress(ARowIndex, ValueOffset);
end;

function TBasePainter.RowVisible(ARowIndex: Int64): Boolean;
begin
  Result := FOwner.RowVisible(ARowIndex);
end;

function TBasePainter.ScrollOffset: TLargePoint;
begin
  Result := FOwner.ScrollOffset;
end;

function TBasePainter.SelectedColor(SelectStyle: TSelectStyle): TColor;
begin
  if SelectStyle = ssNone then
    Result := ColorMap.BackgroundColor
  else
    if Focused then
      Result := ColorMap.SelectColor
    else
      if FOwner.HideSelection then
        Result := ColorMap.BackgroundColor
      else
        Result := ColorMap.SelectInactiveColor;
end;

function TBasePainter.Selections: TSelections;
begin
  Result := FOwner.Selections;
end;

function TBasePainter.SeparateGroupByColor: Boolean;
begin
  Result := FOwner.SeparateGroupByColor;
end;

function TBasePainter.SplitMargin: Integer;
begin
  Result := FOwner.SplitMargin;
end;

function TBasePainter.TextMargin: Integer;
begin
  Result := FOwner.TextMargin;
end;

function TBasePainter.TextMetric: TAbstractTextMetric;
begin
  Result := FOwner.TextMetric;
end;

function TBasePainter.ToDpi(Value: Integer): Integer;
begin
  Result := FOwner.ToDpi(Value);
end;

function TBasePainter.ValueOffsetToCharIndex(AColumn: TColumnType;
  AValueOffset: Integer): Integer;
begin
  if AColumn in [ctOpcode, ctDescription] then
    Result := TextMetric.ValueOffsetToCharIndex(AColumn, AValueOffset)
  else
    Result := 0;
end;

function TBasePainter.VisibleRowDiapason: TVisibleRowDiapason;
begin
  Result := FOwner.VisibleRowDiapason;
end;

{ TAbstractPrimaryRowPainter }

function TAbstractPrimaryRowPainter.AcceptEdit(AColumn: TColumnType): Boolean;
begin
  Result := CaretEditMode(AColumn) <> cemDisabled;
end;

function TAbstractPrimaryRowPainter.AcceptSelection: Boolean;
begin
  Result := False;
end;

function TAbstractPrimaryRowPainter.CalcColumnLengthForCopy(
  AColumn: TColumnType): Integer;
const
  AddrLengthHex: array [TAddressMode] of Byte = (2, 4, 8, 16);
  AddrLengthInt: array [TAddressMode] of Byte = (3, 5, 10, 19);
begin
  case AColumn of
    ctAddress:
    begin
      Result := IfThen(AddressView = avHex,
        AddrLengthHex[AddressMode], AddrLengthInt[AddressMode]);
      if AddressViewOffsetBase >= 0 then
        Inc(Result, Result + AddrModeOffsetSeparator);
    end;
    ctOpcode:
    begin
      Result := TextMetric.CharCount(AColumn, BytesInRow);
      Inc(Result, TextMetric.FieldCount(BytesInRow) - 1);
    end;
    ctDescription: Result := TextMetric.CharCount(AColumn, BytesInRow);
  else
    Result := 0;
  end;
end;

function TAbstractPrimaryRowPainter.CalcEditParam(ACaretPosData: TCaretPosData;
  var Offset: TPoint; out EditChar: string): Boolean;
begin
  Result := False;
  EditChar := #0;
end;

function TAbstractPrimaryRowPainter.CalcLeftNCWidth: Integer;
begin
  Result := 0;
end;

function TAbstractPrimaryRowPainter.CaretEditMode(AColumn: TColumnType): TCaretEditMode;
begin
  Result := cemDisabled;
end;

function TAbstractPrimaryRowPainter.CaretKeyIncrement(
  AColumn: TColumnType): Integer;
begin
  Result := 1;
  if ReadOnly or (CaretEditMode(AColumn) <> cemSingleChar) then
    Result := TextMetric.ValueWidthInChar(AColumn);
end;

function TAbstractPrimaryRowPainter.CharCount(AColumn: TColumnType): Integer;
begin
  if AColumn in [ctOpcode, ctDescription] then
    Result := TextMetric.CharCount(AColumn, RawData[RowIndex].RawLength)
  else
    Result := 0;
end;

function TAbstractPrimaryRowPainter.ColumnAsString(
  AColumn: TColumnType): string;
var
  Data: TBytes;
  Distance: Int64;
  AddrLen: Integer;
  OffsetStr: string;
begin
  case AColumn of
    ctAddress:
    begin
      if AddressViewOffsetBase < 0 then
        AddrLen := CalcColumnLengthForCopy(ctAddress)
      else
        AddrLen := (CalcColumnLengthForCopy(ctAddress) - AddrModeOffsetSeparator) shr 1;

      if AddressView = avHex then
        Result := IntToHex(RawData[RowIndex].Address, AddrLen)
      else
      begin
        Result := IntToStr(RawData[RowIndex].Address);
        Result :=
          StringOfChar('0', AddrLen - Length(Result)) + Result;
      end;

      if AddressViewOffsetBase >= 0 then
      begin
        Distance := RawData[RowIndex].Address - AddressViewOffsetBase;
        if Distance = 0 then
          OffsetStr := '==> '
        else
        begin
          if Distance < 0 then
            OffsetStr := '-'
          else
            OffsetStr := '+';
          Distance := Abs(Distance);
          if AddressView = avHex then
            OffsetStr := OffsetStr + IntToHex(Distance, 1)
          else
            OffsetStr := OffsetStr + IntToStr(Distance);
        end;
        Result := OffsetStr + StringOfChar(' ', AddrLen + AddrModeOffsetSeparator -
          Length(OffsetStr)) + Result;
      end;

    end;
    ctOpcode:
    begin
      Owner.GetRawBuff(RowIndex, Data);
      if Length(Data) = 0 then Exit('');
      if ByteViewMode = bvmText then
        Result := Encoder.EncodeBuff(Data)
      else
        Result := RawBufToViewMode(@Data[0], Length(Data), TextMetric.ValueMetric,
          ByteViewMode, FormatMode);
    end;
    ctDescription:
    begin
      Owner.GetRawBuff(RowIndex, Data);
      if Length(Data) = 0 then Exit('');
      Result := Encoder.EncodeBuff(Data);
    end;
    ctComment:
    begin
      Result := RawData[RowIndex].Comment;
    end;
  end;
  DoQueryComment(RawData[RowIndex].Address, AColumn, Result);
end;

function TAbstractPrimaryRowPainter.ColumnRect(const Offset: TPoint;
  AColumn: TColumnType): TRect;
var
  Col: TColumnType;
begin
  Result := Bounds(Offset.X, Offset.Y, ColumnWidth[AColumn], RowHeight);
  for Col := ctWorkSpace to High(TColumnType) do
    if Col in Columns then
    begin
      if Col = AColumn then Break;
      OffsetRect(Result, ColumnWidth[Col], 0);
    end;
end;

function TAbstractPrimaryRowPainter.ColumnsDrawSupport: TFWHexViewColumnTypes;
begin
  Result := [];
end;

procedure TAbstractPrimaryRowPainter.CopyRowAsString(Builder: TSimplyStringBuilder);
var
  Col: TColumnType;
  RowStr: string;

  procedure Append(const Value: string);
  begin
    if RowStr = '' then
      RowStr := Value
    else
      RowStr := RowStr + ' | ' + Value;
  end;

begin
  RowStr := '';
  for Col := ctWorkSpace to High(TColumnType) do
    if Col in Columns then
    case Col of
      ctOpcode,
      ctDescription: Append(FormatRowColumn(Col, ColumnAsString(Col)));
    else
      Append(ColumnAsString(Col));
    end;
  Builder.Append(RowStr + sLineBreak);
end;

procedure TAbstractPrimaryRowPainter.CorrectCanvasFont(ACanvas: TCanvas;
  AColumn: TColumnType);
begin
  // do nothing...
end;

procedure TAbstractPrimaryRowPainter.DoDrawToken(ACanvas: TCanvas;
  ATokenParam: TDrawParam; const ARect: TRect; AToken: PChar;
  var ATokenLen: Integer);
begin
  Owner.DoDrawToken(ACanvas, ATokenParam, ARect, AToken, ATokenLen);
end;

procedure TAbstractPrimaryRowPainter.DrawAddress(ACanvas: TCanvas; var ARect: TRect);
begin
  ACanvas.Font.Color := ColorMap.TextColor;
  if not DrawRowColumnBackground(ACanvas, ctAddress, ARect) then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := SelectedColor(SelData.SelectStyle);
  end;
  CorrectCanvasFont(ACanvas, ctAddress);
  DrawText(ACanvas, PChar(ColumnAsString(ctAddress)), -1, ARect, DT_RIGHT);
end;

procedure TAbstractPrimaryRowPainter.DrawAlignedTextPart(ACanvas: TCanvas;
  AColumn: TColumnType; const Text: string; const ARect: TRect);
var
  AlignBuff: array of Integer;
  I: Integer;
begin
  if Text = '' then Exit;
  SetLength(AlignBuff, Length(Text));
  for I := 0 to Length(AlignBuff) - 1 do
    AlignBuff[I] := CharWidth;
  InternalDrawTextBlock(ACanvas, AColumn, ARect, Text, @AlignBuff[0], False);
end;

procedure TAbstractPrimaryRowPainter.DrawBackground(ACanvas: TCanvas;
  MousePressed: Boolean; const AHitInfo: TMouseHitInfo);
var
  LeftOffset: Int64;
  I: TColumnType;
  ARect: TRect;
begin
  LeftOffset := ScrollOffset.X;
  ARect := ACanvas.ClipRect;
  DrawColumnBackground(ACanvas, ctNone, ARect, False);
  for I := ctWorkSpace to High(TColumnType) do
    if I in Columns then
    begin
      ARect := Bounds(LeftOffset, ARect.Top, ColumnWidth[I], ARect.Height);
      DrawColumnBackground(ACanvas, I, ARect,
        MousePressed and (keSplitter in AHitInfo.Elements) and (AHitInfo.SelectPoint.Column = I));
      Inc(LeftOffset, ColumnWidth[I]);
    end;

  if RawData[RowIndex].Count = 0 then
    if NoDataText <> '' then
    begin
      ARect := Rect(0, RowHeight * 2, ClientWidth, RowHeight * 3);
      InflateRect(ARect, -ToDpi(16), ToDpi(6));
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := ColorMap.InfoBackgroundColor;
      ACanvas.Pen.Color := ColorMap.InfoBorderColor;
      ACanvas.Rectangle(ARect);
      InflateRect(ARect, 0, ToDpi(-6));
      ACanvas.Font.Color := ColorMap.InfoTextColor;
      DrawText(ACanvas, PChar(NoDataText), -1, ARect, DT_CENTER);
    end;
end;

procedure TAbstractPrimaryRowPainter.DrawColumnBackground(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect; InResizeState: Boolean);
begin
  if AColumn = ctNone then
  begin
    ACanvas.Brush.Color := ColorMap.BackgroundColor;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(ARect);
  end
  else
    if DrawColumnSeparator then
    begin
      if InResizeState then
      begin
        ACanvas.Pen.Width := DblSize(SplitMargin);
        ACanvas.Pen.Color := ColorMap.SelectInactiveColor;
        ACanvas.MoveTo(ARect.Right, 0);
        ACanvas.LineTo(ARect.Right, ARect.Bottom);
        ACanvas.Pen.Width := 1;
      end;
      ACanvas.Pen.Color := ColorMap.HeaderColumnSeparatorColor;
      ACanvas.MoveTo(ARect.Right, 0);
      ACanvas.LineTo(ARect.Right, ARect.Bottom);
    end;
end;

procedure TAbstractPrimaryRowPainter.DrawComment(ACanvas: TCanvas; var ARect: TRect);
var
  DataString: string;
begin
  DataString := ColumnAsString(ctComment);
  if DataString = '' then Exit;

  if not DrawRowColumnBackground(ACanvas, ctComment, ARect) then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := SelectedColor(SelData.SelectStyle);
  end;

  ACanvas.Font.Color := ColorMap.TextCommentColor;
  DrawAlignedTextPart(ACanvas, ctComment, DataString, ARect);
end;

procedure TAbstractPrimaryRowPainter.DrawHeaderColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
var
  DataString: string;
begin
  DataString := GetHeaderColumnCaption(AColumn);
  if DataString <> '' then
  begin
    case AColumn of
      ctOpcode, ctDescription:
        ExtTextOut(ACanvas, ARect.Left, ARect.Top, ETO_CLIPPED, @ARect,
          @DataString[1], UTF8StringLength(DataString), TextMetric.CharPointer(AColumn, 0));
    else
      DrawText(ACanvas, PChar(DataString), -1, ARect, DT_CENTER);
    end;
  end;
  ACanvas.Pen.Color := ColorMap.HeaderBorderColor;
  InflateRect(ARect, TextMargin, 0);
  ACanvas.MoveTo(ARect.Right, ARect.Top);
  ACanvas.LineTo(ARect.Right, ARect.Bottom);
end;

procedure TAbstractPrimaryRowPainter.DrawRow(ACanvas: TCanvas;
  var Offset: TPoint);
var
  LeftOffset: Integer;
  I: TColumnType;
  ARect: TRect;
begin
  FSelData := GetSelectData(FRowIndex);
  LeftOffset := Offset.X;
  try
    ARect := Bounds(Offset.X, Offset.Y, HeaderWidth, RowHeight);
    DrawColumn(ACanvas, ctNone, ARect);
    for I := ctWorkSpace to High(TColumnType) do
      if I in Columns then
      begin
        ARect := MakeDrawRect(Offset.X, Offset.Y, ColumnWidth[I]);
        {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('TAbstractPrimaryRowPainter.DrawRow');{$ENDIF}
        DrawColumn(ACanvas, I, ARect);
        {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
        Inc(Offset.X, ColumnWidth[I]);
      end;
  finally
    Offset.X := LeftOffset;
  end;
  Inc(Offset.Y, RowHeight);
end;

function TAbstractPrimaryRowPainter.DrawRowColumnBackground(ACanvas: TCanvas;
  AColumn: TColumnType; const ARect: TRect): Boolean;
begin
  Result := Owner.DoDrawRowColumnBackground(ACanvas, Self, AColumn, ARect);
end;

procedure TAbstractPrimaryRowPainter.DrawTextBlock(ACanvas: TCanvas;
  AColumn: TColumnType; const ARect: TRect; const DrawString: string;
  Dx: PInteger);
begin
  InternalDrawTextBlock(ACanvas, AColumn, ARect, DrawString, Dx, True);
end;

procedure TAbstractPrimaryRowPainter.DrawWorkSpace(ACanvas: TCanvas;
  var ARect: TRect);
begin
  ACanvas.Font.Color := ColorMap.TextColor;
  DrawRowColumnBackground(ACanvas, ctWorkSpace, ARect);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := ColorMap.WorkSpaceTextColor;
  CorrectCanvasFont(ACanvas, ctWorkSpace);
  Dec(ARect.Left, TextMargin - ToDpi(2));
  DrawText(ACanvas, PChar(ColumnAsString(ctWorkSpace)), -1, ARect, 0);
end;

function TAbstractPrimaryRowPainter.FormatMode: TFormatMode;
begin
  Result := DefFormatMode;
end;

function TAbstractPrimaryRowPainter.FormatRowColumn(AColumn: TColumnType;
  const Value: string): string;
const
  UncopiedData = {$IFDEF FPC}'.'{$ELSE}Char(#8230){$ENDIF};
var
  Unformated: string;
  I, A, LastCharWidth, CharWidth, MetricLength: Integer;
  CharStart, CharEnd: Integer;
begin
  A := 0;
  for I := 0 to Owner.Painters.Count - 1 do
    A := Max(A, Owner.Painters[I].CalcColumnLengthForCopy(AColumn));
  Result := StringOfChar(' ', A);
  FSelData := GetSelectData(RowIndex);
  CharStart := TextMetric.ValueOffsetToCharIndex(AColumn, SelData.FirstSelectIndex) + 1;
  CharEnd := TextMetric.ValueOffsetToCharIndex(AColumn, SelData.SecondSelectIndex) +
    TextMetric.CharCount(AColumn, 1);
  case SelData.SelectStyle of
    ssAllSelected:
      Unformated := Value;
    ssLeftSelected:
    begin
      Inc(CharStart, TextMetric.CharCount(AColumn, 1) - 1);
      Unformated := Copy(Value, 1, CharStart) +
        StringOfChar(UncopiedData, Length(Value) - CharStart);
    end;
    ssCenterSelected:
      Unformated :=
        StringOfChar(UncopiedData, CharStart - 1) +
        Copy(Value, CharStart, CharEnd - CharStart + 1) +
        StringOfChar(UncopiedData, Length(Value) - CharEnd);
    ssRightSelected:
      Unformated := StringOfChar(UncopiedData, CharStart - 1) +
        Copy(Value, CharStart, Length(Value));
  end;
  I := 1;
  A := 1;
  LastCharWidth := TextMetric.CharWidth(AColumn, 0);
  MetricLength := TextMetric.InitCharCount(AColumn);
  while I <= Length(Unformated) do
  begin
    Result[A] := Unformated[I];
    CharWidth := TextMetric.CharWidth(AColumn, (I - 1) mod MetricLength);
    if LastCharWidth < CharWidth then
      Inc(A);
    LastCharWidth := CharWidth;
    Inc(I);
    Inc(A);
  end;
end;

function TAbstractPrimaryRowPainter.GetHeaderColumnCaption(
  AColumn: TColumnType): string;
begin
  Result := '';
end;

procedure TAbstractPrimaryRowPainter.GetHitInfo(
  var AMouseHitInfo: TMouseHitInfo);
begin
end;

procedure TAbstractPrimaryRowPainter.InternalDrawTextBlock(ACanvas: TCanvas;
  AColumn: TColumnType; const ARect: TRect; const DrawString: string;
  Dx: PInteger; CheckDxSize: Boolean);
var
  pData: PChar;
  nSize, nLength, nTokenLength, nTokenSize, XOffset: Integer;
  SavedFont: TFont;
  R: TRect;
  ATokenParam: TDrawParam;
  I, KerningLen: Integer;
  EmergencyDxBuff: array of Integer;
begin
  if DrawString = '' then Exit;
  pData := PChar(@DrawString[1]);
  nSize := Length(DrawString);
  nLength := UTF8StringLength(DrawString);

  // Специальная проверка на случай если программист не умеет пользоваться
  // контролом и не позаботился о том, чтобы размер массива кернинга символов
  // передаваемый в Dx, был не меньше чем размер текста.
  // Чтобы этого не случилось, нужно правильно инициализировать TextMetric
  // в наследнике, или вызывать DrawAlignedTextPart

  // Special check in case the programmer doesn't know how to use the control
  // and didn't make sure that the size of the kerning array
  // passed to Dx is not smaller than the text size.
  // To prevent this from happening, you need to initialize TextMetric
  // correctly in the inheritor, or call DrawAlignedTextPart

  if CheckDxSize then
  begin
    KerningLen := TextMetric.CharPointerLength(AColumn, 0);
    if nLength > KerningLen then
    begin
      SetLength(EmergencyDxBuff, nLength);
      Move(Dx^, EmergencyDxBuff[0], KerningLen shl 2);
      for I := KerningLen to nLength - 1 do
        EmergencyDxBuff[I] := CharWidth;
      Dx := @EmergencyDxBuff[0];
    end;
  end;

  SavedFont := TFont.Create;
  try
    SavedFont.Assign(ACanvas.Font);
    XOffset := 0;
    R := ARect;
    ATokenParam.AddrVA := RowToAddress(RowIndex, 0);
    ATokenParam.Column := AColumn;
    ATokenParam.RowIndex := RowIndex;
    ATokenParam.ValueOffset := 0;
    ATokenParam.CharIndex := 0;
    while nSize > 0 do
    begin
      nTokenLength := nLength;
      R.Left := ARect.Left + XOffset;
      DoDrawToken(ACanvas, ATokenParam, R, pData, nTokenLength);
      if nTokenLength > nLength then
        nTokenLength := nLength;
      CorrectCanvasFont(ACanvas, AColumn);
      nTokenSize := UTF8ByteCount(pData, nTokenLength);
      ExtTextOut(ACanvas, R.Left, R.Top, ETO_CLIPPED, @R, pData,
        {$IFDEF LINUX}nTokenSize{$ELSE}nTokenLength{$ENDIF}, Dx);
      Inc(pData, nTokenSize);
      Dec(nSize, nTokenSize);
      Dec(nLength, nTokenLength);
      if nSize > 0 then
      begin
        Inc(ATokenParam.CharIndex, nTokenLength);
        ATokenParam.ValueOffset := TextMetric.CharIndexToValueOffset(AColumn, ATokenParam.CharIndex);
        ATokenParam.AddrVA := RowToAddress(RowIndex, ATokenParam.ValueOffset);
        while nTokenLength > 0 do
        begin
          Inc(XOffset, Dx^);
          Inc(Dx);
          Dec(nTokenLength);
        end;
      end;
      ACanvas.Font.Assign(SavedFont);
    end;
  finally
    SavedFont.Free;
  end;
end;

procedure TAbstractPrimaryRowPainter.SetRowIndex(const Value: Int64);
begin
  if Value < 0 then
    raise Exception.CreateFmt('Invalid Row Index %d', [Value]);
  FRowIndex := Value;
end;

{ TDefaultTextMetric }

function TDefaultTextMetric.ByteViewMode: TByteViewMode;
begin
  Result := Owner.ByteViewMode;
end;

constructor TDefaultTextMetric.Create(AOwner: TFWCustomHexView);
begin
  inherited;
  FColorGroup := TList<TRect>.Create;
end;

destructor TDefaultTextMetric.Destroy;
begin
  FColorGroup.Free;
  inherited;
end;

procedure TDefaultTextMetric.Update;
begin
  inherited;
  UpdateColorGroup(Owner.BytesInRow, Owner.CharWidth);
end;

procedure TDefaultTextMetric.UpdateCharPosition(BytesInRow, CharWidth: Integer);
begin
  if ByteViewMode = bvmText then
    UpdateCharPositionText(BytesInRow, CharWidth)
  else
    UpdateCharPositionDef(BytesInRow, CharWidth);
end;

procedure TDefaultTextMetric.UpdateColorGroup(BytesInRow, CharWidth: Integer);
var
  I, LeftOffset, RightOffset, CharIndex, CurrentByteInGroup: Integer;
  GrayColor: Boolean;
  R: TRect;
begin
  // рассчет оффсетов для бэкграунда цветовых групп

  // Offset calculation for color group backgrounds

  ColorGroup.Clear;
  if Owner.SeparateGroupByColor then
  begin
    LeftOffset := 0;
    RightOffset := LeftOffset;
    CharIndex := 0;
    CurrentByteInGroup := 0;
    GrayColor := False;
    while CharIndex < Owner.BytesInRow - 1 do
    begin
      Inc(CurrentByteInGroup, ValueMetric.ByteCount);
      for I := 0 to ValueMetric.ByteCount - 1 do
      begin
        Inc(RightOffset, FSelectionPositions[True][CharIndex]);
        Inc(CharIndex);
      end;
      if CurrentByteInGroup >= Owner.BytesInColorGroup then
      begin
        Dec(CurrentByteInGroup, Owner.BytesInColorGroup);
        GrayColor := not GrayColor;
        if GrayColor then
        begin
          R := Rect(LeftOffset, 0, RightOffset, Owner.RowHeight);
          ColorGroup.Add(R);
        end;
        LeftOffset := RightOffset;
      end;
    end;
  end;
end;

function TDefaultTextMetric.ValueMetric: TValueMetric;
begin
  if ByteViewMode = bvmAddress then
  begin
    case Owner.AddressMode of
      am8bit:
      begin
        Result.CharCount := 2;
        Result.ByteCount := 1;
      end;
      am16bit:
      begin
        Result.CharCount := 4;
        Result.ByteCount := 2;
      end;
      am32bit:
      begin
        Result.CharCount := 8;
        Result.ByteCount := 4;
      end;
      am64bit:
      begin
        Result.CharCount := 16;
        Result.ByteCount := 8;
      end;
    end;
  end
  else
    Result := DefValueMetric(ByteViewMode);
end;

{ TRowHexPainter }

function TRowHexPainter.AcceptSelection: Boolean;
begin
  Result := True;
end;

procedure TRowHexPainter.AddSelDataToColorMix(ASelData: TSelectData;
  AColor: TColor; Primary: Boolean);
begin
  case ASelData.SelectStyle of
    ssAllSelected:
      AddToColorMix(0, RawData[RowIndex].RawLength - 1, AColor, Primary);
    ssLeftSelected:
      AddToColorMix(0, ASelData.FirstSelectIndex, AColor, Primary);
    ssCenterSelected:
      AddToColorMix(ASelData.FirstSelectIndex, ASelData.SecondSelectIndex, AColor, Primary);
    ssRightSelected:
      AddToColorMix(ASelData.FirstSelectIndex, RawData[RowIndex].RawLength - 1, AColor, Primary);
  end;
end;

procedure TRowHexPainter.AddToColorMix(SelStart, SelEnd: Integer;
  AColor: TColor; Primary: Boolean);
var
  I: Integer;
  C1, C2: Longint;
  R, G, B: Byte;
begin
  C1 := ColorToRGB(AColor);
  for I := SelStart to SelEnd do
  begin
    if FColorMix[I] = 0 then
      FColorMix[I] := C1
    else
    begin
      C2 := FColorMix[I];
      if Primary then
      begin
        R := (Byte(C1) + MulDiv(Byte(C2), 150, 255)) shr 1;
        G := (Byte(C1 shr 8) + MulDiv(Byte(C2 shr 8), 150, 255)) shr 1;
        B := (Byte(C1 shr 16) + MulDiv(Byte(C2 shr 16), 150, 255)) shr 1;
      end
      else
      begin
        R := (Byte(C1) + Byte(C2)) shr 1;
        G := (Byte(C1 shr 8) + Byte(C2 shr 8)) shr 1;
        B := (Byte(C1 shr 16) + Byte(C2 shr 16)) shr 1;
      end;
      FColorMix[I] := R + G shl 8 + B shl 16;
    end;
  end;
end;

function TRowHexPainter.CalcEditParam(ACaretPosData: TCaretPosData;
  var Offset: TPoint; out EditChar: string): Boolean;

  function GetEditChar: string;
  var
    DataString: string;
  begin
    Result := #0;
    if ACaretPosData.Column = ctOpcode then
      DataString := {%H-}ColumnAsString(ctOpcode)
    else
      DataString := {%H-}ColumnAsString(ctDescription);

    if DataString <> '' then
      if ACaretPosData.CharIndex < Length(DataString) then
         Result := UTF8Copy(DataString, ACaretPosData.CharIndex + 1, 1);
  end;

var
  I: TColumnType;
  CaretOffset: Integer;
begin
  Result := (CaretEditMode(ACaretPosData.Column) = cemSingleChar) and
    (ACaretPosData.CharIndex >= 0);
  if not Result then Exit;

  for I := ctWorkSpace to High(TColumnType) do
    if I in Columns then
      case I of
        ctWorkSpace, ctJmpLine, ctAddress, ctComment:
          Inc(Offset.X, ColumnWidth[I]);
        ctOpcode, ctDescription:
        begin
          if ACaretPosData.Column <> I then
            Inc(Offset.X, ColumnWidth[I])
          else
          begin
            if ACaretPosData.CharIndex > TextMetric.CharCount(I,
              RawData[ACaretPosData.RowIndex].RawLength) then
            begin
              Result := False;
              Exit;
            end;

            EditChar := GetEditChar;
            if EditChar = #0 then
            begin
              Result := False;
              Exit;
            end;

            Inc(Offset.X, TextMargin);
            Inc(Offset.Y, RowHeight);

            CaretOffset := TextMetric.CharLength(I,
              0, ACaretPosData.CharIndex - 1);

            if CaretOffset >= ColumnWidth[I] - TextMargin shl 1 then
              Result := False
            else
              Inc(Offset.X, CaretOffset);

            Break;
          end;
        end;
      end;
end;

function TRowHexPainter.CalcLeftNCWidth: Integer;
begin
  Result := 0;
  if ctWorkSpace in Columns then
    Inc(Result, ColumnWidth[ctWorkSpace]);
  if ctJmpLine in Columns then
    Inc(Result, ColumnWidth[ctJmpLine]);
end;

function TRowHexPainter.CaretEditMode(AColumn: TColumnType): TCaretEditMode;
begin
  case AColumn of
    ctOpcode:
    begin
      if ByteViewMode in [bvmHex8..bvmHex64, bvmText] then
        Result := cemSingleChar
      else
        Result := cemValueEditor;
    end;
    ctDescription:
      Result := cemSingleChar;
  else
    Result := cemDisabled;
  end;
end;

function TRowHexPainter.ColumnsDrawSupport: TFWHexViewColumnTypes;
begin
  Result := [ctOpcode];
end;

procedure TRowHexPainter.CorrectCanvasFont(ACanvas: TCanvas;
  AColumn: TColumnType);
begin
  case FColorMixFontMode of
    cmfmNone: ; // stay the same!!!
    cmfmLight: ACanvas.Font.Color := ColorMap.SelectTextContrastLightColor;
    cmfmDark: ACanvas.Font.Color := ColorMap.SelectTextContrastDarkColor;
  end;
end;

procedure TRowHexPainter.DoCanvasFontChange(Sender: TObject);
begin
  FCanvasChanged := True;
  FCanvasFontChange(Sender);
end;

procedure TRowHexPainter.DoDrawToken(ACanvas: TCanvas; ATokenParam: TDrawParam;
  const ARect: TRect; AToken: PChar; var ATokenLen: Integer);
var
  InitialTokenLen, Index, Len: Integer;
  CurrentColor: LongInt;
  IsDarkBackground: Boolean;
begin
  FColorMixFontMode := cmfmNone;
  if FColorMixMode = cmmNone then
  begin
    inherited;
    Exit;
  end;
  FCanvasFontChange := ACanvas.OnChange;
  ACanvas.OnChange := DoCanvasFontChange;
  try
    FCanvasChanged := False;
    InitialTokenLen := ATokenLen;
    inherited;
    if (InitialTokenLen = ATokenLen) and not FCanvasChanged then
    begin
      if Owner.DefaultFontColorIsDark and (FColorMixMode = cmmLight) then Exit;
      if (FColorMixMode = cmmDark) and not Owner.DefaultFontColorIsDark then Exit;
      CurrentColor := FColorMix[ATokenParam.ValueOffset];
      Index := ATokenParam.ValueOffset;
      Len := 0;
      while CurrentColor = FColorMix[Index] do
      begin
        Inc(Len);
        Inc(Index);
        if Len >= ATokenLen then
          Break;
      end;
      ATokenLen := TextMetric.CharCount(ATokenParam.Column, Len);
      if CurrentColor = 0 then Exit;
      IsDarkBackground := IsColorRefDark(CurrentColor);
      if IsDarkBackground <> Owner.DefaultFontColorIsDark then Exit;
      if IsDarkBackground then
        FColorMixFontMode := cmfmLight
      else
        FColorMixFontMode := cmfmDark;
    end;
  finally
    ACanvas.OnChange := FCanvasFontChange;
  end;
end;

procedure TRowHexPainter.DrawColorGroup(ACanvas: TCanvas; var ARect: TRect);
var
  I: Integer;
  R: TRect;
begin
  ACanvas.Brush.Color := ColorMap.GroupColor;
  ACanvas.Brush.Style := bsSolid;
  for I := 0 to TDefaultTextMetric(TextMetric).ColorGroup.Count - 1 do
  begin
    R := TDefaultTextMetric(TextMetric).ColorGroup[I];
    OffsetRect(R, ARect.Left, ARect.Top);
    R.Right := Min(R.Right, ARect.Right);
    if R.Width > 0 then
      ACanvas.FillRect(R);
  end;
end;

procedure TRowHexPainter.DrawColorMix(ACanvas: TCanvas; var ARect: TRect);
var
  R: TRect;
  I, SelStart: Integer;
  AColor: Longint;
  LightColorPresent, DarkColorPresent: Byte;

  function DrawBack: Boolean;
  var
    DrawRect: TRect;
  begin
    Result := True;
    if R.Right > ARect.Width then
      R.Width := ARect.Width - R.Left;
    if R.Width < 0 then Exit(False);
    if AColor <> 0 then
    begin
      ACanvas.Brush.Color := AColor;
      DrawRect := Bounds(ARect.Left + R.Left, ARect.Top, R.Width, RowHeight);
      ACanvas.FillRect(DrawRect);
    end;
    R.Left := R.Right;
  end;

  procedure SetMixColor(ANewColor: Longint);
  begin
    AColor := ANewColor;
    if ANewColor = 0 then Exit;    
    if IsColorRefDark(ANewColor) then
      DarkColorPresent := 2
    else
      LightColorPresent := 1;
  end;

begin
  LightColorPresent := 0;
  DarkColorPresent := 0;
  SelStart := 0;
  SetMixColor(FColorMix[0]);
  R.Left := 0;
  ACanvas.Brush.Style := bsSolid;
  for I := 1 to Length(FColorMix) - 1 do
  begin
    if FColorMix[I] <> AColor then
    begin
      R.Right := R.Left + TextMetric.SelectionLength(FColorMixColumn, SelStart, I - 1);
      SelStart := I;
      if not DrawBack then Exit;
      SetMixColor(FColorMix[I]);
    end;
  end;
  R.Right := R.Left + TextMetric.SelectionLength(FColorMixColumn, SelStart, Length(FColorMix) - 1);
  DrawBack;
  FColorMixMode := TColorMixMode(LightColorPresent + DarkColorPresent);
end;

procedure TRowHexPainter.DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
  var ARect: TRect);
begin
  FColorMixMode := cmmNone;
  FColorMixFontMode := cmfmNone;
  case AColumn of
    ctWorkSpace:
    begin
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('ctWorkSpace');{$ENDIF}
      DrawWorkSpace(ACanvas, ARect);
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
    end;
    ctAddress:
    begin
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('ctAddress');{$ENDIF}
      DrawAddress(ACanvas, ARect);
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
    end;
    ctOpcode:
    begin
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('ctOpcode');{$ENDIF}
      DrawHexPart(ACanvas, ARect);
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
    end;
    ctDescription:
    begin
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('ctDescription');{$ENDIF}
      DrawDataPart(ACanvas, ARect);
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
    end;
    ctComment:
    begin
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('ctComment');{$ENDIF}
      DrawComment(ACanvas, ARect);
      {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
    end
  else
    {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('DrawRowColumnBackground');{$ENDIF}
    DrawRowColumnBackground(ACanvas, AColumn, ARect);
    {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
  end;
end;

procedure TRowHexPainter.DrawDataPart(ACanvas: TCanvas; var ARect: TRect);
var
  SelectionCount, I: Integer;
  Selection: TSelection;
  ASelStart, ASelEnd: TSelectPoint;
begin
  if not DrawRowColumnBackground(ACanvas, ctDescription, ARect) then
  begin
    SelectionCount := Selections.VisibleIndex.Count;
    if SelectionCount > 0 then
    begin
      InitColorMix(ctDescription);
      for I := 0 to SelectionCount - 1 do
      begin
        Selection := Selections[Selections.VisibleIndex[I]];
        ASelStart := AddressToSelectPoint(Selection.SelStart);
        ASelEnd := AddressToSelectPoint(Selection.SelEnd);
        AddSelDataToColorMix(GetSelectDataWithSelection(
          RowIndex, ASelStart, ASelEnd), Selection.Color, False);
      end;
      AddSelDataToColorMix(SelData, SelectedColor(ssAllSelected), True);
      DrawColorMix(ACanvas, ARect);
    end
    else
      DrawSelectedBackround(ACanvas, ctDescription, ARect, SelData);
  end;
  ACanvas.Brush.Style := bsClear;
  DrawTextBlock(ACanvas, ctDescription, ARect, ColumnAsString(ctDescription),
    TextMetric.CharPointer(ctDescription, 0));
end;

procedure TRowHexPainter.DrawHeaderColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
begin
  case ByteViewMode of
    bvmText, bvmAddress:
      DefaultDrawHeaderColumn(ACanvas, ARect,
        GetHeaderColumnCaption(AColumn), 0);
  else
    inherited;
  end;
end;

procedure TRowHexPainter.DrawHexPart(ACanvas: TCanvas; var ARect: TRect);
var
  AColor: TColor;
  I, SelectionCount: Integer;
  Selection: TSelection;
  ASelStart, ASelEnd: TSelectPoint;
begin
  if not DrawRowColumnBackground(ACanvas, ctOpcode, ARect) then
  begin

    if SeparateGroupByColor then
      DrawColorGroup(ACanvas, ARect);

    SelectionCount := Selections.VisibleIndex.Count;
    if SelectionCount > 0 then
    begin
      InitColorMix(ctOpcode);
      for I := 0 to SelectionCount - 1 do
      begin
        Selection := Selections[Selections.VisibleIndex[I]];
        ASelStart := AddressToSelectPoint(Selection.SelStart);
        ASelEnd := AddressToSelectPoint(Selection.SelEnd);
        AddSelDataToColorMix(GetSelectDataWithSelection(
          RowIndex, ASelStart, ASelEnd), Selection.Color, False);
      end;
      AddSelDataToColorMix(SelData, SelectedColor(ssAllSelected), True);
      DrawColorMix(ACanvas, ARect);
    end
    else
      DrawSelectedBackround(ACanvas, ctOpcode, ARect, SelData);
  end;

  AColor := RawData[RowIndex].Color;
  if AColor = clDefault then
    AColor := ColorMap.TextColor;
  ACanvas.Font.Color := AColor;
  ACanvas.Brush.Style := bsClear;
  DrawTextBlock(ACanvas, ctOpcode, ARect, ColumnAsString(ctOpcode),
    TextMetric.CharPointer(ctOpcode, 0));
end;

procedure TRowHexPainter.DrawSelectedBackround(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect; ASelData: TSelectData);
var
  SelStart, SelEnd: Integer;
  R: TRect;
begin
  case ASelData.SelectStyle of
    ssAllSelected:
    begin
      SelStart := 0;
      SelEnd := RawData[RowIndex].RawLength - 1;
    end;
    ssLeftSelected:
    begin
      SelStart := 0;
      SelEnd := ASelData.FirstSelectIndex;
    end;
    ssCenterSelected:
    begin
      SelStart := ASelData.FirstSelectIndex;
      SelEnd := ASelData.SecondSelectIndex;
    end;
    ssRightSelected:
    begin
      SelStart := ASelData.FirstSelectIndex;
      SelEnd := RawData[RowIndex].RawLength - 1;
    end;
  else
    Exit;
  end;
  R.Left := TextMetric.SelectionLength(AColumn, 0, SelStart - 1);
  R.Right := R.Left + TextMetric.SelectionLength(AColumn, SelStart, SelEnd);
  if R.Right > ARect.Width then
    R.Width := ARect.Width - R.Left;
  if R.Width <= 0 then Exit;
  R := Bounds(ARect.Left + R.Left, ARect.Top, R.Width, RowHeight);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := SelectedColor(ssAllSelected);
  ACanvas.FillRect(R);
end;

function TRowHexPainter.GetHeaderColumnCaption(
  AColumn: TColumnType): string;

  function AddZeroToString(const Value: string): string;
  var
    L: Integer;
  begin
    L := TextMetric.ValueMetric.CharCount - Length(Value);
    Result := StringOfChar('0', L) + Value;
  end;

var
  I: Integer;
begin
  case ByteViewMode of
    bvmText:
      Result := Encoder.DisplayName;
    bvmAddress:
      Result := 'Value';
  else
    Result := '';
    I := 0;
    while I < BytesInRow do
    begin
      if AddressView <> avDecimal then
        Result := Result + AddZeroToString(IntToHex(I, 2))
      else
        Result := Result + AddZeroToString(IntToStr(I));
      Inc(I, TextMetric.ValueMetric.ByteCount);
    end;
  end;
end;

procedure TRowHexPainter.GetHitInfo(var AHitInfo: TMouseHitInfo);
var
  LeftOffset, I, RawLength: Integer;
begin
  // выделять для модификации можно только данные в колонках с опкодами и описанием

  // only data in columns with opcodes and description can be selected for modification

  if not (AHitInfo.SelectPoint.Column in [ctOpcode, ctDescription]) then Exit;

  AHitInfo.SelectPoint.ValueOffset := -1;
  AHitInfo.SelectPoint.CharIndex := -1;

  LeftOffset := AHitInfo.ColumnStart;
  Inc(LeftOffset, TextMargin);

  // если курсор находится левее начала текста в колонке,
  // выделяем первый символ и выходим

  // if the cursor is to the left of the beginning of the text in the column,
  // select the first character and exit.

  if LeftOffset > AHitInfo.ScrolledCursorPos.X then
    Exit;

  RawLength := RawData[AHitInfo.SelectPoint.RowIndex].RawLength;

  if AHitInfo.SelectPoint.Column = ctOpcode then
  begin

    // в колонке с опкодами каждое значение представлено ввиде нескольких
    // символов (точное количество определяется в TextMetric)
    // поэтому нужно анализировать координаты каждого символа

    // in the column with opcodes each value is represented as several
    // symbols (the exact number is defined in TextMetric),
    // so you need to analyze the coordinates of each symbol.

    for I := 0 to TextMetric.CharCount(ctOpcode, RawLength) - 1 do
    begin
      Inc(LeftOffset, TextMetric.CharWidth(ctOpcode, I));
      if LeftOffset > AHitInfo.ScrolledCursorPos.X then
      begin
        AHitInfo.SelectPoint.ValueOffset := CharIndexToValueOffset(ctOpcode, I);
        if AHitInfo.SelectPoint.ValueOffset < RawLength then
          AHitInfo.SelectPoint.CharIndex := I
        else
          AHitInfo.SelectPoint.ValueOffset := -1;
        Exit;
      end;
    end;
    Exit;
  end;

  // а в колонке с описанием символы идут как есть, поэтому банально ищем нужный

  // and in the column with the description the characters are as they are,
  // so we just look for the necessary one.

  for I := 0 to RawLength - 1 do
  begin
    Inc(LeftOffset, TextMetric.CharWidth(ctDescription, I));
    if LeftOffset > AHitInfo.ScrolledCursorPos.X then
    begin
      AHitInfo.SelectPoint.ValueOffset := CharIndexToValueOffset(ctDescription, I);
      AHitInfo.SelectPoint.CharIndex := I;
      Exit;
    end;
  end;
end;

function TRowHexPainter.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TDefaultTextMetric;
end;

procedure TRowHexPainter.InitColorMix(AColumn: TColumnType);
begin
  FColorMixColumn := AColumn;
  SetLength(FColorMix, Length(TextMetric.FSelectionPositions[AColumn = ctOpcode]));
  FillChar(FColorMix[0], Length(FColorMix) * SizeOf(TColor), 0);
end;

{ TSecondaryRowPainter }

function TSecondaryRowPainter.GetTextMetricClass: TAbstractTextMetricClass;
begin
  {$ifdef fpc} Result := nil; {$endif}
  raise Exception.CreateFmt('%s can''t be the default painter.', [ClassName]);
end;

{ TLinesPostPainter }

procedure TLinesPostPainter.DrawArrowWithOffset(ACanvas: TCanvas;
  Direction: TScrollDirection; Location: TPoint; ArrowSize: Integer;
  StartPoint: Boolean);
begin
  case Direction of
    sdLeft, sdRight:
    begin
      Dec(Location.Y, ArrowSize);
      Inc(Location.X, ArrowSize);
    end;
    sdUp, sdDown:
    begin
      Dec(Location.X, ArrowSize);
      if StartPoint then
        Dec(Location.Y, DblSize(ArrowSize))
      else
        Inc(Location.Y, ArrowSize);
    end;
  end;
  DrawArrow(ACanvas, Direction, Location, ArrowSize);
end;

function TLinesPostPainter.DrawLine(ACanvas: TCanvas;
  const Param: TDrawLineParam): Boolean;

type
{
  // Варианты отрисовки линий
  TLineDrawingType = (
    ldtNone,
    ldtVisibleLines,       // обе строки видимы
    ldtDownFromInvisible,  // начальная строка сверху невидима
    ldtUpToInvisible,      // конечная строка сверху невидима
    ldtDownToInvisible,    // конечная строка снизу невидима
    ldtUpFromInvisible,    // начальная строка снизу невидима
    ldtInvisibleLines      // обе строки не видимы, но пайнтер требует отрисовки
    );
}

  // Line drawing options
  TLineDrawingType = (
    ldtNone,
    ldtVisibleLines,       // both lines are visible
    ldtDownFromInvisible,  // the start line at the top is invisible
    ldtUpToInvisible,      // the end line at the top is invisible
    ldtDownToInvisible,    // the end line at the bottom is invisible
    ldtUpFromInvisible,    // the start line at the bottom is invisible
    ldtInvisibleLines      // both lines are invisible, but painter requires rendering
    );

  function CalcVisibleRowOffset(ARowIndex: Integer;
    PathBegin, FirstRow: Boolean): TPoint;
  begin
    if PathBegin then
      Result.X := GetLeftNCWidth - TextMargin
    else
      Result.X := GetLeftNCWidth -
        TextMargin - PaintedLinesCount * Param.LineIndent;

    Result.Y := GetRowOffset(ARowIndex) + RowHeight shr 1;

    if IsNeedOffsetRow(ARowIndex, FirstRow) then
      if FirstRow then
        Inc(Result.Y, SplitMargin)
      else
       Dec(Result.Y, SplitMargin);

    Inc(Result.X, Param.Offset.X);
  end;

  function CalcInvisibleRowOffset(UpLine: Boolean): TPoint;
  begin
    Result.X := GetLeftNCWidth -
      TextMargin - PaintedLinesCount * Param.LineIndent;

    if UpLine then
    begin
      Result.Y := Param.LineVerticalMargin;
      if HeaderVisible then
        Inc(Result.Y, RowHeight);
    end
    else
      Result.Y := ClientHeight - Param.LineVerticalMargin;
    Inc(Result.X, Param.Offset.X);
  end;

var
  Selected: Boolean;
  StartRowVisible, EndRowVisible: Boolean;
  DrawLineType: TLineDrawingType;
  StartPoint, EndPoint: TPoint;
  ArrowSize: Integer;
  PenColor: TColor;
  Diapason: TVisibleRowDiapason;
begin
  Result := True;

  Selected :=
    (GetSelectData(Param.RowFrom).SelectStyle <> ssNone) or
    (GetSelectData(Param.RowTo).SelectStyle <> ssNone);

  if not LineColorPresent(Selected, Param, PenColor) then Exit;

  StartRowVisible := RowVisible(Param.RowFrom);
  EndRowVisible := RowVisible(Param.RowTo);
  if Param.SecondDraw and StartRowVisible then Exit(False);

  DrawLineType := ldtNone;
  if StartRowVisible and EndRowVisible then
  begin
    StartPoint := CalcVisibleRowOffset(Param.RowFrom, True, True);
    EndPoint := CalcVisibleRowOffset(Param.RowTo, False, False);
    DrawLineType := ldtVisibleLines;
  end;

  if StartRowVisible and not EndRowVisible then
  begin
    if Param.RowFrom < Param.RowTo then
    begin
      DrawLineType := ldtDownToInvisible;
      StartPoint := CalcVisibleRowOffset(Param.RowFrom, True, True);
      EndPoint := CalcInvisibleRowOffset(False);
    end
    else
    begin
      DrawLineType := ldtUpToInvisible;
      StartPoint := CalcVisibleRowOffset(Param.RowFrom, True, True);
      EndPoint := CalcInvisibleRowOffset(True);
    end;
  end;

  if not StartRowVisible and EndRowVisible then
  begin
    if Param.RowFrom < Param.RowTo then
    begin
      DrawLineType := ldtDownFromInvisible;
      StartPoint := CalcInvisibleRowOffset(True);
      EndPoint := CalcVisibleRowOffset(Param.RowTo, True, False);
    end
    else
    begin
      DrawLineType := ldtUpFromInvisible;
      StartPoint := CalcInvisibleRowOffset(False);
      EndPoint := CalcVisibleRowOffset(Param.RowTo, True, False);
    end;
  end;

  if not StartRowVisible and not EndRowVisible and Param.DrawAlwais then
  begin
    Diapason := VisibleRowDiapason;
    if (Param.RowFrom < Diapason.StartRow) and (Param.RowTo > Diapason.EndRow) then
    begin
      DrawLineType := ldtInvisibleLines;
      StartPoint := CalcInvisibleRowOffset(True);
      EndPoint := CalcInvisibleRowOffset(False);
    end;
  end;

  if DrawLineType = ldtNone then Exit;

  ACanvas.Brush.Color := PenColor;
  ACanvas.Pen.Color := PenColor;

  case DrawLineType of
    ldtVisibleLines:
    begin
      PatBlt(ACanvas, EndPoint.X, StartPoint.Y,
        StartPoint.X - EndPoint.X + Param.LineWidth, Param.LineWidth, PATCOPY);
      PatBlt(ACanvas, EndPoint.X, StartPoint.Y,
        Param.LineWidth, EndPoint.Y - StartPoint.Y, PATCOPY);
      PatBlt(ACanvas, EndPoint.X, EndPoint.Y,
        StartPoint.X - EndPoint.X + Param.LineWidth, Param.LineWidth, PATCOPY);
    end;
    ldtDownFromInvisible, ldtUpFromInvisible:
    begin
      PatBlt(ACanvas, StartPoint.X, StartPoint.Y,
        Param.LineWidth, EndPoint.Y - StartPoint.Y, PATCOPY);
      PatBlt(ACanvas, StartPoint.X, EndPoint.Y,
        EndPoint.X - StartPoint.X + Param.LineWidth, Param.LineWidth, PATCOPY);
    end;
    ldtDownToInvisible, ldtUpToInvisible:
    begin
      PatBlt(ACanvas, EndPoint.X, StartPoint.Y,
        StartPoint.X - EndPoint.X + Param.LineWidth, Param.LineWidth, PATCOPY);
      PatBlt(ACanvas, EndPoint.X, StartPoint.Y,
        Param.LineWidth, EndPoint.Y - StartPoint.Y, PATCOPY);
    end;
    ldtInvisibleLines:
      PatBlt(ACanvas, EndPoint.X, StartPoint.Y,
        Param.LineWidth, EndPoint.Y - StartPoint.Y, PATCOPY);
  end;

  if not Param.DrawArrow then Exit;

  ArrowSize := ToDpi(2);

  case DrawLineType of
    ldtVisibleLines:
    begin
      EndPoint.X := StartPoint.X;
      DrawArrowWithOffset(ACanvas, sdLeft, StartPoint, ArrowSize, True);
      DrawArrowWithOffset(ACanvas, sdRight, EndPoint, ArrowSize, False);
    end;
    ldtDownFromInvisible:
    begin
      DrawArrowWithOffset(ACanvas, sdDown, StartPoint, ArrowSize, True);
      DrawArrowWithOffset(ACanvas, sdRight, EndPoint, ArrowSize, False);
    end;
    ldtUpToInvisible:
    begin
      DrawArrowWithOffset(ACanvas, sdLeft, StartPoint, ArrowSize, False);
      DrawArrowWithOffset(ACanvas, sdUp, EndPoint, ArrowSize, True);
    end;
    ldtDownToInvisible:
    begin
      DrawArrowWithOffset(ACanvas, sdLeft, StartPoint, ArrowSize, True);
      DrawArrowWithOffset(ACanvas, sdDown, EndPoint, ArrowSize, False);
    end;
    ldtUpFromInvisible:
    begin
      DrawArrowWithOffset(ACanvas, sdUp, StartPoint, ArrowSize, False);
      DrawArrowWithOffset(ACanvas, sdRight, EndPoint, ArrowSize, True);
    end;
  end;
end;

function TLinesPostPainter.IsNeedOffsetRow(ARowIndex: Int64;
  FirstRow: Boolean): Boolean;
begin
  Result := False;
end;

function TLinesPostPainter.LineColorPresent(Selected: Boolean;
  const Param: TDrawLineParam; out LineColor: TColor): Boolean;
begin
  if Param.LineColor <> clDefault then
    LineColor := Param.LineColor
  else
    LineColor := clBlack;

  // линия рисуется либо когда она выделена

  // the line is drawn either when it is selected

  if Selected then
    Result := True
  else

    // либо когда отключен флаг "рисовать только выделеные линии"

    // or when the “draw only selected lines” flag is disabled

    Result := not Param.DrawOnlySelectedArrow;
end;

{ TBookmarkPostPainter }

procedure TBookmarkPostPainter.PostPaint(ACanvas: TCanvas;
  StartRow, EndRow: Int64; var Offset: TPoint);

  procedure DrawBack(ARect: TRect);
  begin
    InflateRect(ARect, -1, -1);
    Dec(ARect.Bottom);
    ACanvas.Brush.Color := ColorMap.BookmarkBackgroundColor;
    ACanvas.Pen.Color := ColorMap.BookmarkBorderColor;
    ACanvas.Rectangle(ARect);
  end;

var
  I: TBookMark;
  RowIndex: Int64;
  R: TRect;
begin
  if not (ctWorkSpace in Columns) then Exit;
  if RawData.Count = 0 then Exit;
  for I := Low(TBookMark) to High(TBookMark) do
    if BookMark(I) <> 0 then
    begin
      RowIndex := AddressToRowIndex(BookMark(I));
      if RowIndex < StartRow then Continue;
      if RowIndex > EndRow then Continue;
      R.Left := TextMargin + Offset.X;
      R.Top := GetRowOffset(RowIndex);
      R.Width := DblSize(TextMargin);
      R.Height := RowHeight;
      DrawBack(R);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := ColorMap.BookmarkTextColor;
      OffsetRect(R, (R.Width - CharWidth) div 2, -1 + (R.Height - RowHeight) div 2);
      DrawText(ACanvas, PChar(IntToStr(I)), 1, R, 0);
    end;
end;

{ TCustomHexViewHeader }

constructor TCustomHexViewHeader.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  InitDefault;
end;

procedure TCustomHexViewHeader.DoChange;
begin
  FOwner.DoChange(cmHeader);
end;

procedure TCustomHexViewHeader.DoWidthChange(AType: TColumnType);
var
  AMinWidth, AMaxWidth, AWidth: Integer;
begin
  AWidth := ColumnWidth[AType];
  AMinWidth := FOwner.ToDpi(ColumnMinWidth[AType]);
  if AMinWidth = 0 then
    AMinWidth := FOwner.MinColumnWidth;
  if AWidth < AMinWidth then
    AWidth := AMinWidth;
  AMaxWidth := FOwner.ToDpi(ColumnMaxWidth[AType]);
  if (AMaxWidth > 0) and (AWidth > AMaxWidth) then
    AWidth := AMaxWidth;
  FColumnsData[AType].Width := AWidth;
  UpdateWidth;
end;

procedure TCustomHexViewHeader.DrawHeaderColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
var
  Painter: TAbstractPrimaryRowPainter;
  Flags: DWORD;
begin
  Painter := FOwner.DefaultPainter;
  if AColumn in Painter.ColumnsDrawSupport then
  begin
    Painter.DrawHeaderColumn(ACanvas, AColumn, ARect);
    Exit;
  end;
  Flags := 0;
  case AColumn of
    ctNone:
    begin
      InflateRect(ARect, FOwner.TextMargin, 0);
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := FOwner.ColorMap.HeaderBackgroundColor;
      ACanvas.FillRect(ARect);
      ACanvas.Pen.Color := FOwner.ColorMap.HeaderBorderColor;
      ACanvas.MoveTo(0, ARect.Bottom - 1);
      ACanvas.LineTo(ARect.Right, ARect.Bottom - 1);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := FOwner.ColorMap.HeaderTextColor;
      Exit;
    end;
    ctJmpLine, ctAddress:
      Flags := DT_RIGHT;
  end;
  Painter.DefaultDrawHeaderColumn(ACanvas, ARect, ColumnCaption[AColumn], Flags);
end;

function TCustomHexViewHeader.GetColCaption(AType: TColumnType): string;
begin
  Result := FColumnsData[AType].Caption;
end;

function TCustomHexViewHeader.GetColMaxWidth(AType: TColumnType): Integer;
begin
  Result := FColumnsData[AType].MaxWidth;
end;

function TCustomHexViewHeader.GetColMinWidth(AType: TColumnType): Integer;
begin
  Result := FColumnsData[AType].MinWidth;
end;

function TCustomHexViewHeader.GetColWidth(AType: TColumnType): Integer;
begin
  Result := FColumnsData[AType].Width;
end;

procedure TCustomHexViewHeader.InitDefault;
begin
  FDrawColumnSeparator := True;
  FVisible := True;
  FColumns := [ctAddress, ctOpcode, ctDescription];
  FColumnsData[ctJmpLine].Caption := 'Jumps';
  FColumnsData[ctAddress].Caption := 'Address';
  FColumnsData[ctDescription].Caption := 'Description';
  FColumnsData[ctComment].Caption := 'Comment';
end;

procedure TCustomHexViewHeader.Paint(ACanvas: TCanvas; AScrollXOffset: Integer);
var
  LeftOffset: Integer;
  I: TColumnType;
  ARect: TRect;
begin
  LeftOffset := AScrollXOffset;
  ARect := FOwner.MakeDrawRect(0, 0, FOwner.ClientWidth);
  DrawHeaderColumn(ACanvas, ctNone, ARect);
  for I := ctWorkSpace to High(TColumnType) do
    if I in Columns then
    begin
      ARect := FOwner.MakeDrawRect(LeftOffset, 0, ColumnWidth[I]);
      DrawHeaderColumn(ACanvas, I, ARect);
      Inc(LeftOffset, ColumnWidth[I]);
    end;
end;

procedure TCustomHexViewHeader.SetColCaption(AType: TColumnType;
  const NewCaption: string);
begin
  if ColumnCaption[AType] <> NewCaption then
  begin
    FColumnsData[AType].Caption := NewCaption;
    DoChange;
  end;
end;

procedure TCustomHexViewHeader.SetColMaxWidth(AType: TColumnType; AWidth: Integer);
begin
  if AWidth < ColumnMinWidth[AType] then
    AWidth := ColumnMinWidth[AType];
  if ColumnMaxWidth[AType] <> AWidth then
  begin
    FColumnsData[AType].MaxWidth := AWidth;
    DoWidthChange(AType);
  end;
end;

procedure TCustomHexViewHeader.SetColMinWidth(AType: TColumnType; AWidth: Integer);
begin
  if AWidth < 0 then
    AWidth := 0;
  if (ColumnMaxWidth[AType] > 0) and (AWidth > ColumnMaxWidth[AType]) then
    AWidth := ColumnMaxWidth[AType];
  if ColumnMinWidth[AType] <> AWidth then
  begin
    FColumnsData[AType].MinWidth := AWidth;
    DoWidthChange(AType);
  end;
end;

procedure TCustomHexViewHeader.SetColumns(const Value: TFWHexViewColumnTypes);
begin
  if Columns <> Value then
  begin
    FColumns := Value;
    UpdateWidth;
    DoChange;
  end;
end;

procedure TCustomHexViewHeader.SetColWidth(AType: TColumnType; AWidth: Integer);
begin
  if AType = ctNone then AWidth := 0;
  if ColumnWidth[AType] <> AWidth then
  begin
    FColumnsData[AType].Width := AWidth;
    DoWidthChange(AType);
  end;
end;

procedure TCustomHexViewHeader.SetDrawColumnSeparator(const Value: Boolean);
begin
  if DrawColumnSeparator <> Value then
  begin
    FDrawColumnSeparator := Value;
    DoChange;
  end;
end;

procedure TCustomHexViewHeader.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

procedure TCustomHexViewHeader.UpdateWidth;
var
  I: TColumnType;
begin
  FWidth := 0;
  for I in Columns do
    Inc(FWidth, ColumnWidth[I]);
  DoChange;
end;

{ THexViewInplaceEdit }

function THexViewInplaceEdit.CalcCharAtPoint(X, Y: Integer): Integer;
var
  AHitInfo: TMouseHitInfo;
  ACharOffset: Integer;
begin
  Result := -1;
  if X >= FTextRect.Right then
    Exit(Length(Text));
  AHitInfo := Owner.GetHitInfo(X, Y, []);
  if AHitInfo.SelectPoint.InvalidRow then Exit;
  Result := ConvertRowCharIndexToLocalIndex(AHitInfo.SelectPoint.CharIndex);
  if Result < 0 then
    Result := 0
  else
  begin
    ACharOffset := FTextMetric.CharLength(EditColumn, 0,
      AHitInfo.SelectPoint.CharIndex - 1);
    Inc(ACharOffset, AHitInfo.ColumnStart + Owner.TextMargin);
    if X > ACharOffset + Owner.CharWidth shr 1 then
      Inc(Result);
  end;
end;

procedure THexViewInplaceEdit.CalcEditRect;
var
  AColumnRect: TRect;
  CharPosition, SelPosition: Integer;
begin
  AColumnRect := Owner.GetColumnRect(EditColumn, FRowIndex);
  FEditRect := AColumnRect;
  FEditRect.Left := FEditRect.Left +
    FTextMetric.SelectionLength(EditColumn, 0, FValueOffset - 1);
  Inc(FEditRect.Left, Owner.TextMargin);
  FEditRect.Right := FEditRect.Left +
    FTextMetric.SelectionLength(EditColumn, FValueOffset, FValueOffset);
  FTextRect := FEditRect;
  if FValueOffset <> 0 then
  begin
    CharPosition := FTextMetric.CharLength(EditColumn, 0, FCharIndex - 1);
    SelPosition := FTextMetric.SelectionLength(EditColumn, 0, FValueOffset - 1);
    FTextRect.Left := FEditRect.Left + CharPosition - SelPosition;
  end;
  FTextRect.Width := FTextMetric.ValueMetric.CharCount * Owner.CharWidth;
  if FEditRect.Left = FTextRect.Left then
    Dec(FEditRect.Left, Owner.ToDpi(2));
  if FEditRect.Right = FTextRect.Right then
    Inc(FEditRect.Right, Owner.ToDpi(3));
  if AColumnRect.Right < FEditRect.Right + Owner.SplitMargin then
  begin
    AColumnRect.Right := FEditRect.Right + Owner.SplitMargin;
    Owner.Header.ColumnWidth[EditColumn] := AColumnRect.Width;
  end;
end;

function THexViewInplaceEdit.CanUndo: Boolean;
begin
  Result := FActiveUndoType <> utNone;
end;

procedure THexViewInplaceEdit.Clear;
begin
  InternalSetText('');
  ClearUndo;
end;

procedure THexViewInplaceEdit.ClearSelection;
var
  L, R: Integer;
begin
  GetSelection(L, R);
  if L <> R then
    DoKeyDelete(L, R);
end;

procedure THexViewInplaceEdit.ClearUndo;
begin
  FUndoSnapshot[False] := Default(TUndoSnapshot);
  FUndoSnapshot[True] := Default(TUndoSnapshot);
  FActiveUndoType := utNone;
end;

function THexViewInplaceEdit.ConvertLocalIndexToRowCharIndex(
  Value: Integer): Integer;
begin
  Result := ConvertLocalIndexToValueIndex(Value) + FCharIndex;
end;

function THexViewInplaceEdit.ConvertLocalIndexToValueIndex(
  Value: Integer): Integer;
begin
  Result := Value + FTextMetric.ValueMetric.CharCount - Length(FText);
end;

function THexViewInplaceEdit.ConvertRowCharIndexToLocalIndex(
  Value: Integer): Integer;
begin
  Result := Value - FCharIndex - FTextMetric.ValueMetric.CharCount + Length(FText);
end;

function THexViewInplaceEdit.ConvertTextToEditParam(const AText: string;
  out AData: TEditParam): Boolean;
var
  F32: Single;
  F64: Double;
  F80: Extended;
  {$IF SizeOf(Extended) = 8}
  Extended80: TExtended80Support;
  {$ENDIF}
  UVal: UInt64;

    function GetCheckedFloatStr: string;
    const
      ExponentChars = ['E', 'e'];
    var
      L: Integer;
    begin
      Result := StringReplace(AText, '.', FormatSettings.DecimalSeparator, []);
      Result := StringReplace(Result, ',', FormatSettings.DecimalSeparator, []);
      L := Length(AText);
      if L > 2 then
      begin
        // if the exponent is not entered completely, for example “1.E” or “3.14E-”,
        // we help the string conversion algorithm by adding a zero to the end of it
        if CharInSet(AText[L], ExponentChars) or
          (CharInSet(AText[L - 1], ExponentChars) and CharInSet(AText[L], ['-', '+'])) then
          Result := Result + '0';
      end;
    end;

begin
  Result := False;
  AData := Default(TEditParam);
  AData.ValueSize := FValueSize;
  if AText = '' then Exit(True);
  if AText = '-' then
  begin
    Result := FViewMode in [bvmInt8..bvmInt64, bvmFloat32..bvmFloat80];
    Exit;
  end;
  if Length(AText) > FTextMetric.ValueMetric.CharCount then Exit;
  if AData.ValueSize <= SizeOf(AData.OldValue) then
    Move(FData[FValueOffset], AData.OldValue, AData.ValueSize);
  SetLength(AData.OldExtValue, AData.ValueSize);
  Move(FData[FValueOffset], AData.OldExtValue[0], AData.ValueSize);
  SetLength(AData.NewExtValue, AData.ValueSize);
  case FViewMode of
    bvmHex8..bvmHex64, bvmAddress:
    begin
      if not TryStrToInt64('0x' + AText, AData.NewValue) then Exit;
      Move(AData.NewValue, AData.NewExtValue[0], AData.ValueSize);
    end;
    bvmInt8..bvmInt64:
    begin
      if not TryStrToInt64(AText, AData.NewValue) then Exit;
      Move(AData.NewValue, AData.NewExtValue[0], AData.ValueSize);
    end;
    bvmUInt8..bvmUInt64:
    begin
      if not TryStrToUInt64(AText, UVal) then Exit;
      Move(UVal, AData.NewValue, AData.ValueSize);
      Move(UVal, AData.NewExtValue[0], AData.ValueSize);
    end;
    bvmFloat32:
    begin
      if not TryStrToFloat(GetCheckedFloatStr, F32) then Exit;
      Move(F32, AData.NewValue, 4);
      Move(F32, AData.NewExtValue[0], AData.ValueSize);
    end;
    bvmFloat64:
    begin
      if not TryStrToFloat(GetCheckedFloatStr, F64) then Exit;
      Move(F64, AData.NewValue, 8);
      Move(F64, AData.NewExtValue[0], AData.ValueSize);
    end;
    bvmFloat80:
    begin
      if not TryStrToFloat(GetCheckedFloatStr, F80) then Exit;
      {$IF SizeOf(Extended) = 8}
      Extended80 := DoubleToExtended80(F80);
      Move(Extended80, AData.NewExtValue[0], AData.ValueSize);
      {$ELSE}
      Move(F80, AData.NewExtValue[0], AData.ValueSize);
      {$ENDIF}
    end;
  else
    raise Exception.CreateFmt('Unsupported editor mode: %s',
      [GetEnumName(TypeInfo(TByteViewMode), Integer(FViewMode))]);
  end;
  Result := True;
end;

procedure THexViewInplaceEdit.CopyToClipboard;
var
  L, R: Integer;
begin
  GetSelection(L, R);
  DoKeyCopy(L, R);
end;

constructor THexViewInplaceEdit.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  FDefaultMenu := TPopupMenu.Create(AOwner);
  InitPopupMenu;
  FCaretDownPos := -1;
end;

procedure THexViewInplaceEdit.CutToClipboard;
var
  L, R: Integer;
begin
  GetSelection(L, R);
  if L <> R then
  begin
    DoKeyCopy(L, R);
    DoKeyDelete(L, R);
  end;
end;

destructor THexViewInplaceEdit.Destroy;
begin
  FDefaultMenu.Free;
  inherited;
end;

function THexViewInplaceEdit.DoKeyBackspace(L, R: Integer): Boolean;
var
  ANewText: string;
begin
  Result := True;
  if L <> R then
  begin
    UndoSnapshotMake(utDelete);
    InternalDeleteSelected(L, R);
    UndoSnapshotUpdate(utDelete, UndoSnapshotCalcDelValue(L, R));
    Exit;
  end;
  if L = 0 then Exit(False);
  UndoSnapshotMake(utDelete);
  ANewText := Text;
  Dec(L);
  Delete(ANewText, L + 1, 1);
  InternalSetText(ANewText);
  UpdateSelection(L, 0, L);
  UndoSnapshotUpdate(utDelete, -1);
end;

function THexViewInplaceEdit.DoKeyChar(L, R: Integer;
  AChar: TNativeChar): Boolean;
var
  ANewText: string;
begin
  Result := False;
  ANewText := Text;
  ANewText := Copy(ANewText, 1, L) + AChar + Copy(ANewText, R + 1, Length(ANewText));
  if not Validate(ANewText) then
  begin
    FInvalidState := True;
    Invalidate;
    Beep;
    Exit;
  end;
  UndoSnapshotMake(utPaste);
  InternalSetText(ANewText);
  Inc(L);
  UpdateSelection(L, 0, L);
  UndoSnapshotUpdate(utPaste, 1);
  Result := True;
end;

function THexViewInplaceEdit.DoKeyCopy(L, R: Integer): Boolean;
begin
  Result := L <> R;
  if Result then
    Clipboard.AsText := Copy(Text, L + 1, R - L);
end;

function THexViewInplaceEdit.DoKeyDelete(L, R: Integer): Boolean;
var
  ANewText: string;
begin
  Result := True;
  if L <> R then
  begin
    UndoSnapshotMake(utDelete);
    InternalDeleteSelected(L, R);
    UndoSnapshotUpdate(utDelete, UndoSnapshotCalcDelValue(L, R));
    Exit;
  end;
  ANewText := Text;
  if L = Length(ANewText) then Exit(False);
  UndoSnapshotMake(utDelete);
  Delete(ANewText, L + 1, 1);
  InternalSetText(ANewText);
  UpdateSelection(L, 0, L);
  UndoSnapshotUpdate(utDelete, 1);
end;

function THexViewInplaceEdit.DoKeyEnd(L, R: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := R < Length(Text);
  if ssShift in Shift then
    SetNewSelection(R, Length(Text), False)
  else
    UpdateSelection(Length(Text), 0, Length(Text));
end;

function THexViewInplaceEdit.DoKeyHome(L, R: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := L > 0;
  if ssShift in Shift then
    UpdateSelection(0, L, 0)
  else
    UpdateSelection(0, 0, 0);
end;

function THexViewInplaceEdit.DoKeyLeft(L, R: Integer;
  Shift: TShiftState): Boolean;
var
  OldSel, N: Integer;
begin
  OldSel := SelLength;
  if ssShift in Shift then
  begin
    if CaretPos = L then
      SetNewSelection(LeftValue(Shift, L - 1), R, True)
    else
      SetNewSelection(L, LeftValue(Shift, R - 1), False);
  end
  else
  begin
    if SelLength = 0 then
      N := LeftValue(Shift, L - 1)
    else
      N := LeftValue(Shift, L);
    SetNewSelection(N, N, True)
  end;
  Result := (CaretPos < L) or (SelLength <> OldSel);
end;

function THexViewInplaceEdit.DoKeyPaste: Boolean;
var
  APasteText: string;
  I: Integer;
begin
  Result := False;
  APasteText := Clipboard.AsText;
  for I := 1 to Length(APasteText) do
    if not HandleChar(APasteText[I], []) then
      Exit;
  Result := True;
end;

function THexViewInplaceEdit.DoKeyRight(L, R: Integer;
  Shift: TShiftState): Boolean;
var
  OldSel, N: Integer;
begin
  OldSel := SelLength;
  if ssShift in Shift then
  begin
    if CaretPos = R then
      SetNewSelection(L, RightValue(Shift, R + 1), False)
    else
      SetNewSelection(RightValue(Shift, L + 1), R, True)
  end
  else
  begin
    if SelLength = 0 then
      N := RightValue(Shift, R + 1)
    else
      N := RightValue(Shift, R);
    SetNewSelection(N, N, False);
  end;
  Result := (CaretPos > R) or (SelLength <> OldSel);
end;

function THexViewInplaceEdit.EditColumn: TColumnType;
begin
  Result := ctOpcode;
end;

function THexViewInplaceEdit.GetMenuCaption(AId: TEditMenuId): string;
begin
  case AId of
    emiUndo: Result := 'Undo';
    emiCut: Result := 'Cut';
    emiCopy: Result := 'Copy';
    emiPaste: Result := 'Paste';
    emiDelete: Result := 'Delete';
    emiSelectAll: Result := 'Select All';
    emiSeparator: Result := '-';
  else
    Result := '';
  end;
end;

procedure THexViewInplaceEdit.GetSelection(out L, R: Integer);
begin
  if SelLength > 0 then
  begin
    L := SelStart;
    R := L + SelLength;
  end
  else
  begin
    L := CaretPos;
    R := L;
  end;
end;

function THexViewInplaceEdit.HandleChar(AChar: TNativeChar;
  Shift: TShiftState): Boolean;
var
  L, R: Integer;
begin
  Result := False;
  if not Visible then Exit;
  if Shift <> [] then Exit;
  if CharInSet(AChar{$IFDEF FPC}[1]{$ENDIF}, [#8..#10, #13]) then Exit;
  GetSelection(L, R);
  Result := DoKeyChar(L, R, AChar);
end;

function THexViewInplaceEdit.HandleKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
var
  L, R: Integer;
begin
  if not Visible then Exit(False);
  GetSelection(L, R);
  case Key of
    VK_LEFT, VK_UP: Result := DoKeyLeft(L, R, Shift);
    VK_RIGHT, VK_DOWN: Result := DoKeyRight(L, R, Shift);
    VK_HOME: Result := DoKeyHome(L, R, Shift);
    VK_END: Result := DoKeyEnd(L, R, Shift);
    VK_DELETE: Result := DoKeyDelete(L, R);
    VK_BACK: Result := DoKeyBackspace(L, R);
    VK_INSERT:
    begin
      if ssCtrl in Shift then
        DoKeyCopy(L, R);
      if ssShift in Shift then
        DoKeyPaste;
      Result := True;
    end;
    VK_ESCAPE, VK_RETURN:
    begin
      Hide(Key = VK_RETURN);
      Result := True;
    end;
    // Ctrl+A
    $41:
    begin
      if ssCtrl in Shift then
        SelectAll;
      Result := True;
    end;
    // Ctrl+C
    $43:
    begin
      if ssCtrl in Shift then
        DoKeyCopy(L, R);
      Result := True;
    end;
    // Ctrl+V
    $56:
    begin
      if ssCtrl in Shift then
        DoKeyPaste;
      Result := True;
    end;
    // Ctrl+X
    $58:
    begin
      if ssCtrl in Shift then
      begin
        DoKeyCopy(L, R);
        DoKeyDelete(L, R);
      end;
      Result := True;
    end;
    // Ctrl+Z
    $5A:
    begin
      if ssCtrl in Shift then
        UndoSnapshotRevert;
      Result := True;
    end;
  else
    Result := False;
  end;
  if Result then
  begin
    Owner.InvalidateCaretPosData(True);
    Owner.UpdateCaretTimer;
  end;
end;

function THexViewInplaceEdit.HandleMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := Visible and PtInRect(EditRect, Point(X, Y));
  if Result then
  begin
    Owner.SetFocus;
    if ssDouble in Shift then
      SelectAll
    else
    begin
      FCaretDownPos := CalcCharAtPoint(X, Y);
      UpdateSelection(FCaretDownPos, 0, FCaretDownPos);
    end;
  end
  else
    Hide;
end;

function THexViewInplaceEdit.HandleMouseMove(Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  NewCaretPos: Integer;
begin
  Result := Visible and ((FCaretDownPos >= 0) or PtInRect(EditRect, Point(X, Y)));
  if not Result then Exit;
  if FCaretDownPos >= 0 then
  begin
    NewCaretPos := CalcCharAtPoint(X, Y);
    UpdateSelection(Min(FCaretDownPos, NewCaretPos),
      Abs(FCaretDownPos - NewCaretPos), NewCaretPos);
  end;
end;

function THexViewInplaceEdit.HandleMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := Visible;
  FCaretDownPos := -1;
end;

procedure THexViewInplaceEdit.Hide(Apply: Boolean);
var
  ACursor: TDrawParam;
  AData: TEditParam;
  Handled: Boolean;
begin
  if not Visible then Exit;
  Visible := False;
  Apply := Apply and (FInitialText <> Text);
  if Apply and Modifyed then
  begin
    ACursor.Column := EditColumn;
    ACursor.RowIndex := FRowIndex;
    ACursor.CharIndex := FCharIndex;
    ACursor.ValueOffset := FValueOffset;
    ACursor.AddrVA := Owner.RawData[FRowIndex].Address + FValueOffset;
    if ConvertTextToEditParam(Text, AData) then
    begin
      Handled := False;
      Owner.DoOnEdit(ACursor, AData, Handled);
      FModifyed := False;
    end;
  end;
end;

procedure THexViewInplaceEdit.InitPopupMenu;

  procedure AddMenuItem(AId: TEditMenuId);
  var
    AItem: TMenuItem;
  begin
    AItem := TMenuItem.Create(FDefaultMenu);
    AItem.Caption := GetMenuCaption(AId);
    AItem.Tag := Integer(AId);
    AItem.OnClick := OnMenuItemClick;
    FDefaultMenu.Items.Add(AItem);
  end;

begin
  AddMenuItem(emiUndo);
  AddMenuItem(emiSeparator);
  AddMenuItem(emiCut);
  AddMenuItem(emiCopy);
  AddMenuItem(emiPaste);
  AddMenuItem(emiDelete);
  AddMenuItem(emiSeparator);
  AddMenuItem(emiSelectAll);
end;

procedure THexViewInplaceEdit.InternalDeleteSelected(L, R: Integer);
var
  ANewText: string;
begin
  ANewText := Text;
  Delete(ANewText, L + 1, R - L);
  InternalSetText(ANewText);
  UpdateSelection(L, 0, L);
end;

procedure THexViewInplaceEdit.InternalSetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Invalidate;
  end;
end;

procedure THexViewInplaceEdit.Invalidate;
begin
  InvalidateRect(Owner.Handle, @FEditRect, False);
end;

function THexViewInplaceEdit.LeftValue(Shift: TShiftState;
  AValue: Integer): Integer;
begin
  if ssCtrl in Shift then
    Result := 0
  else
    Result := AValue;
end;

procedure THexViewInplaceEdit.OnMenuItemClick(Sender: TObject);
begin
  case TEditMenuId(TMenuItem(Sender).Tag) of
    emiUndo: UndoSnapshotRevert;
    emiCut: CutToClipboard;
    emiCopy: CopyToClipboard;
    emiPaste: PasteFromClipboard;
    emiDelete: ClearSelection;
    emiSelectAll: SelectAll;
  end;
end;

procedure THexViewInplaceEdit.PaintTo(ACanvas: TCanvas);
var
  R: TRect;
  AColor: TColor;
  ACaretOffset, ACaretWidth, ACaretIndex, ACorrection: Integer;
  ADrawText: string;
begin
  if FInvalidState then
  begin
    ACanvas.Pen.Color := Owner.ColorMap.EditErrorBorderColor;
    FInvalidState := False;
  end
  else
    ACanvas.Pen.Color := Owner.ColorMap.EditBorderColor;
  ACanvas.Brush.Color := Owner.ColorMap.EditBackgroundColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Rectangle(EditRect);
  if FText <> '' then
  begin
    ACanvas.Brush.Style := bsClear;
    R := FTextRect;
    AColor := Owner.RawData[FRowIndex].Color;
    if AColor = clDefault then
      AColor := Owner.ColorMap.EditTextColor;
    ACanvas.Font.Color := AColor;
    ADrawText := StringOfChar(' ',
      FTextMetric.ValueMetric.CharCount - Length(Text)) + Text;
    ExtTextOut(ACanvas, R.Left, R.Top, ETO_CLIPPED, @R,
      @ADrawText[1], UTF8StringLength(ADrawText),
      FTextMetric.CharPointer(EditColumn, 0));
    if SelLength > 0 then
    begin
      ADrawText := UTF8Copy(Text, SelStart + 1, SelLength);
      Inc(R.Left, ConvertLocalIndexToValueIndex(SelStart) * Owner.CharWidth);
      R.Right := R.Left + SelLength * Owner.CharWidth;
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := clHighlight;
      InflateRect(R, 0, Owner.ToDpi(-2));
      ACanvas.FillRect(R);
      InflateRect(R, 0, Owner.ToDpi(2));
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := clHighlightText;
      ExtTextOut(ACanvas, R.Left, R.Top, ETO_CLIPPED, @R,
        @ADrawText[1], UTF8StringLength(ADrawText),
        FTextMetric.CharPointer(EditColumn, 0));
    end;
  end;
  if not Owner.CaretPosData.Showed then Exit;
  ACaretWidth := Owner.ToDpi(1);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWindowText;
  if FCaretPos = Length(Text) then
  begin
    ACorrection := Owner.CharWidth;
    ACaretIndex := FCaretPos - 2;
  end
  else
  begin
    ACorrection := 0;
    ACaretIndex := FCaretPos - 1;
  end;
  ACaretOffset := FTextMetric.CharLength(EditColumn, 0,
    ConvertLocalIndexToValueIndex(ACaretIndex));
  Inc(ACaretOffset, FTextRect.Left + ACorrection);
  PatBlt(ACanvas, ACaretOffset, EditRect.Top + Owner.ToDpi(2),
    ACaretWidth, EditRect.Height - Owner.ToDpi(4), PATCOPY);
end;

procedure THexViewInplaceEdit.PasteFromClipboard;
begin
  DoKeyPaste;
end;

function THexViewInplaceEdit.RightValue(Shift: TShiftState;
  AValue: Integer): Integer;
begin
  if ssCtrl in Shift then
    Result := Length(Text)
  else
    Result := AValue;
end;

procedure THexViewInplaceEdit.SelectAll;
begin
  UpdateSelection(0, Length(Text), Length(Text));
end;

procedure THexViewInplaceEdit.SetCaretPos(Value: Integer);
begin
  UpdateSelection(SelStart, SelLength, Value);
end;

procedure THexViewInplaceEdit.SetNewSelection(L, R: Integer;
  CaretAtLeft: Boolean);
var
  Tmp: Integer;
begin
  if L > R then
  begin
    Tmp := L;
    L := R;
    R := Tmp;
    CaretAtLeft := not CaretAtLeft;
  end;
  if CaretAtLeft then
    UpdateSelection(L, R - L, L)
  else
    UpdateSelection(L, R - L, R);
end;

procedure THexViewInplaceEdit.SetSelLength(const Value: Integer);
begin
  if SelLength <> Value then
    UpdateSelection(SelStart, Value, CaretPos);
end;

procedure THexViewInplaceEdit.SetSelStart(const Value: Integer);
begin
  if SelStart <> Value then
    UpdateSelection(Value, SelLength, CaretPos);
end;

procedure THexViewInplaceEdit.SetSelText(const Value: string);
var
  L, R: Integer;
  ANewText: string;
begin
  GetSelection(L, R);
  ANewText := Text;
  ANewText := Copy(ANewText, 1, L) + Value + Copy(ANewText, R + 1, Length(ANewText));
  Text := ANewText;
  ClearUndo;
end;

procedure THexViewInplaceEdit.SetText(const Value: string);
begin
  if Validate(Value) then
    InternalSetText(Value);
end;

procedure THexViewInplaceEdit.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then
  begin
    FVisible := Value;
    if Value then
      SelectAll;
    Invalidate;
  end;
end;

procedure THexViewInplaceEdit.Show(APainter: TAbstractPrimaryRowPainter);
var
  AValueMetric: TValueMetric;
  AFormatMode: TFormatMode;
begin
  FActiveUndoType := utNone;
  FPainter := APainter;
  FCharIndex := Owner.CaretPosData.CharIndex;
  FModifyed := False;
  FRowIndex := FPainter.RowIndex;
  FValueOffset := FPainter.CharIndexToValueOffset(EditColumn, FCharIndex);
  FCharIndex := FPainter.ValueOffsetToCharIndex(EditColumn, FValueOffset);
  FTextMetric := FPainter.TextMetric;
  CalcEditRect;
  Owner.GetRawBuff(FRowIndex, FData);
  AValueMetric := FTextMetric.ValueMetric;
  FViewMode := FPainter.ByteViewMode;
  FFormatMode := FPainter.FormatMode;
  AFormatMode := DefFormatMode;
  AFormatMode.Align := False;
  FValueSize := Min(AValueMetric.ByteCount, Length(FData) - FValueOffset);
  FInitialText := RawBufToViewMode(@FData[FValueOffset],
    FValueSize, AValueMetric, FViewMode, AFormatMode);
  FText := FInitialText;
  if Visible then
    SelectAll
  else
    Visible := True;
end;

procedure THexViewInplaceEdit.TrackPopupMenuAtPos(MousePos: TPoint);

  function GetMenuItem(AId: TEditMenuId): TMenuItem;
  begin
    case AId of
      emiUndo: Result := FDefaultMenu.Items[0];
      emiCut: Result := FDefaultMenu.Items[2];
      emiCopy: Result := FDefaultMenu.Items[3];
      emiPaste: Result := FDefaultMenu.Items[4];
      emiDelete: Result := FDefaultMenu.Items[5];
      emiSelectAll: Result := FDefaultMenu.Items[7];
    else
      Result := nil;
    end;
  end;

begin
  MousePos := Owner.ClientToScreen(MousePos);
  GetMenuItem(emiUndo).Enabled := CanUndo;
  GetMenuItem(emiCut).Enabled := SelLength > 0;
  GetMenuItem(emiCopy).Enabled := SelLength > 0;
  GetMenuItem(emiPaste).Enabled := Clipboard.HasFormat(CF_TEXT);
  GetMenuItem(emiDelete).Enabled := SelLength > 0;
  GetMenuItem(emiSelectAll).Enabled := SelLength < Length(Text);
  FDefaultMenu.Popup(MousePos.X, MousePos.Y);
end;

procedure THexViewInplaceEdit.Undo;
begin
  UndoSnapshotRevert;
end;

function THexViewInplaceEdit.UndoSnapshotCalcDelValue(L, R: Integer): Integer;
begin
  if FUndoSnapshot[True].WaitUpdate then Exit(0);
  if (L = FUndoSnapshot[True].SelStart) then
    Result := R - L
  else
    if (R = FUndoSnapshot[True].SelStart) then
      Result := L - R
    else
      Result := 0;
end;

procedure THexViewInplaceEdit.UndoSnapshotMake(AUndoType: TUndoType);

  procedure MakeNewSnapshot;
  begin
    if (AUndoType = utPaste) and (FActiveUndoType = utDelete) then
    begin
      // FUndoSnapshot[False] is valid!
    end
    else
    begin
      FUndoSnapshot[False].Text := Text;
      FUndoSnapshot[False].SelStart := SelStart;
      FUndoSnapshot[False].SelEnd := SelStart + SelLength;
    end;
    FActiveUndoType := AUndoType;
    FUndoSnapshot[True] := FUndoSnapshot[False];
    FUndoSnapshot[True].WaitUpdate := True;
  end;

begin
  try
    if (FActiveUndoType <> AUndoType) or FUndoSnapshotReverted then
    begin
      MakeNewSnapshot;
      Exit;
    end;
    if (AUndoType = utDelete) and UndoSnapshotNeedReset then
    begin
      MakeNewSnapshot;
      Exit;
    end;
    if (AUndoType = utPaste) and (FUndoSnapshot[True].SelEnd <> FCaretPos) then
    begin
      FUndoSnapshot[False].Text := Text;
      FUndoSnapshot[False].SelStart := SelStart;
      FUndoSnapshot[False].SelEnd := SelStart + SelLength;
      FUndoSnapshot[True].SelStart := SelStart;
      FUndoSnapshot[True].SelEnd := SelStart;
    end;
  finally
    FUndoSnapshotReverted := False;
  end;
end;

function THexViewInplaceEdit.UndoSnapshotNeedReset: Boolean;
begin
  Result :=
    (FUndoSnapshot[True].SelStart <> FCaretPos) and
    (UndoSnapshotCalcDelValue(SelStart, SelStart + SelLength) = 0);
end;

procedure THexViewInplaceEdit.UndoSnapshotRevert;
var
  L, R: Integer;
begin
  if FActiveUndoType = utNone then Exit;
  Text := FUndoSnapshot[FUndoSnapshotReverted].Text;
  if FActiveUndoType = utDelete then
  begin
    if FUndoSnapshotReverted then
    begin
      L := FUndoSnapshot[True].SelStart;
      R := L;
    end
    else
    begin
      L := FUndoSnapshot[False].SelStart;
      R := FUndoSnapshot[False].SelEnd;
    end;
  end
  else
  begin
    if FUndoSnapshotReverted then
    begin
      L := FUndoSnapshot[True].SelStart;
      R := FUndoSnapshot[True].SelEnd;
    end
    else
    begin
      L := FUndoSnapshot[False].SelStart;
      R := FUndoSnapshot[False].SelEnd;
    end;
  end;
  SetNewSelection(L, R, False);
  FUndoSnapshotReverted := not FUndoSnapshotReverted;
end;

procedure THexViewInplaceEdit.UndoSnapshotUpdate(AUndoType: TUndoType;
  AValue: Integer);
begin
  FUndoSnapshot[True].Text := Text;
  FUndoSnapshot[True].WaitUpdate := False;
  if AUndoType = utDelete then
  begin
    if AValue < 0 then
      Inc(FUndoSnapshot[False].SelStart, AValue)
    else
      Inc(FUndoSnapshot[False].SelEnd, AValue);
    FUndoSnapshot[True].SelStart := FCaretPos;
  end;
  if AUndoType = utPaste then
    FUndoSnapshot[True].SelEnd := FCaretPos;
  FModifyed := True;
end;

procedure THexViewInplaceEdit.UpdateSelection(AStart, ALen, ACaret: Integer);
var
  L: Integer;
begin
  L := Length(Text);
  if AStart < 0 then
    AStart := 0;
  if AStart > L then
    AStart := L;
  FSelStart := AStart;

  if ALen < 0 then
    ALen := 0;
  if AStart + ALen > L then
    ALen := L - AStart;
  FSelLength := ALen;

  if ACaret < 0 then
    ACaret := 0;
  if ACaret > L then
    ACaret := L;
  if FCaretPos <> ACaret then
  begin
    FCaretPos := ACaret;
    Owner.InvalidateCaretPosData(True);
    Owner.UpdateCaretTimer;
  end;

  Invalidate;
end;

function THexViewInplaceEdit.Validate(const ANewText: string): Boolean;
var
  ACheckData: TEditParam;
begin
  Result := ConvertTextToEditParam(ANewText, ACheckData);
end;

{ TViewShortCut }

procedure TViewShortCut.AssignTo(Dest: TPersistent);
begin
  if Dest is TViewShortCut then
  begin
    if Assigned(TViewShortCut(Dest).SecondaryShortCuts) then
    begin
      if Assigned(FSecondaryShortCuts) then
        TViewShortCut(Dest).SecondaryShortCuts := SecondaryShortCuts
      else
        TViewShortCut(Dest).SecondaryShortCuts.Clear;
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
  FSecondaryShortCuts.Free;
  inherited;
end;

function TViewShortCut.GetSecondaryShortCuts: TShortCutList;
begin
  if FSecondaryShortCuts = nil then
    FSecondaryShortCuts := TShortCutList.Create;
  Result := FSecondaryShortCuts;
end;

function TViewShortCut.IsCustomViewShortCutStored: Boolean;
begin
  Result := IsSecondaryStored or IsShortSutStored;
end;

function TViewShortCut.IsSecondaryStored: Boolean;
begin
  Result := Assigned(FSecondaryShortCuts) and (FSecondaryShortCuts.Count > 0);
end;

function TViewShortCut.IsShortCut(Key: Word; Shift: TShiftState): Boolean;
var
  AShortCut: TShortCut;
  I: Integer;
begin
  Result := False;
  AShortCut := Menus.ShortCut(Key, Shift);
  if ShortCut = AShortCut then Exit(True);
  if FSecondaryShortCuts = nil then Exit;
  for I := 0 to FSecondaryShortCuts.Count - 1 do
    if FSecondaryShortCuts.ShortCuts[I] = AShortCut then
      Exit(True);
end;

function TViewShortCut.IsShortSutStored: Boolean;
begin
  Result := ShortCut <> FDefault;
end;

procedure TViewShortCut.SetSecondaryShortCuts(const Value: TShortCutList);
begin
  SecondaryShortCuts.Assign(Value);
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

{ TJumpStack }

function TJumpStack.Add(const AValue: TJmpItem): Integer;
begin
  FStack.Count := FUndoIndex;
  FStack.Add(AValue);
  FUndoIndex := FStack.Count;
  Result := FUndoIndex;
  FCurrent := AValue;
end;

function TJumpStack.CanRedo: Boolean;
begin
  Result := FUndoIndex < FStack.Count;
end;

function TJumpStack.CanUndo: Boolean;
begin
  Result := FUndoIndex > 0;
end;

procedure TJumpStack.Clear;
begin
  FCurrent := Default(TJmpItem);
  FStack.Clear;
  FUndoIndex := 0;
end;

constructor TJumpStack.Create;
begin
  FStack := TList<TJmpItem>.Create;
end;

destructor TJumpStack.Destroy;
begin
  FStack.Free;
  inherited;
end;

function TJumpStack.Redo: Boolean;
begin
  Result := CanRedo;
  if Result then
  begin
    Inc(FUndoIndex);
    FCurrent := FStack[FUndoIndex - 1];
  end;
end;

function TJumpStack.Undo: Boolean;
begin
  Result := CanUndo;
  if Result then
  begin
    FCurrent := FStack[FUndoIndex - 1];
    Dec(FUndoIndex);
  end;
end;

{ TFWCustomHexView }

function TFWCustomHexView.AddressToRowIndex(Value: Int64): Int64;
begin
  Result := RawData.AddressToRowIndex(Value);
end;

function TFWCustomHexView.AddressToSelectPoint(Value: Int64): TSelectPoint;
var
  Painter: TAbstractPrimaryRowPainter;
begin
  Result.RowIndex := RawData.AddressToRowIndex(Value);
  if Result.RowIndex < 0 then
  begin
    Result.Erase;
    Exit;
  end;
  Result.Column := ctOpcode;
  Result.ValueOffset := Value - RawData[Result.RowIndex].Address;
  Painter := GetRowPainter(Result.RowIndex, True);
  if Assigned(Painter) then
    Result.CharIndex := Painter.ValueOffsetToCharIndex(ctOpcode, Result.ValueOffset)
  else
    Result.CharIndex := -1;
end;

procedure TFWCustomHexView.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
begin
  if Parent = nil then
  begin
    inherited;
    Exit;
  end;
  DoChangeScale(True);
  inherited;
  FCurrentPPI := AToPPI;
  DoChangeScale(False);
end;

procedure TFWCustomHexView.DoChangeScale(BeforeScaleStep: Boolean);
begin
  if BeforeScaleStep then
  begin
    // Column resizing is proportional to font size, not DPI.
    // Save previos value
    UpdateTextExtent;
    SaveViewParam;
  end
  else
  begin
    UpdateTextExtent;
    FTextMargin := ToDpi(NoDpiTextMargin);
    FSplitMargin := ToDpi(NoDpiSplitMargin);
    FMinColumnWidth := FTextMargin shl 2;
    RestoreViewParam;
  end;
end;

procedure TFWCustomHexView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  if InplaceEdit.Visible then
  begin
    if PtInRect(InplaceEdit.EditRect, MousePos) then
    begin
      DoEditContextPopup(MousePos, Handled);
      if not Handled then
        InplaceEdit.TrackPopupMenuAtPos(MousePos);
      Handled := True;
      Exit;
    end
    else
      InplaceEdit.Hide;
  end;
  inherited;
end;

procedure TFWCustomHexView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TFWCustomHexView.ByteViewModeCommandEnabled(
  Value: TByteViewMode; var AChecked: Boolean): Boolean;
begin
  Result := True;
  AChecked := ByteViewMode = Value;
end;

function TFWCustomHexView.ByteViewModeCommandHandled(
  Value: TByteViewMode): Boolean;
begin
  Result := True;
end;

function TFWCustomHexView.CalculateColumnBestSize(Value: TColumnType): Integer;
var
  AddrBuf: string;
begin
  case Value of
    ctAddress:
    begin
      AddrBuf := StringOfChar('0',
        DefaultPainter.CalcColumnLengthForCopy(ctAddress));
      Result := MeasureCanvas.TextWidth(AddrBuf);
    end;
    ctOpcode, ctDescription:
      Result := TextMetric.SelectionLength(Value, 0, BytesInRow - 1);
    ctComment:
    begin
      Result := TextMetric.SelectionLength(ctDescription, 0, BytesInRow - 1);
      Inc(Result, Result);
    end;
  else
    if Value in DefaultPainter.ColumnsDrawSupport then
      Result := MeasureCanvas.TextWidth(
        DefaultPainter.GetHeaderColumnCaption(Value))
    else
      Result := MeasureCanvas.TextWidth(Header.ColumnCaption[Value]);
  end;
  Inc(Result, TextMargin shl 1);
end;

function TFWCustomHexView.CaretPosToAddress(const Value: TCaretPosData): Int64;
var
  ASelectPoint: TSelectPoint;
begin
  ASelectPoint := SelectPoint(Value.RowIndex, Value.CharIndex, Value.Column);
  Result := RowToAddress(ASelectPoint.RowIndex, ASelectPoint.ValueOffset);
end;

procedure TFWCustomHexView.ChangeScale(M, D: Integer
  {$IFNDEF FPC}; isDpiChange: Boolean{$ENDIF});
begin
  DoChangeScale(True);
  inherited;
  DoChangeScale(False);
end;

procedure TFWCustomHexView.ClearSelection(ResetCaretPos: Boolean);
begin
  FSelStart.Erase;
  FSelEnd.Erase;
  FSelStartAddr := -1;
  FSelEndAddr := -1;
  if ResetCaretPos then
  begin
    FillChar(FCaretPosData, SizeOf(FCaretPosData), 0);
    if HandleAllocated then
      SetNewEditRowIndex(-1)
    else
      FCaretPosData.RowIndex := -1;
  end;
end;

procedure TFWCustomHexView.CMHintShow(var Message: TCMHintShow);
var
  HintParam: THintParam;
  Painter: TAbstractPrimaryRowPainter;
  Offset: TPoint;
begin
  HintParam := Default(THintParam);
  HintParam.MouseHitInfo := GetHitInfo(
    Message.HintInfo.CursorPos.X, Message.HintInfo.CursorPos.Y, SavedShift);
  Painter := GetRowPainter(HintParam.MouseHitInfo.SelectPoint.RowIndex);
  if Painter = nil then Exit;
  HintParam.AddrVA := Painter.RowToAddress(
    HintParam.MouseHitInfo.SelectPoint.RowIndex,
    HintParam.MouseHitInfo.SelectPoint.ValueOffset);
  Offset := GetRowOffsetPoint(Painter.RowIndex);
  Message.HintInfo.CursorRect := Painter.ColumnRect(Offset, HintParam.MouseHitInfo.SelectPoint.Column);
  Message.HintInfo.HideTimeout := HintHideTimeout;
  HintParam.HintInfo := Message.HintInfo;
  DoGetHint(HintParam, Message.HintInfo.HintStr);
end;

procedure TFWCustomHexView.CMHintShowPause(var Message: TCMHintShowPause);
begin
  if Message.WasActive = 1 then
    inherited
  else
    Message.Pause^ := HintShowPause;
end;

function TFWCustomHexView.ColumnAsString(ARowIndex: Int64; AColumn: TColumnType
  ): string;
var
  Painter: TAbstractPrimaryRowPainter;
begin
  Result := '';
  begin
    Painter := GetRowPainter(ARowIndex);
    if Assigned(Painter) then
      Result := Painter.ColumnAsString(AColumn);
  end;
end;

function TFWCustomHexView.ColumnResetSelection(AColumn: TColumnType): Boolean;
begin
  Result := AColumn in [ctNone, ctWorkSpace, ctJmpLine];
end;

function TFWCustomHexView.CopyCommandEnabled(Value: TCopyStyle): Boolean;
begin
  Result := Focused and not (FSelStart.InvalidRow or FSelEnd.InvalidRow);
  if Result and InplaceEdit.Visible then
  begin
    Result := Value = csAsText;
    if Result then
      Result := InplaceEdit.SelLength > 0;
  end;
end;

function TFWCustomHexView.CopyCommandHandled(Value: TCopyStyle): Boolean;
begin
  Result := True;
end;

procedure TFWCustomHexView.CopySelected(CopyStyle: TCopyStyle);
var
  RawBuff: TBytes;
  I, StartRow, EndRow: Int64;
  Builder: TSimplyStringBuilder;
  Painter: TAbstractPrimaryRowPainter;
begin
  if InplaceEdit.Visible then
  begin
    if CopyStyle = csAsText then
      InplaceEdit.CopyToClipboard;
    Exit;
  end;

  if not GetSelectedRow(StartRow, EndRow) then Exit;

  if CopyStyle = csAddress then
  begin
    if IgnoreSelectionWhenCopyAddress or not (LeftSelPoint.Column in [ctOpcode, ctDescription]) then
    begin
      Painter := GetRowPainter(SelectedRowIndex);
      Clipboard.AsText := Painter.ColumnAsString(ctAddress);
    end
    else
      Clipboard.AsText := IntToHex(SelectPointToAddress(LeftSelPoint));
    Exit;
  end;

  if CopyStyle = csAsText then
  begin
    Builder := TSimplyStringBuilder.Create;
    try
      I := StartRow;
      while I <= EndRow do
      begin
        Painter := GetRowPainter(I);
        if Painter = nil then
          Builder.Append(sLineBreak)
        else
          Painter.CopyRowAsString(Builder);
        Inc(I);
      end;
      Clipboard.AsText := Builder.AsString(False);
    finally
      Builder.Free;
    end;
    Exit;
  end;

  RawBuff := GetSelectedRawBuff(StartRow, EndRow);
  if RawBuff = nil then Exit;

  case CopyStyle of
    csBytes: Clipboard.AsText := RawBufToHex(@RawBuff[0], Length(RawBuff));
    csPascal: Clipboard.AsText := RawBufToPasArray(@RawBuff[0], Length(RawBuff));
    csCpp: Clipboard.AsText := RawBufToCPPArray(@RawBuff[0], Length(RawBuff));
    csAsmOpcodes: Clipboard.AsText := RawBufToAsmDB(@RawBuff[0], Length(RawBuff));
  end;
end;

procedure TFWCustomHexView.CopySelectedToStream(AStream: TStream);
var
  RawBuff: TBytes;
  StartRow, EndRow: Int64;
begin
  if GetSelectedRow(StartRow, EndRow) then
  begin
    RawBuff := GetSelectedRawBuff(StartRow, EndRow);
    if RawBuff <> nil then
      AStream.WriteBuffer(RawBuff[0], Length(RawBuff));
  end;
end;

constructor TFWCustomHexView.Create(AOwner: TComponent);
begin
  inherited;

  {$IFDEF FPC}
  FCurrentPPI := USER_DEFAULT_SCREEN_DPI;
  {$ENDIF}

  Width := 185;
  Height := 89;
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csNeedsBorderPaint] -
    [csSetCaption, csParentBackground];
  TabStop := True;
  FBorderStyle := bsSingle;

  FAcceptSelForced := True;
  FAddressMode := am32bit;
  FAddressViewAutoToggle := True;
  FAddressViewOffsetBase := -1;
  FBytesInGroup := 8;
  FBytesInColorGroup := 4;
  FBytesInRow := 16;
  FEncoder := TCharEncoder.Create(Self);
  FHintHideTimeout := 7000;
  FHintShowPause := 300;
  FInplaceEdit := GetInplaceEditClass.Create(Self);
  FJumpStack := TJumpStack.Create;
  FScrollBars := TScrollStyle.ssBoth;
  FSeparateGroupByColor := True;
  FTextMargin := ToDpi(NoDpiTextMargin);
  FSelectOnMouseDown := True;
  FSelectOnMouseMove := True;
  FSplitMargin := ToDpi(NoDpiSplitMargin);
  FShortCuts := GetShortCutsClass.Create;
  FMeasureCanvas := TBitmap.Create;
  FMeasureCanvas.SetSize(1, 1);
  FMinColumnWidth := FTextMargin shl 2;
  FReadOnly := True;

  FColorMap := GetColorMapClass.Create(Self);
  FRawData := GetRawDataClass.Create(Self);
  FHeader := GetHeaderClass.Create(Self);
  FPainters := TObjectList<TAbstractPrimaryRowPainter>.Create;
  FPostPainters := TObjectList<TAbstractPostPainter>.Create;
  FSelections := TSelections.Create(Self);
  FSelections.OnChange := OnSelectionsChange;
  FTextMetrics := TObjectList<TAbstractTextMetric>.Create;
  InitPainters;
  InitDefault;
  ClearSelection;

  FOldOnFontChange := Font.OnChange;
  Font.OnChange := DoFontChange;
end;

procedure TFWCustomHexView.CreateCaretTimer;
var
  NeedStartTimer: Boolean;
begin
  if not Focused then Exit;
  case FCaretPosData.EditMode of
    cemSingleChar: NeedStartTimer := True;
    cemValueEditor: NeedStartTimer := InplaceEdit.Visible;
  else
    NeedStartTimer := False;
  end;
  if NeedStartTimer then
  begin
    FCaretTimerEnabled := True;
    SetTimer(Handle, 0, GetCaretBlinkTime, nil);
  end;
end;

procedure TFWCustomHexView.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD =
    (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL
    {$IFDEF FPC}
    , WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL
    {$ENDIF}
    );
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited;
  with Params do
  begin
    Style := ((Style or WS_CLIPCHILDREN or WS_TABSTOP) or
      BorderStyles[FBorderStyle]) or LBS_OWNERDRAWFIXED or LBS_MULTICOLUMN or
      ScrollBar[ScrollBars];
    if NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TFWCustomHexView.CreateWnd;
begin
  inherited;
  DoChange(cmHandle);
  if FNeedFitBesSizes then
    FitColumnsToBestSize;
end;

function TFWCustomHexView.CurrentVisibleRow: Int64;
begin
  if FRowHeight = 0 then Exit(0);
  Result := -FScrollOffset.Y div FRowHeight;
  if Result < 0 then
  begin
    SetScrollOffset(FScrollOffset.X, 0);
    Result := 0;
  end;
end;

procedure TFWCustomHexView.DblClick;
var
  NewAddressViewOffsetBase: Int64;
  Painter: TAbstractPrimaryRowPainter;
begin
  if AddressViewAutoToggle then
  begin
    if CaretPosData.Column = ctAddress then
    begin
      NewAddressViewOffsetBase := RowToAddress(CaretPosData.RowIndex, 0);
      if NewAddressViewOffsetBase = AddressViewOffsetBase then
        NewAddressViewOffsetBase := -1;
      AddressViewOffsetBase := NewAddressViewOffsetBase;
    end;
  end;
  try
    inherited;
    if not EditAtCaretPos then
    begin
      Painter := GetRowPainter(MousePressedHitInfo.SelectPoint.RowIndex);
      UpdateCaretPosData(MousePressedHitInfo.SelectPoint,
        GetCaretChangeMode(Painter, FCaretPosData.Column, SavedShift + [ssDouble]));
    end;
  finally
    FMousePressed := False;
  end;
end;

procedure TFWCustomHexView.DestroyHandle;
begin
  DestroyCaretTimer;
  inherited DestroyHandle;
end;

destructor TFWCustomHexView.Destroy;
begin
  DestroyCaretTimer;
  FShortCuts.Free;
  FJumpStack.Free;
  FPostPainters.Free;
  FPainters.Free;
  FSelections.OnChange := nil;
  FSelections.Free;
  FTextMetrics.Free;
  FHeader.Free;
  FRawData.Free;
  FEncoder.Free;
  FColorMap.Free;
  FMeasureCanvas.Free;
  FInplaceEdit.Free;
  ReleaseDataStream;
  inherited;
end;

procedure TFWCustomHexView.DestroyCaretTimer;
begin
  if FCaretTimerEnabled and HandleAllocated then
  begin
    InternalKillCaretTimer;
    InvalidateCaretPosData(False);
  end;
end;

procedure TFWCustomHexView.DoCaretDown(Shift: TShiftState; ForcedLine: Integer);
var
  NewRowIndex, NewCharIndex, MaxCharIndex: Integer;
  Painter: TAbstractPrimaryRowPainter;
begin
  NewRowIndex := GetCaretNextRowIndex(FCaretPosData.RowIndex + ForcedLine);
  if NewRowIndex >= RawData.Count then
  begin
    DoCaretEnd(Shift);
    Exit;
  end;
  NewCharIndex := FCaretPosData.CharIndex;
  Painter := GetRowPainter(NewRowIndex);
  if Painter = nil then Exit;
  MaxCharIndex := Painter.CharCount(FCaretPosData.Column);
  if NewCharIndex < 0 then
    NewCharIndex := 0;
  if NewCharIndex >= MaxCharIndex then
    NewCharIndex := MaxCharIndex - 1;
  UpdateCaretPosData(
    SelectPoint(NewRowIndex, NewCharIndex, FCaretPosData.Column),
    GetCaretChangeMode(Painter, FCaretPosData.Column, Shift));
end;

procedure TFWCustomHexView.DoCaretEdit(AKey: TNativeChar);
var
  Painter: TAbstractPrimaryRowPainter;
  ValueMetric: TValueMetric;
  HexChar: Char;
  StrData, ValueData: string;
  ValueStartIndex, CaretIndex, CharLen, ARawLength, OldRowIndex: Integer;
  ACursor: TDrawParam;
  AData: TEditParam;
  Handled: Boolean;
  NewCharBytes: TBytes;
  Data: TBytes;
  AEditorMode: TColumnType;
begin
  if ReadOnly or not Assigned(FOnEdit) then Exit;
  if InplaceEdit.HandleChar(AKey, SavedShift) then Exit;
  ARawLength := RawData[FCaretPosData.RowIndex].RawLength;
  if ARawLength = 0 then Exit;
  Painter := GetRowPainter(FCaretPosData.RowIndex);
  if Painter = nil then Exit;
  if Painter.CaretEditMode(FCaretPosData.Column) <> cemSingleChar then Exit;

  ACursor.Column := FCaretPosData.Column;
  ACursor.RowIndex := FCaretPosData.RowIndex;
  ACursor.CharIndex := FCaretPosData.CharIndex;
  ACursor.ValueOffset := Painter.CharIndexToValueOffset(ACursor.Column, ACursor.CharIndex);
  if Painter.FormatMode.Inverted then
    ACursor.ValueOffset := Pred(ARawLength) - ACursor.ValueOffset;
  ACursor.AddrVA := RawData[FCaretPosData.RowIndex].Address + ACursor.ValueOffset;

  ValueMetric := Painter.TextMetric.ValueMetric;

  Handled := False;
  CharLen := 0;
  AEditorMode := FCaretPosData.Column;
  if ByteViewMode = bvmText then
    AEditorMode := ctDescription;
  case AEditorMode of
    ctOpcode:
    begin

      if Length(AKey) > 1 then Exit;
      HexChar := AKey{$IFDEF FPC}[1]{$ENDIF};
      if not CharInSet(HexChar, ['0'..'9', 'a'..'f', 'A'..'F']) then Exit;

      // чтобы не учитывать BE/LE просто работаем со строкой
      // а RTL возьмет на себя задачу преобразований

      // To ignore BE/LE, just work with the string and RTL
      // will take care of the conversion task

      StrData := Painter.ColumnAsString(FCaretPosData.Column);
      if FCaretPosData.Column = ctOpcode then
        CaretIndex := FCaretPosData.CharIndex mod ValueMetric.CharCount
      else
        CaretIndex := FCaretPosData.CharIndex mod ValueMetric.ByteCount;
      ValueStartIndex := FCaretPosData.CharIndex - CaretIndex + 1;
      ValueData := Copy(StrData, ValueStartIndex, ValueMetric.CharCount);
      Inc(CaretIndex);

      AData.ValueSize := ValueMetric.ByteCount;
      if not TryStrToInt64('0x' + ValueData, AData.OldValue) then Exit;
      if ValueData[CaretIndex] = HexChar then
      begin
        Handled := True;
        AData.NewValue := AData.OldValue;
      end
      else
      begin
        ValueData[CaretIndex] := HexChar;
        if not TryStrToInt64('0x' + ValueData, AData.NewValue) then Exit;
      end;
    end;
    ctDescription:
    begin
      GetRawBuff(FCaretPosData.RowIndex, Data);
      NewCharBytes := Encoder.EncodeChar(AKey);
      CharLen := Length(NewCharBytes);
      CaretIndex := FCaretPosData.CharIndex mod CharLen;
      AData.OldValue := 0;
      AData.NewValue := 0;
      ValueStartIndex := FCaretPosData.CharIndex - CaretIndex;

      // если позиция символа расположена в начале значения,
      // просто копируем новый символ

      // if the character position is at the beginning of the value,
      // simply copy the new character

      if ValueStartIndex = ACursor.ValueOffset then
      begin
        Move(NewCharBytes[0], AData.NewValue, CharLen);
        AData.ValueSize := CharLen;
        Move(Data[ACursor.ValueOffset], AData.OldValue, AData.ValueSize);
      end
      else
      begin

        // в противном случае нужно разместить символ по нужному оффсету в значении

        // otherwise it is necessary to place the symbol
        // on the required offset in the value of

        while ValueStartIndex < ACursor.ValueOffset do
        begin
          Dec(ACursor.ValueOffset, ValueMetric.ByteCount);
          Dec(ACursor.AddrVA, ValueMetric.ByteCount);
        end;
        AData.ValueSize := ValueStartIndex + CharLen - ACursor.ValueOffset;
        Move(Data[ACursor.ValueOffset], AData.OldValue, AData.ValueSize);
        Move(NewCharBytes[0], Data[ValueStartIndex], CharLen);
        Move(Data[ACursor.ValueOffset], AData.NewValue, AData.ValueSize);
      end;

      Inc(CaretIndex);
      Handled := AData.OldValue = AData.NewValue;
    end;
  else
    Exit;
  end;

  SetLength(AData.OldExtValue, AData.ValueSize);
  Move(AData.OldValue, AData.OldExtValue[0], AData.ValueSize);
  SetLength(AData.NewExtValue, AData.ValueSize);
  Move(AData.NewValue, AData.NewExtValue[0], AData.ValueSize);

  DoOnEdit(ACursor, AData, Handled);

  if Handled then
  begin
    OldRowIndex := FCaretPosData.RowIndex;
    DoCaretRight(FSavedShift);
    if FCaretPosData.Column = ctDescription then
      while CharLen > CaretIndex do
      begin
        DoCaretRight(FSavedShift);
        Inc(CaretIndex);
      end;

    // При переходе к новой строке режим выделения будет автоматически изменен
    // при вызове GetCaretChangeMode. Поэтому сбрасывать выделение необходимо
    // только в случае если тип редактора в новой строке не изменился.

    // The selection mode will be automatically changed when GetCaretChangeMode
    // is called when you move to a new line. Therefore it is necessary to reset
    // the selection only if the editor type in the new line has not changed.

    if OldRowIndex <> FCaretPosData.RowIndex then
    begin
      Painter := GetRowPainter(FCaretPosData.RowIndex);
      if Painter = nil then Exit;
      if Painter.CaretEditMode(FCaretPosData.Column) <> cemSingleChar then Exit;
    end;
    ClearSelection(False);
  end;
end;

procedure TFWCustomHexView.DoCaretEnd(Shift: TShiftState);
var
  NewRowIndex, NewCharIndex: Integer;
  Painter: TAbstractPrimaryRowPainter;
begin
  NewRowIndex := GetCaretPreviosRowIndex(RawData.Count, FCaretPosData.Column);
  if NewRowIndex >= RawData.Count then Exit;
  Painter := GetRowPainter(NewRowIndex);
  if Painter = nil then Exit;
  NewCharIndex := Painter.CharCount(FCaretPosData.Column) - 1;
  UpdateCaretPosData(
    SelectPoint(NewRowIndex, NewCharIndex, FCaretPosData.Column),
    GetCaretChangeMode(Painter, FCaretPosData.Column, Shift));
end;

procedure TFWCustomHexView.DoCaretKeyDown(var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  // Базовый редактор не знает как обрабатывать ShortCuts.JmpTo,
  // эта задача должна быть решена в наследниках или во внешнем обработчике события.

  // The base editor does not know how to handle ShortCuts.JmpTo,
  // this task should be solved in the successors or in an external event handler.

  if ShortCuts.JmpTo.IsShortCut(Key, Shift) then
  begin
    Handled := False;
    DoJmpTo(SelectPointToAddress(LeftSelPoint), jsQueryJump, Handled);
    Exit;
  end;

  if ShortCuts.JmpBack.IsShortCut(Key, Shift) then
    JumpUndo;
end;

procedure TFWCustomHexView.DoCaretHome(Shift: TShiftState);
var
  NewRowIndex, NewCharIndex: Integer;
begin
  NewRowIndex := GetCaretNextRowIndex(-1, FCaretPosData.Column);
  if NewRowIndex < 0 then Exit;
  NewCharIndex := 0;
  UpdateCaretPosData(
    SelectPoint(NewRowIndex, NewCharIndex, FCaretPosData.Column),
    GetCaretChangeMode(GetRowPainter(NewRowIndex), FCaretPosData.Column, Shift));
end;

procedure TFWCustomHexView.DoCaretLeft(Shift: TShiftState);
var
  NewRowIndex, NewCharIndex: Integer;
  Painter: TAbstractPrimaryRowPainter;
begin
  NewRowIndex := FCaretPosData.RowIndex;
  Painter := GetRowPainter(NewRowIndex);
  if Painter = nil then Exit;
  NewCharIndex := FCaretPosData.CharIndex -
    Painter.CaretKeyIncrement(FCaretPosData.Column);
  if NewCharIndex < 0 then
  begin
    NewRowIndex := GetCaretPreviosRowIndex(FCaretPosData.RowIndex);
    if NewRowIndex < 0 then Exit;
    Painter := GetRowPainter(NewRowIndex);
    if Painter = nil then Exit;
    NewCharIndex := Painter.CharCount(FCaretPosData.Column) -
      Painter.CaretKeyIncrement(FCaretPosData.Column);
  end;
  UpdateCaretPosData(
    SelectPoint(NewRowIndex, NewCharIndex, FCaretPosData.Column),
    GetCaretChangeMode(Painter, FCaretPosData.Column, Shift));
end;

procedure TFWCustomHexView.DoCaretPageDown(Shift: TShiftState);
begin
  DoCaretDown(Shift, VisibleRowCount - 1);
end;

procedure TFWCustomHexView.DoCaretPageUp(Shift: TShiftState);
begin
  DoCaretUp(Shift, VisibleRowCount - 1);
end;

procedure TFWCustomHexView.DoCaretRight(Shift: TShiftState);
var
  NewRowIndex, NewCharIndex, MaxCharIndex: Integer;
  Painter: TAbstractPrimaryRowPainter;
begin
  NewRowIndex := FCaretPosData.RowIndex;
  Painter := GetRowPainter(FCaretPosData.RowIndex);
  if Painter = nil then Exit;
  NewCharIndex := FCaretPosData.CharIndex +
    Painter.CaretKeyIncrement(FCaretPosData.Column);
  MaxCharIndex := Painter.CharCount(FCaretPosData.Column);
  if NewCharIndex >= MaxCharIndex then
  begin
    NewRowIndex := GetCaretNextRowIndex(FCaretPosData.RowIndex);
    if NewRowIndex >= RawData.Count then Exit;
    Painter := GetRowPainter(NewRowIndex);
    if Painter = nil then Exit;
    NewCharIndex := Min(0, Painter.CharCount(FCaretPosData.Column) - 1);
  end;
  UpdateCaretPosData(
    SelectPoint(NewRowIndex, NewCharIndex, FCaretPosData.Column),
    GetCaretChangeMode(Painter, FCaretPosData.Column, Shift));
end;

procedure TFWCustomHexView.DoCaretUp(Shift: TShiftState; ForcedLine: Integer);
var
  NewRowIndex, NewCharIndex, MaxCharIndex: Integer;
  Painter: TAbstractPrimaryRowPainter;
begin
  NewRowIndex := GetCaretPreviosRowIndex(FCaretPosData.RowIndex - ForcedLine);
  if NewRowIndex < 0 then
  begin
    DoCaretHome(Shift);
    Exit;
  end;
  NewCharIndex := FCaretPosData.CharIndex;
  Painter := GetRowPainter(NewRowIndex);
  if Painter = nil then Exit;
  MaxCharIndex := Painter.CharCount(FCaretPosData.Column);
  if NewCharIndex < 0 then
    NewCharIndex := 0;
  if NewCharIndex >= MaxCharIndex then
    NewCharIndex := MaxCharIndex - 1;
  UpdateCaretPosData(
    SelectPoint(NewRowIndex, NewCharIndex, FCaretPosData.Column),
    GetCaretChangeMode(Painter, FCaretPosData.Column, Shift));
end;

procedure TFWCustomHexView.DoChange(ChangeCode: Integer);
begin
  if (csCreating in ControlState) then Exit;
  if Parent = nil then Exit;
  if FUpdateCount = 0 then
  begin
    case ChangeCode of
      cmHeader:
      begin
        UpdateTextExtent;
        UpdateTextBoundary;
        UpdateScrollPos;
      end;
      cmAddressMode, cmAddressView:
      begin
        FitColumnToBestSize(ctAddress);
        Invalidate;
      end;
      cmColorMap:
      begin
        UpdateTextDarknessColor;
        Invalidate;
      end;
      cmBookmark, cmEncoding:
        Invalidate;
      cmByteViewMode:
        UpdateView;
      cmScrollX, cmScrollY:
        Invalidate;
      cmDiapasoneChange:
      begin
        FSelections.UpdateVisibleSelections;
        Invalidate;
      end
    else
      UpdateTextExtent;
      UpdateTextMetrics;
      UpdateTextBoundary;
      if not ChangeCode in [cmHeader..cmFont] then
        UpdateCaretPosData(FSelStart, ccmNone);
      UpdateScrollPos;
    end;
  end;
end;

function TFWCustomHexView.DoDrawRowColumnBackground(ACanvas: TCanvas;
  APainter: TAbstractPrimaryRowPainter; AColumn: TColumnType;
  const ARect: TRect): Boolean;
var
  AParam: TDrawParam;
begin
  Result := False;
  if Assigned(FOnDrawColBack) then
  begin
    AParam.Column := AColumn;
    AParam.RowIndex := APainter.RowIndex;
    AParam.AddrVA := APainter.RowToAddress(AParam.RowIndex, 0);
    AParam.CharIndex := 0;
    AParam.ValueOffset := 0;
    FOnDrawColBack(Self, ACanvas, AParam, ARect, Result);
  end;
end;

procedure TFWCustomHexView.DoDrawToken(ACanvas: TCanvas;
  ATokenParam: TDrawParam; const ARect: TRect;
  AToken: PChar; var ATokenLen: Integer);
begin
  if Assigned(FOnDrawToken) then
    FOnDrawToken(Self, ACanvas, ATokenParam, ARect, AToken, ATokenLen);
end;

procedure TFWCustomHexView.DoEditContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  if Assigned(FOnEditContextPopup) then
    FOnEditContextPopup(Self, MousePos, Handled);
end;

procedure TFWCustomHexView.DoEncodingChange;
begin
  DoChange(cmEncoding);
end;

procedure TFWCustomHexView.DoFontChange(Sender: TObject);
var
  PresiosTopRow: Integer;
begin
  if csLoading in ComponentState then Exit;
  if Parent = nil then Exit;
  PresiosTopRow := CurrentVisibleRow;
  if Assigned(FOldOnFontChange) then
    FOldOnFontChange(Sender);
  DoChange(cmFont);
  SetTopRow(PresiosTopRow);
end;

procedure TFWCustomHexView.DoGetHint(var AHintParam: THintParam;
  var AHint: string);
begin
  if Assigned(FOnGetHint) then
    FOnGetHint(Self, AHintParam, AHint);
end;

function TFWCustomHexView.DoInplaceEditHandleKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := InplaceEdit.Visible;
  if Result then
  begin
    if Key = VK_TAB then
    begin
      if ssShift in Shift then
        DoCaretLeft([])
      else
        DoCaretRight([]);
      EditAtCaretPos;
    end
    else
      InplaceEdit.HandleKeyDown(Key, Shift);
    {$IFDEF LINUX}
    // Special processing under Lazarus to block focus transfer
    if Key in [VK_LEFT, VK_RIGHT, VK_DOWN, VK_UP, VK_TAB] then
      Key := VK_UNKNOWN;
    {$ENDIF}
  end;
end;

procedure TFWCustomHexView.DoInvalidateRange(AStartRow, AEndRow: Int64);
var
  R: TRect;
  LeftOffset: Integer;
begin
  LeftOffset := GetLeftNCWidth;
  R := Rect(LeftOffset,
    AStartRow * FRowHeight + FScrollOffset.Y,
    ClientWidth, AEndRow * FRowHeight + FScrollOffset.Y);
  if Header.Visible then
    OffsetRect(R, 0, FRowHeight);
  InvalidateRect(Handle, @R, False);
end;

procedure TFWCustomHexView.DoJmpTo(AAddrVA: Int64; AJmpState: TJmpState;
  var Handled: Boolean);
begin
  if Assigned(FOnJmpTo) then
    FOnJmpTo(Self, AAddrVA, AJmpState, Handled);
end;

function TFWCustomHexView.DoLButtonDown(const AHitInfo: TMouseHitInfo): Boolean;
begin
  Result := InplaceEdit.HandleMouseDown(mbLeft, AHitInfo.Shift,
    AHitInfo.CursorPos.X, AHitInfo.CursorPos.Y);
  Result := Result or (ssDouble in AHitInfo.Shift);
end;

function TFWCustomHexView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  try
    if ssCtrl in Shift then
    begin
      Zoom(IfThen(WheelDelta > 0, 1, -1));
      Exit;
    end;
    if not (ScrollBars in [TScrollStyle.ssBoth, TScrollStyle.ssVertical]) then Exit;
    UpdateScrollY(FScrollOffset.Y + (FRowHeight * WheelRowCount) * Sign(WheelDelta));
  finally
    MousePos := ScreenToClient(MousePos);
    MouseMove(Shift, MousePos.X, MousePos.Y);
  end;
end;

procedure TFWCustomHexView.DoOnEdit(const ACursor: TDrawParam;
  const AData: TEditParam; var Handled: Boolean);
begin
  if Assigned(FOnEdit) then
    FOnEdit(Self, ACursor, AData, Handled);
end;

procedure TFWCustomHexView.DoQueryString(AddrVA: Int64; AColumn: TColumnType;
  var AComment: string);
begin
  if Assigned(FQueryStringEvent) then
    FQueryStringEvent(Self, AddrVA, AColumn, AComment);
end;

procedure TFWCustomHexView.DoSelectionChage(AStartAddr, AEndAddr: Int64);
begin
  if Assigned(FSelectionChange) then
    FSelectionChange(Self);
end;

procedure TFWCustomHexView.DrawEditMark(var Offset: TPoint);
var
  DataChar: string;
  pData: PChar;
  R: TRect;
  Painter: TAbstractPrimaryRowPainter;
begin
  if FCaretPosData.EditMode <> cemSingleChar then Exit;
  if not RowVisible(FCaretPosData.RowIndex) then Exit;
  Painter := GetRowPainter(FCaretPosData.RowIndex);
  if Painter = nil then Exit;
  if not Painter.CalcEditParam(FCaretPosData, Offset, DataChar) then Exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := ColorMap.CaretColor;
  Canvas.Font.Color := ColorMap.CaretTextColor;
  R := Rect(Offset.X, Offset.Y,
    Offset.X + FCharWidth, Offset.Y + RowHeight);
  pData := @DataChar[1];
  ExtTextOut(Canvas, R.Left, R.Top, ETO_CLIPPED, @R, pData,
    UTF8ByteCount(pData, 1), nil);
end;

procedure TFWCustomHexView.DrawRows(StartRow, EndRow: Int64;
  var Offset: TPoint);
var
  I: Int64;
  Painter: TAbstractPrimaryRowPainter;
  {$ifdef show_dbg_lines}
  R: TRect;
  {$endif}
begin
  if FRawData.Count = 0 then Exit;
  I := StartRow;
  while I <= EndRow do
  begin
    Painter := GetRowPainter(I, False);
    if Assigned(Painter) then
      Painter.DrawRow(Canvas, Offset)
    else
      Inc(Offset.Y, FRowHeight);
    if FRawData[I].DrawRowSmallSeparator then
      DrawRowSmallSeparator(Offset);

    {$ifdef show_dbg_lines}
    R := Bounds(0, Offset.Y - FRowHeight, FHeader.ColumnWidth[ctWorkSpace], 20);
    DrawText(Canvas, IntToStr(I), -1, R, DT_CENTER);
    {$endif}

    Inc(I);
  end;
end;

procedure TFWCustomHexView.DrawRowSmallSeparator(var Offset: TPoint);
begin
  ResetCanvas;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := ColorMap.RowSeparatorColor;
  Canvas.Pen.Style := psDot;
  Canvas.MoveTo(GetLeftNCWidth + FScrollOffset.X, Offset.Y - 1);
  Canvas.LineTo(FHeader.Width, Offset.Y - 1);
  Canvas.Pen.Style := psSolid;
end;

function TFWCustomHexView.EditAtCaretPos: Boolean;
var
  Painter: TAbstractPrimaryRowPainter;
begin
  Result := False;
  InplaceEdit.Hide;
  if ReadOnly then Exit;
  if CaretPosData.RowIndex < 0 then Exit;
  if CaretPosData.Column <> InplaceEdit.EditColumn then Exit;
  Painter := GetRowPainter(CaretPosData.RowIndex, False);
  if Painter = nil then Exit;
  // Для режимов bvmHex8 и bvmText используется штатный режим редактирования, не требующий контроля ввода.
  // The bvmHex8 and bvmText modes use the normal editing mode, which does not require input control.
  if Painter.ByteViewMode in [bvmHex8, bvmText] then Exit(False);
  if CaretPosData.EditMode <> cemValueEditor then Exit;
  if CaretPosData.CharIndex < 0 then Exit;
  InplaceEdit.Show(Painter);
  Result := True;
end;

procedure TFWCustomHexView.EndUpdate;
begin
  Dec(FUpdateCount);
  RebuildData;
end;

procedure TFWCustomHexView.FitColumnsToBestSize;
var
  I: TColumnType;
begin
  if not HandleAllocated then
  begin
    FNeedFitBesSizes := True;
    Exit;
  end;
  for I := ctWorkSpace to High(TColumnType) do
    if I in Header.Columns then
      FitColumnToBestSize(I);
end;

function TFWCustomHexView.Focused: Boolean;
begin
  if FProcessMenu then
    Result := True
  else
    Result := inherited Focused;
end;

procedure TFWCustomHexView.FitColumnToBestSize(Value: TColumnType);
begin
  Header.ColumnWidth[Value] := CalculateColumnBestSize(Value);
end;

procedure TFWCustomHexView.FocusOnAddress(Address: Int64;
  ACaretChangeMode: TCaretChangeMode);
var
  SelectPoint: TSelectPoint;
begin
  SelectPoint := AddressToSelectPoint(Address);
  if SelectPoint.RowIndex < 0 then Exit;

  // делаем чтобы строка была самой первой сверху

  // make the row the first one from the top.

  if not RowVisible(SelectPoint.RowIndex) then
  begin
    if SelectPoint.RowIndex > CurrentVisibleRow then
      FocusOnRow(SelectPoint.RowIndex + (ClientHeight div FRowHeight) - 2, ccmNone)
    else
      FocusOnRow(SelectPoint.RowIndex, ccmNone);
  end;

  // ну и выделяем ее, дабы в глаза бросалась

  // and highlight it to make it stand out

  UpdateCaretPosData(SelectPoint, ACaretChangeMode);

  // снимаем реакцию с мышки, чтобы не было артефактов
  // иначе она перейдет в режим выделения

  // deselect the mouse so that there are no artifacts
  // or it will go into selection mode.

  FMousePressed := False;
end;

procedure TFWCustomHexView.FocusOnRow(ARowIndex: Int64;
  ACaretChangeMode: TCaretChangeMode);
var
  Diapason: TVisibleRowDiapason;
  NewVerticalOffset: Int64;
begin
  if RowVisible(ARowIndex) then
  begin
    UpdateCaretPosData(SelectPoint(ARowIndex, 0, ctOpcode), ACaretChangeMode);
    Exit;
  end;
  Diapason := VisibleRowDiapason;
  if ARowIndex < Diapason.StartRow then
    NewVerticalOffset := ARowIndex * FRowHeight
  else
    NewVerticalOffset := (ARowIndex - (Diapason.EndRow - Diapason.StartRow)) * FRowHeight;
  UpdateScrollY(-NewVerticalOffset);
  UpdateCaretPosData(SelectPoint(ARowIndex, 0, ctOpcode), ACaretChangeMode);
end;

procedure TFWCustomHexView.JumpClear;
begin
  FJumpStack.Clear;
end;

function TFWCustomHexView.GetBookMark(AIndex: TBookMark): Int64;
begin
  Result := FBookMarks[AIndex];
end;

function TFWCustomHexView.GetCaretChangeMode(
  APainter: TAbstractPrimaryRowPainter; AColumn: TColumnType;
  Shift: TShiftState): TCaretChangeMode;
begin
  Result := ccmNone;
  if ssShift in Shift then Exit(ccmContinueSelection);
  if ssDouble in Shift then Exit(ccmSelectRow);
  if APainter = nil then Exit;
  if ReadOnly then Exit(ccmSetNewSelection);
  case APainter.CaretEditMode(AColumn) of
    cemDisabled: Result := ccmReset;
    cemSingleChar:
    begin

      // FCaretPosData по-прежнему содержит информацию по фактическом пайнтеру,
      // поэтому здесь можно отследить переход между различными стилями редактирования

      // FCaretPosData still contains information about the actual painter,
      // so you can track the transition between different editing styles here

      if FCaretPosData.EditMode = cemSingleChar then
        Result := ccmNone
      else
        Result := ccmSetNewSelection;
    end;
  else
    Result := ccmSetNewSelection;
  end;
end;

function TFWCustomHexView.GetCaretNextRowIndex(FromIndex: Int64;
  AColumn: TColumnType): Int64;
var
  Painter: TAbstractPrimaryRowPainter;
begin
  Result := FromIndex + 1;
  if AColumn = ctNone then
    AColumn := FCaretPosData.Column;

  // если результирующая строка не принимает выделение, значит ищем следующую

  // if the resulting string does not accept selection, then look for the next one

  Painter := GetRowPainter(Result);
  while (Result < RawData.Count) and ((Painter = nil) or not Painter.AcceptSelection) do
  begin
    Inc(Result);
    Painter := GetRowPainter(Result);
  end;

  // проверка необходимости поиска следующей редактируемой строки

  // check if the next line to be edited should be searched

  if ReadOnly or AcceptKeySelectionAllways then Exit;

  // причем проверку будем делать по признаку что текущая строка редактируемая

  // and we will check if the current line is editable

  Painter := GetRowPainter(FCaretPosData.RowIndex);
  if (Painter = nil) or not Painter.AcceptEdit(AColumn) then Exit;

  // если текущая строка редактируемая, значит ищем следующую редактируемую

  // if the current line is editable, then look for the next editable line

  Painter := GetRowPainter(Result);
  while (Result < RawData.Count) and ((Painter = nil) or not Painter.AcceptEdit(AColumn)) do
  begin
    Inc(Result);
    Painter := GetRowPainter(Result);
  end;
end;

function TFWCustomHexView.GetCaretPreviosRowIndex(FromIndex: Int64;
  AColumn: TColumnType): Int64;
var
  Painter: TAbstractPrimaryRowPainter;
begin
  Result := FromIndex - 1;
  if AColumn = ctNone then
    AColumn := FCaretPosData.Column;

  // если результирующая строка не принимает выделение, значит ищем предыдущую

  // if the resulting string does not accept selection, then we search for the previous string

  Painter := GetRowPainter(Result);
  while (Result >= 0) and ((Painter = nil) or not Painter.AcceptSelection) do
  begin
    Dec(Result);
    Painter := GetRowPainter(Result);
  end;

  // проверка необходимости поиска предыдущей редактируемой строки

  // check if the previous edited line should be searched

  if ReadOnly or AcceptKeySelectionAllways then Exit;

  // причем проверку будем делать по признаку что текущая строка редактируемая

  // and we will check if the current line is editable

  Painter := GetRowPainter(FCaretPosData.RowIndex);
  if (Painter = nil) or not Painter.AcceptEdit(AColumn) then Exit;

  // если текущая строка редактируемая, значит ищем предыдущую редактируемую

  // if the current line is editable, then search for the previous editable line

  Painter := GetRowPainter(Result);
  while (Result >= 0) and ((Painter = nil) or not Painter.AcceptEdit(AColumn)) do
  begin
    Dec(Result);
    Painter := GetRowPainter(Result);
  end;
end;

function TFWCustomHexView.GetSelectedRawBuff(ASelStartRow, ASelEndRow: Int64): TBytes;
var
  RawBuff, Data: TBytes;
  CurPos, Len: Integer;
  I: Int64;
  SelData: TSelectData;

  procedure CheckCapacity(NeedSize: Integer);
  begin
    if CurPos + NeedSize > Length(RawBuff) then
      SetLength(RawBuff, CurPos + NeedSize + 16);
  end;

begin
  CurPos := 0;
  I := ASelStartRow;
  while I <= ASelEndRow do
  begin
    GetRawBuff(I, Data);
    if Length(Data) = 0 then
    begin
      Inc(I);
      Continue;
    end;
    SelData := GetSelectData(I);
    case SelData.SelectStyle of
      ssAllSelected:
      begin
        Len := Length(Data);
        CheckCapacity(Len);
        Move(Data[0], RawBuff[CurPos], Len);
        Inc(CurPos, Len);
      end;
      ssLeftSelected:
      begin
        Len := SelData.FirstSelectIndex + 1;
        CheckCapacity(Len);
        Move(Data[0], RawBuff[CurPos], Len);
        Inc(CurPos, Len);
      end;
      ssCenterSelected:
      begin
        Len := SelData.SecondSelectIndex - SelData.FirstSelectIndex + 1;
        CheckCapacity(Len);
        Move(Data[SelData.FirstSelectIndex], RawBuff[CurPos], Len);
        Inc(CurPos, Len);
      end;
      ssRightSelected:
      begin
        Len := Length(Data) - SelData.FirstSelectIndex;
        CheckCapacity(Len);
        Move(Data[SelData.FirstSelectIndex], RawBuff[CurPos], Len);
        Inc(CurPos, Len);
      end;
    end;
    Inc(I);
  end;

  Result := RawBuff;
  SetLength(Result, CurPos);
end;

function TFWCustomHexView.GetSelectedRow(out ASelStartRow, ASelEndRow: Int64): Boolean;
begin
  Result := False;
  if FSelStart.InvalidRow then Exit;
  if FSelEnd.InvalidRow then Exit;
  if FSelStart.RowIndex > FSelEnd.RowIndex then
  begin
    ASelStartRow := FSelEnd.RowIndex;
    ASelEndRow := FSelStart.RowIndex;
  end
  else
  begin
    ASelStartRow := FSelStart.RowIndex;
    ASelEndRow := FSelEnd.RowIndex;
  end;
  Result := True;
end;

function TFWCustomHexView.IgnoreSelectionWhenCopyAddress: Boolean;
begin
  Result := False;
end;

function TFWCustomHexView.GetColorMapClass: THexViewColorMapClass;
begin
  Result := THexViewColorMap;
end;

function TFWCustomHexView.GetColumnRect(AColumnType: TColumnType;
  ARowIndex: Int64): TRect;
var
  Painter: TAbstractPrimaryRowPainter;
begin
  Painter := GetRowPainter(ARowIndex, True);
  if Painter <> nil then
    Result := Painter.ColumnRect(GetRowOffsetPoint(ARowIndex), AColumnType)
  else
    Result := TRect.Empty;
end;

function TFWCustomHexView.GetDataStreamSize: Int64;
begin
  if Assigned(FDataStream) then
    Result := CheckNegative(FDataStream.Size)
  else
    Result := 0;
end;

function TFWCustomHexView.GetDefaultFontHeight: Integer;
begin
  Result := -12;
end;

function TFWCustomHexView.GetDefaultFontName: string;
begin
  {$IFDEF UNIX}
  Result := 'DejaVu Sans Mono'; // 'Monospace';
  {$ELSE}
  Result := 'Consolas'; //'Lucida Console';
  {$ENDIF}
end;

function TFWCustomHexView.GetDefaultPainterClass: TPrimaryRowPainterClass;
begin
  Result := TRowHexPainter;
end;

function TFWCustomHexView.GetHeaderClass: THeaderClass;
begin
  Result := THexViewHeader;
end;

function TFWCustomHexView.GetHitInfo(XPos, YPos: Int64; AShift: TShiftState): TMouseHitInfo;
var
  I: TColumnType;
  LeftOffset: Integer;
  Painter: TAbstractPrimaryRowPainter;
begin
  Result.Erase;
  Result.CursorPos := Point(XPos, YPos);
  Result.Shift := AShift;

  if InplaceEdit.Visible and PtInRect(InplaceEdit.EditRect, Result.CursorPos) then
    Include(Result.Elements, keInplaceEdit);

  Dec(XPos, FScrollOffset.X);

  LeftOffset := 0;
  for I := ctWorkSpace to High(TColumnType) do
  begin
    if not (I in FHeader.Columns) then Continue;
    Inc(LeftOffset, FHeader.ColumnWidth[I]);
    if LeftOffset + SplitMargin >= XPos then
    begin
      Result.SelectPoint.Column := I;
      Result.ColumnWidth := FHeader.ColumnWidth[I];

      if Header.DrawColumnSeparator then
      begin
        if LeftOffset - SplitMargin < XPos then
          Include(Result.Elements, keSplitter);

        // если правая граница колонки за экраном,
        // позволяем быстро изменить ее размеры без скролирования по горизонтали

        // if the right border of a column is off the screen,
        // we allow you to quickly resize it without scrolling horizontally

        if not (keSplitter in Result.Elements) and (XPos + FScrollOffset.X >= ClientWidth - DblSize(SplitMargin)) then
        begin
          Result.ColumnWidth := FHeader.ColumnWidth[I] - (LeftOffset - XPos - FScrollOffset.X);
          if Result.ColumnWidth >= MinColumnWidth then
            Include(Result.Elements, keSplitter);
        end;
      end;

      Break;
    end;
  end;

  if Header.Visible then
  begin
    if (YPos >= 0) and (YPos <= FRowHeight) then
      Include(Result.Elements, keHeader);
    Dec(YPos, FRowHeight);
  end;
  if keHeader in Result.Elements then Exit;
  Dec(YPos, FScrollOffset.Y);

  Result.SelectPoint.RowIndex := YPos div FRowHeight;
  if Result.SelectPoint.RowIndex >= FRawData.Count then
    Result.SelectPoint.RowIndex := -1;

  Result.ScrolledCursorPos := Point(XPos, YPos);

  if keSplitter in Result.Elements then Exit;

  if Result.SelectPoint.RowIndex < 0 then Exit;

  Dec(LeftOffset, Result.ColumnWidth);
  Result.ColumnStart := LeftOffset;

  Painter := GetRowPainter(Result.SelectPoint.RowIndex);
  if Assigned(Painter) then
    Painter.GetHitInfo(Result);

  if SelectPointInSelection(Result.SelectPoint) then
    Include(Result.Elements, keSelection);
end;

function TFWCustomHexView.GetInplaceEditClass: THexViewInplaceEditClass;
begin
  Result := THexViewInplaceEdit;
end;

function TFWCustomHexView.GetLeftNCWidth: Integer;
begin
  Result := DefaultPainter.CalcLeftNCWidth;
end;

function TFWCustomHexView.GetOverloadPainterClass(
  Value: TPrimaryRowPainterClass): TPrimaryRowPainterClass;
begin
  Result := Value;
end;

function TFWCustomHexView.GetPageHeight: Integer;
begin
  if FRowHeight = 0 then
    Result := ClientHeight
  else
    Result := ClientHeight - (ClientHeight mod FRowHeight) - FRowHeight;
  if Header.Visible then
    Dec(Result, FRowHeight);
end;

procedure TFWCustomHexView.GetRawBuff(ARowIndex: Int64; var Data: TBytes);
begin
  if FDataStream = nil then
    raise Exception.Create('DataStream = nil');
  SetLength(Data, FRawData[ARowIndex].RawLength);
  if Length(Data) = 0 then Exit;
  FDataStream.Position := FRawData[ARowIndex].DataOffset;
  FDataStream.ReadBuffer(Data[0], Length(Data));
end;

function TFWCustomHexView.GetRawDataClass: TRawDataClass;
begin
  Result := TRawData;
end;

function TFWCustomHexView.GetRowOffset(ARowIndex: Int64): Int64;
begin
  Result := FScrollOffset.Y + ARowIndex * FRowHeight;
  if Header.Visible then
    Inc(Result, FRowHeight);
end;

function TFWCustomHexView.GetRowOffsetPoint(ARowIndex: Int64): TPoint;
begin
  Result.X := FScrollOffset.X;
  Result.Y := ARowIndex * FRowHeight + FScrollOffset.Y;
  if Header.Visible then
    Inc(Result.Y, FRowHeight);
end;

function TFWCustomHexView.GetRowPainter(ARowIndex: Int64;
  TempPainter: Boolean): TAbstractPrimaryRowPainter;
begin
  Result := InternalGetRowPainter(ARowIndex);
  if Assigned(Result) and not TempPainter then
  begin
    if (ARowIndex < 0) or (ARowIndex >= RawData.Count) then
      Result := nil
    else
      Result.RowIndex := ARowIndex;
  end;
end;

function TFWCustomHexView.GetSelectData(ARowIndex: Int64): TSelectData;
begin
  Result := GetSelectDataWithSelection(ARowIndex, FSelStart, FSelEnd);
end;

function TFWCustomHexView.GetSelectDataWithSelection(ARowIndex: Int64; ASelStart,
  ASelEnd: TSelectPoint): TSelectData;
var
  SelStartIdx, SelEndIdx: Int64;
  MaxPosInRow: Integer;
  LeftSel, RightSel: TSelectPoint;
begin
  Result := Default(TSelectData);

  if ASelStart.InvalidRow then Exit;
  if ASelEnd.InvalidRow then Exit;

  if ASelEnd < ASelStart then
  begin
    LeftSel := ASelEnd;
    RightSel := ASelStart;
  end
  else
  begin
    LeftSel := ASelStart;
    RightSel := ASelEnd;
  end;

  if LeftSel.RowIndex > ARowIndex then Exit;
  if RightSel.RowIndex < ARowIndex then Exit;

  if LeftSel.RowIndex = ARowIndex then
    SelStartIdx := LeftSel.ValueOffset
  else
    SelStartIdx := 0;

  if SelStartIdx < 0 then
    SelStartIdx := 0;

  MaxPosInRow := FRawData[RightSel.RowIndex].RawLength;
  if MaxPosInRow > 0 then
    Dec(MaxPosInRow);

  if RightSel.RowIndex > ARowIndex then
    SelEndIdx := MaxPosInRow
  else
    SelEndIdx := RightSel.ValueOffset;

  if SelEndIdx < 0 then
    SelEndIdx := MaxPosInRow;

  if SelStartIdx = 0 then
  begin
    if SelEndIdx >= MaxPosInRow - (TextMetric.ValueMetric.ByteCount - 1) then
      Result.SelectStyle := ssAllSelected
    else
      Result.SelectStyle := ssLeftSelected;
  end
  else
    if SelEndIdx >= MaxPosInRow then
      Result.SelectStyle := ssRightSelected
    else
      Result.SelectStyle := ssCenterSelected;

  case Result.SelectStyle of
    ssLeftSelected:
      Result.FirstSelectIndex := SelEndIdx;
    ssCenterSelected:
    begin
      Result.FirstSelectIndex := SelStartIdx;
      Result.SecondSelectIndex := SelEndIdx;
    end;
    ssRightSelected:
      Result.FirstSelectIndex := SelStartIdx;
  end;
end;

function TFWCustomHexView.GetShortCutsClass: TViewShortCutsClass;
begin
  Result := TViewShortCuts;
end;

function TFWCustomHexView.GetTextExtent: TSize;
begin
  Result := MeasureCanvas.TextExtent(CHAR_STRING);
end;

procedure TFWCustomHexView.InitDefault;
begin
  ColorMap.ColorMode := cmAuto;
  UpdateTextDarknessColor;
  FHeader.InitDefault;
  FHeader.ColumnWidth[ctWorkSpace] := ToDpi(34);
  FHeader.ColumnWidth[ctJmpLine] := ToDpi(90);
  FHeader.ColumnWidth[ctAddress] := ToDpi(130);
  FHeader.ColumnWidth[ctOpcode] := ToDpi(355);
  FHeader.ColumnWidth[ctDescription] := ToDpi(250);
  FHeader.ColumnWidth[ctComment] := ToDpi(300);
  Font.Color := ColorMap.TextColor;
  {$IFDEF FPC}

  // Контрол может быть еще не пересчитан в текущий DPI но шрифт подхватит
  // скалинг от монитора, поэтому перед назначением высоты для начала
  // актуализируем PPI в котором будет назначен Height, потом все пересчитается
  // на вызове AutoAdjustLayout

  // The control may not be recalculated to the current DPI yet, but the font
  // will pick up scaling from the monitor, so before assigning the height,
  // first of all, actualize the PPI in which the Height will be assigned,
  // then everything will be recalculated on the AutoAdjustLayout call.

  Font.PixelsPerInch := FCurrentPPI;
  {$ENDIF}
  Font.Height := ToDpi(GetDefaultFontHeight);
  Font.Name := GetDefaultFontName;
end;

procedure TFWCustomHexView.InitPainters;
begin
  DefaultPainter := GetDefaultPainterClass.Create(Self);
  Painters.Add(DefaultPainter);
  PostPainters.Add(TBookmarkPostPainter.Create(Self));
end;

procedure TFWCustomHexView.InternalClear;
begin
  ClearSelection;
  Selections.Clear;
  FillChar(FScrollOffset, SizeOf(FScrollOffset), 0);
  FillChar(FTextBoundary, SizeOf(FTextBoundary), 0);
end;

procedure TFWCustomHexView.InternalKillCaretTimer;
begin
  FCaretTimerEnabled := False;
  KillTimer(Handle, 0);
end;

function TFWCustomHexView.InternalGetRowPainter(
  ARowIndex: Int64): TAbstractPrimaryRowPainter;
begin
  Result := DefaultPainter;
end;

procedure TFWCustomHexView.InvalidateCaretPosData(NewState: Boolean);
begin
  FCaretPosData.Showed := NewState;
  if InplaceEdit.Visible then
    InplaceEdit.Invalidate
  else
    InvalidateRow(FCaretPosData.RowIndex);
end;

procedure TFWCustomHexView.InvalidateRow(ARowIndex: Int64);
var
  R: TRect;
  LeftOffset: Integer;
  Offset: TPoint;
begin
  if not RowVisible(ARowIndex) then
  begin
    Invalidate;
    Exit;
  end;
  LeftOffset := GetLeftNCWidth;
  Offset := GetRowOffsetPoint(ARowIndex);
  R := Rect(Offset.X + LeftOffset, Offset.Y,
    ClientWidth, Offset.Y + FRowHeight);
  InvalidateRect(Handle, @R, False);
end;

procedure TFWCustomHexView.InvalidateSelections;
var
  TopRow, BottomRow: Int64;
  Diapason: TVisibleRowDiapason;
begin
  if not GetSelectedRow(TopRow, BottomRow) then Exit;
  Diapason := VisibleRowDiapason;
  if (TopRow > Diapason.EndRow) or
    (BottomRow < Diapason.StartRow) then Exit;
  TopRow := Max(TopRow, Diapason.StartRow);

  // +1 так как нужно отрисовать нижнюю строку целиком

  // +1 because we need to draw the entire bottom line.

  BottomRow := Min(Min(BottomRow, Diapason.EndRow) + 1, RawData.Count - 1);
  DoInvalidateRange(TopRow, BottomRow);
end;

function TFWCustomHexView.IsAddrVisible(AAddrVA: Int64): Boolean;
begin
  Result := IsRowVisible(RawData.AddressToRowIndex(AAddrVA));
end;

function TFWCustomHexView.IsColorMapStored: Boolean;
begin
  Result := FColorMap.IsColorStored;
end;

function TFWCustomHexView.LeftSelPoint: TSelectPoint;
begin
  if SelStart <= SelEnd then
    Result := FSelStart
  else
    Result := FSelEnd;
end;

function TFWCustomHexView.RightSelPoint: TSelectPoint;
begin
  if SelStart >= SelEnd then
    Result := FSelStart
  else
    Result := FSelEnd;
end;

function TFWCustomHexView.IsFontStored: Boolean;
begin
  Result :=
    (Font.Height <> ToDpi(GetDefaultFontHeight)) or
    (Font.Name <> GetDefaultFontName);
end;

function TFWCustomHexView.IsRowVisible(ARowIndex: Int64): Boolean;
var
  Diapason: TVisibleRowDiapason;
begin
  Result := False;
  if ARowIndex < 0 then Exit;
  Diapason := VisibleRowDiapason;
  Result := (ARowIndex >= Diapason.StartRow) and (ARowIndex <= Diapason.EndRow);
end;

procedure TFWCustomHexView.FillDrawMetrics(out AValue: TDrawMetrics);
begin
  AValue.CharWidth := CharWidth;
  AValue.MinColumnWidth := MinColumnWidth;
  AValue.RowHeight := RowHeight;
  AValue.SplitMargin := SplitMargin;
  AValue.TextMargin := TextMargin;
end;

function TFWCustomHexView.IsShortCutsStored: Boolean;
begin
  Result := ShortCuts.IsShortCutsStored;
end;

function TFWCustomHexView.JumpRedo: Boolean;
var
  Handled: Boolean;
begin
  Result := FJumpStack.Redo;
  Handled := False;
  DoJmpTo(FJumpStack.Current.JmpAddr, jsJmpRedo, Handled);
  if Handled then Exit(True);
  if Result then
  begin
    if FJumpStack.Current.SelLength > 0 then
    begin
      FocusOnAddress(FJumpStack.Current.JmpAddr, ccmSetNewSelection);
      UpdateSelectionAddr(FJumpStack.Current.JmpAddr,
        FJumpStack.Current.JmpAddr + FJumpStack.Current.SelLength - 1);
    end
    else
      FocusOnAddress(FJumpStack.Current.JmpAddr, GetDefaultCaretChangeMode);
    DoJmpTo(FJumpStack.Current.JmpAddr, jsJmpDone, Result);
  end;
end;

function TFWCustomHexView.JumpToAddress(AJmpAddr: Int64; ASelLength: Integer): Boolean;
var
  Handled: Boolean;
  JmpItem: TJmpItem;
begin
  if RawData.Count = 0 then Exit(False);

  Result := (FJumpStack.Current.JmpAddr <> AJmpAddr) or
    (FJumpStack.Current.SelLength <> ASelLength);

  if Result then
  begin
    //Вьювер снаружи может быть перестроен, поэтому сначала запоминаем его актуальное состояние.

    // The viewer outside can be rebuilt, so we first memorize its current state.

    JmpItem.JmpAddr := AJmpAddr;
    JmpItem.SelLength := ASelLength;
    JmpItem.JmpFrom := RawData[CurrentVisibleRow].Address;
    JmpItem.SelStart := FSelStart;
    JmpItem.SelEnd := FSelEnd;

    Handled := False;
    DoJmpTo(AJmpAddr, jsJmpPushToStack, Handled);
    if Handled then Exit;
    FJumpStack.Add(JmpItem);
  end
  else
  begin
    Handled := False;
    DoJmpTo(AJmpAddr, jsJmpPushToStack, Handled);
    if Handled then Exit;
    Result := FJumpStack.Redo;
  end;

  if ASelLength > 0 then
  begin
    FocusOnAddress(AJmpAddr, ccmSetNewSelection);
    UpdateSelectionAddr(AJmpAddr, AJmpAddr + ASelLength - 1);
  end
  else
    FocusOnAddress(AJmpAddr, GetDefaultCaretChangeMode);

  DoJmpTo(AJmpAddr, jsJmpDone, Handled);
end;

function TFWCustomHexView.JumpToBookmark(ABookmark: TBookMark): Boolean;
begin
  Result := JumpToAddress(Bookmark[ABookmark]);
end;

function TFWCustomHexView.JumpUndo: Boolean;
var
  Handled: Boolean;
begin
  Result := FJumpStack.Undo;
  Handled := False;
  DoJmpTo(FJumpStack.Current.JmpFrom, jsJmpUndo, Handled);
  if Handled then Exit(True);
  if Result then
  begin
    FocusOnAddress(FJumpStack.Current.JmpFrom, ccmNone);
    UpdateSelection(FJumpStack.Current.SelStart, FJumpStack.Current.SelEnd);

    // редактор уже восстановлен в то состояние, которое было перед прыжком,
    // поэтому адрес для последующей коррекции извне не передается

    // the editor is already restored to the state it was in before the jump,
    // so the address for further external correction is not transmitted

    DoJmpTo(-1, jsJmpDone, Handled);
  end;
end;

procedure TFWCustomHexView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  UpdateCaretTimer;
  UpdateSavedShift(Shift);
  if DoInplaceEditHandleKeyDown(Key, Shift) then Exit;
  DoCaretKeyDown(Key, Shift);
  case Key of
    VK_SHIFT: FShiftSelectionInit := True;
    VK_LEFT: DoCaretLeft(Shift);
    VK_RIGHT: DoCaretRight(Shift);
    VK_DOWN: DoCaretDown(Shift);
    VK_UP: DoCaretUp(Shift);
    VK_NEXT: DoCaretPageDown(Shift);
    VK_PRIOR: DoCaretPageUp(Shift);
    VK_HOME: DoCaretHome(Shift);
    VK_END: DoCaretEnd(Shift);
    VK_RETURN, VK_F2: EditAtCaretPos;
  end;
  {$IFDEF LINUX}
  // Special processing under Lazarus to block focus transfer
  if Key in [VK_LEFT, VK_RIGHT, VK_DOWN, VK_UP] then
    Key := VK_UNKNOWN;
  {$ENDIF}
end;

procedure TFWCustomHexView.KeyPress(var Key: Char);
begin
  inherited;
  {$IFNDEF FPC}
  DoCaretEdit(Key);
  {$ENDIF}
end;

procedure TFWCustomHexView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  UpdateSavedShift(Shift);
  if Key = VK_SHIFT then
    FShiftSelectionInit := False;
end;

function TFWCustomHexView.MakeDrawRect(LeftOffset, TopOffset,
  ColumnWidth: Integer): TRect;
begin
  Result := Bounds(
    LeftOffset + TextMargin,
    TopOffset,
    ColumnWidth - TextMargin shl 1,
    FRowHeight);
end;

function TFWCustomHexView.MeasureCanvas: TCanvas;
begin
  FMeasureCanvas.Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  FMeasureCanvas.Canvas.Font := Font;
  Result := FMeasureCanvas.Canvas;
end;

procedure TFWCustomHexView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CaretChangeMode: TCaretChangeMode;
begin
  try
    SetFocus;

    {$IFDEF FPC}
    case Button of
      mbExtra1: JumpUndo;
      mbExtra2: JumpRedo;
    end;
    {$ENDIF}

    if Button <> TMouseButton.mbLeft then Exit;

    FMousePressed := True;
    FMousePressedHitInfo := GetHitInfo(X, Y, Shift);

    if DoLButtonDown(FMousePressedHitInfo) then Exit;

    if keSplitter in FMousePressedHitInfo.Elements then
    begin
      FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] :=
        FMousePressedHitInfo.ColumnWidth;
    end
    else
    begin

      if RawData.Count = 0 then Exit;
      if FMousePressedHitInfo.SelectPoint.RowIndex < 0 then
        CaretChangeMode := ccmReset
      else if keHeader in FMousePressedHitInfo.Elements then
        CaretChangeMode := ccmNone
      else if ColumnResetSelection(FMousePressedHitInfo.SelectPoint.Column) then
        CaretChangeMode := ccmReset
      else if ssDouble in Shift then
        CaretChangeMode := ccmSelectRow
      else if ssShift in Shift then
        CaretChangeMode := ccmContinueSelection
      else if SelectOnMouseDown then
        CaretChangeMode := ccmSetNewSelection
      else
        CaretChangeMode := ccmNone;

      UpdateCaretPosData(FMousePressedHitInfo.SelectPoint, CaretChangeMode);
    end;

    Invalidate;

  finally
    inherited;
  end;
end;

procedure TFWCustomHexView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  HitTest: TMouseHitInfo;
  SelPoint: TSelectPoint;
  Painter: TAbstractPrimaryRowPainter;
begin
  try
    HitTest := GetHitInfo(X, Y, Shift);

    if InplaceEdit.HandleMouseMove(Shift, X, Y) or not FMousePressed then
    begin
      UpdateCursor(HitTest);
      Exit;
    end;

    if FMousePressed and (keSplitter in FMousePressedHitInfo.Elements) then
    begin
      FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] :=
        FMousePressedHitInfo.ColumnWidth + X - FMousePressedHitInfo.CursorPos.X;

      if FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] < MinColumnWidth then
        FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] := MinColumnWidth;

      UpdateTextBoundary;
      if X > FMousePressedHitInfo.CursorPos.X then
        UpdateScrollPos;
      Invalidate;
      Exit;
    end;

    if not SelectOnMouseDown and
      (FSelStart <> FMousePressedHitInfo.SelectPoint) and
      (HitTest.SelectPoint <> FMousePressedHitInfo.SelectPoint) then
    begin
      InvalidateSelections;
      FSelStart := FMousePressedHitInfo.SelectPoint;
      FSelEnd := HitTest.SelectPoint;
      InvalidateSelections;
    end;

    // проверка условий при которых невозможно выделение

    // checking for conditions under which allocation is not possible

    if not SelectOnMouseMove then Exit;
    if HitTest.SelectPoint.InvalidRow then Exit;
    if FSelStart.InvalidRow then Exit;
    if HitTest.SelectPoint = FMousePressedHitInfo.SelectPoint then Exit;

    // коррекция выхода за конец видимых данных в пределах одной линии

    // correction of going beyond the end of visible data within one line

    if (FMousePressedHitInfo.SelectPoint.RowIndex = HitTest.SelectPoint.RowIndex) and
      not HitTest.SelectPoint.ValidSelectedByte then
    begin
      Painter := GetRowPainter(HitTest.SelectPoint.RowIndex);
      if Assigned(Painter) then
      begin
        SelPoint.Column := HitTest.SelectPoint.Column;
        SelPoint.RowIndex := HitTest.SelectPoint.RowIndex;
        if FMousePressedHitInfo.CursorPos.X < X then
          SelPoint.CharIndex :=
            Painter.CharCount(HitTest.SelectPoint.Column) - 1
        else
          SelPoint.CharIndex := 0;
        SelPoint.ValueOffset :=
          Painter.CharIndexToValueOffset(SelPoint.Column, SelPoint.CharIndex);
        UpdateSelection(FSelStart, SelPoint);
        UpdateCaretPosData(HitTest.SelectPoint, ccmNone);
        Exit;
      end;
    end;

    UpdateCaretPosData(HitTest.SelectPoint, ccmContinueSelection);
  finally
    inherited;
  end;
end;

procedure TFWCustomHexView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FMousePressed := False;
  if keSplitter in FMousePressedHitInfo.Elements then
  begin
    UpdateTextBoundary;
    UpdateScrollPos;
  end
  else
    CreateCaretTimer;
  InplaceEdit.HandleMouseUp(Button, Shift, X, Y);
  inherited;
end;

procedure TFWCustomHexView.OnSelectionsChange(Sender: TObject);
begin
  FSelections.UpdateVisibleSelections;
  Invalidate;
end;

procedure TFWCustomHexView.Paint;
var
  Diapason, PostPaintDiapason: TVisibleRowDiapason;
  Offset: TPoint;
  SavedTopOffset: Integer;
  AClipRect: TRect;
  Clipped: Boolean;
  I: Integer;
{$ifdef profile_paint_speed}
  Stopwatch: TStopwatch;
{$endif}
begin
  {$ifdef profile_paint_speed}
  Stopwatch := TStopwatch.StartNew;
  {$endif}

  {$IFDEF USE_PROFILER}
  NeedProfile := ClassName = 'TAsmView';
  if NeedProfile then uprof.Start('Paint full');
  {$ENDIF}

  Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  Canvas.Font := Font;

  if FRowHeight = 0 then
    UpdateTextMetrics;

  AClipRect := Canvas.ClipRect;
  if InplaceEdit.Visible and (InplaceEdit.EditRect = AClipRect) then
  begin
    FInplaceEdit.PaintTo(Canvas);
    Exit;
  end;

  Diapason := VisibleRowDiapason;
  PostPaintDiapason := Diapason;

  // отсекаем все что не входит в клипинг

  // we're cutting out everything that's not clipping

  Clipped := (AClipRect <> ClientRect) and (AClipRect.Top > RowHeight);
  if Clipped then
  begin
    Offset := GetRowOffsetPoint(Diapason.EndRow);
    while Offset.Y > AClipRect.Bottom do
    begin
      Dec(Offset.Y, FRowHeight);
      Dec(Diapason.EndRow);
    end;
    Diapason.EndRow := Max(0, Diapason.EndRow);
    Diapason.StartRow := Diapason.EndRow;
    while Offset.Y > AClipRect.Top do
    begin
      Dec(Offset.Y, FRowHeight);
      Dec(Diapason.StartRow);
    end;
    Diapason.StartRow := Max(0, Diapason.StartRow);
  end;

  Offset := GetRowOffsetPoint(Diapason.StartRow);

  if Diapason.EndRow < FRawData.Count - 1 then
    Inc(Diapason.EndRow);

  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('DoBeforePaint');{$ENDIF}
  DoBeforePaint(Diapason);
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}

  // бэкграунд

  // background

  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('DrawBackground');{$ENDIF}
  DefaultPainter.DrawBackground(Canvas, FMousePressed, FMousePressedHitInfo);
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}

  // сами данные

  // data itself

  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('DrawRows');{$ENDIF}
  DrawRows(Diapason.StartRow, Diapason.EndRow, Offset);
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}

  // обработка постпайнтеров

  // postpainter handling

  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Start('postpainter handling');{$ENDIF}
  Offset := GetRowOffsetPoint(PostPaintDiapason.StartRow);
  SavedTopOffset := Offset.Y;
  for I := 0 to FPostPainters.Count - 1 do
  begin
    Offset.Y := SavedTopOffset;
    FPostPainters[I].PostPaint(Canvas,
      PostPaintDiapason.StartRow, PostPaintDiapason.EndRow, Offset);
  end;
  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}

  // верхняя строка может быть не до конца выровнена, если скрол внизу
  // поэтому заголовок рисуем последним, он затрет лишнее в шапке

  // the top line may not be fully aligned, if you scroll down,
  // so the header is drawn last, it will obscure the excess in the header

  if Header.Visible and not Clipped then
    Header.Paint(Canvas, FScrollOffset.X);

  // ну и отрисовка анимации редактируемой позиции самая последняя

  // and rendering of the animation of the edited position is the latest

  if InplaceEdit.Visible then
    InplaceEdit.PaintTo(Canvas)
  else
    if FCaretPosData.Showed then
    begin
      if not RowVisible(FCaretPosData.RowIndex) then Exit;
      Offset := GetRowOffsetPoint(FCaretPosData.RowIndex);
      Dec(Offset.Y, FRowHeight);
      DrawEditMark(Offset);
    end;

  {$IFDEF LINUX}
  if BorderStyle = bsSingle then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Rectangle(ClientRect);
  end;
  {$ENDIF}

  {$ifdef profile_paint_speed}
  Stopwatch.Stop;
  OutputDebugString(PChar(IntToStr(Stopwatch.ElapsedMilliseconds)));
  {$endif}

  {$IFDEF USE_PROFILER}if NeedProfile then uprof.Stop;{$ENDIF}
end;

function TFWCustomHexView.PrefferededSize: TLargePoint;
var
  I: TColumnType;
begin
  Result.X := 0;
  for I := ctWorkSpace to High(TColumnType) do
    if I in FHeader.Columns then
      Inc(Result.X, CalculateColumnBestSize(I));
  Inc(Result.X, NoDpiBorderMargin);
  Result.Y := FRawData.Count * FRowHeight;
  if Header.Visible then
    Inc(Result.Y, FRowHeight);
  Inc(Result.X, ToDpi(4));
  Inc(Result.Y, ToDpi(4));
end;

function TFWCustomHexView.ReadDataAtSelStart(var pBuffer;
  nSize: Integer): Integer;
begin
  Result := 0;
  if DataStream = nil then Exit;
  DataStream.Position := Min(SelStart, SelEnd) - StartAddress;
  Result := DataStream.Read(pBuffer, Min(nSize, Abs(SelEnd - SelStart) + 1));
end;

procedure TFWCustomHexView.RebuildData;
begin
  if FUpdateCount = 0 then
  begin
    UpdateDataMap;
    DoChange(cmData);
  end;
end;

procedure TFWCustomHexView.RegisterTextMetric(Value: TAbstractTextMetric);
begin
  FTextMetrics.Add(Value);
end;

procedure TFWCustomHexView.ReleaseDataStream;
begin
  if FStreamOwnerShip = soOwned then
    FreeAndNil(FDataStream);
end;

procedure TFWCustomHexView.ResetCanvas;
{$IFDEF LINUX}
begin
end;
{$ELSE}
var
  R: TRect;
begin
  // bug-bug!!!
  // если не восстановить состояние шрифта пустым вызовом DrawText
  // то DrawFocusRect и стили пера типа psDot будет рисоваться сплошной линией

  // bug-bug!!!
  // If the font state is not restored by an empty call to DrawText,
  // DrawFocusRect and pen styles like psDot will be drawn with a solid line

  R := TRect.Empty;
  Canvas.Font.Color := ColorMap.TextColor;
  Canvas.Font.Style := [];
  Canvas.Brush.Color := ColorMap.BackgroundColor;
  DrawText(Canvas, '', 0, R, 0);
end;
{$ENDIF}

procedure TFWCustomHexView.ResetPainters;
begin
  DestroyCaretTimer;
  FPainters.Clear;
  FPostPainters.Clear;
end;

procedure TFWCustomHexView.ResetViewState;
begin
  InitDefault;
  FitColumnsToBestSize;
end;

function TFWCustomHexView.RowRawLength(ARowIndex: Int64): Integer;
begin
  Result := 0;
  if (ARowIndex >= 0) and (ARowIndex < RawData.Count) then
    Result := RawData[ARowIndex].RawLength;
end;

procedure TFWCustomHexView.Resize;
begin
  inherited;
  if HandleAllocated and (FRowHeight > 0) then
  begin
    UpdateTextBoundary;
    UpdateScrollPos;
  end;
end;

procedure TFWCustomHexView.SetParent(AParent: TWinControl);
{$IFDEF FPC}
var
  AParentDpi: Integer;
  ADesigner: TCustomDesignControl;
{$ENDIF}
begin
  inherited;
  if csDestroying in ComponentState then
    Exit;
  if Parent = nil then
    Exit;
{$IFDEF FPC}
  ADesigner := GetParentDesignControl(Self);
  if ADesigner = nil then
    AParentDpi := USER_DEFAULT_SCREEN_DPI
  else
    AParentDpi := ADesigner.PixelsPerInch;
  if FCurrentPPI <> AParentDpi then
    AutoAdjustLayout(lapAutoAdjustForDPI, FCurrentPPI, AParentDpi, 0, 0);
{$ELSE}
  {$IF RtlVersion <= 34}

  // csFreeNotification:
  //   VCL не скейлит контрол, если у него есть этот флаг, делаем сами
  // csDesigning:
  //   Если вызывать GetParentCurrentDpi, то мы доберемся до главной
  //   формы IDE и возьмем ее DPI, а не DPI дизайнера (они отличаются)
  // Оба бага пофикшены в Delphi 11.0

  // csFreeNotification:
  //   VCL will not skale a control if it has this flag, do it yourself
  // csDesigning:
  //   If GetParentCurrentDpi is called, we will get to the main
  //   IDE form and take its DPI, not the designer's DPI (they are different).
  // Both bugs are fixed in Delphi 11.0

  if csDesigning in Parent.ComponentState then
    ScaleForPPI(TFWCustomHexView(Parent).FCurrentPPI)
  else if csFreeNotification in ComponentState then
    ScaleForPPI(TFWCustomHexView(Parent).GetParentCurrentDpi);
  {$ENDIF}
{$ENDIF}
end;

procedure TFWCustomHexView.RestoreViewParam;
var
  I: TColumnType;
  M: Integer;
begin
  // New proportions for columns resize
  BeginUpdate;
  try
    M := CharWidth * NoDpiMultipier;
    if not (csDesigning in ComponentState) then
    begin
      for I := ctWorkSpace to High(TColumnType) do
        FHeader.ColumnWidth[I] := MulDiv(FHeader.ColumnWidth[I], M, FPreviosCharWidth);
    end;
    if InplaceEdit.Visible then
      InplaceEdit.CalcEditRect;
  finally
    EndUpdate;
  end;
end;

function TFWCustomHexView.SelectPointInSelection(const Value: TSelectPoint
  ): Boolean;
var
  LeftSel, RightSel: TSelectPoint;
begin
  Result := False;

  if FSelStart.InvalidRow then Exit;
  if FSelEnd.InvalidRow then Exit;
  if Value.InvalidRow then Exit;

  if FSelEnd < FSelStart then
  begin
    LeftSel := FSelEnd;
    RightSel := FSelStart;
  end
  else
  begin
    LeftSel := FSelStart;
    RightSel := FSelEnd;
  end;

  Result := (Value >= LeftSel) and (Value <= RightSel);
end;

function TFWCustomHexView.RowToAddress(ARowIndex: Int64;
  ValueOffset: Integer): Int64;
begin
  Result := RawData.RowToAddress(ARowIndex, ValueOffset);
end;

function TFWCustomHexView.SelectedColumnAsString(AColumn: TColumnType): string;
begin
  Result := ColumnAsString(SelectedRowIndex, AColumn);
end;

function TFWCustomHexView.SelectedRawLength: Integer;
begin
  Result := RowRawLength(SelectedRowIndex);
end;

function TFWCustomHexView.SelectedRowIndex: Int64;
begin
  Result := LeftSelPoint.RowIndex;
end;

function TFWCustomHexView.SelectPointToAddress(const AValue: TSelectPoint
  ): Int64;
begin
  Result := RowToAddress(AValue.RowIndex, AValue.ValueOffset);
end;

function TFWCustomHexView.RowVisible(ARowIndex: Int64): Boolean;
var
  D: TVisibleRowDiapason;
begin
  if ARowIndex < 0 then Exit(False);
  D := VisibleRowDiapason;
  Result := (D.StartRow <= ARowIndex) and (D.EndRow >= ARowIndex);
end;

procedure TFWCustomHexView.SaveViewParam;
begin
  FPreviosCharWidth := FCharWidth * NoDpiMultipier;
end;

function TFWCustomHexView.GetDefaultCaretChangeMode: TCaretChangeMode;
begin
  Result := ccmSelectPointer;
end;

function TFWCustomHexView.Scroll32To64(Value: Integer): Int64;
var
  Tmp: Double;
begin
  if FTextBoundary.Y < MaxInt then
    Result := Value
  else
  begin
    Tmp := Value * (FTextBoundary.Y / MaxInt);
    if Tmp > High(Int64) - 1 then
      Result := High(Int64)
    else
      Result := Round(Tmp);
  end;
end;

function TFWCustomHexView.Scroll64To32(Value: Int64): Integer;
begin
  if FTextBoundary.Y < MaxInt then
    Result := Value
  else
    Result := Round(Value * (MaxInt / FTextBoundary.Y));
end;

function TFWCustomHexView.SelectPoint(ARowIndex: Int64; ACharIndex: Integer;
  AColumn: TColumnType): TSelectPoint;
var
  Painter: TAbstractPrimaryRowPainter;
begin
  Result.RowIndex := ARowIndex;
  Result.Column := AColumn;
  Painter := GetRowPainter(ARowIndex);
  if Assigned(Painter) then
  begin
    if ACharIndex < 0 then
      ACharIndex := Painter.CharCount(AColumn) - 1;
    Result.CharIndex := ACharIndex;
    Result.ValueOffset := Painter.CharIndexToValueOffset(AColumn, ACharIndex);
  end
  else
  begin
    Result.ValueOffset := -1;
    Result.CharIndex := -1;
  end;
end;

procedure TFWCustomHexView.SetAddressMode(const Value: TAddressMode);
begin
  if AddressMode <> Value then
  begin
    FAddressMode := Value;
    DoChange(cmAddressMode);
  end;
end;

procedure TFWCustomHexView.SetAddressView(const Value: TAddressView);
begin
  if AddressView <> Value then
  begin
    FAddressView := Value;
    DoChange(cmAddressView);
  end;
end;

procedure TFWCustomHexView.SetAddressViewOffsetBase(const Value: Int64);
begin
  if AddressViewOffsetBase <> Value then
  begin
    FAddressViewOffsetBase := Value;
    DoChange(cmAddressView);
  end;
end;

procedure TFWCustomHexView.SetBookMark(AIndex: TBookMark; const Value: Int64);
begin
  if FBookMarks[AIndex] <> Value then
  begin
    FBookMarks[AIndex] := Value;
    DoChange(cmBookmark);
  end;
end;

procedure TFWCustomHexView.SetBytesInColorGroup(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.CreateFmt('Wrong BytesInColorGroup value (%d)', [Value]);
  if BytesInColorGroup <> Value then
  begin
    FBytesInColorGroup := Value;
    DoChange(cmBytesInColorGroup);
  end;
end;

procedure TFWCustomHexView.SetBytesInGroup(const Value: Integer);
begin
  if Value < 2 then
    raise Exception.CreateFmt('Wrong BytesInGroup value (%d)', [Value]);
  if BytesInGroup <> Value then
  begin
    FBytesInGroup := Value;
    DoChange(cmBytesInGroup);
  end;
end;

procedure TFWCustomHexView.SetBytesInRow(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.CreateFmt('Wrong BytesInRow value (%d)', [Value]);
  if BytesInRow <> Value then
  begin
    FBytesInRow := Value;
    DoChange(cmBytesInRow);
    ClearSelection;
    RebuildData;
  end;
end;

procedure TFWCustomHexView.SetByteViewMode(Value: TByteViewMode);
begin
  if ByteViewMode <> Value then
  begin
    FByteViewMode := Value;
    DoChange(cmByteViewMode);
  end;
end;

procedure TFWCustomHexView.SetCaretPos(AColumn: TColumnType; ARowIndex: Int64;
  ACharIndex: Integer);
begin
  UpdateCaretPosData(SelectPoint(ARowIndex, ACharIndex, AColumn), ccmNone);
end;

procedure TFWCustomHexView.SetColorMap(const Value: THexViewColorMap);
begin
  ColorMap.Assign(Value);
end;

procedure TFWCustomHexView.SetCtlBorderStyle(const Value: TBorderStyle);
begin
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd{$IFDEF FPC}(Self){$ENDIF};
  end;
end;

procedure TFWCustomHexView.SetDataStream(Value: TStream;
  StartAddress: Int64; AOwnerShip: TStreamOwnership);
begin
  if (FDataStream <> Value) or (StartAddress <> FStartAddress) then
  begin
    BeginUpdate;
    try
      ReleaseDataStream;
      FStreamOwnerShip := AOwnerShip;
      FDataStream := Value;
      FStartAddress := StartAddress;
      InternalClear;
      RebuildData;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TFWCustomHexView.SetDefaultPainter(const Value: TAbstractPrimaryRowPainter);
begin
  if DefaultPainter <> Value then
  begin
    FDefaultPainter := Value;

    // с очисткой списка метрик все зарегистрированные метрики будут разрушены

    // with clearing the metrics list, all registered metrics will be destroyed

    FTextMetrics.Clear;
    FTextMetric := FDefaultPainter.GetTextMetricClass.Create(Self);
  end;
end;

procedure TFWCustomHexView.SetEncoder(const Value: TCharEncoder);
begin
end;

procedure TFWCustomHexView.SetHeader(const Value: TCustomHexViewHeader);
begin
end;

procedure TFWCustomHexView.SetHideSelection(const Value: Boolean);
begin
  if HideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateSelections;
  end;
end;

function TFWCustomHexView.SetNewEditRowIndex(ANewRowIndex: Int64): Int64;
begin
  Result := FCaretPosData.RowIndex;
  if FCaretPosData.RowIndex <> ANewRowIndex then
  begin
    FCaretPosData.RowIndex := ANewRowIndex;
    if ANewRowIndex >= 0 then
    begin
      if not RowVisible(ANewRowIndex) then
        FocusOnRow(ANewRowIndex, ccmNone)
      else
        InvalidateCaretPosData(FCaretPosData.Showed);
    end;
  end;
end;

procedure TFWCustomHexView.SetNoDataText(const Value: string);
begin
  if NoDataText <> Value then
  begin
    FNoDataText := Value;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.SetReadOnly(const Value: Boolean);
begin
  if ReadOnly <> Value then
  begin
    FReadOnly := Value;
  end;
end;

procedure TFWCustomHexView.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    if HandleAllocated then
      RecreateWnd{$IFDEF FPC}(Self){$ENDIF};
  end;
end;

procedure TFWCustomHexView.SetScrollOffset(X, Y: Int64);
begin
  if FScrollOffset.X <> X then
  begin
    FScrollOffset.X := X;
    DoChange(cmScrollX);
  end;
  if FScrollOffset.Y <> Y then
  begin
    FScrollOffset.Y := Y;
    DoChange(cmScrollY);
  end;
end;

procedure TFWCustomHexView.SetSelEnd(const Value: Int64);
begin
  UpdateSelection(FSelStart, AddressToSelectPoint(Value), False);
end;

procedure TFWCustomHexView.SetSelStart(const Value: Int64);
begin
  UpdateSelection(AddressToSelectPoint(Value), FSelEnd, False);
end;

procedure TFWCustomHexView.SetSeparateGroupByColor(const Value: Boolean);
begin
  if SeparateGroupByColor <> Value then
  begin
    FSeparateGroupByColor := Value;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.SetShortCuts(const Value: TViewShortCuts);
begin
  FShortCuts.Assign(Value);
end;

procedure TFWCustomHexView.SetTopRow(ARowIndex: Int64);
begin
  if ARowIndex < 0 then
    ARowIndex := 0;
  UpdateScrollY(-ARowIndex * FRowHeight);
end;

function TFWCustomHexView.ToDpi(Value: Integer): Integer;
begin
  Result := MulDiv(Value, FCurrentPPI, USER_DEFAULT_SCREEN_DPI);
end;

procedure TFWCustomHexView.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TFWCustomHexView.WMHScroll(var Msg: TWMHScroll);
begin
  case Msg.ScrollCode of
    SB_LINELEFT: Inc(FScrollOffset.X, CharWidth);
    SB_LINERIGHT: Dec(FScrollOffset.X, CharWidth);
    SB_PAGELEFT: Inc(FScrollOffset.X, ClientWidth);
    SB_PAGERIGHT: Dec(FScrollOffset.X, ClientWidth);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      SetScrollOffset(Msg.Pos * -1, FScrollOffset.Y);
  end;
  UpdateScrollPos;
end;

procedure TFWCustomHexView.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  DestroyCaretTimer;
  InvalidateSelections;
  if not FProcessMenu then
    InplaceEdit.Hide;
end;

procedure TFWCustomHexView.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  InvalidateSelections;
  CreateCaretTimer;
end;

procedure TFWCustomHexView.WMTimer(var Msg: TWMTimer);
begin
  InvalidateCaretPosData(not FCaretPosData.Showed);
end;

procedure TFWCustomHexView.WMVScroll(var Msg: TWMVScroll);
var
  SI: TScrollInfo;
  NewVerticalOffset: Int64;
begin
  NewVerticalOffset := FScrollOffset.Y;
  case Msg.ScrollCode of
    SB_LINEUP: Inc(NewVerticalOffset, FRowHeight);
    SB_LINEDOWN: Dec(NewVerticalOffset, FRowHeight);
    SB_PAGEUP: Inc(NewVerticalOffset, GetPageHeight);
    SB_PAGEDOWN: Dec(NewVerticalOffset, GetPageHeight);
    SB_THUMBPOSITION, SB_THUMBTRACK:
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask :=  SIF_TRACKPOS;
      GetScrollInfo(Handle, SB_VERT, SI);
      NewVerticalOffset := -Scroll32To64(SI.nTrackPos);
    end;
  end;
  NewVerticalOffset := NewVerticalOffset - (NewVerticalOffset mod FRowHeight);
  UpdateScrollY(NewVerticalOffset);
end;

{$IFNDEF FPC}
procedure TFWCustomHexView.WMXButtonDown(var Msg: TWMMouse);
const
  MK_XBUTTON1 = $20;
  MK_XBUTTON2 = $40;
begin
  case Word(Msg.Keys) of
    MK_XBUTTON1: JumpUndo;
    MK_XBUTTON2: JumpRedo;
  end;
end;
{$ENDIF}

procedure TFWCustomHexView.UpdateCaretColumn(AColumn: TColumnType);
begin
  FCaretPosData.Column := AColumn;
end;

procedure TFWCustomHexView.UpdateCaretPosData(Value: TSelectPoint;
  AChangeMode: TCaretChangeMode);
var
  Painter: TAbstractPrimaryRowPainter;
  ASelStartAddrVA, APointerSize: Int64;
begin
  DestroyCaretTimer;

  case AChangeMode of
    ccmSetNewSelection:
      UpdateSelection(Value, Value);
    ccmContinueSelection:
    begin
      if FShiftSelectionInit then
      begin
        FShiftSelectionInit := False;
        UpdateSelection(
          SelectPoint(FCaretPosData.RowIndex, FCaretPosData.CharIndex, FCaretPosData.Column),
          Value);
      end
      else
        UpdateSelection(FSelStart, Value);
    end;
    ccmSelectRow:
      UpdateSelection(SelectPoint(Value.RowIndex, 0, Value.Column),
        SelectPoint(Value.RowIndex, -1, Value.Column));
    ccmSelectPointer:
    begin
      ASelStartAddrVA := RawData.RowToAddress(Value.RowIndex, Max(0, Value.ValueOffset));
      case AddressMode of
        am8bit: APointerSize := 1;
        am16bit: APointerSize := 2;
        am32bit: APointerSize := 4;
      else
        APointerSize := 8;
      end;
      UpdateSelectionAddr(ASelStartAddrVA, ASelStartAddrVA + APointerSize - 1);
    end;
    ccmReset:
    begin
      Value.Erase;
      UpdateSelection(Value, Value);
    end;
  end;

  if Value.RowIndex < 0 then Exit;
  if FRawData.Count = 0 then Exit;

  if Value.Column <> ctNone then
    SetNewEditRowIndex(Value.RowIndex);
  FCaretPosData.Column := Value.Column;
  FCaretPosData.CharIndex := Value.CharIndex;
  FCaretPosData.Showed := Value.ValidSelectedByte and Focused;
  FCaretPosData.EditMode := cemDisabled;
  if not ReadOnly then
  begin
    Painter := GetRowPainter(Value.RowIndex);
    if Assigned(Painter) then
      FCaretPosData.EditMode := Painter.CaretEditMode(Value.Column);
  end;

  if FCaretPosData.EditMode = cemDisabled then Exit;
  if FCaretPosData.Showed then
  begin
    InvalidateCaretPosData(FCaretPosData.Showed);
    CreateCaretTimer;
  end;

  if Assigned(FOnCaretPosChange) then
    FOnCaretPosChange(Self);
end;

procedure TFWCustomHexView.UpdateCaretTimer;
begin
  if FCaretPosData.EditMode <> cemDisabled then
  begin
    InternalKillCaretTimer;
    if not FCaretPosData.Showed then
      InvalidateCaretPosData(True);
    CreateCaretTimer;
  end;
end;

procedure TFWCustomHexView.UpdateCursor(var AHitInfo: TMouseHitInfo);
begin
  if (keSplitter in AHitInfo.Elements) and (AHitInfo.Cursor = crDefault) then
    AHitInfo.Cursor := crHSplit;
  if (keInplaceEdit in AHitInfo.Elements) and (AHitInfo.Cursor = crDefault) then
    AHitInfo.Cursor := crIBeam;
  Cursor := AHitInfo.Cursor;
end;

procedure TFWCustomHexView.UpdateDataMap;
{$ifdef profile_speed}
var
  Stopwatch: TStopwatch;
{$endif}
begin
  {$ifdef profile_speed}
  Stopwatch := TStopwatch.StartNew;
  {$endif}

  FRawData.Clear;
  if GetDataStreamSize = 0 then Exit;
  FRawData.Update;

  {$ifdef profile_speed}
  Stopwatch.Stop;
  OutputDebugString(PChar(IntToStr(Stopwatch.ElapsedMilliseconds)));
  {$endif}
end;

procedure TFWCustomHexView.UpdateSavedShift(Shift: TShiftState);
var
  P: TPoint;
  HitTest: TMouseHitInfo;
begin
  if FSavedShift <> Shift then
  begin
    FSavedShift := Shift;
    if not FMousePressed then
    begin
      GetCursorPos(P{%H-});
      P := ScreenToClient(P);
      HitTest := GetHitInfo(P.X, P.Y, Shift);
      UpdateCursor(HitTest);
    end;
  end;
end;

procedure TFWCustomHexView.UpdateSystemWheelMultiplier;
begin
  {$IFDEF LINUX}
  FSystemWheelMultiplier := 3;
  {$ELSE}
  if GetSystemMetrics(SM_MOUSEWHEELPRESENT) = 1 then
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FSystemWheelMultiplier, 0);
  {$ENDIF}
end;

procedure TFWCustomHexView.UpdateScrollPos;
var
  ScrollInfo : TScrollInfo;
  MaxOffset: Int64;
begin
  if not HandleAllocated then Exit;

  Invalidate;

  case ScrollBars of
    TScrollStyle.ssVertical:
    begin
      UpdateVerticalScrollPos;
      Exit;
    end;
    TScrollStyle.ssNone: Exit;
  end;

  MaxOffset := -(FTextBoundary.X - ClientWidth);
  if MaxOffset < 0 then
  begin
    if FScrollOffset.X < MaxOffset then
      SetScrollOffset(MaxOffset, FScrollOffset.Y);
    if FScrollOffset.X >= -NoDpiBorderMargin then
      SetScrollOffset(0, FScrollOffset.Y);

    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPos := -FScrollOffset.X;
    ScrollInfo.nPage := ClientWidth;
    ScrollInfo.nMax := FTextBoundary.X;
    ShowScrollBar(Handle, SB_HORZ, True);
    SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  end
  else
  begin
    SetScrollOffset(0, FScrollOffset.Y);
    ShowScrollBar(Handle, SB_HORZ, False);
  end;

  UpdateVerticalScrollPos;
end;

procedure TFWCustomHexView.UpdateScrollY(AOffset: Int64);
begin
  SetScrollOffset(FScrollOffset.X, AOffset);
  UpdateVerticalScrollPos;
end;

procedure TFWCustomHexView.UpdateSelection(ANewStart, ANewEnd: TSelectPoint;
  AUpdateSelAddr: Boolean);
var
  Painter: TAbstractPrimaryRowPainter;
begin
  if (FSelStart <> ANewStart) or (FSelEnd <> ANewEnd) then
  begin
    InvalidateSelections;
    FSelStart := ANewStart;
    FSelEnd := ANewEnd;

    if (ANewStart.RowIndex < 0) or (ANewEnd.RowIndex < 0) then
    begin
      FSelStartAddr := -1;
      FSelEndAddr := -1;
    end
    else
    begin
      FSelStartAddr := RawData.RowToAddress(ANewStart.RowIndex, Max(0, ANewStart.ValueOffset));
      FSelEndAddr := RawData.RowToAddress(ANewEnd.RowIndex, ANewEnd.ValueOffset);
      if AUpdateSelAddr then
      begin
        if FSelStartAddr <= FSelEndAddr then
        begin
          Painter := GetRowPainter(FSelEnd.RowIndex);
          if Assigned(Painter) and (ANewEnd.ValueOffset >= 0) then
            Inc(FSelEndAddr, Painter.TextMetric.ValueMetric.ByteCount - 1);
        end
        else
        begin
          Painter := GetRowPainter(FSelStart.RowIndex);
          if Assigned(Painter) and (ANewStart.ValueOffset >= 0) then
            Inc(FSelStartAddr, Painter.TextMetric.ValueMetric.ByteCount - 1);
        end;
      end;
    end;

    DoSelectionChage(FSelStartAddr, FSelEndAddr);
    InvalidateSelections;
  end;
end;

procedure TFWCustomHexView.UpdateSelectionAddr(ANewStart, ANewEnd: Int64);
begin
  UpdateSelection(AddressToSelectPoint(ANewStart), AddressToSelectPoint(ANewEnd), False);
end;

procedure TFWCustomHexView.DoBeforePaint(const ADiapason: TVisibleRowDiapason);
begin
  // do nothing...
end;

procedure TFWCustomHexView.UpdateTextBoundary;
var
  I: TColumnType;
begin
  FTextBoundary.X := 0;
  for I := ctWorkSpace to High(TColumnType) do
    if I in FHeader.Columns then
      Inc(FTextBoundary.X, FHeader.ColumnWidth[I]);
  Inc(FTextBoundary.X, NoDpiBorderMargin);
  if FRowHeight = 0 then
    FTextBoundary.Y := 0
  else
    FTextBoundary.Y := FRawData.Count * FRowHeight + FRowHeight +
      ClientHeight mod FRowHeight;
  if not Header.Visible then
    Dec(FTextBoundary.Y, FRowHeight);
end;

procedure TFWCustomHexView.UpdateTextDarknessColor;
begin
  FDefaultFontColorIsDark := IsColorRefDark(ColorToRGB(ColorMap.TextColor));
end;

procedure TFWCustomHexView.UpdateTextExtent;
{$IFDEF UNIX}
var
  TM: TTextMetric;
{$ENDIF}
begin
  {$IFDEF UNIX}
  GetTextMetrics(MeasureCanvas.Handle, TM{%H-});
  FCharWidth := TM.tmAveCharWidth;
  FRowHeight := GetTextExtent.cy;
  {$ELSE}
  with GetTextExtent do
  begin
    FCharWidth := cx;
    FRowHeight := cy;
  end;
  {$ENDIF}
end;

procedure TFWCustomHexView.UpdateTextMetrics;
var
  I: Integer;
begin
  for I := 0 to FTextMetrics.Count - 1 do
    FTextMetrics[I].Update;
end;

procedure TFWCustomHexView.UpdateVerticalScrollPos;
var
  ScrollInfo : TScrollInfo;
  MaxOffset: Int64;
begin
  case ScrollBars of
    TScrollStyle.ssHorizontal, TScrollStyle.ssNone: Exit;
  end;
  MaxOffset := -(FTextBoundary.Y - ClientHeight);
  if MaxOffset >= 0 then
  begin
    SetScrollOffset(FScrollOffset.X, 0);
    ShowScrollBar(Handle, SB_VERT, False);
  end
  else
  begin
    if FScrollOffset.Y < MaxOffset then
      SetScrollOffset(FScrollOffset.X, MaxOffset);
    if FScrollOffset.Y > 0 then
      SetScrollOffset(FScrollOffset.X, 0);

    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPos := -Scroll64To32(FScrollOffset.Y);
    ScrollInfo.nPage := Scroll64To32(ClientHeight);
    ScrollInfo.nMax := Scroll64To32(FTextBoundary.Y);
    ShowScrollBar(Handle, SB_VERT, True);
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

    Invalidate;
  end;

  if InplaceEdit.Visible then
    InplaceEdit.CalcEditRect;
end;

procedure TFWCustomHexView.UpdateView;
begin
  ClearSelection;
  UpdateTextMetrics;
  Invalidate;
end;

procedure TFWCustomHexView.UTF8KeyPress(var UTF8Key: TNativeChar);
begin
  inherited;
  DoCaretEdit(UTF8Key);
end;

function TFWCustomHexView.VisibleRowCount: Integer;
begin
  if FRowHeight = 0 then
    Result := 0
  else
    Result := GetPageHeight div FRowHeight;
end;

function TFWCustomHexView.WheelRowCount: Integer;
begin
  if WheelMultiplier <> 0 then
    Result := WheelMultiplier
  else
  begin
    UpdateSystemWheelMultiplier;
    Result := FSystemWheelMultiplier;
    if Result < 0 then
    begin
      Result := ClientHeight div FRowHeight;
      if Header.Visible then
        Dec(Result);
    end;
  end;
end;

procedure TFWCustomHexView.WndProc(var AMsg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  if AMsg.Msg = {$IFDEF FPC}LM_CONTEXTMENU{$ELSE}WM_CONTEXTMENU{$ENDIF} then
  begin
    FProcessMenu := True;
    try
      inherited WndProc(AMsg);
    finally
      FProcessMenu := False;
    end;
    Exit;
  end;
  inherited WndProc(AMsg);
end;

procedure TFWCustomHexView.Zoom(AZoomValue: Integer);
begin
  SaveViewParam;
  if AZoomValue > 0 then
    Font.Size := Font.Size + AZoomValue
  else
    if Font.Size + AZoomValue > 4 then
      Font.Size := Font.Size + AZoomValue;
  RestoreViewParam;
end;

function TFWCustomHexView.VisibleRowDiapason: TVisibleRowDiapason;
begin
  Result.StartRow := CurrentVisibleRow;
  Result.EndRow := Min(Result.StartRow +
    GetPageHeight div FRowHeight, FRawData.Count - 1);
  if (FLastDiapasone.StartRow <> Result.StartRow) or (FLastDiapasone.EndRow <> Result.EndRow) then
  begin
    FLastDiapasone := Result;
    DoChange(cmDiapasoneChange);
  end;
end;

end.
