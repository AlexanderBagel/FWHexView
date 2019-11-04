////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView
//  * Purpose   : Класс для просмотра данных
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2019.
//  * Version   : 1.0.5
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//

unit FWHexView;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  Graphics,
  ClipBrd,
  SysUtils,
  Forms,
  Types,
  Themes,
  UxTheme,
  GraphUtil,
  UITypes,
  Generics.Collections,
  Generics.Defaults;

{ TODO:
  Hint для гиперлинка или строки + внешний обработчик для кастомного хинта на ActivateHintWindow
  Цветовая схема
  Возможность выделять фон другим цветом (в режиме HEX) с собственным хинтом и подчеркиванием
  Внешний обрабточик на изменение (можно или нельзя)
  Бряки в дополнение к букмаркам
  Выделять зеленым и желтым цветом по аналогии с дельфей на ctWorkSpace места редактирования
  Undo список (он же завязан на предыдущий пункт с желтыми и зелеными линиями)
}

type
  TLineStyle = (
    lsNone,
    lsSeparator,
    lsLine,
    lsRaw,
    lsRawWithExDescription,
    lsAsm,
    lsCheck,
    lsRadio,
    lsCode);

  THintType = (htNone, htDescriptionLine, htHiperlink);

  TLineData = record
    Style: TLineStyle;
    Address: ULONG_PTR;
    DataOffset: NativeUInt;
    RawLength: Byte;
    Description: string;
    Comment: string;
    RawColor: TColor;

    HintType: THintType;
    Hint: string;
    case Integer of
      0: (
        // для Style = lsRawWithExDescription, lsAsm
        JmpToAddr: ULONG_PTR;
        // параметры линка в колонке Description
        LinkStart: Integer;   // индекс символа с которого начинается гиперлинк
        LinkLength: Integer;  // размер гиперлинка
        Linked: Boolean);     // метка оначает что есть входящий линк
      1: (
        // для Style = lsCheck, lsRadio
        LineIndex: Integer;
        CharIndex: Byte;
        Checked: Boolean);
  end;

  TMapLine = record
    Style: TLineStyle;
    Address: ULONG_PTR;
    RawLength: Byte;
    Description: string;
    Comment: string;
    RawColor: TColor;
    case Integer of
      0: (
        JmpToAddr: ULONG_PTR;
        LinkStart: Integer;
        LinkLength: Integer);
      1: (
        LineIndex: Integer;
        CharIndex: Byte;
        Checked: Boolean);
  end;

  TFWCustomHexView = class;

  TDataMap = class
  strict private
    FOwner: TFWCustomHexView;
    FData: TList<TMapLine>;
    FUpdateCount: Integer;
    FCurrentAddr: ULONG_PTR;
    procedure DataChange(Sender: TObject; const Item: TMapLine;
      Action: TCollectionNotification);
    procedure RebuildDataMap;
  protected
    property CurrentAddr: ULONG_PTR read FCurrentAddr write FCurrentAddr;
  public
    constructor Create(AOwner: TFWCustomHexView);
    destructor Destroy; override;

    procedure Clear;

    function AddCheck(ByteIndex: Byte;
      const Description, Comment: string; Checked: Boolean): Integer; overload;
    function AddCheck(Address: ULONG_PTR; CharIndex: Byte;
      const Description, Comment: string; Checked: Boolean): Integer; overload;

    function AddRadio(ByteIndex: Byte;
      const Description, Comment: string; Checked: Boolean): Integer; overload;
    function AddRadio(Address: ULONG_PTR; CharIndex: Byte;
      const Description, Comment: string; Checked: Boolean): Integer; overload;

    function AddSeparator(const Description: string): Integer; overload;
    function AddSeparator(Address: ULONG_PTR; const Description: string): Integer; overload;

    function AddExDescription(Address: ULONG_PTR; DataLength: Byte;
      const Description, Comment: string; JmpToAddr: ULONG_PTR;
      LinkStart, LinkLength: Integer; RawColor: TColor = clWindowText): Integer; overload;
    function AddExDescription(DataLength: Byte;
      const Description, Comment: string; JmpToAddr: ULONG_PTR;
      LinkStart, LinkLength: Integer; RawColor: TColor = clWindowText): Integer; overload;
    function AddExDescription(DataLength: Byte;
      const Description, Comment: string; RawColor: TColor = clWindowText): Integer; overload;
    function AddExDescription(DataLength: Byte;
      const Description: string; RawColor: TColor = clWindowText): Integer; overload;

    function AddAsm(Address: ULONG_PTR; DataLength: Byte;
      const Description, Comment: string; JmpToAddr: ULONG_PTR;
      LinkStart, LinkLength: Integer; RawColor: TColor = clWindowText): Integer; overload;
    function AddAsm(DataLength: Byte;
      const Description, Comment: string; JmpToAddr: ULONG_PTR;
      LinkStart, LinkLength: Integer; RawColor: TColor = clWindowText): Integer; overload;
    function AddAsm(DataLength: Byte;
      const Description, Comment: string; RawColor: TColor = clWindowText): Integer; overload;
    function AddAsm(DataLength: Byte;
      const Description: string; RawColor: TColor = clWindowText): Integer; overload;

    function AddCode(const Description: string): Integer; overload;
    function AddCode(Address: ULONG_PTR; const Description: string): Integer; overload;

    function AddLine(Address: ULONG_PTR): Integer; overload;
    function AddLine: Integer; overload;

    function AddNone(Address: ULONG_PTR): Integer; overload;
    function AddNone: Integer; overload;

    procedure Assign(Value: TDataMap);
    procedure BeginUpdate;
    procedure EndUpdate;
    property Data: TList<TMapLine> read FData;
  end;

  ///<summary> TAddressMode - режим отображения адреса 32/64 бита. </summary>
  TAddressMode = (am32bit, am64bit);
  ///<summary> TAddressView - режим отображения адреса HEX/Decimal. </summary>
  TAddressView = (avHex, avDecimal);

  ///<summary> TColumnType - доступные типы колонок. </summary>
  TColumnType =
    (ctNone, ctWorkSpace, ctJmpLine, ctAddress, ctOpcode, ctDescription, ctComment);
  ///<summary> Отображаемые типы колонок.</summary>
  TFWHexViewColumnTypes = set of TColumnType;

  ///<summary> Номера закладок. </summary>
  TBookMark = 0..9;

  ///<summary> Точка остановки. </summary>
  TBreakPoint = record
    ///<summary> TBreakPoint - адрес точки остановки. </summary>
    Address: ULONG_PTR;
    ///<summary> TBreakPoint - состояние точки остановки, активна/не активна. </summary>
    Active: Boolean;
  end;

  TJmpState = (jsPushToUndo, jsPopFromUndo);
  TJmpToEvent = procedure(Sender: TObject; const AJmpAddr: ULONG_PTR;
    AJmpState: TJmpState; var Handled: Boolean) of object;

  /// <summary> Тип копирования данных
  ///  csAllData - все с коментариями
  ///  csBytes - только данные
  ///  csPascal - массив в паскаль коде
  ///  csCpp - массив в С++ коде
  /// </summary>
  TCopyStyle = (csAllData, csBytes, csPascal, csCpp);

  TFWCustomHexView = class(TCustomControl)
  private type
    TControlState = (csMousePressed, csNeedResizeColumn,
      csNeedUpdateDataMap, csLeftCharSelected);
    TControlStates = set of TControlState;

    TVisibleLineDiapason = record
      StartLine: Integer;
      EndLine: Integer;
    end;

    TSelectPoint = record
      LineIndex: Integer;
      ByteIndex: Integer;
      Column: TColumnType;
      OnHeader: Boolean;
      class operator LessThan(A, B: TSelectPoint): Boolean; inline;
      class operator NotEqual(A, B: TSelectPoint): Boolean; inline;
      procedure Erase; inline;
      function InvalidLine: Boolean; inline;
      function ValidSelectedByte: Boolean; inline;
    end;

    TMouseHitInfo = record
      XPos, YPos: Integer;
      SelectPoint: TSelectPoint;
      OnSplitter: Boolean;
      OnJmpMark: Boolean;
      ColumnWidth: Integer;
      procedure Erase; inline;
    end;

    TSelectStyle = (ssNone, ssAllSelected, ssLeftSelected,
      ssCenterSelected, ssRightSelected);
    TSelectData = record
      SelectStyle: TSelectStyle;
      FirstSelectIndex: Integer;
      SecondSelectIndex: Integer;
    end;

    TEditPosData = record
      LineIndex: Integer;
      Column: TColumnType;
      Showed: Boolean;
      case Integer of
        0: (
          OpcodeCharIndex: Integer);
        1: (
          DescriptionByteIndex: Integer);
    end;

    TJmpData = record
      JmpFrom, JmpTo: Integer;
    end;

    TGroupMode = (gmOff, gmOn);

    TArrowDirection = (adLeft, adRight, adUp, adDown);

    // Варианты отрисовки линий прыжков
    TJmpLineType = (
      jtNone,
      jtVisibleLines,       // обе строки видимы
      jtDownFromInvisible,  // прыжок с невидимой строки сверху
      jtUpToInvisible,      // прыжок на верхнюю невидимую строку
      jtDownToInvisible,    // прыжок на нижнюю невидимую строку
      jtUpFromInvisible     // прыжок с невидимой строки снизу
      );

    {$M+} // [dcc32 Warning] W1055 PUBLISHED caused RTTI ($M+) to be added to type 'TFWHexView.THeader'

    THeader = class
    strict private
      FOwner: TFWCustomHexView;
      FVisible: Boolean;
      FColumns: TFWHexViewColumnTypes;
      FColumnCaption: array [TColumnType] of string;
      FColumnWidth: array [TColumnType] of Integer;
      procedure SetVisible(const Value: Boolean);
      procedure SetColumns(const Value: TFWHexViewColumnTypes);
      function GetColWidth(Value: TColumnType): Integer;
      procedure SetColWidth(Value: TColumnType; AWidth: Integer);
      private
        function GetColCaption(Value: TColumnType): string;
        procedure SetColCaption(Value: TColumnType; const NewCaption: string);
    public
      constructor Create(AOwner: TFWCustomHexView);
      property ColumnCaption[Value: TColumnType]: string read GetColCaption write SetColCaption;
      property ColumnWidth[Value: TColumnType]: Integer read GetColWidth write SetColWidth;
    published
      property Columns: TFWHexViewColumnTypes read FColumns write SetColumns;
      property Visible: Boolean read FVisible write SetVisible;
    end;

  private const
    BORDER_MARGIN = 2;
    TEXT_MARGIN = 8;
    SPLIT_MARGIN = 3;
    MIN_COLUNN_WIDTH = TEXT_MARGIN * 4;
    WG_STRING = 'Wg';
    CHAR_STRING = 'W';
  private
    FHeader: THeader;
    FDataMap: TDataMap;
    FDataStream: TStream;
    FStates: TControlStates;
    FStartAddress: ULONG_PTR;
    FAddressMode: TAddressMode;
    FBorderStyle: TBorderStyle;
    FSeparateGroupByColor: Boolean;
    FBytesInGroup, FBytesInColorGroup: Integer;
    FRawData: TList<TLineData>;
    FScrollOffset, FTextBoundary: TPoint;
    FScrollBars: TScrollStyle;
    FUpdateCount: Integer;
    FLineHeight: Integer;
    FCharWidth: Integer;
    FCharPositions: array [TGroupMode] of array of Integer;
    FSelectionPositions: array [TGroupMode] of array of Integer;
    FOldOnFontChange: TNotifyEvent;
    FMousePressedHitInfo: TMouseHitInfo;
    FSelStart, FSelEnd: TSelectPoint;
    FEditPosData: TEditPosData;
    FSelectColor, FActiveCaretColor, FInactiveSelectColor: TColorRef;
    FAddressView: TAddressView;
    FBytesInRow: Integer;
    FArrowColor: TColor;
    FSelectedArrowColor: TColor;
    FJmpMarkColor: TColor;
    FPaintedJmpLinesCount: Integer;
    FJmpData: TList<TJmpData>;
    FDrawIncomingJmp: Boolean;
    FBreakPoints: TList<TBreakPoint>;
    FBookMarks: array [TBookMark] of ULONG_PTR;
    FPreviosJmp: TList<Integer>;
    FJmpToEvent: TJmpToEvent;
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetAddressMode(const Value: TAddressMode);
    procedure SetAddressView(const Value: TAddressView);
    procedure SetBytesInGroup(const Value: Integer);
    procedure SetSeparateGroupByColor(const Value: Boolean);
    procedure SetBytesInColorGroup(const Value: Integer);
    procedure SetBytesInRow(const Value: Integer);
    procedure SetDrawIncomingJmp(const Value: Boolean);
    function LineVisible(LineIndex: Integer): Boolean;
    procedure SelectLine(LineIndex: Integer);
    function AddressToLineIndex(Value: ULONG_PTR): Integer;
    function GetHitInfo(XPos, YPos: Integer): TMouseHitInfo;
    function GetSelectData(LineIndex: Integer): TSelectData;
    procedure RebuildDataMap;
    function GetLeftNCWidth: Integer;
    function GetBookMark(AIndex: TBookMark): ULONG_PTR;
    procedure SetBookMark(AIndex: TBookMark; const Value: ULONG_PTR);
  protected
    // перекрытые методы
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    // внутренние события
    procedure FontChange(Sender: TObject);
    procedure DoChange(UpdateSelected: Boolean = True);
    procedure DoJmpTo(LineIndex: Integer; AJmpState: TJmpState);
  protected
    // функции получения текстовых данных для отрисовки и для копирования в буфер
    function GetAddrPartStr(LineIndex: Integer): string;
    function RawToHex(LineIndex: Integer): string;
    function RawToString(LineIndex: Integer): string;
  protected
    // отрисовка
    procedure CreateCaretTimer;
    procedure DestroyCaretTimer;
    procedure UpdateCaretTimer;
    // отрисовка линий
    procedure DrawBackground(StartLine, EndLine: Integer);
    procedure DrawCodeLine(LineIndex: Integer; var Offset: TPoint);
    procedure DrawHeader(Offset: TPoint);
    procedure DrawLines(StartLine, EndLine: Integer; var Offset: TPoint);
    procedure DrawRawExLine(LineIndex: Integer; var Offset: TPoint);
    procedure DrawRawLine(LineIndex: Integer; var Offset: TPoint);
    procedure DrawSeparator(LineIndex: Integer; var Offset: TPoint);
    procedure DrawLineSeparator(var Offset: TPoint);
    procedure DrawCheck(LineIndex: Integer;
      var Offset: TPoint; DrawOnlySelectedArrow: Boolean;
      DrawRadio: Boolean);
    // отрисовка элементов
    procedure DrawArrow(ArrowPoint: TPoint; Direction: TArrowDirection);
    procedure DrawAlignedTextPart(const Text: string; ARect: TRect);
    procedure DrawAddressPart(LineIndex: Integer; var Offset: TPoint);
    procedure DrawArrowPart(LineIndex: Integer; var Offset: TPoint;
      DrawOnlySelectedArrow: Boolean);
    procedure DrawCheckPart(LineIndex: Integer; var Offset: TPoint);
    procedure DrawRadioPart(LineIndex: Integer; var Offset: TPoint);
    procedure DrawCommentPart(LineIndex: Integer; var Offset: TPoint);
    procedure DrawDataPart(LineIndex: Integer; var Offset: TPoint);
    procedure DrawDescriptionPart(LineIndex: Integer; var Offset: TPoint);
    procedure DrawHexPart(LineIndex: Integer; var Offset: TPoint);
    function DrawJmpLine(LineFrom, LineTo: Integer;
      DrawOnlySelectedArrow, SecondDraw: Boolean): Boolean;
    procedure DrawGradientRect(ARect: TRect);
    // отрисовка стрелок
    procedure DrawArrows(StartLine, EndLine: Integer; Offset: TPoint);
    procedure DrawJmpLines(StartLine, EndLine: Integer; Offset: TPoint);

    // отрискова закладок
    procedure DrawWorkSpacePart(StartLine, EndLine: Integer; Offset: TPoint);

    procedure DrawSelectedBackround(var Offset: TPoint;  Column: TColumnType;
      SelStart, SelEnd: Integer; AGroupMode: TGroupMode);
    procedure DrawEditMark(var Offset: TPoint);

    procedure DrawTextBlock(var Offset: TPoint; Column: TColumnType;
      const DrawString: string; AGroupMode: TGroupMode);

    function MakeDrawRect(LeftOffset, TopOffset: Integer; Column: TColumnType): TRect; overload;
    function MakeDrawRect(LeftOffset, TopOffset, ColumnWidth: Integer): TRect; overload;
    function MakeSelectRect(LeftOffset, TopOffset, SelectWidth: Integer): TRect;
    function GetLineJmpMarkRect(LineIndex: Integer; Offset: TPoint): TRect;
  protected
    // валидаторы
    function CharVisible(Value: Byte): Boolean;
    function GetPageHeight: Integer;
    function GetVisibleLineDiapason: TVisibleLineDiapason;
    function GetVisibleLinesCount: Integer;
    function GetEditChar: Char;
    function GetLineOffset(LineIndex: Integer): Integer;
    procedure UpdateDataMap;
    procedure UpdateTextBoundary;
    procedure UpdateWidthHeight;
    procedure UpdateSelectedPosition(Value: TSelectPoint);
    procedure UpdateScrollPos;
  protected
    // оффсеты выделения
    function DblSize(Value: Integer): Integer; inline;
    function DblSizeInc(Value: Integer): Integer; inline;
    function DblSizeDec(Value: Integer): Integer; inline;
    function Min(A, B: Integer): Integer; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopySelected(CopyStyle: TCopyStyle);
    procedure DefaultHandler(var Message); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearDataMap;
    function CurrentVisibleLine: Integer;
    property DataMap: TDataMap read FDataMap;
    procedure FocusOnLine(LineIndex: Integer);
    procedure FocusOnAddress(Address: ULONG_PTR);
    procedure SetDataStream(Value: TStream; StartAddress: ULONG_PTR);
    property Bookmark[AIndex: TBookMark]: ULONG_PTR read GetBookMark write SetBookMark;
  protected
    property AddressMode: TAddressMode read FAddressMode write SetAddressMode;
    property AddressView: TAddressView read FAddressView write SetAddressView;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property BytesInColorGroup: Integer read FBytesInColorGroup write SetBytesInColorGroup default 4;
    property BytesInGroup: Integer read FBytesInGroup write SetBytesInGroup default 8;
    property BytesInRow: Integer read FBytesInRow write SetBytesInRow default 16;
    property DrawIncomingJmp: Boolean read FDrawIncomingJmp write SetDrawIncomingJmp default True;
    property Header: THeader read FHeader;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default TScrollStyle.ssNone;
    property SelectedArrowColor: TColor read FSelectedArrowColor write FSelectedArrowColor;
    property SeparateGroupByColor: Boolean read FSeparateGroupByColor write SetSeparateGroupByColor;
    property OnJmpTo: TJmpToEvent read FJmpToEvent write FJmpToEvent;
  end;

  TFWHexView = class(TFWCustomHexView)
  published
    property AddressMode;
    property AddressView;
    property Align;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BytesInColorGroup;
    property BytesInGroup;
    property BytesInRow;
    property Color;
    property Constraints;
    property Ctl3D;
    property Cursor default crIBeam;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawIncomingJmp;
    property Enabled;
    property Font;
    property Header;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property SelectedArrowColor;
    property SeparateGroupByColor;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnJmpTo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

const
  // дабы снаружи не перекрыли эту переменную
  EmptyStr = '';

{ TDataMap }

function TDataMap.AddSeparator(const Description: string): Integer;
begin
  Result := AddSeparator(CurrentAddr, Description);
end;

function TDataMap.AddCheck(ByteIndex: Byte;
  const Description, Comment: string; Checked: Boolean): Integer;
begin
  Result :=
    AddCheck(CurrentAddr, ByteIndex, Description, Comment, Checked);
end;

function TDataMap.AddAsm(DataLength: Byte; const Description, Comment: string;
  JmpToAddr: ULONG_PTR; LinkStart, LinkLength: Integer;
  RawColor: TColor): Integer;
begin
  Result := AddAsm(CurrentAddr, DataLength, Description, Comment,
    JmpToAddr, LinkStart, LinkLength, RawColor);
end;

function TDataMap.AddAsm(Address: ULONG_PTR; DataLength: Byte;
  const Description, Comment: string; JmpToAddr: ULONG_PTR; LinkStart,
  LinkLength: Integer; RawColor: TColor): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsAsm;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.RawLength := DataLength;
  LineData.RawColor := RawColor;
  LineData.JmpToAddr := JmpToAddr;
  LineData.LinkStart := LinkStart;
  LineData.LinkLength := LinkLength;
  CurrentAddr := Address + DataLength;
  Result := FData.Add(LineData);
end;

function TDataMap.AddAsm(DataLength: Byte; const Description: string;
  RawColor: TColor): Integer;
begin
  Result := AddAsm(CurrentAddr, DataLength, Description, EmptyStr, 0, 0, 0, RawColor);
end;

function TDataMap.AddAsm(DataLength: Byte; const Description, Comment: string;
  RawColor: TColor): Integer;
begin
  Result := AddAsm(CurrentAddr, DataLength, Description, Comment, 0, 0, 0, RawColor);
end;

function TDataMap.AddCheck(Address: ULONG_PTR; CharIndex: Byte;
  const Description, Comment: string; Checked: Boolean): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsCheck;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.CharIndex := CharIndex;
  LineData.Checked := Checked;
  Result := FData.Add(LineData);
end;

function TDataMap.AddCode(const Description: string): Integer;
begin
  Result := AddCode(CurrentAddr, Description);
end;

function TDataMap.AddCode(Address: ULONG_PTR;
  const Description: string): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsCode;
  LineData.Address := Address;
  LineData.Description := Description;
  CurrentAddr := Address;
  Result := FData.Add(LineData);
end;

function TDataMap.AddExDescription(DataLength: Byte;
  const Description: string; RawColor: TColor): Integer;
begin
  Result := AddExDescription(CurrentAddr,
    DataLength, Description, EmptyStr, 0, 0, 0, RawColor);
end;

function TDataMap.AddExDescription(DataLength: Byte; const Description,
  Comment: string; JmpToAddr: ULONG_PTR;
  LinkStart, LinkLength: Integer; RawColor: TColor): Integer;
begin
  Result := AddExDescription(CurrentAddr,
    DataLength, Description, Comment, JmpToAddr, LinkStart, LinkLength, RawColor);
end;

function TDataMap.AddExDescription(DataLength: Byte; const Description,
  Comment: string; RawColor: TColor): Integer;
begin
  Result := AddExDescription(
    CurrentAddr, DataLength, Description, Comment, 0, 0, 0, RawColor);
end;

function TDataMap.AddExDescription(Address: ULONG_PTR; DataLength: Byte;
  const Description, Comment: string; JmpToAddr: ULONG_PTR;
  LinkStart, LinkLength: Integer; RawColor: TColor): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsRawWithExDescription;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.RawLength := DataLength;
  LineData.RawColor := RawColor;
  LineData.JmpToAddr := JmpToAddr;
  LineData.LinkStart := LinkStart;
  LineData.LinkLength := LinkLength;
  CurrentAddr := Address + DataLength;
  Result := FData.Add(LineData);
end;

function TDataMap.AddLine(Address: ULONG_PTR): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsLine;
  LineData.Address := Address;
  CurrentAddr := Address;
  Result := FData.Add(LineData);
end;

function TDataMap.AddLine: Integer;
begin
  Result := AddLine(CurrentAddr);
end;

function TDataMap.AddNone(Address: ULONG_PTR): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsNone;
  LineData.Address := Address;
  CurrentAddr := Address;
  Result := FData.Add(LineData);
end;

function TDataMap.AddNone: Integer;
begin
  Result := AddNone(CurrentAddr);
end;

function TDataMap.AddRadio(ByteIndex: Byte; const Description, Comment: string;
  Checked: Boolean): Integer;
begin
  Result :=
    AddRadio(CurrentAddr, ByteIndex, Description, Comment, Checked);
end;

function TDataMap.AddRadio(Address: ULONG_PTR; CharIndex: Byte;
  const Description, Comment: string; Checked: Boolean): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsRadio;
  LineData.Address := Address;
  LineData.Description := Description;
  LineData.Comment := Comment;
  LineData.CharIndex := CharIndex;
  LineData.Checked := Checked;
  Result := FData.Add(LineData);
end;

function TDataMap.AddSeparator(Address: ULONG_PTR;
  const Description: string): Integer;
var
  LineData: TMapLine;
begin
  ZeroMemory(@LineData, SizeOf(TMapLine));
  LineData.Style := lsSeparator;
  LineData.Address := Address;
  LineData.Description := Description;
  CurrentAddr := Address;
  Result := FData.Add(LineData);
end;

procedure TDataMap.Assign(Value: TDataMap);
begin
  FData.Clear;
  FData.AddRange(FData.ToArray);
  RebuildDataMap;
end;

procedure TDataMap.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDataMap.Clear;
begin
  FData.Clear;
  FCurrentAddr := 0;
  RebuildDataMap;
end;

constructor TDataMap.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  FData := TList<TMapLine>.Create;
  FData.OnNotify := DataChange;
end;

procedure TDataMap.DataChange(Sender: TObject; const Item: TMapLine;
  Action: TCollectionNotification);
begin
  RebuildDataMap;
end;

destructor TDataMap.Destroy;
begin
  FData.OnNotify := nil;
  FData.Free;
  inherited;
end;

procedure TDataMap.EndUpdate;
begin
  Dec(FUpdateCount);
  RebuildDataMap;
end;

procedure TDataMap.RebuildDataMap;
begin
  if FUpdateCount = 0 then
    FOwner.RebuildDataMap;
end;

{ TFWCustomHexView.TSelectPoint }

function TFWCustomHexView.TSelectPoint.InvalidLine: Boolean;
begin
  Result := LineIndex < 0;
end;

function TFWCustomHexView.TSelectPoint.ValidSelectedByte: Boolean;
begin
  Result := (LineIndex >= 0) and (ByteIndex >= 0);
end;

procedure TFWCustomHexView.TSelectPoint.Erase;
begin
  LineIndex := -1;
  ByteIndex := -1;
  Column := ctNone;
  OnHeader := False;
end;

class operator TFWCustomHexView.TSelectPoint.LessThan(A,
  B: TSelectPoint): Boolean;
begin
  Result := False;
  if A.OnHeader <> B.OnHeader then
    Exit(A.OnHeader);
  if (A.LineIndex < B.LineIndex) then
    Exit(True);
  if A.LineIndex = B.LineIndex then
    Result := A.ByteIndex < B.ByteIndex;
end;

class operator TFWCustomHexView.TSelectPoint.NotEqual(A,
  B: TSelectPoint): Boolean;
begin
  Result :=
    (A.OnHeader <> B.OnHeader) or
    (A.LineIndex <> B.LineIndex) or
    (A.ByteIndex <> B.ByteIndex) or
    (A.Column <> B.Column);
end;

{ TFWCustomHexView.TMouseHitInfo }

procedure TFWCustomHexView.TMouseHitInfo.Erase;
begin
  XPos := 0;
  YPos := 0;
  SelectPoint.Erase;
  OnSplitter := False;
  OnJmpMark := False;
  ColumnWidth := 0;
end;

{ TFWCustomHexView.THeader }

constructor TFWCustomHexView.THeader.Create(AOwner: TFWCustomHexView);
begin
  FOwner := AOwner;
  FVisible := True;
  FColumnCaption[ctJmpLine] := 'Jump Lines';
  FColumnCaption[ctAddress] := 'Address';
  FColumnCaption[ctDescription] := 'Description';
  FColumnCaption[ctComment] := 'Comment';
end;

function TFWCustomHexView.THeader.GetColCaption(Value: TColumnType): string;
begin
  Result := FColumnCaption[Value];
end;

function TFWCustomHexView.THeader.GetColWidth(Value: TColumnType): Integer;
begin
  Result := FColumnWidth[Value];
end;

procedure TFWCustomHexView.THeader.SetColCaption(Value: TColumnType;
  const NewCaption: string);
begin
  if ColumnCaption[Value] <> NewCaption then
  begin
    FColumnCaption[Value] := NewCaption;
    FOwner.DoChange(False);
  end;
end;

procedure TFWCustomHexView.THeader.SetColumns(const Value: TFWHexViewColumnTypes);
begin
  if Columns <> Value then
  begin
    FColumns := Value;
    FOwner.DoChange(False);
  end;
end;

procedure TFWCustomHexView.THeader.SetColWidth(Value: TColumnType; AWidth: Integer);
begin
  if AWidth < MIN_COLUNN_WIDTH then
    AWidth := MIN_COLUNN_WIDTH;
  if ColumnWidth[Value] <> AWidth then
  begin
    FColumnWidth[Value] := AWidth;
    FOwner.DoChange(False);
  end;
end;

procedure TFWCustomHexView.THeader.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then
  begin
    FVisible := Value;
    FOwner.DoChange(False);
  end;
end;

{ TFWCustomHexView }

function TFWCustomHexView.AddressToLineIndex(Value: ULONG_PTR): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Value = 0 then Exit;
  for I := 0 to FRawData.Count - 1 do
  begin
    if FRawData.List[I].Style in [lsRaw..lsAsm] then
    begin
      if FRawData.List[I].Address = Value then
        Exit(I);
      if FRawData.List[I].Address > Value then
        Break;
      Result := I;
    end;
  end;
end;

procedure TFWCustomHexView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TFWCustomHexView.CharVisible(Value: Byte): Boolean;
begin
  Result := Value in [$21..$FF];
end;

procedure TFWCustomHexView.ClearDataMap;
begin
  FDataMap.Clear;
  FSelStart.Erase;
  FSelEnd.Erase;
  FEditPosData.LineIndex := -1;
  SetDataStream(nil, 0);
  RebuildDataMap;
end;

procedure TFWCustomHexView.CopySelected(CopyStyle: TCopyStyle);

  function AlignLine(const Value: string; CharInLine: Integer): string;
  begin
    Result := Value;
    if Length(Result) < CharInLine then
      Result := Result +
        StringOfChar(Space, CharInLine - Length(Result));
  end;

const
  ColumnSeparator = ' | ';
  UncopiedData = '_';
var
  CopyBuff, LineData, FormatedLineData: string;
  I, A, StartLine, EndLine, CharInRow, CurrentCharInRow: Integer;
  SelData: TSelectData;
begin
  if FSelStart.InvalidLine then Exit;
  if FSelEnd.InvalidLine then Exit;
  if FSelStart.LineIndex > FSelEnd.LineIndex then
  begin
    StartLine := FSelEnd.LineIndex;
    EndLine := FSelStart.LineIndex;
  end
  else
  begin
    StartLine := FSelStart.LineIndex;
    EndLine := FSelEnd.LineIndex;
  end;

  CopyBuff := '';
  if CopyStyle <> csAllData then
  begin
    for I := StartLine to EndLine do
    begin
      if FRawData.List[I].Style in [lsRaw..lsAsm] then
      begin
        LineData := RawToHex(I);
        SelData := GetSelectData(I);
        SelData.FirstSelectIndex := SelData.FirstSelectIndex shl 1;
        SelData.SecondSelectIndex := SelData.SecondSelectIndex shl 1;
        case SelData.SelectStyle of
          ssLeftSelected:
            LineData := Copy(LineData, 1, SelData.FirstSelectIndex + 2);
          ssCenterSelected:
            LineData := Copy(LineData, SelData.FirstSelectIndex + 1,
              SelData.SecondSelectIndex - SelData.FirstSelectIndex + 2);
          ssRightSelected:
            LineData := Copy(LineData, SelData.FirstSelectIndex + 1,
              Length(LineData));
        end;
      end;
      CopyBuff := CopyBuff + LineData;
    end;

    if Trim(CopyBuff) = '' then Exit;

    if CopyStyle = csPascal then
    begin
      LineData := CopyBuff;
      CopyBuff := 'const' + sLineBreak + '  Buffer: array[0..' +
        IntToStr((Length(LineData) shr 1) - 1) + '] of Byte = (' + sLineBreak + '    ';
      I := 1;
      A := 0;
      repeat
        CopyBuff := CopyBuff + '$' + LineData[I] + LineData[I + 1];
        Inc(I, 2);
        if I <= Length(LineData) - 1 then
          CopyBuff := CopyBuff + ', ';
        Inc(A);
        if A = 14 then
        begin
          A := 0;
          CopyBuff := CopyBuff + sLineBreak + '    ';
        end;
      until I > Length(LineData) - 1;
      CopyBuff := CopyBuff + ');';
    end;

    if CopyStyle = csCpp then
    begin
      LineData := CopyBuff;
      CopyBuff := 'int buffer[' +
        IntToStr((Length(LineData) shr 1)) + '] = {' + sLineBreak + '    ';
      I := 1;
      A := 0;
      repeat
        CopyBuff := CopyBuff + '0x' + LineData[I] + LineData[I + 1];
        Inc(I, 2);
        if I <= Length(LineData) - 1 then
          CopyBuff := CopyBuff + ', ';
        Inc(A);
        if A = 14 then
        begin
          A := 0;
          CopyBuff := CopyBuff + sLineBreak + '    ';
        end;
      until I > Length(LineData) - 1;
      CopyBuff := CopyBuff + '};';
    end;

    Clipboard.AsText := CopyBuff;
    Exit;
  end;

  CharInRow := BytesInRow shl 1;
  for I := StartLine to EndLine do
  begin
    case FRawData.List[I].Style of
      lsNone:
        CopyBuff := CopyBuff + sLineBreak;
      lsLine:
        CopyBuff := CopyBuff + StringOfChar(UncopiedData, 120) + sLineBreak;
      lsSeparator, lsCode:
        CopyBuff := CopyBuff + FRawData.List[I].Description + sLineBreak;
      lsRaw..lsAsm:
      begin
        LineData := RawToHex(I);
        SelData := GetSelectData(I);
        SelData.FirstSelectIndex := SelData.FirstSelectIndex shl 1;
        SelData.SecondSelectIndex := SelData.SecondSelectIndex shl 1;
        CurrentCharInRow := FRawData.List[I].RawLength shl 1;
        case SelData.SelectStyle of
          ssLeftSelected:
            LineData := Copy(LineData, 1, SelData.FirstSelectIndex + 2) +
              StringOfChar(UncopiedData, CurrentCharInRow - SelData.FirstSelectIndex - 2);
          ssCenterSelected:
            LineData :=
              StringOfChar(UncopiedData, SelData.FirstSelectIndex) +
              Copy(LineData, SelData.FirstSelectIndex + 1,
              SelData.SecondSelectIndex - SelData.FirstSelectIndex + 2) +
              StringOfChar(UncopiedData, CurrentCharInRow - SelData.SecondSelectIndex - 2);
          ssRightSelected:
            LineData := StringOfChar(UncopiedData, SelData.FirstSelectIndex) +
              Copy(LineData, SelData.FirstSelectIndex + 1, Length(LineData));
        end;
        LineData := AlignLine(LineData, CharInRow);

        FormatedLineData := '';
        for A := 1 to Length(LineData) do
        begin
          FormatedLineData := FormatedLineData + LineData[A];
          if (A > 1) and (A mod 2 = 0) then
            FormatedLineData := FormatedLineData + Space;
        end;

        FormatedLineData := GetAddrPartStr(I) + ColumnSeparator + FormatedLineData +
          ColumnSeparator;

        case FRawData.List[I].Style of
          lsRaw:
            LineData := RawToString(I);
        else
          LineData := FRawData.List[I].Description;
        end;
        LineData := AlignLine(LineData, 32);

        CopyBuff := CopyBuff + FormatedLineData + LineData +
          ColumnSeparator + FRawData.List[I].Comment + sLineBreak;
      end;
    end;
  end;
  Clipboard.AsText := CopyBuff;
end;

constructor TFWCustomHexView.Create(AOwner: TComponent);
begin
  inherited;
  Width := 185;
  Height := 89;
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csNeedsBorderPaint] -
    [csSetCaption, csParentBackground];
  Ctl3D := True;
  TabStop := True;
  FBorderStyle := bsSingle;

  Font.Charset := RUSSIAN_CHARSET;
  Font.Color := clWindowText;
  Font.Height := -11;
  Font.Name := 'Courier New';
  FOldOnFontChange := Font.OnChange;
  Font.OnChange := FontChange;
  Canvas.Font := Font;

  FAddressMode := am32bit;
  FAddressView := avHex;
  FBytesInGroup := 8;
  FBytesInColorGroup := 4;
  FBytesInRow := 16;
  FSeparateGroupByColor := True;

  FRawData := TList<TLineData>.Create;
  FHeader := THeader.Create(Self);
  FDataMap := TDataMap.Create(Self);

  FHeader.Columns := [ctWorkSpace..ctComment];
  FHeader.ColumnWidth[ctWorkSpace] := 34;
  FHeader.ColumnWidth[ctJmpLine] := 90;
  FHeader.ColumnWidth[ctAddress] := 130;
  FHeader.ColumnWidth[ctOpcode] := 355;
  FHeader.ColumnWidth[ctDescription] := 250;
  FHeader.ColumnWidth[ctComment] := 300;

  FSelStart.Erase;
  FSelEnd.Erase;
  FEditPosData.LineIndex := -1;

  FActiveCaretColor := RGB(31, 31, 255);
  FInactiveSelectColor := RGB(239, 239, 239);
  FSelectColor := RGB(224, 224, 255);
  FArrowColor := RGB(200, 200, 200);
  FSelectedArrowColor := clBlack;
  FJmpMarkColor := RGB(73, 113, 184);

  FJmpData := TList<TJmpData>.Create(
    TComparer<TJmpData>.Construct(
      function (const A, B: TJmpData): Integer
      begin
        Result := LONG_PTR(A.JmpTo) - LONG_PTR(B.JmpTo);
      end)
  );

  FDrawIncomingJmp := True;

  FBreakPoints := TList<TBreakPoint>.Create;
  FPreviosJmp := TList<Integer>.Create;
end;

procedure TFWCustomHexView.CreateCaretTimer;
begin
  SetTimer(Handle, 0, GetCaretBlinkTime, nil);
end;

procedure TFWCustomHexView.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited;
  with Params do
  begin
    Style := ((Style or WS_CLIPCHILDREN or WS_TABSTOP) or
      BorderStyles[FBorderStyle]) or LBS_OWNERDRAWFIXED or LBS_MULTICOLUMN or
      ScrollBar[ScrollBars];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TFWCustomHexView.CreateWnd;
begin
  inherited;
  UpdateWidthHeight;
  UpdateTextBoundary;
  UpdateScrollPos;
end;

function TFWCustomHexView.CurrentVisibleLine: Integer;
begin
  Result := -FScrollOffset.Y div FLineHeight;
end;

function TFWCustomHexView.DblSize(Value: Integer): Integer;
begin
  Result := Value + Value;
end;

function TFWCustomHexView.DblSizeDec(Value: Integer): Integer;
begin
  if Value = 0 then
    Result := 0
  else
    Result := Value + Value - 1;
end;

function TFWCustomHexView.DblSizeInc(Value: Integer): Integer;
begin
  Result := Value + Value + 1;
end;

procedure TFWCustomHexView.DefaultHandler(var Message);

  function WrongLineStyle(LineIndex: Integer; CheckAllColumn: Boolean): Boolean;
  begin
    Result := (LineIndex >= 0) and (LineIndex < FRawData.Count);
    if Result then
      if CheckAllColumn or (FEditPosData.Column = ctOpcode) then
        Result := FRawData.List[LineIndex].Style in [lsNone..lsLine, lsCheck, lsRadio]
      else
        Result := FRawData.List[LineIndex].Style <> lsRaw;
  end;

  function NeedChangeCulumn(LineIndex: Integer): Boolean;
  begin
    Result := FRawData.List[LineIndex].Style = lsRawWithExDescription;
  end;

  function GetPreviosLineIndex(LineIndex: Integer; CheckAllColumn: Boolean): Integer;
  begin
    Result := LineIndex;
    Dec(Result);
    while WrongLineStyle(Result, CheckAllColumn) do
      Dec(Result);
    if Result < 0 then
      Result := LineIndex;
  end;

  function GetNextLineIndex(LineIndex: Integer; CheckAllColumn: Boolean): Integer;
  begin
    Result := LineIndex;
    Inc(Result);
    while WrongLineStyle(Result, CheckAllColumn) do
      Inc(Result);
    if Result > FRawData.Count - 1 then
      Result := LineIndex;
  end;

  procedure UpdateOpcodeCharIndex(NewLineIndex: Integer);
  var
    ValidLength: Integer;
  begin
    if NewLineIndex <> FEditPosData.LineIndex then
    begin
      FEditPosData.LineIndex := NewLineIndex;
      if FEditPosData.Column = ctOpcode then
        ValidLength := DblSizeDec(FRawData.List[FEditPosData.LineIndex].RawLength)
      else
        ValidLength := FRawData.List[FEditPosData.LineIndex].RawLength - 1;
      FEditPosData.OpcodeCharIndex := Min(FEditPosData.OpcodeCharIndex, ValidLength);
    end;
  end;

var
  NewLineIndex, ValidLength: Integer;
begin
  inherited;
  case TMessage(Message).Msg of
    WM_SETFOCUS, WM_KILLFOCUS:
      Invalidate;
    WM_TIMER:
    begin
      FEditPosData.Showed := not FEditPosData.Showed;
      Repaint;
    end;
    WM_LBUTTONDBLCLK:
    begin
      FSelStart.ByteIndex := -1;
      FSelEnd.ByteIndex := -1;
      Invalidate;
    end;
    WM_KEYDOWN:
    begin
      UpdateCaretTimer;
      case TWMKey(Message).CharCode of
        VK_LEFT:
        begin
          if FRawData.List[FEditPosData.LineIndex].Style = lsAsm then
            FEditPosData.OpcodeCharIndex := 0;
          Dec(FEditPosData.OpcodeCharIndex);
          if FEditPosData.OpcodeCharIndex < 0 then
          begin
            NewLineIndex := GetPreviosLineIndex(FEditPosData.LineIndex, False);
            if NewLineIndex <> FEditPosData.LineIndex then
            begin
              FEditPosData.LineIndex := NewLineIndex;
              if FEditPosData.Column = ctOpcode then
                FEditPosData.OpcodeCharIndex :=
                  DblSizeDec(FRawData.List[FEditPosData.LineIndex].RawLength)
              else
              begin
                if NeedChangeCulumn(NewLineIndex) then
                begin
                  FEditPosData.Column := ctOpcode;
                  FEditPosData.OpcodeCharIndex :=
                    DblSizeDec(FRawData.List[FEditPosData.LineIndex].RawLength)
                end
                else
                  FEditPosData.DescriptionByteIndex :=
                    FRawData.List[FEditPosData.LineIndex].RawLength - 1;
              end;
            end
            else
              FEditPosData.OpcodeCharIndex := 0;
          end;
        end;
        VK_RIGHT:
        begin
          if FRawData.List[FEditPosData.LineIndex].Style = lsAsm then
            FEditPosData.OpcodeCharIndex := DblSize(FBytesInRow);
          Inc(FEditPosData.OpcodeCharIndex);
          if FEditPosData.Column = ctOpcode then
            ValidLength := DblSizeDec(FRawData.List[FEditPosData.LineIndex].RawLength)
          else
            ValidLength := FRawData.List[FEditPosData.LineIndex].RawLength - 1;
          if FEditPosData.OpcodeCharIndex > ValidLength then
          begin
            NewLineIndex := GetNextLineIndex(FEditPosData.LineIndex, False);
            if NewLineIndex > FEditPosData.LineIndex then
            begin
              FEditPosData.LineIndex := NewLineIndex;
              FEditPosData.OpcodeCharIndex := 0;
            end
            else
              FEditPosData.OpcodeCharIndex := ValidLength;
          end;
        end;
        VK_UP:
          UpdateOpcodeCharIndex(GetPreviosLineIndex(FEditPosData.LineIndex, False));
        VK_DOWN:
          UpdateOpcodeCharIndex(GetNextLineIndex(FEditPosData.LineIndex, False));
        VK_NEXT:
        begin
          NewLineIndex := GetVisibleLineDiapason.EndLine;
          if NewLineIndex >= FRawData.Count then
            NewLineIndex := FRawData.Count - 1;
          if WrongLineStyle(NewLineIndex, True) then
            NewLineIndex := GetNextLineIndex(NewLineIndex, True);
          if WrongLineStyle(NewLineIndex, True) then
            NewLineIndex := GetPreviosLineIndex(NewLineIndex, True);
          UpdateOpcodeCharIndex(NewLineIndex);
          FocusOnAddress(FRawData.List[NewLineIndex].Address);
        end;
        VK_PRIOR:
        begin
          NewLineIndex := CurrentVisibleLine - GetVisibleLinesCount;
          if NewLineIndex < 0 then
            NewLineIndex := 0;
          if WrongLineStyle(NewLineIndex, True) then
            NewLineIndex := GetPreviosLineIndex(NewLineIndex, True);
          if WrongLineStyle(NewLineIndex, True) then
            NewLineIndex := GetNextLineIndex(NewLineIndex, True);
          UpdateOpcodeCharIndex(NewLineIndex);
          FocusOnAddress(FRawData.List[NewLineIndex].Address);
        end;
        VK_HOME:
        begin
          NewLineIndex := 0;
          if WrongLineStyle(NewLineIndex, True) then
            NewLineIndex := GetNextLineIndex(NewLineIndex, True);
          UpdateOpcodeCharIndex(NewLineIndex);
          FocusOnAddress(FRawData.List[NewLineIndex].Address);
        end;
        VK_END:
        begin
          NewLineIndex := FRawData.Count - 1;
          if WrongLineStyle(NewLineIndex, True) then
            NewLineIndex := GetPreviosLineIndex(NewLineIndex, True);
          UpdateOpcodeCharIndex(NewLineIndex);
          FocusOnAddress(FRawData.List[NewLineIndex].Address);
        end;
        VK_ESCAPE:
        begin
          DoJmpTo(0, jsPopFromUndo);
          Exit;
        end;
      else
        Exit;
      end;
      if not LineVisible(FEditPosData.LineIndex) then
        FocusOnLine(FEditPosData.LineIndex);
      if FEditPosData.LineIndex >= 0 then
        if FRawData.List[FEditPosData.LineIndex].Style = lsAsm then
          SelectLine(FEditPosData.LineIndex);
      Invalidate;
    end;
  end;
end;

destructor TFWCustomHexView.Destroy;
begin
  FPreviosJmp.Free;
  FBreakPoints.Free;
  FJmpData.Free;
  FDataMap.Free;
  FHeader.Free;
  FRawData.Free;
  inherited;
end;

procedure TFWCustomHexView.DestroyCaretTimer;
begin
  KillTimer(Handle, 0);
end;

procedure TFWCustomHexView.DoChange(UpdateSelected: Boolean);
begin
  if (csCreating in ControlState) then Exit;
  if Parent = nil then Exit;  
  if FUpdateCount = 0 then
  begin
    if csNeedUpdateDataMap in FStates then
      UpdateDataMap;
    UpdateWidthHeight;
    UpdateTextBoundary;
    if UpdateSelected then
      UpdateSelectedPosition(FSelStart);
    UpdateScrollPos;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.DoJmpTo(LineIndex: Integer; AJmpState: TJmpState);
var
  Handled: Boolean;
  JmpAddr: ULONG_PTR;
  NewLineIndex: Integer;
begin
  Handled := False;
  if AJmpState = jsPushToUndo then
    JmpAddr := FRawData.List[LineIndex].JmpToAddr
  else
    JmpAddr := 0;
  if Assigned(FJmpToEvent) then
    FJmpToEvent(Self, JmpAddr, AJmpState, Handled);
  if Handled then Exit;
  if AJmpState = jsPushToUndo then
  begin
    FPreviosJmp.Add(LineIndex);
    FocusOnAddress(FRawData.List[LineIndex].JmpToAddr);
  end
  else
  begin
    if FPreviosJmp.Count = 0 then Exit;
    NewLineIndex := FPreviosJmp[FPreviosJmp.Count - 1];
    FPreviosJmp.Delete(FPreviosJmp.Count - 1);
    FocusOnAddress(FRawData.List[NewLineIndex].Address);
  end;
end;

procedure TFWCustomHexView.DrawAddressPart(LineIndex: Integer; var Offset: TPoint);
var
  R: TRect;
  DataString: string;
begin
  Canvas.Font.Color := clWindowText;
  Canvas.Brush.Style := bsSolid;
  case GetSelectData(LineIndex).SelectStyle of
    ssNone:
      Canvas.Brush.Color := clWindow;
    ssAllSelected:
      Canvas.Brush.Color := FSelectColor;
  else
    Canvas.Brush.Color := FInactiveSelectColor;
  end;

  DataString := GetAddrPartStr(LineIndex);

  R := MakeDrawRect(Offset.X, Offset.Y, ctAddress);
  DrawText(Canvas.Handle, DataString, Length(DataString), R, DT_RIGHT);
  Inc(Offset.X, FHeader.ColumnWidth[ctAddress]);
end;

procedure TFWCustomHexView.DrawAlignedTextPart(const Text: string;
  ARect: TRect);
var
  AlignBuff: array of Integer;
  I: Integer;
begin
  if Text = '' then Exit;
  SetLength(AlignBuff, Length(Text));
  for I := 0 to Length(AlignBuff) - 1 do
    AlignBuff[I] := FCharWidth;
  ExtTextOut(Canvas.Handle, ARect.Left, ARect.Top, ETO_CLIPPED{ or ETO_OPAQUE}, @ARect,
    PChar(@Text[1]), Length(Text), @AlignBuff[0]);
end;

procedure TFWCustomHexView.DrawBackground(StartLine, EndLine: Integer);
var
  A: Integer;
  I: TColumnType;
  R: TRect;
  LeftOffset, OpcodeOffset,
  RightOffset, CharIndex, CurrentByteInGroup: Integer;
  GrayColor: Boolean;
  LineCount: Integer;
  SavedDC: HDC;
begin
  Canvas.Brush.Color := clWindow;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := clGray;
  Canvas.FillRect(GetClientRect);
  LeftOffset := FScrollOffset.X;
  OpcodeOffset := 0;
  for I := Low(TColumnType) to High(TColumnType) do
  begin
    if not (I in FHeader.Columns) then Continue;
    if I = ctOpcode then
      OpcodeOffset := LeftOffset;
    Inc(LeftOffset, FHeader.ColumnWidth[I]);
    if (csMousePressed in FStates) and (FMousePressedHitInfo.OnSplitter) and
      (FMousePressedHitInfo.SelectPoint.Column = I) then
    begin
      Canvas.Pen.Width := DblSize(TEXT_MARGIN);
      Canvas.Pen.Color := FInactiveSelectColor;
      Canvas.MoveTo(LeftOffset, 0);
      Canvas.LineTo(LeftOffset, Height);
      Canvas.Pen.Width := 1;
    end;
    Canvas.Pen.Color := RGB(140, 140, 140);
    Canvas.MoveTo(LeftOffset, 0);
    Canvas.LineTo(LeftOffset, Height);
  end;

  if ctOpcode in FHeader.Columns then
  begin
    if SeparateGroupByColor then
    begin
      LeftOffset := OpcodeOffset + TEXT_MARGIN;
      RightOffset := LeftOffset;
      SavedDC := SaveDC(Canvas.Handle);
      try
        // чет лениво заморачиваться с правым отсечением по границам колонки
        // отсеку правую часть клипингом
        IntersectClipRect(Canvas.Handle, LeftOffset, 0,
          LeftOffset + FHeader.ColumnWidth[ctOpcode] - DblSize(TEXT_MARGIN), ClientHeight);
        CharIndex := 0;
        CurrentByteInGroup := 0;
        GrayColor := False;
        while CharIndex < DblSizeDec(BytesInRow) do
        begin
          Inc(CurrentByteInGroup);
          Inc(RightOffset, FCharPositions[gmOn][CharIndex]);
          if CurrentByteInGroup = FBytesInColorGroup then
          begin
            CurrentByteInGroup := 0;
            GrayColor := not GrayColor;
            if GrayColor then
              Canvas.Brush.Color := FInactiveSelectColor
            else
              Canvas.Brush.Color := clWindow;

            LineCount := 0;
            if Header.Visible then
              LineCount := 1;

            for A := StartLine to EndLine do
            begin
              if FRawData.List[A].Style = lsRaw then
              begin
                R := Rect(LeftOffset, LineCount * FLineHeight,
                  RightOffset + FCharWidth, LineCount * FLineHeight + FLineHeight);
                Canvas.FillRect(R);
              end;
              Inc(LineCount);
            end;

            Inc(RightOffset, FCharPositions[gmOn][CharIndex + 1]);
            LeftOffset := RightOffset;
          end
          else
          begin
            Inc(RightOffset, FCharPositions[gmOn][CharIndex + 1]);
          end;
          Inc(CharIndex, 2);
        end;
      finally
        RestoreDC(Canvas.Handle, SavedDC);
      end;
    end;
  end;
end;

procedure TFWCustomHexView.DrawCheck(LineIndex: Integer;
  var Offset: TPoint; DrawOnlySelectedArrow: Boolean; DrawRadio: Boolean);
var
  LeftOffset: Integer;
  I: TColumnType;
begin
  LeftOffset := Offset.X;
  try
    for I := ctWorkSpace to High(TColumnType) do
      if I in FHeader.Columns then
        if DrawOnlySelectedArrow then
        begin
          if I = ctOpcode then
            DrawArrowPart(LineIndex, Offset, True)
          else
            Inc(Offset.X, FHeader.ColumnWidth[I]);
        end
        else
        begin
          case I of
            ctWorkSpace, ctJmpLine, ctAddress:
              Inc(Offset.X, FHeader.ColumnWidth[I]);
            ctOpcode:
              DrawArrowPart(LineIndex, Offset, False);
            ctDescription:
              if DrawRadio then
                DrawRadioPart(LineIndex, Offset)
              else
                DrawCheckPart(LineIndex, Offset);
            ctComment:
              DrawCommentPart(LineIndex, Offset);
          end;
        end;
  finally
    Offset.X := LeftOffset;
  end;
  Inc(Offset.Y, FLineHeight);
end;

procedure TFWCustomHexView.DrawCheckPart(LineIndex: Integer;
  var Offset: TPoint);
var
  R, CheckRect: TRect;
  Details: TThemedElementDetails;
begin
  R := MakeDrawRect(Offset.X, Offset.Y, ctDescription);
  CheckRect := Rect(R.Left, R.Top, R.Left + 14, R.Top + 14);
  OffsetRect(CheckRect, 0, (FLineHeight - CheckRect.Height) shr 1);
  if StyleServices.Enabled then
  begin
    if FRawData.List[LineIndex].Checked then
      Details := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal)
    else
      Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    StyleServices.DrawElement(Canvas.Handle, Details, CheckRect);
  end
  else
  begin
    if FRawData.List[LineIndex].Checked then
      DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, DFCS_CHECKED)
    else
      DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, 0);
  end;
  if GetSelectData(LineIndex).SelectStyle <> ssNone then
    Canvas.Brush.Color := FSelectColor
  else
    Canvas.Brush.Color := clWindow;
  R.Left := CheckRect.Right + 4;
  DrawText(Canvas.Handle, PChar(FRawData.List[LineIndex].Description),
    Length(FRawData.List[LineIndex].Description), R, 0);
  Inc(Offset.X, FHeader.ColumnWidth[ctDescription]);
end;

procedure TFWCustomHexView.DrawCodeLine(LineIndex: Integer; var Offset: TPoint);
var
  R: TRect;
  SelData: TSelectData;
  ADescription: string;
begin
  SelData := GetSelectData(LineIndex);
  Canvas.Brush.Style := bsSolid;
  if SelData.SelectStyle = ssNone then
    Canvas.Brush.Color := clWindow
  else
    Canvas.Brush.Color := FSelectColor;
  R := Rect(GetLeftNCWidth, Offset.Y, ClientWidth + 1, Offset.Y + FLineHeight);
  Inc(R.Left, FScrollOffset.X + TEXT_MARGIN);
  Canvas.Font.Color := clGrayText;
  ADescription := FRawData.List[LineIndex].Description;
  DrawText(Canvas.Handle, PChar(ADescription),
    Length(ADescription), R, DT_CALCRECT);
  Canvas.FillRect(R);
  DrawText(Canvas.Handle, PChar(ADescription),
    Length(ADescription), R, DT_LEFT);
  Inc(Offset.Y, FLineHeight);
end;

procedure TFWCustomHexView.DrawCommentPart(LineIndex: Integer; var Offset: TPoint);
var
  R: TRect;
  DataString: string;
begin
  DataString := FRawData.List[LineIndex].Comment;
  if DataString = '' then Exit;

  Canvas.Brush.Style := bsSolid;
  case GetSelectData(LineIndex).SelectStyle of
    ssNone:
      Canvas.Brush.Color := clWindow;
    ssAllSelected:
      Canvas.Brush.Color := FSelectColor;
  else
    Canvas.Brush.Color := FInactiveSelectColor;
  end;

  R := MakeDrawRect(Offset.X, Offset.Y, ctComment);
  DrawAlignedTextPart(DataString, R);
  Inc(Offset.X, FHeader.ColumnWidth[ctComment]);
end;

procedure TFWCustomHexView.DrawDataPart(LineIndex: Integer; var Offset: TPoint);
var
  SelData: TSelectData;
  DrawString: string;
begin
  SelData := GetSelectData(LineIndex);
  case SelData.SelectStyle of
    ssAllSelected:
      DrawSelectedBackround(Offset, ctDescription,
        0, FRawData.List[LineIndex].RawLength - 1, gmOff);
    ssLeftSelected:
      DrawSelectedBackround(Offset, ctDescription,
        0, SelData.FirstSelectIndex, gmOff);
    ssCenterSelected:
      DrawSelectedBackround(Offset, ctDescription,
        SelData.FirstSelectIndex, SelData.SecondSelectIndex, gmOff);
    ssRightSelected:
      DrawSelectedBackround(Offset, ctDescription,
        SelData.FirstSelectIndex, FRawData.List[LineIndex].RawLength - 1, gmOff);
  end;

  DrawString := RawToString(LineIndex);
  DrawTextBlock(Offset, ctDescription, DrawString, gmOff);
end;

procedure TFWCustomHexView.DrawDescriptionPart(LineIndex: Integer;
  var Offset: TPoint);
var
  R: TRect;
  DataString: string;
begin
  Canvas.Brush.Style := bsSolid;

  case GetSelectData(LineIndex).SelectStyle of
    ssNone:
      Canvas.Brush.Color := clWindow;
    ssAllSelected:
      Canvas.Brush.Color := FSelectColor;
  else
    Canvas.Brush.Color := FInactiveSelectColor;
  end;

  DataString := FRawData.List[LineIndex].Description;
  R := MakeDrawRect(Offset.X, Offset.Y, ctDescription);
  DrawAlignedTextPart(DataString, R);

  if FRawData.List[LineIndex].Style in [lsRawWithExDescription, lsAsm] then
    if FRawData.List[LineIndex].LinkLength > 0 then
    begin
      R := GetLineJmpMarkRect(LineIndex, Offset);
      Canvas.Brush.Color := FJmpMarkColor;
      Canvas.Font.Color := clWhite;
      DataString := Copy(DataString, FRawData.List[LineIndex].LinkStart + 1,
        FRawData.List[LineIndex].LinkLength);
      DrawAlignedTextPart(DataString, R);
      Canvas.Font.Color := clWindowText;
    end;

  Inc(Offset.X, FHeader.ColumnWidth[ctDescription]);
end;

procedure TFWCustomHexView.DrawEditMark(var Offset: TPoint);
var
  I: TColumnType;
  DataChar: Char;
  R: TRect;
  A: Integer;
begin
  if not (FEditPosData.Column in [ctOpcode, ctDescription]) then Exit;
  if FEditPosData.LineIndex < 0 then Exit;

  if not LineVisible(FEditPosData.LineIndex) then Exit;

  // редактировать асмкод нельзя, только какими-то внешними инструментами
  if FRawData.List[FEditPosData.LineIndex].Style = lsAsm then Exit;

  for I := ctWorkSpace to High(TColumnType) do
    if I in FHeader.Columns then
      case I of
        ctWorkSpace, ctJmpLine, ctAddress, ctComment:
          Inc(Offset.X, FHeader.ColumnWidth[I]);
        ctOpcode, ctDescription:
        begin
          if FRawData.List[FEditPosData.LineIndex].Style <> lsRaw then
            if FEditPosData.Column = ctDescription then
              Exit;

          if FEditPosData.Column <> I then
            Inc(Offset.X, FHeader.ColumnWidth[I])
          else
          begin
            if FEditPosData.Column = ctOpcode then
              if FEditPosData.DescriptionByteIndex >
                DblSize(FRawData.List[FEditPosData.LineIndex].RawLength) then
                Exit;

            DataChar := GetEditChar;
            Inc(Offset.X, FCharWidth);
            Inc(Offset.Y, FLineHeight);

            if FEditPosData.Column = ctDescription then
              Inc(Offset.X, FEditPosData.DescriptionByteIndex * FCharWidth)
            else
              for A := 0 to FEditPosData.OpcodeCharIndex - 1 do
                Inc(Offset.X, FCharPositions[gmOn][A]);
            Inc(Offset.X, 1);

            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := FActiveCaretColor;

            Canvas.Font.Color := clWhite;
            R := Rect(Offset.X, Offset.Y,
              Offset.X + FCharWidth, Offset.Y + FLineHeight);
            DrawText(Canvas.Handle, @DataChar, 1, R, 0);
            Canvas.Font.Color := clWindowText;
          end;
        end;
      end;
end;

procedure TFWCustomHexView.DrawGradientRect(ARect: TRect);
var
  LRect: TRect;
  LTheme: HTHEME;
begin
  if (Win32MajorVersion >= 6) and StyleServices.Enabled then
  begin
    InflateRect(ARect, -2, -2);
    Dec(ARect.Top);
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ARect);
    LTheme := StyleServices.Theme[teMenu];
    LRect := ARect;
    DrawThemeBackground(LTheme, Canvas.Handle, MENU_POPUPITEM, MPI_HOT,
      LRect, @ARect);
  end
  else
  begin
    InflateRect(ARect, -1, -1);
    Dec(ARect.Bottom);
    LRect := ARect;
    Canvas.Brush.Color := clHighlight;
    Canvas.FrameRect(LRect);
    InflateRect(LRect, -1, -1);
    GradientFillCanvas(Canvas,
      TColor(ColorToRGB(clWhite)),
      RGB(237, 245, 255),
      LRect, gdVertical);
  end;
end;

procedure TFWCustomHexView.DrawHeader(Offset: TPoint);

  procedure DrawHeaderPart(const Caption: string;
    Column: TColumnType; Flags: DWORD = DT_CENTER);
  var
    R: TRect;
  begin
    R := MakeDrawRect(Offset.X, Offset.Y, Column);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, Flags);
    Inc(Offset.X, FHeader.ColumnWidth[Column]);
    Canvas.Pen.Color := clGrayText;
    Canvas.MoveTo(Offset.X, Offset.Y);
    Canvas.LineTo(Offset.X, Offset.Y + FLineHeight);
  end;

  function AddZeroToString(const Value: string): string;
  begin
    if Length(Value) = 1 then
      Result := '0' + Value
    else
      Result := Value;
  end;

  procedure DrawRawOffsets;
  var
    R: TRect;
    DataString: string;
    I: Integer;
  begin
    R := MakeDrawRect(Offset.X, Offset.Y, ctOpcode);
    DataString := EmptyStr;
    for I := 0 to BytesInRow - 1 do
      if AddressView = avHex then
        DataString := DataString + AddZeroToString(IntToHex(I, 2))
      else
        DataString := DataString + AddZeroToString(IntToStr(I));
    ExtTextOut(Canvas.Handle, R.Left, Offset.Y, ETO_CLIPPED, @R,
      @DataString[1], DblSize(BytesInRow), @FCharPositions[gmOn][0]);
    Inc(Offset.X, FHeader.ColumnWidth[ctOpcode]);
    Canvas.Pen.Color := clGrayText;
    Canvas.MoveTo(Offset.X, Offset.Y);
    Canvas.LineTo(Offset.X, Offset.Y + FLineHeight);
  end;

const
  Captions: array [TAddressMode] of array [TAddressView] of string =
  (
    ('(HEX32)', '(DEC32)'),
    ('(HEX64)', '(DEC64)')
  );
var
  LeftOffset: Integer;
  I: TColumnType;
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    Details := StyleServices.GetElementDetails(tgFixedCellNormal);
    StyleServices.DrawElement(Canvas.Handle, Details,
      Rect(0, 0, ClientWidth, FLineHeight));
  end
  else
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(0, 0, ClientWidth, FLineHeight));
  end;
  Canvas.Pen.Color := clGray;
  Canvas.MoveTo(0, FLineHeight);
  Canvas.LineTo(ClientWidth, FLineHeight);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clWindowText;
  Offset.Y := 0;
  LeftOffset := Offset.X;
  try
    for I := ctWorkSpace to High(TColumnType) do
      if I in FHeader.Columns then
        case I of
          ctWorkSpace:
            DrawHeaderPart(FHeader.ColumnCaption[ctWorkSpace], I);
          ctJmpLine:
            DrawHeaderPart(FHeader.ColumnCaption[ctJmpLine], I, DT_RIGHT);
          ctAddress:
            DrawHeaderPart(Captions[AddressMode, AddressView] + ' ' +
              FHeader.ColumnCaption[ctAddress], I, DT_RIGHT);
          ctOpcode:
            DrawRawOffsets;
          ctDescription:
            DrawHeaderPart(FHeader.ColumnCaption[ctDescription], I);
          ctComment:
            DrawHeaderPart(FHeader.ColumnCaption[ctComment], I);
        end;
  finally
    Offset.X := LeftOffset;
  end;
end;

procedure TFWCustomHexView.DrawHexPart(LineIndex: Integer; var Offset: TPoint);
var
  SelData: TSelectData;
  DrawString: string;
begin
  SelData := GetSelectData(LineIndex);
  case SelData.SelectStyle of
    ssAllSelected:
      DrawSelectedBackround(Offset, ctOpcode,
        0, DblSizeDec(FRawData.List[LineIndex].RawLength), gmOn);
    ssLeftSelected:
      DrawSelectedBackround(Offset, ctOpcode,
        0, DblSizeInc(SelData.FirstSelectIndex), gmOn);
    ssCenterSelected:
      DrawSelectedBackround(Offset, ctOpcode,
        DblSize(SelData.FirstSelectIndex),
        DblSizeInc(SelData.SecondSelectIndex), gmOn);
    ssRightSelected:
      DrawSelectedBackround(Offset, ctOpcode,
        DblSize(SelData.FirstSelectIndex),
        DblSizeDec(FRawData.List[LineIndex].RawLength), gmOn);
  end;

  DrawString := RawToHex(LineIndex);
  Canvas.Font.Color := FRawData.List[LineIndex].RawColor;
  try
    DrawTextBlock(Offset, ctOpcode, DrawString, gmOn);
  finally
    Canvas.Font.Color := clWindowText;
  end;
end;

function TFWCustomHexView.DrawJmpLine(LineFrom, LineTo: Integer;
  DrawOnlySelectedArrow, SecondDraw: Boolean): Boolean;

  function CalcVisibleLineOffset(LineIndex: Integer;
    StartLine, JmpFromLine: Boolean): TPoint;
  begin
    if StartLine then
      Result.X := GetLeftNCWidth - TEXT_MARGIN
    else
      Result.X := GetLeftNCWidth -
        TEXT_MARGIN - FPaintedJmpLinesCount * DblSize(SPLIT_MARGIN);
    if Result.X < TEXT_MARGIN then
      Result.X := TEXT_MARGIN;
    Result.Y := GetLineOffset(LineIndex) + FLineHeight shr 1;
    if JmpFromLine then
    begin
      if FRawData.List[LineIndex].Linked then
        Inc(Result.Y, SPLIT_MARGIN);
    end
    else
      if FRawData.List[LineIndex].JmpToAddr <> 0 then
        Dec(Result.Y, SPLIT_MARGIN);
    Inc(Result.X, FScrollOffset.X);
  end;

  function CalcInvisibleLineOffset(UpLine: Boolean): TPoint;
  begin
    Result.X := GetLeftNCWidth -
      TEXT_MARGIN - FPaintedJmpLinesCount * DblSize(SPLIT_MARGIN);
    if Result.X < TEXT_MARGIN then
      Result.X := TEXT_MARGIN;
    if UpLine then
    begin
      Result.Y := SPLIT_MARGIN;
      if Header.Visible then
        Inc(Result.Y, FLineHeight);
    end
    else
      Result.Y := ClientHeight - DblSize(SPLIT_MARGIN);
    Inc(Result.X, FScrollOffset.X);
  end;

var
  Selected: Boolean;
  LineFromVisible, LineToVisible: Boolean;
  JmpLineType: TJmpLineType;
  StartPoint, EndPoint: TPoint;
begin
  Result := True;
  Selected :=
    (GetSelectData(LineFrom).SelectStyle <> ssNone) or
    (GetSelectData(LineTo).SelectStyle <> ssNone);

  if Selected then
    Canvas.Pen.Color := FJmpMarkColor
  else
    if not DrawOnlySelectedArrow then
      Canvas.Pen.Color := FArrowColor
    else
      Exit;

  LineFromVisible := LineVisible(LineFrom);
  LineToVisible := LineVisible(LineTo);
  if SecondDraw and LineFromVisible then Exit(False);

  JmpLineType := jtNone;
  if LineFromVisible and LineToVisible then
  begin
    StartPoint := CalcVisibleLineOffset(LineFrom, True, True);
    EndPoint := CalcVisibleLineOffset(LineTo, False, False);
    JmpLineType := jtVisibleLines;
  end;

  if LineFromVisible and not LineToVisible then
  begin
    if LineFrom < LineTo then
    begin
      JmpLineType := jtDownToInvisible;
      StartPoint := CalcVisibleLineOffset(LineFrom, True, True);
      EndPoint := CalcInvisibleLineOffset(False);
    end
    else
    begin
      JmpLineType := jtUpToInvisible;
      StartPoint := CalcVisibleLineOffset(LineFrom, True, True);
      EndPoint := CalcInvisibleLineOffset(True);
    end;
  end;

  if not LineFromVisible and LineToVisible then
  begin
    if LineFrom < LineTo then
    begin
      JmpLineType := jtDownFromInvisible;
      StartPoint := CalcInvisibleLineOffset(True);
      EndPoint := CalcVisibleLineOffset(LineTo, True, False);
    end
    else
    begin
      JmpLineType := jtUpFromInvisible;
      StartPoint := CalcInvisibleLineOffset(False);
      EndPoint := CalcVisibleLineOffset(LineTo, True, False);
    end;
  end;

  if JmpLineType = jtNone then Exit;

  case JmpLineType of
    jtVisibleLines:
    begin
      Canvas.MoveTo(StartPoint.X, StartPoint.Y);
      Canvas.LineTo(EndPoint.X, StartPoint.Y);
      Canvas.LineTo(EndPoint.X, EndPoint.Y);
      Canvas.LineTo(StartPoint.X, EndPoint.Y);
    end;
    jtDownFromInvisible, jtUpFromInvisible:
    begin
      Canvas.MoveTo(StartPoint.X, StartPoint.Y);
      Canvas.LineTo(StartPoint.X, EndPoint.Y);
      Canvas.LineTo(EndPoint.X, EndPoint.Y);
    end;
    jtDownToInvisible, jtUpToInvisible:
    begin
      Canvas.MoveTo(StartPoint.X, StartPoint.Y);
      Canvas.LineTo(EndPoint.X, StartPoint.Y);
      Canvas.LineTo(EndPoint.X, EndPoint.Y);
    end;
  end;

  case JmpLineType of
    jtVisibleLines:
    begin
      EndPoint.X := StartPoint.X;
      DrawArrow(StartPoint, adLeft);
      DrawArrow(EndPoint, adRight);
    end;
    jtDownFromInvisible:
    begin
      DrawArrow(StartPoint, adDown);
      DrawArrow(EndPoint, adRight);
    end;
    jtUpToInvisible:
    begin
      DrawArrow(StartPoint, adLeft);
      DrawArrow(EndPoint, adUp);
    end;
    jtDownToInvisible:
    begin
      DrawArrow(StartPoint, adLeft);
      DrawArrow(EndPoint, adDown);
    end;
    jtUpFromInvisible:
    begin
      DrawArrow(StartPoint, adUp);
      DrawArrow(EndPoint, adRight);
    end;
  end;
end;

procedure TFWCustomHexView.DrawJmpLines(StartLine, EndLine: Integer; Offset: TPoint);

  procedure Process(DrawOnlySelectedArrow: Boolean);
  var
    I, A, JmpLine: Integer;
    Found: Boolean;
  begin
    FPaintedJmpLinesCount := 1;
    for I := StartLine to EndLine do
    begin
      // последняя линяя может быть невидимой (часть затерта скролом)
      if I = EndLine then
        if not LineVisible(I) then Exit;

      if FRawData.List[I].Style in [lsRawWithExDescription, lsAsm] then
        if FRawData.List[I].JmpToAddr <> 0 then
        begin
          JmpLine := AddressToLineIndex(FRawData.List[I].JmpToAddr);
          if JmpLine >= 0 then
          begin
            DrawJmpLine(I, JmpLine, DrawOnlySelectedArrow, False);
            Inc(FPaintedJmpLinesCount);
          end;
        end;

      if not DrawIncomingJmp then
        Continue;

      Found := False;
      if FRawData.List[I].Linked then
        for A := 0 to FJmpData.Count - 1 do
          if FJmpData.List[A].JmpTo = I then
          begin
            Found := True;
            DrawJmpLine(FJmpData.List[A].JmpFrom, I, DrawOnlySelectedArrow, True);
            Inc(FPaintedJmpLinesCount);
          end
          else
            if Found then
              Break;
    end;
  end;

var
  TopOffset: Integer;
begin
  TopOffset := Offset.Y;
  Process(False);
  Offset.Y := TopOffset;
  Process(True);
end;

procedure TFWCustomHexView.DrawArrow(ArrowPoint: TPoint; Direction: TArrowDirection);
begin
  case Direction of
    adLeft:
    begin
      Canvas.MoveTo(ArrowPoint.X + 3, ArrowPoint.Y - 2);
      Canvas.LineTo(ArrowPoint.X + 3, ArrowPoint.Y + 3);
      Canvas.MoveTo(ArrowPoint.X + 2, ArrowPoint.Y - 1);
      Canvas.LineTo(ArrowPoint.X + 2, ArrowPoint.Y + 2);
      Canvas.MoveTo(ArrowPoint.X + 1, ArrowPoint.Y);
      Canvas.LineTo(ArrowPoint.X + 1, ArrowPoint.Y + 1);
      Canvas.Pixels[ArrowPoint.X, ArrowPoint.Y] := clWindow;
      Canvas.Pixels[ArrowPoint.X + 4, ArrowPoint.Y] := clWindow;
    end;
    adRight:
    begin
      Canvas.MoveTo(ArrowPoint.X + 1, ArrowPoint.Y - 2);
      Canvas.LineTo(ArrowPoint.X + 1, ArrowPoint.Y + 3);
      Canvas.MoveTo(ArrowPoint.X + 2, ArrowPoint.Y - 1);
      Canvas.LineTo(ArrowPoint.X + 2, ArrowPoint.Y + 2);
      Canvas.MoveTo(ArrowPoint.X + 3, ArrowPoint.Y);
      Canvas.LineTo(ArrowPoint.X + 3, ArrowPoint.Y + 1);
      Canvas.Pixels[ArrowPoint.X, ArrowPoint.Y] := clWindow;
      Canvas.Pixels[ArrowPoint.X + 4, ArrowPoint.Y] := clWindow;
    end;
    adUp:
    begin
      Canvas.MoveTo(ArrowPoint.X - 2, ArrowPoint.Y + 3);
      Canvas.LineTo(ArrowPoint.X + 3, ArrowPoint.Y + 3);
      Canvas.MoveTo(ArrowPoint.X - 1, ArrowPoint.Y + 2);
      Canvas.LineTo(ArrowPoint.X + 2, ArrowPoint.Y + 2);
      Canvas.MoveTo(ArrowPoint.X, ArrowPoint.Y + 1);
      Canvas.LineTo(ArrowPoint.X + 1, ArrowPoint.Y + 1);
      Canvas.Pixels[ArrowPoint.X, ArrowPoint.Y] := clWindow;
      Canvas.Pixels[ArrowPoint.X, ArrowPoint.Y + 4] := clWindow;
    end;
    adDown:
    begin
      Canvas.MoveTo(ArrowPoint.X - 2, ArrowPoint.Y + 1);
      Canvas.LineTo(ArrowPoint.X + 3, ArrowPoint.Y + 1);
      Canvas.MoveTo(ArrowPoint.X - 1, ArrowPoint.Y + 2);
      Canvas.LineTo(ArrowPoint.X + 2, ArrowPoint.Y + 2);
      Canvas.MoveTo(ArrowPoint.X, ArrowPoint.Y + 3);
      Canvas.LineTo(ArrowPoint.X + 1, ArrowPoint.Y + 3);
      Canvas.Pixels[ArrowPoint.X, ArrowPoint.Y] := clWindow;
      Canvas.Pixels[ArrowPoint.X, ArrowPoint.Y + 4] := clWindow;
    end;
  end;
end;

procedure TFWCustomHexView.DrawArrowPart(
  LineIndex: Integer; var Offset: TPoint; DrawOnlySelectedArrow: Boolean);
var
  StartPoint, EndPoint: TPoint;
  I: Integer;
  Selected: Boolean;
begin
  Selected := GetSelectData(LineIndex).SelectStyle <> ssNone;

  if not Selected and DrawOnlySelectedArrow then Exit;

  if Selected then
    Canvas.Pen.Color := FSelectedArrowColor
  else
    Canvas.Pen.Color := FArrowColor;

  StartPoint.X := Offset.X + TEXT_MARGIN;
  for I := 0 to FRawData.List[LineIndex].CharIndex - 1 do
    Inc(StartPoint.X, FCharPositions[gmOn][I]);
  Inc(StartPoint.X, FCharWidth shr 1);

  StartPoint.Y := Offset.Y;

  // оффсет нужен на линию, на которую указываем
  Inc(StartPoint.Y, FLineHeight);
  Dec(StartPoint.Y, (LineIndex - FRawData.List[LineIndex].LineIndex) * FLineHeight);

  EndPoint.X := Offset.X + FHeader.ColumnWidth[ctOpcode] - TEXT_MARGIN;
  EndPoint.Y := Offset.Y + FLineHeight shr 1;

  Canvas.MoveTo(StartPoint.X, StartPoint.Y + 5);
  Canvas.LineTo(StartPoint.X, EndPoint.Y);
  Canvas.LineTo(EndPoint.X, EndPoint.Y);
  DrawArrow(StartPoint, adDown);
  DrawArrow(EndPoint, adRight);

  Inc(Offset.X, FHeader.ColumnWidth[ctOpcode]);
end;

procedure TFWCustomHexView.DrawArrows(StartLine, EndLine: Integer; Offset: TPoint);
var
  I, TopOffset, StartLineEndLineWithRadio, EndLineWithRadio: Integer;
begin
  TopOffset := Offset.Y;

  // Определяем где начнутся и закончатся расширенные чеки и радиокнопки
  StartLineEndLineWithRadio := StartLine;
  while FRawData.List[StartLineEndLineWithRadio].Style in [lsCheck, lsRadio, lsNone, lsLine] do
  begin
    if StartLineEndLineWithRadio = 0 then Break;
    Dec(StartLineEndLineWithRadio);
    Dec(TopOffset, FLineHeight);
  end;

  EndLineWithRadio := EndLine;
  while FRawData.List[EndLineWithRadio].Style in [lsCheck, lsRadio, lsNone, lsLine] do
  begin
    if EndLineWithRadio = FRawData.Count - 1 then Break;
    Inc(EndLineWithRadio);
  end;

  // рисуем стрелки в два прохода, лениво заморачиваться с отсечкой
  // по вертикали чтобы не подсвеченные линии не перезатирали подсвеченные
  Offset.Y := TopOffset;
  for I := StartLineEndLineWithRadio to EndLineWithRadio do
    if FRawData.List[I].Style in [lsCheck, lsRadio] then
      DrawCheck(I, Offset, False, FRawData.List[I].Style = lsRadio)
    else
      Inc(Offset.Y, FLineHeight);

  Offset.Y := TopOffset;
  for I := StartLineEndLineWithRadio to EndLineWithRadio do
    if FRawData.List[I].Style in [lsCheck, lsRadio] then
      DrawCheck(I, Offset, True, FRawData.List[I].Style = lsRadio)
    else
      Inc(Offset.Y, FLineHeight);
end;

procedure TFWCustomHexView.DrawLines(StartLine, EndLine: Integer;
  var Offset: TPoint);
var
  I, TopOffset: Integer;
begin
  if FRawData.Count = 0 then Exit;
  TopOffset := Offset.Y;
  for I := StartLine to EndLine do
  begin
    case FRawData.List[I].Style of
      lsNone, lsCheck, lsRadio:
        Inc(Offset.Y, FLineHeight);
      lsSeparator:
        DrawSeparator(I, Offset);
      lsLine:
        DrawLineSeparator(Offset);
      lsRaw:
        DrawRawLine(I, Offset);
      lsRawWithExDescription, lsAsm:
        DrawRawExLine(I, Offset);
      lsCode:
        DrawCodeLine(I, Offset);
    end;
  end;
  Offset.Y := TopOffset;
  DrawJmpLines(StartLine, EndLine, Offset);
  DrawArrows(StartLine, EndLine, Offset);
  DrawWorkSpacePart(StartLine, EndLine, Offset);
end;

procedure TFWCustomHexView.DrawLineSeparator(var Offset: TPoint);
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := clGrayText;
  Canvas.MoveTo(GetLeftNCWidth + FScrollOffset.X, Offset.Y + FLineHeight shr 1);
  Canvas.LineTo(ClientWidth, Offset.Y + FLineHeight shr 1);
  Inc(Offset.Y, FLineHeight);
end;

procedure TFWCustomHexView.DrawRadioPart(LineIndex: Integer; var Offset: TPoint);
var
  R, CheckRect: TRect;
  Details: TThemedElementDetails;
begin
  R := MakeDrawRect(Offset.X, Offset.Y, ctDescription);
  CheckRect := Rect(R.Left, R.Top, R.Left + 14, R.Top + 14);
  OffsetRect(CheckRect, 0, (FLineHeight - CheckRect.Height) shr 1);
  if StyleServices.Enabled then
  begin
    if FRawData.List[LineIndex].Checked then
      Details := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal)
    else
      Details := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
    StyleServices.DrawElement(Canvas.Handle, Details, CheckRect);
  end
  else
  begin
    if FRawData.List[LineIndex].Checked then
      DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, DFCS_BUTTONRADIO or DFCS_CHECKED)
    else
      DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, DFCS_BUTTONRADIO);
  end;
  if GetSelectData(LineIndex).SelectStyle <> ssNone then
    Canvas.Brush.Color := FSelectColor
  else
    Canvas.Brush.Color := clWindow;
  R.Left := CheckRect.Right + 4;
  DrawText(Canvas.Handle, PChar(FRawData.List[LineIndex].Description),
    Length(FRawData.List[LineIndex].Description), R, 0);
  Inc(Offset.X, FHeader.ColumnWidth[ctDescription]);
end;

procedure TFWCustomHexView.DrawRawExLine(LineIndex: Integer;
  var Offset: TPoint);
var
  LeftOffset: Integer;
  I: TColumnType;
begin
  LeftOffset := Offset.X;
  try
    for I := ctWorkSpace to High(TColumnType) do
      if I in FHeader.Columns then
        case I of
          ctWorkSpace, ctJmpLine:
            Inc(Offset.X, FHeader.ColumnWidth[I]);
          ctAddress:
            DrawAddressPart(LineIndex, Offset);
          ctOpcode:
            DrawHexPart(LineIndex, Offset);
          ctDescription:
            DrawDescriptionPart(LineIndex, Offset);
          ctComment:
            DrawCommentPart(LineIndex, Offset);
        end;
  finally
    Offset.X := LeftOffset;
  end;
  Inc(Offset.Y, FLineHeight);
end;

procedure TFWCustomHexView.DrawRawLine(LineIndex: Integer; var Offset: TPoint);
var
  LeftOffset: Integer;
  I: TColumnType;
begin
  LeftOffset := Offset.X;
  try
    for I := ctWorkSpace to High(TColumnType) do
      if I in FHeader.Columns then
        case I of
          ctWorkSpace, ctJmpLine:
            Inc(Offset.X, FHeader.ColumnWidth[I]);
          ctAddress:
            DrawAddressPart(LineIndex, Offset);
          ctOpcode:
            DrawHexPart(LineIndex, Offset);
          ctDescription:
            DrawDataPart(LineIndex, Offset);
          ctComment:
            DrawCommentPart(LineIndex, Offset);
        end;
  finally
    Offset.X := LeftOffset;
  end;
  Inc(Offset.Y, FLineHeight);
end;

procedure TFWCustomHexView.DrawSelectedBackround(var Offset: TPoint;
 Column: TColumnType; SelStart, SelEnd: Integer; AGroupMode: TGroupMode);
var
  R: TRect;
  I: Integer;
begin
  R.Left := 0;
  for I := 0 to SelStart - 1 do
    Inc(R.Left, FSelectionPositions[AGroupMode][I]);
  R.Width := 0;
  for I := SelStart to SelEnd do
    Inc(R.Right, FSelectionPositions[AGroupMode][I]);
  if R.Width + R.Left + DblSize(TEXT_MARGIN) > FHeader.ColumnWidth[Column] then
    R.Width := FHeader.ColumnWidth[Column] - R.Left - DblSize(TEXT_MARGIN);
  R := MakeSelectRect(Offset.X + R.Left, Offset.Y, R.Width);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FSelectColor;
  Canvas.FillRect(R);
end;

procedure TFWCustomHexView.DrawSeparator(LineIndex: Integer;
  var Offset: TPoint);
var
  R: TRect;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBtnFace;
  Canvas.Pen.Color := clGrayText;
  R := Rect(GetLeftNCWidth, Offset.Y,
    ClientWidth + 1, Offset.Y + FLineHeight);
  Inc(R.Left, FScrollOffset.X);
  Canvas.Rectangle(R);
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clWindowText;
  DrawText(Canvas.Handle, PChar(FRawData.List[LineIndex].Description),
    Length(PChar(FRawData.List[LineIndex].Description)), R, DT_CENTER);
  Inc(Offset.Y, FLineHeight);
end;

procedure TFWCustomHexView.DrawTextBlock(var Offset: TPoint;
  Column: TColumnType; const DrawString: string; AGroupMode: TGroupMode);
var
  R: TRect;
begin
  R := MakeDrawRect(Offset.X, Offset.Y, Column);
  Canvas.Brush.Style := bsClear;
  ExtTextOut(Canvas.Handle, R.Left, Offset.Y, ETO_CLIPPED, @R,
    PChar(@DrawString[1]), Length(DrawString),
    @FCharPositions[AGroupMode][0]);
  Inc(Offset.X, FHeader.ColumnWidth[Column]);
end;

procedure TFWCustomHexView.DrawWorkSpacePart(StartLine, EndLine: Integer;
  Offset: TPoint);
var
  I: TBookMark;
  LineIndex: Integer;
  R: TRect;
  TextSize: TSize;
begin
  if not(ctWorkSpace in Header.Columns) then Exit;
  for I := Low(TBookMark) to High(TBookMark) do
    if FBookMarks[I] <> 0 then
    begin
      LineIndex := AddressToLineIndex(FBookMarks[I]);
      if LineIndex < StartLine then Continue;
      if LineIndex > EndLine then Continue;
      R.Left := TEXT_MARGIN + FScrollOffset.X;
      R.Top := GetLineOffset(LineIndex);
      R.Width := DblSize(TEXT_MARGIN);
      R.Height := FLineHeight;
      DrawGradientRect(R);
      Canvas.Brush.Style := bsClear;
      TextSize := Canvas.TextExtent(CHAR_STRING);
      OffsetRect(R, (R.Width - TextSize.cx) div 2, -1 + (R.Height - TextSize.cy) div 2);
      DrawText(Canvas.Handle, PChar(IntToStr(I)), 1, R, 0);
    end;
end;

procedure TFWCustomHexView.EndUpdate;
begin
  Dec(FUpdateCount);
  DoChange;
end;

procedure TFWCustomHexView.FocusOnAddress(Address: ULONG_PTR);
var
  Index: Integer;
begin
  Index := AddressToLineIndex(Address);
  if Index < 0 then Exit;

  // делаем чтобы строка была самой первой сверху
  if Index > GetVisibleLineDiapason.StartLine then
    FocusOnLine(Index + (ClientHeight div FLineHeight) - 3)
  else
    FocusOnLine(Index - 1);

  // ну и выделяем ее, дабы в глаза бросалась
  SelectLine(Index);
end;

procedure TFWCustomHexView.FocusOnLine(LineIndex: Integer);
var
  Diapason: TVisibleLineDiapason;
begin
  if LineVisible(LineIndex) then Exit;
  Diapason := GetVisibleLineDiapason;
  if LineIndex < Diapason.StartLine then
    FScrollOffset.Y := LineIndex * FLineHeight
  else
    FScrollOffset.Y :=
      (LineIndex - (Diapason.EndLine - Diapason.StartLine)) * FLineHeight;
  FScrollOffset.Y := -FScrollOffset.Y;
  UpdateScrollPos;
  Invalidate;
end;

procedure TFWCustomHexView.FontChange(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  if Assigned(FOldOnFontChange) then
    FOldOnFontChange(Sender);
  DoChange(False);
end;

function TFWCustomHexView.GetAddrPartStr(LineIndex: Integer): string;
const
  AddrLength: array [TAddressMode] of Byte = (8, 16);
begin
  if AddressView = avDecimal then
  begin
    Result := IntToStr(FRawData.List[LineIndex].Address);
    Result :=
      StringOfChar('0',  AddrLength[AddressMode] - Length(Result)) +
      Result;
  end
  else
    Result :=
      IntToHex(FRawData.List[LineIndex].Address, AddrLength[AddressMode]);
end;

function TFWCustomHexView.GetBookMark(AIndex: TBookMark): ULONG_PTR;
begin
  Result := FBookMarks[AIndex];
end;

function TFWCustomHexView.GetEditChar: Char;
var
  DataString: string;
  Index: Integer;
begin
  Result := #0;
  if FEditPosData.Column = ctOpcode then
  begin
    DataString := RawToHex(FEditPosData.LineIndex);
    Index := FEditPosData.OpcodeCharIndex;
  end
  else
  begin
    DataString := RawToString(FEditPosData.LineIndex);
    Index := FEditPosData.DescriptionByteIndex;
  end;

  if DataString <> EmptyStr then
    if Index < Length(DataString) then
      Result := Char(DataString[Index + 1]);
end;

function TFWCustomHexView.GetHitInfo(XPos, YPos: Integer): TMouseHitInfo;
var
  I: TColumnType;
  A: Integer;
  LeftOffset, RawLength: Integer;
begin
  Result.Erase;
  Result.XPos := XPos;
  Result.YPos := YPos;

  Exclude(FStates, csNeedResizeColumn);

  Dec(XPos, FScrollOffset.X);

  LeftOffset := 0;
  for I := ctWorkSpace to High(TColumnType) do
  begin
    if not (I in FHeader.Columns) then Continue;
    Inc(LeftOffset, FHeader.ColumnWidth[I]);
    if LeftOffset + SPLIT_MARGIN >= XPos then
    begin
      Result.SelectPoint.Column := I;
      Result.ColumnWidth := FHeader.ColumnWidth[I];
      Result.OnSplitter := LeftOffset - SPLIT_MARGIN < XPos;

      // если правая граница колонки за экраном,
      // позволяем быстро изменить ее размеры без скролирования по горизонтали
      if not Result.OnSplitter and (XPos + FScrollOffset.X >= ClientWidth - DblSize(SPLIT_MARGIN)) then
      begin
        Result.ColumnWidth := FHeader.ColumnWidth[I] - (LeftOffset - XPos - FScrollOffset.X);
        if Result.ColumnWidth >= MIN_COLUNN_WIDTH then
        begin
          Include(FStates, csNeedResizeColumn);
          Result.OnSplitter := True;
        end;
      end;

      Break;
    end;
  end;

  if Header.Visible then
  begin
    Result.SelectPoint.OnHeader := YPos <= FLineHeight;
    Dec(YPos, FLineHeight);
  end;
  if Result.SelectPoint.OnHeader then Exit;
  Dec(YPos, FScrollOffset.Y);

  Result.SelectPoint.LineIndex := YPos div FLineHeight;
  if Result.SelectPoint.LineIndex >= FRawData.Count then
    Result.SelectPoint.LineIndex := -1;

  if Result.OnSplitter then Exit;

  if Result.SelectPoint.LineIndex < 0 then Exit;

  // выделять для модификации можно только данные в колонках с опкодами и описанием
  if not (Result.SelectPoint.Column in [ctOpcode, ctDescription]) then Exit;

  // причем только у строк содержащих RAW данные
  if not (FRawData.List[Result.SelectPoint.LineIndex].Style in
    [lsRaw, lsRawWithExDescription, lsAsm]) then Exit;

  // если у строки есть расширенное описание, то стандартное описание для нее не генерируется
  // и выделать в этом случае на этой колонке нечего
  // но нужно проверить, не находимся ли мы над гиперлинком
  if (FRawData.List[Result.SelectPoint.LineIndex].Style in [lsRawWithExDescription, lsAsm]) then
  begin
    if Result.SelectPoint.Column = ctDescription then
    begin
      if FRawData.List[Result.SelectPoint.LineIndex].LinkLength > 0 then
      begin
        if XPos > LeftOffset - TEXT_MARGIN then Exit;
        Dec(LeftOffset, FHeader.ColumnWidth[Result.SelectPoint.Column]);
        Inc(LeftOffset, FRawData.List[Result.SelectPoint.LineIndex].LinkStart * FCharWidth);
        Inc(LeftOffset, TEXT_MARGIN);
        Result.OnJmpMark := (XPos >= LeftOffset);
        if Result.OnJmpMark then
          Result.OnJmpMark := XPos < (LeftOffset + FRawData.List[Result.SelectPoint.LineIndex].LinkLength * FCharWidth);
      end;
      Exit;
    end;
    // Для асма дальнейшая обработка не нужна, в отличие от lsRawWithExDescription
    if FRawData.List[Result.SelectPoint.LineIndex].Style = lsAsm then Exit;
  end;

  Dec(LeftOffset, FHeader.ColumnWidth[Result.SelectPoint.Column]);
  Inc(LeftOffset, TEXT_MARGIN);

  // если курсор находится левее начала текста в колонке,
  // выделяем первый символ  и выходим
  if LeftOffset > XPos then
  begin
    Result.SelectPoint.ByteIndex := -1;
    Exit;
  end;

  if Result.SelectPoint.Column = ctOpcode then
  begin
    // в колонке с опкодами каждый байт представлен ввиде двух символов
    // поэтому нам нужно анализировать координаты каждого символа
    for A := 0 to DblSizeDec(BytesInRow) do
    begin
      Inc(LeftOffset, FSelectionPositions[gmOn][A]);
      if LeftOffset > XPos then
      begin
        RawLength := FRawData.List[Result.SelectPoint.LineIndex].RawLength;
        Result.SelectPoint.ByteIndex := A shr 1;
        if Result.SelectPoint.ByteIndex > RawLength - 1 then
          Result.SelectPoint.ByteIndex := -1;
        if A and 1 = 0 then
          Include(FStates, csLeftCharSelected)
        else
          Exclude(FStates, csLeftCharSelected);
        Exit;
      end;
    end;
    Exit;
  end;

  // а в колонке с описанием символы идут как есть, поэтому банально ищем нужный
  for A := 0 to BytesInRow - 1 do
  begin
    Inc(LeftOffset, FCharWidth);
    if LeftOffset > XPos then
    begin
      RawLength := FRawData.List[Result.SelectPoint.LineIndex].RawLength;
      Result.SelectPoint.ByteIndex := Min(A, RawLength - 1);
      if Result.SelectPoint.ByteIndex >= RawLength then
        Result.SelectPoint.ByteIndex := RawLength - 1;
      Exit;
    end;
  end;
end;

function TFWCustomHexView.GetLeftNCWidth: Integer;
begin
  Result := 0;
  if ctWorkSpace in Header.Columns then
    Inc(Result, Header.ColumnWidth[ctWorkSpace]);
  if ctJmpLine in Header.Columns then
    Inc(Result, Header.ColumnWidth[ctJmpLine]);
end;

function TFWCustomHexView.GetLineJmpMarkRect(LineIndex: Integer;
  Offset: TPoint): TRect;
var
  LeftOffset, MarkWidth: Integer;
begin
  LeftOffset := FRawData.List[LineIndex].LinkStart * FCharWidth;
  MarkWidth := FRawData.List[LineIndex].LinkLength * FCharWidth;
  if LeftOffset + MarkWidth + DblSize(TEXT_MARGIN) > FHeader.ColumnWidth[ctDescription]  then
    MarkWidth := FHeader.ColumnWidth[ctDescription] - DblSize(TEXT_MARGIN) - LeftOffset;
  Result := MakeSelectRect(Offset.X + LeftOffset, Offset.Y, MarkWidth);
end;

function TFWCustomHexView.GetLineOffset(LineIndex: Integer): Integer;
begin
  Result := FScrollOffset.Y + LineIndex * FLineHeight;
  if Header.Visible then
    Inc(Result, FLineHeight);
end;

function TFWCustomHexView.GetPageHeight: Integer;
begin
  if FLineHeight = 0 then
    Result := ClientHeight
  else
    Result := ClientHeight - (ClientHeight mod FLineHeight) - FLineHeight;
  if Header.Visible then
    Dec(Result, FLineHeight);
end;

function TFWCustomHexView.GetSelectData(LineIndex: Integer): TSelectData;
var
  SelStart, SelEnd, MaxPosInLine: Integer;
  LeftSel, RightSel: TSelectPoint;
begin
  ZeroMemory(@Result, SizeOf(TSelectData));

  if FSelStart.InvalidLine then Exit;
  if FSelEnd.InvalidLine then Exit;

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

  if LeftSel.LineIndex > LineIndex then Exit;
  if RightSel.LineIndex < LineIndex then Exit;

  if LeftSel.LineIndex = LineIndex then
    SelStart := LeftSel.ByteIndex
  else
    SelStart := 0;

  if SelStart < 0 then
    SelStart := 0;

  MaxPosInLine := FRawData.List[RightSel.LineIndex].RawLength - 1;

  if RightSel.LineIndex > LineIndex then
    SelEnd := MaxPosInLine
  else
    SelEnd := RightSel.ByteIndex;

  if SelEnd < 0 then
    SelEnd := MaxPosInLine;

  if SelStart = 0 then
  begin
    if SelEnd = MaxPosInLine then
      Result.SelectStyle := ssAllSelected
    else
      Result.SelectStyle := ssLeftSelected;
  end
  else
    if SelEnd = MaxPosInLine then
      Result.SelectStyle := ssRightSelected
    else
      Result.SelectStyle := ssCenterSelected;

  case Result.SelectStyle of
    ssLeftSelected:
      Result.FirstSelectIndex := SelEnd;
    ssCenterSelected:
    begin
      Result.FirstSelectIndex := SelStart;
      Result.SecondSelectIndex := SelEnd;
    end;
    ssRightSelected:
      Result.FirstSelectIndex := SelStart;
  end;
end;

function TFWCustomHexView.GetVisibleLineDiapason: TVisibleLineDiapason;
begin
  Result.StartLine := CurrentVisibleLine;
  Result.EndLine := Min(Result.StartLine +
    GetPageHeight div FLineHeight, FRawData.Count - 1);
end;

function TFWCustomHexView.GetVisibleLinesCount: Integer;
begin
  Result := GetPageHeight div FLineHeight;
end;

function TFWCustomHexView.LineVisible(LineIndex: Integer): Boolean;
var
  D: TVisibleLineDiapason;
begin
  D := GetVisibleLineDiapason;
  Result := (D.StartLine <= LineIndex) and (D.EndLine >= LineIndex);
end;

function TFWCustomHexView.MakeDrawRect(LeftOffset, TopOffset,
  ColumnWidth: Integer): TRect;
begin
  Result := Rect(
    LeftOffset + TEXT_MARGIN,
    TopOffset,
    LeftOffset - TEXT_MARGIN + ColumnWidth,
    TopOffset + FLineHeight);
end;

function TFWCustomHexView.MakeSelectRect(LeftOffset, TopOffset,
  SelectWidth: Integer): TRect;
begin
  Result := Rect(
    LeftOffset + TEXT_MARGIN,
    TopOffset,
    LeftOffset + TEXT_MARGIN + SelectWidth,
    TopOffset + FLineHeight);
end;

function TFWCustomHexView.Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function TFWCustomHexView.MakeDrawRect(
  LeftOffset, TopOffset: Integer; Column: TColumnType): TRect;
begin
  Result := MakeDrawRect(LeftOffset, TopOffset, FHeader.ColumnWidth[Column]);
end;

procedure TFWCustomHexView.Paint;
var
  Diapason: TVisibleLineDiapason;
  Offset: TPoint;
begin
  if FLineHeight = 0 then
    UpdateWidthHeight;

  Diapason := GetVisibleLineDiapason;

  Offset.X := FScrollOffset.X;
  Offset.Y := Diapason.StartLine * FLineHeight + FScrollOffset.Y;
  if Header.Visible then
    Inc(Offset.Y, FLineHeight);

  if Diapason.EndLine < FRawData.Count - 1 then
    Inc(Diapason.EndLine);

  // бэкграунд
  DrawBackground(Diapason.StartLine, Diapason.EndLine);

  // сами данные
  DrawLines(Diapason.StartLine, Diapason.EndLine, Offset);

  // верхняя строка может быть не до конца выровнена, если скрол внизу
  // поэтому заголовок рисуем последним, он затрет лишнее в шапке
  if Header.Visible then
    DrawHeader(Offset);

  // ну и отрисовка анимации редактируемой позиции самая последняя
  if FEditPosData.Showed then
  begin
    Offset.Y := FEditPosData.LineIndex * FLineHeight + FScrollOffset.Y;
    if not Header.Visible then
      Dec(Offset.Y, FLineHeight);
    DrawEditMark(Offset);
  end;
end;

function TFWCustomHexView.RawToHex(LineIndex: Integer): string;
const
  TwoHexLookup : packed array[0..255] of array[1..2] of AnsiChar =
  ('00','01','02','03','04','05','06','07','08','09','0A','0B','0C','0D','0E','0F',
   '10','11','12','13','14','15','16','17','18','19','1A','1B','1C','1D','1E','1F',
   '20','21','22','23','24','25','26','27','28','29','2A','2B','2C','2D','2E','2F',
   '30','31','32','33','34','35','36','37','38','39','3A','3B','3C','3D','3E','3F',
   '40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
   '50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
   '60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
   '70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
   '80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
   '90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
   'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
   'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
   'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
   'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
   'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
   'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','FA','FB','FC','FD','FE','FF');
var
  I: Integer;
  Data: array of Byte;
  AnsiResult: AnsiString;
begin
  SetLength(Data, FRawData.List[LineIndex].RawLength);
  if Length(Data) = 0 then Exit;
  FDataStream.Position := FRawData.List[LineIndex].DataOffset;
  FDataStream.ReadBuffer(Data[0], Length(Data));
  SetLength(AnsiResult, DblSize(Length(Data)));
  for I := 0 to FRawData.List[LineIndex].RawLength - 1 do
  begin
    AnsiResult[I shl 1 + 1] := TwoHexLookup[Data[I]][1];
    AnsiResult[I shl 1 + 2] := TwoHexLookup[Data[I]][2];
  end;
  Result := string(AnsiResult);
end;

function TFWCustomHexView.RawToString(LineIndex: Integer): string;
var
  I: Integer;
  Data: TBytes;
  AnsiResult: AnsiString;
begin
  if FRawData.List[LineIndex].RawLength = 0 then Exit;
  SetLength(Data, FRawData.List[LineIndex].RawLength);
  FDataStream.Position := FRawData.List[LineIndex].DataOffset;
  FDataStream.ReadBuffer(Data[0], Length(Data));
  SetLength(AnsiResult, Length(Data));
  for I := 0 to FRawData.List[LineIndex].RawLength - 1 do
    if CharVisible(Data[I]) then
      AnsiResult[I + 1] := AnsiChar(Data[I])
    else
      AnsiResult[I + 1] := '.';
  Result := string(AnsiResult);
end;

procedure TFWCustomHexView.RebuildDataMap;
begin
  Include(FStates, csNeedUpdateDataMap);
  DoChange;
end;

procedure TFWCustomHexView.Resize;
begin
  inherited;
  UpdateWidthHeight;
  UpdateTextBoundary;
  UpdateScrollPos;
end;

procedure TFWCustomHexView.SelectLine(LineIndex: Integer);
begin
  FSelStart.LineIndex := LineIndex;
  FSelStart.ByteIndex := 0;
  FSelStart.Column := ctOpcode;
  FSelStart.OnHeader := False;
  FSelEnd.LineIndex := LineIndex;
  FSelEnd.ByteIndex := FRawData.List[LineIndex].RawLength - 1;
  FSelEnd.Column := ctOpcode;
  FSelEnd.OnHeader := False;
end;

procedure TFWCustomHexView.SetAddressMode(const Value: TAddressMode);
begin
  if AddressMode <> Value then
  begin
    FAddressMode := Value;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.SetAddressView(const Value: TAddressView);
begin
  if AddressView <> Value then
  begin
    FAddressView := Value;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.SetBookMark(AIndex: TBookMark; const Value: ULONG_PTR);
begin
  FBookMarks[AIndex] := Value;
end;

procedure TFWCustomHexView.SetBorderStyle(const Value: TBorderStyle);
begin
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TFWCustomHexView.SetBytesInColorGroup(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.CreateFmt('Wrong BytesInColorGroup value (%d)', [Value]);
  if BytesInColorGroup <> Value then
  begin
    FBytesInColorGroup := Value;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.SetBytesInGroup(const Value: Integer);
begin
  if Value < 2 then
    raise Exception.CreateFmt('Wrong BytesInGroup value (%d)', [Value]);
  if BytesInGroup <> Value then
  begin
    FBytesInGroup := Value;
    DoChange;
  end;
end;

procedure TFWCustomHexView.SetBytesInRow(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.CreateFmt('Wrong BytesInRow value (%d)', [Value]);
  if BytesInRow <> Value then
  begin
    FBytesInRow := Value;
    FSelStart.Erase;
    FSelEnd.Erase;
    FEditPosData.Column := ctNone;
    Include(FStates, csNeedUpdateDataMap);
    DoChange;
  end;
end;

procedure TFWCustomHexView.SetDataStream(Value: TStream;
  StartAddress: ULONG_PTR);
begin
  if FDataStream <> Value then
  begin
    FDataStream := Value;
    FStartAddress := StartAddress;
    FDataMap.CurrentAddr := StartAddress;
    RebuildDataMap;
  end;
end;

procedure TFWCustomHexView.SetDrawIncomingJmp(const Value: Boolean);
begin
  if DrawIncomingJmp <> Value then
  begin
    FDrawIncomingJmp := Value;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TFWCustomHexView.SetSeparateGroupByColor(const Value: Boolean);
begin
  if SeparateGroupByColor <> Value then
  begin
    FSeparateGroupByColor := Value;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.UpdateWidthHeight;
var
  I, DoubleWidth, CharsInGroup, NextGroup: Integer;
  AGroupMode: TGroupMode;
begin
  FLineHeight := Canvas.TextHeight(WG_STRING);
  FCharWidth := Canvas.TextWidth(CHAR_STRING);

  SetLength(FCharPositions[gmOff], DblSize(BytesInRow));
  SetLength(FCharPositions[gmOn], DblSize(BytesInRow));
  SetLength(FSelectionPositions[gmOff], DblSize(BytesInRow));
  SetLength(FSelectionPositions[gmOn], DblSize(BytesInRow));

  // раскидываем позиции символов для режима группировки
  CharsInGroup := DblSize(FBytesInGroup);
  NextGroup := CharsInGroup - 1;
  FCharPositions[gmOff][0] := FCharWidth;
  FCharPositions[gmOn][0] := FCharWidth;
  for I := 1 to DblSizeDec(BytesInRow) do
  begin
    FCharPositions[gmOff][I] := FCharWidth;
    if I and 1 = 0 then
      FCharPositions[gmOn][I] := FCharWidth
    else
    begin
      if I = NextGroup then
      begin
        FCharPositions[gmOn][I] := DblSize(FCharWidth) + TEXT_MARGIN;
        Inc(NextGroup, CharsInGroup);
      end
      else
        FCharPositions[gmOn][I] := DblSize(FCharWidth);
    end;
  end;
  FCharPositions[gmOff][DblSizeDec(BytesInRow)] := FCharWidth;
  FCharPositions[gmOn][DblSizeDec(BytesInRow)] := FCharWidth;

  // раскидываем позиции начала и конца выделения для всех режимов отображения
  FSelectionPositions[gmOff][0] := FCharWidth;
  FSelectionPositions[gmOn][0] := FCharWidth;
  for AGroupMode := Low(TGroupMode) to High(TGroupMode) do
  begin
    I := 1;
    while I < DblSizeDec(BytesInRow) do
    begin
      DoubleWidth :=
        FCharPositions[AGroupMode][I] +
        FCharPositions[AGroupMode][I + 1];
      FSelectionPositions[AGroupMode][I] := DoubleWidth shr 1;
      FSelectionPositions[AGroupMode][I + 1] :=
        DoubleWidth - FSelectionPositions[AGroupMode][I];
      Inc(I, 2);
    end;
  end;
  FSelectionPositions[gmOff][DblSizeDec(BytesInRow)] := FCharWidth;
  FSelectionPositions[gmOn][DblSizeDec(BytesInRow)] := FCharWidth;
end;

procedure TFWCustomHexView.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TFWCustomHexView.WMHScroll(var Msg: TWMHScroll);
begin
  case Msg.ScrollCode of
    SB_LINELEFT: Inc(FScrollOffset.X, 10);
    SB_LINERIGHT: Dec(FScrollOffset.X, 10);
    SB_PAGELEFT: Inc(FScrollOffset.X, ClientWidth);
    SB_PAGERIGHT: Dec(FScrollOffset.X, ClientWidth);
    SB_THUMBPOSITION, SB_THUMBTRACK: FScrollOffset.X := Msg.Pos * -1;
  end;
  UpdateScrollPos;
  Invalidate;
end;

procedure TFWCustomHexView.WMLButtonDown(var Message: TWMLButtonDown);
var
  StartSelect, ShiftSelect: Boolean;
begin
  inherited;
  SetFocus;
  Include(FStates, csMousePressed);
  FMousePressedHitInfo := GetHitInfo(Message.XPos, Message.YPos);

  if FMousePressedHitInfo.OnJmpMark then
  begin
    DoJmpTo(FMousePressedHitInfo.SelectPoint.LineIndex, jsPushToUndo);
    Exit;
  end;

  StartSelect := not FMousePressedHitInfo.OnSplitter;
  if StartSelect then
    if not FMousePressedHitInfo.SelectPoint.OnHeader then
      StartSelect := FMousePressedHitInfo.SelectPoint.Column in FHeader.Columns;
  if StartSelect then
    StartSelect := not (FMousePressedHitInfo.SelectPoint.Column in [ctWorkSpace, ctJmpLine]);

  if FMousePressedHitInfo.OnSplitter  then
  begin
    FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] :=
      FMousePressedHitInfo.ColumnWidth;
  end
  else
  begin
    if StartSelect then
    begin
      ShiftSelect := False;
      if ssShift in KeysToShiftState(Message.Keys) then
      begin
        if not FSelStart.InvalidLine then
        begin
          FSelEnd := FMousePressedHitInfo.SelectPoint;
          ShiftSelect := True;
        end;
      end;
      if not ShiftSelect then
      begin
        FSelStart := FMousePressedHitInfo.SelectPoint;
        FSelEnd := FSelStart;
      end;
    end
    else
    begin
      FSelStart.Erase;
      FSelEnd.Erase;
    end;
  end;

  UpdateCaretTimer;
  UpdateSelectedPosition(FSelEnd);
  Invalidate;
end;

procedure TFWCustomHexView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  Exclude(FStates, csMousePressed);
  if FMousePressedHitInfo.OnSplitter then
  begin
    UpdateTextBoundary;
    UpdateScrollPos;
    Invalidate;
  end
  else
    CreateCaretTimer;
end;

procedure TFWCustomHexView.WMMouseMove(var Message: TWMMouseMove);
var
  HitTest: TMouseHitInfo;
begin
  inherited;
  HitTest := GetHitInfo(Message.XPos, Message.YPos);

  if not (csMousePressed in FStates)  then
  begin
    if HitTest.OnSplitter then
      Cursor := crHSplit
    else
      if HitTest.OnJmpMark then
        Cursor := crHandPoint
      else
        Cursor := crDefault;
  end;

  if (csMousePressed in FStates) and FMousePressedHitInfo.OnSplitter then
  begin
    FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] :=
      FMousePressedHitInfo.ColumnWidth +
      Message.XPos -
      FMousePressedHitInfo.XPos;

    if FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] < MIN_COLUNN_WIDTH then
      FHeader.ColumnWidth[FMousePressedHitInfo.SelectPoint.Column] := MIN_COLUNN_WIDTH;

    UpdateTextBoundary;
    if Message.XPos > FMousePressedHitInfo.XPos then
      UpdateScrollPos;
    Invalidate;
    Exit;
  end;

  // проверка условий при которых невозможно выделение
  if not (csMousePressed in FStates) then Exit;

  DestroyCaretTimer;
  FEditPosData.Showed := True;

  if HitTest.SelectPoint.InvalidLine then Exit;
  if FSelStart.InvalidLine then Exit;
  if not HitTest.SelectPoint.OnHeader then
    if not (HitTest.SelectPoint.Column in FHeader.Columns) then Exit;
  UpdateSelectedPosition(HitTest.SelectPoint);
  FSelEnd := HitTest.SelectPoint;
  if HitTest.SelectPoint.InvalidLine then
    if FSelEnd < FSelStart then
      FSelEnd.ByteIndex := 0
    else
      FSelEnd.ByteIndex := BytesInRow - 1;

  Invalidate;
end;

procedure TFWCustomHexView.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if not (ScrollBars in [TScrollStyle.ssBoth, TScrollStyle.ssVertical]) then Exit;
  Inc(FScrollOffset.Y, FLineHeight * Message.WheelDelta div WHEEL_DELTA);
  UpdateScrollPos;
  Invalidate;
end;

procedure TFWCustomHexView.WMVScroll(var Msg: TWMVScroll);
var
  SI: TScrollInfo;
begin
  case Msg.ScrollCode of
    SB_LINEUP: Inc(FScrollOffset.Y, FLineHeight);
    SB_LINEDOWN: Dec(FScrollOffset.Y, FLineHeight);
    SB_PAGEUP: Inc(FScrollOffset.Y, GetPageHeight);
    SB_PAGEDOWN: Dec(FScrollOffset.Y, GetPageHeight);
    SB_THUMBPOSITION, SB_THUMBTRACK:
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask :=  SIF_TRACKPOS;
      GetScrollInfo(Handle, SB_VERT, SI);
      FScrollOffset.Y := -SI.nTrackPos;
    end;
  end;
  FScrollOffset.Y := FScrollOffset.Y - (FScrollOffset.Y mod FLineHeight);
  UpdateScrollPos;
  Invalidate;
end;

procedure TFWCustomHexView.UpdateCaretTimer;
begin
  KillTimer(Handle, 0);
  CreateCaretTimer;
  if not FEditPosData.Showed then
  begin
    FEditPosData.Showed := True;
    Invalidate;
  end;
end;

procedure TFWCustomHexView.UpdateDataMap;
var
  I, MapIndex: Integer;
  StreamOffset: ULONG_PTR;
  Line: TLineData;
  RawLength: Integer;
  JmpData: TJmpData;
begin
  Exclude(FStates, csNeedUpdateDataMap);
  FRawData.Clear;
  FJmpData.Clear;
  if FDataStream = nil then Exit;
  StreamOffset := 0;
  FDataStream.Position := 0;
  MapIndex := 0;
  while StreamOffset < FDataStream.Size do
  begin
    Line.Address := FStartAddress + StreamOffset;
    Line.DataOffset := StreamOffset;
    Line.Style := lsRaw;
    Line.RawLength := Min(BytesInRow, FDataStream.Size - StreamOffset);
    Line.Comment := EmptyStr;
    Line.RawColor := clWindowText;
    Line.Description := EmptyStr;
    Line.JmpToAddr := 0;
    Line.LinkStart := 0;
    Line.LinkLength := 0;
    Line.Linked := False;

    if MapIndex < FDataMap.Data.Count then
    begin
      if (FDataMap.Data[MapIndex].Address = Line.Address) or
        (FDataMap.Data[MapIndex].Style in [lsCheck, lsRadio]) then
      begin
        RawLength := Min(FDataMap.Data[MapIndex].RawLength,
          FDataStream.Size - StreamOffset);
        Line.Description := FDataMap.Data[MapIndex].Description;
        Line.Comment := FDataMap.Data[MapIndex].Comment;
        Line.Style := FDataMap.Data[MapIndex].Style;
        Line.RawColor := FDataMap.Data[MapIndex].RawColor;

        case Line.Style of
          lsRawWithExDescription, lsAsm:
          begin
            Line.JmpToAddr := FDataMap.Data[MapIndex].JmpToAddr;
            Line.LinkStart := FDataMap.Data[MapIndex].LinkStart;
            Line.LinkLength := FDataMap.Data[MapIndex].LinkLength;
          end;
          lsCheck, lsRadio:
          begin
            // определяем на какой элемент ссылается чек
            // для этого пропускаем все элементы на которые ссылка не может происходить
            for I := FRawData.Count - 1 downto 0 do
              if not (FRawData.List[I].Style in [lsNone, lsLine, lsCheck, lsRadio]) then
              begin
                Line.LineIndex := I;
                Break;
              end;
            Line.CharIndex := FDataMap.Data[MapIndex].CharIndex;
            Line.Checked := FDataMap.Data[MapIndex].Checked;
          end;
        end;

        Inc(MapIndex);
        while RawLength > BytesInRow do
        begin
          Line.RawLength := BytesInRow;
          FRawData.Add(Line);
          Line.Description := EmptyStr;
          Line.Comment := EmptyStr;
          Line.JmpToAddr := 0;
          Line.LinkStart := 0;
          Line.LinkLength := 0;
          Dec(RawLength, BytesInRow);
          Inc(StreamOffset, Line.RawLength);
          Line.Address := FStartAddress + StreamOffset;
        end;
        Line.RawLength := RawLength;
      end
      else
        if Line.Address + Line.RawLength > FDataMap.Data[MapIndex].Address then
          Line.RawLength := FDataMap.Data[MapIndex].Address - Line.Address;
    end;

    FRawData.Add(Line);
    Inc(StreamOffset, Line.RawLength);
  end;

  // заполняем массив известных прыжков
  for I := 0 to FRawData.Count - 1 do
    if FRawData.List[I].JmpToAddr <> 0 then
      if FRawData.List[I].Style in [lsRawWithExDescription, lsAsm] then
      begin
        JmpData.JmpFrom := I;
        JmpData.JmpTo := AddressToLineIndex(FRawData.List[I].JmpToAddr);
        if JmpData.JmpTo >= 0 then
          FRawData.List[JmpData.JmpTo].Linked := True;
        FJmpData.Add(JmpData);
      end;
  FJmpData.Sort;
end;

procedure TFWCustomHexView.UpdateScrollPos;
var
  ScrollInfo : TScrollInfo;
  MaxOffset: Integer;
begin
  if (ScrollBars = TScrollStyle.ssNone) or not HandleAllocated then Exit;

  MaxOffset := -(FTextBoundary.X - ClientWidth);
  if FScrollOffset.X < MaxOffset then
    FScrollOffset.X := MaxOffset;
  if FScrollOffset.X >= -BORDER_MARGIN then
    FScrollOffset.X := 0;

  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nPos := -FScrollOffset.X;
  ScrollInfo.nPage := ClientWidth;
  ScrollInfo.nMax := FTextBoundary.X;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

  MaxOffset := -(FTextBoundary.Y - ClientHeight);
  if FScrollOffset.Y < MaxOffset then
    FScrollOffset.Y := MaxOffset;
  if FScrollOffset.Y > 0 then
    FScrollOffset.Y := 0;

  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nPos := -FScrollOffset.Y;
  ScrollInfo.nPage := GetPageHeight;
  ScrollInfo.nMax := FTextBoundary.Y;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TFWCustomHexView.UpdateSelectedPosition(Value: TSelectPoint);
begin
  if Value.LineIndex < 0 then Exit;
  if FRawData.Count = 0 then Exit;
  if not (FRawData.List[Value.LineIndex].Style in [lsRaw..lsAsm]) then Exit;
  FEditPosData.LineIndex := Value.LineIndex;
  FEditPosData.Column := Value.Column;
  FEditPosData.Showed := Value.ValidSelectedByte;
  if FEditPosData.Showed then
  begin
    if FEditPosData.Column = ctOpcode then
    begin
      FEditPosData.OpcodeCharIndex := DblSize(Value.ByteIndex);
      if not (csLeftCharSelected in FStates) then
        Inc(FEditPosData.OpcodeCharIndex);
    end
    else
      FEditPosData.DescriptionByteIndex := Value.ByteIndex;
  end;
end;

procedure TFWCustomHexView.UpdateTextBoundary;
var
  I: TColumnType;
begin
  FTextBoundary.X := 0;
  for I := Low(TColumnType) to High(TColumnType) do
    Inc(FTextBoundary.X, FHeader.ColumnWidth[I]);
  Inc(FTextBoundary.X, BORDER_MARGIN);
  FTextBoundary.Y := FRawData.Count * FLineHeight +
    ClientHeight mod FLineHeight + FLineHeight;
  if not Header.Visible then
    Dec(FTextBoundary.Y, FLineHeight);
end;

end.
