////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.Common.pas
//  * Purpose   : Common auxiliary code for all classes
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 2.0.14
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

unit FWHexView.Common;

{$UNDEF EXTENDED_RTL}
{$IFDEF FPC}
  {$I FWHexViewConfig.inc}
{$ELSE}
  {$DEFINE EXTENDED_RTL}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLType,
  LCLIntf,
  {$ELSE}
  Classes,
  UITypes,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Types,
  Graphics,
  SysUtils,
  Math,
  {$IFDEF USE_CAIRO}
  FWHexView.Cairo,
  {$ENDIF}
  Generics.Collections;

{$IFDEF FPC}
type
  TArithmeticExceptionMask = TFPUExceptionMask;

const
  exAllArithmeticExceptions = [
    exInvalidOp,
    exDenormalized,
    exZeroDivide,
    exOverflow,
    exUnderflow,
    exPrecision];
{$ENDIF}

type
  TListEx<T> = class(TList<T>)
  {$IFNDEF EXTENDED_RTL}
  protected type
    TArrayOfT = array of T;
  public
    property List: TArrayOfT read FItems;
  {$ENDIF}
  end;

type
  ///<summary> TAddressMode - 8/16/32/64 bit address display mode. </summary>
  TAddressMode = (am8bit, am16bit, am32bit, am64bit);
  ///<summary> TAddressView - HEX/Decimal address display mode. </summary>
  TAddressView = (avHex, avDecimal);

  ///<summary> TColumnType - available column types. </summary>
  TColumnType =
    (ctNone, ctWorkSpace, ctJmpLine, ctAddress, ctOpcode, ctDescription, ctComment);
  ///<summary> TFWHexViewColumnTypes - Displayed column types.</summary>
  TFWHexViewColumnTypes = set of TColumnType;

  /// <summary> Data copying style
  ///  csAsText - all columns with comments
  ///  csAddress - selection start address
  ///  csBytes - only data in the form that a particular editor allows.
  ///            for RegView - value of the first register in the selection
  ///            for StackView - value of the first field in the selection
  ///            for other editors - just an array of bytes without taking into account the current ByteViewMode
  ///  csPascal - byte array as pascal code
  ///  csCpp - byte array as C code
  ///  csAsmOpcodes - DB opcode array: DB $xx, $xx, $xx...
  /// </summary>
  TCopyStyle = (csAsText, csAddress, csBytes, csPascal, csCpp, csAsmOpcodes);
  TCopyStyles = set of TCopyStyle;

  TVisibleRowDiapason = record
    StartRow: Int64;
    EndRow: Int64;
  end;

const
  TwoHexLookup : packed array[0..255] of array[1..2] of Char =
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

type
  TSimplyStringBuilder = class
  strict private
    FData: TList<string>;
    FLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(const Value: string);
    function AsString(Inverted: Boolean): string;
  end;

  ///<summary> TByteViewMode - data display mode. </summary>
  TByteViewMode = (
    bvmHex8, bvmHex16, bvmHex32, bvmHex64,
    bvmInt8, bvmInt16, bvmInt32, bvmInt64,
    bvmUInt8, bvmUInt16, bvmUInt32, bvmUInt64,
    bvmFloat32, bvmFloat64, bvmFloat80,
    bvmText,
    bvmAddress);

  TValueMetric = record
    CharCount: Integer; // number of characters allocated for value output
    ByteCount: Integer; // number of bytes representing the value
  end;

  TFormatMode = record
    Align: Boolean;
    Inverted: Boolean;
    Divide: Boolean;
  end;

const
  DefFormatMode: TFormatMode = (
    Align: True;        // aligning the result to the metric value
    Inverted: False;    // parts of the result are inverted
    Divide: False;      // separators have been added between parts
  );

  RegFormatMode: TFormatMode = (
    Align: True;
    {$IF DEFINED (FPC) AND DEFINED (FPC_BIG_ENDIAN)}
    Inverted: False;
    {$ELSE}
    Inverted: True;
    {$ENDIF}
    Divide: True;
  );

  RegFormatModeNoAlign: TFormatMode = (
    Align: False;
    {$IF DEFINED (FPC) AND DEFINED (FPC_BIG_ENDIAN)}
    Inverted: False;
    {$ELSE}
    Inverted: True;
    {$ENDIF}
    Divide: False;
  );

  // HexView Change Codes
  cmHeader            = 0;
  cmHandle            = 1;
  cmFont              = 2;
  cmData              = 3;
  cmAddressMode       = 4;
  cmAddressView       = 5;
  cmBookmark          = 6;
  cmBytesInColorGroup = 7;
  cmBytesInGroup      = 8;
  cmBytesInRow        = 9;
  cmByteViewMode      = 10;
  cmColorMap          = 11;
  cmEncoding          = 12;
  cmScrollX           = 14;
  cmScrollY           = 15;
  cmDiapasoneChange   = 16;

  function DefValueMetric(ViewMode: TByteViewMode): TValueMetric;
  function RawBufToViewMode(Value: PByte; nSize: Integer;
    const Metric: TValueMetric; ViewMode: TByteViewMode;
    const FormatMode: TFormatMode): string;
  function RawBufToHex(Value: PByte; nSize: Integer;
    Inverted: Boolean = False): string;
  function RawBufToAsmDB(Value: PByte; nSize: Integer): string;
  function RawBufToCPPArray(Value: PByte; nSize: Integer): string;
  function RawBufToPasArray(Value: PByte; nSize: Integer): string;
  procedure PatBlt(ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight, ARop: Integer);{$IFNDEF FPC}inline;{$ENDIF}
  function IsColorRefDark(Value: LongInt): Boolean;

type
  TScrollDirection = (sdLeft, sdRight, sdUp, sdDown);

  procedure DrawArrow(ACanvas: TCanvas; Direction: TScrollDirection; Location: TPoint; Size: Integer);

type
  PExtended80Support = ^TExtended80Support;
  TExtended80Support = packed record
    case Integer of
      0: (Frac: UInt64; Exp: UInt16);
      1: (Words: array [0..4] of Word);
      2: (Bytes: array [0..9] of Byte);
  end;

  function Extended80Mantissa(const Value: TExtended80Support): UInt64;
  function Extended80Sign(const Value: TExtended80Support): Byte;
  function Extended80ToDouble(const Value: TExtended80Support): Double;

type
  TFloatType = (
    ftInfinity,
    ftNegInfinity,
    ftSNaN,
    ftQNaN,
    ftNormalized,
    ftDenormal,
    ftZero,
    ftInvalid
  );

  function GetFloatType(const Value: TExtended80Support): TFloatType;

const
  TByteViewModeStr: array [TByteViewMode] of string = (
    'Hex Byte (8-bit)',
    'Hex Short (16-bit)',
    'Hex Long (32-bit)',
    'Hex Long Long (64-bit)',
    'Signed Byte (8-bit)',
    'Signed Short (16-bit)',
    'Signed Long (32-bit)',
    'Signed Long Long (64-bit)',
    'Unsigned Byte (8-bit)',
    'Unsigned Short (16-bit)',
    'Unsigned Long (32-bit)',
    'Unsigned Long Long (64-bit)',
    'Float (32-bit)',
    'Double (64-bit)',
    'Long Double (80-bit)',
    'Text',
    'Address');

  function ExtractExtended80Fmt(const Value: TExtended80Support): string;

  function DrawText(ACanvas: TCanvas; Str: string; Count: Integer;
    var ARect: TRect; Flags: Cardinal): Integer; inline;
  function ExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
    ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean; inline;

implementation

function ExtractExtended80Fmt(const Value: TExtended80Support): string;
begin
  case GetFloatType(Value) of
    ftInfinity: Exit('Infinity');
    ftNegInfinity: Exit('Neg Infinity');
    ftSNaN: Exit('SNaN');
    ftQNaN: Exit('QNaN');
    ftZero: Exit('0');
    ftInvalid: Exit('Invalid');
  else
    {$IF SizeOf(Extended) = 8}
    Result := FloatToStr(Extended80ToDouble(Value));
    {$ELSE}
    Result := FloatToStr(PExtended(@Value)^);
    {$ENDIF}
  end;
end;

function DrawText(ACanvas: TCanvas; Str: string; Count: Integer;
  var ARect: TRect; Flags: Cardinal): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.DrawText(ACanvas.Handle, PChar(Str), Count, ARect, Flags);
  {$ENDIF}
  {$IFDEF LINUX}
    {$IFDEF USE_CAIRO}
    Result := CairoDrawText(ACanvas, PChar(Str), ARect, Flags);
    {$ELSE}
    Result := LCLIntf.DrawText(ACanvas.Handle, PChar(Str), Count, ARect, Flags);
    {$ENDIF}
  {$ENDIF}
end;

function ExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
  ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF FPC}
    Result := Windows.ExtTextOutW(ACanvas.Handle, X, Y, Options, ARect,
      PWideChar(UnicodeString(Str)), Count, Dx);
    {$ELSE}
    Result := Windows.ExtTextOut(ACanvas.Handle, X, Y, Options, ARect, Str, Count, Dx);
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
    {$IFDEF USE_CAIRO}
    Result := CairoExtTextOut(ACanvas, X, Y, Options, ARect, Str, Count, Dx);
    {$ELSE}
    Result := LCLIntf.ExtTextOut(ACanvas.Handle, X, Y, Options, ARect, Str, Count, Dx);
    {$ENDIF}
  {$ENDIF}
end;

function Extended80Mantissa(const Value: TExtended80Support): UInt64;
begin
  Result := PUInt64(@Value)^;
end;

function Extended80Sign(const Value: TExtended80Support): Byte;
begin
  Result := Value.Bytes[9] shr 7;
end;

function Extended80ToDouble(const Value: TExtended80Support): Double;
var
  Exp: Word;
  Sign: Byte;
  UIntRes: UInt64;
begin
  Exp := Value.Words[4];
  Sign := Exp shr 15;
  if Exp and $7FFF >= 15360 then
  begin
    UIntRes := (Exp and $7FFF) - 15360;
    UIntRes := UIntRes + (Sign shl 11);
    UIntRes := UIntRes shl 52;
    PUInt64(@Result)^ := UIntRes + ((PUInt64(@Value)^ shr 11) and $FFFFFFFFFFFFF);
  end
  else
  begin
    UIntRes := ((PUInt64(@Value)^ shr 12) and $FFFFFFFFFFFFF);
    Exp := Exp and $7FFF;
    while Exp < 15360 do
    begin
      UIntRes := UIntRes shr 1;
      if UIntRes = 0 then
        Exit(0);
      Inc(Exp);
    end;
    PUInt64(@Result)^ := UIntRes;
    if Sign = 1 then
      Result := -Result;
  end;
end;

type
  TUint64Rec = record
    case Integer of
      0: (Val64: UInt64);
      1: (LowPart, HiPart: Cardinal);
  end;

// based on https://github.com/x64dbg/ldconvert/blob/master/ldconvert.cpp
function GetFloatType(const Value: TExtended80Support): TFloatType;
var
  Uint64Rec: TUint64Rec;
  Bit61_0, Bit62, Bit63: Boolean;
begin
  Uint64Rec.Val64 := Extended80Mantissa(Value);
  Bit63 := (Uint64Rec.HiPart shr 31) = 1;
  case Value.Words[4] and $7FFF of
    0:
    begin
      if Bit63 then
        Result := ftDenormal
      else
        if (Uint64Rec.LowPart = 0) and (Uint64Rec.HiPart and $7FFFFFFF = 0) then // Bits 62..0: Zero
          Result := ftZero
        else
          Result := ftDenormal;
    end;
    $7FFF:
    begin
      Bit62 := Uint64Rec.HiPart and $40000000 <> 0;
      Bit61_0 := ((Uint64Rec.HiPart and $3FFFFFFF) = 0) and (Uint64Rec.LowPart = 0);
      if Bit63 then
      begin
        if Bit62 then // Bits 63, 62: 11
          Result := ftQNaN
        else // Bits 63, 62: 10
        begin
          if Bit61_0 then // Bits 61..0: Zero
          begin
            if Extended80Sign(Value) = 1 then
              Result := ftNegInfinity
            else
              Result := ftInfinity;
          end
          else // Bits 61..0: Non-zero
            Result := ftSNaN;
        end;
      end
      else
        Result := ftInvalid; // 63, 62: 01 or 00
    end;
  else
    if Bit63 then
      Result := ftNormalized
    else
      Result := ftInvalid;
  end;
end;

function DefValueMetric(ViewMode: TByteViewMode): TValueMetric;
begin
  case ViewMode of
    bvmHex8:
    begin
      Result.CharCount := 2;
      Result.ByteCount := 1;
    end;
    bvmHex16:
    begin
      Result.CharCount := 4;
      Result.ByteCount := 2;
    end;
    bvmHex32:
    begin
      Result.CharCount := 8;
      Result.ByteCount := 4;
    end;
    bvmHex64:
    begin
      Result.CharCount := 16;
      Result.ByteCount := 8;
    end;
    bvmInt8, bvmUInt8:
    begin
      Result.CharCount := 4;
      Result.ByteCount := 1;
    end;
    bvmInt16, bvmUInt16:
    begin
      Result.CharCount := 6;
      Result.ByteCount := 2;
    end;
    bvmInt32, bvmUInt32:
    begin
      Result.CharCount := 11;
      Result.ByteCount := 4;
    end;
    bvmInt64, bvmUInt64:
    begin
      Result.CharCount := 20;
      Result.ByteCount := 8;
    end;
    bvmFloat32:
    begin
      Result.CharCount := 16;
      Result.ByteCount := 4;
    end;
    bvmFloat64:
    begin
      Result.CharCount := 22;
      Result.ByteCount := 8;
    end;
    bvmFloat80:
    begin
      Result.CharCount := 27;
      Result.ByteCount := 10;
    end;
  else
    Result.CharCount := 1;
    Result.ByteCount := 1;
  end;
end;

function RawBufToInt(Value: PByte; nSize: Integer;
  const Metric: TValueMetric; ViewMode: TByteViewMode;
  const FormatMode: TFormatMode): string;

  function FillValueString(const Value: string): string;
  var
    AChar: Char;
  begin
    case ViewMode of
      bvmHex8..bvmHex64, bvmAddress: AChar := '0';
    else
      AChar := ' ';
    end;
    Result := StringOfChar(AChar, Metric.CharCount - Length(Value)) + Value;
  end;

  function ValueString(const Value: UInt64): string;
  begin
    case ViewMode of
      bvmHex8..bvmHex64, bvmAddress: Result := IntToHex(Value, 1);
      bvmInt8:  Result := IntToStr(ShortInt(Value));
      bvmInt16:  Result := IntToStr(SmallInt(Value));
      bvmInt32:  Result := IntToStr(Integer(Value));
      bvmInt64: Result := IntToStr(Int64(Value));
    else
      Result := UIntToStr(Value);
    end;
    if FormatMode.Align then
      Result := FillValueString(Result);
  end;

var
  I, UnmappedCount: Integer;
  Buff: UInt64;
  Builder: TSimplyStringBuilder;
begin
  Builder := TSimplyStringBuilder.Create;
  try
    UnmappedCount := nSize mod Metric.ByteCount;
    I := 0;
    while I < nSize - UnmappedCount do
    begin
      Buff := 0;
      Move(Value[I], Buff, Metric.ByteCount);
      if (I > 0) and FormatMode.Divide then
        Builder.Append(' ');
      Builder.Append(ValueString(Buff));
      Inc(I, Metric.ByteCount);
    end;
    if UnmappedCount > 0 then
    begin
      Buff := 0;
      Move(Value[I], Buff, UnmappedCount);
      Builder.Append(ValueString(Buff));
    end;
    Result := Builder.AsString(FormatMode.Inverted);
  finally
    Builder.Free;
  end;
end;

function RawBufToSingle(Value: PByte; nSize: Integer;
  const Metric: TValueMetric; const FormatMode: TFormatMode): string;

  function ValueString(const Value: Single): string;
  begin
    if Value = 0 then
      Result := '0'
    else
      Result := FloatToStr(Value);
    if FormatMode.Align then
      Result := StringOfChar(' ', Metric.CharCount - Length(Result)) + Result;
  end;

var
  I, UnmappedCount: Integer;
  Buff: Single;
  Builder: TSimplyStringBuilder;
begin
  Builder := TSimplyStringBuilder.Create;
  try
    UnmappedCount := nSize mod Metric.ByteCount;
    I := 0;
    while I < nSize - UnmappedCount do
    begin
      Buff := 0;
      Move(Value[I], Buff, Metric.ByteCount);
      if (I > 0) and FormatMode.Divide then
        Builder.Append(' ');
      Builder.Append(ValueString(Buff));
      Inc(I, Metric.ByteCount);
    end;
    if UnmappedCount > 0 then
    begin
      Buff := 0;
      Move(Value[I], Buff, UnmappedCount);
      Builder.Append(ValueString(Buff));
    end;
    Result := Builder.AsString(FormatMode.Inverted);
  finally
    Builder.Free;
  end;
end;

function RawBufToDouble(Value: PByte; nSize: Integer;
  const Metric: TValueMetric; const FormatMode: TFormatMode): string;

  function ValueString(const Value: Double): string;
  begin
    if Value = 0 then
      Result := '0'
    else
      Result := FloatToStr(Value);
    if FormatMode.Align then
      Result := StringOfChar(' ', Metric.CharCount - Length(Result)) + Result;
  end;

var
  I, UnmappedCount: Integer;
  Buff: Double;
  Builder: TSimplyStringBuilder;
begin
  Builder := TSimplyStringBuilder.Create;
  try
    UnmappedCount := nSize mod Metric.ByteCount;
    I := 0;
    while I < nSize - UnmappedCount do
    begin
      Buff := 0;
      Move(Value[I], Buff, Metric.ByteCount);
      if (I > 0) and FormatMode.Divide then
        Builder.Append(' ');
      Builder.Append(ValueString(Buff));
      Inc(I, Metric.ByteCount);
    end;
    if UnmappedCount > 0 then
    begin
      Buff := 0;
      Move(Value[I], Buff, UnmappedCount);
      Builder.Append(ValueString(Buff));
    end;
    Result := Builder.AsString(FormatMode.Inverted);
  finally
    Builder.Free;
  end;
end;

function RawBufToExtended(Value: PByte; nSize: Integer;
  const Metric: TValueMetric; const FormatMode: TFormatMode): string;

  function ValueString(const Value: TExtended80Support): string;
  begin
    Result := ExtractExtended80Fmt(Value);
    if FormatMode.Align then
      Result := StringOfChar(' ', Metric.CharCount - Length(Result)) + Result;
  end;

var
  I,  UnmappedCount: Integer;
  Buff: TExtended80Support;
  Builder: TSimplyStringBuilder;
begin
  Builder := TSimplyStringBuilder.Create;
  try
    UnmappedCount := nSize mod Metric.ByteCount;
    I := 0;
    while I < nSize - UnmappedCount do
    begin
      FillChar(Buff{%H-}, SizeOf(TExtended80Support), 0);
      Move(Value[I], Buff, Metric.ByteCount);
      if (I > 0) and FormatMode.Divide then
        Builder.Append(' ');
      Builder.Append(ValueString(Buff));
      Inc(I, Metric.ByteCount);
    end;
    if UnmappedCount > 0 then
    begin
      FillChar(Buff{%H-}, SizeOf(TExtended80Support), 0);
      Move(Value[I], Buff, UnmappedCount);
      Builder.Append(ValueString(Buff));
    end;
    Result := Builder.AsString(FormatMode.Inverted);
  finally
    Builder.Free;
  end;
end;

function RawBufToViewMode(Value: PByte; nSize: Integer;
  const Metric: TValueMetric; ViewMode: TByteViewMode;
  const FormatMode: TFormatMode): string;
var
  OldMask: TArithmeticExceptionMask;
begin
  OldMask := SetExceptionMask(exAllArithmeticExceptions);
  try
    case ViewMode of
      bvmHex8: Result := RawBufToHex(Value, nSize);
      bvmFloat32: Result := RawBufToSingle(Value, nSize, Metric, FormatMode);
      bvmFloat64: Result := RawBufToDouble(Value, nSize, Metric, FormatMode);
      bvmFloat80: Result := RawBufToExtended(Value, nSize, Metric, FormatMode);
    else
      Result := RawBufToInt(Value, nSize, Metric, ViewMode, FormatMode);
    end;
  finally
    SetExceptionMask(OldMask);
  end;
end;

function RawBufToHex(Value: PByte; nSize: Integer; Inverted: Boolean): string;
var
  I, A: Integer;
begin
  SetLength(Result{%H-}, nSize shl 1);
  A := nSize - 1;
  for I := 0 to A do
  begin
    if Inverted then
    begin
      Result[(A - I) shl 1 + 1] := TwoHexLookup[Value[I]][1];
      Result[(A - I) shl 1 + 2] := TwoHexLookup[Value[I]][2];
    end
    else
    begin
      Result[I shl 1 + 1] := TwoHexLookup[Value[I]][1];
      Result[I shl 1 + 2] := TwoHexLookup[Value[I]][2];
    end;
  end;
end;

function RawBufToAsmDB(Value: PByte; nSize: Integer): string;
const
  AsmNewLine = '    db ';
var
  I, A: Integer;
begin
  Result := '  asm' + sLineBreak;
  A := 0;
  for I := 0 to nSize - 1 do
  begin
    if A = 0 then
      Result := Result + AsmNewLine + '$' + TwoHexLookup[Value[I]]
    else
      Result := Result + ', $' + TwoHexLookup[Value[I]];
    Inc(A);
    if A = 8 then
    begin
      A := 0;
      Result := Result + sLineBreak;
    end;
  end;
  Result := Result + sLineBreak + '  end;';
end;

function RawBufToCPPArray(Value: PByte; nSize: Integer): string;
var
  I, A: Integer;
begin
  Result := 'char buffer[' +
    IntToStr(nSize) + '] = {' + sLineBreak + '    ';
  A := 0;
  for I := 0 to nSize - 1 do
  begin
    Result := Result + '0x' + TwoHexLookup[Value[I]];
    if I < nSize - 1 then
      Result := Result + ', ';
    Inc(A);
    if A = 14 then
    begin
      A := 0;
      Result := Result + sLineBreak + '    ';
    end;
  end;
  Result := Result + '};';
end;

function RawBufToPasArray(Value: PByte; nSize: Integer): string;
var
  I, A: Integer;
begin
  Result := 'const' + sLineBreak + '  Buffer: array[0..' +
    IntToStr(nSize - 1) + '] of Byte = (' + sLineBreak + '    ';
  A := 0;
  for I := 0 to nSize - 1 do
  begin
    Result := Result + '$' + TwoHexLookup[Value[I]];
    if I < nSize - 1 then
      Result := Result + ', ';
    Inc(A);
    if A = 14 then
    begin
      A := 0;
      Result := Result + sLineBreak + '    ';
    end;
  end;
  Result := Result + ');';
end;

procedure PatBlt(ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight, ARop: Integer);
{$IFDEF FPC}
var
  R: TRect;
  B: TBitmap;
begin
  R.Left := ALeft;
  R.Top := ATop;
  R.Width := AWidth;
  R.Height := AHeight;
  R.NormalizeRect;
  case ARop of
    BLACKNESS:
    begin
      ACanvas.Brush.Color := clBlack;
      ACanvas.FillRect(R);
    end;
    WHITENESS:
    begin
      ACanvas.Brush.Color := clWhite;
      ACanvas.FillRect(R);
    end;
    PATCOPY: ACanvas.FillRect(R);
    PATINVERT:
    begin
      B := TBitmap.Create;
      try
        B.PixelFormat := pf32bit;
        B.SetSize(AWidth, AHeight);
        BitBlt(B.Canvas.Handle, 0, 0, AWidth, AHeight, ACanvas.Handle, ALeft, ATop, SRCCOPY);
        BitBlt(ACanvas.Handle, ALeft, ATop, AWidth, AHeight, ACanvas.Handle, 0, 0, SRCINVERT);
      finally
        B.Free;
      end;
    end;
  end;
end;
{$ELSE}
begin
  Windows.PatBlt(ACanvas.Handle, ALeft, ATop, AWidth, AHeight, ARop);
end;
{$ENDIF}

function IsColorRefDark(Value: LongInt): Boolean;
begin
  Result := (
    GetRValue(Value) * 30 +
    GetGValue(Value) * 59 +
    GetBValue(Value) * 11)  div 100 <= 130;
end;

procedure DrawArrow(ACanvas: TCanvas; Direction: TScrollDirection;
  Location: TPoint; Size: Integer);
const
  ArrowPts: array[TScrollDirection, 0..2] of TPoint =
    (((X:1; Y:0), (X:0; Y:1), (X:1; Y:2)),
     ((X:0; Y:0), (X:1; Y:1), (X:0; Y:2)),
     ((X:0; Y:1), (X:1; Y:0), (X:2; Y:1)),
     ((X:0; Y:0), (X:1; Y:1), (X:2; Y:0)));
var
  I: Integer;
  Pts: array[0..2] of TPoint;
  OldWidth: Integer;
  OldColor: TColor;
begin
  if ACanvas = nil then exit;
  OldColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := ACanvas.Pen.Color;
  for I := 0 to 2 do
    Pts[I] := Point(ArrowPts[Direction,I].x * Size + Location.X,
       ArrowPts[Direction,I].y * Size + Location.Y);
  with ACanvas do
  begin
    OldWidth := Pen.Width;
    Pen.Width := 1;
    Polygon(Pts);
    Pen.Width := OldWidth;
    Brush.Color := OldColor;
  end;
end;

{ TSimplyStringBuilder }

procedure TSimplyStringBuilder.Append(const Value: string);
begin
  FData.Add(Value);
  Inc(FLength, Length(Value));
end;

function TSimplyStringBuilder.AsString(Inverted: Boolean): string;
var
  I, Cursor, Len: Integer;
begin
  SetLength(Result{%H-}, FLength);
  Cursor := 1;
  if Inverted then
  begin
    for I := FData.Count - 1 downto 0 do
    begin
      Len := Length(FData[I]);
      Move(FData[I][1], Result[Cursor], Len * SizeOf(Char));
      Inc(Cursor, Len);
    end;
  end
  else
    for I := 0 to FData.Count - 1 do
    begin
      Len := Length(FData[I]);
      if Len > 0 then
      begin
        Move(FData[I][1], Result[Cursor], Len * SizeOf(Char));
        Inc(Cursor, Len);
      end;
    end;
end;

constructor TSimplyStringBuilder.Create;
begin
  FData := TList<string>.Create;
end;

destructor TSimplyStringBuilder.Destroy;
begin
  FData.Free;
  inherited;
end;

end.
