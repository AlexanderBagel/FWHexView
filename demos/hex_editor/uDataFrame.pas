unit uDataFrame;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, Generics.Collections, Math,

  FWHexView,
  FWHexView.Common,
  uDataStream,
  uFindData, Types;

type
  TUndoRec = record
    AddrVA: Int64;
    Buff, OrigBuf: TBytes;
    // эти три параметра для восстановления позиции курсора при Undo
    RowIndex: Int64;
    Column: TColumnType;
    CharIndex: Integer;
  end;

  TSearchStyle = (ssNew, ssContinue, ssSelAndContinue);

  { TPageFrame }

  TPageFrame = class(TFrame)
    HexView: TFWHexView;
    procedure HexViewEdit(Sender: TObject; const ACursor: TDrawParam;
      const AData: TEditParam; var Handled: Boolean);
    procedure HexViewDrawToken(Sender: TObject; ACanvas: TCanvas;
      ATokenParam: TDrawParam; const ARect: TRect; AToken: PChar;
      var ATokenLen: Integer);
    procedure HexViewDrawColumnBackground(Sender: TObject; ACanvas: TCanvas;
      ARowParam: TDrawParam; const ARect: TRect; var Handled: Boolean);
  private
    FFilePath: string;
    FStream: TStream;
    FOrigBuffer, FBuffer: TBufferedStream;
    FStack: TListEx<TUndoRec>;
    FHighlightBuff: array of Boolean;
    FHighlightBuffLen: Integer;
    FSavedIndex, FTopIndex: Integer;
    FReadOnly: Boolean;
    FSearchBuff: TBytes;
    FSearchPos: Int64;
    FHighAddress: Int64;
    FHasSearchResult: Boolean;
    procedure RefreshView;
    function GetSelBkColor(DefSelection: Boolean): TColor;
    function GetHighlightBuff(AddrVA: Int64): Boolean;
    procedure PushToUndoStack(const Value: TUndoRec);
    procedure OnBuffUpdate(AddrVA: Int64; ASize: Integer; pBuff: PByte);
    function SearchStep(Data: Pointer; DataSize: Int64): Boolean;
    procedure SearchAtSearchPos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Active: Boolean;
    function BookmarkPresent(Index: Integer): Boolean;
    procedure Close;
    function CanToggleBookmark: Boolean;
    function FileMode: Boolean;
    function FileName: string;
    procedure FillZeros;
    procedure GotoBookmark(Index: Integer);
    procedure LoadSelection(Value: TStrings);
    function Modifyed: Boolean;
    procedure OpenFile(const FilePath: string);
    procedure OpenProcess(ProcessID: Integer);
    procedure Redo;
    function RedoPresent: Boolean;
    procedure Save(FilePath: string = '');
    procedure Search(ASearchStyle: TSearchStyle);
    function SearchNextAvailable: Boolean;
    procedure ToggleBookmark(Value: Integer);
    procedure Undo;
    function UndoPresent: Boolean;
    property ReadOnly: Boolean read FReadOnly;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

const
  SearchTag = 1;
  SearchSelTag = 2;
  SelectColorL = $CCFF;
  SelectColorD = $8FB2;
  SearchSelColorL = $98FB98;
  SearchSelColorD = $A07B51;

{ TPageFrame }

function TPageFrame.Active: Boolean;
begin
  Result := FStream <> nil;
end;

function TPageFrame.BookmarkPresent(Index: Integer): Boolean;
begin
  if Index in [0..9] then
    Result := HexView.Bookmark[Index] <> 0
  else
    Result := False;
end;

function TPageFrame.CanToggleBookmark: Boolean;
begin
  Result := HexView.CaretPosData.RowIndex >= 0;
end;

procedure TPageFrame.Close;
begin
  FreeAndNil(FOrigBuffer);
  FreeAndNil(FStream);
  FBuffer := nil;
  HexView.SetDataStream(nil, 0);
end;

constructor TPageFrame.Create(AOwner: TComponent);
begin
  inherited;
  FStack := TListEx<TUndoRec>.Create;
  SetLength(FHighlightBuff, HexView.BytesInRow);
end;

destructor TPageFrame.Destroy;
begin
  FStack.Free;
  Close;
  inherited;
end;

function TPageFrame.FileMode: Boolean;
begin
  Result := FStream is TFileStream;
end;

function TPageFrame.FileName: string;
begin
  Result := ExtractFileName(FFilePath);
end;

procedure TPageFrame.FillZeros;
var
  SelStart, SelEnd, Len: Int64;
  SelectPoint: TSelectPoint;
  Undo: TUndoRec;
begin
  SelStart := Min(HexView.SelStart, HexView.SelEnd);
  SelEnd := Max(HexView.SelStart, HexView.SelEnd);
  Len := SelEnd - SelStart + 1;
  if Len = 0 then Exit;
  SelectPoint := HexView.AddressToSelectPoint(SelStart);
  Undo.AddrVA := SelStart;
  Undo.RowIndex := SelectPoint.RowIndex;
  Undo.Column := SelectPoint.Column;
  Undo.CharIndex := SelectPoint.CharIndex;
  SetLength(Undo.OrigBuf, Len);
  FBuffer.Position := SelStart;
  Len := FBuffer.Read(Undo.OrigBuf[0], Len);
  if Len = 0 then Exit;
  SetLength(Undo.Buff, Len);
  PushToUndoStack(Undo);
end;

function TPageFrame.GetHighlightBuff(AddrVA: Int64): Boolean;
var
  I, Len: Integer;
  Metric: TValueMetric;
  CurrentBuf, RealBuff: TBytes;
begin
  Result := False;
  FHighlightBuffLen := 0;
  SetLength(FHighlightBuff, HexView.BytesInRow);
  Len := Min(HexView.BytesInRow, FStream.Size - AddrVA);
  SetLength(CurrentBuf, Len);
  FBuffer.Position := AddrVA;
  FBuffer.ReadBuffer(CurrentBuf[0], Len);
  SetLength(RealBuff, Len);
  FOrigBuffer.Position := AddrVA;
  FOrigBuffer.ReadBuffer(RealBuff[0], Len);
  FillChar(FHighlightBuff[0], Length(FHighlightBuff), 0);
  if CompareMem(@CurrentBuf[0], @RealBuff[0], Len) then Exit;
  Metric := DefValueMetric(HexView.ByteViewMode);
  I := 0;
  while I < Len do
  begin
    if not CompareMem(@CurrentBuf[I], @RealBuff[I], Metric.ByteCount) then
      FHighlightBuff[FHighlightBuffLen] := True;
    Inc(FHighlightBuffLen);
    Inc(I, Metric.ByteCount);
  end;
  Result := FHighlightBuffLen > 0;
end;

function TPageFrame.GetSelBkColor(DefSelection: Boolean): TColor;
begin
  if HexView.ColorMap.IsDarkMode then
  begin
    if DefSelection then
      Result := SelectColorD
    else
      Result := SearchSelColorD;
  end
  else
    if DefSelection then
      Result := SelectColorL
    else
      Result := SearchSelColorL;
end;

procedure TPageFrame.GotoBookmark(Index: Integer);
begin
  if (Index in [0..9]) and (HexView.Bookmark[Index] <> 0) then
    HexView.FocusOnAddress(HexView.Bookmark[Index], ccmSelectRow);
end;

procedure TPageFrame.HexViewDrawColumnBackground(Sender: TObject;
  ACanvas: TCanvas; ARowParam: TDrawParam; const ARect: TRect;
  var Handled: Boolean);
var
  DrawMetrics: TDrawMetrics;
begin
  case ARowParam.Column of
    ctNone: GetHighlightBuff(ARowParam.AddrVA);
    ctWorkSpace:
    begin
      if FHighlightBuffLen = 0 then Exit;
      ACanvas.Brush.Color := clRed;
      ACanvas.Brush.Style := bsSolid;
      HexView.FillDrawMetrics(DrawMetrics);
      PatBlt(ACanvas, ARect.Right - HexView.ToDpi(6) + DrawMetrics.TextMargin,
        ARect.Top, HexView.ToDpi(4), ARect.Height, PATCOPY);
    end;
  end;
end;

procedure TPageFrame.HexViewDrawToken(Sender: TObject; ACanvas: TCanvas;
  ATokenParam: TDrawParam; const ARect: TRect; AToken: PChar;
  var ATokenLen: Integer);
var
  Index: Integer;
  Metrik: TValueMetric;
  Current: Boolean;
  GlyphLen: Integer;
begin
  if FHighlightBuffLen = 0 then Exit;
  Metrik := DefValueMetric(HexView.ByteViewMode);
  Index := ATokenParam.ValueOffset div Metrik.ByteCount;
  Current := FHighlightBuff[Index];
  GlyphLen := IfThen(ATokenParam.Column = ctOpcode,
    Metrik.CharCount, Metrik.ByteCount);
  ATokenLen := GlyphLen;
  Inc(Index);
  while Index < FHighlightBuffLen do
  begin
    if FHighlightBuff[Index] = Current then
    begin
      Inc(ATokenLen, GlyphLen);
      Inc(Index);
    end
    else
      Break;
  end;
  if Current then
    ACanvas.Font.Color := clRed;
end;

procedure TPageFrame.HexViewEdit(Sender: TObject; const ACursor: TDrawParam;
  const AData: TEditParam; var Handled: Boolean);
var
  Undo: TUndoRec;
begin
  if not Handled then
  begin
    Undo.AddrVA := ACursor.AddrVA;
    Undo.RowIndex := ACursor.RowIndex;
    Undo.Column := ACursor.Column;
    Undo.CharIndex := ACursor.CharIndex;
    SetLength(Undo.Buff, AData.ValueSize);
    Move(AData.NewExtValue[0], Undo.Buff[0], AData.ValueSize);
    SetLength(Undo.OrigBuf, AData.ValueSize);
    Move(AData.OldExtValue[0], Undo.OrigBuf[0], AData.ValueSize);
    PushToUndoStack(Undo);
    Handled := True;
  end;
end;

procedure TPageFrame.LoadSelection(Value: TStrings);
var
  I: Integer;
begin
  Value.BeginUpdate;
  try
    Value.Clear;
    for I := 0 to HexView.Selections.Count - 1 do
      if HexView.Selections[I].Tag = SearchSelTag then
        Value.Add(IntToHex(HexView.Selections[I].SelStart));
  finally
    Value.EndUpdate;
  end;
end;

function TPageFrame.Modifyed: Boolean;
begin
  Result := Active and (FTopIndex <> FSavedIndex);
end;

procedure TPageFrame.OnBuffUpdate(AddrVA: Int64; ASize: Integer; pBuff: PByte);

  procedure PatchBuff(I: Integer);
  var
    MaxAddrVA, MoveLen, Idx: Integer;
    pByteValue: PByte;
  begin
    MaxAddrVA := AddrVA + ASize;
    MoveLen := Length(FStack.List[I].Buff);
    if (FStack.List[I].AddrVA >= AddrVA) and (FStack.List[I].AddrVA < MaxAddrVA) then
    begin
      pByteValue := pBuff + FStack.List[I].AddrVA - AddrVA;
      if FStack.List[I].AddrVA + MoveLen >= MaxAddrVA then
        MoveLen := MaxAddrVA - FStack.List[I].AddrVA;
      if I < FSavedIndex then
        Move(FStack.List[I].OrigBuf[0], pByteValue^, MoveLen)
      else
        Move(FStack.List[I].Buff[0], pByteValue^, MoveLen);
    end
    else
      if (FStack.List[I].AddrVA + MoveLen >= AddrVA) and (FStack.List[I].AddrVA + MoveLen < MaxAddrVA) then
      begin
        Idx := AddrVA - FStack.List[I].AddrVA;
        Dec(MoveLen, Idx);
        if MoveLen = 0 then Exit;
        if I < FSavedIndex then
          Move(FStack.List[I].OrigBuf[Idx], pBuff^, MoveLen)
        else
          Move(FStack.List[I].Buff[Idx], pBuff^, MoveLen);
      end;
  end;

var
  I: Integer;
begin
  if FSavedIndex <= FTopIndex then
  begin
    for I := FSavedIndex to FTopIndex - 1 do
      PatchBuff(I);
  end
  else
    for I := FSavedIndex - 1 downto FTopIndex do
      PatchBuff(I);
end;

procedure TPageFrame.OpenFile(const FilePath: string);
begin
  try
    FStream := TFileStream.Create(FilePath, fmOpenReadWrite or fmShareDenyWrite);
    HexView.ReadOnly := False;
  except
    on E: EFOpenError do
    begin
      FStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
      HexView.ReadOnly := True;
    end;
  end;
  FReadOnly := HexView.ReadOnly;
  FHighAddress := FStream.Size;
  FFilePath := FilePath;
  FOrigBuffer := TBufferedStream.Create(FStream);
  FBuffer := TBufferedStream.Create(FStream);
  FBuffer.OnUpdate := OnBuffUpdate;
  HexView.SetDataStream(FBuffer, 0, soOwned);
  TTabSheet(Parent).Caption := ExtractFileName(FilePath);
  if FHighAddress <= MAXBYTE then
  begin
    HexView.AddressMode := am8bit;
    HexView.Header.ColumnCaption[ctAddress] := '';
  end
  else if FHighAddress <= MAXWORD then
  begin
    HexView.AddressMode := am16bit;
    HexView.Header.ColumnCaption[ctAddress] := 'Addr';
  end
  else if FHighAddress <= MaxInt then
    HexView.AddressMode := am32bit
  else
    HexView.AddressMode := am64bit;
end;

procedure TPageFrame.OpenProcess(ProcessID: Integer);
begin

end;

procedure TPageFrame.PushToUndoStack(const Value: TUndoRec);
var
  I: Integer;
begin
  if FTopIndex < FStack.Count then
    for I := FStack.Count - 1 downto FTopIndex do
      FStack.Delete(I);
  FStack.Add(Value);
  FTopIndex := FStack.Count;
  RefreshView;
end;

procedure TPageFrame.Redo;
var
  AUndo: TUndoRec;
begin
  if FTopIndex = FStack.Count then Exit;
  AUndo := FStack[FTopIndex];
  HexView.FocusOnRow(AUndo.RowIndex, ccmNone);
  HexView.SetCaretPos(AUndo.Column, AUndo.RowIndex, AUndo.CharIndex);
  Inc(FTopIndex);
  RefreshView;
end;

function TPageFrame.RedoPresent: Boolean;
begin
  Result := FTopIndex < FStack.Count;
end;

procedure TPageFrame.RefreshView;
begin
  FBuffer.Invalidate;
  HexView.Invalidate;
end;

procedure TPageFrame.Save(FilePath: string);

  procedure PatchBuff(I: Integer);
  begin
    FStream.Position := FStack.List[I].AddrVA;
    if I < FSavedIndex then
      FStream.WriteBuffer(FStack.List[I].OrigBuf[0], Length(FStack.List[I].Buff))
    else
      FStream.WriteBuffer(FStack.List[I].Buff[0], Length(FStack.List[I].Buff));
  end;

var
  I: Integer;
  NewStream: TFileStream;
begin
  if FilePath <> '' then
  begin
    NewStream := TFileStream.Create(FilePath, fmCreate);
    NewStream.CopyFrom(FStream, 0);
    FStream.Free;
    FStream := NewStream;
    FBuffer.Stream := FStream;
    FOrigBuffer.Stream := FStream;
  end;

  if FSavedIndex <= FTopIndex then
  begin
    for I := FSavedIndex to FTopIndex - 1 do
      PatchBuff(I);
  end
  else
    for I := FSavedIndex - 1 downto FTopIndex do
      PatchBuff(I);

  FSavedIndex := FTopIndex;
  FOrigBuffer.Invalidate;
  RefreshView;
end;

procedure TPageFrame.Search(ASearchStyle: TSearchStyle);
begin
  if (ASearchStyle <> ssNew) and not SearchNextAvailable then
    ASearchStyle := ssNew;
  case ASearchStyle of
    ssNew:
    begin
      dlgFindData := TdlgFindData.Create(Self);
      if dlgFindData.GetSearchBuff(FSearchBuff) then
      begin
        FSearchPos := 0;
        FHasSearchResult := False;
        SearchAtSearchPos;
      end;
    end;
    ssContinue: SearchAtSearchPos;
    ssSelAndContinue:
    begin
      HexView.Selections.DropSelectionsAtTag(SearchTag);
      HexView.Selections.Add(SearchSelTag,
        FSearchPos - Length(FSearchBuff), FSearchPos - 1, GetSelBkColor(False));
      SearchAtSearchPos;
    end;
  end;

end;

procedure TPageFrame.SearchAtSearchPos;
var
  Buff: array of Byte;
  Size: Int64;
begin
  Size := 4096;
  SetLength(Buff, Size);
  FBuffer.Position := FSearchPos;
  while FSearchPos < FHighAddress - Length(FSearchBuff) do
  begin
    Size := FBuffer.Read(Buff[0], Size);
    if Size = 0 then Break;
    if SearchStep(@Buff[0], Size) then
    begin
      FHasSearchResult := True;
      Break;
    end;
    Inc(FSearchPos, Size - Length(FSearchBuff));
    FBuffer.Position := FSearchPos;
  end;
  if (Size = 0) or (FSearchPos >= FHighAddress - Length(FSearchBuff)) then
  begin
    if FHasSearchResult then
      Application.MessageBox('No data available', PChar(Caption), MB_ICONINFORMATION)
    else
      Application.MessageBox('All data found', PChar(Caption), MB_ICONINFORMATION);
    FSearchPos := 0;
  end;
end;

function TPageFrame.SearchNextAvailable: Boolean;
begin
  Result := FSearchPos > 0;
end;

function TPageFrame.SearchStep(Data: Pointer; DataSize: Int64): Boolean;
var
  pRemote, pSearch, pTmp: PByte;
  I, A: NativeInt;
begin
  Result := False;
  pRemote := Data;
  pSearch := @FSearchBuff[0];
  for I := 0 to DataSize - 1 do
  begin
    if pRemote^ <> pSearch^ then
    begin
      Inc(pRemote);
      Continue;
    end;
    pTmp := pRemote;
    Inc(pRemote);
    for A := 1 to Length(FSearchBuff) - 1 do
    begin
      if I + A >= DataSize then Exit(False);
      if pTmp^ <> pSearch^ then
        Break
      else
      begin
        Inc(pTmp);
        Inc(pSearch);
      end;
    end;
    Result := pTmp^ = pSearch^;
    if Result then
    begin
      HexView.Selections.DropSelectionsAtTag(SearchTag);
      HexView.Selections.Add(SearchTag, FSearchPos + I,
        FSearchPos + I + Length(FSearchBuff) - 1, GetSelBkColor(True));
      HexView.FocusOnAddress(FSearchPos + I, ccmSetNewSelection);
      Inc(FSearchPos, I + Length(FSearchBuff));
      Exit;
    end
    else
      pSearch := @FSearchBuff[0];
  end;
end;

procedure TPageFrame.ToggleBookmark(Value: Integer);
var
  BookmarkAddr: Int64;
begin
  BookmarkAddr := HexView.RowToAddress(HexView.CaretPosData.RowIndex, 0);
  if BookmarkAddr = HexView.Bookmark[Value] then
    HexView.Bookmark[Value] := 0
  else
    HexView.Bookmark[Value] := BookmarkAddr;
end;

procedure TPageFrame.Undo;
var
  AUndo: TUndoRec;
begin
  if FTopIndex = 0 then Exit;
  AUndo := FStack[FTopIndex - 1];
  HexView.FocusOnRow(AUndo.RowIndex, ccmNone);
  HexView.SetCaretPos(AUndo.Column, AUndo.RowIndex, AUndo.CharIndex);
  Dec(FTopIndex);
  RefreshView;
end;

function TPageFrame.UndoPresent: Boolean;
begin
  Result := FTopIndex > 0;
end;

end.

