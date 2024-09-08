unit uMain;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF FPC}
  LCLType,
  {$ELSE}
  Windows,
  Messages,
  Actions,
  UITypes,
  {$ENDIF}
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList,
  Menus, ComCtrls, Math,

  FWHexView.Common,
  FWHexView,
  uDataFrame,
  uSelections;

type

  { TdlgHexEditor }

  TdlgHexEditor = class(TForm)
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    ActionList: TActionList;
    PageControl: TPageControl;
    File1: TMenuItem;
    acUndo: TAction;
    acRedo: TAction;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    acRedo1: TMenuItem;
    acSave: TAction;
    acSaveAs: TAction;
    acSaveAll: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    acOpen: TAction;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    SaveAll1: TMenuItem;
    acClose: TAction;
    acCloseAll: TAction;
    N1: TMenuItem;
    Close1: TMenuItem;
    CloseAll1: TMenuItem;
    N2: TMenuItem;
    acExit: TAction;
    Exit1: TMenuItem;
    acEncAnsi: TAction;
    acEncAscii: TAction;
    acEncUnicode: TAction;
    acEncUnicodeBE: TAction;
    acEncUtf7: TAction;
    acEncUtf8: TAction;
    Search1: TMenuItem;
    View1: TMenuItem;
    N1251ANSICyrillic1: TMenuItem;
    N1251ANSICyrillic2: TMenuItem;
    N20127USASCII1: TMenuItem;
    N1200Unicode1: TMenuItem;
    N1201UnicodeBigEndian1: TMenuItem;
    N65000UTF71: TMenuItem;
    N65001UTF81: TMenuItem;
    Displaymode1: TMenuItem;
    acBvmHex8: TAction;
    acBvmHex16: TAction;
    acBvmHex32: TAction;
    acBvmHex64: TAction;
    acBvmInt8: TAction;
    acBvmInt16: TAction;
    acBvmInt32: TAction;
    acBvmInt64: TAction;
    acBvmUInt8: TAction;
    acBvmUInt16: TAction;
    acBvmUInt32: TAction;
    acBvmUInt64: TAction;
    acBvmFloat32: TAction;
    acBvmFloat64: TAction;
    acBvmText: TAction;
    HexByte8bit1: TMenuItem;
    HexShort16bit1: TMenuItem;
    HexLong32bit1: TMenuItem;
    HexLongLong64bit1: TMenuItem;
    N3: TMenuItem;
    SignedByte8bit1: TMenuItem;
    SignedShort16bit1: TMenuItem;
    SignedLong32bit1: TMenuItem;
    SignedLongLong64bit1: TMenuItem;
    N4: TMenuItem;
    UnsignedByte8bit1: TMenuItem;
    UnsignedShort16bit1: TMenuItem;
    UnsignedLong32bit1: TMenuItem;
    UnsignedLongLong64bit1: TMenuItem;
    N5: TMenuItem;
    Float32bit1: TMenuItem;
    Double64bit1: TMenuItem;
    N6: TMenuItem;
    ext1: TMenuItem;
    acCopy: TAction;
    acCopyAsText: TAction;
    acCopyAsCpp: TAction;
    acCopyAsPas: TAction;
    acCopyAsAsm: TAction;
    PopupMenu: TPopupMenu;
    Copy1: TMenuItem;
    CopyasText1: TMenuItem;
    CopyasCppArray1: TMenuItem;
    CopyasPasArray1: TMenuItem;
    CopyasAsmArray1: TMenuItem;
    CopyasArray1: TMenuItem;
    N7: TMenuItem;
    Copy2: TMenuItem;
    CopyasText2: TMenuItem;
    CopyasArray2: TMenuItem;
    CopyasPasArray2: TMenuItem;
    CopyasCppArray2: TMenuItem;
    CopyasAsmArray2: TMenuItem;
    acFind: TAction;
    Find1: TMenuItem;
    acFindNext: TAction;
    acFindNextSelect: TAction;
    FindNext1: TMenuItem;
    SelectandFindNext1: TMenuItem;
    acFillZeros: TAction;
    N8: TMenuItem;
    FillbyZeros1: TMenuItem;
    N9: TMenuItem;
    FillbyZeros2: TMenuItem;
    acToggleBookmark1: TAction;
    N10: TMenuItem;
    oggleBookmarks1: TMenuItem;
    oggleBookmark01: TMenuItem;
    acGotoBookmark1: TAction;
    acToggleBookmark2: TAction;
    acToggleBookmark3: TAction;
    acToggleBookmark4: TAction;
    acToggleBookmark5: TAction;
    acToggleBookmark6: TAction;
    acToggleBookmark7: TAction;
    acToggleBookmark8: TAction;
    acToggleBookmark9: TAction;
    acGotoBookmark2: TAction;
    acGotoBookmark3: TAction;
    acGotoBookmark4: TAction;
    acGotoBookmark5: TAction;
    acGotoBookmark6: TAction;
    acGotoBookmark7: TAction;
    acGotoBookmark8: TAction;
    acGotoBookmark9: TAction;
    Bookmark21: TMenuItem;
    Bookmark31: TMenuItem;
    Bookmark41: TMenuItem;
    Bookmark51: TMenuItem;
    Bookmark61: TMenuItem;
    Bookmark71: TMenuItem;
    Bookmark81: TMenuItem;
    Bookmark91: TMenuItem;
    GotoBookmarks1: TMenuItem;
    Bookmark11: TMenuItem;
    Bookmark22: TMenuItem;
    Bookmark32: TMenuItem;
    Bookmark42: TMenuItem;
    Bookmark52: TMenuItem;
    Bookmark62: TMenuItem;
    Bookmark72: TMenuItem;
    Bookmark82: TMenuItem;
    Bookmark92: TMenuItem;
    FitColumnstoBestSize1: TMenuItem;
    TabPopupMenu: TPopupMenu;
    acCloseRight: TAction;
    Close2: TMenuItem;
    CloseAll2: TMenuItem;
    CloseAlltotheRight1: TMenuItem;
    N11: TMenuItem;
    ShowSelected1: TMenuItem;
    acViewDisplayHex: TAction;
    acViewDisplayInt: TAction;
    acViewDisplayOffset: TAction;
    acViewMode8: TAction;
    acViewMode16: TAction;
    acViewMode32: TAction;
    acViewMode64: TAction;
    AddressViewMode1: TMenuItem;
    acViewMode81: TMenuItem;
    acViewMode161: TMenuItem;
    acViewMode321: TMenuItem;
    acViewMode641: TMenuItem;
    AddressDisplayMode1: TMenuItem;
    Hex1: TMenuItem;
    Int1: TMenuItem;
    OffsetfromCursorPos1: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    OffsetfromCursorPos2: TMenuItem;
    N15: TMenuItem;
    acBvmFloat80: TAction;
    Extended80bit1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure acRedoUpdate(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acUndoUpdate(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acSaveUpdate(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acCloseUpdate(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acSaveAllUpdate(Sender: TObject);
    procedure acSaveAllExecute(Sender: TObject);
    procedure acCloseAllExecute(Sender: TObject);
    procedure acEncAnsiExecute(Sender: TObject);
    procedure acEncAnsiUpdate(Sender: TObject);
    procedure acBvmHex8Update(Sender: TObject);
    procedure acBvmHex8Execute(Sender: TObject);
    procedure acCopyUpdate(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure acFillZerosUpdate(Sender: TObject);
    procedure acFillZerosExecute(Sender: TObject);
    procedure acToggleBookmark1Execute(Sender: TObject);
    procedure acToggleBookmark1Update(Sender: TObject);
    procedure acGotoBookmark1Update(Sender: TObject);
    procedure acGotoBookmark1Execute(Sender: TObject);
    procedure acCloseAllUpdate(Sender: TObject);
    procedure FitColumnstoBestSize1Click(Sender: TObject);
    procedure acCloseRightUpdate(Sender: TObject);
    procedure acCloseRightExecute(Sender: TObject);
    procedure ShowSelected1Click(Sender: TObject);
    procedure acViewMode8Update(Sender: TObject);
    procedure acViewMode8Execute(Sender: TObject);
    procedure acViewDisplayHexUpdate(Sender: TObject);
    procedure acViewDisplayHexExecute(Sender: TObject);
    procedure acViewDisplayOffsetUpdate(Sender: TObject);
    procedure acViewDisplayOffsetExecute(Sender: TObject);
  private
    function ActiveDoc: TPageFrame;
    function CreateNewFrame: TPageFrame;
    procedure OpenFile(const FilePath: string);
    procedure CloseFromPage(Index: Integer);
    function GetFrameAtIndex(Index: Integer): TPageFrame;
    function DoCloseView(Index: Integer; ShowYesToAll, ForceSave: Boolean): Integer;
    procedure SelectionsDblClick(Sender: TObject);
    procedure UpdateSelections;
    procedure UpdateStatusBar(Sender: TObject);
    procedure UpdateView;
  end;

var
  dlgHexEditor: TdlgHexEditor;

implementation

const
  NoData = 'No Data';

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

procedure TdlgHexEditor.acBvmHex8Execute(Sender: TObject);
begin
  ActiveDoc.HexView.ByteViewMode := TByteViewMode(TAction(Sender).Tag);
  UpdateView;
  UpdateStatusBar(nil);
end;

procedure TdlgHexEditor.acBvmHex8Update(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Active;
  TAction(Sender).Checked := TAction(Sender).Tag = Byte(ActiveDoc.HexView.ByteViewMode);
end;

procedure TdlgHexEditor.acCloseAllExecute(Sender: TObject);
begin
  CloseFromPage(0);
end;

procedure TdlgHexEditor.acCloseAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := PageControl.PageCount > 1;
end;

procedure TdlgHexEditor.acCloseExecute(Sender: TObject);
begin
  if DoCloseView(PageControl.ActivePageIndex, False, False) = IDCANCEL then Exit;
  if PageControl.PageCount > 1 then
    PageControl.ActivePage.Free
  else
    PageControl.Pages[0].Caption := NoData;
  UpdateSelections;
end;

procedure TdlgHexEditor.acCloseRightExecute(Sender: TObject);
begin
  CloseFromPage(PageControl.ActivePageIndex + 1);
end;

procedure TdlgHexEditor.acCloseRightUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := PageControl.ActivePageIndex < PageControl.PageCount - 1;
end;

procedure TdlgHexEditor.acCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Active;
end;

procedure TdlgHexEditor.acCopyExecute(Sender: TObject);
begin
  ActiveDoc.HexView.CopySelected(TCopyStyle(TAction(Sender).Tag));
end;

procedure TdlgHexEditor.acCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Active and
    (ActiveDoc.HexView.SelStart >= 0);
end;

procedure TdlgHexEditor.acEncAnsiExecute(Sender: TObject);
begin
  ActiveDoc.HexView.Encoder.EncodeType := TCharEncoderType(TAction(Sender).Tag);
  UpdateStatusBar(nil);
end;

procedure TdlgHexEditor.acEncAnsiUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Active;
  TAction(Sender).Checked := TAction(Sender).Tag = Byte(ActiveDoc.HexView.Encoder.EncodeType);
end;

procedure TdlgHexEditor.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TdlgHexEditor.acFillZerosExecute(Sender: TObject);
begin
  ActiveDoc.FillZeros;
end;

procedure TdlgHexEditor.acFillZerosUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Active and
    (ActiveDoc.HexView.SelStart <> ActiveDoc.HexView.SelEnd);
end;

procedure TdlgHexEditor.acFindExecute(Sender: TObject);
begin
  ActiveDoc.Search(TSearchStyle(TAction(Sender).Tag));
  UpdateSelections;
end;

procedure TdlgHexEditor.acGotoBookmark1Execute(Sender: TObject);
begin
  ActiveDoc.GotoBookmark(TAction(Sender).Tag);
end;

procedure TdlgHexEditor.acGotoBookmark1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.BookmarkPresent(TAction(Sender).Tag);
end;

procedure TdlgHexEditor.acOpenExecute(Sender: TObject);
var
  I: Integer;
begin
  if OpenDialog.Execute then
    for I := 0 to OpenDialog.Files.Count - 1 do
      OpenFile(OpenDialog.Files[I]);
end;

procedure TdlgHexEditor.acRedoExecute(Sender: TObject);
begin
  ActiveDoc.Redo;
end;

procedure TdlgHexEditor.acRedoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.RedoPresent;
end;

procedure TdlgHexEditor.acSaveAllExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PageControl.PageCount - 1 do
    GetFrameAtIndex(I).Save;
end;

procedure TdlgHexEditor.acSaveAllUpdate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PageControl.PageCount - 1 do
   if GetFrameAtIndex(I).Modifyed then
   begin
     TAction(Sender).Enabled := True;
     Exit;
   end;
   TAction(Sender).Enabled := False;
end;

procedure TdlgHexEditor.acSaveAsExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    ActiveDoc.Save(SaveDialog.FileName);
end;

procedure TdlgHexEditor.acSaveExecute(Sender: TObject);
begin
  ActiveDoc.Save;
end;

procedure TdlgHexEditor.acSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Modifyed;
end;

function TdlgHexEditor.ActiveDoc: TPageFrame;
begin
  Result := GetFrameAtIndex(PageControl.ActivePageIndex);
end;

procedure TdlgHexEditor.acToggleBookmark1Execute(Sender: TObject);
begin
  ActiveDoc.ToggleBookmark(TAction(Sender).Tag);
end;

procedure TdlgHexEditor.acToggleBookmark1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.CanToggleBookmark;
end;

procedure TdlgHexEditor.acUndoExecute(Sender: TObject);
begin
  ActiveDoc.Undo;
end;

procedure TdlgHexEditor.acUndoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.UndoPresent;
end;

procedure TdlgHexEditor.acViewDisplayHexExecute(Sender: TObject);
begin
  ActiveDoc.HexView.AddressView := TAddressView(TAction(Sender).Tag);
end;

procedure TdlgHexEditor.acViewDisplayHexUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Active;
  TAction(Sender).Checked :=
    ActiveDoc.HexView.AddressView = TAddressView(TAction(Sender).Tag);
end;

procedure TdlgHexEditor.acViewDisplayOffsetExecute(Sender: TObject);
var
  CurrentAddrVA: Int64;
  cpd: TCaretPosData;
begin
  cpd := ActiveDoc.HexView.CaretPosData;
  cpd.CharIndex := Min(0, cpd.CharIndex);
  CurrentAddrVA := ActiveDoc.HexView.CaretPosToAddress(cpd);
  if CurrentAddrVA = ActiveDoc.HexView.AddressViewOffsetBase then
    CurrentAddrVA := -1;
  ActiveDoc.HexView.AddressViewOffsetBase := CurrentAddrVA;
end;

procedure TdlgHexEditor.acViewDisplayOffsetUpdate(Sender: TObject);
var
  CurrentAddrVA: Int64;
  cpd: TCaretPosData;
begin
  if ActiveDoc.Active then
  begin
    TAction(Sender).Enabled := True;
    cpd := ActiveDoc.HexView.CaretPosData;
    cpd.CharIndex := Min(0, cpd.CharIndex);
    CurrentAddrVA := ActiveDoc.HexView.CaretPosToAddress(cpd);
    if (ActiveDoc.HexView.AddressViewOffsetBase < 0) or
      (ActiveDoc.HexView.AddressViewOffsetBase <> CurrentAddrVA ) then
      TAction(Sender).Caption := 'Set Base Offset from Current Row'
    else
      TAction(Sender).Caption := 'Remove Base Offset from Current Row';
  end
  else
    TAction(Sender).Enabled := False;
end;

procedure TdlgHexEditor.acViewMode8Execute(Sender: TObject);
begin
  ActiveDoc.HexView.AddressMode := TAddressMode(TAction(Sender).Tag);
end;

procedure TdlgHexEditor.acViewMode8Update(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveDoc.Active;
  TAction(Sender).Checked :=
    ActiveDoc.HexView.AddressMode = TAddressMode(TAction(Sender).Tag);
end;

procedure TdlgHexEditor.CloseFromPage(Index: Integer);
var
  I, ModifyCount: Integer;
  ForceSave: Boolean;
begin
  ModifyCount := 0;
  for I := Index to PageControl.PageCount - 1 do
    if GetFrameAtIndex(I).Modifyed then
      Inc(ModifyCount);
  ForceSave := False;
  for I := PageControl.PageCount - 1 downto Index do
  begin
    case DoCloseView(I, ModifyCount > 1, ForceSave) of
      mrYesToAll: ForceSave := True;
      mrCancel: Exit;
      mrNo, mrYes: Dec(ModifyCount);
    end;
    if PageControl.PageCount > 1 then
      PageControl.Pages[I].Free
    else
      PageControl.Pages[0].Caption := NoData;
  end;
  PageControl.ActivePageIndex := Max(0, Index - 1);
  UpdateSelections;
end;

function TdlgHexEditor.CreateNewFrame: TPageFrame;
var
  NewPage: TTabSheet;
  Buff: array of Byte;
begin
  SetLength(Buff, 10);
  NewPage := TTabSheet.Create(Self);
  NewPage.Caption := NoData;
  NewPage.PageControl := PageControl;
  Result := TPageFrame.Create(Self);
  Result.Name := '';
  Result.Parent := NewPage;
  Result.Align := alClient;
  Result.HexView.PopupMenu := PopupMenu;
  Result.HexView.OnCaretPosChange := UpdateStatusBar;
  Result.HexView.OnSelectionChange := UpdateStatusBar;
  Result.HexView.Font.Height := Result.HexView.ToDpi(-18);
  Result.HexView.FitColumnsToBestSize;
  PageControl.ActivePage := NewPage;
  UpdateSelections;
end;

function TdlgHexEditor.DoCloseView(Index: Integer;
  ShowYesToAll, ForceSave: Boolean): Integer;
var
  AFrame: TPageFrame;
  Buttons: TMsgDlgButtons;
begin
  AFrame := GetFrameAtIndex(Index);
  Result := mrOk;
  Buttons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
  if ShowYesToAll then
    Include(Buttons, TMsgDlgBtn.mbYesToAll);
  if AFrame.Modifyed and AFrame.FileMode then
  begin
    if ForceSave then
      AFrame.Save
    else
    begin
      Result := MessageDlg(Format('Save file "%s"', [AFrame.FileName]),
        TMsgDlgType.mtConfirmation, Buttons, 0);
      if Result in [mrYesToAll, mrYes] then
        AFrame.Save;
    end;
  end;
  if Result <> mrCancel then
    AFrame.Close;
end;

procedure TdlgHexEditor.FitColumnstoBestSize1Click(Sender: TObject);
begin
  ActiveDoc.HexView.FitColumnsToBestSize;
end;

procedure TdlgHexEditor.FormCreate(Sender: TObject);
begin
  {$IFNDEF FPC}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  CreateNewFrame;
  UpdateView;
  UpdateStatusBar(nil);
end;

function TdlgHexEditor.GetFrameAtIndex(Index: Integer): TPageFrame;
begin
  Result := PageControl.Pages[Index].Controls[0] as TPageFrame;
end;

procedure TdlgHexEditor.OpenFile(const FilePath: string);
var
  NewPage: TTabSheet;
  NewFrame: TPageFrame;
begin
  if not ActiveDoc.Active then
  begin
    NewPage := nil;
    NewFrame := ActiveDoc;
  end
  else
  begin
    NewFrame := CreateNewFrame;
    NewPage := NewFrame.Parent as TTabSheet;
  end;
  try
    NewFrame.OpenFile(FilePath);
  except
    NewPage.Free;
    raise;
  end;
  NewFrame.HexView.FitColumnsToBestSize;
end;

procedure TdlgHexEditor.PageControlChange(Sender: TObject);
begin
  UpdateSelections;
  UpdateStatusBar(nil);
  ActiveDoc.HexView.SetFocus;
end;

procedure TdlgHexEditor.SelectionsDblClick(Sender: TObject);
var
  AddrVA: Int64;
begin
  if TryStrToInt64('0x' +
    dlgSelections.ListBox.Items[dlgSelections.ListBox.ItemIndex], AddrVA) then
    ActiveDoc.HexView.FocusOnAddress(AddrVA, ccmSetNewSelection);
end;

procedure TdlgHexEditor.ShowSelected1Click(Sender: TObject);
begin
  if dlgSelections = nil then
  begin
    dlgSelections := TdlgSelections.Create(Self);
    dlgSelections.ListBox.OnDblClick := SelectionsDblClick;
  end;
  UpdateSelections;
  dlgSelections.Show;
end;

procedure TdlgHexEditor.UpdateSelections;
begin
  if dlgSelections = nil then Exit;
  ActiveDoc.LoadSelection(dlgSelections.ListBox.Items);
end;

procedure TdlgHexEditor.UpdateStatusBar(Sender: TObject);
var
  ss, se: Int64;
  cpd: TCaretPosData;
begin
  if ActiveDoc.ReadOnly then
    StatusBar.Panels[0].Text := 'ReadOnly'
  else
    StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := TByteViewModeStr[ActiveDoc.HexView.ByteViewMode];
  StatusBar.Panels[2].Text := ActiveDoc.HexView.Encoder.DisplayName;
  ss := Min(ActiveDoc.HexView.SelStart, ActiveDoc.HexView.SelEnd);
  se := Max(ActiveDoc.HexView.SelStart, ActiveDoc.HexView.SelEnd);
  if ss < 0 then
    StatusBar.Panels[3].Text := 'Sel: none'
  else
    StatusBar.Panels[3].Text := Format('Sel: %x-%x len: %d', [ss, se, se - ss + 1]);
  cpd := ActiveDoc.HexView.CaretPosData;
  if cpd.Enabled and (cpd.Column in [ctOpcode, ctDescription]) then
    StatusBar.Panels[4].Text := Format('Pos: %x', [
      ActiveDoc.HexView.CaretPosToAddress(cpd)])
  else
    StatusBar.Panels[4].Text := '';
end;

procedure TdlgHexEditor.UpdateView;
begin
  with ActiveDoc.HexView do
  begin
    case ByteViewMode of
      bvmInt8, bvmUInt8: BytesInRow := 8;
      bvmFloat80: BytesInRow := 20;
      bvmText: BytesInRow := 64;
    else
      BytesInRow := 16;
    end;
    case ByteViewMode of
      bvmText:
      begin
        Header.Columns := [ctWorkSpace, ctAddress, ctOpcode];
        SeparateGroupByColor := False;
      end;
    else
      SeparateGroupByColor := True;
      Header.Columns := [ctWorkSpace, ctAddress, ctOpcode, ctDescription];
    end;
    FitColumnsToBestSize;
  end;
end;

end.
