unit uFindData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics,
  Controls, Forms, ComCtrls, StdCtrls;

type
  TdlgFindData = class(TForm)
    Label1: TLabel;
    edAnsi: TEdit;
    Label2: TLabel;
    edUnicode: TEdit;
    Label3: TLabel;
    edHex: TMemo;
    btnCancel: TButton;
    btnSearch: TButton;
    procedure edAnsiChange(Sender: TObject);
    procedure edUnicodeChange(Sender: TObject);
    procedure edHexChange(Sender: TObject);
    procedure edHexKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FInUpdateMode: Boolean;
    FSearchBuff: array of Byte;
  public
    function GetSearchBuff(var ASearchBuff: TBytes): Boolean;
  end;

var
  dlgFindData: TdlgFindData;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TdlgFindData.edAnsiChange(Sender: TObject);
var
  AnsiBuff: AnsiString;
  Len: Integer;
begin
  if FInUpdateMode then Exit;
  edUnicode.Text := '';
  edHex.Text := '';
  FInUpdateMode := True;
  try
    AnsiBuff := AnsiString(edAnsi.Text);
    Len := Length(AnsiBuff);
    if Len = 0 then Exit;
    SetLength(FSearchBuff, Len);
    Move(AnsiBuff[1], FSearchBuff[0], Len);
  finally
    FInUpdateMode := False;
  end;
end;

procedure TdlgFindData.edHexChange(Sender: TObject);
var
  Buff: string;
  I, ByteCount: Integer;
  LoPartPresent: Boolean;
  ByteValue: Byte;
begin
  if FInUpdateMode then Exit;
  edAnsi.Text := '';
  edUnicode.Text := '';
  FInUpdateMode := True;
  try
    Buff := Trim(edHex.Text);
    SetLength(FSearchBuff, Length(Buff));
    LoPartPresent := False;
    ByteValue := 0;
    ByteCount := 0;
    for I := 1 to Length(Buff) do
    begin
      if CharInSet(Buff[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
      begin
        if LoPartPresent then
        begin
          ByteValue := ByteValue shl 4;
          Inc(ByteValue, StrToInt('$' + Buff[I]));
          FSearchBuff[ByteCount] := ByteValue;
          ByteValue := 0;
          LoPartPresent := False;
          Inc(ByteCount);
        end
        else
        begin
          ByteValue := StrToInt('$' + Buff[I]);
          LoPartPresent := True;
        end;
      end
      else
        if Buff[I] <> #32 then
        begin
          edHex.Text := '';
          Beep;
        end;
    end;
    if LoPartPresent then
      FSearchBuff[ByteCount] := ByteValue;
    SetLength(FSearchBuff, ByteCount);
  finally
    FInUpdateMode := False;
  end;
end;

procedure TdlgFindData.edHexKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, [#8, #22, '0'..'9', 'a'..'f', 'A'..'F']) then
    Key := #0
  else
    if Key > #22 then
      Key := UpCase(Key);
end;

procedure TdlgFindData.edUnicodeChange(Sender: TObject);
var
  Buff: string;
  Len: Integer;
begin
  if FInUpdateMode then Exit;
  edAnsi.Text := '';
  edHex.Text := '';
  FInUpdateMode := True;
  try
    Buff := edUnicode.Text;
    Len := Length(Buff) * 2;
    SetLength(FSearchBuff, Len);
    if Len > 0 then
      Move(Buff[1], FSearchBuff[0], Len);
  finally
    FInUpdateMode := False;
  end;
end;

procedure TdlgFindData.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdlgFindData.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ModalResult := mrOk;
end;

function TdlgFindData.GetSearchBuff(var ASearchBuff: TBytes): Boolean;
var
  Len: Integer;
begin
  Result := ShowModal = mrOk;
  if not Result then Exit;
  Len := Length(FSearchBuff);
  if Len = 0 then Exit(False);
  SetLength(ASearchBuff, Len);
  Move(FSearchBuff[0], ASearchBuff[0], Len);
end;

end.
