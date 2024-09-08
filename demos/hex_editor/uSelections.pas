unit uSelections;

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
  Controls, Forms, StdCtrls;

type
  TdlgSelections = class(TForm)
    ListBox: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgSelections: TdlgSelections;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TdlgSelections.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dlgSelections := nil;
  Action := TCloseAction.caFree;
end;

end.
