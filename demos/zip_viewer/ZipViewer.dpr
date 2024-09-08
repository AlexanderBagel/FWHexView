program ZipViewer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  uMain in 'uMain.pas' {dlgMain};

{$R *.res}

begin
  {$IFDEF FPC}
  Application.Scaled:=True;
  {$ENDIF}
  Application.Initialize;
  {$IFNDEF FPC}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TdlgMain, dlgMain);
  Application.Run;
end.
