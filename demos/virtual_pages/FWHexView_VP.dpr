program FWHexView_VP;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  {$IFDEF FPC}
  Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  {$IFNDEF FPC}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
