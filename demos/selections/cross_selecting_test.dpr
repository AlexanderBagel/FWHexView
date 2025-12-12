program cross_selecting_test;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$ifdef fpc}
  Interfaces,
  {$endif}
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
