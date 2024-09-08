program FWHexView_Demo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
