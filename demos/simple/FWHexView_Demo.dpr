program FWHexView_Demo;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  FWHexView in '..\..\FWHexView.pas',
  FWHexView.Common in '..\..\FWHexView.Common.pas',
  FWHexView.MappedView in '..\..\FWHexView.MappedView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
