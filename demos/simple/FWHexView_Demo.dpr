program FWHexView_Demo;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  FWHexView.Actions in '..\..\src\FWHexView.Actions.pas',
  FWHexView.AsmTokenizer in '..\..\src\FWHexView.AsmTokenizer.pas',
  FWHexView.Common in '..\..\src\FWHexView.Common.pas',
  FWHexView.MappedView in '..\..\src\FWHexView.MappedView.pas',
  FWHexView in '..\..\src\FWHexView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
