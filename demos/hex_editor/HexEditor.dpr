program HexEditor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  uMain in 'uMain.pas' {dlgHexEditor},
  uDataFrame in 'uDataFrame.pas' {PageFrame: TFrame},
  uDataStream in 'uDataStream.pas' {PageFrame: TFrame},
  uFindData in 'uFindData.pas' {dlgFindData},
  uSelections in 'uSelections.pas' {dlgSelections},
  FWHexView.Actions in '..\..\src\FWHexView.Actions.pas',
  FWHexView.Common in '..\..\src\FWHexView.Common.pas',
  FWHexView.MappedView in '..\..\src\FWHexView.MappedView.pas',
  FWHexView in '..\..\src\FWHexView.pas';

{$R *.res}

begin
  {$IFDEF FPC}
  Application.Scaled:=True;
  {$ENDIF}
  Application.Initialize;
  {$IFNDEF FPC}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TdlgHexEditor, dlgHexEditor);
  Application.Run;
end.
