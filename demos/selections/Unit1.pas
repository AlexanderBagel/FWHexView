unit Unit1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Messages, SysUtils, Classes, Graphics,
  Controls, Forms, FWHexView, FWHexView.Common;

type
  TForm1 = class(TForm)
    FWHexView1: TFWHexView;
    procedure FormCreate(Sender: TObject);
  private
    F: TFileStream;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
begin
  F := TFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyWrite);
  FWHexView1.SetDataStream(F, 0, soOwned);
  FWHexView1.FitColumnsToBestSize;
  FWHexView1.Selections.Add(1, 1, 10, clRed);
  FWHexView1.Selections.Add(2, 3, 8, clYellow);
  FWHexView1.Selections.Add(2, 5, 6, clBlue);
  FWHexView1.Selections.Add(3, 18, 21, clGreen);
  FWHexView1.Selections.Add(4, 20, 23, clYellow);
end;

end.
