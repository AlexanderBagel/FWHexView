unit Unit1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,

  FWHexView,
  FWHexView.Common,
  FWHexView.MappedView;

type
  TForm1 = class(TForm)
    MappedHexView1: TMappedHexView;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
const
  S: AnsiString = '1234567890';
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  M.Size := 100;
  M.Position := 0;
  M.WriteBuffer(S[1], Length(S));
  MappedHexView1.BeginUpdate;

  // set data stream
  MappedHexView1.SetDataStream(M, $400000, soOwned);

  // add virtual pages
  MappedHexView1.Pages.AddPage('Test Page №1', $400000, 3);
  MappedHexView1.Pages.AddPage('Test Page №2', $500000, 20);
  MappedHexView1.Pages.AddPage('Test Page №3', $600100, 5);

  // hide page caption if needed
  MappedHexView1.Pages[0].ShowCaption := False;

  // add test map
  MappedHexView1.DataMap.AddExDescription($400000, 3, 'Test Jump to $600110', '', $600110, 14, 7);
  MappedHexView1.DataMap.AddRaw($500000, 2);
  MappedHexView1.DataMap.AddNone($60014b);
  MappedHexView1.DataMap.AddExDescription(2, 'Test Jump to $500000', '', $500000, 14, 7);

  MappedHexView1.EndUpdate;
end;

end.
