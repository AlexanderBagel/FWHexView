object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TMappedHexView Virtual Pages demo'
  ClientHeight = 244
  ClientWidth = 904
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MappedHexView1: TMappedHexView
    Left = 0
    Top = 0
    Width = 904
    Height = 244
    Align = alClient
    Header.Columns = [ctJmpLine, ctAddress, ctOpcode, ctDescription]
    TabOrder = 0
    ExplicitWidth = 304
    ExplicitHeight = 201
  end
end
