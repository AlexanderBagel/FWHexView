object dlgFindData: TdlgFindData
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'HexView - Search Data'
  ClientHeight = 191
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 11
    Width = 28
    Height = 13
    Caption = 'ASCII'
  end
  object Label2: TLabel
    Left = 16
    Top = 38
    Width = 46
    Height = 13
    Caption = 'UNICODE'
  end
  object Label3: TLabel
    Left = 16
    Top = 67
    Width = 19
    Height = 13
    Caption = 'HEX'
  end
  object edAnsi: TEdit
    Left = 80
    Top = 8
    Width = 401
    Height = 21
    TabOrder = 0
    OnChange = edAnsiChange
  end
  object edUnicode: TEdit
    Left = 80
    Top = 35
    Width = 401
    Height = 21
    TabOrder = 1
    OnChange = edUnicodeChange
  end
  object edHex: TMemo
    Left = 80
    Top = 64
    Width = 401
    Height = 89
    TabOrder = 2
    OnChange = edHexChange
    OnKeyPress = edHexKeyPress
  end
  object btnCancel: TButton
    Left = 406
    Top = 159
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnSearch: TButton
    Left = 325
    Top = 159
    Width = 75
    Height = 25
    Caption = 'Search'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
end
