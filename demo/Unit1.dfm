object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 604
  ClientWidth = 1100
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 544
    Width = 1100
    Height = 60
    Align = alBottom
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 16
      Top = 8
      Width = 129
      Height = 17
      Caption = 'AddressMode 64 bit'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 32
      Width = 129
      Height = 17
      Caption = 'Address View HEX'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 176
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Header'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox3Click
    end
    object Button1: TButton
      Left = 256
      Top = 8
      Width = 90
      Height = 25
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 1000
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 4
      OnClick = Button2Click
    end
  end
end
