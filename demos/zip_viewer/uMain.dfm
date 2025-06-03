object dlgMain: TdlgMain
  Left = 0
  Top = 0
  Caption = 'ZipViewer'
  ClientHeight = 572
  ClientWidth = 1390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 233
    Top = 0
    Height = 572
    ExplicitLeft = 504
    ExplicitTop = 264
    ExplicitHeight = 100
  end
  object tvZip: TTreeView
    Left = 0
    Top = 0
    Width = 233
    Height = 572
    Align = alLeft
    Indent = 19
    TabOrder = 0
    OnChange = tvZipChange
  end
  object HexView: TMappedHexView
    Left = 236
    Top = 0
    Width = 1154
    Height = 572
    Align = alClient
    Header.Columns = [ctJmpLine, ctAddress, ctOpcode, ctDescription, ctComment]
    NoDataText = 'Open any ZIP'
    TabOrder = 1
    OnJmpTo = HexViewJmpTo
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 8
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.zip'
    Filter = 'Zip archive (*.zip)|*.zip|Any file (*.*)|*.*'
    FilterIndex = 0
    Left = 80
    Top = 8
  end
end
