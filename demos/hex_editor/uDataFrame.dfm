object PageFrame: TPageFrame
  Left = 0
  Top = 0
  Width = 785
  Height = 313
  TabOrder = 0
  object HexView: TFWHexView
    Left = 0
    Top = 0
    Width = 785
    Height = 313
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Consolas'
    Font.Style = []
    Header.Columns = [ctWorkSpace, ctAddress, ctOpcode, ctDescription]
    NoDataText = 'Open file using the menu or drag and drop it from Explorer'
    ReadOnly = False
    TabOrder = 0
    OnDrawColumnBackground = HexViewDrawColumnBackground
    OnDrawToken = HexViewDrawToken
    OnEdit = HexViewEdit
  end
end
