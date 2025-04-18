object dlgHexEditor: TdlgHexEditor
  Left = 0
  Top = 0
  Caption = 'HexEditor'
  ClientHeight = 617
  ClientWidth = 959
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
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 598
    Width = 959
    Height = 19
    Panels = <
      item
        Width = 75
      end
      item
        Width = 175
      end
      item
        Width = 150
      end
      item
        Width = 300
      end
      item
        Width = 50
      end>
    ParentColor = True
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 959
    Height = 598
    Align = alClient
    PopupMenu = TabPopupMenu
    TabOrder = 1
    OnChange = PageControlChange
  end
  object MainMenu: TMainMenu
    Left = 352
    Top = 144
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Action = acOpen
      end
      object Save1: TMenuItem
        Action = acSave
      end
      object SaveAs1: TMenuItem
        Action = acSaveAs
      end
      object SaveAll1: TMenuItem
        Action = acSaveAll
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = acClose
      end
      object CloseAll1: TMenuItem
        Action = acCloseAll
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = acExit
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Action = acUndo
      end
      object acRedo1: TMenuItem
        Action = acRedo
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Copy2: TMenuItem
        Action = acCopyBytes
      end
      object CopyasText2: TMenuItem
        Action = acCopyText
      end
      object CopyAddress1: TMenuItem
        Action = acCopyAddr
      end
      object CopyasArray2: TMenuItem
        Caption = 'Copy as Array'
        object CopyasPasArray2: TMenuItem
          Action = acCopyPas
        end
        object CopyasCppArray2: TMenuItem
          Action = acCopyCpp
        end
        object CopyasAsmArray2: TMenuItem
          Action = acCopyAsm
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object FillbyZeros1: TMenuItem
        Action = acFillZeros
      end
    end
    object Search1: TMenuItem
      Caption = 'Search'
      object Find1: TMenuItem
        Action = acFind
      end
      object FindNext1: TMenuItem
        Action = acFindNext
      end
      object SelectandFindNext1: TMenuItem
        Action = acFindNextSelect
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object ShowSelected1: TMenuItem
        Caption = 'Show Selected...'
        OnClick = ShowSelected1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object AddressDisplayMode1: TMenuItem
        Caption = 'Address Display Mode'
        object Hex1: TMenuItem
          Action = acViewDisplayHex
        end
        object Int1: TMenuItem
          Action = acViewDisplayInt
        end
        object N12: TMenuItem
          Caption = '-'
        end
        object OffsetfromCursorPos1: TMenuItem
          Action = acViewDisplayOffset
        end
      end
      object AddressViewMode1: TMenuItem
        Caption = 'Address View Mode'
        object acViewMode81: TMenuItem
          Action = acViewMode8
        end
        object acViewMode161: TMenuItem
          Action = acViewMode16
        end
        object acViewMode321: TMenuItem
          Action = acViewMode32
        end
        object acViewMode641: TMenuItem
          Action = acViewMode64
        end
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object N1251ANSICyrillic1: TMenuItem
        Caption = 'Code Page'
        object N1251ANSICyrillic2: TMenuItem
          Action = acEncAnsi
        end
        object N20127USASCII1: TMenuItem
          Action = acEncAscii
        end
        object N1200Unicode1: TMenuItem
          Action = acEncUnicode
        end
        object N1201UnicodeBigEndian1: TMenuItem
          Action = acEncUnicodeBE
        end
        object N65000UTF71: TMenuItem
          Action = acEncUtf7
        end
        object N65001UTF81: TMenuItem
          Action = acEncUtf8
        end
      end
      object Displaymode1: TMenuItem
        Caption = 'Display mode'
        object HexByte8bit1: TMenuItem
          Action = acBvmHex8
        end
        object HexShort16bit1: TMenuItem
          Action = acBvmHex16
        end
        object HexLong32bit1: TMenuItem
          Action = acBvmHex32
        end
        object HexLongLong64bit1: TMenuItem
          Action = acBvmHex64
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object SignedByte8bit1: TMenuItem
          Action = acBvmInt8
        end
        object SignedShort16bit1: TMenuItem
          Action = acBvmInt16
        end
        object SignedLong32bit1: TMenuItem
          Action = acBvmInt32
        end
        object SignedLongLong64bit1: TMenuItem
          Action = acBvmInt64
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object UnsignedByte8bit1: TMenuItem
          Action = acBvmUInt8
        end
        object UnsignedShort16bit1: TMenuItem
          Action = acBvmUInt16
        end
        object UnsignedLong32bit1: TMenuItem
          Action = acBvmUInt32
        end
        object UnsignedLongLong64bit1: TMenuItem
          Action = acBvmUInt64
        end
        object N5: TMenuItem
          Caption = '-'
        end
        object Float32bit1: TMenuItem
          Action = acBvmFloat32
        end
        object Double64bit1: TMenuItem
          Action = acBvmFloat64
        end
        object Extended80bit1: TMenuItem
          Action = acBvmFloat80
        end
        object N6: TMenuItem
          Caption = '-'
        end
        object ext1: TMenuItem
          Action = acBvmText
        end
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object FitColumnstoBestSize1: TMenuItem
        Caption = 'Fit Columns to Best Size'
        OnClick = FitColumnstoBestSize1Click
      end
    end
  end
  object ActionList: TActionList
    Left = 224
    Top = 144
    object acUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      ShortCut = 16474
      OnExecute = acUndoExecute
      OnUpdate = acUndoUpdate
    end
    object acRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      ShortCut = 24666
      OnExecute = acRedoExecute
      OnUpdate = acRedoUpdate
    end
    object acOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      ShortCut = 16463
      OnExecute = acOpenExecute
    end
    object acSave: TAction
      Category = 'File'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = acSaveExecute
      OnUpdate = acSaveUpdate
    end
    object acSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      ShortCut = 49235
      OnExecute = acSaveAsExecute
      OnUpdate = acCloseUpdate
    end
    object acSaveAll: TAction
      Category = 'File'
      Caption = 'Save All'
      ShortCut = 24659
      OnExecute = acSaveAllExecute
      OnUpdate = acSaveAllUpdate
    end
    object acClose: TAction
      Category = 'File'
      Caption = 'Close'
      OnExecute = acCloseExecute
      OnUpdate = acCloseUpdate
    end
    object acCloseAll: TAction
      Category = 'File'
      Caption = 'Close All'
      OnExecute = acCloseAllExecute
      OnUpdate = acCloseAllUpdate
    end
    object acCloseRight: TAction
      Category = 'File'
      Caption = 'Close All to the Right'
      OnExecute = acCloseRightExecute
      OnUpdate = acCloseRightUpdate
    end
    object acExit: TAction
      Category = 'File'
      Caption = 'Exit'
      ShortCut = 32883
      OnExecute = acExitExecute
    end
    object acEncAnsi: TAction
      Tag = 1
      Category = 'Encoding'
      Caption = 'ANSI - Cyrillic (1251)'
      OnExecute = acEncAnsiExecute
      OnUpdate = acEncAnsiUpdate
    end
    object acEncAscii: TAction
      Tag = 2
      Category = 'Encoding'
      Caption = 'US-ASCII (20127)'
      OnExecute = acEncAnsiExecute
      OnUpdate = acEncAnsiUpdate
    end
    object acEncUnicode: TAction
      Tag = 3
      Category = 'Encoding'
      Caption = 'Unicode (1200)'
      OnExecute = acEncAnsiExecute
      OnUpdate = acEncAnsiUpdate
    end
    object acEncUnicodeBE: TAction
      Tag = 4
      Category = 'Encoding'
      Caption = 'Unicode - Big-Endian (1201)'
      OnExecute = acEncAnsiExecute
      OnUpdate = acEncAnsiUpdate
    end
    object acEncUtf7: TAction
      Tag = 5
      Category = 'Encoding'
      Caption = 'UTF-7 (65000)'
      OnExecute = acEncAnsiExecute
      OnUpdate = acEncAnsiUpdate
    end
    object acEncUtf8: TAction
      Tag = 6
      Category = 'Encoding'
      Caption = 'UTF-8 (65001)'
      OnExecute = acEncAnsiExecute
      OnUpdate = acEncAnsiUpdate
    end
    object acBvmHex8: TAction
      Category = 'ByteViewMode'
      Caption = 'Hex Byte (8-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmHex16: TAction
      Tag = 1
      Category = 'ByteViewMode'
      Caption = 'Hex Short (16-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmHex32: TAction
      Tag = 2
      Category = 'ByteViewMode'
      Caption = 'Hex Long (32-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmHex64: TAction
      Tag = 3
      Category = 'ByteViewMode'
      Caption = 'Hex Long Long (64-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmInt8: TAction
      Tag = 4
      Category = 'ByteViewMode'
      Caption = 'Signed Byte (8-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmInt16: TAction
      Tag = 5
      Category = 'ByteViewMode'
      Caption = 'Signed Short (16-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmInt32: TAction
      Tag = 6
      Category = 'ByteViewMode'
      Caption = 'Signed Long (32-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmInt64: TAction
      Tag = 7
      Category = 'ByteViewMode'
      Caption = 'Signed Long Long (64-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmUInt8: TAction
      Tag = 8
      Category = 'ByteViewMode'
      Caption = 'Unsigned Byte (8-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmUInt16: TAction
      Tag = 9
      Category = 'ByteViewMode'
      Caption = 'Unsigned Short (16-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmUInt32: TAction
      Tag = 10
      Category = 'ByteViewMode'
      Caption = 'Unsigned Long (32-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmUInt64: TAction
      Tag = 11
      Category = 'ByteViewMode'
      Caption = 'Unsigned Long Long (64-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmFloat32: TAction
      Tag = 12
      Category = 'ByteViewMode'
      Caption = 'Float (32-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmFloat64: TAction
      Tag = 13
      Category = 'ByteViewMode'
      Caption = 'Double (64-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmFloat80: TAction
      Tag = 14
      Category = 'ByteViewMode'
      Caption = 'Extended (80-bit)'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acBvmText: TAction
      Tag = 15
      Category = 'ByteViewMode'
      Caption = 'Text'
      OnExecute = acBvmHex8Execute
      OnUpdate = acBvmHex8Update
    end
    object acFind: TAction
      Category = 'Search'
      Caption = 'Find...'
      ShortCut = 16454
      OnExecute = acFindExecute
      OnUpdate = acCloseUpdate
    end
    object acFindNext: TAction
      Tag = 1
      Category = 'Search'
      Caption = 'Find Next'
      ShortCut = 114
      OnExecute = acFindExecute
      OnUpdate = acCloseUpdate
    end
    object acFindNextSelect: TAction
      Tag = 2
      Category = 'Search'
      Caption = 'Select and Find Next'
      ShortCut = 16498
      OnExecute = acFindExecute
      OnUpdate = acCloseUpdate
    end
    object acFillZeros: TAction
      Category = 'Edit'
      Caption = 'Fill by Zeros'
      OnExecute = acFillZerosExecute
      OnUpdate = acFillZerosUpdate
    end
    object acToggleBookmark1: TAction
      Tag = 1
      Category = 'Bookmarks'
      Caption = 'Bookmark 1'
      ShortCut = 24625
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark2: TAction
      Tag = 2
      Category = 'Bookmarks'
      Caption = 'Bookmark 2'
      ShortCut = 24626
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark3: TAction
      Tag = 3
      Category = 'Bookmarks'
      Caption = 'Bookmark 3'
      ShortCut = 24627
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark4: TAction
      Tag = 4
      Category = 'Bookmarks'
      Caption = 'Bookmark 4'
      ShortCut = 24628
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark5: TAction
      Tag = 5
      Category = 'Bookmarks'
      Caption = 'Bookmark 5'
      ShortCut = 24629
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark6: TAction
      Tag = 6
      Category = 'Bookmarks'
      Caption = 'Bookmark 6'
      ShortCut = 24630
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark7: TAction
      Tag = 7
      Category = 'Bookmarks'
      Caption = 'Bookmark 7'
      ShortCut = 24631
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark8: TAction
      Tag = 8
      Category = 'Bookmarks'
      Caption = 'Bookmark 8'
      ShortCut = 24632
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acToggleBookmark9: TAction
      Tag = 9
      Category = 'Bookmarks'
      Caption = 'Bookmark 9'
      ShortCut = 24633
      OnExecute = acToggleBookmark1Execute
      OnUpdate = acToggleBookmark1Update
    end
    object acGotoBookmark1: TAction
      Tag = 1
      Category = 'Bookmarks'
      Caption = 'Bookmark 1'
      ShortCut = 16433
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark2: TAction
      Tag = 2
      Category = 'Bookmarks'
      Caption = 'Bookmark 2'
      ShortCut = 16434
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark3: TAction
      Tag = 3
      Category = 'Bookmarks'
      Caption = 'Bookmark 3'
      ShortCut = 16435
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark4: TAction
      Tag = 4
      Category = 'Bookmarks'
      Caption = 'Bookmark 4'
      ShortCut = 16436
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark5: TAction
      Tag = 5
      Category = 'Bookmarks'
      Caption = 'Bookmark 5'
      ShortCut = 16437
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark6: TAction
      Tag = 6
      Category = 'Bookmarks'
      Caption = 'Bookmark 6'
      ShortCut = 16438
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark7: TAction
      Tag = 7
      Category = 'Bookmarks'
      Caption = 'Bookmark 7'
      ShortCut = 16439
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark8: TAction
      Tag = 8
      Category = 'Bookmarks'
      Caption = 'Bookmark 8'
      ShortCut = 16440
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acGotoBookmark9: TAction
      Tag = 9
      Category = 'Bookmarks'
      Caption = 'Bookmark 9'
      ShortCut = 16441
      OnExecute = acGotoBookmark1Execute
      OnUpdate = acGotoBookmark1Update
    end
    object acViewDisplayHex: TAction
      Category = 'View'
      Caption = 'Hexadecimal'
      OnExecute = acViewDisplayHexExecute
      OnUpdate = acViewDisplayHexUpdate
    end
    object acViewDisplayInt: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Numerical'
      OnExecute = acViewDisplayHexExecute
      OnUpdate = acViewDisplayHexUpdate
    end
    object acViewDisplayOffset: TAction
      Category = 'View'
      Caption = 'Offset from Cursor Pos'
      OnExecute = acViewDisplayOffsetExecute
      OnUpdate = acViewDisplayOffsetUpdate
    end
    object acViewMode8: TAction
      Category = 'View'
      Caption = 'Byte (0-FF)'
      OnExecute = acViewMode8Execute
      OnUpdate = acViewMode8Update
    end
    object acViewMode16: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Word (0-FFFF)'
      OnExecute = acViewMode8Execute
      OnUpdate = acViewMode8Update
    end
    object acViewMode32: TAction
      Tag = 2
      Category = 'View'
      Caption = 'DWord (0-FFFFFFFF)'
      OnExecute = acViewMode8Execute
      OnUpdate = acViewMode8Update
    end
    object acViewMode64: TAction
      Tag = 3
      Category = 'View'
      Caption = 'QWord (0-FFFFFFFFFFFFFFFF)'
      OnExecute = acViewMode8Execute
      OnUpdate = acViewMode8Update
    end
    object acCopyAddr: THexViewCopyAction
      Category = 'HexView Actions'
      CopyStyle = csAddress
    end
    object acCopyAsm: THexViewCopyAction
      Category = 'HexView Actions'
      CopyStyle = csAsmOpcodes
    end
    object acCopyText: THexViewCopyAction
      Category = 'HexView Actions'
    end
    object acCopyBytes: THexViewCopyAction
      Category = 'HexView Actions'
      CopyStyle = csBytes
    end
    object acCopyCpp: THexViewCopyAction
      Category = 'HexView Actions'
      CopyStyle = csCpp
    end
    object acCopyPas: THexViewCopyAction
      Category = 'HexView Actions'
      CopyStyle = csPascal
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 296
    Top = 224
  end
  object SaveDialog: TSaveDialog
    Filter = 'All files (*.*)|*.*'
    Left = 216
    Top = 224
  end
  object PopupMenu: TPopupMenu
    Left = 436
    Top = 144
    object Copy1: TMenuItem
      Action = acCopyBytes
    end
    object CopyasText1: TMenuItem
      Action = acCopyText
    end
    object CopyAddress2: TMenuItem
      Action = acCopyAddr
    end
    object CopyasArray1: TMenuItem
      Caption = 'Copy as Array'
      object CopyasPasArray1: TMenuItem
        Action = acCopyPas
      end
      object CopyasCppArray1: TMenuItem
        Action = acCopyCpp
      end
      object CopyasAsmArray1: TMenuItem
        Action = acCopyAsm
      end
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object OffsetfromCursorPos2: TMenuItem
      Action = acViewDisplayOffset
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object FillbyZeros2: TMenuItem
      Action = acFillZeros
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object oggleBookmarks1: TMenuItem
      Caption = 'Toggle Bookmarks'
      object oggleBookmark01: TMenuItem
        Action = acToggleBookmark1
      end
      object Bookmark21: TMenuItem
        Action = acToggleBookmark2
      end
      object Bookmark31: TMenuItem
        Action = acToggleBookmark3
      end
      object Bookmark41: TMenuItem
        Action = acToggleBookmark4
      end
      object Bookmark51: TMenuItem
        Action = acToggleBookmark5
      end
      object Bookmark61: TMenuItem
        Action = acToggleBookmark6
      end
      object Bookmark71: TMenuItem
        Action = acToggleBookmark7
      end
      object Bookmark81: TMenuItem
        Action = acToggleBookmark8
      end
      object Bookmark91: TMenuItem
        Action = acToggleBookmark9
      end
    end
    object GotoBookmarks1: TMenuItem
      Caption = 'Goto Bookmarks'
      object Bookmark11: TMenuItem
        Action = acGotoBookmark1
      end
      object Bookmark22: TMenuItem
        Action = acGotoBookmark2
      end
      object Bookmark32: TMenuItem
        Action = acGotoBookmark3
      end
      object Bookmark42: TMenuItem
        Action = acGotoBookmark4
      end
      object Bookmark52: TMenuItem
        Action = acGotoBookmark5
      end
      object Bookmark62: TMenuItem
        Action = acGotoBookmark6
      end
      object Bookmark72: TMenuItem
        Action = acGotoBookmark7
      end
      object Bookmark82: TMenuItem
        Action = acGotoBookmark8
      end
      object Bookmark92: TMenuItem
        Action = acGotoBookmark9
      end
    end
  end
  object TabPopupMenu: TPopupMenu
    Left = 544
    Top = 144
    object Close2: TMenuItem
      Action = acClose
    end
    object CloseAll2: TMenuItem
      Action = acCloseAll
    end
    object CloseAlltotheRight1: TMenuItem
      Action = acCloseRight
    end
  end
end
