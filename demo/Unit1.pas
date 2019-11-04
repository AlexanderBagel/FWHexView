unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, FWHexView, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Hex: TFWHexView;
    Data: TMemoryStream;
    procedure FillHex;
    procedure HexKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Hex.CopySelected(csCpp);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Hex.BytesInGroup := 8;
  Hex.BytesInColorGroup := 3;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    Hex.AddressMode := am64bit
  else
    Hex.AddressMode := am32bit;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    Hex.AddressView := avHex
  else
    Hex.AddressView := avDecimal;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  Hex.Header.Visible := TCheckBox(Sender).Checked;
end;

procedure TForm1.FillHex;
begin
  // RAW данные введены строкой со снимка и не соответствуют действительной ситуации
  // Просто чтобы показать возможности контрола
  Hex.DataMap.AddSeparator('IMAGE_DOS_HEADER');
  Hex.DataMap.AddExDescription(2, 'e_magic = "MZ"', 'Magic number');
  Hex.DataMap.AddExDescription(2, 'e_cblp = 50', 'Bytes on last page of file');
  Hex.DataMap.AddExDescription(2, 'e_cp = 2', 'Pages in file');
  Hex.DataMap.AddExDescription(2, 'e_crlc = 0', 'Relocations');
  Hex.DataMap.AddExDescription(2, 'e_cparhdr = 4', 'Size of header in paragraphs');
  Hex.DataMap.AddExDescription(2, 'e_minalloc = F', 'Minimum extra paragraphs needed');
  Hex.DataMap.AddExDescription(2, 'e_maxalloc = FFFF', 'Maximum extra paragraphs needed');
  Hex.DataMap.AddExDescription(2, 'e_ss = 0', 'Initial (relative) SS value');
  Hex.DataMap.AddExDescription(2, 'e_sp = B8', 'Initial SP value');
  Hex.DataMap.AddExDescription(2, 'e_csum = 0', 'Checksum');
  Hex.DataMap.AddExDescription(2, 'e_ip = 0', 'Initial IP value');
  Hex.DataMap.AddExDescription(2, 'e_cs = 0', 'Initial (relative) CS value');
  Hex.DataMap.AddExDescription(2, 'e_lfarlc = 40', 'File address of relocation table');
  Hex.DataMap.AddExDescription(2, 'e_ovno = 1A', 'Overlay number');
  Hex.DataMap.AddExDescription(8, 'e_res', 'Reserved words', clGrayText);
  Hex.DataMap.AddExDescription(2, 'e_oemid = 0', 'OEM identifier (for e_oeminfo)');
  Hex.DataMap.AddExDescription(2, 'e_oeminfo = 0', 'OEM information; e_oemid specific');
  Hex.DataMap.AddExDescription(20, 'e_res2', 'Reserved words', clGrayText);
  Hex.DataMap.AddExDescription(4, '_lfanew = 100 ($400100)', 'File address of new exe header', $400100, 15, 7);
  Hex.DataMap.AddLine;
  Hex.DataMap.AddSeparator($400100, 'IMAGE_NT_HEADERS');
  Hex.DataMap.AddExDescription(4, 'Signature = "PE"');
  Hex.DataMap.AddSeparator('IMAGE_FILE_HEADER');
  Hex.DataMap.AddExDescription(2, 'Machine = 14C', 'IMAGE_FILE_MACHINE_I386');
  Hex.DataMap.AddExDescription(2, 'NumberOfSections = C');
  Hex.DataMap.AddExDescription(4, 'TimeDateStamp = 58F333A2', '16.04.2017 9:04:34');
  Hex.DataMap.AddExDescription(4, 'PointerToSymbolTable = 0', '');
  Hex.DataMap.AddExDescription(4, 'NumberOfSymbols = 0');
  Hex.DataMap.AddExDescription(2, 'SizeOfOptionalHeader = E0');
  Hex.DataMap.AddExDescription(2, 'Characteristics = 818E');

  Hex.DataMap.AddCheck(3, '32BIT_MACHINE', '0x0100', True);
  Hex.DataMap.AddCheck(3, 'DEBUG_STRIPPED', '0x0200', False);
  Hex.DataMap.AddCheck(3, 'REMOVABLE_RUN_FROM_SWAP', '0x0400', False);
  Hex.DataMap.AddCheck(3, 'NET_RUN_FROM_SWAP', '0x0800', False);
  Hex.DataMap.AddCheck(2, 'SYSTEM', '0x1000', False);
  Hex.DataMap.AddCheck(2, 'DLL', '0x2000', False);
  Hex.DataMap.AddCheck(2, 'UP_SYSTEM_ONLY', '0x4000', False);
  Hex.DataMap.AddCheck(2, 'BYTES_REVERSED_HI', '0x8000', True);
  Hex.DataMap.AddCheck(1, 'RELOCS_STRIPPED', '0x0001', False);
  Hex.DataMap.AddCheck(1, 'EXECUTABLE_IMAGE', '0x0002', True);
  Hex.DataMap.AddCheck(1, 'LINE_NUMS_STRIPPED', '0x0004', True);
  Hex.DataMap.AddCheck(1, 'LOCAL_SYMS_STRIPPED', '0x0008', True);
  Hex.DataMap.AddCheck(0, 'AGGRESIVE_WS_TRIM', '0x0010', False);
  Hex.DataMap.AddCheck(0, 'LARGE_ADDRESS_AWARE', '0x0020', False);
  Hex.DataMap.AddCheck(0, 'BYTES_REVERSED_LO', '0x0080', True);

  Hex.DataMap.AddSeparator('IMAGE_OPTIONAL_HEADER32');
  Hex.DataMap.AddExDescription(2, 'Magic = 10B', 'IMAGE_NT_OPTIONAL_HDR32_MAGIC');
  Hex.DataMap.AddExDescription(1, 'MajorLinkerVersion = 2');
  Hex.DataMap.AddExDescription(1, 'MinorLinkerVersion = 19');
  Hex.DataMap.AddExDescription(4, 'SizeOfCode = 1F8200');
  Hex.DataMap.AddExDescription(4, 'SizeOfInitializedData = 901F5B');
  Hex.DataMap.AddExDescription(4, 'SizeOfUninitializedData = 0');
  Hex.DataMap.AddExDescription(4, 'AddressOfEntryPoint = 1F956C', 'Вот тут активный линк на точку входа', $005E952C, 22, 6);
  Hex.DataMap.AddExDescription(4, 'BaseOfCode = 1000');
  Hex.DataMap.AddExDescription(4, 'BaseOfData = 1FA000');
  Hex.DataMap.AddExDescription(4, 'ImageBase = 400000');
  Hex.DataMap.AddExDescription(4, 'SectionAlignment = 1000');
  Hex.DataMap.AddExDescription(4, 'FileAlignment = 200');
  Hex.DataMap.AddExDescription(2, 'MajorOperatingSystemVersion = 5');
  Hex.DataMap.AddExDescription(2, 'MinorOperatingSystemVersion = 0');
  Hex.DataMap.AddExDescription(2, 'MajorImageVersion = 0');
  Hex.DataMap.AddExDescription(2, 'MinorImageVersion = 0');
  Hex.DataMap.AddExDescription(2, 'MajorSubsystemVersion = 5');
  Hex.DataMap.AddExDescription(2, 'MinorSubsystemVersion = 0');
  Hex.DataMap.AddExDescription(4, 'Win32VersionValue = 0');
  Hex.DataMap.AddExDescription(4, 'SizeOfImage = AD6000');
  Hex.DataMap.AddExDescription(4, 'SizeOfHeaders = 400');
  Hex.DataMap.AddExDescription(4, 'CheckSum = 0');
  Hex.DataMap.AddExDescription(2, 'Subsystem = 2', 'IMAGE_SUBSYSTEM_WINDOWS_GUI');
  Hex.DataMap.AddExDescription(2, 'DllCharacteristics = 0');
  Hex.DataMap.AddExDescription(4, 'SizeOfStackReserve = 100000');
  Hex.DataMap.AddExDescription(4, 'SizeOfStackCommit = 4000');
  Hex.DataMap.AddExDescription(4, 'SizeOfHeapReserve = 100000');
  Hex.DataMap.AddExDescription(4, 'SizeOfHeapCommit = 1000');
  Hex.DataMap.AddExDescription(4, 'LoaderFlags = 0');
  Hex.DataMap.AddExDescription(4, 'NumberOfRvaAndSizes = 10');

  Hex.DataMap.AddSeparator('IMAGE_DATA_DIRECTORY');
  Hex.DataMap.AddExDescription(4, 'Export Directory Address = 1F9000');
  Hex.DataMap.AddExDescription(4, 'Export Directory Size = A0');
  Hex.DataMap.AddExDescription(4, 'Import Directory Address = 1F4000');
  Hex.DataMap.AddExDescription(4, 'Import Directory Size = 360E');
  Hex.DataMap.AddExDescription(4, 'Resource Directory Address = 229000');
  Hex.DataMap.AddExDescription(4, 'Resource Directory Size = 14A00');
  Hex.DataMap.AddExDescription(4, 'Exception Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'Exception Directory Size = 0');
  Hex.DataMap.AddExDescription(4, 'Security Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'Security Directory Size = 0');
  Hex.DataMap.AddExDescription(4, 'BaseReloc Directory Address = 1FC000');
  Hex.DataMap.AddExDescription(4, 'BaseReloc Directory Size = 2C104');
  Hex.DataMap.AddExDescription(4, 'Debug Directory Address = 23E000');
  Hex.DataMap.AddExDescription(4, 'Debug Directory Size = 1');
  Hex.DataMap.AddExDescription(4, 'Copyright Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'Copyright Directory Size = 0');
  Hex.DataMap.AddExDescription(4, 'GlobalPTR Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'GlobalPTR Directory Size = 0');
  Hex.DataMap.AddExDescription(4, 'TLS Directory Address = 1FB000');
  Hex.DataMap.AddExDescription(4, 'TLS Directory Size = 18');
  Hex.DataMap.AddExDescription(4, 'Load config Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'Load config Directory Size = 0');
  Hex.DataMap.AddExDescription(4, 'Bound import Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'Bound import Directory Size = 0');
  Hex.DataMap.AddExDescription(4, 'Iat Directory Address = 1F49F8');
  Hex.DataMap.AddExDescription(4, 'Iat Directory Size = 854');
  Hex.DataMap.AddExDescription(4, 'Delay import Directory Address = 1F8000');
  Hex.DataMap.AddExDescription(4, 'Delay import Directory Size = A40');
  Hex.DataMap.AddExDescription(4, 'COM Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'COM Directory Size = 0');
  Hex.DataMap.AddExDescription(4, 'Reserved Directory Address = 0');
  Hex.DataMap.AddExDescription(4, 'Reserved Directory Size = 0');

  Hex.DataMap.AddSeparator('IMAGE_SECTION_HEADERS');
  Hex.DataMap.AddExDescription(8, 'Name = ".text"');
  Hex.DataMap.AddExDescription(4, 'VirtualSize = 1E391C');
  Hex.DataMap.AddExDescription(4, 'VirtualAddress = 1000');
  Hex.DataMap.AddExDescription(4, 'SizeOfRawData = 1E3A00');
  Hex.DataMap.AddExDescription(4, 'PointerToRawData = 400');
  Hex.DataMap.AddExDescription(4, 'PointerToRelocations = 0');
  Hex.DataMap.AddExDescription(4, 'PointerToLinenumbers = 0');
  Hex.DataMap.AddExDescription(2, 'NumberOfRelocations = 0');
  Hex.DataMap.AddExDescription(2, 'NumberOfLinenumbers = 0');
  Hex.DataMap.AddExDescription(4, 'Characteristics = 60000020');

  Hex.DataMap.AddCheck(7, 'IMAGE_SCN_LNK_NRELOC_OVFL',    '0x01000000', False);
  Hex.DataMap.AddCheck(7, 'IMAGE_SCN_MEM_DISCARDABLE',    '0x02000000', False);
  Hex.DataMap.AddCheck(7, 'IMAGE_SCN_MEM_NOT_CACHED',     '0x04000000', False);
  Hex.DataMap.AddCheck(7, 'IMAGE_SCN_MEM_NOT_PAGED',      '0x08000000', False);

  Hex.DataMap.AddCheck(6, 'IMAGE_SCN_MEM_SHARED',         '0x10000000', False);
  Hex.DataMap.AddCheck(6, 'IMAGE_SCN_MEM_EXECUTE',        '0x20000000', True);
  Hex.DataMap.AddCheck(6, 'IMAGE_SCN_MEM_READ',           '0x40000000', True);
  Hex.DataMap.AddCheck(6, 'IMAGE_SCN_MEM_WRITE',          '0x80000000', False);

  Hex.DataMap.AddCheck(5, 'IMAGE_SCN_MEM_16BIT',          '0x00020000', False);
  Hex.DataMap.AddCheck(5, 'IMAGE_SCN_MEM_LOCKED',         '0x00040000', False);
  Hex.DataMap.AddCheck(5, 'IMAGE_SCN_MEM_PRELOAD',        '0x00080000', False);

  Hex.DataMap.AddNone;
  Hex.DataMap.AddRadio(4, 'IMAGE_SCN_ALIGN_1BYTES',       '0x00100000', False);
  Hex.DataMap.AddRadio(4, 'IMAGE_SCN_ALIGN_2BYTES',       '0x00200000', False);
  Hex.DataMap.AddRadio(4, 'IMAGE_SCN_ALIGN_4BYTES',       '0x00300000', False);
  Hex.DataMap.AddRadio(4, 'IMAGE_SCN_ALIGN_8BYTES',       '0x00400000', False);
  Hex.DataMap.AddRadio(4, 'IMAGE_SCN_ALIGN_16BYTES',      '0x00500000 - Default alignment if no others are specified.', True);
  Hex.DataMap.AddRadio(4, 'IMAGE_SCN_ALIGN_32BYTES',      '0x00600000', False);
  Hex.DataMap.AddRadio(4, 'IMAGE_SCN_ALIGN_64BYTES',      '0x00700000', False);
  Hex.DataMap.AddNone;

  Hex.DataMap.AddCheck(3, 'IMAGE_SCN_LNK_OTHER',          '0x00000100', False);
  Hex.DataMap.AddCheck(3, 'IMAGE_SCN_LNK_INFO',           '0x00000200', False);
  Hex.DataMap.AddCheck(3, 'IMAGE_SCN_LNK_REMOVE',         '0x00000800', False);

  Hex.DataMap.AddCheck(2, 'IMAGE_SCN_LNK_COMDAT',         '0x00001000', False);
  Hex.DataMap.AddCheck(2, 'IMAGE_SCN_MEM_FARDATA',        '0x00008000', False);

  Hex.DataMap.AddCheck(1, 'IMAGE_SCN_TYPE_NO_PAD',        '0x00000008', False);

  Hex.DataMap.AddCheck(0, 'IMAGE_SCN_CNT_CODE',               '0x00000020', True);
  Hex.DataMap.AddCheck(0, 'IMAGE_SCN_CNT_INITIALIZED_DATA',   '0x00000040', False);
  Hex.DataMap.AddCheck(0, 'IMAGE_SCN_CNT_UNINITIALIZED_DATA', '0x00000080', False);


  Hex.DataMap.AddSeparator('НУ И ТАК ДАЛЕЕ');


  // добавляем описание на точку входа

  Hex.DataMap.AddSeparator($005E952C, 'Entry point');
  Hex.DataMap.AddNone;
  Hex.DataMap.AddCode('FWHexView_Demo.dpr.10: begin');
  Hex.DataMap.AddAsm(1, 'PUSH EBP');
  Hex.DataMap.AddAsm(2, 'MOV EBP, ESP');
  Hex.DataMap.AddAsm(3, 'ADD ESP, -0x10');
  Hex.DataMap.AddAsm(5, 'MOV EAX, 0x5E0F1C');
  Hex.DataMap.AddAsm(5, 'CALL 0x41048C', 'FWHexView_Demo.exe!SysInit.@InitExe', $41048C, 0, 13);
  Hex.DataMap.AddCode('FWHexView_Demo.dpr.11: Application.Initialize;');
  Hex.DataMap.AddAsm(5, 'MOV EAX, [0x5EEFF4]');
  Hex.DataMap.AddAsm(2, 'MOV EAX, [EAX]');
  Hex.DataMap.AddAsm(5, 'CALL 0x5C47D4', 'FWHexView_Demo.exe!Vcl.Forms.TApplication.Initialize', $5C47D4, 0, 13);
  Hex.DataMap.AddCode('FWHexView_Demo.dpr.12: Application.MainFormOnTaskbar := True;');
  Hex.DataMap.AddAsm(5, 'MOV EAX, [0x5EEFF4]');
  Hex.DataMap.AddAsm(2, 'MOV EAX, [EAX]');
  Hex.DataMap.AddAsm(2, 'MOV DL, 0x1');
  Hex.DataMap.AddAsm(5, 'CALL 0x5C650C', 'FWHexView_Demo.exe!Vcl.Forms.TApplication.SetMainFormOnTaskBar', $5C650C, 0, 13);
  Hex.DataMap.AddCode('FWHexView_Demo.dpr.13: Application.CreateForm(TForm1, Form1);');
  Hex.DataMap.AddAsm(5, 'MOV ECX, [0x5EF1B0]');
  Hex.DataMap.AddAsm(5, 'MOV EAX, [0x5EEFF4]');
  Hex.DataMap.AddAsm(2, 'MOV EAX, [EAX]');
  Hex.DataMap.AddAsm(5, 'MOV EDX, [0x5DC7E4]');
  Hex.DataMap.AddAsm(5, 'CALL 0x5C47EC', 'FWHexView_Demo.exe!Vcl.Forms.TApplication.CreateForm', $5C47EC, 0, 13);
  Hex.DataMap.AddCode('FWHexView_Demo.dpr.14: Application.Run;');
  Hex.DataMap.AddAsm(5, 'MOV EAX, [0x5EEFF4]');
  Hex.DataMap.AddAsm(2, 'MOV EAX, [EAX]');
  Hex.DataMap.AddAsm(5, 'CALL 0x5C4948', 'FWHexView_Demo.exe!Vcl.Forms.TApplication.Run', $5C4948, 0, 13);
  Hex.DataMap.AddCode('FWHexView_Demo.dpr.15: end.');
  Hex.DataMap.AddAsm(5, 'CALL 0x4098C0', 'FWHexView_Demo.exe!System.@Halt0', $4098C0, 0, 13);
  Hex.DataMap.AddAsm(1, 'NOP');
  Hex.DataMap.AddNone;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  Hex := TFWHexView.Create(Self);
  Hex.Align := alClient;
  Hex.Parent := Self;
  Hex.ScrollBars := ssBoth;
  Hex.AddressMode := am64bit;
  Hex.SeparateGroupByColor := True;
  Hex.BeginUpdate;
  try
    Data := TMemoryStream.Create;
    Data.LoadFromFile(ParamStr(0));
    Hex.SetDataStream(Data, $400000);
    FillHex;
  finally
    Hex.EndUpdate;
  end;
  Hex.Bookmark[1] := $400000;
  Hex.Bookmark[2] := $005E952C;
  Hex.OnKeyDown := HexKeyDown;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

procedure TForm1.HexKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [48..57] then
    if ssCtrl in Shift then
      if Hex.Bookmark[Key - 48] <> 0 then
        Hex.FocusOnAddress(Hex.Bookmark[Key - 48]);
end;

end.
