unit uMain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, bufstream, Types,
{$ENDIF}
  SysUtils, Classes, Graphics, Dialogs,
  Controls, Forms, ComCtrls, Menus, ExtCtrls, Generics.Collections,

  ZipSupport,
  FWHexView,
  FWHexView.Common,
  FWHexView.MappedView;

type

  { TdlgMain }

  TdlgMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;
    tvZip: TTreeView;
    HexView: TMappedHexView;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure tvZipChange(Sender: TObject; Node: TTreeNode);
    procedure HexViewJmpTo(Sender: TObject; const AJmpAddr: Int64;
      AJmpState: TJmpState; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure Open1Click(Sender: TObject);
  private
    FCentralFileDirNode: TTreeNode;
    FTreeAddrList: TList<Int64>;
    function GetCFDNode: TTreeNode;
    function FindSign(Stream: TStream): DWORD;
    procedure LoadStringValue(Stream: TStream; out Value: string;
      nSize: Cardinal; UTF: Boolean);
    procedure Open(const FilePath: string);
    procedure ShowCentralFileHeader(Stream: TStream);
    procedure ShowDataDescryptor(Stream: TStream);
    procedure ShowEndOfCentralDir(Stream: TStream);
    procedure ShowExtraFields(Stream: TStream; Size: Integer; Root: TTreeNode;
      UncompressedSize, CompressedSize, RelativeOffsetOfLocalHeader: Cardinal; DiskNumberStart: Word); overload;
    procedure ShowExtraFields(Stream: TStream; FileHeader: TCentralDirectoryFileHeader; Root: TTreeNode); overload;
    procedure ShowLocalFileHeader(Stream: TStream);
    procedure ShowZip64(Stream: TStream);
    procedure ShowZip64Locator(Stream: TStream);
  end;

var
  dlgMain: TdlgMain;

implementation

const
  BuffSize = 1024;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function CompressionMethodToStr(Value: Integer): string;
begin
  case Value of
    0: Result := 'NO_COMPRESSION';
    1: Result := 'Shrunk';
    2..5: Result := 'Reduced with compression factor ' + IntToStr(Value - 1);
    6: Result := 'Imploding';
    8: Result := 'DEFLATE';
    9: Result := 'DEFLATE64';
    10: Result := 'PKWARE Imploding';
    11, 13, 15..17: Result := 'Reserved by PKWARE';
    12: Result := 'BZIP2';
    14: Result := 'LZMA';
    18: Result := 'IBM TERSE';
    19: Result := 'IBM LZ77';
    97: Result := 'WavPack';
    98: Result := 'PPMd';
  else
    Result := 'unknown';
  end;
end;

function VersionMadeByToStr(Value: Word): string;
const
  KnownCompat: array [0..19] of string = (
    '0 - MS-DOS and OS/2 (FAT / VFAT / FAT32 file systems)',
    '1 - Amiga',
    '2 - OpenVMS',
    '3 - UNIX',
    '4 - VM/CMS',
    '5 - Atari ST',
    '6 - OS/2 H.P.F.S.',
    '7 - Macintosh',
    '8 - Z-System',
    '9 - CP/M',
    '10 - Windows NTFS',
    '11 - MVS (OS/390 - Z/OS)',
    '12 - VSE',
    '13 - Acorn Risc',
    '14 - VFAT',
    '15 - alternate MVS',
    '16 - BeOS',
    '17 - Tandem',
    '18 - OS/400',
    '19 - OS X (Darwin)'
  );
var
  FileAttrCompatible: string;
begin
  if Value shr 8 in [0..19] then
    FileAttrCompatible := ', compatible: ' + KnownCompat[Value shr 8]
  else
    FileAttrCompatible := '';
  Result := Format('%d.%d%s', [Byte(Value) div 10, Byte(Value) mod 10, FileAttrCompatible]);
end;

function GPBFToStr(Value: Word): string;

  procedure AddValue(const S: string);
  begin
    if Result = '' then
      Result := S
    else
      Result := Result + ', ' + S;
  end;

begin
  if Value = 0 then
  begin
    Result := 'PBF_COMPRESS_NORMAL';
    Exit;
  end;

  if (Value and 7) in [PBF_COMPRESS_NORMAL, PBF_CRYPTED] then
    AddValue('PBF_COMPRESS_NORMAL');

  case Value and 6 of
    PBF_COMPRESS_MAXIMUM: AddValue('PBF_COMPRESS_MAXIMUM');
    PBF_COMPRESS_FAST: AddValue('PBF_COMPRESS_FAST');
    PBF_COMPRESS_SUPERFAST: AddValue('PBF_COMPRESS_SUPERFAST');
  end;

  if PBF_CRYPTED and Value = PBF_CRYPTED then
    AddValue('PBF_CRYPTED');

  if PBF_DESCRIPTOR and Value = PBF_DESCRIPTOR then
    AddValue('PBF_DESCRIPTOR');

  if PBF_UTF8 and Value = PBF_UTF8 then
    AddValue('PBF_UTF8');

  if PBF_STRONG_CRYPT and Value = PBF_STRONG_CRYPT then
    AddValue('PBF_STRONG_CRYPT');
end;

function TdlgMain.FindSign(Stream: TStream): DWORD;
const
  KnownSigns: array [0..5] of DWORD = (
    LOCAL_FILE_HEADER_SIGNATURE,
    DATA_DESCRIPTOR_SIGNATURE,
    CENTRAL_FILE_HEADER_SIGNATURE,
    ZIP64_END_OF_CENTRAL_DIR_SIGNATURE,
    ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE,
    END_OF_CENTRAL_DIR_SIGNATURE
  );

  function CalcLen: Int64;
  begin
    Result := Stream.Size - Stream.Position;
    if Result > BuffSize then
      Result := BuffSize;
  end;

var
  pBuff, pCursor: PByte;
  I, Len, OldPosition: Int64;
  A: Integer;
begin
  Result := 0;
  GetMem(pBuff, BuffSize);
  try
    Len := CalcLen;
    while (Result = 0) and (Len > 4) do
    begin
      OldPosition := Stream.Position;
      Stream.ReadBuffer(pBuff^, Len);
      pCursor := pBuff;
      for I := 0 to Len - 4 do
      begin

        for A := 0 to 5 do
          if PCardinal(pCursor)^ = KnownSigns[A] then
          begin
            Result := KnownSigns[A];
            Break;
          end;
        if Result = 0 then
          Inc(pCursor)
        else
        begin
          Stream.Position := OldPosition + I;
          Break;
        end;
      end;
      if Result = 0 then
      begin
        Len := CalcLen;
        if Len > 0 then
          Stream.Position := Stream.Position - 4;
      end;
    end;
  finally
    FreeMem(pBuff);
  end;
end;

procedure TdlgMain.FormCreate(Sender: TObject);
begin
  HexView.Header.ColumnCaption[ctJmpLine] := '';
  HexView.Header.ColumnWidth[ctDescription] := HexView.ToDpi(340);
  HexView.AddressToRowIndexMode := armFindFirstAny;
  FTreeAddrList := TList<Int64>.Create;
end;

procedure TdlgMain.FormDestroy(Sender: TObject);
begin
  FTreeAddrList.Free;
end;

function TdlgMain.GetCFDNode: TTreeNode;
begin
  if FCentralFileDirNode = nil then
    FCentralFileDirNode := tvZip.Items.AddChild(nil, 'CENTRAL_DIR');
  Result := FCentralFileDirNode;
end;

procedure TdlgMain.HexViewJmpTo(Sender: TObject; const AJmpAddr: Int64;
  AJmpState: TJmpState; var Handled: Boolean);
var
  RowIndex: Integer;
begin
  // Редактор в режиме armFindFirstAny.
  // В этом режиме он будет прыгать на самое первое вхождение адреса
  // где будет располагаться информационный заголовок.
  // Для выделения строки нужно к строке заголовка прибавить единицу
  if (AJmpState = jsJmpDone) and (AJmpAddr >= 0) then
  begin
    RowIndex := HexView.AddressToRowIndex(AJmpAddr);
    HexView.FocusOnRow(RowIndex + 1, ccmSelectRow);
  end;
end;

procedure TdlgMain.LoadStringValue(Stream: TStream; out Value: string;
  nSize: Cardinal; UTF: Boolean);
var
  aString: AnsiString;
begin
  if Integer(nSize) > 0 then
  begin
    {$IFDEF FPC}
    aString := '';
    {$ENDIF}
    SetLength(aString, nSize);
    Stream.ReadBuffer(aString[1], nSize);
    if UTF then
    begin
      {$IFDEF UNICODE}
      Value := string(UTF8ToUnicodeString(aString))
      {$ELSE}
      Value := string(UTF8Decode(aString));
      Value := StringReplace(Value, '?', '_', [rfReplaceAll]);
      {$ENDIF}
    end
    else
      Value := string(ConvertFromOemString(aString));
  end;
end;

procedure TdlgMain.Open(const FilePath: string);
var
  AStream: TBufferedFileStream;
  Sign: DWORD;
begin
  AStream := TBufferedFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone, 1024 * 64);
  HexView.SetDataStream(AStream, 0, soOwned);
  Sign := FindSign(AStream);
  while Sign <> 0 do
  begin
    case Sign of
      LOCAL_FILE_HEADER_SIGNATURE: ShowLocalFileHeader(AStream);
      DATA_DESCRIPTOR_SIGNATURE: ShowDataDescryptor(AStream);
      CENTRAL_FILE_HEADER_SIGNATURE: ShowCentralFileHeader(AStream);
      ZIP64_END_OF_CENTRAL_DIR_SIGNATURE: ShowZip64(AStream);
      ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE: ShowZip64Locator(AStream);
      END_OF_CENTRAL_DIR_SIGNATURE: ShowEndOfCentralDir(AStream);
    else
      AStream.Seek(BuffSize - SizeOf(Cardinal) + 1, soCurrent);
    end;
    Sign := FindSign(AStream);
  end;
end;

procedure TdlgMain.Open1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    Open(OpenDialog.FileName);
end;

procedure TdlgMain.ShowCentralFileHeader(Stream: TStream);
var
  AddrVA, ExtraFieldJmpTo: Int64;
  Data: TCentralDirectoryFileHeader;
  Node: TTreeNode;
  FileName, AddrVAString: string;
begin
  AddrVA := Stream.Position;
  Stream.ReadBuffer(Data, SizeOf(TCentralDirectoryFileHeader));
  if (Byte(Data.VersionMadeBy) > 100) or (Data.VersionNeededToExtract > 100) or (Data.CompressionMethod > 100) then
  begin
    Stream.Seek(1 - SizeOf(TCentralDirectoryFileHeader), soFromCurrent);
    Exit;
  end;
  LoadStringValue(Stream, FileName, Data.FilenameLength,
    Data.GeneralPurposeBitFlag and PBF_UTF8 <> 0);

  Node := tvZip.Items.AddChild(GetCFDNode, 'CENTRAL_FILE_HEADER_' + IntToHex(AddrVA, 8));
  Node.Data := UIntToPtr(FTreeAddrList.Add(AddrVA));
  HexView.DataMap.AddSeparator(AddrVA, 'CENTRAL_FILE_HEADER');
  HexView.DataMap.AddExDescription(4, Format('Signature = 0x%x', [Data.CentralFileHeaderSignature]));
  HexView.DataMap.AddExDescription(2, Format('VersionMadeBy = %d', [Data.VersionMadeBy]), VersionMadeByToStr(Data.VersionMadeBy));
  HexView.DataMap.AddExDescription(2, Format('VersionNeededToExtract = %d', [Data.VersionNeededToExtract]));
  HexView.DataMap.AddMask(2, Format('GeneralPurposeBitFlag = 0x%x', [Data.GeneralPurposeBitFlag]), GPBFToStr(Data.GeneralPurposeBitFlag), False);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddMaskCheck(11, 'PBF_UTF8', '0x800', PBF_UTF8 and Data.GeneralPurposeBitFlag = PBF_UTF8);
  HexView.DataMap.AddMaskCheck(0, 'PBF_CRYPTED', '0x1', PBF_CRYPTED and Data.GeneralPurposeBitFlag = PBF_CRYPTED);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddMaskRadio(0, 'PBF_COMPRESS_NORMAL', '0x0', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_NORMAL);
  HexView.DataMap.AddMaskRadio(1, 'PBF_COMPRESS_MAXIMUM', '0x2', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_MAXIMUM);
  HexView.DataMap.AddMaskRadio(2, 'PBF_COMPRESS_FAST', '0x4', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_FAST);
  HexView.DataMap.AddMaskRadio(2, 'PBF_COMPRESS_SUPERFAST', '0x6', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_SUPERFAST);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddMaskCheck(3, 'PBF_DESCRIPTOR', '0x8', PBF_DESCRIPTOR and Data.GeneralPurposeBitFlag = PBF_DESCRIPTOR);
  HexView.DataMap.AddMaskCheck(6, 'PBF_STRONG_CRYPT', '0x40', PBF_STRONG_CRYPT and Data.GeneralPurposeBitFlag = PBF_STRONG_CRYPT);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddExDescription(2, Format('CompressionMethod = 0x%x', [Data.CompressionMethod]), CompressionMethodToStr(Data.CompressionMethod));
  HexView.DataMap.AddExDescription(2, Format('LastModFileTimeTime = 0x%x', [Data.LastModFileTimeTime]));
  HexView.DataMap.AddExDescription(2, Format('LastModFileTimeDate = 0x%x', [Data.LastModFileTimeDate]));
  HexView.DataMap.AddExDescription(4, Format('Crc32 = 0x%x', [Data.Crc32]));
  HexView.DataMap.AddExDescription(4, Format('CompressedSize = %u', [Data.CompressedSize]));
  HexView.DataMap.AddExDescription(4, Format('UncompressedSize = %u', [Data.UncompressedSize]));
  HexView.DataMap.AddExDescription(2, Format('FilenameLength = %d', [Data.FilenameLength]), FileName, AddrVA + SizeOf(TCentralDirectoryFileHeader), 0, 0);
  if Data.ExtraFieldLength > 0 then
    ExtraFieldJmpTo := AddrVA + SizeOf(TCentralDirectoryFileHeader) + Data.FilenameLength
  else
    ExtraFieldJmpTo := 0;
  HexView.DataMap.AddExDescription(2, Format('ExtraFieldLength = %d', [Data.ExtraFieldLength]), '', ExtraFieldJmpTo, 0, 0);
  HexView.DataMap.AddExDescription(2, Format('FileCommentLength = %d', [Data.FileCommentLength]));
  HexView.DataMap.AddExDescription(2, Format('DiskNumberStart = %d', [Data.DiskNumberStart]));
  HexView.DataMap.AddExDescription(2, Format('InternalFileAttributes = %d', [Data.InternalFileAttributes]));
  HexView.DataMap.AddExDescription(4, Format('ExternalFileAttributes = %d', [Data.ExternalFileAttributes]));
  if Data.RelativeOffsetOfLocalHeader = $FFFFFFFF then
    HexView.DataMap.AddExDescription(4, 'RelativeOffsetOfLocalHeader = -1')
  else
  begin
    AddrVAString := IntToHex(Data.RelativeOffsetOfLocalHeader, 8);
    HexView.DataMap.AddExDescription(4, 'RelativeOffsetOfLocalHeader = ' + AddrVAString,
      '', Data.RelativeOffsetOfLocalHeader, 30, Length(AddrVAString));
  end;
  HexView.DataMap.AddLine;
  HexView.DataMap.AddRaw(Data.FilenameLength);
  ShowExtraFields(Stream, Data, Node);
  if Data.FileCommentLength > 0 then
  begin
    HexView.DataMap.SetRowLineSeparated(HexView.DataMap.LastDataMapIndex, True);
    HexView.DataMap.AddComment('File Comment buffer');
    HexView.DataMap.SetRowLineSeparated(HexView.DataMap.LastDataMapIndex, True);
    HexView.DataMap.AddRaw(Data.FileCommentLength);
    HexView.DataMap.AddLine;
  end;
end;

procedure TdlgMain.ShowDataDescryptor(Stream: TStream);
var
  AddrVA: Int64;
  Data: TDataDescriptor;
  Node: TTreeNode;
begin
  AddrVA := Stream.Position;
  Stream.ReadBuffer(Data, SizeOf(TDataDescriptor));
  if Data.Crc32 = LOCAL_FILE_HEADER_SIGNATURE then
  begin
    Node := tvZip.Items.AddChild(nil, 'SPAN_DESCRIPTOR_' + IntToHex(AddrVA, 8));
    HexView.DataMap.AddSeparator(AddrVA, 'SPAN_DESCRIPTOR');
    HexView.DataMap.AddExDescription(4, Format('Signature = 0x%x', [Data.DescriptorSignature]));
    Stream.Position := AddrVA + SizeOf(Data.DescriptorSignature);
  end
  else
  begin
    Node := tvZip.Items.AddChild(nil, 'DATA_DESCRIPTOR_' + IntToHex(AddrVA, 8));
    HexView.DataMap.AddSeparator(AddrVA, 'DATA_DESCRIPTOR');
    HexView.DataMap.AddExDescription(4, Format('Signature = 0x%x', [Data.DescriptorSignature]));
    HexView.DataMap.AddExDescription(4, Format('Crc32 = 0x%x', [Data.Crc32]));
    HexView.DataMap.AddExDescription(4, Format('CompressedSize = %d', [Data.CompressedSize]));
    HexView.DataMap.AddExDescription(4, Format('UncompressedSize = %d', [Data.UncompressedSize]));
  end;
  Node.Data := UIntToPtr(FTreeAddrList.Add(AddrVA));
end;

procedure TdlgMain.ShowEndOfCentralDir(Stream: TStream);
var
  AddrVA, CommentJmp: Int64;
  Data: TEndOfCentralDir;
  Node: TTreeNode;
  AddrVAString, Comment: string;
begin
  AddrVA := Stream.Position;
  Node := tvZip.Items.AddChild(nil, 'END_OF_CENTRAL_DIR_' + IntToHex(AddrVA, 8));
  Node.Data := UIntToPtr(FTreeAddrList.Add(AddrVA));
  HexView.DataMap.AddSeparator(AddrVA, 'END_OF_CENTRAL_DIR');
  Stream.ReadBuffer(Data, SizeOf(TEndOfCentralDir));
  LoadStringValue(Stream, Comment, Data.ZipfileCommentLength, False);
  HexView.DataMap.AddExDescription(4, Format('Signature = 0x%x', [Data.EndOfCentralDirSignature]));
  HexView.DataMap.AddExDescription(2, Format('NumberOfThisDisk = %d', [Data.NumberOfThisDisk]));
  HexView.DataMap.AddExDescription(2, Format('DiskNumberStart = %d', [Data.DiskNumberStart]));
  HexView.DataMap.AddExDescription(2, Format('TotalNumberOfEntriesOnThisDisk = %d', [Data.TotalNumberOfEntriesOnThisDisk]));
  HexView.DataMap.AddExDescription(2, Format('TotalNumberOfEntries = %d', [Data.TotalNumberOfEntries]));
  HexView.DataMap.AddExDescription(4, Format('SizeOfTheCentralDirectory = %d', [Data.SizeOfTheCentralDirectory]));
  AddrVAString := IntToHex(Data.RelativeOffsetOfCentralDirectory, 8);
  HexView.DataMap.AddExDescription(4, 'RelativeOffsetOfCentralDirectory = ' + AddrVAString,
    '', Data.RelativeOffsetOfCentralDirectory, 35, Length(AddrVAString));
  if Data.ZipfileCommentLength > 0 then
    CommentJmp := AddrVA + SizeOf(TEndOfCentralDir)
  else
    CommentJmp := 0;
  HexView.DataMap.AddExDescription(2, Format('ZipfileCommentLength = %d', [Data.ZipfileCommentLength]), '', CommentJmp, 0, 0);
  if Data.ZipfileCommentLength > 0 then
    HexView.DataMap.AddLine;
end;

procedure TdlgMain.ShowExtraFields(Stream: TStream; Size: Integer;
  Root: TTreeNode; UncompressedSize, CompressedSize,
  RelativeOffsetOfLocalHeader: Cardinal; DiskNumberStart: Word);
var
  Buff, EOFBuff: Pointer;
  HeaderID, BlockSize: Word;
  StartPos, BuffCount, RelativeOffset, AddrVA: Int64;
  RelativeOffsetStr: string;
  FileTime: TFileTime;
  Node: TTreeNode;

  function GetOffset(Value: Integer): Pointer;
  begin
    Result := UIntToPtr(PtrToUInt(EOFBuff) - NativeUInt(Value));
  end;

begin
  StartPos := Stream.Position;
  BuffCount := Size;
  GetMem(Buff, Size);
  try
    Stream.ReadBuffer(Buff^, Size);
    EOFBuff := UIntToPtr(PtrToUInt(Buff) + NativeUInt(Size));
    while Size > 4 do
    begin
      Node := tvZip.Items.AddChild(Root, '');
      AddrVA := StartPos + BuffCount - Size;
      Node.Data := UIntToPtr(FTreeAddrList.Add(AddrVA));
      HeaderID := PWord(GetOffset(Size))^;
      Dec(Size, 2);
      BlockSize := PWord(GetOffset(Size))^;
      Dec(Size, 2);
      case HeaderID of
        SUPPORTED_EXDATA_ZIP64:
        begin
          Node.Text := 'EXDATA_ZIP64_' + IntToHex(AddrVA, 8);
          HexView.DataMap.AddSeparator(Node.Text);
          HexView.DataMap.AddExDescription(2, Format('Tag = 0x%x', [HeaderID]));
          HexView.DataMap.AddExDescription(2, Format('Size = %d', [BlockSize]));
          if UncompressedSize = MAXDWORD then
          begin
            if Size < 8 then Break;
            HexView.DataMap.AddExDescription(8, Format('UncompressedSize = %d', [PInt64(GetOffset(Size))^]));
            Dec(Size, 8);
            Dec(BlockSize, 8);
          end;
          if CompressedSize = MAXDWORD then
          begin
            if Size < 8 then Break;
            HexView.DataMap.AddExDescription(8, Format('CompressedSize = %d', [PInt64(GetOffset(Size))^]));
            Dec(Size, 8);
            Dec(BlockSize, 8);
          end;
          if RelativeOffsetOfLocalHeader = MAXDWORD then
          begin
            if Size < 8 then Break;
            RelativeOffset := PInt64(GetOffset(Size))^;
            RelativeOffsetStr := IntToHex(RelativeOffset, 8);
            HexView.DataMap.AddExDescription(8, 'RelativeOffsetOfLocalHeader = ' +
              RelativeOffsetStr, '', RelativeOffset, 30, Length(RelativeOffsetStr));
            Dec(Size, 8);
            Dec(BlockSize, 8);
          end;
          if DiskNumberStart = MAXWORD then
          begin
            if Size < 4 then Break;
            HexView.DataMap.AddExDescription(8, Format('DiskNumberStart = %d', [PInteger(GetOffset(Size))^]));
            Dec(Size, 4);
            Dec(BlockSize, 4);
          end;
          Dec(Size, BlockSize);
        end;

        SUPPORTED_EXDATA_NTFSTIME:
        begin

          Node.Text := 'EXDATA_NTFSTIME_' + IntToHex(AddrVA, 8);
          HexView.DataMap.AddSeparator(Node.Text);
          HexView.DataMap.AddExDescription(2, Format('Tag = 0x%x', [HeaderID]));
          HexView.DataMap.AddExDescription(2, Format('Size = %d', [BlockSize]));

          if Size < 32 then
          begin
            HexView.DataMap.AddRaw(Size);
            Break;
          end;

          if BlockSize <> 32 then
          begin
            HexView.DataMap.AddRaw(BlockSize);
            Dec(Size, BlockSize);
            Continue;
          end;

          HexView.DataMap.AddExDescription(4, 'Reserved');
          Dec(Size, 4);
          Dec(BlockSize, 4);

          if PWord(GetOffset(Size))^ <> 1 then
          begin
            HexView.DataMap.AddRaw(BlockSize);
            Dec(Size, BlockSize);
            Continue;
          end;

          HexView.DataMap.AddExDescription(2, 'Tag1', 'NTFS attribute tag value #1');
          Dec(Size, 2);
          Dec(BlockSize, 2);

          if PWord(GetOffset(Size))^ <> SizeOf(TNTFSFileTime) then
          begin
            HexView.DataMap.AddRaw(BlockSize);
            Dec(Size, BlockSize);
            Continue;
          end;

          HexView.DataMap.AddExDescription(2, 'Size1', 'Size of attribute #1, in bytes');
          Dec(Size, 2);
          Dec(BlockSize, 2);

          FileTime := PFileTime(GetOffset(Size))^;
          Dec(Size, SizeOf(TFileTime));
          Dec(BlockSize, SizeOf(TFileTime));
          HexView.DataMap.AddExDescription(SizeOf(TFileTime), 'Mtime',
            Format('Last Write Time: %s', [DateTimeToStr(FileTimeToLocalDateTime(FileTime))]));

          FileTime := PFileTime(GetOffset(Size))^;
          Dec(Size, SizeOf(TFileTime));
          Dec(BlockSize, SizeOf(TFileTime));
          HexView.DataMap.AddExDescription(SizeOf(TFileTime), 'Atime',
            Format('Last Access Time: %s', [DateTimeToStr(FileTimeToLocalDateTime(FileTime))]));

          FileTime := PFileTime(GetOffset(Size))^;
          Dec(Size, SizeOf(TFileTime));
          Dec(BlockSize, SizeOf(TFileTime));
          HexView.DataMap.AddExDescription(SizeOf(TFileTime), 'Ctime',
            Format('Creation Time: %s', [DateTimeToStr(FileTimeToLocalDateTime(FileTime))]));

       end;
      else

        Node.Text := 'UNKNOWN EXDATA_' + IntToHex(AddrVA, 8);
        HexView.DataMap.AddSeparator(Node.Text);
        HexView.DataMap.AddExDescription(2, Format('Tag = 0x%x', [HeaderID]));
        HexView.DataMap.AddExDescription(2, Format('Size = %d', [BlockSize]));
        HexView.DataMap.SetRowLineSeparated(HexView.DataMap.LastDataMapIndex, True);
        HexView.DataMap.AddRaw(BlockSize);

      end;
      Dec(Size, BlockSize);
    end;
  finally
    FreeMem(Buff);
  end;
end;

procedure TdlgMain.ShowExtraFields(Stream: TStream;
  FileHeader: TCentralDirectoryFileHeader; Root: TTreeNode);
begin
  if FileHeader.ExtraFieldLength > 0 then
    ShowExtraFields(Stream, FileHeader.ExtraFieldLength, Root,
    FileHeader.UncompressedSize, FileHeader.CompressedSize,
    FileHeader.RelativeOffsetOfLocalHeader, FileHeader.DiskNumberStart)
  else
    HexView.DataMap.AddLine;
end;

procedure TdlgMain.ShowLocalFileHeader(Stream: TStream);
var
  AddrVA, ExtraFieldJmpTo: Int64;
  Data: TLocalFileHeader;
  Node: TTreeNode;
  FileName: string;
begin
  AddrVA := Stream.Position;
  Stream.ReadBuffer(Data, SizeOf(TLocalFileHeader));
  if (Data.VersionNeededToExtract > 100) or (Data.CompressionMethod > 100) then
  begin
    Stream.Seek(1 - SizeOf(TLocalFileHeader), soFromCurrent);
    Exit;
  end;
  LoadStringValue(Stream, FileName, Data.FilenameLength,
    Data.GeneralPurposeBitFlag and PBF_UTF8 <> 0);
  Node := tvZip.Items.AddChild(nil, 'LOCAL_FILE_HEADER_' + IntToHex(AddrVA, 8));
  Node.Data := UIntToPtr(FTreeAddrList.Add(AddrVA));
  HexView.DataMap.AddSeparator(AddrVA, 'LOCAL_FILE_HEADER');
  HexView.DataMap.AddExDescription(4, Format('Signature = 0x%x', [Data.LocalFileHeaderSignature]));
  HexView.DataMap.AddExDescription(2, Format('VersionNeededToExtract = %d', [Data.VersionNeededToExtract]));
  HexView.DataMap.AddMask(2, Format('GeneralPurposeBitFlag = 0x%x', [Data.GeneralPurposeBitFlag]), GPBFToStr(Data.GeneralPurposeBitFlag), False);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddMaskCheck(11, 'PBF_UTF8', '0x800', PBF_UTF8 and Data.GeneralPurposeBitFlag = PBF_UTF8);
  HexView.DataMap.AddMaskCheck(0, 'PBF_CRYPTED', '0x1', PBF_CRYPTED and Data.GeneralPurposeBitFlag = PBF_CRYPTED);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddMaskRadio(0, 'PBF_COMPRESS_NORMAL', '0x0', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_NORMAL);
  HexView.DataMap.AddMaskRadio(1, 'PBF_COMPRESS_MAXIMUM', '0x2', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_MAXIMUM);
  HexView.DataMap.AddMaskRadio(2, 'PBF_COMPRESS_FAST', '0x4', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_FAST);
  HexView.DataMap.AddMaskRadio(2, 'PBF_COMPRESS_SUPERFAST', '0x6', 6 and Data.GeneralPurposeBitFlag = PBF_COMPRESS_SUPERFAST);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddMaskCheck(3, 'PBF_DESCRIPTOR', '0x8', PBF_DESCRIPTOR and Data.GeneralPurposeBitFlag = PBF_DESCRIPTOR);
  HexView.DataMap.AddMaskCheck(6, 'PBF_STRONG_CRYPT', '0x40', PBF_STRONG_CRYPT and Data.GeneralPurposeBitFlag = PBF_STRONG_CRYPT);
  HexView.DataMap.AddMaskSeparator;
  HexView.DataMap.AddExDescription(2, Format('CompressionMethod = 0x%x', [Data.CompressionMethod]), CompressionMethodToStr(Data.CompressionMethod));
  HexView.DataMap.AddExDescription(2, Format('LastModFileTimeTime = 0x%x', [Data.LastModFileTimeTime]));
  HexView.DataMap.AddExDescription(2, Format('LastModFileTimeDate = 0x%x', [Data.LastModFileTimeDate]));
  HexView.DataMap.AddExDescription(4, Format('Crc32 = 0x%x', [Data.Crc32]));
  HexView.DataMap.AddExDescription(4, Format('CompressedSize = %d', [Data.CompressedSize]));
  HexView.DataMap.AddExDescription(4, Format('UncompressedSize = %d', [Data.UncompressedSize]));
  HexView.DataMap.AddExDescription(2, Format('FilenameLength = %d', [Data.FilenameLength]), FileName, AddrVA + SizeOf(TLocalFileHeader), 0, 0);
  if Data.ExtraFieldLength > 0 then
    ExtraFieldJmpTo := AddrVA + SizeOf(TLocalFileHeader) + Data.FilenameLength
  else
    ExtraFieldJmpTo := 0;
  HexView.DataMap.AddExDescription(2, Format('ExtraFieldLength = %d', [Data.ExtraFieldLength]), '', ExtraFieldJmpTo, 0, 0);
  HexView.DataMap.AddLine;
  HexView.DataMap.AddRaw(Data.FilenameLength);
  HexView.DataMap.AddLine;
end;

procedure TdlgMain.ShowZip64(Stream: TStream);
var
  AddrVA: Int64;
  Data: TZip64EOFCentralDirectoryRecord;
  Node: TTreeNode;
  AddrVAString: string;
begin
  AddrVA := Stream.Position;
  Stream.ReadBuffer(Data, SizeOf(TZip64EOFCentralDirectoryRecord));
  if (Byte(Data.VersionMadeBy) > 100) or (Data.VersionNeededToExtract > 100) then
  begin
    Stream.Seek(1 - SizeOf(TZip64EOFCentralDirectoryRecord), soFromCurrent);
    Exit;
  end;
  Node := tvZip.Items.AddChild(nil, 'ZIP64_END_OF_CENTRAL_DIR_' + IntToHex(AddrVA, 8));
  Node.Data := UIntToPtr(FTreeAddrList.Add(AddrVA));
  HexView.DataMap.AddSeparator(AddrVA, 'ZIP64_END_OF_CENTRAL_DIR');
  HexView.DataMap.AddExDescription(4, Format('Signature = 0x%x', [Data.Zip64EndOfCentralDirSignature]));
  HexView.DataMap.AddExDescription(8, Format('SizeOfZip64EOFCentralDirectoryRecord = %d', [Data.SizeOfZip64EOFCentralDirectoryRecord]));
  HexView.DataMap.AddExDescription(2, Format('VersionMadeBy = %d', [Data.VersionMadeBy]), VersionMadeByToStr(Data.VersionMadeBy));
  HexView.DataMap.AddExDescription(2, Format('VersionNeededToExtract = %d', [Data.VersionNeededToExtract]));
  HexView.DataMap.AddExDescription(4, Format('NumberOfThisDisk = %d', [Data.NumberOfThisDisk]));
  HexView.DataMap.AddExDescription(4, Format('DiskNumberStart = %d', [Data.DiskNumberStart]));
  HexView.DataMap.AddExDescription(8, Format('TotalNumberOfEntriesOnThisDisk = %d', [Data.TotalNumberOfEntriesOnThisDisk]));
  HexView.DataMap.AddExDescription(8, Format('TotalNumberOfEntries = %d', [Data.TotalNumberOfEntries]));
  HexView.DataMap.AddExDescription(8, Format('SizeOfTheCentralDirectory = %d', [Data.SizeOfTheCentralDirectory]));
  AddrVAString := IntToHex(Data.RelativeOffsetOfCentralDirectory, 8);
  HexView.DataMap.AddExDescription(8, 'RelativeOffsetOfCentralDirectory = ' + AddrVAString,
    '', Data.RelativeOffsetOfCentralDirectory, 35, Length(AddrVAString));
end;

procedure TdlgMain.ShowZip64Locator(Stream: TStream);
var
  AddrVA: Int64;
  Data: TZip64EOFCentralDirectoryLocator;
  Node: TTreeNode;
  AddrVAString: string;
begin
  AddrVA := Stream.Position;
  Node := tvZip.Items.AddChild(nil, 'ZIP64_END_OF_CENTRAL_DIR_LOCATOR_' + IntToHex(AddrVA, 8));
  Node.Data := UIntToPtr(FTreeAddrList.Add(AddrVA));
  HexView.DataMap.AddSeparator(AddrVA, 'ZIP64_END_OF_CENTRAL_DIR_LOCATOR');
  Stream.ReadBuffer(Data, SizeOf(TZip64EOFCentralDirectoryLocator));
  HexView.DataMap.AddExDescription(4, Format('Signature = 0x%x', [Data.Signature]));
  HexView.DataMap.AddExDescription(4, Format('DiskNumberStart = %d', [Data.DiskNumberStart]));
  AddrVAString := IntToHex(Data.RelativeOffset, 8);
  HexView.DataMap.AddExDescription(8, 'RelativeOffset = ' + AddrVAString,
    '', Data.RelativeOffset, 17, Length(AddrVAString));
  HexView.DataMap.AddExDescription(4, Format('TotalNumberOfDisks = %d', [Data.TotalNumberOfDisks]));
end;

procedure TdlgMain.tvZipChange(Sender: TObject; Node: TTreeNode);
var
  RowIndex: Integer;
  AddrVA: Int64;
begin
  if Assigned(Node) and (Node <> FCentralFileDirNode) then
  begin
    AddrVA := FTreeAddrList[PtrToUInt(Node.Data)];
    RowIndex := HexView.AddressToRowIndex(AddrVA);
    HexView.FocusOnAddress(AddrVA, ccmNone);
    HexView.FocusOnRow(RowIndex + 1, ccmSelectRow);
  end;
end;

end.
