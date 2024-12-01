unit ZipSupport;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Unix, Baseunix, DateUtils, Types,
{$ENDIF}
{$IFDEF FPC}
  LConvEncoding,
{$ENDIF}
 SysUtils;

const
  LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
  DATA_DESCRIPTOR_SIGNATURE = $08074B50;
  EXTRA_DATA_SIGNATURE = $08064B50;
  CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;
  CENTRAL_DIRECTORY_DIGITAL_SIGNATURE = $05054B50;
  ZIP64_END_OF_CENTRAL_DIR_SIGNATURE = $06064B50;
  ZIP64_END_OF_CENTRAL_DIR_LOCATOR_SIGNATURE = $07064B50;
  END_OF_CENTRAL_DIR_SIGNATURE = $06054B50;

  PBF_CRYPTED = 1;
  // (For Methods 8 and 9 - Deflating)
  PBF_COMPRESS_NORMAL = 0;
  PBF_COMPRESS_MAXIMUM = 2;
  PBF_COMPRESS_FAST = 4;
  PBF_COMPRESS_SUPERFAST = 6;

  PBF_DESCRIPTOR = 8;
  PBF_STRONG_CRYPT = 64;
  PBF_UTF8 = $800;

  SUPPORTED_EXDATA_ZIP64 = 1;
  SUPPORTED_EXDATA_NTFSTIME = 10;

type
  TFileTime = FILETIME;

  TNTFSFileTime = packed record
    Mtime: TFileTime;
    Atime: TFileTime;
    Ctime: TFileTime;
  end;

  TCentralDirectoryFileHeader = packed record
    CentralFileHeaderSignature: Cardinal;   // (0x02014b50)
    VersionMadeBy,
    VersionNeededToExtract,
    GeneralPurposeBitFlag,
    CompressionMethod,
    LastModFileTimeTime,
    LastModFileTimeDate: Word;
    Crc32,
    CompressedSize,
    UncompressedSize: Cardinal;
    FilenameLength,
    ExtraFieldLength,
    FileCommentLength,
    DiskNumberStart,
    InternalFileAttributes: Word;
    ExternalFileAttributes,
    RelativeOffsetOfLocalHeader: Cardinal;
    // file name (variable size)
    // extra field (variable size)
    // file comment (variable size)
  end;

  TDataDescriptor = packed record
    DescriptorSignature,        // (0x08074b50)
    Crc32,
    CompressedSize,
    UncompressedSize: Cardinal;
    {For Zip64 format archives, the compressed
    and uncompressed sizes are 8 bytes each. ??!!}
  end;

  TEndOfCentralDir = packed record
    EndOfCentralDirSignature: Cardinal; // (0x06054b50)
    NumberOfThisDisk,
    DiskNumberStart,
    TotalNumberOfEntriesOnThisDisk,
    TotalNumberOfEntries: Word;
    SizeOfTheCentralDirectory,
    RelativeOffsetOfCentralDirectory: Cardinal;
    ZipfileCommentLength: Word;
    // .ZIP file comment       (variable size)
  end;

  PLocalFileHeader = ^TLocalFileHeader;
  TLocalFileHeader = packed record
    LocalFileHeaderSignature: Cardinal; // (0x04034b50)
    VersionNeededToExtract,
    GeneralPurposeBitFlag,
    CompressionMethod,
    LastModFileTimeTime,
    LastModFileTimeDate: Word;
    Crc32,
    CompressedSize,
    UncompressedSize: Cardinal;
    FilenameLength,
    ExtraFieldLength: Word;
    // file name (variable size)
    // extra field (variable size)
  end;

  TZip64EOFCentralDirectoryRecord = packed record
    Zip64EndOfCentralDirSignature: Cardinal; // (0x06064b50)
    SizeOfZip64EOFCentralDirectoryRecord: int64;
    VersionMadeBy,
    VersionNeededToExtract: Word;
    NumberOfThisDisk,                        // number of this disk
    DiskNumberStart: Cardinal;               // number of the disk with the start of the central directory
    TotalNumberOfEntriesOnThisDisk,          // total number of entries in the central directory on this disk
    TotalNumberOfEntries,                    // total number of entries in the central directory
    SizeOfTheCentralDirectory,               // size of the central directory
    RelativeOffsetOfCentralDirectory: Int64; // offset of start of central directory with respect to the starting disk number
    // zip64 extensible data sector    (variable size)
  end;

  TZip64EOFCentralDirectoryLocator = packed record
    Signature,                 // zip64 end of central dir locator signature  (0x07064b50)
    DiskNumberStart: Cardinal; // number of the disk with the start of the zip64 end of central directory
    RelativeOffset: Int64;     // relative offset of the zip64 end of central directory record
    TotalNumberOfDisks: Cardinal;
  end;

  function ConvertFromOemString(const Value: AnsiString): AnsiString;
  function FileTimeToLocalDateTime(AFileTime: TFileTime): TDateTime;
  function PtrToUInt(Value: Pointer): NativeUInt;
  function UIntToPtr(Value: NativeUInt): Pointer;

implementation

function ConvertFromOemString(const Value: AnsiString): AnsiString;
begin
  Result := Value;
  if Result = '' then Exit;
  UniqueString(Result);
  {$IFDEF FPC}
  Result := CP866ToUTF8(Value);
  {$ELSE}
  OemToAnsi(PAnsiChar(Result), PAnsiChar(Result));
  {$ENDIF}
end;

{$IFDEF LINUX}

// emulation of api for working with NTFS attributes, in particular with time

const
  NSECPERSEC = 10000000;
  NSECPERMSEC = 10000;
  MSECPERSEC = 1000;
  SECSPERMIN  = 60;
  MINSPERHOUR = 60;
  HOURSPERDAY = 24;
  EPOCHWEEKDAY = 1;  // January 1, 1601 was a Monday
  DAYSPERWEEK = 7;
  DAYSPERQUADRICENTENNIUM = 365 * 400 + 97;
  DAYSPERNORMALQUADRENNIUM = 365 * 4 + 1;
  MonthLength: array [Boolean] of array [0..11] of Integer =
	  ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
	   (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

function SystemTimeToFileTime(
  const ATime: TSystemTime; out AFileTime: TFileTime): Boolean;

  function IsLeapYear: Boolean;
  begin
    Result := (ATime.Year mod 400 = 0) and
      (ATime.Year mod 100 <> 0) or (ATime.Year mod 4 = 0);
  end;

  function IsDayTooBig: Boolean;
  begin
    Result := ATime.Day >
      MonthLength[(ATime.Month = 2) and IsLeapYear][ATime.Month - 1];
  end;

var
  CalcYear, CalcMonth, CalcLeapCount, CalcDay: UInt64;
begin
  Result := False;

  if ATime.Millisecond > 999 then Exit;
  if ATime.Second > 59 then Exit;
  if ATime.Minute > 59 then Exit;
  if ATime.Hour > 23 then Exit;
  if (ATime.Month < 1) or (ATime.Month > 12) then Exit;
  if (ATime.Day < 1) or IsDayTooBig then Exit;
  if (ATime.Year < 1601) or (ATime.Year > 30827) then Exit;

  // conversion with leap years
  // if the current month is less than March, we simply subtract the year
  // and add 12 months, this will be taken into account in the future
  CalcYear := ATime.Year;
  CalcMonth := ATime.Month;
  if ATime.Month < 3 then
  begin
    Dec(CalcYear);
    Inc(CalcMonth, 12);
  end;

  // number of leap years within a century
  CalcLeapCount := (3 * (CalcYear div 100) + 3) shr 2;

  // number of days since 1601
  CalcDay :=
    (36525 * CalcYear) div 100 - CalcLeapCount + // year * number of days in a century with correction for leap years
    (1959 * (CalcMonth + 1)) shr 6 +             // month * average number of days
    ATime.Day -                                  // day
    584817;                                      // minus the number of days before 1601.

  // result calculation
  PUint64(@AFileTime)^ := ((((
    CalcDay * HOURSPERDAY +
    ATime.Hour) * MINSPERHOUR +
    ATime.Minute) * SECSPERMIN +
    ATime.Second) * MSECPERSEC +
    ATime.Millisecond) * NSECPERMSEC;

  Result := True;
end;

function FileTimeToSystemTime(const AFileTime: TFileTime;
  out ASystemTime: TSystemTime): Boolean;
var
  FullTime, CalcYear, CalcMonth, CalcLeapCount,
  CalcYearDay, CalcDay, CalcSecond: Int64;
begin
  Result := False;
  FullTime := PInt64(@AFileTime)^;
  if FullTime < 0 then Exit;

  // get the number of milliseconds and convert the time to seconds.
  ASystemTime.Millisecond := (FullTime mod NSECPERSEC) div NSECPERMSEC;
  FullTime := FullTime div NSECPERSEC;

  // get the number of seconds
  CalcSecond := FullTime mod SECSPERDAY;

  // count the time of day
  ASystemTime.Hour := CalcSecond div SECSPERHOUR;
  CalcSecond := CalcSecond mod SECSPERHOUR;
  ASystemTime.Minute := CalcSecond div SECSPERMIN;
  ASystemTime.Second := CalcSecond mod SECSPERMIN;

  // get the number of days
  CalcDay := FullTime div SECSPERDAY;

  // day of the week
  ASystemTime.DayOfWeek := (EPOCHWEEKDAY + CalcDay) mod DAYSPERWEEK;

  // count the year, month and day of the month
  CalcLeapCount :=
    (3 * ((CalcDay shl 2 + 1227) div DAYSPERQUADRICENTENNIUM) + 3) shr 2;
  Inc(CalcDay, 28188 + CalcLeapCount);
  CalcYear := (20 * CalcDay - 2442) div (5 * DAYSPERNORMALQUADRENNIUM);
  CalcYearDay := CalcDay - (CalcYear * DAYSPERNORMALQUADRENNIUM) shr 2;
  CalcMonth := (CalcYearDay shl 6) div 1959;

  // the result for the year that starts from March, if it extend to the next year,
  // then for conversion we subtract 12 months and increase the year
  ASystemTime.Month := CalcMonth - 1;
  ASystemTime.Year := CalcYear + 1524;
  if ASystemTime.Month > 12 then
  begin
    Dec(ASystemTime.Month, 12);
    Inc(ASystemTime.Year);
  end;

  ASystemTime.Day := CalcYearDay - (1959 * CalcMonth) shr 6;
end;

function UnixDateToFileTime(Value: Int64): TFileTime;
var
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(UnixToDateTime(Value), SystemTime);
  SystemTimeToFileTime(SystemTime, Result);
end;

function FileTimeToUnixDate(Value: TFileTime): Int64;
var
  SystemTime: TSystemTime;
begin
  FileTimeToSystemTime(Value, SystemTime);
  Result := DateTimeToUnix(SystemTimeToDateTime(SystemTime));
end;
{$ENDIF}

function FileTimeToLocalDateTime(AFileTime: TFileTime): TDateTime;
var
  {$IFDEF LINUX}
  UnixTime: Int64;
  {$ELSE}
  SystemTime: TSystemTime;
  {$ENDIF}
begin
  {$IFDEF LINUX}
  UnixTime := FileTimeToUnixDate(AFileTime);
  Result := UnixToDateTime(UnixTime, False);
  {$ELSE}
  // Rouse_ 25.10.2013
  // Правка небольшой ошибки замеченой Владиславом Нечепоренко
  //FileTimeToSystemTime(CurrentItem.Attributes.ftLastWriteTime, SystemTyme);
  FileTimeToLocalFileTime(AFileTime, AFileTime);
  {$IFDEF FPC}
  SystemTime := Default(TSystemTime);
  {$ENDIF}
  FileTimeToSystemTime(AFileTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
  {$ENDIF}
end;

function PtrToUInt(Value: Pointer): NativeUInt;
begin
  Result := {%H-}NativeUInt(Value);
end;

function UIntToPtr(Value: NativeUInt): Pointer;
begin
  Result := {%H-}Pointer(Value);
end;

end.

