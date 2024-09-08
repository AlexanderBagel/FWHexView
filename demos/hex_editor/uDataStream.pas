unit uDataStream;

interface

uses
  Classes,
  SysUtils;

type
  TUpdateBufEvent = procedure(AddrVA: Int64; ASize: Integer; pBuff: PByte) of object;

  TBufferedStream = class(TStream)
  private
    FStream: TStream;
    FPosition: Int64;
    FBuff: array of Byte;
    FBuffStartPosition: Int64;
    FBuffSize: Integer;
    FUpdate: TUpdateBufEvent;
    function GetBuffer_EndPosition: Int64;
    procedure SetStream(const Value: TStream);
  protected
    property Buffer_StartPosition: Int64 read FBuffStartPosition;
    property Buffer_EndPosition: Int64 read GetBuffer_EndPosition;
    function Buffer_Read(var Buffer; Size: LongInt): Longint;
    function Buffer_Update: Boolean;
    function Buffer_Contains(APosition: Int64): Boolean;
  public
    constructor Create(AStream: TStream);
    procedure Invalidate;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Stream: TStream read FStream write SetStream;
    property OnUpdate: TUpdateBufEvent read FUpdate write FUpdate;
  end;

implementation

{ TBufferedStream }

function TBufferedStream.Buffer_Contains(APosition: Int64): Boolean;
begin
  Result := (FBuffStartPosition <= APosition) and (GetBuffer_EndPosition >= APosition);
end;

function TBufferedStream.Buffer_Read(var Buffer; Size: LongInt): Longint;
begin
  Result := 0;
  if not Buffer_Contains(FPosition) then
    Exit;
  Result := Buffer_EndPosition - FPosition + 1;
  Assert(Result > 0);
  if Result > Size then
    Result := Size;
  Move(FBuff[Integer(FPosition - Buffer_StartPosition)], Buffer, Result);
  Inc(FPosition, Result);
end;

function TBufferedStream.Buffer_Update: Boolean;
begin
  FStream.Position := FPosition;
  FBuffStartPosition := FPosition;
  SetLength(FBuff, FBuffSize);
  SetLength(FBuff, FStream.Read(FBuff[0], FBuffSize));
  Result := Length(FBuff) > 0;
  if Result and Assigned(FUpdate) then
    FUpdate(FPosition, Length(FBuff), @FBuff[0]);
end;

constructor TBufferedStream.Create(AStream: TStream);
begin
  FStream := AStream;
  FBuffSize := 1024 * 1024 * 64;
end;

function TBufferedStream.GetBuffer_EndPosition: Int64;
begin
  Result := Int64(Length(FBuff)) + FBuffStartPosition - Int64(1);
end;

procedure TBufferedStream.Invalidate;
begin
  FBuff := nil;
end;

function TBufferedStream.Read(var Buffer; Count: Longint): Longint;
var
  Readed: Integer;
begin
  Result := 0;
  while Result < Count do
  begin
    Readed := Buffer_Read(PAnsiChar(@Buffer)[Result], Count - Result);
    Inc(Result, Readed);
    if Readed = 0 then
      if not Buffer_Update then
        Exit;
  end;
end;

function TBufferedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FStream.Size + Offset;
  else
    Assert(False, 'Unknown TSeekOrigin');
  end;
  Result := FPosition;
  if not Buffer_Contains(FPosition) then
    Invalidate;
end;

procedure TBufferedStream.SetStream(const Value: TStream);
begin
  FStream := Value;
  Invalidate;
end;

function TBufferedStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('Stream is read-only');
end;

end.
