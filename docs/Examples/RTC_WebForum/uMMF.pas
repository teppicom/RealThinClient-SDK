unit uMMF;

{$include rtcDefs.inc}

interface

uses
  Classes, Windows, SysUtils, Dialogs;

var
  MEMORY_DELTA : Longint = $FFFF;

type

  TMemoryMappedFileStream = class (TStream)
  private
    FFileHandle:THandle;
    FMapHandle:THandle;
    FFileName:string;
    FMemory:pointer;
    FCapacity:longint;
    FSize:longint;
    FPosition:longint;
    FAccess:dword;     //file access attributes
    FShare:longint;      //file sharing attributes
    FPageProtect:longint;        //mapping object page protection attributes
    FMemProtect:longint;     //protection of the mapped memory
    FMapName:string;               //Name of the file mapping object
    FMemoryDelta:longint; //file size increment
    procedure SetCapacity(NewCapacity:longint);
    procedure SetAttributes(Mode:word);
    procedure CloseHandles(Shrink : Boolean = False);
  public
    constructor Create(const FileName, MapName: string; Mode: Word);
    constructor Open(const MapName:string; Mode:word);
    destructor Destroy; override;
    function Read(var Buffer;Count:longint):longint;override;
    function Write(const Buffer;Count:longint):longint;override;
    procedure SetSize(NewSize:longint);override;
    function Seek(Offset:longint;Origin:word):longint;override;
    procedure Flush;virtual;
    property Memory:pointer read FMemory;
    property FileName:string read FFileName;
    property MapName:string read FMapName;
    property MemoryDelta:longint read FMemoryDelta write FMemoryDelta;
  end;


implementation

procedure TMemoryMappedFileStream.SetAttributes(Mode:word);
begin
  if  (Mode and fmOpenReadWrite) = fmOpenReadWrite then
    begin
      FAccess :=GENERIC_WRITE OR GENERIC_READ;
      FPageProtect:=PAGE_READWRITE OR SEC_COMMIT;
      FMemProtect:=FILE_MAP_ALL_ACCESS;
    end
  else if (Mode and fmOpenWrite) = fmOpenWrite then
    begin
      FAccess :=GENERIC_WRITE OR GENERIC_READ;
      FPageProtect:=PAGE_READWRITE OR SEC_COMMIT;
      FMemProtect:=FILE_MAP_WRITE;
    end
  else  //all others including fmOpenRead=0
    begin
      FAccess :=GENERIC_READ;
      FPageProtect:=PAGE_READONLY OR SEC_COMMIT;
      FMemProtect:=FILE_MAP_READ;
    end;

  FShare:=0;

  if (Mode and fmShareDenyWrite) = fmShareDenyWrite then
    FShare:=FILE_SHARE_READ;

  if (Mode and fmShareDenyRead) = fmShareDenyRead then
    FShare:=FILE_SHARE_WRITE;

  if (Mode and fmShareExclusive) = fmShareExclusive then
    FShare:=0;
end;

constructor TMemoryMappedFileStream.Create(const FileName, MapName: string; Mode: Word);
var
  err : integer;
begin
  FMemoryDelta := MEMORY_DELTA;     //san
  FFileName:=FileName;
  FMapName:=MapName;
  SetAttributes(Mode);
  if Mode = fmCreate then
    //Create new file
    begin
      FFileHandle := CreateFile(PChar(FFileName),FAccess,FShare,nil,CREATE_ALWAYS,0,0);
      if (FFileHandle = 0) then
        raise EFCreateError.CreateFmt('Error creating %s'#13#10'Code %d', [FFileName,GetLastError])
      else
        begin
          FSize:=FMemoryDelta;
          FCapacity:=FMemoryDelta;
        end;
    end
  else
    //Open existing file
    begin
      FFileHandle := CreateFile(PChar(FFileName),FAccess,FShare,nil,OPEN_EXISTING,0,0);
      FSize := GetFileSize(FFileHandle,nil);
      FCapacity := FSize;
      if (FFileHandle = 0) or (FSize=longint($FFFFFFFF)) then
        raise EFOpenError.CreateFmt('Error opening %s'#13#10'Code %d', [FFileName,GetLastError]);
    end;
    
  FMapHandle := CreateFileMapping(FFileHandle, nil, FPageProtect, 0, FCapacity, PChar(FMapName));
  err := GetLastError;
  if (FMapHandle = 0) or (err <> ERROR_SUCCESS) then
    raise EFOpenError.CreateFmt('Error creating file mapping object %s'#13#10'Code %d', [FMapName,err]);
  FMemory := MapViewOfFile(FmapHandle, FILE_MAP_WRITE, 0, 0, 0);
  err := GetLastError;
  if (err <> ERROR_SUCCESS) then
    raise EFOpenError.CreateFmt('Error mapping file %s'#13#10'Code %d', [FFileName,err]);
  FPosition := 0;
end;

constructor TMemoryMappedFileStream.Open(const MapName:string; Mode:word);
var
  err : integer;
begin
  FMemoryDelta := MEMORY_DELTA;
  FFileName := '';
  FMapName := MapName;
  SetAttributes(Mode);
  if Mode = fmCreate then
    raise Exception.Create('Use "Create" constructor with fmCreate mode');
  FSize := FMemoryDelta;
  FCapacity := FMemoryDelta;
  FFileHandle := 0;

  FMapHandle := OpenFileMapping(FMemProtect, FALSE, PChar(FMapName));
  err := GetLastError;
  if (FMapHandle = 0) or (err <> ERROR_SUCCESS) then
    raise EFOpenError.CreateFmt('Error opening file mapping object %s'#13#10'Code %d', [FMapName,err]);

  FMemory := MapViewOfFile(FmapHandle, FILE_MAP_WRITE, 0, 0, 0);
  err := GetLastError;
  if err <> ERROR_SUCCESS then
    raise EFOpenError.CreateFmt('Error mapping file %s'#13#10'Code %d', [FMapName,err]);

  FPosition := 0;
end;

procedure TMemoryMappedFileStream.CloseHandles(Shrink : Boolean = False);
begin
  if FMemory <> nil then begin
   if not FlushViewOfFile(FMemory,0) then
     raise EStreamError.CreateFmt('Error flushing'#13#10'Code %d', [GetLastError]);

   if not UnmapViewOfFile(FMemory) then
     raise EStreamError.CreateFmt('Error unmapping view'#13#10'Code %d', [GetLastError]);
  end;

  if (FMapHandle <> 0) and not CloseHandle(FMapHandle) then
    raise EStreamError.CreateFmt('Error closing map %s'#13#10'Code %d', [FMapName,GetLastError]);

  if Shrink then begin
    SetFilePointer(FFileHandle, FSize, nil, FILE_BEGIN);
    SetEndOfFile(FFileHandle);
  end;

  if (FFileHandle <> 0) and not CloseHandle(FFileHandle) then

    raise EStreamError.CreateFmt('Error closing file %s'#13#10'Code %d', [FFileName,GetLastError]);
end;

destructor TMemoryMappedFileStream.Destroy;
begin
  CloseHandles(True);
end;

function TMemoryMappedFileStream.Read(var Buffer; Count:longint) : longint;
begin
  Result := 0;
  if (FPosition >= 0) and (Count >= 0) then begin
    Result := FSize - FPosition;
    if Result > 0 then
      begin
        if Result > Count then
          Result := Count;
        Move(Pointer(Longint(FMemory) + FPosition)^, Buffer, Result);
        Inc(FPosition, Result);
      end
    else
      Result := 0;
  end;
end;

function TMemoryMappedFileStream.Write(const Buffer; Count : longint) : longint;
var
  Pos: Longint;
begin
  Result := 0;
  if (FPosition >= 0) and (Count >= 0) then begin
    Pos := FPosition + Count;
    if Pos > 0 then begin
      if Pos > FSize then begin
        if Pos > FCapacity then
          SetCapacity(Pos);
        FSize := Pos;
      end;
      System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
      FPosition := Pos;
      Result := Count;
    end;
  end;
end;


procedure TMemoryMappedFileStream.SetSize(NewSize : longint);
begin
  if NewSize < FSize then
    SetCapacity(NewSize)
  else if NewSize >= FSize then
    begin
      if NewSize <= FCapacity then
        FSize:=NewSize
      else
        begin
          SetCapacity(NewSize);
          if NewSize <= FCapacity then
            FSize:=NewSize;
        end;
    end;
end;

procedure TMemoryMappedFileStream.SetCapacity(NewCapacity : longint);
var
  err : integer;
begin
  if NewCapacity = 0 then
    begin
      Flush;
      FSize:=0;
      FCapacity:=0;
    end
  else
    begin
      CloseHandles;
      //Close MMF and try to recreate it with the new size
      //Is there a better way to change the size of an MMF?

      //FFileHandle=0 for the open (not created!)named mapping object
      if FFileHandle <> 0 then
        begin
          FFileHandle := CreateFile(PChar(FFileName), FAccess, FShare, nil, OPEN_EXISTING, 0, 0);
          err := GetLastError;
          if (FFileHandle = 0) or (FSize=longint($FFFFFFFF)) or (err <> ERROR_SUCCESS) then
            raise EFOpenError.CreateFmt('Error opening %s'#13#10'Code %d', [FileName,err]);
        end
      else
        raise EFOpenError.CreateFmt('Cannot change size of an open mapping object %s', [FMapName]);

      FCapacity := NewCapacity + FMemoryDelta;
      FMapHandle := CreateFileMapping(FFileHandle, nil, FPageProtect, 0, FCapacity, PChar(FMapName));
      err := GetLastError;
      if (FMapHandle = 0) or (err <> ERROR_SUCCESS) then
        raise EFOpenError.CreateFmt('Error creating file mapping object %s'#13#10'Code %d', [FMapName,err]);

      FMemory := MapViewOfFile(FMapHandle, FILE_MAP_WRITE, 0, 0, 0);
      err := GetLastError;
      if err <> ERROR_SUCCESS then
        raise EFOpenError.CreateFmt('Error mapping file %s'#13#10'Code %d', [FMapName,err]);
    //san  FPosition:=0;
    end;
end;

function TMemoryMappedFileStream.Seek(Offset : longint; Origin : word) : longint;
begin
  case Origin of
    soFromBeginning :
      FPosition := Offset;

    soFromCurrent :
      Inc(FPosition, Offset);

    soFromEnd :
      FPosition := FSize + Offset;
  end;
  Result:=FPosition;
end;

procedure TMemoryMappedFileStream.Flush;
begin
  FlushViewOfFile(FMemory, 0);
end;

end.
