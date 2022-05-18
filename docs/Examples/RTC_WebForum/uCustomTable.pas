unit uCustomTable;

interface

{$include rtcDefs.inc}

uses
  {$IFDEF IDE_1}
  FileCtrl,
  {$ENDIF}
  uMMF, Classes, uTypes, rtcTypes;

type
  PListHeaderRecord = ^TListHeaderRecord;
  TListHeaderRecord = packed record     // --- size ---
    LastID : integer;                   //      4
    Count : integer;                    //      4
    CountDeleted : integer              //      4
  end;                                  //  =  12 = total

const
  LENGTH_HEADER_RECORD = SizeOf(TListHeaderRecord);

type
  TCustomMemoryMappedTable = class
  private
    fFilename : RtcString;
    fMap : TMemoryMappedFileStream;
    fDataBlockLength : integer;
    fCompareMethod : TListSortCompare;
    // Header
    function GetHeader: PListHeaderRecord;
    procedure SetHeader(const Value: PListHeaderRecord);
    function GetCount: integer;
    function GetCountDeleted: integer;

  protected
    // Items
    function CalcOffset(Index : integer) : Longint; dynamic;
    function GetItemByIndex(Index: integer): Pointer; dynamic;
    function DoDelete(Index : integer) : boolean; virtual; abstract;
    function NewItem (const AdditionalBlockSize : integer = 0) : Pointer;
    // Misc
    procedure InitFile; dynamic;
    function IsDeleted(P : Pointer) : Boolean; virtual; abstract;

    property Map : TMemoryMappedFileStream read fMap;
    property DataBlockLength : integer read fDataBlockLength;
    property CompareMethod : TListSortCompare read fCompareMethod write fCompareMethod;
    function Delete(P : Pointer) : boolean; overload; virtual;

    function Delete(Index : integer) : boolean; overload;

    property CountDeleted : integer read GetCountDeleted;
    property Filename : RtcString read fFilename;
    property Header : PListHeaderRecord read GetHeader write SetHeader;

    constructor Create(Filename : String; DataBlockLength : integer);

  public
    destructor Destroy; override;

    procedure Flush;
    function GetOrderedList(SkipDeleted : Boolean = True): TList; dynamic;

    property Count : integer read GetCount;    // total count of record (with deleted records)
  end;


implementation

uses SysUtils, Windows;

{ TCustomMemoryMappedTable }

function TCustomMemoryMappedTable.CalcOffset(Index: integer): Longint;
begin
  Result := LENGTH_HEADER_RECORD + Index * fDataBlockLength;
end;

constructor TCustomMemoryMappedTable.Create(Filename : String; DataBlockLength : integer);
begin
  fDataBlockLength := DataBlockLength;

  fFilename := Filename;
  ForceDirectories(ExtractFilePath(fFilename));

  if not FileExists(fFilename) then
    InitFile;

  fMap := TMemoryMappedFileStream.Create(
    fFilename,
    Format('mmf.%.8x.%.8x', [Integer(Self), GetTickCount]),
    fmOpenWrite or fmShareDenyWrite);
end;

function TCustomMemoryMappedTable.Delete(Index: integer): boolean;
begin
  if (Index >= 0) and (Index < Count) then
    begin

      if DoDelete(Index) then
        begin
          Inc(Header.CountDeleted);
          Result := True;
        end
      else
        Result := False;
    end
  else
    Result := False;
end;

function TCustomMemoryMappedTable.Delete(P: Pointer): boolean;
begin
 Inc(Header.CountDeleted);
 Result := True;
end;

destructor TCustomMemoryMappedTable.Destroy;
begin
  if fMap <> nil then begin
    fMap.Flush;
    fMap.Free;
    fMap:=nil;
  end;

  inherited;
end;

procedure TCustomMemoryMappedTable.Flush;
  begin
  if fMap<>nil then fMap.Flush;
  end;

function TCustomMemoryMappedTable.GetCount: integer;
begin
  Result := Header.Count;
end;

function TCustomMemoryMappedTable.GetCountDeleted: integer;
begin
  Result := Header.CountDeleted;
end;

function TCustomMemoryMappedTable.GetHeader: PListHeaderRecord;
begin
  Result := PListHeaderRecord(fMap.Memory);
end;

function TCustomMemoryMappedTable.GetItemByIndex(Index: integer): Pointer;
begin
  Result := nil;
  if Index < 0 then
    Exit;
  Result := Pointer(Longint(fMap.Memory) + CalcOffset(Index));
end;

function TCustomMemoryMappedTable.GetOrderedList
  (SkipDeleted : Boolean = True) : TList;
var
  fList : TList;
  I : integer;
  P : pointer;
begin
  fList := TList.Create;
  try
    if SkipDeleted then
      fList.Capacity := Count - CountDeleted
    else
      fList.Capacity := Count;

    for I := 0 to Count-1 do begin
      P := GetItemByIndex(I);
      if not SkipDeleted or (SkipDeleted and not IsDeleted(P))  then
        fList.Add(P);
    end;
    if Assigned(fCompareMethod) then
      fList.Sort(fCompareMethod);
  except
    fList.Free;
    fList:=nil;
  end;
  Result := fList;
end;

procedure TCustomMemoryMappedTable.InitFile;
var
  H : TListHeaderRecord;
begin
  with TMemoryStream.Create do try
    H.LastID := 0;
    H.Count := 0;
    H.CountDeleted := 0;
    WriteBuffer(H, LENGTH_HEADER_RECORD);
    SaveToFile(fFilename);
  finally
    Free;
  end;
end;

function TCustomMemoryMappedTable.NewItem(
  const AdditionalBlockSize : integer = 0): Pointer;
var
  Rec : Pointer;
  BlockSize : integer;
begin
  Inc(Header.LastID);
  fMap.Position := CalcOffset(Header.Count);

  BlockSize := fDataBlockLength + AdditionalBlockSize;

  GetMem(Rec, BlockSize);
  try
    FillMemory(Rec, BlockSize, 0);
    fMap.WriteBuffer(Rec^, BlockSize);
  finally
    FreeMem(Rec);
  end;

  Result := GetItemByIndex(Header.Count);
  Inc(Header.Count);
end;

procedure TCustomMemoryMappedTable.SetHeader(const Value: PListHeaderRecord);
begin
  with PListHeaderRecord(fMap.Memory)^ do begin
    LastID := Value^.LastID;
    Count := Value^.Count;
    CountDeleted := Value^.CountDeleted;
  end;
end;

end.
