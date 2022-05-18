unit uTopics;

{$include rtcDefs.inc}

interface

uses
  uMMF, Classes, uCustomTable, uTypes;

type
  TTopicsTable = class(TCustomMemoryMappedTable)
  private
    function GetItem(Index: integer): PTopicRecord;
    procedure SetItem(Index: integer; const Value: PTopicRecord);
    function GetItemByID(ID: integer): PTopicRecord;
    procedure SetItemByID(ID: integer; const Value: PTopicRecord);
  protected
    function IsDeleted(P : Pointer) : Boolean; override;

    property Filename;
    property Header;
    property Count;
    property CountDeleted;
    property Items [Index : integer] : PTopicRecord read GetItem write SetItem;

    function DoDelete(Index : integer) : boolean; override;
    function DeleteByID(ID : integer) : boolean;
    function IndexOf(ID : integer) : integer;  // get Index of record by ID

  public
    constructor Create(Filename : string); virtual;

    function New : PTopicRecord;
    function Delete(P : Pointer) : boolean; override;

    property ItemsByID [ID : integer] : PTopicRecord read GetItemByID write SetItemByID; default;
  end;

implementation

uses SysUtils, Windows;

function CompareTimeStampDesc(Item1, Item2: Pointer): Integer;
begin
  if PTopicRecord(Item2).LastPostTimeStamp=PTopicRecord(Item1).LastPostTimeStamp then
    Result:=0
  else if PTopicRecord(Item2).LastPostTimeStamp<PTopicRecord(Item1).LastPostTimeStamp then
    Result:=-1
  else
    Result:=1;
  // Result := CompareDateTime(PTopicRecord(Item2).LastPostTimeStamp,PTopicRecord(Item1).LastPostTimeStamp);
end;

{ TTopicsTable }

constructor TTopicsTable.Create(Filename : string);
begin
  inherited Create(Filename, SizeOf(TTopicRecord));
  CompareMethod := CompareTimeStampDesc;
end;

function TTopicsTable.DoDelete(Index: integer): boolean;
begin
  Result := False;
  if Index < 0 then
    Exit;
  Items[Index].Deleted := True;
  Inc(Header.CountDeleted);
  Result := True;
end;

function TTopicsTable.DeleteByID(ID: integer): boolean;
begin
  Result := DoDelete(IndexOf(ID));
end;

function TTopicsTable.GetItem(Index: integer): PTopicRecord;
begin
  Result := nil;
  if Index < 0 then
    Exit;
  Result := PTopicRecord(GetItemByIndex(Index));
end;

function TTopicsTable.GetItemByID(ID: integer): PTopicRecord;
begin
  Result := GetItem(IndexOf(ID));
end;

function TTopicsTable.IndexOf(ID: integer): integer;
var
  I : integer;
  P : PTopicRecord;
begin
  Result := -1;
  // TODO: need to redesign with hash !
  for I := 0 to Header.Count - 1 do begin
    P := Items[I];
    if not P^.Deleted and (P^.ID = ID) then begin
      Result := I;
      Break;
    end;
  end;
end;

function TTopicsTable.New: PTopicRecord;
begin
  Result := PTopicRecord(NewItem);
  // initialization of item
  with Result^ do begin
    ID := Header.LastID;
    Name := '';
    RepliesCount := 0;
    LastPostTimeStamp := 0.0;
    LastPostUser := '';
    LastPostUserType := utNone;
    CreatorUser := '';
    CreatorUserType := utNone;
    CreatedTimeStamp := Now;
    First_Reply_ID := 0;
    Deleted := False;
  end;
end;

procedure TTopicsTable.SetItem(Index: integer; const Value: PTopicRecord);
begin
  if Index < 0 then
    Exit;
  with GetItem(Index)^ do begin
    ID := Value^.ID;
    Name := Value^.Name;
    RepliesCount := Value^.RepliesCount;
    LastPostTimeStamp := Value^.LastPostTimeStamp;
    LastPostUser := Value^.LastPostUser;
    LastPostUserType := Value^.LastPostUserType;
    CreatorUser := Value^.CreatorUser;
    CreatorUserType := Value^.CreatorUserType;
    First_Reply_ID := Value^.First_Reply_ID;
    Deleted := Value^.Deleted;
  end;
end;

procedure TTopicsTable.SetItemByID(ID: integer;
  const Value: PTopicRecord);
begin
  SetItem(IndexOf(ID), Value);
end;

function TTopicsTable.Delete(P: Pointer): boolean;
begin
  Result := False;
  if not Assigned(P) then
    Exit;
  PTopicRecord(P)^.Deleted := True;
  Result := inherited Delete(P);
end;

function TTopicsTable.IsDeleted(P: Pointer): Boolean;
begin
  Result := PTopicRecord(P)^.Deleted;
end;

end.
