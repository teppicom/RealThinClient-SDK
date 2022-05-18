unit uSections;

{$include rtcDefs.inc}

interface

uses
  uMMF, Classes, uCustomTable, uTypes, rtcTypes;

type
  TSectionsTable = class(TCustomMemoryMappedTable)
  private
    function GetItem(Index: integer): PSectionRecord;
    procedure SetItem(Index: integer; const Value: PSectionRecord);
    function GetItemByID(ID: integer): PSectionRecord;
    procedure SetItemByID(ID: integer; const Value: PSectionRecord);

  protected
    function IsDeleted(P : Pointer) : Boolean; override;
    function Delete(P : Pointer) : boolean; override;

    function DoDelete(Index : integer) : boolean; override;
    function IndexOf(ID : integer) : integer;  // get Index of record by ID


  public
    constructor Create(Filename : RtcString); virtual;

    function New : PSectionRecord;

    function DeleteByID(ID : integer) : boolean;

    property Items [Index : integer] : PSectionRecord read GetItem write SetItem;
    property ItemsByID [ID : integer] : PSectionRecord read GetItemByID write SetItemByID; default;
  end;

implementation

uses SysUtils, Windows;

function CompareOrder(Item1, Item2: Pointer): Integer;
begin
  // Result := CompareValue(PSectionRecord(Item1).SortOrder, PSectionRecord(Item2).SortOrder);

  if PSectionRecord(Item1).SortOrder=PSectionRecord(Item2).SortOrder then
    Result:=0
  else if PSectionRecord(Item1).SortOrder<PSectionRecord(Item2).SortOrder then
    Result:=-1
  else
    Result:=1;
end;


{ TSectionsTable }

constructor TSectionsTable.Create(Filename : RtcString);
begin
  inherited Create(Filename, SizeOf(TSectionRecord));
  CompareMethod := CompareOrder;
end;

function TSectionsTable.DoDelete(Index: integer): boolean;
begin
  Result := False;
  if Index < 0 then
    Exit;
  Items[Index].Deleted := True;
  Inc(Header.CountDeleted);
  Result := True;
end;

function TSectionsTable.DeleteByID(ID: integer): boolean;
begin
  Result := DoDelete(IndexOf(ID));
end;

function TSectionsTable.GetItem(Index: integer): PSectionRecord;
begin
  Result := nil;
  if Index < 0 then
    Exit;
  Result := PSectionRecord(GetItemByIndex(Index));
end;

function TSectionsTable.GetItemByID(ID: integer): PSectionRecord;
begin
  Result := GetItem(IndexOf(ID));
end;

function TSectionsTable.IndexOf(ID: integer): integer;
var
  I : integer;
  P : PSectionRecord;
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

function TSectionsTable.New: PSectionRecord;
begin
  Result := PSectionRecord(NewItem);
  // initialization of item
  with Result^ do begin
    ID := Header.LastID;
    VisibilityLevel := vlPublic;
    AccessLevel := alOpen;
    PostCount := 0;
    TopicsCount := 0;
    LastPostTimeStamp := 0.0;
    LastPostTopicID := 0;
    LastPostTopicName := '';
    LastPostUserName := '';
    LastPostUserType := utNone;
    CreatedTimeStamp := Now;
    SortOrder := 0;
    Deleted := False;
  end;
end;

procedure TSectionsTable.SetItem(Index: integer; const Value: PSectionRecord);
begin
  if Index < 0 then
    Exit;
  with GetItem(Index)^ do begin
    ID := Value^.ID;
    Name := Value^.Name;
    VisibilityLevel := Value^.VisibilityLevel;
    AccessLevel := Value^.AccessLevel;
    PostCount := Value^.PostCount;
    TopicsCount := Value^.TopicsCount;
    LastPostTimeStamp := Value^.LastPostTimeStamp;
    LastPostTopicID := Value^.LastPostTopicID;
    LastPostUserName := Value^.LastPostUserName;
    LastPostUserType := Value^.LastPostUserType;
    SortOrder := Value^.SortOrder;
    Deleted := Value^.Deleted;
  end;
end;

procedure TSectionsTable.SetItemByID(ID: integer;
  const Value: PSectionRecord);
begin
  SetItem(IndexOf(ID), Value);
end;


function TSectionsTable.Delete(P: Pointer): boolean;
begin
  Result := False;
  if not Assigned(P) then
    Exit;
  PSectionRecord(P)^.Deleted := True;
  Result := inherited Delete(P);
end;

function TSectionsTable.IsDeleted(P: Pointer): Boolean;
begin
  Result := PSectionRecord(P)^.Deleted;
end;

end.
