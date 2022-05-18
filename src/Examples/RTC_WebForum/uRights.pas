unit uRights;

{$include rtcDefs.inc}

interface

uses
  uMMF, Classes, uCustomTable, uTypes;

type
  TRightsTable = class(TCustomMemoryMappedTable)
  private
    // Items
    function GetItem(Index: integer): PRightsRecord;
    procedure SetItem(Index: integer; const Value: PRightsRecord);
    function GetItemByID(ID: integer): PRightsRecord;
    procedure SetItemByID(ID: integer; const Value: PRightsRecord);

  protected
    function IsDeleted(P : Pointer) : Boolean; override;
    function DoDelete(Index : integer) : boolean; override;

    property Filename;
    property Header;
    property Count;
    property CountDeleted;
    property Items [Index : integer] : PRightsRecord read GetItem write SetItem;
    property ItemsByID [ID : integer] : PRightsRecord read GetItemByID write SetItemByID; default;

    function DeleteByID(ID : integer) : boolean;
    function Delete(P : Pointer) : boolean; override;
    function IndexOf(ID : integer) : integer;  // get Index of record by ID

  public
    constructor Create(Filename : string); virtual;

    function New : PRightsRecord;

    function GetUserAccessLevel(User:TUserLogin; ID_section : integer) : TUserAccessLevel;
    procedure SetUserAccessLevel(User:TUserLogin; ID_section : integer; Level : TUserAccessLevel);
  end;

implementation

uses SysUtils, Windows;

{ TRightsTable }

constructor TRightsTable.Create(Filename : string);
begin
  inherited Create(Filename, SizeOf(TRightsRecord));
end;

function TRightsTable.DoDelete(Index: integer): boolean;
begin
  Result := False;
  if Index < 0 then
    Exit;
  Items[Index].Deleted := True;
  Inc(Header.CountDeleted);
  Result := True;
end;

function TRightsTable.DeleteByID(ID: integer): boolean;
begin
  Result := DoDelete(IndexOf(ID));
end;

function TRightsTable.GetItem(Index: integer): PRightsRecord;
begin
  Result := nil;
  if Index < 0 then
    Exit;
  Result := PRightsRecord(GetItemByIndex(Index));
end;

function TRightsTable.GetItemByID(ID: integer): PRightsRecord;
begin
  Result := GetItem(IndexOf(ID));
end;

function TRightsTable.IndexOf(ID: integer): integer;
var
  I : integer;
  P : PRightsRecord;
begin
  Result := -1;
  for I := 0 to Header.Count - 1 do begin
    P := Items[I];
    if not P^.Deleted and (P^.ID = ID) then begin
      Result := I;
      Break;
    end;
  end;
end;

function TRightsTable.New: PRightsRecord;
begin
  Result := PRightsRecord(NewItem);
  // initialization of item
  with Result^ do begin
    ID := Header.LastID;
    User := '';
    ID_section := -1;
    UserAccessLevel := uaNone;
    Deleted := False;
  end;
end;

procedure TRightsTable.SetItem(Index: integer; const Value: PRightsRecord);
begin
  if Index < 0 then
    Exit;
  with GetItem(Index)^ do begin
    ID := Value^.ID;
    User := Value^.User;
    ID_section := Value^.ID_section;
    UserAccessLevel := Value^.UserAccessLevel;
    Deleted := Value^.Deleted;
  end;
end;

procedure TRightsTable.SetItemByID(ID: integer;
  const Value: PRightsRecord);
begin
  SetItem(IndexOf(ID), Value);
end;


function TRightsTable.GetUserAccessLevel(User:TUserLogin; ID_section: integer): TUserAccessLevel;
var
  I : integer;
  P : PRightsRecord;
begin
  Result := uaNone;
  for I := 0 to Header.Count - 1 do
    begin
    P := Items[I];
    if not P^.Deleted and (P^.ID_section = ID_section) then
      if (P^.User = User)then
        begin
        Result := P^.UserAccessLevel;
        Break;
        end;
    end;
end;

procedure TRightsTable.SetUserAccessLevel(User:TUserLogin; ID_section: integer; Level: TUserAccessLevel);
var
  I : integer;
  P : PRightsRecord;
  Found : boolean;
begin
  Found := False;
  P:=nil;

  for I := 0 to Header.Count - 1 do
    begin
    P := Items[I];
    if (P^.ID_section = ID_section) then
      if (P^.User = User) then
        begin
        Found := True;
        Break;
        end;
    end;

  if not Found then begin
    P := New;
    P^.User := User;
    P^.ID_section := ID_section;
  end;

  P^.UserAccessLevel := Level;
  P^.Deleted := False;
end;

function TRightsTable.Delete(P: Pointer): boolean;
begin
  Result := False;
  if not Assigned(P) then
    Exit;
  PRightsRecord(P)^.Deleted := True;
  Result := inherited Delete(P);
end;

function TRightsTable.IsDeleted(P: Pointer): Boolean;
begin
  Result := PRightsRecord(P)^.Deleted;
end;

end.
