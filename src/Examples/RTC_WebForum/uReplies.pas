unit uReplies;

{$include rtcDefs.inc}

interface

uses
  uMMF, Classes, uCustomTable, uTypes, rtcTypes;

type
  TRepliesTable = class(TCustomMemoryMappedTable)
  private
    function GetItem(Index: integer): PReplyRecord;
    procedure SetItem(Index: integer; const Value: PReplyRecord);
    function GetItemByID(ID: integer): PReplyRecord;
    procedure SetItemByID(ID: integer; const Value: PReplyRecord);
    function GetMessages(Reply: PReplyRecord): RtcString;
  protected
    function IsDeleted(P : Pointer) : Boolean; override;
    function CalcOffset(Index : integer) : Longint; override;

    property Filename;
    property Header;
    property Count;
    property CountDeleted;
    property Items [Index : integer] : PReplyRecord read GetItem write SetItem;

    function DoDelete(Index : integer) : boolean; override;
    function DeleteByID(ID : integer) : boolean;
    function IndexOf(ID : integer) : integer;  // get Index of record by ID

  public
    constructor Create(Filename : string); virtual;

    function New(const Msg : RtcString) : PReplyRecord;
    function Delete(P : Pointer) : boolean; override;

    property ItemsByID [ID : integer] : PReplyRecord read GetItemByID write SetItemByID; default;
    property Messages[Reply : PReplyRecord] : RtcString read GetMessages;
  end;

implementation

uses SysUtils, Windows, Math;

function CompareTimeStamp(Item1, Item2: Pointer): Integer;
begin
  if PReplyRecord(Item1).TimeStamp=PReplyRecord(Item2).TimeStamp then
    Result:=0
  else if PReplyRecord(Item1).TimeStamp<PReplyRecord(Item2).TimeStamp then
    Result:=-1
  else
    Result:=1;
  // Result := CompareDateTime(PReplyRecord(Item1).TimeStamp, PReplyRecord(Item2).TimeStamp);
end;

{ TRepliesTable }

constructor TRepliesTable.Create(Filename : string);
begin
  inherited Create(Filename, SizeOf(TReplyRecord));
  CompareMethod := CompareTimeStamp;
end;

function TRepliesTable.DoDelete(Index: integer): boolean;
begin
  Result := False;
  if Index < 0 then
    Exit;
  Items[Index].Deleted := True;
  Inc(Header.CountDeleted);
  Result := True;
end;

function TRepliesTable.DeleteByID(ID: integer): boolean;
begin
  Result := DoDelete(IndexOf(ID));
end;

function TRepliesTable.GetItem(Index: integer): PReplyRecord;
begin
  Result := nil;
  if Index < 0 then
    Exit;
  Result := PReplyRecord(GetItemByIndex(Index));
end;

function TRepliesTable.GetItemByID(ID: integer): PReplyRecord;
begin
  Result := GetItem(IndexOf(ID));
end;

function TRepliesTable.IndexOf(ID: integer): integer;
var
  I : integer;
  P : PReplyRecord;
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

function TRepliesTable.New(const Msg : RtcString): PReplyRecord;
var
  MsgSize : integer;
begin
  MsgSize := Length(Msg);
  Result := PReplyRecord(NewItem(MsgSize));
  // initialization of item
  with Result^ do begin
    ID := Header.LastID;
    ID_Topic := 0;
    User := '';
    UserType := utNone;
    TimeStamp := Now;
    UserIP := '';
    TextLength := MsgSize;
    Deleted := False;
    Move(Msg[1], Pointer(Longint(Result) + DataBlockLength)^, MsgSize);
  end;
end;

procedure TRepliesTable.SetItem(Index: integer; const Value: PReplyRecord);
begin
  if Index < 0 then
    Exit;

  with GetItem(Index)^ do begin
    User := Value^.User;
    UserType := Value^.UserType;
    TimeStamp := Value^.TimeStamp;
    UserIP := Value^.UserIP;
  end;
end;

procedure TRepliesTable.SetItemByID(ID: integer;
  const Value: PReplyRecord);
begin
  SetItem(IndexOf(ID), Value);
end;

function TRepliesTable.Delete(P: Pointer): boolean;
begin
  Result := False;
  if not Assigned(P) then
    Exit;
  PReplyRecord(P)^.Deleted := True;
  Result := inherited Delete(P);
end;

function TRepliesTable.CalcOffset(Index: integer): Longint;
var
  I : integer;
  P : PReplyRecord;
begin
  P := PReplyRecord(Longint(Map.Memory) + LENGTH_HEADER_RECORD);
  for I := 1 to Index do
    P := Pointer(Longint(P) + DataBlockLength + PReplyRecord(P)^.TextLength);

  Result := Longint(P) - Longint(Map.Memory);
end;

function TRepliesTable.GetMessages(Reply: PReplyRecord): RtcString;
begin
  SetLength(Result, Reply^.TextLength);
  Move(Pointer(Longint(Reply) + DataBlockLength)^, Result[1], Reply^.TextLength);
end;

function TRepliesTable.IsDeleted(P: Pointer): Boolean;
begin
  Result := PReplyRecord(P)^.Deleted;
end;

end.
