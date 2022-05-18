unit uTypes;

interface

uses
  SysUtils;

type
  TUserName = string[50];
  TUserLogin = string[50];
  TIPAddress = string[15];
  TUserType = (utNone, utGuest, utRegistered);
  TSectionName = string[200];
  TTopicName = string[200];

  TVisibilityLevel = (vlPublic, vlPrivate);
  TAccessLevel = (alOpen, alClosed);

  TUserAccessLevel = (uaNone, uaRead, uaWrite, uaModerate);
  TFilterUserAccessRights = (frNone, frRead, frWrite, frModerate, frPower, frAll);

  PRightsRecord = ^TRightsRecord;
  TRightsRecord = packed record
    ID : integer;
    User : TUserLogin;
    ID_section : integer;
    UserAccessLevel : TUserAccessLevel;
    Deleted : boolean;
  end;

  PSectionRecord=^TSectionRecord;
  TSectionRecord = packed record
    ID : integer;
    Name : TSectionName;
    VisibilityLevel : TVisibilityLevel;
    AccessLevel : TAccessLevel;
    PostCount : integer;
    TopicsCount : integer;
    LastPostTimeStamp : TDateTime;
    LastPostTopicID : integer;
    LastPostTopicName : TTopicName;
    LastPostUserName : TUserName;
    LastPostUserType : TUserType;
    CreatedTimeStamp : TDateTime;
    SortOrder : cardinal;
    Deleted : boolean;
  end;

  PTopicRecord = ^TTopicRecord;
  TTopicRecord = packed record
    ID : integer;
    Name : TTopicName;
    RepliesCount : integer;
    LastPostTimeStamp : TDateTime;
    LastPostUser : TUserName;
    LastPostUserType : TUserType;
    CreatorUser : TUserName;
    CreatorUserType : TUserType;
    CreatedTimeStamp : TDateTime;
    First_Reply_ID : integer;
    Deleted : boolean;
  end;

  PReplyRecord = ^TReplyRecord;
  TReplyRecord = packed record
    ID : integer;
    ID_Topic : integer;
    User : TUserName;
    UserType : TUserType;
    TimeStamp : TDateTime;
    UserIP : TIPAddress;
    TextLength : integer;
    Deleted : boolean;
  end;

function VisibilityToStr(V: TVisibilityLevel): string;
function StrToVisibility(S : string) : TVisibilityLevel;
function AccessToStr(V: TAccessLevel): string;
function StrToAccess(S: string) : TAccessLevel;

function UserAccessToStr(V: TUserAccessLevel): string;
function StrToUserAccess(S: string) : TUserAccessLevel;

implementation

const
  cVisibilityLevelStrs: array [TVisibilityLevel] of String = ('Public', 'Private');
  cAccessLevelStrs: array [TAccessLevel] of String = ('Open', 'Closed');

function VisibilityToStr(V: TVisibilityLevel): string;
begin
  Result := cVisibilityLevelStrs[V];
end;

function StrToVisibility(S : string) : TVisibilityLevel;
begin
  if CompareText(cVisibilityLevelStrs[vlPublic], S)=0 then
    Result := vlPublic
  else
    Result := vlPrivate;
end;

function AccessToStr(V: TAccessLevel): string;
begin
  Result := cAccessLevelStrs[V];
end;

function StrToAccess(S: string) : TAccessLevel;
begin
  if CompareText(cAccessLevelStrs[alOpen], S)=0 then
    Result := alOpen
  else
    Result := alClosed;
end;

const
  cUserAccessLevelStrs: array [TUserAccessLevel] of String = ('none', 'read', 'write', 'moderate');

function UserAccessToStr(V: TUserAccessLevel): string;
begin
  Result := cUserAccessLevelStrs[V];
end;

function StrToUserAccess(S: string) : TUserAccessLevel;
begin
  if CompareText(cUserAccessLevelStrs[uaModerate], S)=0 then
    Result := uaModerate
  else if CompareText(cUserAccessLevelStrs[uaWrite], S)=0 then
    Result := uaWrite
  else if CompareText(cUserAccessLevelStrs[uaRead], S)=0 then
    Result := uaRead
  else
    Result := uaNone;
end;

end.
