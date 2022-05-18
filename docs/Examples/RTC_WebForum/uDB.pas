unit uDB;

{$INCLUDE defines.inc}
{$include rtcDefs.inc}

interface

uses
  Classes,
  IniFiles,
  Windows,
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcInfo;


const
  DEFAULT_ADMIN_USER:RtcString='admin';
  DEFAULT_ADMIN_PWD:RtcString='admin';
  USER_DATA_FILENAME:RtcString='users.data';

type
  TCallBackProc = procedure (Login : RtcString);

procedure InitUserData(folder:RtcString);

// Users support
function GetUserInfo(login : RtcString; var name, pwd : RtcString) : boolean; overload;
function GetUserInfo(login : RtcString; var name : RtcString) : boolean; overload;
function GetUserName(login : RtcString) : RtcString;
function SaveUserInfo(login, name, pwd : RtcString) : boolean;
function CheckUser(login, pwd : RtcString) : boolean;
procedure GetUsers(Users : TStrings);
function DeleteUser(login : RtcString) : boolean;
function ChangePassword(login, old_pwd, pwd : RtcString) : boolean;
function IsUserExists(login : RtcString) : boolean;
function SetLastLogon(login: RtcString; Timestamp: TDateTime) : boolean;
function GetLastLogon(login: RtcString; var Timestamp: TDateTime) : boolean;

// Packages support
function AddPack(name, order_link, extend_lic_link : RtcString; var id : RtcString) : boolean;
function GetPackInfo(id: RtcString; var name, order_link, extend_lic_link : RtcString) : boolean;
function SavePackInfo(id, name, order_link, extend_lic_link : RtcString) : boolean;
procedure GetPacks(Packs : TStrings);
function DeletePack(id : RtcString) : boolean;
function IsPackExists(packname : RtcString) : boolean;
function SetPackVisibility(pack_id, vis_level : RtcString) : boolean;
function GetPackVisibility(pack_id : RtcString) : RtcString;

// Files support
function AddFile(pack_id, fname, description, size, datetime : RtcString; var id : RtcString) : boolean;
function GetFileInfo(pack_id, file_id : RtcString; var fname, description, filedatetime, size : RtcString) : boolean;
procedure GetFiles(pack_id : RtcString; Files : TStrings);
procedure GetFilesTiny(pack_id : RtcString; Files : TStrings);
procedure GetFilesAll(Files : TStrings);
function SaveFileDesc(file_id, description : RtcString) : boolean;
function BindFile(pack_id, file_id : RtcString) : boolean;
function DelFile(pack_id, file_id : RtcString) : boolean;
function IsFileInPackExists(pack_id, fname : RtcString) : boolean;
function IsFileExists(fname : RtcString; var packname : RtcString) : boolean;

// Access support
function GetGrant(login, pack_id : RtcString; var expiredate : RtcString) : boolean;
function SaveUserAccess(login, pack_id : RtcString; grant: boolean; expiredate : RtcString) : boolean;
function IsFileAvailableForUser(login, filename : RtcString) : boolean;

//Common functions
function GetNextID(entity : RtcString) : RtcString;   //entity: names, packages, files

const
  DEFAULT_PACK_VISIBILITY_LEVEL = 'private';

function _GetValueFromIndex(SL:TStringList; Index: Integer): RtcString;

implementation

var
  DB : TMemIniFile = nil;
  RW : TRtcRWSec = nil;

const
  //
  // Misc
  //
  T_IDS             = 'last used id';
  //
  // Users
  //
  T_USERS           = 'users';
  T_USERS_NAMES     = 'users::names';
  T_USERS_PASSWORDS = 'users::passwords';
  T_USERS_GRANTS    = 'users::grants::%s';
  T_USERS_LAST_LOGON= 'users::last logon';
  //
  // Packages
  //
  T_PACKS           = 'packages';
  T_PACKS_FILES     = 'files::package::%s';
  T_PACKS_LINKS     = 'links::package::%s';
  T_PACKS_VISIBILITY= 'packages::visibility';
  ORDER             = 'Order';
  EXTEND_LIC        = 'Extend license';
  //
  // Files
  //
  T_FILES           = 'files';

procedure InitUserData(folder:RtcString);
  var
    S : RtcString;
  begin
  if Assigned(DB) then
    begin
    DB.Free;
    DB:=nil;
    end;
  S := folder + USER_DATA_FILENAME;
  Write_File(s+'.bak',Read_File(s));
  DB := TMemIniFile.Create(S);
  end;

function GetNextID(entity : RtcString) : RtcString;   //entity: users, packages, files
var
  id : integer;
begin
  id := DB.ReadInteger(T_IDS, entity, 0) + 1;
  DB.WriteInteger(T_IDS, entity, id);
  Result := IntToStr(id);
end;

function GetUserInfo(login : RtcString; var name, pwd : RtcString) : boolean;
begin
  RW.EnterRead;
  try
    Result := DB.ValueExists(T_USERS, login);
    if Result then
      begin
        name := DB.ReadString(T_USERS_NAMES, login, '');
        pwd := DB.ReadString(T_USERS_PASSWORDS, login, '');
      end
    else
      begin
        name := '';
        pwd := '';
      end;
  finally
    RW.LeaveRead
  end;
end;

function GetUserInfo(login : RtcString; var name : RtcString) : boolean; overload;
var
  pwd : RtcString;
begin
  Result := GetUserInfo(login, name, pwd);
end;

function GetUserName(login : RtcString) : RtcString;
begin
  if not GetUserInfo(login, Result) then
    Result := login;
end;

function SaveUserInfo(login, name, pwd : RtcString) : boolean;
begin
  RW.EnterWrite;
  try
    if login <> '' then
      begin
        DB.WriteString(T_USERS, login, '');
        DB.WriteString(T_USERS_NAMES, login, URL_Decode(name));
        if pwd <> '' then
          DB.WriteString(T_USERS_PASSWORDS, login, pwd);
        DB.UpdateFile;
        Result := True;
      end
    else
      Result := False;
  finally
    RW.LeaveWrite;
  end;
end;

function CheckUser(login, pwd : RtcString) : boolean;
var
  S : RtcString;
begin
  RW.EnterRead;
  try
    if DB.ValueExists(T_USERS, login) then
      begin
        S := DB.ReadString(T_USERS_PASSWORDS, login, '');
        Result := S = pwd
      end
    else if (login=DEFAULT_ADMIN_USER) and (pwd=DEFAULT_ADMIN_PWD) then
      Result:=True
    else
      Result := False;
  finally
    RW.LeaveRead
  end;
end;

procedure GetUsers(Users : TStrings);
var
  I : integer;
begin
  RW.EnterRead;
  try
    DB.ReadSection(T_USERS, Users);
    for I := 0 to Users.Count - 1 do
      Users[I] := Format('%s=%s', [Users[I], DB.ReadString(T_USERS_NAMES, Users[I], '')]);
  finally
    RW.LeaveRead
  end;
end;

function DeleteUser(login : RtcString) : boolean;
begin
  RW.EnterWrite;
  try
    if DB.ValueExists(T_USERS, login) then
      begin
        DB.DeleteKey(T_USERS, login);
        DB.DeleteKey(T_USERS_NAMES, login);
        DB.DeleteKey(T_USERS_PASSWORDS, login);
        DB.UpdateFile;
        Result := True;
      end
    else
      Result := False;
  finally
    RW.LeaveWrite
  end;
end;

function ChangePassword(login, old_pwd, pwd : RtcString) : boolean;
begin
  RW.EnterWrite;
  try
    if (login <> '') and (old_pwd <> '') and (pwd <> '') and
       (old_pwd = DB.ReadString(T_USERS_PASSWORDS, login, '')) then
      begin
        DB.WriteString(T_USERS_PASSWORDS, login, pwd);
        DB.UpdateFile;
        Result := True;
      end
    else if (login=DEFAULT_ADMIN_USER) and (old_pwd=DEFAULT_ADMIN_PWD) and (pwd<>'') and
       not DB.ValueExists(T_USERS, login) then
      begin
        DB.WriteString(T_USERS_PASSWORDS, login, pwd);
        DB.UpdateFile;
        Result := True;
      end
    else
      Result := False;
  finally
    RW.LeaveWrite
  end;
end;

function IsUserExists(login : RtcString) : boolean;
begin
  RW.EnterRead;
  try
    Result := DB.ValueExists(T_USERS, login);
  finally
    RW.LeaveRead;
  end;
end;

function SetLastLogon(login: RtcString; Timestamp: TDateTime) : boolean;
begin
  RW.EnterWrite;
  try
    Result := DB.ValueExists(T_USERS, login);
    if Result then
      DB.WriteDateTime(T_USERS_LAST_LOGON, login, Timestamp);
  finally
    RW.LeaveWrite;
  end;
end;

function GetLastLogon(login: RtcString; var Timestamp: TDateTime) : boolean;
begin
  RW.EnterRead;
  try
    Result := DB.ValueExists(T_USERS, login);
    if Result then
      Timestamp := DB.ReadDateTime(T_USERS_LAST_LOGON, login, 0.0)
    else
      Timestamp := 0.0;
  finally
    RW.LeaveRead
  end;
end;

function AddPack(name, order_link, extend_lic_link : RtcString; var id : RtcString) : boolean;
begin
  RW.EnterWrite;
  try
    id := GetNextID(T_PACKS);
    DB.WriteString(T_PACKS, id, name);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), ORDER, order_link);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), EXTEND_LIC, extend_lic_link);
    DB.UpdateFile;
  finally
    RW.LeaveWrite
  end;
  Result := True;
end;

function GetPackInfo(id: RtcString; var name, order_link, extend_lic_link : RtcString) : boolean;
begin
  RW.EnterRead;
  try
    Result := DB.ValueExists(T_PACKS, id);
    if Result then
      begin
        name := DB.ReadString(T_PACKS, id, '');
        order_link := DB.ReadString(Format(T_PACKS_LINKS, [id]), ORDER, '');
        extend_lic_link := DB.ReadString(Format(T_PACKS_LINKS, [id]), EXTEND_LIC, '');
      end
    else
      begin
        name := '';
        order_link := '';
        extend_lic_link := '';
      end;
  finally
    RW.LeaveRead
  end;
end;

function SavePackInfo(id, name, order_link, extend_lic_link : RtcString) : boolean;
begin
  RW.EnterWrite;
  try
    DB.WriteString(T_PACKS, id, name);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), ORDER, order_link);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), EXTEND_LIC, extend_lic_link);
    DB.UpdateFile;
  finally
    RW.LeaveWrite
  end;
  Result := True;
end;

procedure GetPacks(Packs : TStrings);
begin
  RW.EnterRead;
  try
    DB.ReadSectionValues(T_PACKS, Packs);
  finally
    RW.LeaveRead;
  end;
end;

function DeletePack(id : RtcString) : boolean;
var
  Files : TStringList;
begin
  Result := False;

  RW.EnterWrite;
  try
    if DB.ValueExists(T_PACKS, id) then
      begin
        if DB.SectionExists(Format(T_PACKS_FILES, [id])) then begin
          Files := TStringList.Create;
          try
            DB.ReadSectionValues(Format(T_PACKS_FILES, [id]), Files);
            if Files.Count > 0 then
              Exit;
          finally
            Files.Free;
          end;
        end;
        DB.DeleteKey(T_PACKS, id);
        DB.DeleteKey(T_PACKS_VISIBILITY, id);
        DB.UpdateFile;
        Result := True;
      end;
  finally
    RW.LeaveWrite
  end;
end;

function _GetValueFromIndex(SL:TStringList; Index: Integer): RtcString;
  begin
  if Index >= 0 then
    Result := Copy(SL.Strings[Index], Length(SL.Names[Index]) + 2, MaxInt) else
    Result := '';
  end;

function IsPackExists(packname : RtcString) : boolean;
var
  SL : TStringList;
  I : integer;
begin
  Result := False;
  RW.EnterRead;
  try
    SL := TStringList.Create;
    try
      GetPacks(SL);
      for I := 0 to SL.Count - 1 do
        if CompareText(packname, _GetValueFromIndex(SL,I))=0 then begin
          Result := True;
          Break;
        end;
    finally
      SL.Free;
    end;
  finally
    RW.LeaveRead;
  end;
end;

function SetPackVisibility(pack_id, vis_level : RtcString) : boolean;
begin
  RW.EnterWrite;
  try
    DB.WriteString(T_PACKS_VISIBILITY, pack_id, vis_level);
    DB.UpdateFile;
  finally
    RW.LeaveWrite;
  end;
  Result := True;
end;

function GetPackVisibility(pack_id : RtcString) : RtcString;
begin
  Result := DEFAULT_PACK_VISIBILITY_LEVEL;
  RW.EnterRead;
  try
    Result := DB.ReadString(T_PACKS_VISIBILITY, pack_id, DEFAULT_PACK_VISIBILITY_LEVEL);
  finally
    RW.LeaveRead;
  end;
end;

function AddFile(pack_id, fname, description, size, datetime : RtcString; var id : RtcString) : boolean;
var
  SL : TStringList;
  sec_pack_files : RtcString;

  function __GetIDbyName (fname : RtcString) : RtcString;
  var
    Files : TStringList;
    I : integer;
    subs : RtcString;
  begin
    Files := TStringList.Create;
    try
      DB.ReadSectionValues(T_FILES, Files);
      subs := Format('fname=%s', [fname]);
      for I := 0 to Files.Count - 1 do
        if Pos(subs, Files[I]) > 0 then begin
          Result := Files.Names[I];
          Break;
        end;
    finally
      Files.Free;
    end;
  end;
  
begin
  RW.EnterWrite;
  try
    if DB.ValueExists(T_PACKS, pack_id) then
      begin
        sec_pack_files := Format(T_PACKS_FILES, [pack_id]);
        SL := TStringList.Create;
        try
          if id = '' then begin
            id := __GetIDbyName(fname);
            if id = '' then
              id := GetNextID(T_FILES);
          end;

          SL.Clear;
          SL.CommaText := DB.ReadString(T_FILES, id, '');
          SL.Values['fname'] := fname;
          SL.Values['desc'] := description;
          SL.Values['date'] := datetime;
          SL.Values['size'] := size;

          if not DB.ValueExists(sec_pack_files, id) then begin
            SL.Values['count'] := IntToStr(StrToIntDef(SL.Values['count'], 0) + 1);
            DB.WriteString(sec_pack_files, id, '');
          end;
          
          DB.WriteString(T_FILES, id, SL.CommaText);
          DB.UpdateFile;
          Result := True;
        finally
          SL.Free;
        end;
      end
    else
      Result := False;
  finally
    RW.LeaveWrite
  end;
end;

function GetFileInfo(pack_id, file_id : RtcString; var fname, description, filedatetime, size : RtcString) : boolean;
var
  SL : TStringList;
  sec_pack_files : RtcString;
begin
  RW.EnterRead;
  try
    sec_pack_files := format(T_PACKS_FILES, [pack_id]);
    Result := DB.ValueExists(sec_pack_files, file_id);
    if Result then
      begin
        SL := TStringList.Create;
        try
          SL.CommaText := DB.ReadString(T_FILES, file_id, '');
          fname := SL.Values['fname'];
          description := SL.Values['desc'];
          filedatetime := SL.Values['date'];
          size := SL.Values['size'];
        finally
          SL.Free;
        end;
      end
    else
      begin
        fname := '';
        description := '';
        filedatetime := '';
        size := '';
      end;
  finally
    RW.LeaveRead
  end;
end;

function IsFileExists(fname : RtcString; var packname : RtcString) : boolean;
var
  Packs : TStringList;
  pack_id : RtcString;
  I : integer;
begin
  Result := False;
  Packs := TStringList.Create;
  try
    GetPacks(Packs);
    for I := 0 to Packs.Count - 1 do begin
      pack_id := Packs.Names[I];
      if IsFileInPackExists(pack_id, fname) then begin
        packname := _GetValueFromIndex(Packs,I);
        Result := true;
        Break;
      end;
    end;
  finally
    Packs.Free;
  end;
end;

function IsFileInPackExists(pack_id, fname : RtcString) : boolean;
var
  Files : TStringList;
  SL : TStringList;
  I : integer;
begin
  Result := False;

  RW.EnterRead;
  try
    Files := TStringList.Create;
    try
      GetFiles(pack_id, Files);
      if Files.Count = 0 then
        Exit;

      SL := TStringList.Create;
      try
        for I := 0 to Files.Count - 1 do begin
          SL.CommaText := _GetValueFromIndex(Files,I);
          if CompareText(fname, SL.Values['fname'])=0 then begin
            Result := True;
            Break;
          end;
        end;
      finally
        SL.Free;
      end;
    finally
      Files.Free;
    end;
  finally
    RW.LeaveRead;
  end;
end;

procedure GetFiles(pack_id : RtcString; Files : TStrings);
var
  I : integer;
  file_id : RtcString;
begin
  RW.EnterRead;
  try
    DB.ReadSectionValues(Format(T_PACKS_FILES, [pack_id]), Files);
    for I := 0 to Files.Count - 1 do begin
      file_id := Files.Names[I];
      Files[I] := Format('%s=%s', [file_id, DB.ReadString(T_FILES, file_id, '')]);
    end;
  finally
    RW.LeaveRead
  end;
end;

procedure GetFilesTiny(pack_id : RtcString; Files : TStrings);
begin
  RW.EnterRead;
  try
    DB.ReadSectionValues(Format(T_PACKS_FILES, [pack_id]), Files);
  finally
    RW.LeaveRead
  end;
end;

procedure GetFilesAll(Files : TStrings); 
begin
  RW.EnterRead;
  try
    DB.ReadSectionValues(T_FILES, Files);
  finally
    RW.LeaveRead
  end;
end;

function SaveFileDesc(file_id, description : RtcString) : boolean;
var
  SL : TStringList;
begin
  RW.EnterWrite;
  try
    if DB.ValueExists(T_FILES, file_id) then
      begin
        SL := TStringList.Create;
        try
          SL.CommaText := DB.ReadString(T_FILES, file_id, '');
          SL.Values['desc'] := description;
          DB.WriteString(T_FILES, file_id, SL.CommaText);
          DB.UpdateFile;
          Result := True;
        finally
          SL.Free;
        end;
      end
    else
      Result := False;
  finally
    RW.LeaveWrite;
  end;
end;

function BindFile(pack_id, file_id : RtcString) : boolean;
var
  SL : TStringList;
  sec_pack_files : RtcString;
begin
  RW.EnterWrite;
  try
    if DB.ValueExists(T_PACKS, pack_id) then
      begin
        sec_pack_files := Format(T_PACKS_FILES, [pack_id]);
        SL := TStringList.Create;
        try
          SL.CommaText := DB.ReadString(T_FILES, file_id, '');
          SL.Values['count'] := IntToStr(StrToIntDef(SL.Values['count'], 0) + 1);
          DB.WriteString(sec_pack_files, file_id, '');
          DB.WriteString(T_FILES, file_id, SL.CommaText);
          DB.UpdateFile;
          Result := True;
        finally
          SL.Free;
        end;
      end
    else
      Result := False;
  finally
    RW.LeaveWrite
  end;
end;

function DelFile(pack_id, file_id : RtcString) : boolean;
var
  SL : TStringList;
  cnt : integer;
begin
  Result := False;

  RW.EnterWrite;
  try
    if DB.ValueExists(T_PACKS, pack_id) then begin
      DB.DeleteKey(Format(T_PACKS_FILES, [pack_id]), file_id);
      SL := TStringList.Create;
      try
        SL.CommaText := DB.ReadString(T_FILES, file_id, '');
        cnt := StrToIntDef(SL.Values['count'], 0) - 1;
        SL.Values['count'] := IntToStr(cnt);
        if cnt <= 0 then
          begin
            DB.DeleteKey(T_FILES, file_id);
            Result := True;
          end
        else
          DB.WriteString(T_FILES, file_id, SL.CommaText);
      finally
        SL.Free;
      end;
      DB.UpdateFile;
    end;
  finally
    RW.LeaveWrite
  end;
end;

function GetGrant(login, pack_id : RtcString; var expiredate : RtcString) : boolean;
begin
  RW.EnterRead;
  try
    expiredate := '';
    Result := CompareText(GetPackVisibility(pack_id), 'public')=0;
    if Result then
      Exit
    else
      Result := DB.ValueExists(format(T_USERS_GRANTS, [login]), pack_id);
      
    if Result then
      begin
        expiredate := DB.ReadString(format(T_USERS_GRANTS, [login]), pack_id, '');
      end
    else
      begin
        expiredate := '';
      end;
  finally
    RW.LeaveRead
  end;
end;

function SaveUserAccess(login, pack_id : RtcString; grant: boolean; expiredate : RtcString) : boolean;
begin
  RW.EnterWrite;
  try
    if grant then
      DB.WriteString(format(T_USERS_GRANTS, [login]), pack_id, expiredate)
    else
      DB.DeleteKey(format(T_USERS_GRANTS, [login]), pack_id);
    DB.UpdateFile;
    Result := True;
  finally
    RW.LeaveWrite
  end;
end;

function IsFileAvailableForUser(login, filename : RtcString) : boolean;
var
  Packs : TStringList;
  grant : boolean;
  s_expiredate : RtcString;
  d_expiredate : TDateTime;
  I : integer;
  pack_id : RtcString;
begin
  Result := False;
  Packs := TStringList.Create;
  GetPacks(Packs);
  if Packs.Count > 0 then begin
    for I := 0 to Packs.Count - 1 do begin
      pack_id := Packs.Names[I];
      grant := GetGrant(login, pack_id, s_expiredate);
      if grant then begin
        if Trim(s_expiredate) <> '' then
          d_expiredate := Str2DateTime(s_expiredate)
        else
          d_expiredate := Now;

        if Trunc(d_expiredate) >= Trunc(Now) then begin
          Result := IsFileInPackExists(pack_id, filename);
          if Result then
            Break;
        end;
      end;
    end;
  end;
  Packs.Free;
end;

initialization
  RW:=TRtcRWSec.Create;;

finalization;
  FreeAndNil(DB);
  FreeAndNil(RW);
end.
