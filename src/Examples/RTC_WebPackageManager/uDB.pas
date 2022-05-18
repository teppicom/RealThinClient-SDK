unit uDB;

{$INCLUDE defines.inc}

interface

uses
  Classes,
  IniFiles,
  SysUtils,
  rtcSystem,
  rtcInfo;

type
  TCallBackProc = procedure (Login : string);

// Users support
function GetUserInfo(login : string; var name, pwd : string) : boolean;
function SaveUserInfo(login, name, pwd : string) : boolean;
function CheckUser(login, pwd : string) : boolean;
procedure GetUsers(Users : TStrings);
function DeleteUser(login : string) : boolean;
function ChangePassword(login, old_pwd, pwd : string) : boolean;
function IsUserExists(login : string) : boolean;

// Packages support
function AddPack(name, order_link, extend_lic_link : string; var id : string) : boolean;
function GetPackInfo(id: string; var name, order_link, extend_lic_link : string) : boolean;
function SavePackInfo(id, name, order_link, extend_lic_link : string) : boolean;
procedure GetPacks(Packs : TStrings);
function DeletePack(id : string) : boolean;
function IsPackExists(packname : string) : boolean;

// Files support
function AddFile(pack_id, fname, description, size, datetime : string; var id : string) : boolean;
function GetFileInfo(pack_id, file_id : string; var fname, description, filedatetime, size : string) : boolean;
procedure GetFiles(pack_id : string; Files : TStrings);
function SaveFile(pack_id, file_id, fname, description : string) : boolean;
function DelFile(pack_id, file_id : string) : boolean;
function IsFileInPackExists(pack_id, fname : string) : boolean;
function IsFileExists(fname : string; var packname : string) : boolean;

// Access support
function GetGrant(login, pack_id : string; var expiredate : string) : boolean;
function SaveUserAccess(login, pack_id : string; grant: boolean; expiredate : string) : boolean;
function IsFileAvailableForUser(login, filename : string) : boolean;

//Common functions
function GetNextID(entity : string) : string;   //entity: names, packages, files

function _GetValueFromIndex(SL:TStringList; Index: Integer): string;

implementation

var
  __DB : TMemIniFile = nil;
  __RW : TRtcRWSec;

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
  //
  // Packages
  //
  T_PACKS           = 'packages';
  T_PACKS_FILES     = 'files::package::%s';
  T_PACKS_LINKS     = 'links::package::%s';
  ORDER             = 'Order';
  EXTEND_LIC        = 'Extend license';

function DB : TMemIniFile;
begin
  if not Assigned(__DB) then
    __DB := TMemIniFile.Create(ExtractFilePath(AppFileName) + 'users.data');
  Result := __DB;
end;

function RW : TRtcRWSec;
begin
  if not Assigned(__RW) then
    __RW := TRtcRWSec.Create;
  Result := __RW;
end;

function GetNextID(entity : string) : string;   //entity: users, packages, files
var
  id : integer;
begin
  id := DB.ReadInteger(T_IDS, entity, 0) + 1;
  DB.WriteInteger(T_IDS, entity, id);
  Result := IntToStr(id);
end;

function GetUserInfo(login : string; var name, pwd : string) : boolean;
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

function SaveUserInfo(login, name, pwd : string) : boolean;
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

function CheckUser(login, pwd : string) : boolean;
var
  S : string;
begin
  RW.EnterRead;
  try
    if DB.ValueExists(T_USERS, login) then
      begin
        S := DB.ReadString(T_USERS_PASSWORDS, login, '');
        Result := S = pwd
      end
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

function DeleteUser(login : string) : boolean;
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

function ChangePassword(login, old_pwd, pwd : string) : boolean;
begin
  RW.EnterWrite;
  try
    if (login <> '') and (old_pwd <> '') and
      (pwd <> '') and (old_pwd = DB.ReadString(T_USERS_PASSWORDS, login, '')) then
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

function IsUserExists(login : string) : boolean;
begin
  Result := DB.ValueExists(T_USERS, login);
end;

function AddPack(name, order_link, extend_lic_link : string; var id : string) : boolean;
begin
  // Result := False;

  RW.EnterWrite;
  try
    id := GetNextID(T_PACKS);
    DB.WriteString(T_PACKS, id, name);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), ORDER, order_link);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), EXTEND_LIC, extend_lic_link);
    DB.UpdateFile;
    Result := True;
  finally
    RW.LeaveWrite
  end;
end;

function GetPackInfo(id: string; var name, order_link, extend_lic_link : string) : boolean;
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

function SavePackInfo(id, name, order_link, extend_lic_link : string) : boolean;
begin
  // Result := False;

  RW.EnterWrite;
  try
    DB.WriteString(T_PACKS, id, name);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), ORDER, order_link);
    DB.WriteString(Format(T_PACKS_LINKS, [id]), EXTEND_LIC, extend_lic_link);
    DB.UpdateFile;
    Result := True;
  finally
    RW.LeaveWrite
  end;
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

function DeletePack(id : string) : boolean;
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
        DB.UpdateFile;
        Result := True;
      end;
  finally
    RW.LeaveWrite
  end;
end;

function _GetValueFromIndex(SL:TStringList; Index: Integer): string;
  begin
  if Index >= 0 then
    Result := Copy(SL.Strings[Index], Length(SL.Names[Index]) + 2, MaxInt) else
    Result := '';
  end;

function IsPackExists(packname : string) : boolean;
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

function AddFile(pack_id, fname, description, size, datetime : string; var id : string) : boolean;
var
  SL : TStringList;
  sec_files : string;
begin
  // Result := False;

  RW.EnterWrite;
  try
    if DB.ValueExists(T_PACKS, pack_id) then
      begin
        sec_files := Format(T_PACKS_FILES, [pack_id]);
        id := GetNextID(sec_files);
        SL := TStringList.Create;
        try
          SL.Values['fname'] := fname;
          SL.Values['desc'] := description;
          SL.Values['date'] := datetime;
          SL.Values['size'] := size;
          DB.WriteString(sec_files, id, SL.CommaText);
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

function GetFileInfo(pack_id, file_id : string; var fname, description, filedatetime, size : string) : boolean;
var
  SL : TStringList;
  sec_files : string;
begin
  // Result := False;

  RW.EnterRead;
  try
    sec_files := format(T_PACKS_FILES, [pack_id]);
    Result := DB.ValueExists(sec_files, file_id);
    if Result then
      begin
        SL := TStringList.Create;
        try
          SL.CommaText := DB.ReadString(sec_files, file_id, '');
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

function IsFileExists(fname : string; var packname : string) : boolean;
var
  SL : TStringList;
  pack_id : string;
  I : integer;
begin
  Result := False;
  SL := TStringList.Create;
  try
    GetPacks(SL);
    for I := 0 to SL.Count - 1 do begin
      pack_id := SL.Names[I];
      if IsFileInPackExists(pack_id, fname) then begin
        packname := _GetValueFromIndex(SL,I);
        Result := true;
        Break;
      end;
    end;
  finally
    SL.Free;
  end;

end;

function IsFileInPackExists(pack_id, fname : string) : boolean;
var
  Files : TStringList;
  SL : TStringList;
  I : integer;
begin
  Result := False;
  Files := TStringList.Create;
  try
    GetFiles(pack_id, Files);
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
end;

procedure GetFiles(pack_id : string; Files : TStrings);
begin
  RW.EnterRead;
  try
    DB.ReadSectionValues(Format(T_PACKS_FILES, [pack_id]), Files);
  finally
    RW.LeaveRead
  end;
end;

function SaveFile(pack_id, file_id, fname, description : string) : boolean;
var
  SL : TStringList;
begin
  // Result := False;
  RW.EnterWrite;
  try
    if DB.ValueExists(T_PACKS, pack_id) then
      begin
        SL := TStringList.Create;
        try
          SL.Values['fname'] := fname;
          SL.Values['desc'] := description;
          DB.WriteString(Format(T_PACKS_FILES, [pack_id]), file_id, SL.CommaText);
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

function DelFile(pack_id, file_id : string) : boolean;
begin
  // Result := False;
  
  RW.EnterWrite;
  try
    if DB.ValueExists(T_PACKS, pack_id) then
      begin
        DB.DeleteKey(Format(T_PACKS_FILES, [pack_id]), file_id);
        DB.UpdateFile;
        Result := True;
      end
    else
      Result := False;
  finally
    RW.LeaveWrite
  end;
end;

function GetGrant(login, pack_id : string; var expiredate : string) : boolean;
begin
  RW.EnterRead;
  try
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

function SaveUserAccess(login, pack_id : string; grant: boolean; expiredate : string) : boolean;
begin
  // Result := False;
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

function IsFileAvailableForUser(login, filename : string) : boolean;
var
  Packs : TStringList;
  // Files : TStringList;
  grant : boolean;
  s_expiredate : string;
  d_expiredate : TDateTime;
  I : integer;
  pack_id : string;
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
  RW;
  DB;

finalization;
  if Assigned(__DB) then begin
    try
      __DB.UpdateFile;
    except
    end;
    __DB.Free;
    __DB:=nil;
  end;

  if Assigned(__RW) then begin
    __RW.Free;
    __RW:=nil;
  end;
end.




