unit uPageProvider;

{$INCLUDE defines.inc}

interface

uses
  SysUtils, Classes,

  {$IFDEF VER120}
  Forms, FileCtrl, // D4
  {$ENDIF}
  {$IFDEF VER130}
  Forms, FileCtrl, // D5
  {$ENDIF}

  rtcSystem, rtcInfo, rtcConn, rtcDataSrv, rtcParse;

type
  TPageDM = class(TDataModule)
    ServerLink: TRtcDataServerLink;
    Provider: TRtcDataProvider;
    procedure DataModuleDestroy(Sender: TObject);
    procedure ProviderDisconnect(Sender: TRtcConnection);
    procedure ProviderCheckRequest(Sender: TRtcConnection);
    procedure ProviderSendBody(Sender: TRtcConnection);
    procedure DataModuleCreate(Sender: TObject);
  private
    fTemplatePath: TStringList;
    fUploadPath: TStringList;
  public
    property TemplatePath : TStringList read fTemplatePath;
    property UploadPath : TStringList read fUploadPath;
  end;

  TCommandsDispatcher = class
  private
    fSrv : TRtcDataServer;
    fPageMessage : string;
    function GetRequestValue (name : string) : string;
    function GetSession: TRtcServerSession;
    function GetTemplatePath: string;
    function GetUploadPath: string;
    procedure ShowPageMessage(Page : TRtcParse);
  protected
    procedure Relocation (URL : string);
    //
    function DoCommonAccess(Cmd : string) : string;
    function DoAdminAccess(Cmd : string) : string;
    //
    function DoIndexPage (msg : string = '' ) : string;
    function DoAdmin : string;
    function DoUser : string;
    //
    function DoLogin : integer;
    function DoLogOut : string;
    //
    function DoShowUsers : string;
    function DoEditUser : string;
    function DoSaveUser : string;
    function DoDelUser : string;
    //
    function DoEditAccess : string;
    function DoShowAccess : string;
    function DoSaveAccess : string;
    //
    function DoShowPackages : string;
    function DoAddPack : string;
    function DoEditPack : string;
    function DoSavePack : string;
    function DoDelPack : string;
    function DoPackFiles : string;
    function DoAddFile : string;
    function DoEditFile : string;
    function DoSaveFile : string;
    function DoDelFile : string;
    //
    function DoSavePwd : string;
    function DoChangePwd : string;
  public
    constructor Create(aSrv : TRtcDataServer);
    function Output : string;
    //
    property Srv : TRtcDataServer read fSrv;
    property Session : TRtcServerSession read GetSession;
    property TemplatePath : string read GetTemplatePath;
    property UploadPath : string read GetUploadPath;
  end;

const
  ACCESS_LEVEL_DENIED = -1;
  ACCESS_LEVEL_USER   = 0;
  ACCESS_LEVEL_ADMIN  = 1;

function GetPageProvider : TPageDM;

implementation

uses
  rtcLog,
  uDB, uMessages;

{$R *.dfm}

var
  __PageDM : TPageDM = nil;

function GetPageProvider : TPageDM;
begin
  if not Assigned(__PageDM) then
    __PageDM := TPageDM.Create(nil);

  Result := __PageDM;
end;

function FineSize(Size : string) : string;
var
  N : cardinal;
  suff : string;
  devisor : cardinal;
  R : Double;
begin
  suff := '';
  N := StrToIntDef(Size, 0);
  if N < 1024 then
    begin
      suff := 'b';
      devisor := 1;
    end
  else if N < 1048576 then
    begin
      suff := 'Kb';
      devisor := 1024;
    end
  else if N < 1073741824 then
    begin
      suff := 'Mb';
      devisor := 1048576;
    end
  else
    begin
      suff := 'Gb';
      devisor := 1073741824;
    end;

  if devisor > 1 then
    begin
      R := N / devisor;
      Result := Format('%-.2f %s', [R, suff]);
    end
  else
    Result := Format('%d %s', [N, suff]);
end;

procedure TPageDM.DataModuleCreate(Sender: TObject);
begin
  fTemplatePath := TStringList.Create;
  fTemplatePath.Sorted := True;

  fUploadPath := TStringList.Create;
  fUploadPath.Sorted := True;
end;

procedure TPageDM.DataModuleDestroy(Sender: TObject);
begin
  __PageDM := nil;

  fTemplatePath.Free;
  fUploadPath.Free;
end;

procedure TPageDM.ProviderDisconnect(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do begin
    if Request.DataSize > Request.DataIn then
      begin
        // did not receive a complete request
        XLog('ERR! '+PeerAddr+' > '+Request['HOST'] {.rHost} +
             ' "'+Request.Method+' '+Request.URI+'"'+
             ' 0'+
             ' REF "'+Request.Referer+'"'+
             ' AGENT "'+Request.Agent+'" '+
             '> DISCONNECTED while receiving a Request ('+IntToStr(Request.DataIn)+' of '+IntToStr(Request.DataSize)+' bytes received).');
      end
    else if Response.DataSize > Response.DataOut then
      begin
        // did not send a complete result
        XLog('ERR! '+PeerAddr+' > '+Request.Host+
             ' "'+Request.Method+' '+Request.URI+'"'+
             ' -'+IntToStr(Response.DataSize-Response.DataOut)+
             ' REF "'+Request.Referer+'"'+
             ' AGENT "'+Request.Agent+'" '+
             '> DISCONNECTED while sending a Result ('+IntToStr(Response.DataOut)+' of '+IntToStr(Response.DataSize)+' bytes sent).');
      end;
    end;
end;

procedure TPageDM.ProviderCheckRequest(Sender: TRtcConnection);
var
  S : string;
begin
  with Sender as TRtcDataServer do
    if Request.FileName = '/' then begin
      S := TemplatePath.Values[Request.Host];
      if S = '' then
        S := TemplatePath.Values['*'];
      Request.Info['template path'] := S;

      S := UploadPath.Values[Request.Host];
      if S = '' then
        S := UploadPath.Values['*'];
      Request.Info['upload path'] := S;

      Accept;
    end;
end;

procedure TPageDM.ProviderSendBody(Sender: TRtcConnection);
var
  Srv : TRtcDataServer;
  Disp : TCommandsDispatcher;
begin
  Srv := TRtcDataServer(Sender);

  if Srv.Request.Method='POST' then
    Srv.Request.Params.AddText(Srv.Read);

  if Srv.Request.Complete then begin
    Disp := TCommandsDispatcher.Create(Srv);
    try
      Srv.Write( Disp.Output );
    finally
      Disp.Free;
    end;
  end;
end;

{-- TCommandsDispatcher  -------------------------------------------------------}

function TCommandsDispatcher.Output : string;
var
  Cmd : string;
  Access_Level : integer;

  function FindSession : boolean;
  begin
    Result :=
      Srv.FindSession(GetRequestValue('sid'))
      {$IFDEF USE_COOKIE_SESSIONID}
      or Srv.FindSession(Srv.Request.Cookie['session'])
      {$ENDIF}
      ;
  end;

begin
  Access_Level := ACCESS_LEVEL_DENIED;
  Cmd := GetRequestValue('cmd');
  if Cmd = 'login' then begin
      Access_Level := DoLogin;
      case Access_Level of
        ACCESS_LEVEL_USER :
          begin
            Result := DoUser;
            Exit;
          end;
        ACCESS_LEVEL_ADMIN :
          begin
            Result := DoAdmin;
            Exit;
          end;
        else
          begin
            fPageMessage := GetMsg('error_login_failed');
            Result := DoIndexPage;
            Exit;
          end;
      end;
  end;

  if (Access_Level <> ACCESS_LEVEL_DENIED) or
              FindSession and Srv.Session.asBoolean['login'] then
    begin
      Access_Level := Srv.Session.asInteger['access_level'];

      case Access_Level of
        ACCESS_LEVEL_ADMIN :
          Result := DoAdminAccess(Cmd);

        ACCESS_LEVEL_USER :
          Result := DoCommonAccess(Cmd);

        else
          Result := DoIndexPage;
      end;

    end
  else
    Result := DoIndexPage;
end;

function TCommandsDispatcher.DoLogin : integer;
var
  user, pwd: string;
begin
  user := GetRequestValue('user');
  pwd := GetRequestValue('pwd');
  if CheckUser(user, pwd) then
    begin
      Srv.OpenSession;
      if user = 'admin' then
        Srv.Session.asInteger['access_level'] := ACCESS_LEVEL_ADMIN
      else
        Srv.Session.asInteger['access_level'] := ACCESS_LEVEL_USER;
      Srv.Session.asString['user_name'] := user;
      Srv.Session.asBoolean['login']:=True;
      Srv.Session.KeepAlive:=240; // 240 seconds = 4 minutes
      {$IFDEF USE_COOKIE_SESSIONID}
      Srv.Response.Cookie['session'] := Srv.Session.ID;
      {$ENDIF}

      Result := Srv.Session.asInteger['access_level'];
    end
  else
    Result := ACCESS_LEVEL_DENIED;
end;

function TCommandsDispatcher.DoShowUsers : string;
var
  Page : TRtcParse;
  TableRow : TRtcParse;
  S : string;
  Users : TStringList;
  I : integer;
begin
  Page := TRtcParse.Create(TemplatePath + 'users.htm');
  try
    ShowPageMessage(Page);
    S := '';
    TableRow := TRtcParse.Create(TemplatePath + 'users_table_row.htm');
    try
      Users := TStringList.Create;
      try
        GetUsers(Users);
        for I := 0 to Users.Count - 1 do begin
          TableRow['user_login'] := Users.Names[I];
          TableRow['user_login_enc'] := URL_Encode(Users.Names[I]);
          TableRow['user_name'] := _GetValueFromIndex(Users,I);
          TableRow['sid'] := Session.ID;
          S := S + TableRow.Output;
        end;
      finally
        Users.Free;
      end;
    finally
      TableRow.Free;
    end;

    Page['table_rows'] := S;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoShowPackages : string;
var
  Page : TRtcParse;
  TableRow : TRtcParse;
  S : string;
  Packs, Files : TStringList;
  I : integer;
  pack_id : string;
begin
  Page := TRtcParse.Create(TemplatePath + 'packages.htm');
  try
    ShowPageMessage(Page);
    S := '';
    TableRow := TRtcParse.Create(TemplatePath + 'packages_table_row.htm');
    try
      Packs := TStringList.Create;
      Files := TStringList.Create;
      try
        GetPacks(Packs);
        for I := 0 to Packs.Count - 1 do begin
          pack_id := Packs.Names[I];
          GetFiles(pack_id, Files);
          TableRow['pack_id'] := pack_id;
          TableRow['package_name'] := _GetValueFromIndex(Packs,I);
          TableRow['files_count'] := IntToStr(Files.Count);
          TableRow['sid'] := Session.ID;
          S := S + TableRow.Output;
        end;
      finally
        Files.Free;
        Packs.Free;
      end;
    finally
      TableRow.Free;
    end;

    Page['table_rows'] := S;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoEditUser : string;
var
  Page : TRtcParse;
  name, pwd : string;
  login: string;
begin
  Page := TRtcParse.Create(TemplatePath + 'edituser.htm');
  try
    login := GetRequestValue('user');
    GetUserInfo(login, name, pwd);
    if name = '' then
      begin
        Page['caption'] := 'Add user';
        Page['new'] := 'true';
      end
    else
      begin
        Page['caption'] := 'Edit user';
        Page['new'] := '';
      end;
    Page['name'] := name;
    Page['login'] := login;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoSaveUser : string;
var
  login : string;
begin
  login := GetRequestValue('login');
  if (GetRequestValue('new')='true') and IsUserExists(login) then
    fPageMessage := Format(GetMsg('error_user_exists'), [login])
  else
    SaveUserInfo(login, GetRequestValue('name'), GetRequestValue('pwd'));

  Result := DoShowUsers;
end;

function TCommandsDispatcher.DoDelUser : string;
var
  login: string;
begin
  login := GetRequestValue('user');
  if login = 'admin' then
    fPageMessage := Format(GetMsg('error_del_user'), [login])
  else if not DeleteUser(login) then
      fPageMessage := Format(GetMsg('error_del_user'), [login]);

  Result := DoShowUsers;
end;

function TCommandsDispatcher.DoEditAccess : string;
var
  Page : TRtcParse;
  expiredate : string;
  grant : boolean;
  login, pack_id: string;
  packname, order_link, extend_lic_link : string;
begin
  login := GetRequestValue('user');
  pack_id := GetRequestValue('pack_id');
  GetPackInfo(pack_id, packname, order_link, extend_lic_link);


  Page := TRtcParse.Create(TemplatePath + 'editaccess.htm');
  try
    grant := GetGrant(login, pack_id, expiredate);
    if grant then
      Page['grant_checked'] := 'checked';

    Page['expiredate'] := expiredate;

    Page['pack_id'] := pack_id;
    Page['package_name'] := packname;
    Page['user'] := login;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoShowAccess : string;
var
  Page : TRtcParse;
  TableRow : TRtcParse;
  S : string;
  Packs : TStringList;
  I : integer;
  grant : boolean;
  expiredate : string;
  pack_id : string;
  login: string;
begin
  Page := TRtcParse.Create(TemplatePath + 'grantaccess.htm');
  try
    ShowPageMessage(Page);
    login := GetRequestValue('user');
    Page['user'] := login;
    S := '';
    TableRow := TRtcParse.Create(TemplatePath + 'access_table_row2.htm');
    try
      Packs := TStringList.Create;
      try
        GetPacks(Packs);
        for I := 0 to Packs.Count - 1 do begin
          pack_id := Packs.Names[I];
          TableRow['user'] := login;
          TableRow['pack_id'] := pack_id;
          TableRow['package_name'] := _GetValueFromIndex(Packs,I);

          grant := GetGrant(login, pack_id, expiredate);

          if grant then
            TableRow['grant_checked'] := 'checked'
          else
            TableRow['grant_checked'] := '';

          TableRow['expiredate'] := expiredate;
          TableRow['sid'] := Session.ID;
          S := S + TableRow.Output;
        end;
      finally
        Packs.Free;
      end;
    finally
      TableRow.Free;
    end;

    Page['table_rows'] := S;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoSaveAccess : string;
var
  login, pack_id, grant, expiredate: string;
begin
  login := GetRequestValue('user');
  pack_id := GetRequestValue('pack_id');
  grant := GetRequestValue('grant');
  expiredate := Trim(GetRequestValue('expiredate'));
  if expiredate <> '' then
    try
      Str2DateTime(expiredate);
    except
      fPageMessage := GetMsg('error_bad_date');
      expiredate := '';
    end;
  SaveUserAccess(login, pack_id, grant = 'on', expiredate);

  Result := DoShowAccess;
end;

function TCommandsDispatcher.DoAddPack : string;
var
  Page : TRtcParse;
begin
  Page := TRtcParse.Create(TemplatePath + 'editpack.htm');
  try
    Page['caption'] := 'Add package';
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoEditPack : string;
var
  Page : TRtcParse;
  pack_id, packname, order_link, extend_lic_link : string;
begin
  Page := TRtcParse.Create(TemplatePath + 'editpack.htm');
  try
    pack_id := GetRequestValue('pack_id');
    GetPackInfo(pack_id, packname, order_link, extend_lic_link);
    Page['caption'] := 'Edit package';
    Page['pack_id'] := pack_id;
    Page['name'] := packname;
    Page['order_link'] := order_link;
    Page['extend_lic_link'] := extend_lic_link;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoSavePack: string;
var
  pack_id : string;
  packname, order_link, extend_lic_link: string;
begin
  pack_id := GetRequestValue('pack_id');
  packname := URL_Decode(GetRequestValue('name'));
  order_link := URL_Decode(GetRequestValue('order_link'));
  extend_lic_link := URL_Decode(GetRequestValue('extend_lic_link'));

  if pack_id = '' then
    begin
      if IsPackExists(packname) then
        fPageMessage := Format(GetMsg('error_pack_exists'), [packname])
      else
        AddPack(packname, order_link, extend_lic_link, pack_id);
    end
  else
    SavePackInfo(pack_id, packname, order_link, extend_lic_link);
    
  Result := DoShowPackages;
end;

function TCommandsDispatcher.DoDelPack : string;
begin
  if not DeletePack(GetRequestValue('pack_id')) then
    fPageMessage := GetMsg('error_del_pack');

  Result := DoShowPackages;
end;

function TCommandsDispatcher.DoPackFiles : string;
var
  Page : TRtcParse;
  TableRow : TRtcParse;
  S : string;
  Files : TStringList;
  SL : TStringList;
  I : integer;
  pack_id : string;
  packname, order_link, extend_lic_link : string;
begin
  Result := '';
  Page := TRtcParse.Create(TemplatePath + 'packfiles.htm');
  try
    ShowPageMessage(Page);
    pack_id := GetRequestValue('pack_id');
    if not GetPackInfo(pack_id, packname, order_link, extend_lic_link) then
      Exit;

    Page['pack_id'] := pack_id;
    Page['package_name'] := packname;

    S := '';
    TableRow := TRtcParse.Create(TemplatePath + 'files_table_row.htm');
    try
      Files := TStringList.Create;
      SL := TStringList.Create;
      try
        GetFiles(pack_id, Files);
        TableRow['pack_id'] := pack_id;
        for I := 0 to Files.Count - 1 do begin
          SL.CommaText := _GetValueFromIndex(Files,I);
          TableRow['file_id'] := Files.Names[I];
          TableRow['file_name'] := SL.Values['fname'];
          if SL.Values['desc'] <> '' then
            TableRow['file_description'] := SL.Values['desc']
          else
            TableRow['file_description'] := '&nbsp;';
          TableRow['sid'] := Session.ID;
          S := S + TableRow.Output;
        end;
      finally
        SL.Free;
        Files.Free;
      end;
    finally
      TableRow.Free;
    end;

    Page['table_rows'] := S;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoAddFile : string;
var
  Page : TRtcParse;
  pack_id : string;
  packname, order_link, extend_lic_link : string;
begin
  Result := '';
  Page := TRtcParse.Create(TemplatePath + 'addfile.htm');
  try
    pack_id := GetRequestValue('pack_id');
    if not GetPackInfo(pack_id, packname, order_link, extend_lic_link) then
      Exit;

    Page['pack_id'] := pack_id;
    Page['package_name'] := packname;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoEditFile : string;
var
  Page : TRtcParse;
  pack_id, file_id : string;
  packname, order_link, extend_lic_link : string;
  fname, description, filedatetime, size : string;
begin
  Result := '';
  Page := TRtcParse.Create(TemplatePath + 'editfile.htm');
  try
    pack_id := GetRequestValue('pack_id');
    if not GetPackInfo(pack_id, packname, order_link, extend_lic_link) then
      Exit;

    file_id := GetRequestValue('file_id');
    if not GetFileInfo(pack_id, file_id, fname, description, filedatetime, size) then
      Exit;

    Page['pack_id'] := pack_id;
    Page['file_id'] := file_id;
    Page['package_name'] := packname;
    Page['file_name'] := fname;
    Page['name'] := description;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoSaveFile : string;
var
  pack_id, file_id, datetime : string;
  packname, fname, description, size: string;
begin
  Result := '';
  pack_id := GetRequestValue('pack_id');

  file_id := GetRequestValue('file_id');
  fname := Srv.Request.Info['fname'];
  description := URL_Decode(GetRequestValue('description'));

  if file_id = '' then
    begin
      if IsFileExists(fname, packname) then
        fPageMessage := Format(GetMsg('error_file_exists'), [fname, packname])
      else
        begin
          datetime := DateTime2Str(Now);
          datetime := Copy(datetime, 1, Pos('.', datetime)-1);
          size := Srv.Request.Info['size'];
          AddFile(pack_id, fname, description, size, datetime, file_id)
        end
    end
  else
    begin
      SaveFile(pack_id, file_id, fname, description);
    end;

  Result := DoPackFiles;
end;

function TCommandsDispatcher.DoDelFile : string;
var
  pack_id, file_id : string;
  fname, description, filedatetime, size : string;
begin
  pack_id := GetRequestValue('pack_id');
  file_id := GetRequestValue('file_id');
  if GetFileInfo(pack_id, file_id, fname, description, filedatetime, size) then
    Delete_File(fname);

  DelFile(pack_id, file_id);

  Result := DoPackFiles;
end;

function TCommandsDispatcher.DoIndexPage(msg: string): string;
var
  Page : TRtcParse;
begin
  Page := TRtcParse.Create(TemplatePath + 'index.htm');
  try
    ShowPageMessage(Page);
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoLogOut: string;
begin
  Result := DoIndexPage;
end;

function TCommandsDispatcher.DoSavePwd : string;
begin
  ChangePassword(Srv.Session.asString['user_name'], GetRequestValue('old_pwd'), GetRequestValue('pwd'));
  Result := DoLogOut;
end;

function TCommandsDispatcher.DoChangePwd : string;
var
  Page : TRtcParse;
begin
  Page := TRtcParse.Create(TemplatePath + 'user_changepwd.htm');
  try
    Page['login'] := Srv.Session.asString['user_name'];
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoCommonAccess(Cmd: string): string;
begin
  if Cmd = 'logout' then
    begin
      Srv.Session.Close;
      Result := DoLogOut;
    end
  else if Cmd = 'changepwd' then
    begin
      if Srv.Session.asBoolean['login'] then
        Result := DoChangePwd;
    end
  else if Cmd = 'savepwd' then
    begin
      if Srv.Session.asBoolean['login'] then
        Result := DoSavePwd;
    end
  {
  else if Cmd = 'getfile' then
    begin
      if Srv.Session.asBoolean['login'] then
        Result := DoGetFile(Srv.Session.asString['user_name'], GetRequestValue('file'));
    end
  }
  else
    Result := DoIndexPage;
end;

function TCommandsDispatcher.DoAdminAccess(Cmd: string): string;
var
  fname : string;
  size : string;
begin
  if Cmd = 'showusers' then
    Result := DoShowUsers

  else if Cmd = 'showpacks' then
    Result := DoShowPackages

  else if Cmd = 'edituser' then
    Result := DoEditUser

  else if Cmd = 'saveuser' then
    Result := DoSaveUser

  else if Cmd = 'deluser' then
    Result := DoDelUser

  else if Cmd = 'showaccess' then
    Result := DoShowAccess

  else if Cmd = 'editaccess' then
    Result := DoEditAccess

  else if Cmd = 'saveaccess' then
    Result := DoSaveAccess

  else if Cmd = 'addpack' then
    Result := DoAddPack

  else if Cmd = 'editpack' then
    Result := DoEditPack

  else if Cmd = 'savepack' then
    Result := DoSavePack

  else if Cmd = 'delpack' then
    Result := DoDelPack

  else if Cmd = 'packfiles' then
    Result := DoPackFiles

  else if Cmd = 'addfile' then
    Result := DoAddFile

  else if Cmd = 'editfile' then
    Result := DoEditFile

  else if Cmd = 'savefile' then
    begin
      if Srv.Request.Params['datafile']<>'' then
        begin // save after add file
          if not DirectoryExists(UploadPath) then
            CreateDir(UploadPath);
          fname := ExtractFileName(Srv.Request.Params['datafile']);
          Srv.Request.Info['fname'] := fname;
          if Srv.Request.Params.GetFile('datafile', UploadPath+'\' + fname) then begin
            size := IntToStr(File_Size(UploadPath+'\' + fname));
            Srv.Request.Info['size'] := size;
            Result := DoSaveFile;
          end;
        end
      else // save after edit file
        begin
          Srv.Request.Info['fname'] := GetRequestValue('filename');
          Result := DoSaveFile;
        end;
    end

  else if Cmd = 'delfile' then
    Result := DoDelFile

  else
    Result := DoCommonAccess(Cmd);
end;

function TCommandsDispatcher.DoAdmin : string;
var
  Page : TRtcParse;
begin
  Page := TRtcParse.Create(TemplatePath + 'admin.htm');
  try
    ShowPageMessage(Page);
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.DoUser : string;
var
  login : string;
  Page : TRtcParse;
  Packs : TStrings;
  I : integer;
  S : string;
  grant : boolean;
  s_expiredate : string;
  d_expiredate : TDateTime;

  RowFiles : TRtcParse;
  RowOneFile : TRtcParse;
  RowExpired : TRtcParse;
  RowBuy : TRtcParse;

  pack_id : string;
  packname, order_link, extend_lic_link : string;

  procedure InitRows;
  begin
    RowFiles := TRtcParse.Create(TemplatePath + 'user_pack_files.htm');
    RowOneFile := TRtcParse.Create(TemplatePath + 'user_pack_files_row.htm');
    RowExpired := TRtcParse.Create(TemplatePath + 'user_pack_expired.htm');
    RowBuy := TRtcParse.Create(TemplatePath + 'user_pack_order.htm');;
  end;

  procedure DoneRows;
  begin
    if Assigned(RowFiles) then
       begin
       RowFiles.Free;
       RowFiles:=nil;
       end;
    if Assigned(RowOneFile) then
       begin
       RowOneFile.Free;
       RowOneFile:=nil;
       end;
    if Assigned(RowExpired) then
       begin
       RowExpired.Free;
       RowExpired:=nil;
       end;
    if Assigned(RowBuy) then
       begin
       RowBuy.Free;
       RowBuy:=nil;
       end;
  end;

  function PackFiles (pack_id, name, expiredate : string) : string;
  var
    Files : TStringList;
    I : integer;
    description : string;
    filedatetime : string;
    size : string;
    s_out : string;
    fname : string;
    SL : TStringList;
    file_id : string;
  begin
    RowFiles['pack_id'] := pack_id;
    RowFiles['pack_name'] := name;

    if Trim(expiredate) <> '' then
      RowFiles['expire_date'] := expiredate
    else
      RowFiles['expire_date'] := 'n/a';

    s_out := '';

    Files := TStringList.Create;
    SL := TStringList.Create;
    try
      GetFiles(pack_id, Files);
      RowFiles['files_count'] := IntToStr(Files.Count);
      for I := 0 to Files.Count - 1 do begin
        file_id := Files.Names[I];
        GetFileInfo(pack_id, file_id, fname, description, filedatetime, size);
        RowOneFile['file_link'] := URL_Encode(ExtractFileName(fname));
        RowOneFile['file_description'] := description;
        RowOneFile['file_datetime'] := filedatetime;
        RowOneFile['file_size'] := FineSize(size);
        RowOneFile['sid'] := Session.ID;
        s_out := s_out + RowOneFile.Output;
      end;
    finally
      SL.Free;
      Files.Free;
    end;

    RowFiles['table_rows'] := s_out;
    Page['sid'] := Session.ID;
    Result := RowFiles.Output;
  end;

  function PackExpired (name, extend_lic_link : string) : string;
  begin
    RowExpired['pack_name'] := name;
    RowExpired['extend_lic_link'] := extend_lic_link;
    RowExpired['sid'] := Session.ID;
    Result := RowExpired.Output;
  end;

  function PackBuy (name, order_link : string) : string;
  begin
    RowBuy['pack_name'] := name;
    RowBuy['order_link'] := order_link;
    RowBuy['sid'] := Session.ID;
    Result := RowBuy.Output;
  end;

begin
  login := Session.asString['user_name'];
  Page := TRtcParse.Create(TemplatePath + 'user_index.htm');
  try
    ShowPageMessage(Page);
    Page['login'] := login;

    Packs := TStringList.Create;
    GetPacks(Packs);

    S := '';

    if Packs.Count > 0 then
      begin
        InitRows;
        try
          for I := 0 to Packs.Count - 1 do begin
            pack_id := Packs.Names[I];
            grant := GetGrant(login, pack_id, s_expiredate);
            GetPackInfo(pack_id, packname, order_link, extend_lic_link);
            if grant then
              begin
                if Trim(s_expiredate) <> '' then
                  d_expiredate := Str2DateTime(s_expiredate)
                else
                  d_expiredate := Now;
                if Trunc(d_expiredate) < Trunc(Now) then
                  S := S + PackExpired(packname, extend_lic_link)
                else
                  S := S + PackFiles(pack_id, packname, s_expiredate)
              end
            else
              S := S + PackBuy(packname, order_link);

          end;
          Page['table_rows'] := S;
        finally
          DoneRows;
        end;
      end
    else
      begin
       // TODO: show 'no packages' message 
      end;
    Packs.Free;
    Page['sid'] := Session.ID;
    Result := Page.Output;
  finally
    Page.Free;
  end;
end;

function TCommandsDispatcher.GetRequestValue (name : string) : string;
begin
  if Srv.Request.Method='GET' then
    Result := Srv.Request.Query[name]
  else
    Result := Srv.Request.Params[name];
end;

constructor TCommandsDispatcher.Create(aSrv: TRtcDataServer);
begin
  fSrv := aSrv;
end;

function TCommandsDispatcher.GetSession: TRtcServerSession;
begin
  Result := Srv.Session;
end;

function _IncludeTrailingPathDelimiter(const S: string): string;
  begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + '\';
  end;

function TCommandsDispatcher.GetTemplatePath: string;
begin
  Result := _IncludeTrailingPathDelimiter(
    ExpandFileName(Srv.Request.Info['template path'])
  );
end;

function TCommandsDispatcher.GetUploadPath: string;
begin
  Result := _IncludeTrailingPathDelimiter(
    ExpandFileName(Srv.Request.Info['upload path'])
  );
end;

procedure TCommandsDispatcher.ShowPageMessage(Page: TRtcParse);
begin
  if Trim(fPageMessage) <> '' then
    begin
      Page['message_text'] := fPageMessage;
      Page['message_visible'] := '1';
    end
  else
    begin
      Page['message_text'] := '';
      Page['message_visible'] := '';
    end;
  fPageMessage := '';  
end;

procedure TCommandsDispatcher.Relocation(URL: string);
begin
  Srv.Response.Status(302,'Moved');
  Srv.Response['Location'] := URL;
  Srv.Write('Status 302: Moved');
end;

initialization

finalization
  if Assigned(__PageDM) then
    begin
    __PageDM.Free;
    __PageDM:=nil;
    end;

end.
