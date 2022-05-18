unit rtcForumProvider;

{$INCLUDE defines.inc}
{$include rtcDefs.inc}

interface

uses
  SysUtils, Classes, Windows, Forms,
  rtcTypes, rtcSystem, rtcLog,
  rtcConn, rtcDataSrv, rtcInfo,
  uDB, uForumDB, uMessages, uCmdDisp;

var
  MAX_SEND_BLOCK_SIZE : int64 = 1460*38; // larger files will be sent in smaller blocks
  MAX_ACCEPT_BODY_SIZE : int64 = 128000;

type
  TForum_Provider = class(TDataModule)
    ServerLink: TRtcDataServerLink;
    PageProvider: TRtcDataProvider;
    FileProvider: TRtcDataProvider;

    procedure DataModuleDestroy(Sender: TObject);
    procedure PageProviderDisconnect(Sender: TRtcConnection);
    procedure PageProviderCheckRequest(Sender: TRtcConnection);
    procedure PageProviderDataReceived(Sender: TRtcConnection);
    procedure FileProviderCheckRequest(Sender: TRtcConnection);
    procedure FileProviderSendBody(Sender: TRtcConnection);
    procedure FileProviderDisconnect(Sender: TRtcConnection);
    procedure DataModuleCreate(Sender: TObject);

  private
    ExtList:TStringList;
    CTypesList:TList;

    Web_Host,
    Web_RootFile,
    Web_Root,
    Web_Files,

    Templates_Path, // Templates and static files are stored here
    Upload_Path:string; // Uploaded files are stored here

    function GetContentType(FName: string): string;

  public
    procedure Init(WebHost,WebRoot,LocalPath:string);

    procedure ClearContentTypes;
    procedure AddContentType(a:string);
  end;

function GetForumProvider : TForum_Provider;

implementation


{$R *.dfm}

type
  TStringObject=class
    public
      value:RtcString;
    end;

var
  __ForumDM : TForum_Provider;

function GetForumProvider : TForum_Provider;
  begin
  if not Assigned(__ForumDM) then
    __ForumDM := TForum_Provider.Create(nil);

  Result := __ForumDM;
  end;

function _IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + '\';
end;

procedure TForum_Provider.Init(WebHost, WebRoot, LocalPath: string);
  begin
  Web_Host:=LowerCase(WebHost); // Forum Host starts with ...

  // Forum Application URI
  if WebRoot='' then
    WebRoot:='/'
  else if Copy(WebRoot,length(WebRoot),1)<>'/' then
    WebRoot:=WebRoot+'/';

  Web_Root:= LowerCase(WebRoot);
  Web_RootFile:= Copy(Web_Root,1,length(Web_Root)-1);

  Web_Files:=Web_Root+'files/'; // URI where files will be downloaded from (used in templates)

  if ExpandFileName(LocalPath)<>LocalPath then
    LocalPath:=_IncludeTrailingPathDelimiter(ExtractFilePath(AppFileName))+LocalPath;

  LocalPath:=_IncludeTrailingPathDelimiter(ExpandFileName(LocalPath));

  Templates_Path:=LocalPath+'www\';
  Upload_Path:=LocalPath+'files\';

  InitForumData(LocalPath+'data\',
                Templates_Path,
                Upload_Path);
  end;

procedure TForum_Provider.DataModuleDestroy(Sender: TObject);
begin
  __ForumDM := nil;

  ClearContentTypes;
  ExtList.Free;
  CTypesList.Free;
end;

procedure TForum_Provider.PageProviderCheckRequest(Sender: TRtcConnection);
  begin
  with Sender as TRtcDataServer do
    if (Web_Host='') or (CompareText(Copy(Request.Host,1,length(Web_Host)),Web_Host)=0) then
      if ( CompareText(Request.FileName,Web_Root)=0 ) or
         ( CompareText(Request.FileName,Web_RootFile)=0 ) then
        Accept;
  end;

procedure TForum_Provider.PageProviderDataReceived(Sender: TRtcConnection);
var
  Srv : TRtcDataServer;
  Disp : TCommandsDispatcher;
  len : cardinal;
  Body : RtcString;
begin
  Srv := TRtcDataServer(Sender);

  if Srv.Request.Method='POST' then
    Srv.Request.Params.AddText(Srv.Read);

  if Srv.Request.Complete then
    begin
    if CompareText(Srv.Request.FileName,Web_RootFile)=0 then
      begin
      Srv.Response.Status(301,'Moved Permanently');
      Srv.Response['LOCATION']:= Web_Root;
      Srv.Write('Status 301: Moved Permanently');
      end
    else
      begin
      Disp := TCommandsDispatcher.Create(Srv);
      try
        Body := Disp.MakeDispatch;
        len := Length(Body);
        if len > 0 then
          Srv.Response['Content-Type'] := 'text/html';

        Srv.Response['Cache-Control'] := 'no-store, no-cache, must-revalidate, post-check=0, pre-check=0';
        Srv.Response['Pragma'] := 'no-cache';
        Srv.Write(Body);

        with Srv do
          XLog('EXEC '+PeerAddr+' > '+Request['HOST'] {.rHost} +
               ' "'+Request.Method+' '+Request.FileName+'"'+
               ' '+IntToStr(len)+
               ' REF "'+Request.Referer+'"'+
               ' AGENT "'+Request.Agent+'"');
      finally
        Disp.Free;
        // Make sure the session is unlocked, we don't need it anymore.
        Srv.UnLockSession;
      end;
    end;
  end;
end;

procedure TForum_Provider.PageProviderDisconnect(Sender: TRtcConnection);
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

procedure TForum_Provider.FileProviderCheckRequest(Sender: TRtcConnection);

  procedure CheckDiskFile(Sender: TRtcDataServer);
    var
      fsize:int64;
      Content_Type:string;
      MyFileName,
      DocRoot:string;

    function _FindSession : boolean;
    begin
      Result :=
        Sender.FindSession(Sender.Request.Query['sid'])
        {$IFDEF USE_COOKIE_SESSIONID}
        or Sender.FindSession(Sender.Request.Cookie['session'])
        {$ENDIF}
    end;

    function RepairFileName(NeedAuth:boolean; FileName:string):string;

      function AccessAllowed:boolean;
        begin
        if not NeedAuth or (FileName='') then
          Result:=True
        else if not _FindSession then
          Result:=IsFileAvailableForUser('', ExtractFileName(FileName))
        else
          begin
          if Sender.Session.asBoolean['login'] and
              IsFileAvailableForUser(Sender.Session.asString['user_name'], ExtractFileName(FileName)) then
            Result:=True
          else
            Result:=False;
          { Make sure session is unlocked,
            so it can be used by the rest of the app. }
          Sender.UnLockSession;
          end;
        end;

      begin
      // Get only file name
      FileName:=ExtractFileName(StringReplace(FileName,'/','\',[rfreplaceall]));
      // Generate Full file name, with full path
      FileName:=ExpandFileName(DocRoot + FileName);

      // Using special characters to move outside of root folder, DENY access ...
      if (Pos('\..',FileName)>0) or
         (LowerCase(Copy(FileName,1,length(DocRoot)))<>LowerCase(DocRoot)) then
        begin
        Sender.Accept;

        XLog('DENY '+Sender.PeerAddr+' > '+Sender.Request.Host+
             ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
             ' 0'+
             ' REF "'+Sender.Request.Referer+'"'+
             ' AGENT "'+Sender.Request.Agent+'" > Invalid FileName: "'+FileName+'".');

        Sender.Response.Status(403,'Forbidden');
        Sender.Write(GetMsg('error_403_forbidden'));

        Result:='';
        fsize:=-1;
        end
      else
        begin
        // Check if user is allowed to access this file ...
        if not AccessAllowed then
          begin
          // Forbidden

          Sender.Accept;

          XLog('DENY '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' 0'+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'" > Forbidden!: "'+MyFileName+'".');

          Sender.Response.Status(403,'Forbidden');
          Sender.Write(GetMsg('error_403_forbidden_download'));

          Result:='';
          fsize:=-1;
          end
        else
          begin
          fsize:=File_Size(FileName);
          Sender.Request.Info['Filename.Full'] := FileName;
          Result:=StringReplace(FileName,'\','/',[rfreplaceall]);
          end;
        end;
      end;

    begin
    with Sender do
      begin
      if LowerCase(Copy(Request.FileName,1,length(Web_Files))) = Web_Files then
        begin
        DocRoot:=Upload_Path;
        MyFileName := RepairFileName(True, URL_Decode(Request.FileName));
        end
      else
        begin
        DocRoot:=Templates_Path;
        MyFileName := RepairFileName(False, URL_Decode(Request.FileName));
        end;

      if MyFileName <> '' then
        begin
        if fsize > 0 then
          begin
          Accept; // found the file, we will be responding to this request.

          // Check if we have some info about the content type for this file ...
          Content_Type:=GetContentType(MyFileName);

          XLog('SEND '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' '+IntToStr(fsize)+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'"'+
               ' TYPE "'+Content_Type+'"');

          Request.FileName:=StringReplace(MyFileName,'/','\',[rfReplaceAll]);

          Response.ContentType:=Content_Type;
          Response.ContentLength:=fsize;
          if Request.Method='HEAD' then
            Response.SendContent:=False;

          WriteHeader;
          end
        else if fsize = 0 then
          begin
          // Found the file, but it is empty.
          Accept;

          XLog('SEND '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' 0'+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'"');

          Write;
          end
        else
          begin
          // File not found.
          Accept;

          XLog('FAIL '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' 0'+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'" > File not found: "'+MyFileName+'".');

          Response.Status(404,'File not found');
          Write(GetMsg('error_404_file_not_found'));
          end;
        end;
      end;
    end;

  begin
  with TRtcDataServer(Sender).Request do
    if ( (Web_Host='') or (LowerCase(Copy(Host,1,length(Web_Host))) = Web_Host) ) and
       ( LowerCase(Copy(FileName,1,length(Web_Root))) = Web_Root ) and
       ( (Method='GET') or (Method='HEAD') ) then
      CheckDiskFile(TRtcDataServer(Sender));
  end;

procedure TForum_Provider.FileProviderSendBody(Sender: TRtcConnection);
  var
    s :string;
    Filename : string;
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.Complete then
      begin
      if Response.DataOut<Response.DataSize then
        begin // need to send more content
        Filename := Request.Info['Filename.Full'];

        if Response.DataSize-Response.DataOut>MAX_SEND_BLOCK_SIZE then
          s := Read_File(FileName, Response.DataOut, MAX_SEND_BLOCK_SIZE)
        else
          s := Read_File(FileName, Response.DataOut, Response.DataSize-Response.DataOut);

        if s = '' then // Error reading file.
          begin
          XLog('ERR! '+PeerAddr+' > '+Request.Host+
               ' "'+Request.Method+' '+Request.URI+'"'+
               ' > Error reading File: "'+FileName+'".');
          Disconnect;
          end
        else
          Write(s);
        end;
      end
    else if Request.DataSize>MAX_ACCEPT_BODY_SIZE then // Do not accept requests with body longer than 128K
      begin
      XLog('BAD! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> Content size exceeds 128K limit (size='+IntToStr(Request.DataSize)+' bytes).');

      Response.Status(400,'Bad Request');
      Write(GetMsg('error_400_bad_request'));
      end;
    end;
  end;

procedure TForum_Provider.FileProviderDisconnect(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
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

procedure TForum_Provider.ClearContentTypes;
var
    a:integer;
begin
  for a:=0 to CTypesList.Count-1 do
    with TStringObject(CTypesList.Items[a]) do
      begin
      value:='';
      Free;
      end;
  CTypesList.Clear;
  ExtList.Clear;
end;

procedure TForum_Provider.AddContentType(a: string);
var
    elist,ext:string;
    loc:integer;
    htext:string;
    octype:TStringObject;
begin
  loc:=Pos('=',a);
  if loc>0 then
    begin
    elist:=Trim(Copy(a,1,loc-1));
    htext:=Trim(Copy(a,loc+1,MaxInt));

    octype:=TStringObject.Create;
    octype.value:=htext;

    CTypesList.Add(octype);

    while elist<>'' do
      begin
      if Pos(',',elist)>0 then
        begin
        ext:=UpperCase(Trim(Copy(elist,1,Pos(',',elist)-1)));
        if Copy(ext,1,1)<>'.' then ext:='.'+ext;
        Delete(elist,1,Pos(',',elist));
        elist:=Trim(elist);
        end
      else
        begin
        ext:=UpperCase(elist);
        if Copy(ext,1,1)<>'.' then ext:='.'+ext;
        elist:='';
        end;
      ExtList.AddObject(ext,octype);
      end;
    end;
  ExtList.Sort;
  ExtList.Sorted:=True;
end;

function TForum_Provider.GetContentType(FName: string): string;
var
    loc:integer;
    ext:string;
begin
  ext:=UpperCase(ExtractFileExt(FName));
  loc:=ExtList.IndexOf(ext);
  if loc>=0 then
    Result:=TStringObject(ExtList.Objects[loc]).value
  else
    begin
    loc:=ExtList.IndexOf('*');
    if loc>=0 then
      Result:=TStringObject(ExtList.Objects[loc]).value
    else
      Result:='';
    end;
end;

procedure TForum_Provider.DataModuleCreate(Sender: TObject);
  begin
  ExtList := TStringList.Create;
  CTypesList := TList.Create;
  end;

initialization

finalization
  if Assigned(__ForumDM) then
    begin
    __ForumDM.Free;
    __ForumDM:=nil;
    end;

end.
