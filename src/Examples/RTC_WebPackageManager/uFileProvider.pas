unit uFileProvider;

{$INCLUDE defines.inc}
{$include rtcDefs.inc}

interface

uses
  Windows, SysUtils, Classes,

  {$IFDEF VER120}
  Forms, FileCtrl, // D4
  {$ENDIF}
  {$IFDEF VER130}
  Forms, FileCtrl, // D5
  {$ENDIF}

  rtcSystem, rtcLog,
  rtcInfo, rtcConn,
  rtcDataSrv;

type
  TFileDM = class(TDataModule)
    Provider: TRtcDataProvider;
    ServerLink: TRtcDataServerLink;

    procedure ProviderDisconnect(Sender: TRtcConnection);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ProviderSendBody(Sender: TRtcConnection);
    procedure ProviderCheckRequest(Sender: TRtcConnection);

  private
    { Private declarations }
    HostList:TStringList;
    PageList:TStringList;
    ExtList:TStringList;
    CTypesList:TList;
    VirtualFoldersList:TStringList;

  protected
    function GetDocRoot(Host: string): string;
    function GetContentType(FName: string): string;

    function RepairWebFileName(FileName,DocRoot:string):string;

  public
    { Public declarations }
    procedure ClearHosts;
    procedure AddHost(a:string);

    procedure ClearIndexPages;
    procedure AddIndexPage(a:string);

    procedure ClearContentTypes;
    procedure AddContentType(a:string);

    procedure ClearVirtualFolders;
    procedure AddVirtualFolder(a:string);
  end;


function GetFileProvider : TFileDM;

implementation

uses
  uDB, uMessages;

{$R *.dfm}

type
  TStringObject=class
    public
      value:string;
    end;

var
  __FileDM : TFileDM = nil;

  MAX_SEND_BLOCK_SIZE : int64 = 1460*38; // larger files will be sent in smaller blocks
  MAX_ACCEPT_BODY_SIZE : int64 = 128000;

function GetFileProvider : TFileDM;
begin
  if not Assigned(__FileDM) then
    __FileDM := TFileDM.Create(nil);
  Result := __FileDM;
end;

procedure TFileDM.DataModuleCreate(Sender: TObject);
begin
  HostList:=TStringList.Create;
  HostList.Sorted := True;

  PageList := TStringList.Create;
  ExtList := TStringList.Create;
  CTypesList := TList.Create;

  VirtualFoldersList := TStringList.Create;
  VirtualFoldersList.Sorted := True;
end;

procedure TFileDM.DataModuleDestroy(Sender: TObject);
begin
  __FileDM := nil;

  ClearHosts;
  HostList.Free;

  ClearIndexPages;
  PageList.Free;

  ClearContentTypes;
  ExtList.Free;
  CTypesList.Free;

  VirtualFoldersList.Free;
end;

procedure TFileDM.ClearHosts;
var
    a:integer;
begin
  for a:=0 to HostList.Count-1 do
    with TStringObject(HostList.Objects[a]) do
      begin
      value:='';
      Free;
      end;
  HostList.Clear;
end;

procedure TFileDM.ClearIndexPages;
begin
  PageList.Clear;
end;

procedure TFileDM.ClearContentTypes;
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

procedure TFileDM.AddHost(a: string);
var
    loc:integer;
    hname,hdir:string;
    odir:TStringObject;
begin
  loc:=Pos('=',a);
  if loc>0 then
    begin
    hname:=UpperCase(Trim(Copy(a,1,loc-1)));
    hdir:=Trim(Copy(a,loc+1,MaxInt));

    hdir:=ExpandFileName(StringReplace(hdir,'/','\',[rfReplaceAll]));
    if Copy(hdir,length(hdir),1)='\' then
      Delete(hdir,length(hdir),1);

    odir:=TStringObject.Create;
    odir.value:=hdir;

    HostList.AddObject(hname,odir);
    end;
  HostList.Sort;
  HostList.Sorted:=True;
end;

procedure TFileDM.AddIndexPage(a: string);
begin
  if Trim(a)<>'' then
    PageList.Add(Trim(a));
end;

procedure TFileDM.AddContentType(a: string);
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

function TFileDM.GetContentType(FName: string): string;
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

function TFileDM.GetDocRoot(Host: string): string;
var
  loc, p : integer;
begin
  Host:=UpperCase(Host);

  // find Host
  loc:=HostList.IndexOf(Host);
  if loc>=0 then
    Result:=TStringObject(HostList.Objects[loc]).value
  else
    begin
      // if port specified - find Host without port
      P := Pos(':', Host);
      if P > 0 then
        Host := Copy(Host, 1, P - 1);
      loc:=HostList.IndexOf(Host);
      if loc>=0 then
        Result:=TStringObject(HostList.Objects[loc]).value
      else
        begin
          // find * - Default host
          loc:=HostList.IndexOf('*');
          if loc>=0 then
            Result:=TStringObject(HostList.Objects[loc]).value
          else
            Result:='';
        end;
    end;
end;

function TFileDM.RepairWebFileName(FileName,DocRoot:string):string;
var
    a:integer;
    FullName:string;
begin
  FileName:=StringReplace(FileName,'/','\',[rfreplaceall]);

  FullName:=ExpandFileName(DocRoot+FileName);
  // Check if our path is inside DocRoot
  if (Pos('\..',FullName)>0) or
     (UpperCase(Copy(FullName,1,length(DocRoot)))<>UpperCase(DocRoot)) then
    begin
    Result:='';
    Exit;
    end;

  FileName:=FullName;
  Delete(FileName,1,length(DocRoot));
  // Check if FileName is a folder with missing '\' at the end.
  if Copy(FileName,length(FileName),1)<>'\' then
    if DirectoryExists(DocRoot+FileName) then
      begin
      Result:='';
      Exit;
      end;

  // Check if FileName is a folder with existing index file
  if Copy(FileName,length(FileName),1)='\' then
    begin
    for a:=0 to PageList.Count-1 do
      begin
      if File_Exists(FullName+PageList.Strings[a]) then
        begin
        FileName:=FileName+PageList.Strings[a];
        Break;
        end;
      end;
    end;

  Result:=StringReplace(FileName,'\','/',[rfreplaceall]);
end;

{ *** FILE PROVIDER *** }

procedure TFileDM.ProviderCheckRequest(Sender: TRtcConnection);

  procedure CheckDiskFile(Sender: TRtcDataServer);
    var
      fsize:int64;
      Content_Type:string;
      MyFileName,
      DocRoot:string;
      S : string;

    function _FindSession : boolean;
    begin
      Result :=
        Sender.FindSession(Sender.Request.Query['sid'])
        {$IFDEF USE_COOKIE_SESSIONID}
        or Sender.FindSession(Sender.Request.Cookie['session'])
        {$ENDIF}
    end;

    function RepairFileName(FileName:string):string;
    var
      a : integer;
      FullName : string;
      loc : integer;
      Folder, S : string;
      NeedAuth : boolean;
    begin
      FileName:=StringReplace(FileName,'/','\',[rfreplaceall]);

      NeedAuth := False;

      Folder := ExtractFileDir(Filename);
      if Folder = '\' then
        Folder := DocRoot
      else
        begin
          loc := VirtualFoldersList.IndexOf(Folder);
          if loc >= 0 then
            begin
              NeedAuth := True;
              Delete(Filename, 1, Length(Folder));
              Folder := TStringObject(VirtualFoldersList.Objects[loc]).Value;
            end
          else
            begin
              NeedAuth := False;
              Folder := DocRoot;
            end;
        end;

      FullName := ExpandFileName(Folder + FileName);
      
      if (Pos('\..',FullName)>0) or
         (UpperCase(Copy(FullName,1,length(Folder)))<>UpperCase(Folder)) then
        begin
          Sender.Accept;

          XLog('DENY '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' 0'+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'" > Invalid FileName: "'+FullName+'".');

          Sender.Response.Status(403,'Forbidden');
          S := GetMsg('error_403_forbidden');
          Sender.Response.ContentLength := Length(S);
          Sender.Write(S);

          Result:='';
          fsize:=-1;
          Exit;
        end
      else
        begin
          FileName:=FullName;
          Delete(FileName,1,length(Folder));
        end;

      if Copy(FileName,length(FileName),1)<>'\' then
        if DirectoryExists(Folder+FileName) then begin
          Sender.Accept;

          Sender.Response.Status(301,'Moved Permanently');
          Sender.Response['LOCATION'] := Sender.Request.FileName+'/';
          Sender.Write('Status 301: Moved Permanently');

          Result:='';
          Exit;
        end;

      if Copy(FileName,length(FileName),1)='\' then
        begin
          for a:=0 to PageList.Count-1 do begin
            fsize:=File_Size(FullName+PageList.Strings[a]);
            if fsize>=0 then begin
              FullName := FullName + PageList.Strings[a];
              FileName := FileName + PageList.Strings[a];
              Break;
            end;
          end;
        end
      else
        fsize:=File_Size(FullName);

      // check access restriction
      if NeedAuth and (fsize > 0) and (Filename <> '') and
        not (_FindSession and Sender.Session.asBoolean['login'] and
          IsFileAvailableForUser(Sender.Session.asString['user_name'], ExtractFileName(FileName))) then
      begin
        // Forbidden

        Sender.Accept;
        
        XLog('FAIL '+Sender.PeerAddr+' > '+Sender.Request.Host+
             ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
             ' 0'+
             ' REF "'+Sender.Request.Referer+'"'+
             ' AGENT "'+Sender.Request.Agent+'" > Forbidden!: "'+MyFileName+'".');

        Sender.Response.Status(403,'Forbidden');
        S := GetMsg('error_403_forbidden_download');
        Sender.Response.ContentLength := Length(S);
        Sender.Write(S);

        Result:='';
        fsize:=-1;
        Exit;
      end;

      Sender.Request.Info['Filename.Full'] := FullName;
      Result:=StringReplace(FileName,'\','/',[rfreplaceall]);
    end;

  begin
    with Sender do begin
        // Check HOST and find document root
        DocRoot:=GetDocRoot(Request.Host);
        if DocRoot='' then begin
          Sender.Accept;

          XLog('BAD! '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' 0'+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'" > Invalid Host: "'+Request.Host+'".');

          Response.Status(400,'Bad Request');
          S := GetMsg('error_400_bad_request');
          Response.ContentLength := Length(S);
          Write(S);

          Exit;
        end;

        // Check File Name and send Result Header
        MyFileName := RepairFileName(URL_Decode(Request.FileName));
        if MyFileName <> '' then begin
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
              S := GetMsg('error_404_file_not_found');
              Response.ContentLength := Length(S);
              Write(S);
            end;
        end;
    end;
  end;

begin
  with TRtcDataServer(Sender).Request do
    if (Method='GET') or (Method='HEAD') then
      CheckDiskFile(TRtcDataServer(Sender));
end;

procedure TFileDM.ProviderSendBody(Sender: TRtcConnection);
var
  s :string;
  Filename : string;
begin
  with TRtcDataServer(Sender) do begin
    if Request.Complete then
      begin
        if Response.DataOut<Response.DataSize then begin // need to send more content
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
        S := GetMsg('error_400_bad_request');
        Response.ContentLength := Length(S);
        Write(S);

        Disconnect;
      end;
  end;
end;

procedure TFileDM.ProviderDisconnect(Sender: TRtcConnection);
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

procedure TFileDM.AddVirtualFolder(a: string);
var
  loc : integer;
  v_folder, dir : string;
  odir : TStringObject;
begin
  loc := Pos('=',a);
  if loc > 0 then begin
    v_folder := UpperCase(Trim(Copy(a, 1, loc-1)));
    v_folder := StringReplace(v_folder,'/','\',[rfReplaceAll]);
    if v_folder[1] <> '\' then
      v_folder := '\' + v_folder;
    dir := Trim(Copy(a, loc+1, MaxInt));
    dir := ExpandFileName(StringReplace(dir,'/','\',[rfReplaceAll]));
    if Copy(dir, length(dir), 1)='\' then
      Delete(dir,length(dir), 1);

    odir := TStringObject.Create;
    odir.value := dir;

    VirtualFoldersList.AddObject(v_folder, odir);
  end;
  VirtualFoldersList.Sort;
  VirtualFoldersList.Sorted:=True;
end;

procedure TFileDM.ClearVirtualFolders;
var
  a:integer;
begin
  for a:=0 to VirtualFoldersList.Count-1 do
    with TStringObject(VirtualFoldersList.Objects[a]) do begin
      Value := '';
      Free;
    end;
  VirtualFoldersList.Clear;
end;

initialization

finalization
  if Assigned(__FileDM) then
    begin
    __FileDM.Free;
    __FileDM:=nil;
    end;
end.
