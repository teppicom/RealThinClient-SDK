{
  @html(<b>)
  ISAPI Extensions Provider
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit implements methods for executing ISAPI extensions received with @Link(TRtcDataServer).
}
unit rtcISAPI;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows,
  SysUtils, Classes,

  isapi2, ComObj, ActiveX, // needed for ISAPI

  rtcTypes,
  rtcSystem,
  rtcLog,
  rtcInfo,
  rtcConn,
  rtcDataSrv,

  rtcThrPool;

type
  {@abstract(ISAPI Extensions loader and executer)
   TRtcISAPI declares "class methods" for executing ISAPI extensions
   received as requests for TRtcDataServer. }
  TRtcISAPI=class
  public
    // Start up ISAPI extensions provider: call for "OnListenStart"!
    class procedure StartUp;
    // Shut down ISAPI exensions provider: call for "OnListenStop"!
    class procedure ShutDown;

    // Unload all ISAPI extensions (when needed)
    class procedure UnLoadAll;

    { @html(
      Execute ISAPI Request: Call for "OnDataReceived" event! <br><br>

      Preconditions: <br>
      Con.Request.Info['ROOT'] = Document Root for this Host <br>
      Con.Request.Info['DLL'] = ISAPI DLL File name with full path <br>
      Con.Request.Info['PATH'] = "Path" defined behind the ISAPI DLL file name) }
    class procedure Execute(Con:TRtcDataServer);
    end;

implementation

type
  TIsapi = class
  public
    FName:String;
    init:boolean;
    Handle:THandle;
    Term:TTerminateExtension;
    ExtProc:THttpExtensionProc;
    ExtVer:TGetExtensionVersion;
    end;

  TRtcECB=class(TObject)
  public
    ecb:PEXTENSION_CONTROL_BLOCK;

    constructor Create;
    destructor Destroy; override;
    end;

  TIsapiDLLs = class
  private
    FList:TStringList;
    CS:TRtcCritSec;

  public
    constructor Create;
    destructor Destroy; override;

    function Get(FileName:String):TIsapi;
    procedure Close(isapi:TIsapi);

    procedure CloseAll;
  end;

var
  IsapiDLLs:TIsapiDLLs;
  Isapi_Ready:boolean=False;

{ TRtcECB }

constructor TRtcECB.Create;
  begin
  inherited;
  New(ecb);
  FillChar(ecb^, sizeof(ecb^), 0);
  end;

destructor TRtcECB.Destroy;
  begin
  FreeMem(ecb);
  inherited;
  end;

constructor TIsapiDLLs.Create;
  begin
  inherited;
  FList:=TStringList.Create;
  FList.Sorted:=True;
  CS:=TRtcCritSec.Create;
  end;

destructor TIsapiDLLs.Destroy;
  begin
  CloseAll;
  FList.Free;
  CS.Free;
  inherited;
  end;

function TIsapiDLLs.Get(FileName:String):TIsapi;
  var
    i:integer;
    h:THandle;
  begin
  FileName:=UpperCase(FileName);
  CS.Acquire;
  try
    i:=FList.IndexOf(FileName);
    if i>=0 then
      Result:=TIsapi(FList.Objects[i])
    else
      begin
      XLog('----------------------------------------------------------'#13#10+
           'Loading ISAPI Extension "'+RtcString(FileName)+'" ...','ISAPI');

      h:=LoadLibrary(PChar(FileName));
      if h=0 then
        begin
        XLog('Error loading ISAPI Extension "'+RtcString(FileName)+'".','ISAPI');
        Result:=nil;
        Exit;
        end;

      Result:=TIsapi.Create;
      Result.Handle:=h;
      Result.FName:=FileName;
      Result.Init:=False;
      Result.ExtVer:=GetProcAddress(h, 'GetExtensionVersion');
      Result.ExtProc:=GetProcAddress(h, 'HttpExtensionProc');
      Result.Term:=GetProcAddress(h, 'TerminateExtension');

      FList.AddObject(FileName, Result);

      XLog('ISAPI Extension "'+RtcString(FileName)+'" loaded.','ISAPI');
      end;
  finally
    CS.Release;
    end;
  end;

procedure TIsapiDLLs.Close(isapi:TIsapi);
  var
    Buff:array [0..64] of Byte; // AnsiChar;
  begin
  CS.Acquire;
  try
    if Assigned(isapi.Term) then
      begin
      XLog('Asking ISAPI Extension "'+RtcString(isapi.FName)+'" to terminate ...','ISAPI');
      isapi.Term(0);
      end;
    if GetModuleFileNameA(isapi.Handle, @Buff[0],63)>0 then
      FreeLibrary(isapi.Handle);
    isapi.Free;
  finally
    CS.Release;
    end;
  end;

procedure TIsapiDLLs.CloseAll;
  var
    i:integer;
  begin
  CS.Acquire;
  try
    for i:=0 to FList.Count-1 do
      Close(TIsapi(FList.Objects[i]));
    FList.Clear;
  finally
    CS.Release;
    end;
  end;

function GetServerVariable(hConn: HCONN; VariableName: PAnsiChar; Buffer: Pointer; var Size: DWORD ): BOOL stdcall;
  var
    Con : TRtcDataServer;
    i : integer;
    s,v,vname : RtcString;
    len: dword;
  begin
  con := TRtcDataServer(hConn);
  v:=UpperCase(RtcPBytesZeroToString(VariableName^));
  s:='';

  if v='SERVER_SOFTWARE' then
    s:='www.realthinclient.com'
  else if v='SERVER_SIGNATURE' then
    s:='RealThinClient SDK'
  else if v='SERVER_PROTOCOL' then
    s:='HTTP/1.1'
  else if v='REQUEST_METHOD' then
    s:=con.Request.Method
  else if v='QUERY_STRING' then
    s:=con.Request.Query.Text
  else if (v='REQUEST_URI') or (v='HTTP_URL') then
    s:=con.Request.URI
  else if (v='SCRIPT_FILENAME') or (v='SCRIPT_NAME') or (v='SELF') or (v='URL') then
    s:=con.Request.FileName
  else if (v='SERVER_NAME') then
    s:=con.Request.Host
  else if (v='SERVER_ADDR') then
    s:=con.LocalAddr
  else if (v='SERVER_PORT') then
    s:=con.LocalPort
  else if (v='REMOTE_ADDR') or (v='REMOTE_HOST') or (v='REMOTE_USER') then
    s:=con.PeerAddr
  else if (v='REMOTE_PORT') then
    s:=con.PeerPort
  else if (v='ALL_RAW') then
    s:=con.Request.HeaderText
  else if (v='ALL_HTTP') then
    begin
    s:='';
    for i:=0 to con.Request.ItemCount-1 do
      begin
      vname:=RtcString(UpperCase(StringReplace(String(con.Request.ItemName[i]),'-','_',[rfReplaceAll])));
      s:=s+'HTTP_'+vname+':'+con.Request.ItemValue[i]+#13#10;
      end;
    end
  else if (v='CONTENT_LENGTH') then
    s:=con.Request['CONTENT-LENGTH']
  else if (v='CONTENT_TYPE') then
    s:=con.Request['CONTENT-TYPE']
  else if (v='DOCUMENT_ROOT') then
    s:=RtcString(con.Request.Info['ROOT'])
  else
    begin
    for i:=0 to con.Request.ItemCount-1 do
      begin
      vname:=RtcString(UpperCase(StringReplace(String(con.Request.ItemName[i]),'-','_',[rfReplaceAll])));
      if v='HTTP_'+vname then
        begin
        s:=con.Request.ItemValue[i];
        Break;
        end;
      end;
    end;

  len:=length(s);

  if s='' then
    begin
    Result:=False;
    Size:=0;
    XLog(RtcString('GetServerVariable(')+VariableName+') = <empty>','ISAPI');
    end
  else if Size<=len then
    begin
    Result:=False;
    Size:=length(s)+1;
    XLog(RtcString('GetServerVariable(')+VariableName+') = <buffer too small>','ISAPI');
    end
  else
    begin
    Result:=True;
    Size:=length(s)+1;
    RtcStringToPBytesZero(s,Buffer^,1,length(s));
    XLog(RtcString('GetServerVariable(')+VariableName+')="'+s+'"','ISAPI');
    end;
  end;

function WriteClient(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD; dwReserved: DWORD ): BOOL stdcall;
  var
    con : TRtcDataServer;
    s:RtcString;
  begin
  con := TRtcDataServer(ConnID);
  SetString(s,PAnsiChar(Buffer),Bytes);
  con.Write(s);
  Result := True;
  end;

function ReadClient(ConnID: HCONN; Buffer: Pointer; var Size: DWORD): BOOL stdcall;
  begin
  Result:=False;
  end;

function ServerSupport(hConn: HCONN; HSERRequest: DWORD; Buffer: Pointer; Size: LPDWORD; DataType: LPDWORD ): BOOL stdcall;
  var
    con: TRtcDataServer;
    s:RtcString;
  begin
  con:=TRtcDataServer(hConn);
  case HSERRequest of
    HSE_REQ_SEND_RESPONSE_HEADER:
      begin
      if Buffer<>nil then
        begin
        s:=RtcPBytesZeroToString(Buffer^);
        con.Response.Status(s);
        end;
      if DataType<>nil then
        begin
        s:=RtcPBytesZeroToString(DataType^);
        con.Response.HeaderText:=s;
        XLog('ServerSupport(HSE_REQ_SEND_RESPONSE_HEADER):'#13#10+s,'ISAPI');
        end;
      con.WriteHeader;
      Result := True;
      end;
    HSE_REQ_SEND_URL_REDIRECT_RESP:
      begin
      if Buffer<>nil then
        begin
        s:=RtcPBytesZeroToString(Buffer^);
        con.Response.StatusCode:=302;
        con.Response.StatusText:='Moved Termporarily';
        con.Response['LOCATION']:= s;
        con.WriteHeader;
        XLog('ServerSupport(HSE_REQ_SEND_URL_REDIRECT_RESP):'+s,'ISAPI');
        Result := True;
        end
      else
        Result:=False;
      end;
    HSE_REQ_DONE_WITH_SESSION:
      begin
      if con.Response['CONTENT-LENGTH']='' then
        begin
        con.Disconnect;
        XLog('ServerSupport(HSE_REQ_DONE_WITH_SESSION): No content length, DISCONNECTING!','ISAPI');
        end
      else
        XLog('ServerSupport(HSE_REQ_DONE_WITH_SESSION): Content Length='+Int2Str(con.Response.ContentLength)+'; Out='+Int2Str(con.Response.ContentOut),'ISAPI');
      Result:=True;
      end;
    else
      begin
      XLog('ServerSupport('+Int2Str(HSERRequest)+') - UNSUPPORTED FUNCTION!','ISAPI');
      Result := False;
      end;
    end;
  end;

class procedure TRtcISAPI.Execute(Con:TRtcDataServer);
  var
    isapi : Tisapi;
    Ver   : THSE_VERSION_INFO;
    FName : String;
  begin
  if not Con.Request.Complete then Exit;
  try
    FName:=Con.Request.Info.asText['DLL'];

    isapi:=IsapiDLLs.Get(FName);
    if isapi=nil then
      begin
      con.Response.Status(404,'Not found');
      con.Write('Error loading ISAPI Extension "'+RtcString(FName)+'".');
      Exit;
      end;

    if assigned(isapi.ExtVer) then
      begin
      if not isapi.init then
        begin
        isapi.ExtVer(Ver);
        isapi.init:=True;
        XLog('----------------------------------------------------'#13#10+
             'InitDLL('+RtcString(FName)+'): '+Con.Request.Host+' > '+Con.Request.URI+' (Ver '+
             Int2Str(Ver.dwExtensionVersion shr 16)+','+int2str(Ver.dwExtensionVersion and $FFFF)+' '+
             RtcPBytesZeroToString(Ver.lpszExtensionDesc)+')',
             'ISAPI');
        end;
      end;

    if not assigned(isapi.ExtProc) then
      begin
      con.Response.Status(500,'ISAPI Extension error');
      con.Write('Unable to find ISAPI Extension Procedure.');
      Exit;
      end;

    Con.Request.Info.asObj['ECB']:=TRtcECB.Create;

    with TRtcECB(Con.Request.Info.asObj['ECB']) do
      begin
      FillChar(ecb^, sizeof(ecb^), 0);
      ecb^.cbSize                := sizeof(ecb);
      ecb^.dwVersion             := HSE_VERSION_MAJOR shl 16 + HSE_VERSION_MINOR;
      ecb^.ConnID                := THandle(Con);
      ecb^.dwHttpStatusCode      := Con.Response.StatusCode;
      ecb^.lpszMethod            := @RtcStringToBytesZero(Con.Request.Method)[0];

      ecb^.lpszQueryString       := @RtcStringToBytesZero(Con.Request.Query.Text)[0];
      ecb^.lpszPathInfo          := @RtcStringToBytesZero(Con.Request.Info.asString['PATH'])[0];
      ecb^.lpszPathTranslated    := @RtcStringToBytesZero(Con.Request.Info.asString['PATH'])[0];

      ecb^.lpbData               := @RtcStringToBytesZero(con.Read)[0];
      ecb^.cbAvailable           := con.Request.ContentIn;
      ecb^.cbTotalBytes          := Con.Request.ContentIn;

      ecb^.GetServerVariable     := GetServerVariable;
      ecb^.WriteClient           := WriteClient;
      ecb^.ReadClient            := ReadClient;
      ecb^.ServerSupportFunction := ServerSupport;

      XLog('Execute('+RtcString(FName)+'): '+con.Request.Method+' '+con.Request.URI+#13#10+
           Con.Request.HeaderText,
           'ISAPI');
      isapi.ExtProc(ecb^);
      end;
  except
    on E:Exception do
      begin
      Con.Response.Status(200,'Internal ISAPI Error');
      Con.Write(#13#10+RtcString(E.Message));
      XLog('Internal Exception in ISAPI "'+Con.Request.FileName+'": '+RtcString(E.Message),'ISAPI');
      end;
    end;
  end;

class procedure TRtcIsapi.StartUp;
  begin
  if not Isapi_Ready then
    begin
    Isapi_Ready:=True;
    IsapiDLLs:=TIsapiDLLs.Create;
    end;
  end;

class procedure TRtcIsapi.ShutDown;
  begin
  if Isapi_Ready then
    begin
    Isapi_Ready:=False;
    if assigned(IsapiDLLs) then
      begin
      IsapiDLLs.Free;
      IsapiDLLs:=nil;
      end;
    end;
  end;

class procedure TRtcISAPI.UnLoadAll;
  begin
  if Isapi_Ready then
    IsapiDLLs.CloseAll;
  end;

initialization
CoInitFlags:=COINIT_MULTITHREADED; // needed for ISAPI
finalization
TRtcIsapi.ShutDown;
end.
