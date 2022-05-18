unit rtcISAPIProvider;

interface

uses
  SysUtils, Classes,

  {$IFDEF VER120}
  Forms, // D4
  {$ENDIF}

  rtcTypes, rtcSystem, rtcLog, 
  rtcInfo, rtcConn,
  rtcDataSrv,
  rtcISAPI,

  rtcFileProvider;

type
  TISAPI_Provider = class(TDataModule)
    Provider: TRtcDataProvider;
    ServerLink: TRtcDataServerLink;
    Unloader: TRtcDataProvider;

    procedure ProviderCheckRequest(Sender: TRtcConnection);
    procedure ProviderDisconnect(Sender: TRtcConnection);
    procedure ProviderListenStart(Sender: TRtcConnection);
    procedure ProviderListenStop(Sender: TRtcConnection);
    procedure ProviderDataReceived(Sender: TRtcConnection);

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure UnloaderCheckRequest(Sender: TRtcConnection);
    procedure UnloaderDataReceived(Sender: TRtcConnection);

  private
    { Private declarations }
    ExtList:TStringList;

  public
    procedure UnLoad;

    { Public declarations }
    procedure ClearExts;
    procedure AddExts(a:string);
  end;

function GetISAPIProvider:TISAPI_Provider;

implementation

{$R *.dfm}

var
  ISAPI_Provider: TISAPI_Provider;

function GetISAPIProvider:TISAPI_Provider;
  begin
  if not assigned(ISAPI_Provider) then
    ISAPI_Provider:=TISAPI_Provider.Create(nil);
  Result:=ISAPI_Provider;
  end;

procedure TISAPI_Provider.ClearExts;
  begin
  ExtList.Clear;
  end;

procedure TISAPI_Provider.AddExts(a: string);
  var
    elist,ext:string;
  begin
  elist:=Trim(a);
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
    ExtList.Add(ext);
    end;
  end;

procedure TISAPI_Provider.ProviderListenStart(Sender: TRtcConnection);
  begin
  TRtcISAPI.StartUp;
  end;

procedure TISAPI_Provider.ProviderListenStop(Sender: TRtcConnection);
  begin
  TRtcISAPI.ShutDown;
  end;

procedure TISAPI_Provider.ProviderCheckRequest(Sender: TRtcConnection);
  var
    a,loc:integer;
    MyFileName,FName,Tmp:string;
    DocRoot:string;
  begin
  with TRtcDataServer(Sender) do
    begin
    { Check the request header when it started and accept it immediatelly,
      even if we won't be responding before the request body is received. }
    DocRoot:=GetDocRoot(Request.Host);
    if DocRoot<>'' then
      begin
      MyFileName:=RepairWebFileName(String(Request.FileName),DocRoot);
      if MyFileName<>'' then
        begin
        FName:=ExpandFileName(DocRoot+MyFileName);
        FName:=UpperCase(StringReplace(FName,'/','\',[rfreplaceall]));
        for a:=0 to ExtList.Count-1 do
          begin
          loc:=Pos(ExtList.Strings[a]+'\',FName);
          if loc>0 then
            begin
            Tmp:=Copy(FName,1,loc+length(ExtList.Strings[a])-1);
            if File_Exists(Tmp) then
              begin
              Accept;
              Request.Info['ROOT']:=DocRoot;
              Request.Info['DLL']:=Tmp;
              loc:=loc+length(MyFileName)-length(FName)+length(ExtList.Strings[a]);
              Tmp:=Copy(MyFileName,loc,length(MyFileName)-loc+1);
              Request.Info['PATH']:=Tmp;
              Request.FileName:=RtcString(Copy(MyFileName,1,loc-1));
              Break;
              end;
            end;

          if Copy(FName,length(FName)-length(ExtList.Strings[a])+1,
                  length(ExtList.Strings[a])) = ExtList.Strings[a] then
            begin
            if File_Exists(FName) then
              begin
              Accept; // Accept the request (we will be responding)
              Request.FileName:=RtcString(MyFileName);
              Request.Info['ROOT']:=DocRoot;
              Request.Info['DLL']:=FName;
              Request.Info['PATH']:='';
              Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

procedure TISAPI_Provider.ProviderDataReceived(Sender: TRtcConnection);
  begin
  TRtcISAPI.Execute(TRtcDataServer(Sender));
  end;

procedure TISAPI_Provider.ProviderDisconnect(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.DataSize>Request.DataIn then
      begin
      // did not receive a complete request
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+Int2Str(Request.DataIn)+' of '+Int2Str(Request.DataSize)+' bytes received).','ISAPI');
      end
    else if Response.DataSize>Response.DataOut then
      begin
      // did not send a complete result
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' -'+Int2Str(Response.DataSize-Response.DataOut)+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while sending a Result ('+Int2Str(Response.DataOut)+' of '+Int2Str(Response.DataSize)+' bytes sent).','ISAPI');
      end;
    end;
  end;

procedure TISAPI_Provider.DataModuleCreate(Sender: TObject);
  begin
  ExtList:=TStringList.Create;
  end;

procedure TISAPI_Provider.DataModuleDestroy(Sender: TObject);
  begin
  ISAPI_Provider:=nil;

  ClearExts;
  ExtList.Free;
  end;

procedure TISAPI_Provider.UnLoad;
  begin
  TRtcISAPI.UnLoadAll;
  end;

procedure TISAPI_Provider.UnloaderCheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/$ISAPI.UNLOAD' then
      Accept;
  end;

procedure TISAPI_Provider.UnloaderDataReceived(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      UnLoad;
      Write('<html>All ISAPI modules unloaded.</html>');
      end;
  end;

initialization
finalization
if assigned(ISAPI_Provider) then
  begin
  ISAPI_Provider.Free;
  ISAPI_Provider:=nil;
  end;
end.
