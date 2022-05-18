unit Server_Module;

interface

uses
  SysUtils, Classes, IniFiles,

  Forms,

  rtcTypes, rtcLog, rtcSystem,
  rtcInfo, rtcConn, rtcThrPool,
  rtcDataSrv, rtcHttpSrv,

  rtcFileProvider,
  rtcISAPIProvider,
  rtcMessengerProvider,
  rtcForumProvider;

type
  TData_Server = class(TDataModule)
    Server: TRtcHttpServer;
    Server6: TRtcHttpServer;
    ServerLink: TRtcDualDataServerLink;
    procedure DataModuleDestroy(Sender: TObject);

    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerInvalidRequest(Sender: TRtcConnection);
    procedure ServerDisconnect(Sender: TRtcConnection);
  private
    { Private declarations }
    FOnError: TRtcErrorEvent;
    FOnStart: TRtcNotifyEvent;
    FOnStop: TRtcNotifyEvent;
    FOnConnect: TRtcNotifyEvent;
    FOnDisconnect: TRtcNotifyEvent;

  public
    { Public declarations }

    procedure UnloadIsapi;

    procedure Start;
    procedure Stop;

    property OnStart:TRtcNotifyEvent read FOnStart write FOnStart;
    property OnStop:TRtcNotifyEvent read FOnStop write FOnStop;
    property OnError:TRtcErrorEvent read FOnError write FOnError;
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

var
  Data_Server: TData_Server;

implementation

{$R *.dfm}

procedure TData_Server.DataModuleDestroy(Sender: TObject);
  begin
  Server.StopListenNow;
  Server6.StopListenNow;
  end;

procedure TData_Server.Start;
  var
    a:integer;

    IniName:string;
    Ini:TCustomIniFile;
    SL:TStringList;

    web_useMSG,
    web_useForum:boolean;

  begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  XLog('Read LOG "'+IniName+'"');

  SL := TStringList.Create;
  try
    Ini := TIniFile.Create(IniName);
    try
      with GetISAPIProvider do
        begin
        ClearExts;
        AddExts( Ini.ReadString('ISAPI','Extensions','') );
        end;

      web_UseMsg := (Ini.ReadString('Messenger','Enable', '') = '1');

      web_UseForum := (Ini.ReadString('Forum','Enable', '') = '1');
      if web_UseForum then
        begin
        with GetForumProvider do
          begin
          ClearContentTypes;
          Init(Ini.ReadString('Forum','Host',''),
               Ini.ReadString('Forum','URI',''),
               Ini.ReadString('Forum','Path',''));
          end;
        end;
    finally
      Ini.Free;
      end;

    Ini := TMemIniFile.Create(IniName);
    try
      with GetFileProvider do
        begin
        ClearHosts;
        SL.Clear;
        Ini.ReadSectionValues('Hosts',SL);
        for a:=0 to SL.Count-1 do
          AddHost(SL[a]);

        ClearIndexPages;
        SL.Clear;
        Ini.ReadSectionValues('Index Pages',SL);
        for a:=0 to SL.Count-1 do
          AddIndexPage(SL[a]);

        ClearContentTypes;
        SL.Clear;
        Ini.ReadSectionValues('Content Types',SL);
        for a:=0 to SL.Count-1 do
          AddContentType(SL[a]);
        end;

      if web_UseForum then
        begin
        with GetForumProvider do
          begin
          SL.Clear;
          Ini.ReadSectionValues('Content Types',SL);
          for a:=0 to SL.Count-1 do
            AddContentType(SL[a]);
          end;
        end;
    finally
      Ini.Free;
      end;
  finally
    SL.Free;
    end;

  // Assign our Server to Data Providers
  GetFileProvider.ServerLink.Link:=ServerLink;
  GetISAPIProvider.ServerLink.Link:=ServerLink;
  if web_usemsg then
    GetMessengerProvider.ServerLink.Link:=ServerLink;
  if web_useForum then
    GetForumProvider.ServerLink.Link:=ServerLink;

  // Start DataServer
  Server.Listen;
  Server6.Listen;
  end;

procedure TData_Server.Stop;
  begin
  Server.StopListenNow;
  Server6.StopListenNow;
  end;

function SrvInfo(Sender:TRtcConnection):String;
  var
    Srv:TRtcServer absolute Sender;
  begin
  if Srv.ServerIPV>=rtc_IPv6 then
    Result:='IPv6@'+Srv.ServerPort
  else
    Result:='IPv4@'+Srv.ServerPort;
  end;

procedure TData_Server.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  XLog('Error starting Web Server '+SrvInfo(Sender)+'!'#13#10 + E.ClassName+'>'+E.Message);
  if assigned(OnError) then
    OnError(Sender,E);
  end;

procedure TData_Server.ServerListenStart(Sender: TRtcConnection);
  begin
  XLog('SERVER '+SrvInfo(Sender)+' STARTED ...');
  if assigned(OnStart) then
    OnStart(Sender);
  end;

procedure TData_Server.ServerListenStop(Sender: TRtcConnection);
  begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER '+SrvInfo(Sender)+' STOPPED.');
  end;

procedure TData_Server.ServerConnecting(Sender: TRtcConnection);
  begin
  with Sender do
    XLog('++++ '+PeerAddr+':'+PeerPort+' ['+IntToStr(TotalClientConnectionCount)+' open]');

  if assigned(OnConnect) then
    OnConnect(Sender);
  end;

procedure TData_Server.ServerDisconnecting(Sender: TRtcConnection);
  begin
  with Sender do
    XLog('---- '+PeerAddr+':'+PeerPort+' ['+IntToStr(TotalClientConnectionCount)+' open]');

  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
  end;

procedure TData_Server.ServerRequestNotAccepted(Sender: TRtcConnection);
  begin
  // Anything that comes this far is not acceptable by any DataProvider component.
  with TRtcDataServer(Sender) do
    begin
    XLog('BAD! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Method "'+Request.Method+'" not supported.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    
    Disconnect;
    end;
  end;

procedure TData_Server.ServerInvalidRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    end;
  end;

procedure TData_Server.ServerDisconnect(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.DataSize > Request.DataIn then
      begin
      // did not receive a complete request
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+IntToStr(Request.DataIn)+' of '+IntToStr(Request.DataSize)+' bytes received).');
      end;
    end;
  end;

procedure TData_Server.UnloadIsapi;
  begin
  GetISAPIProvider.UnLoad;
  end;

end.
