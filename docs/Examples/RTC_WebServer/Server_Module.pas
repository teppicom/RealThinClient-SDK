unit Server_Module;

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{ To compile the project with StreamSec Tools 2.1+ components using demo SSL certificates,
  declare the StreamSecII compiler directive below, in your project or in the "rtcDeploy.inc" file.

  When StreamSecII compiler directive is declared, the Server will be listening on 2 ports.
  Port 80 will be plain HTTP and Port 443 will be using HTTP over SSL (HTTPS). }

{.$DEFINE StreamSecII}

interface

uses
  SysUtils, Classes, IniFiles,

  rtcTypes, rtcSystem, rtcLog,
  rtcInfo, rtcConn, rtcThrPool,
  rtcDataSrv, rtcHttpSrv,

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

{$IFDEF WIN32}
//  rtcPHPProvider, {$DEFINE HAVE_PHP}
  rtcISAPIProvider, {$DEFINE HAVE_ISAPI}
{$ENDIF}

  rtcFileProvider,
  rtcMessengerProvider;

type
  TData_Server = class(TDataModule)
    ServerHTTP: TRtcHttpServer;
    ServerHTTPS: TRtcHttpServer;
    ServerLink: TRtcDualDataServerLink;
    ServerHTTP6: TRtcHttpServer;
    ServerHTTPS6: TRtcHttpServer;
    ServerLink_HTTP: TRtcDualDataServerLink;
    ServerLink_HTTPS: TRtcDualDataServerLink;

    procedure DataModuleCreate(Sender: TObject);

    procedure ServerHTTPListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerHTTPListenStart(Sender: TRtcConnection);
    procedure ServerHTTPListenStop(Sender: TRtcConnection);
    procedure ServerHTTPConnecting(Sender: TRtcConnection);
    procedure ServerHTTPDisconnecting(Sender: TRtcConnection);
    procedure ServerHTTPRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerHTTPInvalidRequest(Sender: TRtcConnection);
    procedure ServerHTTPDisconnect(Sender: TRtcConnection);
  private
    { Private declarations }
    FOnError: TRtcErrorEvent;
    FOnStart: TRtcNotifyEvent;
    FOnStop: TRtcNotifyEvent;
    FOnConnect: TRtcNotifyEvent;
    FOnDisconnect: TRtcNotifyEvent;

    function GetClientCount: integer;

  public
    { Public declarations }

    procedure UnloadIsapi;

    procedure Start(IPv4:boolean=True; IPv6:boolean=True);
    procedure Stop;

    property ClientCount:integer read GetClientCount;

    property OnStart:TRtcNotifyEvent read FOnStart write FOnStart;
    property OnStop:TRtcNotifyEvent read FOnStop write FOnStop;
    property OnError:TRtcErrorEvent read FOnError write FOnError;
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

function GetDataServer:TData_Server;

implementation

{$R *.dfm}

var
  Data_Server: TData_Server;

function GetDataServer:TData_Server;
  begin
  if not assigned(Data_Server) then
    Data_Server:=TData_Server.Create(nil);
  Result:=Data_Server;
  end;

procedure TData_Server.DataModuleCreate(Sender: TObject);
  begin
{$IFDEF StreamSecII}
  { In this demo, we will use a 2nd Server to listen
    on the HTTPS Port (443) in addition to the standard
    non-encrypted HTTP server which is listening on Port 80.
    If you only want your Server to work over SSL,
    you do NOT need 2 Server components. You can link the
    ServerCryptPlugin component to "ServerHTTP" (above).
    Also, if you only have a single Server listening on a single Port,
    you do NOT need a TRtcDualDataServerLink component. }

  AddServerRootCertFile(ExtractFilePath(AppFileName)+'root.cer'); // use our demo root certificate
  AddServerPFXFile(ExtractFilePath(AppFileName)+'server.pfx','abc'); // use our demo server PFX file

  ServerHTTPS.CryptPlugin:=GetServerCryptPlugin;
  ServerHTTPS6.CryptPlugin:=GetServerCryptPlugin;
{$ENDIF}
  end;

procedure TData_Server.Start(IPv4:boolean=True; IPv6:boolean=True);
  var
    a:integer;

    IniName:string;
    Ini:TCustomIniFile;
    SL:TStringList;

    {$IFDEF HAVE_PHP}web_usePHP,{$ENDIF}
    web_useMSG:boolean;

  begin
  if not ServerHTTP.isListening and
     not ServerHTTP6.isListening then
    begin
    IniName := ChangeFileExt(AppFileName, '.ini');
    if File_Exists(IniName) then
      begin
      XLog('Reading INI file "'+RtcString(IniName)+'"');

      Ini := TMemIniFile.Create(IniName);
      try
      {$IFDEF HAVE_PHP}
        web_UsePHP := (Ini.ReadString('PHP','Enable','') = '1');
        if web_UsePHP then
          begin
          with GetPHPProvider do
            begin
            DllFolder:= Ini.ReadString('PHP','DllFolder','');
            IniFolder := RtcString(Ini.ReadString('PHP','IniFolder',''));

            ClearExts;
            AddExts( Ini.ReadString('PHP','Extensions','') );
            end;
          end;
      {$ENDIF}

      {$IFDEF HAVE_ISAPI}
        with GetISAPIProvider do
          begin
          ClearExts;
          AddExts( Ini.ReadString('ISAPI','Extensions','') );
          end;
      {$ENDIF}

        web_UseMsg := (Ini.ReadString('Messenger','Enable', '') = '1');

        SL := TStringList.Create;
        try
          with GetFileProvider do
            begin
            ClearHosts;
            SL.Clear;
            Ini.ReadSectionValues('Hosts',SL);
            if SL.Count=0 then
              AddHost('*=www')
            else
              for a:=0 to SL.Count-1 do
                AddHost(SL[a]);

            ClearIndexPages;
            SL.Clear;
            Ini.ReadSectionValues('Index Pages',SL);
            if SL.Count=0 then
              AddIndexPage('index.htm')
            else
              for a:=0 to SL.Count-1 do
                AddIndexPage(SL[a]);

            ClearContentTypes;
            SL.Clear;
            Ini.ReadSectionValues('Content Types',SL);
            if SL.Count=0 then
              AddContentType('html,htm=text/html')
            else
              for a:=0 to SL.Count-1 do
                AddContentType(SL[a]);
            end;
        finally
          SL.Free;
          end;
      finally
        Ini.Free;
        end;
      end
    else
      begin
      XLog('No INI file "'+RtcString(IniName)+'". Using default settings.');

    {$IFDEF HAVE_PHP}
      web_UsePHP := False;
    {$ENDIF}
    {$IFDEF HAVE_ISAPI}
      with GetISAPIProvider do
        begin
        ClearExts;
        AddExts('dll');
        end;
      {$ENDIF}
      web_UseMsg := True;
      with GetFileProvider do
        begin
        ClearHosts;
        AddHost('*=www');
        ClearIndexPages;
        AddIndexPage('index.htm');
        ClearContentTypes;
        AddContentType('html,htm=text/html');
        end;
      end;

  {$IFDEF StreamSecII}
    // Assign our Server to Data Providers
    GetFileProvider.ServerLink.Link:=ServerLink;
    {$IFDEF HAVE_ISAPI}
      GetISAPIProvider.ServerLink.Link:=ServerLink;
    {$ENDIF}
    {$IFDEF HAVE_PHP}
      if web_usephp then
        GetPHPProvider.ServerLink.Link:=ServerLink;
    {$ENDIF}
    if web_usemsg then
      GetMessengerProvider.ServerLink.Link:=ServerLink;
    if web_usegate then
      GetGatewayProvider.ServerLink.Link:=ServerLink;
  {$ELSE}
    // Assign our Server to Data Providers
    GetFileProvider.ServerLink.Link:=ServerLink_HTTP;
    {$IFDEF HAVE_ISAPI}
      GetISAPIProvider.ServerLink.Link:=ServerLink_HTTP;
    {$ENDIF}
    {$IFDEF HAVE_PHP}
      if web_usephp then
        GetPHPProvider.ServerLink.Link:=ServerLink_HTTP;
    {$ENDIF}
    if web_usemsg then
      GetMessengerProvider.ServerLink.Link:=ServerLink_HTTP;
  {$ENDIF}
    end;

  // Start Server(s)
  if IPv6 then ServerHTTP6.Listen;
  if IPv4 then ServerHTTP.Listen;
{$IFDEF StreamSecII}
  if IPv6 then
    if assigned(ServerHTTPS6.CryptPlugin) then
      ServerHTTPS6.Listen;
  if IPv4 then
    if assigned(ServerHTTPS.CryptPlugin) then
      ServerHTTPS.Listen;
{$ENDIF}
  end;

procedure TData_Server.Stop;
  begin
  ServerHTTP.StopListenNow;
  ServerHTTP6.StopListenNow;
{$IFDEF StreamSecII}
  ServerHTTPS.StopListenNow;
  ServerHTTPS6.StopListenNow;
{$ENDIF}
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

procedure TData_Server.ServerHTTPListenError(Sender: TRtcConnection; E: Exception);
  var
    Srv:TRtcServer absolute Sender;
  begin
  XLog('Error starting Web Server '+SrvInfo(Sender)+'!'#13#10 + RtcString(E.ClassName+'>'+E.Message) );
  {$IFDEF CONSOLE}
    Writeln('Error starting Server '+SrvInfo(Sender)+':');
    Writeln(RtcString(E.ClassName+'>'+E.Message));
  {$ENDIF}
  if assigned(OnError) then
    OnError(Sender,E);
  end;

procedure TData_Server.ServerHTTPListenStart(Sender: TRtcConnection);
  var
    Srv:TRtcServer absolute Sender;
  begin
  XLog('SERVER '+SrvInfo(Sender)+' STARTED.');
  {$IFDEF CONSOLE}
    Writeln('Server '+SrvInfo(Sender)+' started.');
  {$ENDIF}
  if assigned(OnStart) then
    OnStart(Sender);
  end;

procedure TData_Server.ServerHTTPListenStop(Sender: TRtcConnection);
  begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER '+SrvInfo(Sender)+' STOPPED.');
  {$IFDEF CONSOLE}
    Writeln('Server '+SrvInfo(Sender)+' stopped.');
  {$ENDIF}
  end;

procedure TData_Server.ServerHTTPConnecting(Sender: TRtcConnection);
  begin
  with Sender do XLog('++++ '+PeerAddr+':'+PeerPort+' ['+Int2Str(Sender.TotalServerConnectionCount)+' open]');
  if assigned(OnConnect) then
    OnConnect(Sender);
  end;

procedure TData_Server.ServerHTTPDisconnecting(Sender: TRtcConnection);
  begin
  with Sender do XLog('---- '+PeerAddr+':'+PeerPort+' ['+Int2Str(Sender.TotalServerConnectionCount)+' open]');
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
  end;

procedure TData_Server.ServerHTTPRequestNotAccepted(Sender: TRtcConnection);
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

procedure TData_Server.ServerHTTPInvalidRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    end;
  end;

procedure TData_Server.ServerHTTPDisconnect(Sender: TRtcConnection);
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
           '> DISCONNECTED while receiving a Request ('+Int2Str(Request.DataIn)+' of '+Int2Str(Request.DataSize)+' bytes received).');
      end;
    end;
  end;

function TData_Server.GetClientCount: integer;
  begin
  Result:=rtcServerConnectionCount;
  end;

procedure TData_Server.UnloadIsapi;
  begin
{$IFDEF HAVE_ISAPI}
  GetISAPIProvider.UnLoad;
{$ENDIF}
  end;

initialization
finalization
if assigned(Data_Server) then
  begin
  Data_Server.Free;
  Data_Server:=nil;
  end;
end.
