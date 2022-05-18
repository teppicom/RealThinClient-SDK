unit HTTP_Module;

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{ To compile the project with StreamSec Tools 2.1+ components using demo SSL certificates,
  declare the StreamSecII compiler directive below, in your project or in the "rtcDeploy.inc" file.

  When StreamSecII compiler directive is declared, the Server will be listening on 2 ports.
  Port 80 will be plain HTTP and Port 443 will be using HTTP over SSL (HTTPS). }

{.$DEFINE StreamSecII}

interface

uses
  SysUtils, Classes,

  Forms,

  rtcTypes, rtcSystem, rtcLog,
  rtcInfo, rtcConn, rtcDataSrv, rtcHttpSrv,

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

  rtcFileProvider;

type
  THTTP_Server = class(TDataModule)
    ServerHTTP: TRtcHttpServer;
    ServerHTTPS: TRtcHttpServer;
    ServerLink: TRtcDualDataServerLink;

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

  public
    { Public declarations }

    procedure Start;
    procedure Stop;

    property OnStart:TRtcNotifyEvent read FOnStart write FOnStart;
    property OnStop:TRtcNotifyEvent read FOnStop write FOnStop;
    property OnError:TRtcErrorEvent read FOnError write FOnError;
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

var
  HTTP_Server: THTTP_Server;

implementation

{$R *.dfm}

procedure THTTP_Server.Start;
  var
    s:string;
    a,i:integer;
    f:TextFile;
  begin
  // Read configuration file ...
  XLog('Read LOG "'+RtcString(ExpandFileName(AppFileName))+'.cfg"');

  AssignFile(f,AppFileName+'.cfg');
  {$I-}
  Reset(f);
  {$I+}
  if IOResult=0 then
    begin
    try
      with GetFileProvider do
        begin
        ClearIndexPages;
        ReadLn(f, s); s:=Trim(s); i:=StrToIntDef(s,0);
        for a:=0 to i-1 do
          begin
          ReadLn(f, s);
          AddIndexPage(s);
          end;

        ClearHosts;
        ReadLn(f, s); s:=Trim(s); i:=StrToIntDef(s,0);
        for a:=0 to i-1 do
          begin
          ReadLn(f, s);
          AddHost(s);
          end;

        // next 3 lines are for ISAPI support
        for a:=1 to 3 do
          ReadLn(f, s);

        ClearContentTypes;
        ReadLn(f, s); s:=Trim(s); i:=StrToIntDef(s,0);
        for a:=0 to i-1 do
          begin
          ReadLn(f, s);
          AddContentType(s);
          end;
        end;
    except
      // ignore errors while reading the CFG file
      end;
    CloseFile(f);
    end;

  // Assign our Server to Data Providers
{$IFDEF StreamSecII}
  GetFileProvider.ServerLink.Link:=ServerLink;
{$ELSE}
  GetFileProvider.ServerLink.Server:=ServerHTTP;
{$ENDIF}

  // Start DataServer
  ServerHTTP.Listen;
{$IFDEF StreamSecII}

  { In this demo, we will use a 2nd Server to listen
    on the HTTPS Port (443) in addition to the standard
    non-encrypted HTTP server which is listening on Port 80.
    If you only want your Server to work over SSL,
    you do NOT need 2 Server components. You can link the
    ServerCryptPlugin component to "ServerHTTP" (above).
    Also, if you only have a single Server listening
    on a single Port, you do need a TRtcDualDataServerLink. }

  // specify which certificates to load.
  // these files need to be deployed with the EXE
  AddServerRootCertFile('root.cer');  // use our "demo" root certificate
  AddServerPFXFile('server.pfx','abc'); // use out "demo" server PFX file

  ServerHTTPS.CryptPlugin:=GetServerCryptPlugin;
  if assigned(ServerHTTPS.CryptPlugin) then
    ServerHTTPS.Listen;
{$ENDIF}
  end;

procedure THTTP_Server.Stop;
  begin
  ServerHTTP.StopListenNow;
{$IFDEF StreamSecII}
  ServerHTTPS.StopListenNow;
{$ENDIF}
  end;

procedure THTTP_Server.ServerHTTPListenError(Sender: TRtcConnection; E: Exception);
  begin
  XLog('Error starting Web Server!'#13#10 + RtcString(E.ClassName+'>'+E.Message));
  if assigned(OnError) then
    OnError(Sender,E);
  end;

procedure THTTP_Server.ServerHTTPListenStart(Sender: TRtcConnection);
  begin
  XLog('SERVER STARTED on Port '+Sender.LocalPort+' ...');
  if assigned(OnStart) then
    OnStart(Sender);
  end;

procedure THTTP_Server.ServerHTTPListenStop(Sender: TRtcConnection);
  begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER STOPPED.');
  end;

procedure THTTP_Server.ServerHTTPConnecting(Sender: TRtcConnection);
  begin
  with Sender do
    XLog('++++ '+PeerAddr+':'+PeerPort+' ['+Int2Str(TotalClientConnectionCount)+' open]');

  if assigned(OnConnect) then
    OnConnect(Sender);
  end;

procedure THTTP_Server.ServerHTTPDisconnecting(Sender: TRtcConnection);
  begin
  with Sender do
    XLog('---- '+PeerAddr+':'+PeerPort+' ['+Int2Str(TotalClientConnectionCount)+' open]');
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
  end;

procedure THTTP_Server.ServerHTTPRequestNotAccepted(Sender: TRtcConnection);
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

procedure THTTP_Server.ServerHTTPInvalidRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    end;
  end;

procedure THTTP_Server.ServerHTTPDisconnect(Sender: TRtcConnection);
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

end.
