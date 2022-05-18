unit HTTP_Module;

{$INCLUDE defines.inc}

interface

uses
  SysUtils, Classes, IniFiles,

  Forms,

  rtcSystem, rtcLog, 
  rtcInfo, rtcConn,
  rtcDataSrv, rtcHttpSrv,

  rtcForumProvider;

type
  THTTP_Server = class(TDataModule)
    Server: TRtcHttpServer;

    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerInvalidRequest(Sender: TRtcConnection);
    procedure ServerDisconnect(Sender: TRtcConnection);
    procedure ServerException(Sender: TRtcConnection; E: Exception);
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
  I : integer;

  IniName : string;
  Ini : TCustomIniFile;

  SL : TStringList;
begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  // Read configuration file ...
  XLog(Format('Read configuration file: "%s"',[IniName]));

  Ini := TIniFile.Create(IniName);
  try
    Server.ServerPort := Ini.ReadString('Server', 'ServerPort', '80');

    with GetForumProvider do
      begin
      ClearContentTypes;
      Init(Ini.ReadString('Forum','Host',''),
           Ini.ReadString('Forum','URI','/'),
           Ini.ReadString('Forum','Path','RtcForumData'));
      end;
  finally
    Ini.Free;
    end;

  Ini := TMemIniFile.Create(IniName);
  try
    SL := TStringList.Create;
    try
      with GetForumProvider do
        begin
        Ini.ReadSectionValues('Content Types', SL);
        for I := 0 to SL.Count - 1 do
          AddContentType(SL[I]);
        end;
    finally
      SL.Free;
      end;
  finally
    Ini.Free;
    end;

  // Assign our Server to Data Provider
  GetForumProvider.ServerLink.Server := Server;

  // Start DataServer
  Server.Listen;
end;

procedure THTTP_Server.Stop;
begin
  Server.StopListenNow;
end;

procedure THTTP_Server.ServerListenError(Sender: TRtcConnection; E: Exception);
begin
  XLog('Error starting Web Server!'#13#10 + E.ClassName+'>'+E.Message);
  if assigned(OnError) then
    OnError(Sender,E);
end;

procedure THTTP_Server.ServerListenStart(Sender: TRtcConnection);
begin
  XLog('SERVER STARTED ...');
  if assigned(OnStart) then
    OnStart(Sender);
end;

procedure THTTP_Server.ServerListenStop(Sender: TRtcConnection);
begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER STOPPED.');
end;

procedure THTTP_Server.ServerConnecting(Sender: TRtcConnection);
begin
  with Sender do
    XLog('++++ '+PeerAddr+':'+PeerPort+' ['+IntToStr(TotalClientConnectionCount)+' open]');

  if assigned(OnConnect) then
    OnConnect(Sender);
end;

procedure THTTP_Server.ServerDisconnecting(Sender: TRtcConnection);
begin
  with Sender do
    XLog('---- '+PeerAddr+':'+PeerPort+' ['+IntToStr(TotalConnectionCount)+' open]');

  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
end;

procedure THTTP_Server.ServerRequestNotAccepted(Sender: TRtcConnection);
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

procedure THTTP_Server.ServerInvalidRequest(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
    begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    end;
end;

procedure THTTP_Server.ServerDisconnect(Sender: TRtcConnection);
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

procedure THTTP_Server.ServerException(Sender: TRtcConnection;
  E: Exception);
begin
  Log('Exception [srv]', E);
end;

end.
