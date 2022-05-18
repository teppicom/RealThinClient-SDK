unit HTTP_Module_TLS;

{$INCLUDE defines.inc}

interface

uses
  SysUtils, Classes, IniFiles,

  Forms,

  rtcLog, rtcSystem,
  rtcInfo, rtcConn,
  rtcDataSrv, rtcHttpSrv,

  rtcForumProvider, rtcPlugins,

  MpX509, TlsInternalServer, SecUtils,
  StreamSecII, SecComp, rtcSSecPlugin;

type
  THTTPS_Server = class(TDataModule)
    Server: TRtcHttpServer;
    SimpleTLSInternalServer1: TSimpleTLSInternalServer;
    SsPrivateKeyRingComponent1: TSsPrivateKeyRingComponent;
    SSecServerPlugin: TRtcSSecServerPlugin;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerInvalidRequest(Sender: TRtcConnection);
    procedure ServerDisconnect(Sender: TRtcConnection);
    procedure ServerException(Sender: TRtcConnection; E: Exception);
    procedure SsPrivateKeyRingComponent1AdminPassword(Sender: TObject;
      Password: ISecretKey);
  private
    { Private declarations }
    FOnError: TRtcErrorEvent;
    FOnStart: TRtcNotifyEvent;
    FOnStop: TRtcNotifyEvent;
    FOnConnect: TRtcNotifyEvent;
    FOnDisconnect: TRtcNotifyEvent;

    CS : TRtcCritSec;
    CliCnt:integer;
    function GetClientCount: integer;

  public
    { Public declarations }

    procedure Start;
    procedure Stop;

    property ClientCount:integer read GetClientCount;

    property OnStart:TRtcNotifyEvent read FOnStart write FOnStart;
    property OnStop:TRtcNotifyEvent read FOnStop write FOnStop;
    property OnError:TRtcErrorEvent read FOnError write FOnError;
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

var
  HTTPS_Server: THTTPS_Server;

implementation

uses
  MPYarrow;

{$R *.dfm}

procedure THTTPS_Server.DataModuleCreate(Sender: TObject);
  begin
  CS := TRtcCritSec.Create;
  while not MPYarrow.YarrowHasReseeded do begin
    Sleep(100);
    Application.ProcessMessages;
  end;
  SimpleTLSInternalServer1.ImportFromPFX('Server.pfx',TSecretKey.CreateBMPStr('abc',3));
  if pkaRSA in SimpleTLSInternalServer1.PublicKeyAlgorithms then
    begin
    SimpleTLSInternalServer1.Options.SignatureRSA := prPrefer;
    SimpleTLSInternalServer1.Options.KeyAgreementRSA := prAllowed;
    SimpleTLSInternalServer1.Options.KeyAgreementDHE := prPrefer;
    SimpleTLSInternalServer1.TLSSetupServer;
    end;
  CliCnt := 0;
end;

procedure THTTPS_Server.DataModuleDestroy(Sender: TObject);
begin
  CS.Free;
end;

procedure THTTPS_Server.Start;
var
  I : integer;

  IniName : string;
  Ini : TMemIniFile;

  SL : TStringList;
begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  // Read configuration file ...
  XLog(Format('Read configuration file: "%s"',[IniName]));

  Ini := TMemIniFile.Create(IniName);
  try
    Server.ServerPort := Ini.ReadString('Server', 'ServerPort', '443');

    with GetForumProvider do
      begin
      Init(Ini.ReadString('Forum','Host',''),
           Ini.ReadString('Forum','URI','/'),
           Ini.ReadString('Forum','Path','RtcForumData'));

      ClearContentTypes;
      SL := TStringList.Create;
      try
        Ini.ReadSectionValues('Content Types', SL);
        for I := 0 to SL.Count - 1 do
          AddContentType(SL[I]);
      finally
        SL.Free;
        end;
      end;
  finally
    Ini.Free;
    end;

  // Assign our Server to Data Provider
  GetForumProvider.ServerLink.Server := Server;

  // Start DataServer
  Server.Listen;
end;

procedure THTTPS_Server.Stop;
begin
  Server.StopListenNow;
end;

procedure THTTPS_Server.ServerListenError(Sender: TRtcConnection; E: Exception);
begin
  XLog('Error starting Web Server!'#13#10 + E.ClassName+'>'+E.Message);
  if assigned(OnError) then
    OnError(Sender,E);
end;

procedure THTTPS_Server.ServerListenStart(Sender: TRtcConnection);
begin
  XLog('SERVER STARTED ...');
  if assigned(OnStart) then
    OnStart(Sender);
end;

procedure THTTPS_Server.ServerListenStop(Sender: TRtcConnection);
begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER STOPPED.');
end;

procedure THTTPS_Server.ServerConnecting(Sender: TRtcConnection);
begin
  CS.Enter;
  try
    Inc(CliCnt);
    with Sender do
      XLog('++++ '+PeerAddr+':'+PeerPort+' ['+IntToStr(CliCnt)+' open]');
  finally
    CS.Leave;
    end;
  if assigned(OnConnect) then
    OnConnect(Sender);
end;

procedure THTTPS_Server.ServerDisconnecting(Sender: TRtcConnection);
begin
  CS.Enter;
  try
    Dec(CliCnt);
    with Sender do
      XLog('---- '+PeerAddr+':'+PeerPort+' ['+IntToStr(CliCnt)+' open]');
  finally
    CS.Leave;
    end;
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
end;

procedure THTTPS_Server.ServerRequestNotAccepted(Sender: TRtcConnection);
begin
  // Anything that comes this far is not acceptable by any DataProvider component.
  with TRtcDataServer(Sender) do
    begin
    XLog('BAD! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Method "'+Request.Method+'" not supported.');

    Response.StatusCode:=400;
    Response.StatusText:='Bad Request';
    Write;

    Disconnect;
    end;
end;

procedure THTTPS_Server.ServerInvalidRequest(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
    begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.StatusCode:=400;
    Response.StatusText:='Bad Request';
    Write;
    end;
end;

procedure THTTPS_Server.ServerDisconnect(Sender: TRtcConnection);
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

function THTTPS_Server.GetClientCount: integer;
begin
  CS.Enter;
  try
    Result:=CliCnt;
  finally
    CS.Leave;
  end;
end;

procedure THTTPS_Server.ServerException(Sender: TRtcConnection;
  E: Exception);
begin
  Log('Exception [srv]', E);
end;

procedure THTTPS_Server.SsPrivateKeyRingComponent1AdminPassword(
  Sender: TObject; Password: ISecretKey);
begin
  Password.SetKeyStr('abc');
end;

end.
