unit Server_ModuleTLS;

interface

uses
  SysUtils, Classes, IniFiles,

  Forms,

  rtcLog, rtcSystem,
  rtcInfo, rtcConn, rtcThrPool,
  rtcDataSrv, rtcHttpSrv,

  rtcFileProvider,
  rtcPHPProvider,
  rtcISAPIProvider,
  rtcMessengerProvider,
  rtcForumProvider, rtcPlugins,

  MpX509, TlsInternalServer, SecUtils,
  StreamSecII, SecComp, rtcSSecPlugin;

type
  TData_Server = class(TDataModule)
    ServerTLS: TRtcHttpServer;
    SimpleTLSInternalServer1: TSimpleTLSInternalServer;
    SsPrivateKeyRingComponent1: TSsPrivateKeyRingComponent;
    SSecServerPlugin: TRtcSSecServerPlugin;
    Server: TRtcHttpServer;
    ServerLink: TRtcDualDataServerLink;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure ServerTLSListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerTLSListenStart(Sender: TRtcConnection);
    procedure ServerTLSListenStop(Sender: TRtcConnection);
    procedure ServerTLSConnecting(Sender: TRtcConnection);
    procedure ServerTLSDisconnecting(Sender: TRtcConnection);
    procedure ServerTLSRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerTLSInvalidRequest(Sender: TRtcConnection);
    procedure ServerTLSDisconnect(Sender: TRtcConnection);
    procedure SsPrivateKeyRingComponent1AdminPassword(Sender: TObject; Password: ISecretKey);
  private
    { Private declarations }
    FOnError: TRtcErrorEvent;
    FOnStart: TRtcNotifyEvent;
    FOnStop: TRtcNotifyEvent;
    FOnConnect: TRtcNotifyEvent;
    FOnDisconnect: TRtcNotifyEvent;

    CS:TRtcCritSec;
    CliCnt:integer;
    SrvCnt:integer;
    function GetClientCount: integer;

  public
    { Public declarations }

    procedure UnloadIsapi;

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
  Data_Server: TData_Server;

implementation

uses
  MPYarrow;

{$R *.dfm}

procedure TData_Server.DataModuleCreate(Sender: TObject);
  var
    PW: iSecretKey;
  begin
  CS := TRtcCritSec.Create;

  PW := TSecretKey.CreateBMPStr('abc',3);
  while not MPYarrow.YarrowHasReseeded do begin
    Sleep(100);
    Application.ProcessMessages;
  end;
  SimpleTLSInternalServer1.ImportFromPFX(ExtractFilePath(AppFileName)+'Server.pfx',PW);
  if pkaRSA in SimpleTLSInternalServer1.PublicKeyAlgorithms then
    begin
    SimpleTLSInternalServer1.Options.SignatureRSA := prPrefer;
    SimpleTLSInternalServer1.Options.KeyAgreementRSA := prAllowed;
    SimpleTLSInternalServer1.Options.KeyAgreementDHE := prPrefer;
    SimpleTLSInternalServer1.TLSSetupServer;
    end;

  CliCnt:=0;
  SrvCnt:=0;
  end;

procedure TData_Server.DataModuleDestroy(Sender: TObject);
  begin
  CS.Free;
  end;

procedure TData_Server.Start;
  var
    a:integer;

    IniName:string;
    Ini:TCustomIniFile;
    SL:TStringList;

    web_usePHP,
    web_useMSG,
    web_useForum:boolean;

  begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  XLog('Read LOG "'+IniName+'"');

  SL := TStringList.Create;
  try
    Ini := TIniFile.Create(IniName);
    try
      web_UsePHP := (Ini.ReadString('PHP','Enable','') = '1');

      if web_UsePHP then
        begin
        with GetPHPProvider do
          begin
          DllFolder:= Ini.ReadString('PHP','DllFolder','');
          IniFolder := Ini.ReadString('PHP','IniFolder','');

          ClearExts;
          AddExts( Ini.ReadString('PHP','Extensions','') );
          end;
        end;

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
  if web_usephp then
    GetPHPProvider.ServerLink.Link:=ServerLink;
  if web_usemsg then
    GetMessengerProvider.ServerLink.Link:=ServerLink;
  if web_useForum then
    GetForumProvider.ServerLink.Link:=ServerLink;

  // Start Servers
  Server.Listen;
  ServerTLS.Listen;
  end;

procedure TData_Server.Stop;
  begin
  Server.StopListenNow;
  ServerTLS.StopListenNow;
  end;

procedure TData_Server.ServerTLSListenError(Sender: TRtcConnection; E: Exception);
  begin
  with TRtcDataServer(Sender) do
    begin
    XLog('> Error starting Web Server @ '+ServerPort+'!'#13#10 + E.ClassName+'>'+E.Message);
    if assigned(OnError) then
      OnError(Sender,E);
    end;
  end;

procedure TData_Server.ServerTLSListenStart(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    Inc(SrvCnt);
    if (SrvCnt=1) then
      begin
      XLog('> SERVER STARTED ...');
      if assigned(OnStart) then
        OnStart(Sender);
      end;
    end;
  end;

procedure TData_Server.ServerTLSListenStop(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    Dec(SrvCnt);
    if (SrvCnt=0) then
      begin
      if assigned(OnStop) then
        OnStop(Sender);
      XLog('> SERVER STOPPED.');
      end;
    end;
  end;

procedure TData_Server.ServerTLSConnecting(Sender: TRtcConnection);
  begin
  CS.Enter;
  try
    Inc(CliCnt);
    with Sender do
      XLog('++++ '+PeerAddr+':'+PeerPort+' @ '+ServerPort+' ['+IntToStr(CliCnt)+' open]');
  finally
    CS.Leave;
    end;
  if assigned(OnConnect) then
    OnConnect(Sender);
  end;

procedure TData_Server.ServerTLSDisconnecting(Sender: TRtcConnection);
  begin
  CS.Enter;
  try
    Dec(CliCnt);
    with Sender do
      XLog('---- '+PeerAddr+':'+PeerPort+' @ '+ServerPort+' ['+IntToStr(CliCnt)+' open]');
  finally
    CS.Leave;
    end;
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
  end;

procedure TData_Server.ServerTLSRequestNotAccepted(Sender: TRtcConnection);
  begin
  // Anything that comes this far is not acceptable by any DataProvider component.
  with TRtcDataServer(Sender) do
    begin
    XLog('> BAD! '+PeerAddr+':'+PeerPort+' @ '+ServerPort+' > "'+Request.Method+' '+Request.FileName+'" > Method "'+Request.Method+'" not supported.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');

    Disconnect;
    end;
  end;

procedure TData_Server.ServerTLSInvalidRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    XLog('> ERR! '+PeerAddr+':'+PeerPort+' @ '+ServerPort+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    end;
  end;

procedure TData_Server.ServerTLSDisconnect(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.DataSize > Request.DataIn then
      begin
      // did not receive a complete request
      XLog('> ERR! '+PeerAddr+':'+PeerPort+' @ '+ServerPort+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+IntToStr(Request.DataIn)+' of '+IntToStr(Request.DataSize)+' bytes received).');
      end;
    end;
  end;

function TData_Server.GetClientCount: integer;
  begin
  CS.Enter;
  try
    Result:=CliCnt;
  finally
    CS.Leave;
    end;
  end;

procedure TData_Server.UnloadIsapi;
  begin
  GetISAPIProvider.UnLoad;
  end;

procedure TData_Server.SsPrivateKeyRingComponent1AdminPassword(
    Sender: TObject; Password: ISecretKey);
  begin
  Password.SetKeyStr('abc');
  end;

end.
