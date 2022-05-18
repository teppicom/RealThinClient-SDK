unit MainModule;

interface

{$include rtcDefs.inc}

uses
  SysUtils, Classes,

  rtcTypes, rtcSystem,
  rtcLog, rtcInfo, rtcConn,
  rtcGateCli, rtcGateSrv,
  rtcGateConst, rtcDataSrv, rtcDataCli;

type
  TRTCPModule = class(TDataModule)
    MultiClient: TRtcHttpMultiGateClient;
    Gateway: TRtcHttpGateway;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure GatewayBeforeUserLogin(Sender: TRtcConnection; UserID: Cardinal; var UserAuth, UserInfo, SecondaryKey: String);
    procedure MultiClientStreamReset(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  private
    { Private declarations }
    NeedProviderChange:boolean;
    procedure LoadConfig(const data:RtcString);
    function SaveConfig:RtcString;
  public
    { Public declarations }
    procedure ChangeProvider;
    property Config:RtcString read SaveConfig write LoadConfig;
  end;

function RTCPModule:TRTCPModule;

implementation

{$R *.dfm}

var
  Module:TRTCPModule=nil;

function RTCPModule:TRTCPModule;
  begin
  if Module=nil then Module:=TRTCPModule.Create(nil);
  Result:=Module;
  end;

procedure TRTCPModule.DataModuleCreate(Sender: TObject);
  begin
  NeedProviderChange:=False;
  end;

procedure TRTCPModule.DataModuleDestroy(Sender: TObject);
  begin
  MultiClient.LogOutAndCleanUp;
  Gateway.Active:=False;
  end;

function TRTCPModule.SaveConfig:RtcString;
  var
    cli:TRtcHttpGateClient;
    c:TRtcDataSet;
    g:TRtcRecord;
    rec:TRtcRecord;
  begin
  c:=nil;
  rec:=TRtcRecord.Create;
  try
    cli:=MultiClient.FirstClient(True);
    while assigned(cli) do
      begin
      try
        if cli.AutoLogin then
          begin
          if c=nil then
            c:=rec.NewDataSet('Clients');
          c.Append;
          c.asString['Addr']:=cli.GateAddr;
          c.asString['Port']:=cli.GatePort;
          c.asString['FileName']:=cli.GateFileName;
          c.asBoolean['Blocking']:=cli.UseBlocking;
          c.asBoolean['Proxy']:=cli.UseProxy;
          c.asBoolean['WinHTTP']:=cli.UseWinHTTP;
          c.asCardinal['StreamIN']:=cli.StreamBlockSizeIn;
          c.asCardinal['StreamOUT']:=cli.StreamBlockSizeOut;
          end;
      finally
        MultiClient.UnLockClient(cli);
        end;
      cli:=MultiClient.NextClient(cli,True);
      end;
    g:=rec.NewRecord('Gateway');
    g.asString['Port']:=Gateway.GatePort;
    g.asBoolean['Active']:=Gateway.Active;
    rec.asByteArray['Accounts']:=MultiClient.SaveToCode;
    Result:=rec.toCode;
  finally
    rec.Free;
    end;
  end;

procedure TRTCPModule.LoadConfig(const data:RtcString);
  var
    c:TRtcDataSet;
    g:TRtcRecord;
    rec:TRtcRecord;
  begin
  if data='' then Exit;
  rec:=TRtcRecord.FromCode(data);
  try
    if not Gateway.Active then
      if rec.CheckType('Gateway',rtc_Record) then
        begin
        g:=rec.asRecord['Gateway'];
        if g.CheckType('Port',rtc_String) then
          Gateway.GatePort:=g.asString['Port'];
        if g.CheckType('Active',rtc_Boolean) then
          if g.asBoolean['Active'] then
            Gateway.Active:=True;
        end;
    if rec.CheckType('Clients',rtc_DataSet) then
      begin
      c:=rec.asDataSet['Clients'];
      c.First;
      while not c.Eof do
        begin
        if c.CheckType('Addr',rtc_String) and c.CheckType('Port',rtc_String) then
          begin
          MultiClient.SelectClient(c.AsString['Addr'],c.asString['Port']);
          if c.CheckType('FileName',rtc_String) then
            MultiClient.GateFileName:=c.asString['FileName']
          else
            MultiClient.GateFileName:='';
          if c.CheckType('Blocking',rtc_Boolean) then
            MultiClient.UseBlocking:=c.asBoolean['Blocking']
          else
            MultiClient.UseBlocking:=False;
          if c.CheckType('Proxy',rtc_Boolean) then
            MultiClient.UseProxy:=c.asBoolean['Proxy']
          else
            MultiClient.UseProxy:=False;
          if c.CheckType('WinHTTP',rtc_Boolean) then
            MultiClient.UseWinHTTP:=c.asBoolean['WinHTTP']
          else
            MultiClient.UseWinHTTP:=False;
          if c.CheckType('StreamIN',rtc_Cardinal) then
            MultiClient.StreamBlockSizeIn:=c.asCardinal['StreamIN']
          else
            MultiClient.StreamBlockSizeIn:=0;
          if c.CheckType('StreamOUT',rtc_Cardinal) then
            MultiClient.StreamBlockSizeOut:=c.asCardinal['StreamOUT']
          else
            MultiClient.StreamBlockSizeOut:=0;
          MultiClient.AutoLogin:=True;
          end;
        c.Next;
        end;
      end;
    if rec.CheckType('Accounts',rtc_ByteArray) then
      MultiClient.RegisterFromCode(rec.asByteArray['Accounts'],True,True);
  finally
    rec.Free;
    end;
  end;

procedure TRTCPModule.ChangeProvider;
  begin
  if MultiClient.AutoLogin then
    begin
    NeedProviderChange:=True;
    MultiClient.Client.ResetStreams;
    end;
  end;

procedure TRTCPModule.GatewayBeforeUserLogin(Sender: TRtcConnection; UserID: Cardinal; var UserAuth, UserInfo, SecondaryKey: String);
  begin
  if UserInfo<>'' then
    UserInfo:=UserInfo+' ('+Sender.PeerAddr+')';
  end;

procedure TRTCPModule.MultiClientStreamReset(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  if NeedProviderChange and (Client=MultiClient.Client) then
    begin
    NeedProviderChange:=False;
    if Client.StreamBlockSizeOut=0 then
      Client.StreamBlockSizeOut:=30 // 5 PINGs without data
    else if Client.StreamBlockSizeIn=0 then
      Client.StreamBlockSizeIn:=30 // 5 PINGs without data
    else
      begin
      Client.StreamBlockSizeOut:=0;
      Client.StreamBlockSizeIn:=0;
      if Client.UseWinHTTP then // WinHTTP -> async WinSock
        begin
        Client.UseBlocking:=False;
        Client.UseProxy:=False;
        Client.UseWinHTTP:=False;
        end
      else if Client.UseProxy then // WinInet -> WinHTTP
        Client.UseWinHTTP:=True
      else if Client.UseBlocking then // blocking WinSock -> WinInet
        Client.UseProxy:=True
      else // async WinSock -> blocking WinSock
        Client.UseBlocking:=True;
      end;
    end;
  end;

initialization
finalization
RtcFreeAndNil(Module);
end.
