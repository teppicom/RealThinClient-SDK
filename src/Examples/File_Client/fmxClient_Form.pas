unit fmxClient_Form;

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{ To compile the project with StreamSec Tools 2.1+ components using demo SSL certificates,
  declare the StreamSecII compiler directive below or in the "rtcDeploy.inc" file. }

{.$DEFINE StreamSecII}

interface

uses
  SysUtils, Classes,

  FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.ExtCtrls,
  FMX.Layouts, FMX.Memo, FMX.Edit,
  {$IFDEF IDE_XE3up} FMX.StdCtrls, {$ENDIF}
  {$IFDEF IDE_XE8up} FMX.ScrollBox, FMX.Controls.Presentation, {$ENDIF}

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

  rtcSystem, rtcLog, rtcInfo,
  rtcConn, rtcPlugins,
  rtcHttpCli, rtcDataCli;

type
  TRtcFileClient = class(TForm)
    Client: TRtcHttpClient;
    DataRequest: TRtcDataRequest;
    SaveDialog1: TSaveDialog;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label9: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Label12: TLabel;
    lblDataInOut: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    lblStatus: TLabel;
    Label5: TLabel;
    eReqMethod: TEdit;
    eReqFileName: TEdit;
    eReqQuery: TEdit;
    xSaveToFile: TCheckBox;
    eFileName: TEdit;
    btnSaveAs: TButton;
    eReqHost: TEdit;
    ePort: TEdit;
    eAddr: TEdit;
    xUseProxy: TCheckBox;
    xThreads: TCheckBox;
    xUseSSL: TCheckBox;
    xAutoConnect: TCheckBox;
    xWinHTTP: TCheckBox;
    eProxyAddr: TEdit;
    eProxyUsername: TEdit;
    eProxyPassword: TEdit;
    xBlocking: TCheckBox;
    xCryptPlugin: TCheckBox;
    xTrustServer: TCheckBox;
    xAllowExpired: TCheckBox;
    xUseHttp10: TCheckBox;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnPost: TButton;
    btn100Post: TButton;
    btnCancelAll: TButton;
    xReconError: TCheckBox;
    xReconFail: TCheckBox;
    xReconLost: TCheckBox;
    xShowWarning: TCheckBox;
    Panel6: TPanel;
    Splitter2: TSplitter;
    Panel7: TPanel;
    Panel9: TPanel;
    Label4: TLabel;
    lblCount: TLabel;
    lblRetry: TLabel;
    Panel10: TPanel;
    eResponseHeader: TMemo;
    Panel8: TPanel;
    Panel11: TPanel;
    lblTime: TLabel;
    Label6: TLabel;
    lblSpeed: TLabel;
    lblBytes: TLabel;
    pBar: TProgressBar;
    Panel12: TPanel;
    eResponseBody: TMemo;
    procedure btnPostClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure ClientConnect(Sender: TRtcConnection);
    procedure ClientDisconnect(Sender: TRtcConnection);
    procedure DataRequestBeginRequest(Sender: TRtcConnection);
    procedure DataRequestDataReceived(Sender: TRtcConnection);
    procedure DataRequestRepostCheck(Sender: TRtcConnection);
    procedure btn100PostClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelAllClick(Sender: TObject);
    procedure xUseProxyClick(Sender: TObject);
    procedure ClientConnectFail(Sender: TRtcConnection);
    procedure xThreadsClick(Sender: TObject);
    procedure eAddrChange(Sender: TObject);
    procedure xUseSSLClick(Sender: TObject);
    procedure ePortChange(Sender: TObject);
    procedure xAutoConnectClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure eReqFileNameChange(Sender: TObject);
    procedure xReconLostClick(Sender: TObject);
    procedure xReconFailClick(Sender: TObject);
    procedure xReconErrorClick(Sender: TObject);
    procedure ClientDataIn(Sender: TRtcConnection);
    procedure ClientDataOut(Sender: TRtcConnection);
    procedure DataRequestResponseAbort(Sender: TRtcConnection);
    procedure xWinHTTPClick(Sender: TObject);
    procedure eProxyAddrChange(Sender: TObject);
    procedure eProxyUsernameChange(Sender: TObject);
    procedure eProxyPasswordChange(Sender: TObject);
    procedure xBlockingClick(Sender: TObject);
    procedure xCryptPluginClick(Sender: TObject);
    procedure xTrustServerClick(Sender: TObject);
    procedure xAllowExpiredClick(Sender: TObject);
  private
    { Private declarations }
  public
    RtcCryptPlugin:TRtcCryptPlugin;

    Cnt:integer;
    TotalDataIn,TotalDataOut:int64;
    TimeStart,TimeNow,TimeCnt,TimeSum,
    TimeMin,TimeMax:cardinal;
    TimeAvg:Extended;
    Ret:integer;
    { Public declarations }
  end;

var
  RtcFileClient: TRtcFileClient;

implementation

{$R *.fmx}

procedure TRtcFileClient.xTrustServerClick(Sender: TObject);
  begin
{$IFDEF StreamSecII}
  { This is a simple DEMO, so we do not care if the Servers we are connecting to have valid certificates.
    To make this possible, we will use a HACK allowing us to work with ANY Server, regardless of its certificate: }
  BeGullableAndTrustAnythingSentToYou(xTrustServer.Checked);
{$ENDIF}
  end;

procedure TRtcFileClient.xAllowExpiredClick(Sender: TObject);
  begin
{$IFDEF StreamSecII}
  { We will also allow our client to accept expired certificates: }
  AllowExpiredCertificates(xAllowExpired.Checked);
{$ENDIF}
  end;

procedure TRtcFileClient.FormCreate(Sender: TObject);
  begin
  StartLog;
{$IFDEF StreamSecII}
  { We want to ignore Host names inside certificates and simply accept anything we get: }
  AddCertificateNameMap('*','*'); // comment this out to work only with Servers using valid certificates

  if File_Exists('root.cer') then // we have a root certificate?
    begin
    // Load our demo certificates ...
    AddClientPFXFile('client.pfx','abc'); // our "demo" client PFX file
    AddClientRootCertFile('root.cer'); // our "demo" root certificate

    { Need to have "NO_DISABLEMD5SIGN" compiler directive declared
      if we want to be able to load root certificates from "p7b" files.
      If this compiler directive is NOT declared for this project,
      windows root certificates will NOT be loaded and we will be
      getting FATAL errors for certificates unless we call:
      BeGullibleAndTrustAnythingSentToYou(TRUE); }
    {$IFDEF NO_DISABLEMD5SIGN}
    AddClientRootCertFile('rootX.p7b'); // Standard Windows Root certificates (exported from IE)
    AddClientRootCertFile('rootY.p7b'); // Additional Windows certificates (exported from IE)
    {$ELSE}
    xTrustServer.Checked:=True;
    xTrustServerClick(xTrustServer);
    {$ENDIF}
    end
  else
    begin
    xTrustServer.Checked:=True;
    xTrustServerClick(xTrustServer);
    end;

  RtcCryptPlugin:=GetClientCryptPlugin;
  if assigned(RtcCryptPlugin) then
    begin
    xCryptPlugin.Caption:='CryptPlugin (use SSL)';
    xCryptPlugin.Font.Style:=[fsBold];
    end
  else
{$ENDIF}
    begin
    xTrustServer.Enabled:=False;
    xAllowExpired.Enabled:=False;
    RtcCryptPlugin:=TRtcDummyCryptPlugin.Create(self);
    end;

  TotalDataIn:=0;
  TotalDataOut:=0;

  TimeStart:=0;
  TimeCnt:=0;
  TimeSum:=0;
  TimeMin:=0;
  TimeMax:=0;

  Cnt:=0;
  Ret:=0;
  end;

procedure TRtcFileClient.btnPostClick(Sender: TObject);
  begin
  with DataRequest do
    begin
    with Request do
      begin
      Close:=xUseHttp10.isChecked;
      Method:=RtcString(eReqMethod.Text);
      FileName:='/'+RtcString(eReqFileName.Text);
      Query.Text:=RtcString(eReqQuery.Text);
      Host:=RtcString(eReqHost.Text);
      if xSaveToFile.isChecked then
        Info.asText['fname']:=eFileName.Text;
      end;
    Post;
    end;
  end;

procedure TRtcFileClient.btnConnectClick(Sender: TObject);
  begin
  Client.Connect;
  end;

procedure TRtcFileClient.btnDisconnectClick(Sender: TObject);
  begin
  Client.Disconnect;
  end;

procedure TRtcFileClient.ClientConnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ClientConnect)
  else
    begin
    btnConnect.Enabled:=False;
    btnDisconnect.Enabled:=True;
    lblStatus.Text:='Connected to '+Sender.sPeerAddr+' on '+Sender.sPeerPort;
    end;
  end;

procedure TRtcFileClient.ClientDisconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ClientDisconnect)
  else
    begin
    btnConnect.Enabled:=True;
    btnDisconnect.Enabled:=False;
    lblStatus.Text:='Not connected.';
    end;
  end;

procedure TRtcFileClient.DataRequestBeginRequest(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    begin
    TimeStart:=GetTickTime;
    WriteHeader;
    // No Body.
    end;
  end;

procedure TRtcFileClient.DataRequestDataReceived(Sender: TRtcConnection);
  var
    s:RtcString;
  begin
  with TRtcDataClient(Sender) do
    if not inMainThread then
      Sync(DataRequestDataReceived)
    else
      begin
      TimeNow:=GetTickTime-TimeStart;
      if TimeNow>1000 then
        lblSpeed.Text:=IntToStr(Response.ContentIn*8 div TimeNow)+'Kbit/s'
      else
        lblSpeed.Text:='???';

      if Response.Started then
        begin
        eResponseHeader.Text:=String('Status: '+Int2Str(Response.StatusCode)+' '+
                              Response.StatusText+#13#10+
                              Response.HeaderText);
        eResponseBody.Text:='';
        if Request.info.asText['fname']<>'' then
          begin
          if not DirectoryExists(ExtractFilePath(ExpandFileName(Request.info.asText['fname']))) then
            CreateDir(ExtractFilePath(ExpandFileName(Request.info.asText['fname'])));
          Delete_File(request.Info.asText['fname']);
          end;
        end;

      s:=Read;

      lblBytes.Text:=IntToStr(Response.ContentIn)+' Byte(s)';

      if Request.Info.asText['fname']<>'' then
        Write_File(Request.Info.asText['fname'], s, Response.ContentIn-length(s))
      else if Request.Info.asString['data']='' then
        Request.Info.asString['data']:=s;

      if not Response.Done then
        begin
        if Response.ContentLength>0 then
          begin
          pBar.Max:=100;
          pBar.Value:=round(Response.ContentIn/Response.ContentLength*100);
          end
        else
          begin
          pBar.Max:=100;
          pBar.Value:=50;
          end;
        end
      else
        begin
        TimeCnt:=TimeCnt+1;
        if (TimeMin=0) or (TimeNow<TimeMin) then
          TimeMin:=TimeNow;
        if (TimeMax=0) or (TimeNow>TimeMax) then
          TimeMax:=TimeNow;
        TimeSum:=TimeSum+TimeNow;

        TimeAvg:=round(TimeSum*100/TimeCnt)/100;

        lblTime.Text:='Time: '+
                         IntToStr(TimeMin)+' < '+
                         FloatToStr(TimeAvg)+' < '+
                         IntToStr(TimeMax)+' ms';

        Inc(Cnt);
        lblCount.Text:=IntToStr(Cnt)+' received, '+
                          IntToStr(RequestCount-1)+' waiting.';

        if Request.info.asText['fname']<>'' then
          eResponseBody.Text:='Saved to "'+Request.info.asText['fname']+'" ...'#13#10+
                               String(Read_File(Request.info.asText['fname'],0,65535))
        else
          eResponseBody.Text:=Request.info.asText['data'];

        pBar.Max:=100;
        pBar.Value:=pBar.Max;
        end;
      end;
  end;

procedure TRtcFileClient.DataRequestRepostCheck(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    if not inMainThread then
      Sync(DataRequestRepostCheck)
    else
      begin
       // We aren't using the AutoRepost property, so we can count the number of reposts here.
      Inc(Ret);
      lblRetry.Text:=IntToStr(Ret)+' Reposts.';
      lblCount.Text:=IntToStr(Cnt)+' received, '+
                        IntToStr(RequestCount)+' waiting.';
      if xShowWarning.isChecked and (Request.Reposted>5) then
        begin
        ShowMessage('Last request was reposted 5 times without success.'#13#10+
                    'Please, check your internet settings and try again.');
        { Since we aren't calling "Request.Repost", all pending requests will be canceled. }
        end
      else
        Request.Repost; // We need to call this if we want the Request to be sent once more.
      end;
  end;

procedure TRtcFileClient.btn100PostClick(Sender: TObject);
  var
    a:integer;
  begin
  for a:=1 to 100 do
    btnPostClick(Sender);
  end;

procedure TRtcFileClient.btnCancelAllClick(Sender: TObject);
  begin
  Client.SkipRequests;

  TimeStart:=0;
  TimeCnt:=0;
  TimeSum:=0;
  TimeMin:=0;
  TimeMax:=0;

  Cnt:=0;
  Ret:=0;

  eResponseHeader.Text:='';
  eResponseBody.Text:='';
  lblTime.Text:='Time: 0 ms';
  lblCount.Text:='0 Received.';
  lblRetry.Text:='0 Reposts.';
  lblBytes.Text:='0 Bytes';
  pBar.Max:=100;
  pBar.Value:=0;
  end;

procedure TRtcFileClient.xUseProxyClick(Sender: TObject);
  begin
  Client.Disconnect;
  try
    Client.UseProxy:=xUseProxy.isChecked;
  finally
    xUseProxy.isChecked:=Client.UseProxy;
    end;
  eProxyAddr.CanFocus:=xUseProxy.isChecked or xWinHTTP.isChecked;
  eProxyUsername.CanFocus:=xUseProxy.isChecked or xWinHTTP.isChecked;
  eProxyPassword.CanFocus:=xUseProxy.isChecked or xWinHTTP.isChecked;
  end;

procedure TRtcFileClient.ClientConnectFail(Sender: TRtcConnection);
  begin
  if not TRtcDataClient(Sender).ReconnectOn.ConnectFail then
    if not Sender.inMainThread then
      Sender.Sync(ClientConnectFail)
    else
      ShowMessage('Failed to connect to Server.');
  end;

procedure TRtcFileClient.xThreadsClick(Sender: TObject);
  begin
  Client.Disconnect;
  try
    Client.MultiThreaded:=xThreads.isChecked;
  finally
    xThreads.isChecked:=Client.MultiThreaded;
    end;
  end;

procedure TRtcFileClient.eAddrChange(Sender: TObject);
  begin
  if not Visible then Exit;

  Client.Disconnect;
  Client.ServerAddr:=RtcString(eAddr.Text);
  eReqHost.Text:=eAddr.Text;
  end;

procedure TRtcFileClient.xUseSSLClick(Sender: TObject);
  begin
  Client.Disconnect;
  try
    Client.UseSSL:=xUseSSL.isChecked;
  finally
    xUseSSL.isChecked:=Client.UseSSL;
    end;
  if xUseSSL.isChecked then
    ePort.Text:='443'
  else
    ePort.Text:='80';
  end;

procedure TRtcFileClient.ePortChange(Sender: TObject);
  begin
  if not Visible then Exit;

  Client.Disconnect;
  Client.ServerPort:=RtcString(ePort.Text);
  end;

procedure TRtcFileClient.xAutoConnectClick(Sender: TObject);
  begin
  Client.Disconnect;
  try
    Client.AutoConnect:=xAutoConnect.isChecked;
  finally
    xAutoConnect.isChecked:=Client.AutoConnect;
    end;
  end;

procedure TRtcFileClient.btnSaveAsClick(Sender: TObject);
  begin
  SaveDialog1.InitialDir:=ExtractFilePath(eFileName.Text);
  SaveDialog1.FileName:=ExtractFileName(eFileName.Text);
  if SaveDialog1.Execute then
    begin
    xSaveToFile.isChecked:=True;
    eFileName.Text:=SaveDialog1.FileName;
    end;
  end;

procedure TRtcFileClient.eReqFileNameChange(Sender: TObject);
  begin
  if not Visible then Exit;

  eFileName.Text:=ExtractFilePath(eFileName.Text)+
                  ExtractFileName(StringReplace(eReqFileName.Text,'/','\',[rfReplaceAll]));
  if ExtractFileName(eFileName.Text)='' then
    eFileName.Text:=eFileName.Text+'index.htm';
  end;

procedure TRtcFileClient.xReconLostClick(Sender: TObject);
  begin
  Client.ReconnectOn.ConnectLost:=xReconLost.isChecked;
  end;

procedure TRtcFileClient.xReconFailClick(Sender: TObject);
  begin
  Client.ReconnectOn.ConnectFail:=xReconFail.isChecked;
  end;

procedure TRtcFileClient.xReconErrorClick(Sender: TObject);
  begin
  Client.ReconnectOn.ConnectError:=xReconError.isChecked;
  end;

procedure TRtcFileClient.ClientDataIn(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ClientDataIn)
  else
    begin
    TotalDataIn:=TotalDataIn+Sender.DataIn;
    lblDataInOut.Text:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

procedure TRtcFileClient.ClientDataOut(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ClientDataOut)
  else
    begin
    TotalDataOut:=TotalDataOut+Sender.DataOut;
    lblDataInOut.Text:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

procedure TRtcFileClient.DataRequestResponseAbort(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    if not inMainThread then
      Sync(DataRequestRepostCheck)
    else
      begin
      // This request was aborted, update our "waiting" display ...
      lblCount.Text:=IntToStr(Cnt)+' received, '+
                        IntToStr(RequestCount-1)+' waiting.';
      // lblCount.Update;
      end;
  end;

procedure TRtcFileClient.xWinHTTPClick(Sender: TObject);
  begin
  Client.Disconnect;
  try
    Client.UseWinHTTP:=xWinHTTP.isChecked;
  finally
    xWinHTTP.isChecked:=Client.UseWinHTTP;
    end;
  eProxyAddr.CanFocus:=xUseProxy.isChecked or xWinHTTP.isChecked;
  eProxyUsername.CanFocus:=xUseProxy.isChecked or xWinHTTP.isChecked;
  eProxyPassword.CanFocus:=xUseProxy.isChecked or xWinHTTP.isChecked;
  end;

procedure TRtcFileClient.eProxyAddrChange(Sender: TObject);
  begin
  if not Visible then Exit;

  Client.Disconnect;
  Client.UserLogin.ProxyAddr:=eProxyAddr.Text;
  end;

procedure TRtcFileClient.eProxyUsernameChange(Sender: TObject);
  begin
  if not Visible then Exit;

  Client.Disconnect;
  Client.UserLogin.ProxyUserName:=eProxyUsername.Text;
  end;

procedure TRtcFileClient.eProxyPasswordChange(Sender: TObject);
  begin
  if not Visible then Exit;

  Client.Disconnect;
  Client.UserLogin.ProxyPassword:=eProxyPassword.Text;
  end;

procedure TRtcFileClient.xBlockingClick(Sender: TObject);
  begin
  Client.Disconnect;
  try
    Client.Blocking:=xBlocking.isChecked;
  finally
    xBlocking.isChecked:=Client.Blocking;
    end;
  end;

procedure TRtcFileClient.xCryptPluginClick(Sender: TObject);
  begin
  Client.Disconnect;
  try
    if xCryptPlugin.isChecked then
      Client.CryptPlugin:=RtcCryptPlugin
    else
      Client.CryptPlugin:=nil;
  finally
    xCryptPlugin.isChecked:=assigned(Client.CryptPlugin);
    end;
  end;

end.
