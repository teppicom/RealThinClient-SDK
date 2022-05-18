unit fmx4Client_Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.TabControl, FMX.Controls.Presentation, FMX.Edit,
  rtcDataCli, rtcSystem, rtcInfo, rtcConn, rtcHttpCli;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    eRespHeaders: TMemo;
    eRespContent: TMemo;
    TabItem3: TTabItem;
    HttpClient: TRtcHttpClient;
    DataRequest: TRtcDataRequest;
    eServerAddr: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    eServerPort: TEdit;
    Label3: TLabel;
    eReqURI: TEdit;
    Label4: TLabel;
    eReqMethod: TEdit;
    btnSend: TButton;
    eReqContent: TMemo;
    lStatus: TLabel;
    xEncodeURI: TCheckBox;
    xUTF8Content: TCheckBox;
    xMultiThreaded: TCheckBox;
    procedure DataRequestBeginRequest(Sender: TRtcConnection);
    procedure DataRequestDataReceived(Sender: TRtcConnection);
    procedure btnSendClick(Sender: TObject);
    procedure DataRequestConnectLost(Sender: TRtcConnection);
    procedure DataRequestDataSent(Sender: TRtcConnection);
    procedure HttpClientConnect(Sender: TRtcConnection);
    procedure HttpClientConnectError(Sender: TRtcConnection; E: Exception);
    procedure HttpClientConnectFail(Sender: TRtcConnection);
    procedure HttpClientBeginRequest(Sender: TRtcConnection);
    procedure HttpClientConnecting(Sender: TRtcConnection);
    procedure HttpClientConnectLost(Sender: TRtcConnection);
    procedure HttpClientDataIn(Sender: TRtcConnection);
    procedure HttpClientDataOut(Sender: TRtcConnection);
    procedure HttpClientDisconnect(Sender: TRtcConnection);
    procedure HttpClientDisconnecting(Sender: TRtcConnection);
    procedure HttpClientException(Sender: TRtcConnection; E: Exception);
    procedure HttpClientInvalidResponse(Sender: TRtcConnection);
    procedure HttpClientReconnect(Sender: TRtcConnection);
    procedure HttpClientRepostCheck(Sender: TRtcConnection);
    procedure HttpClientResponseAbort(Sender: TRtcConnection);
    procedure HttpClientResponseData(Sender: TRtcConnection);
    procedure HttpClientResponseDone(Sender: TRtcConnection);
    procedure HttpClientResponseReject(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnSendClick(Sender: TObject);
  begin
  eRespHeaders.Text:=DateTimeToStr(Now);
  eRespContent.Text:='';

  if (HttpClient.ServerAddr<>eServerAddr.Text) or
     (HttpClient.ServerPort<>eServerPort.Text) or
     (HttpClient.MultiThreaded<>xMultiThreaded.IsChecked) then
    begin
    eRespHeaders.Lines.Add('Preparing ...');
    HttpClient.DisconnectNow(True);
    HttpClient.ServerAddr:=eServerAddr.Text;
    HttpClient.ServerPort:=eServerPort.Text;
    HttpClient.MultiThreaded:=xMultiThreaded.IsChecked;
    HttpClient.AutoConnect:=True;
    end;

  DataRequest.Request.Host:=HttpClient.ServerAddr;

  eRespHeaders.Lines.Add('Posting ...');
  DataRequest.Post;
  end;

procedure TForm1.DataRequestBeginRequest(Sender: TRtcConnection);
  var
    DataCli:TRtcDataClient absolute Sender;
  begin
  DataCli.Request.Method:=eReqMethod.Text;
  if xEncodeURI.IsChecked then
    DataCli.Request.URI:=URL_Encode(eReqURI.Text)
  else
    DataCli.Request.URI:=eReqURI.Text;

  eRespHeaders.Lines.Add('Request: Sending ...');
  eRespHeaders.Lines.Add(DataCli.Request.Method+' '+DataCli.Request.URI);

  if xUTF8Content.IsChecked then
    DataCli.Write(Utf8Encode(eReqContent.Text))
  else
    DataCli.Write(eReqContent.Text);
  end;

procedure TForm1.DataRequestConnectLost(Sender: TRtcConnection);
  begin
  eRespHeaders.Lines.Add('Request: Connection Lost.');
  end;

procedure TForm1.DataRequestDataReceived(Sender: TRtcConnection);
  var
    DataCli:TRtcDataClient absolute Sender;
  begin
  if DataCli.Response.Started then
    begin
    eRespHeaders.Lines.Add('Response: Receiving ...'#13#10);
    eRespHeaders.Lines.Add(IntToStr(DataCli.Response.StatusCode)+' '+DataCli.Response.StatusText);
    eRespHeaders.Lines.Add(DataCli.Response.HeaderText);
    end;
  if not xUTF8Content.IsChecked then
    eRespContent.Text:=eRespContent.Text+DataCli.Read;
  if DataCli.Response.Done then
    begin
    eRespHeaders.Lines.Add('Response: Content IN ('+IntToStr(DataCli.Response.ContentIn)+' Bytes) - DONE'#13#10);
    if xUTF8Content.IsChecked then
      eRespContent.Text:=Utf8Decode(DataCli.Read);
    end
  else
    eRespHeaders.Lines.Add('Response: Content IN ('+IntToStr(DataCli.Response.ContentIn)+' Bytes)');
  end;

procedure TForm1.DataRequestDataSent(Sender: TRtcConnection);
  var
    DataCli:TRtcDataClient absolute Sender;
  begin
  if DataCli.Request.Complete then
    eRespHeaders.Lines.Add('Request: Content OUT ('+IntToStr(DataCli.Request.ContentOut)+' Bytes) - COMPLETE'#13#10)
  else
    eRespHeaders.Lines.Add('Request: Content OUT ('+IntToStr(DataCli.Request.ContentOut)+' Bytes)')
  end;

procedure TForm1.HttpClientBeginRequest(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientBeginRequest) then Exit;
  lStatus.Text:='Begin Request';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientConnect(Sender: TRtcConnection);
  var
    DataCli:TRtcDataClient absolute Sender;
  begin
  if Sender.Sync(HttpClientConnect) then Exit;
  lStatus.Text:='Connected ('+DataCli.PeerAddr+' @ '+DataCli.PeerPort+')';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientConnectError(Sender: TRtcConnection; E: Exception);
  begin
  if Sender.Sync(HttpClientConnectError,E) then Exit;
  lStatus.Text:='Connect Error: '+E.Message;
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientConnectFail(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientConnectFail) then Exit;
  lStatus.Text:='Connect Fail.';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientConnecting(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientConnecting) then Exit;
  lStatus.Text:='Connecting';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientConnectLost(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientConnectLost) then Exit;
  lStatus.Text:='Connect Lost.';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientDataIn(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientDataIn) then Exit;
  lStatus.Text:='Data IN ('+IntToStr(Sender.DataIn)+' bytes)';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientDataOut(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientDataOut) then Exit;
  lStatus.Text:='Data OUT ('+IntToStr(Sender.DataOut)+' bytes)';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientDisconnect(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientDisconnect) then Exit;
  lStatus.Text:='Disconnect';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientDisconnecting(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientDisconnecting) then Exit;
  lStatus.Text:='Disconnecting';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientException(Sender: TRtcConnection; E: Exception);
  begin
  if Sender.Sync(HttpClientException,E) then Exit;
  lStatus.Text:='Exception: '+E.Message;
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientInvalidResponse(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientInvalidResponse) then Exit;
  lStatus.Text:='Invalid Response';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientReconnect(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientReconnect) then Exit;
  lStatus.Text:='Reconnect';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientRepostCheck(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientRepostCheck) then Exit;
  lStatus.Text:='Repost Check';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientResponseAbort(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientResponseAbort) then Exit;
  lStatus.Text:='Response ABORT.';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientResponseData(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientResponseData) then Exit;
  lStatus.Text:='Response Data';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientResponseDone(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientResponseDone) then Exit;
  lStatus.Text:='Response DONE.';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

procedure TForm1.HttpClientResponseReject(Sender: TRtcConnection);
  begin
  if Sender.Sync(HttpClientResponseReject) then Exit;
  lStatus.Text:='Response REJECT.';
  eRespHeaders.Lines.Add(lStatus.Text);
  end;

end.
