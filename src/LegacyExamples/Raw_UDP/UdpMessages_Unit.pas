unit UdpMessages_Unit;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, 

  rtcSystem, rtcInfo, rtcConn,
  rtcUdpCli, rtcUdpSrv;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    eReceive: TMemo;
    UdpServer: TRtcUdpServer;
    Panel5: TPanel;
    Label1: TLabel;
    eSendPort: TEdit;
    eSendAddress: TEdit;
    Label2: TLabel;
    Label6: TLabel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    xFillAddr: TCheckBox;
    Panel7: TPanel;
    btnListen: TButton;
    Panel8: TPanel;
    Panel9: TPanel;
    btnSend: TButton;
    Panel10: TPanel;
    eSend: TMemo;
    Panel11: TPanel;
    Label3: TLabel;
    eMyPort: TEdit;
    eMyAddress: TEdit;
    Label4: TLabel;
    Label7: TLabel;
    eMyNickname: TEdit;
    Label5: TLabel;
    UdpClient: TRtcUdpClient;
    btnReset: TButton;
    procedure btnListenClick(Sender: TObject);
    procedure UdpServerListenStart(Sender: TRtcConnection);
    procedure UdpServerListenStop(Sender: TRtcConnection);
    procedure UdpServerDataReceived(Sender: TRtcConnection);
    procedure UdpClientDataLost(Sender: TRtcConnection);
    procedure UdpClientDataReceived(Sender: TRtcConnection);
    procedure UdpClientDisconnect(Sender: TRtcConnection);
    procedure eSendKeyPress(Sender: TObject; var Key: Char);
    procedure btnSendClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnListenClick(Sender: TObject);
  begin
  if not UdpServer.isListening then
    begin
    UdpServer.ServerPort:=eMyPort.Text;
    { Listening Port (UDP):
        No other application should be listening on
        the same UDP Port on this PC. }

    UdpServer.ServerAddr:=eMyAddress.Text;
    { Listening Address (UDP):
        You can either leave the Addr property empty,
        to listen on all addapters installed in your PC,
        or enter a specific IP address to bind only to
        the Network Addapter using that IP. }

    btnListen.Enabled:=False;
    try
      UdpServer.Listen;
    { Listen:
        When you fill all your properties,
        call Listen to try and start the listener.

        If the acction fails with an error and
        you did not set the OnListenError event,
        exception will be raised with the appropriate error message.
        If the action succeeds, OnListenStart event will be called. }
    except
      on E:Exception do
        begin
        btnListen.Enabled:=True;
        ShowMessage('Can not start Listening:'#13#10 + E.Message);
        end;
      end;
    end
  else
    begin
    { StopListen:
        When you want to 'release the hook' and stop waiting
        for incomming messages, call StopListen. }
    btnListen.Enabled:=False;
    UdpServer.StopListen;
    end;
  end;

procedure TForm1.eSendKeyPress(Sender: TObject; var Key: Char);
  begin
  if Key=#13 then
    begin
    Key:=#0;
    btnSend.Click;
    end;
  end;

procedure TForm1.UdpServerListenStart(Sender: TRtcConnection);
  begin
  eMyPort.Enabled:=False;
  eMyAddress.Enabled:=False;
  btnListen.Caption:='Stop';
  btnListen.Enabled:=True;
  end;

procedure TForm1.UdpServerListenStop(Sender: TRtcConnection);
  begin
  eMyPort.Enabled:=True;
  eMyAddress.Enabled:=True;
  btnListen.Caption:='Listen';
  btnListen.Enabled:=True;
  end;

procedure TForm1.UdpServerDataReceived(Sender: TRtcConnection);
  var
    s:string;
  begin
  { Since we are NOT using MultiThreaded mode,
    we can directly access the GUI.
    Sender can directly be casted to TRtcUdpServer,
    so we can assess all properties
    (for example, WriteStr would not be accessible from TRtcConnection). }
  with TRtcUdpServer(Sender) do
    begin
    s:=Read;
    if PeerAddr<>eSendAddress.Text then
      s:='From '+PeerAddr+' '+s;

    eReceive.Lines.Add('<< '+s);
    Write('OK'); // respond with OK.
    if xFillAddr.Checked then
      begin
      if eSendAddress.Text='' then
        eSendAddress.Text:=PeerAddr;
      end;
    end;
  end;

procedure TForm1.btnSendClick(Sender: TObject);
  begin
  if not eSend.Enabled then
    begin
    MessageBeep(0);
    Exit;
    end;

  with UdpClient do
    begin
    if (ServerPort<>eSendPort.Text) or
       (ServerAddr<>eSendAddress.Text) then
      begin
      if isConnected then Disconnect; // Make sure we are not connected before we change the port and/or address.
      ServerPort:=eSendPort.Text; // This is the Port on which the Recipient is listening.
      ServerAddr:=eSendAddress.Text; // This is Recipient's address (IP or domain name).
      end;

    if not isConnected then // not connected, we need to open a connection
      begin
      try
        Connect; // Before we can send anything, we have to open a connection.
      except
        on E:Exception do
          begin
          { Since we did NOT set the OnConnectError event,
            exception will be raised if errors occure while trying to open a connection. }
          ShowMessage(E.Message);
          Exit;
          end;
        end;
      end;

    if eMyNickName.Text<>'' then
      Write(eMyNickname.Text+': '+eSend.Lines.Text)
    else
      Write(eSend.Lines.Text);
    { Write:
        When using UDP connection, you can immediatelly start
        sending your data after you call Connect.

        On a TCP/IP connection, you at least have to wait for the
        OnConnect event before you start sending anything. }

    btnSend.SetFocus;
    eSend.Enabled:=False;
    { Since we only have one UdpClient connection object and
      we expect to get a responce from the other side,
      the easiest way to tell the User that he can not continue
      typing is to disable the Memo field while sending.

      Also, because we do not want to clear the Text typed in
      case of a send failure, we will leave the Memo as it is
      and clear it ONLY if we get a positive responce from the recipient. }
    end;
  end;

procedure TForm1.UdpClientDataLost(Sender: TRtcConnection);
  begin
  with TRtcUdpClient(Sender) do
    begin
    eReceive.Lines.Add('> Message not deliverable to '+ServerAddr);
    eSend.Enabled:=True;
    eSend.SetFocus;
    { Message could not be delivered, but at least we got notified.
      We do not want to clear the Text typed, user may want to retry sending. }

    Disconnect;
    { We will close this connection now, since it is very likely that
      there is nothing waiting for our messages at the other side.
      If the user tries sending to the same address/port,
      we will simply open a new connection (see the btnSendClick event handler). }
    end;
  end;

procedure TForm1.UdpClientDataReceived(Sender: TRtcConnection);
  var
    s:string;
  begin
  { Message delivered, we got a response. }
  with TRtcUdpClient(Sender) do
    begin
    if Read='OK' then // Server responded as expected, this is our UdpServer.
      begin
      s:=eSend.Text;
      if PeerAddr<>eSendAddress.Text then
        s:='To '+PeerAddr+': '+s;
      eReceive.Lines.Add('>> '+s);

      eSend.Clear;
      eSend.Enabled:=True;
      eSend.SetFocus;
      end
    else // Unexpected responce, treat this as a failure.
      UdpClientDataLost(Sender);
    end;
  end;

procedure TForm1.UdpClientDisconnect(Sender: TRtcConnection);
  begin
  { We have been notified of a disconnect.
    This can mean two things:
      1. We disconnected because we want to send to another address/port, or
      2. A Timeout for sending occured (UdpClient.Timeout.AfterSent) }

  if not eSend.Enabled then
    begin
    { Disabled Memo field (eSend) means that we were trying to send a message
      out when the disconnect happened. This means that our Timeout was triggered,
      which closed the connection and means that the message could not be dilivered in
      the time specified. We will assume now that our message is undeliverable. }
    UdpClientDataLost(Sender);
    end;
  end;

procedure TForm1.btnResetClick(Sender: TObject);
  begin
  UdpClient.Disconnect;
  UdpClient.Connect;
  end;

end.
