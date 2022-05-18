unit MainGateForm;

interface

{$include rtcDeploy.inc}

{ This example demonstrates how to set up and use a TRtcGateway component with a TRtcHttpServer.

  Additionally, this "ChatServer" example will accept ONLY connections from the "RTCGateChatClient"
  example Project, which is accomplished by doing two things:

  1. By using a special GatePrimaryKey value, which is set on the
     Gate:TRtcGateway component in this Project (Server) and on the
     GateCli:TRtcHttpGateClient component in the RTCGateChatClient Project.
     If the GatePrimaryKey values do not exactly match, Client will
     be unable to communicate with the Gateway (encryption errors).

  2. You can use the GateUserAuth property to send any information
     from the User that is required to authenticate the User,
     like a Username and a Password for example. To keep this example
     simple, all our Clients only need "MyChatClient" as GateUserAuth.
     We have implemented the GateBeforeLogin event to raise an exception
     for any Client which tries to Login without the excepted GateUserAuth value.

  For sending ONLINE/OFFLINE notifications to all CHAT users,
  this ChatServer will be using Gateway-side "Channels" feature, registering
  all Chat Clients as Hosts and Listeners to the "Chat" channel with GroupID 4000.
  This is implemented in the "GateUserReady" (user ONLINE)
  and GateUserNotReady events (user OFFLINE). }

{ To compile the project with StreamSec Tools 2.1+ components, declare
  the StreamSecII compiler directive below or in the "rtcDeploy.inc" file. }

{.$DEFINE StreamSecII}

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  rtcInfo,
  rtcConn,
  rtcTypes,
  
{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

  rtcLog,
  rtcDataSrv,
  rtcHttpSrv,
  rtcGateConst,

  rtcGateSrv,
  rtcSrcList,
  rtcSystem,

  ExtCtrls, Spin;

type
  TGateForm = class(TForm)
    Server: TRtcHttpServer;
    Panel1: TPanel;
    Label1: TLabel;
    shGateway: TShape;
    Label2: TLabel;
    Label3: TLabel;
    ePort: TEdit;
    btnStart: TButton;
    eSendSpeed: TSpinEdit;
    lblStatus: TLabel;
    lblConnect: TLabel;
    StatusTimer: TTimer;
    Label4: TLabel;
    Label5: TLabel;
    eRecvSpeed: TSpinEdit;
    Label6: TLabel;
    Gate: TRtcGateway;
    xIPv6: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure ServerException(Sender: TRtcConnection; E: Exception);
    procedure ServerInvalidRequest(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure GateBeforeUserLogin(Sender: TRtcConnection; UserID: Cardinal; var UserAuth, UserInfo: String; var SecondaryKey: String);
    procedure GateBeforeUserLogout(Sender: TRtcConnection; UserID: Cardinal; const UserAuth,UserInfo: String);
    procedure GateUserNotReady(Sender: TRtcConnection; UserID: Cardinal; const UserAuth,UserInfo: String);
    procedure GateUserReady(Sender: TRtcConnection; UserID: Cardinal; const UserAuth,UserInfo: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GateForm: TGateForm;

implementation

{$R *.dfm}

procedure TGateForm.btnStartClick(Sender: TObject);
  begin
  if not Server.isListening then
    begin
    Gate.ReceiveSpeedLimit:=eRecvSpeed.Value;
    Gate.StreamSpeedLimit:=eSendSpeed.Value;
    Server.ServerPort:=ePort.Text;
    Server.ServerIPV:=GetRtcIPV(xIPv6.Checked);
    Server.Listen();
    end
  else
    Server.StopListenNow;
  end;

procedure TGateForm.ServerListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStart)
  else
    begin
    shGateway.Brush.Color:=clGreen;
    lblStatus.Caption:='Listening on Port '+Sender.LocalPort;
    btnStart.Caption:='STOP';
    end;
  end;

procedure TGateForm.ServerListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStop)
  else
    begin
    shGateway.Brush.Color:=clRed;
    lblStatus.Caption:='Stopped listening';
    btnStart.Caption:='START';
    end;
  end;

procedure TGateForm.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenError,E)
  else
    lblStatus.Caption:=E.Message;
  end;

procedure TGateForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  Server.StopListenNow;
  end;

procedure TGateForm.FormCreate(Sender: TObject);
  begin
{$IFDEF StreamSecII}
  AddServerRootCertFile('root.cer');
  AddServerPFXFile('server.pfx','abc');

  Server.CryptPlugin := GetServerCryptPlugin;
  ePort.Text:='443';
{$ELSE}
  ePort.Text:='80';
{$ENDIF}

  StatusTimer.Enabled:=True;
  end;

procedure TGateForm.FormDestroy(Sender: TObject);
  begin
  StatusTimer.Enabled:=False;
  end;

procedure TGateForm.StatusTimerTimer(Sender: TObject);
  begin
  lblConnect.Caption:=IntToStr(Server.TotalConnectionCount);
  end;

procedure TGateForm.ServerException(Sender: TRtcConnection; E: Exception);
  begin
  Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' ERROR '+E.ClassName+':'+E.Message,'ERROR');
  end;

procedure TGateForm.ServerInvalidRequest(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
  begin
  Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' Invalid: '+Srv.Request.Method+' '+Srv.Request.URI,'HTTP');
  if Srv.Request.Complete then
    begin
    Srv.Response.Status(404,'*');
    Srv.Write;
    end
  else
    Srv.Disconnect;
  end;

procedure TGateForm.ServerRequestNotAccepted(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
  begin
  Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' NOT Accepted: '+Srv.Request.Method+' '+Srv.Request.URI+' ('+Srv.Request['CONTENT-LENGTH']+' bytes)','HTTP');
  if Srv.Request.Complete then
    begin
    Srv.Response.Status(404,'*');
    Srv.Write;
    end
  else
    Srv.Disconnect;
  end;

procedure TGateForm.GateBeforeUserLogin(Sender: TRtcConnection; UserID: Cardinal; var UserAuth,UserInfo: String; var SecondaryKey: String);
  begin
  Log('Before Log IN '+IntToStr(UserID)+': '+UserAuth+' / '+UserInfo);

  { User is logging in. Check "UserAuth" if Login is allowed and raise an exception if not.
    Because we ONLY want to accept users with UserAuth "MyChatClient",
    we will raise an exception for all other Clients.

    If you want to accept other Clients,
    the lines below have to be commented out ... }

  if UserAuth<>'MyChatClient' then
    raise Exception.Create('This is a Chat Server. No other Clients allowed.');

  // We can update the "UserInfo" here.
  // For example, to include Users IP address:
  UserInfo:=UserInfo+' @'+Sender.PeerAddr;
  end;

procedure TGateForm.GateBeforeUserLogout(Sender: TRtcConnection; UserID: Cardinal; const UserAuth,UserInfo: String);
  begin
  Log('Before Log OUT '+IntToStr(UserID)+': '+UserAuth+' / '+UserInfo);

  { User ID removed from the List (Logged Out completely) }
  end;

procedure TGateForm.GateUserNotReady(Sender: TRtcConnection; UserID: Cardinal; const UserAuth,UserInfo: String);
  begin
  { This event is called when the User is no longer ready to send or receive data (OFFLINE). }

  Log('User NOT Ready '+IntToStr(UserID)+': '+UserAuth+' / '+UserInfo);

  if UserAuth='MyChatClient' then
    Gate.RemoveUserFromChannel(UserID,'Chat',True,False);
  end;

procedure TGateForm.GateUserReady(Sender: TRtcConnection; UserID: Cardinal; const UserAuth,UserInfo: String);
  begin
  { This event is called when the User is ONLINE and ready to send and receive data. }

  Log('User Ready '+IntToStr(UserID)+': '+UserAuth+' / '+UserInfo);

  if UserAuth='MyChatClient' then
    Gate.AddUserToChannel(UserID,'Chat',4000,True,True,0,True,False);
  end;

end.
