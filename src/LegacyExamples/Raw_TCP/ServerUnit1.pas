unit ServerUnit1;

interface

uses
  Windows, Messages, SysUtils, 
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcSystem, rtcLog,

  rtcThrPool, rtcInfo, rtcConn,

  rtcTcpCli, rtcTcpSrv;

type
  TServerForm1 = class(TForm)
    Memo2: TMemo;
    Panel1: TPanel;
    btnSend: TButton;
    btnListen: TButton;
    Splitter1: TSplitter;
    btnClose: TButton;
    btnConnect: TButton;
    Label1: TLabel;
    edAddr: TEdit;
    edPort: TEdit;
    Label2: TLabel;
    xRetryOnFail: TCheckBox;
    Button1: TButton;
    Label3: TLabel;
    lblConCount: TLabel;
    Button2: TButton;
    lblCliCount: TLabel;
    xMultiThread: TCheckBox;
    lblTotal: TLabel;
    Timer1: TTimer;
    xRetryOnError: TCheckBox;
    xRetryOnLost: TCheckBox;
    Server: TRtcTcpServer;
    Client: TRtcTcpClient;
    btnStop: TButton;
    Label4: TLabel;
    xSilent: TCheckBox;
    Memo1: TMemo;
    lblData: TLabel;
    lblPreCount: TLabel;
    procedure btnListenClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure ServerConnect(Sender: TRtcConnection);
    procedure ServerDataReceived(Sender: TRtcConnection);
    procedure ServerDataSent(Sender: TRtcConnection);
    procedure ServerDisconnect(Sender: TRtcConnection);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure btnConnectClick(Sender: TObject);
    procedure ClientConnectFail(Sender: TRtcConnection);
    procedure ClientReconnect(Sender: TRtcConnection);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ServerClientConnect(Sender: TRtcConnection);
    procedure ServerClientDisconnect(Sender: TRtcConnection);
    procedure xMultiThreadClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClientConnectError(Sender: TRtcConnection;
      E: Exception);
    procedure xRetryOnFailClick(Sender: TObject);
    procedure xRetryOnErrorClick(Sender: TObject);
    procedure ClientConnectLost(Sender: TRtcConnection);
    procedure xRetryOnLostClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure ClientConnect(Sender: TRtcConnection);
    procedure ClientDisconnect(Sender: TRtcConnection);
    procedure ClientDataReceived(Sender: TRtcConnection);
    procedure ClientDataSent(Sender: TRtcConnection);
    procedure ServerRestart(Sender: TRtcConnection);
    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenLost(Sender: TRtcConnection);
    procedure ServerException(Sender: TRtcConnection; E: Exception);
    procedure ClientException(Sender: TRtcConnection; E: Exception);
    procedure xSilentClick(Sender: TObject);
    procedure Memo2Exit(Sender: TObject);
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure ClientConnecting(Sender: TRtcConnection);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure ClientDisconnecting(Sender: TRtcConnection);
    procedure ServerDataOut(Sender: TRtcConnection);
    procedure ClientDataOut(Sender: TRtcConnection);
    procedure ClientReadyToSend(Sender: TRtcConnection);
    procedure btnSrvCloseClick(Sender: TObject);
  private
    FTekst:string;
    FSilent:boolean;

    function GetSilent: boolean;
    function GetTekst: string;
    procedure SetSilent(const Value: boolean);
    procedure SetTekst(const Value: string);
    { Private declarations }

  public
    CliRecv,ConRecv,
    CliSent,ConSent:int64;

    ErrCount,
    PreCount,
    ConCount,
    CliCount:integer;

    CS:TRtcCritSec;

    procedure IncCliCnt;
    procedure DecCliCnt;
    procedure IncCnt;
    procedure DecCnt;
    procedure IncPreCnt;
    procedure DecPreCnt;
    procedure IncErrCnt;

    procedure AddRecv(a:int64);
    function GetRecv:int64;
    procedure AddSent(a:int64);
    function GetSent:int64;

    procedure AddCliRecv(a:int64);
    function GetCliRecv:int64;
    procedure AddCliSent(a:int64);
    function GetCliSent:int64;

    function GetCliCnt:integer;
    function GetCnt:integer;
    function GetPreCnt:integer;
    function GetErrCnt:integer;

    procedure MyException(Sender: TObject; E: Exception);

    procedure Display(s:string);
    property Silent:boolean read GetSilent write SetSilent;
    property Tekst:string read GetTekst write SetTekst;
    { Public declarations }
  end;

var
  ServerForm1: TServerForm1;

implementation

{$R *.dfm}

procedure TServerForm1.Memo2Exit(Sender: TObject);
  begin
  Tekst:=Memo2.Lines.Text;
  end;

function TServerForm1.GetSilent: boolean;
  begin
  CS.Enter;
  try
    Result:=FSilent;
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetTekst: string;
  begin
  CS.Enter;
  try
    Result:=FTekst;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.SetSilent(const Value: boolean);
  begin
  CS.Enter;
  try
    FSilent:=Value;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.SetTekst(const Value: string);
  begin
  CS.Enter;
  try
    FTekst:=Value;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.DecCliCnt;
  begin
  CS.Enter;
  try
    CliCount:=CliCount-1;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.DecCnt;
  begin
  CS.Enter;
  try
    ConCount:=ConCount-1;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.DecPreCnt;
  begin
  CS.Enter;
  try
    PreCount:=PreCount-1;
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetCliCnt:integer;
  begin
  CS.Enter;
  try
    Result:=CliCount;
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetCnt: integer;
  begin
  CS.Enter;
  try
    Result:=ConCount;
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetPreCnt: integer;
  begin
  CS.Enter;
  try
    Result:=PreCount;
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetErrCnt: integer;
  begin
  CS.Enter;
  try
    Result:=ErrCount;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.IncCliCnt;
  begin
  CS.Enter;
  try
    Inc(CliCount);
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.IncCnt;
  begin
  CS.Enter;
  try
    Inc(ConCount);
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.IncPreCnt;
  begin
  CS.Enter;
  try
    Inc(PreCount);
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.IncErrCnt;
  begin
  CS.Enter;
  try
    Inc(ErrCount);
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.AddRecv(a: int64);
  begin
  CS.Enter;
  try
    Inc(ConRecv,a);
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.AddSent(a: int64);
  begin
  CS.Enter;
  try
    Inc(ConSent,a);
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetRecv: int64;
  begin
  CS.Enter;
  try
    Result:=ConRecv;
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetSent: int64;
  begin
  CS.Enter;
  try
    Result:=ConSent;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.AddCliRecv(a: int64);
  begin
  CS.Enter;
  try
    Inc(CliRecv,a);
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.AddCliSent(a: int64);
  begin
  CS.Enter;
  try
    Inc(CliSent,a);
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetCliRecv: int64;
  begin
  CS.Enter;
  try
    Result:=CliRecv;
  finally
    CS.Leave;
    end;
  end;

function TServerForm1.GetCliSent: int64;
  begin
  CS.Enter;
  try
    Result:=CliSent;
  finally
    CS.Leave;
    end;
  end;

procedure TServerForm1.FormCreate(Sender: TObject);
  begin
  Application.OnException:=MyException;
  CS:=TRtcCritSec.Create;
  Tekst:=Memo1.Lines.Text;
  Silent:=xSilent.Checked;
  ErrCount:=0;
  ConCount:=0;
  PreCount:=0;
  CliCount:=0;
  ConRecv:=0;
  ConSent:=0;
  end;

procedure TServerForm1.MyException(Sender: TObject; E: Exception);
  begin
  Log('Exception! '+E.ClassName+': '+E.Message);
  end;

procedure TServerForm1.FormShow(Sender: TObject);
  begin
  Timer1.Enabled:=True;
  end;

procedure TServerForm1.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  Log('Closing ...');
  Timer1.Enabled:=False;
  end;

procedure TServerForm1.Timer1Timer(Sender: TObject);
  begin
  Timer1.Enabled:=False;
  try
    lblPreCount.Caption:=IntToStr(GetPreCnt)+' P';
    lblPreCount.Update;
    lblConCount.Caption:=IntToStr(GetCnt)+' A';
    lblConCount.Update;
    lblCliCount.Caption:=IntToStr(GetCliCnt)+' S';
    lblCliCount.Update;
    lblTotal.Caption:=IntToStr(Server.TotalConnectionCount)+' T';
    lblTotal.Update;
    lblData.Caption:='SERVER:'#13#10+
                     IntToStr(GetRecv)+' read'#13#10+
                     IntToStr(GetSent)+' sent'#13#10+#13#10+
                     'CLIENT:'#13#10+
                     IntToStr(GetCliRecv)+' read'#13#10+
                     IntToStr(GetCliSent)+' sent'#13#10+
                     IntToStr(GetErrCnt)+' err';
    lblData.Update;
  finally
    Timer1.Enabled:=True;
    end;
  end;

procedure TServerForm1.Display(s: string);
  begin
  if Memo1.Lines.Count>16000 then Memo1.Clear;
  Memo1.Lines.Add(s);
  end;

procedure TServerForm1.btnListenClick(Sender: TObject);
  begin
  Server.ServerAddr:=edAddr.Text;
  Server.ServerPort:=edPort.Text;
  Server.Listen;
  end;

procedure TServerForm1.btnConnectClick(Sender: TObject);
  begin
  Client.ServerAddr:=edAddr.Text;
  Client.ServerPort:=edPort.Text;
  Client.Connect;
  end;

type
  TmyJob=class(TRtcJob)
    txt:string;
    conn:TRtcConnection;
    function Run(Thr:TRtcThread):boolean; override;
  end;

{ TmyJob }

function TmyJob.Run(Thr: TRtcThread):boolean;
  begin
  Conn.Write(txt);
  Result:=True;
  end;

procedure TServerForm1.btnSendClick(Sender: TObject);
  var
    myjob:TMyJob;
  begin
  if Client.MultiThreaded then
    begin
    myjob:=Tmyjob.Create;
    myjob.txt:=Tekst;
    myjob.conn:=Client;
    Client.PostJob(myjob);
    end
  else
    Client.Write(Tekst);
  end;

procedure TServerForm1.btnCloseClick(Sender: TObject);
  begin
  Client.Disconnect;
  end;

procedure TServerForm1.ServerListenStart(Sender: TRtcConnection);
  begin
  if Sender.inMainThread then
    begin
    Log('S: Listen Start '+Sender.LocalAddr+':'+Sender.LocalPort);
    Display('S: Listen Start '+Sender.LocalAddr+':'+Sender.LocalPort);
    if Sender=Server then
      begin
      btnListen.Enabled:=False;
      btnStop.Enabled:=True;
      end;
    end
  else if not Sender.Sync(ServerListenStart) then
    Log('S: Listen Start '+Sender.LocalAddr+':'+Sender.LocalPort);
  end;

procedure TServerForm1.ServerListenStop(Sender: TRtcConnection);
  begin
  if Sender.inMainThread then
    begin
    Log('S: Listen Stop '+Sender.LocalAddr+':'+Sender.LocalPort);
    Display('S: Listen Stop '+Sender.LocalAddr+':'+Sender.LocalPort);
    if Sender=Server then
      begin
      btnListen.Enabled:=True;
      btnStop.Enabled:=False;
      end;
    end
  else if not Sender.Sync(ServerListenStop) then
    Log('S: Listen Stop '+Sender.LocalAddr+':'+Sender.LocalPort);
  end;

procedure TServerForm1.ServerConnect(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    IncCnt;
    Log('S: Connect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    end
  else if Sender.inMainThread then
    begin
    IncCnt;
    Log('S: Connect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    Display('S: Connect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    end
  else
    Sender.Sync(ServerConnect)
  end;

procedure TServerForm1.ServerDisconnect(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    DecCnt;
    Log('S: Disconnect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
        IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    end
  else if Sender.inMainThread then
    begin
    DecCnt;
    Log('S: Disconnect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
        IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    Display('S: Disconnect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
        IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    end
  else
    Sender.Sync(ServerDisconnect)
  end;

procedure TServerForm1.ServerClientConnect(Sender: TRtcConnection);
  begin
  if Silent then
    IncCliCnt
  else if Sender.inMainThread then
    begin
    IncCliCnt;
    Display('S: Client Connect');
    end
  else
    Sender.Sync(ServerClientConnect)
  end;

procedure TServerForm1.ServerClientDisconnect(Sender: TRtcConnection);
  begin
  if Silent then
    DecCliCnt
  else if Sender.inMainThread then
    begin
    DecCliCnt;
    Display('S: Client Disconnect');
    end
  else
    Sender.Sync(ServerClientDisconnect);
  end;

procedure TServerForm1.ClientConnectFail(Sender: TRtcConnection);
  begin
  if Sender.inMainThread then
    begin
    if not Silent then
      Display('c. Connect Fail');

    if Sender=Client then
      begin
      btnConnect.Enabled:=True;
      btnClose.Enabled:=False;
      btnSend.Enabled:=False;
      end
    else if Sender is TRtcClient then
      if not TRtcClient(Sender).ReconnectOn.ConnectFail then
        Sender.Release;
    end
  else
    Sender.Sync(ClientConnectFail);
  end;

procedure TServerForm1.ClientConnectError( Sender: TRtcConnection; E: Exception);
  begin
  if Sender.inMainThread then
    begin
    if not Silent then
      Display('c. Connect Error '+E.Message);
    if Sender=Client then
      begin
      end
    else if Sender is TRtcClient then
      if not TRtcClient(Sender).ReconnectOn.ConnectError then
        Sender.Release;
    end
  else
    Sender.Sync(ClientConnectError,E);
  end;

procedure TServerForm1.ClientConnectLost( Sender: TRtcConnection);
  begin
  if Sender.inMainThread then
    begin
    if not Silent then
      Display('c. Connect Lost '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');

    if Sender=Client then
      begin
      end
    else if Sender is TRtcClient then
      if not TRtcClient(Sender).ReconnectOn.ConnectLost then
        Sender.Release;
    end
  else
    Sender.Sync(ClientConnectLost)
  end;

procedure TServerForm1.ClientReconnect(Sender: TRtcConnection);
  begin
  if not Silent then
    if Sender.inMainThread then
      begin
      if not Silent then
        Display('c. Reconnect');
      Sender.ServerAddr:=edAddr.Text;
      Sender.ServerPort:=edPort.Text;
      end
    else
      Sender.Sync(ClientReconnect)
  end;

procedure TServerForm1.ServerDataReceived(Sender: TRtcConnection);
  var
    s:string;
  begin
  if Silent then
    begin
    s:=Sender.Read;
    if s<>'' then
      begin
      AddRecv(length(s));
      Sender.Write('RE:'+Tekst);
      end;
    end
  else if Sender.inMainThread then
    begin
    s:=Sender.Read;
    if (s<>'') or (Sender is TRtcTcpServer) then
      begin
      AddRecv(length(s));
      Sender.Write('RE:'+Tekst);

      Log('S: Data Received '+Sender.PeerAddr+':'+Sender.PeerPort+' at '+Sender.LocalAddr+':'+Sender.LocalPort+' ('+IntToStr(length(s))+' bytes) '+
              IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount)+#13#10+
              s+#13#10);
      Display('S: Data Received '+Sender.PeerAddr+':'+Sender.PeerPort+' at '+Sender.LocalAddr+':'+Sender.LocalPort+' ('+IntToStr(length(s))+' bytes) '+
              IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount)+#13#10+
              s);
      end;
    end
  else
    Sender.Sync(ServerDataReceived);
  end;

procedure TServerForm1.ServerDataSent(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    end
  else if Sender.inMainThread then
    begin
    Display('S: Data Sent '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
            IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    end
  else
    Sender.Sync(ServerDataSent)
  end;

procedure TServerForm1.Button1Click(Sender: TObject);
  var
    a:integer;
  begin
  for a:=1 to 100 do
    begin
    with TRtcTcpClient.New do
      begin
      ServerPort:=edPort.Text;
      ServerAddr:=edAddr.Text;

      MultiThreaded:=xMultiThread.Checked;

      Timeout.AfterConnecting:=1800;

      ReconnectOn.ConnectFail:=xRetryOnFail.Checked;
      ReconnectOn.ConnectLost:=xRetryOnLost.Checked;
      ReconnectOn.ConnectError:=xRetryOnError.Checked;

      OnConnecting:=ClientConnecting;
      OnDisconnecting:=ClientDisconnecting;

      OnConnect:=ClientConnect;
      OnDisconnect:=ClientDisconnect;

      OnConnectError:=ClientConnectError;
      OnConnectLost:=ClientConnectLost;
      OnConnectFail:=ClientConnectFail;

      OnReadyToSend:=ClientReadyToSend;
      OnReconnect:=ClientReconnect;
      OnDataOut:=ClientDataOut;
      OnDataSent:=ClientDataSent;
      OnDataReceived:=ClientDataReceived;

      try
        Connect(True);
      except
        Release; // if we get an error here, release the connection
        Break;
        end;
      end;
    end;
  end;

procedure TServerForm1.Button2Click(Sender: TObject);
  begin
  Memo1.Lines.Clear;
  end;

procedure TServerForm1.xMultiThreadClick(Sender: TObject);
  begin
  if btnListen.Enabled then
    Server.MultiThreaded:=xMultiThread.Checked;
  if btnConnect.Enabled then
    Client.MultiThreaded:=xMultiThread.Checked;
  end;

procedure TServerForm1.xRetryOnFailClick(Sender: TObject);
  begin
  Client.ReconnectOn.ConnectFail:=xRetryOnFail.Checked;
  end;

procedure TServerForm1.xRetryOnErrorClick(Sender: TObject);
  begin
  Client.ReconnectOn.ConnectError:=xRetryOnError.Checked;
  Server.RestartOn.ListenError:=xRetryOnError.Checked;
  end;

procedure TServerForm1.xRetryOnLostClick(Sender: TObject);
  begin
  Client.ReconnectOn.ConnectLost:=xRetryOnLost.Checked;
  Server.RestartOn.ListenLost:=xRetryOnLost.Checked;
  end;

procedure TServerForm1.btnStopClick(Sender: TObject);
  begin
  Server.StopListen;
  end;

procedure TServerForm1.ClientConnect(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    IncCnt;
    end
  else if Sender.inMainThread then
    begin
    IncCnt;

    Display('c. Connect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');

    if Sender=Client then
      begin
      btnConnect.Enabled:=False;
      btnClose.Enabled:=True;
      btnSend.Enabled:=True;
      end;
    end
  else
    Sender.Sync(ClientConnect);
  end;

procedure TServerForm1.ClientDisconnect(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    DecCnt;
    end
  else if Sender.inMainThread then
    begin
    DecCnt;

    Display('c. Disconnect '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
        IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));

    if Sender=Client then
      begin
      btnConnect.Enabled:=True;
      btnClose.Enabled:=False;
      btnSend.Enabled:=False;
      end;
    end
  else
    Sender.Sync(ClientDisconnect)
  end;

procedure TServerForm1.ClientDataReceived(Sender: TRtcConnection);
  var
    s:string;
  begin
  if Silent then
    begin
    s:=Sender.Read;
    if s<>'' then
      AddCliRecv(length(s));
    end
  else if Sender.inMainThread then
    begin
    s:=Sender.Read;
    if s<>'' then
      begin
      AddCliRecv(length(s));

      Display('c. Data Received '+Sender.PeerAddr+':'+Sender.PeerPort+' at '+Sender.LocalAddr+':'+Sender.LocalPort+' ('+IntToStr(length(s))+' bytes) '+
              IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount)+
              #13#10+s);
      end;
    end
  else
    Sender.Sync(ClientDataReceived);
  end;

procedure TServerForm1.ClientDataSent(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    end
  else if Sender.inMainThread then
    begin
    Display('c. Data Sent '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
            IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    end
  else
    Sender.Sync(ClientDataSent)
  end;

procedure TServerForm1.ServerRestart(Sender: TRtcConnection);
  begin
  if Sender.inMainThread then
    begin
    Display('S: Restart');
    Sender.ServerAddr:=edAddr.Text;
    Sender.ServerPort:=edPort.Text;
    end
  else
    Sender.Sync(ServerRestart);
  end;

procedure TServerForm1.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  if Sender.inMainThread then
    begin
    Log('S: Listen Error '+E.Message);
    Display('S: Listen Error '+E.Message);
    end
  else
    Sender.Sync(ServerListenError,E);
  end;

procedure TServerForm1.ServerListenLost(Sender: TRtcConnection);
  begin
  if Sender.inMainThread then
    begin
    Log('S: Listen Lost '+Sender.LocalAddr+':'+Sender.LocalPort);
    Display('S: Listen Lost '+Sender.LocalAddr+':'+Sender.LocalPort);
    end
  else
    Sender.Sync(ServerListenLost)
  end;

procedure TServerForm1.ServerException(Sender: TRtcConnection; E: Exception);
  begin
  if Sender.inMainThread then
    begin
    Display('S: Exception '+E.ClassName+' > '+E.Message);
    Log('S: Exception '+E.ClassName+' > '+E.Message);
    end
  else if not Sender.Sync(ServerException,E) then
    Log('S: Exception '+E.ClassName+' > '+E.Message);
  end;

procedure TServerForm1.ClientException(Sender: TRtcConnection; E: Exception);
  begin
  if Sender.inMainThread then
    begin
    Display('c. Exception '+E.ClassName+' > '+E.Message);
    Log('c. Exception '+E.ClassName+' > '+E.Message);
    end
  else if not Sender.Sync(ClientException,E) then
    Log('c. Exception '+E.ClassName+' > '+E.Message);
  end;

procedure TServerForm1.xSilentClick(Sender: TObject);
  begin
  Silent:=xSilent.Checked;
  end;

procedure TServerForm1.ServerConnecting(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    IncPreCnt;
    Log('S: Connecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    end
  else if Sender.inMainThread then
    begin
    IncPreCnt;
    Log('S: Connecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    Display('S: Connecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    end
  else
    Sender.Sync(ServerConnecting);
  end;

procedure TServerForm1.ClientConnecting(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    IncPreCnt;
    end
  else if Sender.inMainThread then
    begin
    IncPreCnt;
    Display('c. Connecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    end
  else
    Sender.Sync(ClientConnecting);
  end;

procedure TServerForm1.ServerDisconnecting(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    DecPreCnt;

    Log('S: Disconnecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
        IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    end
  else if Sender.inMainThread then
    begin
    DecPreCnt;

    Log('S: Disconnecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
        IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    Display('S: Disconnecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+'] '+
        IntToStr(Sender.ReadCount)+'/'+IntToStr(Sender.WriteCount));
    end
  else
    Sender.Sync(ServerDisconnecting);
  end;

procedure TServerForm1.ClientDisconnecting(Sender: TRtcConnection);
begin
  if Silent then
    begin
    DecPreCnt;
    end
  else if Sender.inMainThread then
    begin
    DecPreCnt;
    Display('c. Disconnecting '+Sender.PeerAddr+':'+Sender.PeerPort+' [at '+Sender.LocalAddr+':'+Sender.LocalPort+']');
    end
  else
    Sender.Sync(ClientDisconnecting)
end;

procedure TServerForm1.ServerDataOut(Sender: TRtcConnection);
  begin
  AddSent(Sender.DataOut);
  end;

procedure TServerForm1.ClientDataOut(Sender: TRtcConnection);
  begin
  AddCliSent(Sender.DataOut);
  end;

procedure TServerForm1.ClientReadyToSend(Sender: TRtcConnection);
  begin
  if Silent then
    begin
    end
  else if Sender.inMainThread then
    begin
    if not Silent then
      Display('c. Ready To Send');
    end
  else
    Sender.Sync(ClientReadyToSend)
  end;

procedure TServerForm1.btnSrvCloseClick(Sender: TObject);
  begin
  Server.Disconnect;
  end;

initialization
StartLog;
end.
