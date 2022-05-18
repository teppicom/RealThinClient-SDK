unit MainUnit;

{$include rtcDefs.inc}

{ This is an example "shell" Project using RTC Gateway and Multi-Gate Client
  components with a flexible UI and basic User Management, including:
    - Basic Gateway setup
    - Basic Multi-Gate Client setup
    - Basic Gate Account setup (creating Key and Public Accounts)
    - Sending Account Copies (for Private, Link and Public Accounts)
    - Sending Account Links (send Links to Private Accounts)
    - Sending and changing a custom "DisplayName" for remote Users
    - Simple Account activation and deactivation
    - Online/Offline User notifications and Account verification ("eUsers" list)
    - Load and Save "Gateway + Client + Account" setup (with Account permissions)
    - Load and Save "Main Form" configuration

  This Project does NOT implement any Application-specific functionality
  (only basic setup and connection testing), so it can be used as a starting
  point for any VCL Application using Gateway and Multi-Gate Client components.

  NOTE: The "Invitations" List is just a placeholder, which you can remove
  entirely or use to manage invitations or messages from remote Users. }

interface

uses
  Windows, Messages, SysUtils,
  Variants, Classes, Graphics,
  Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  Menus, Buttons, Types,
  ImgList, ToolWin,

  rtcTypes,
  rtcSystem,
  rtcSrcList,

  rtcInfo, rtcConn,
  rtcGateCli, rtcGateSrv,
  rtcGateConst, rtcDataCli,

  MainModule,
  SendAccUnit,
  MakeAccUnit;

type
  TRTCGateNodeForm = class(TForm)
    sTimer: TTimer;
    Images: TImageList;
    MultiLink: TRtcMultiGateClientLink;
    WorkSplitter: TSplitter;
    pConfig: TPanel;
    pGateways: TPanel;
    eGateways: TListBox;
    pGatewayCap: TPanel;
    GateSplitter: TSplitter;
    pAccounts: TPanel;
    pInfoPanel: TPanel;
    eInfoPanel: TLabel;
    eAccounts: TListBox;
    pAccountCap: TPanel;
    tbGateways: TToolBar;
    cLocalGateway: TShape;
    btnLocalGateway: TSpeedButton;
    btnConnect: TSpeedButton;
    btnDisconnect: TSpeedButton;
    tbAccounts: TToolBar;
    btnJoinPublic: TSpeedButton;
    btnNewKey: TSpeedButton;
    btnSendLink: TSpeedButton;
    btnSendCopy: TSpeedButton;
    btnReceive: TSpeedButton;
    btnDelete: TSpeedButton;
    pAuthCode: TPanel;
    eAuthUser: TLabel;
    eAuthCode: TLabel;
    eAuthGate: TLabel;
    pWorkspace: TPanel;
    pUsers: TPanel;
    pUserCap: TPanel;
    eUsers: TListBox;
    pUserInfo: TPanel;
    eUserName: TLabel;
    Panel1: TPanel;
    btnSaveConfig: TSpeedButton;
    pInvitations: TPanel;
    pInviteCap: TPanel;
    eInvite: TListBox;
    InviteSplitter: TSplitter;
    procedure eGatewaysDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure sTimerTimer(Sender: TObject);
    procedure eGatewaysDblClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure eGatewaysClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnLocalGatewayClick(Sender: TObject);
    procedure btnNewKeyClick(Sender: TObject);
    procedure btnJoinPublicClick(Sender: TObject);
    procedure eAccountsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure eAccountsDblClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure eUsersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure eAuthUserClick(Sender: TObject);
    procedure btnSendLinkClick(Sender: TObject);
    procedure pGatewayCapClick(Sender: TObject);
    procedure pAccountCapClick(Sender: TObject);
    procedure btnSendCopyClick(Sender: TObject);
    procedure pInfoPanelClick(Sender: TObject);
    procedure pGatewaysResize(Sender: TObject);
    procedure pAccountsResize(Sender: TObject);
    procedure btnReceiveClick(Sender: TObject);
    procedure eUsersClick(Sender: TObject);
    procedure cLocalGatewayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pUsersResize(Sender: TObject);
    procedure eUserNameClick(Sender: TObject);
    procedure btnSaveConfigClick(Sender: TObject);
    procedure pInviteCapClick(Sender: TObject);
    procedure pInvitationsResize(Sender: TObject);
    procedure pUserCapClick(Sender: TObject);
    procedure MultiLinkAfterLogOutGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure MultiLinkInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure MultiLinkInfoReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
    procedure MultiLinkStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  private
    { Private declarations }
    ChangingGateway:boolean;
    GatePanHig, InvitePanHig,
    CfgPanWid, WorkPanWid: integer;
    MyUserName,LastGateAddr:String;
    FClients:tStringIntList;
  public
    { Public declarations }
    procedure GatewayListenStart(Sender: TRtcConnection);
    procedure GatewayListenStop(Sender: TRtcConnection);
    procedure GatewayListenError(Sender: TRtcConnection; E: Exception);

    function OnlyClientText(const UInfo:RtcString):RtcString;

    function CombineUserAcc(const UserAddr,AccID:RtcString):RtcWideString;
    function SplitUserAcc(const UserAcc:RtcWideString; var UserAddr:RtcString):RtcString;

    function CombineAccID(const AccID:RtcString):RtcWideString;
    function SplitAccID(const AccInfo:RtcWideString):RtcString;

    procedure AddClient(const UserAddr,AccID:RtcString);
    procedure RemClient(const UserAddr,AccID:RtcString);
    procedure VerifiedClient(const UserAddr,AccID:RtcString);
    procedure UnVerifiedClient(const UserAddr,AccID:RtcString);
    procedure RemGateClient(const GateAddr,GatePort:RtcString);
    function ClientStatus(const UserAddr,AccID:RtcString):integer;
    function BestClientStatus(const UserAddr:RtcString):integer;

    function MultiCli:TRtcHttpMultiGateClient;
    function Gateway:TRtcHttpGateway;

    procedure LoadConfig;
    procedure SaveConfig;
  end;

var
  RTCGateNodeForm: TRTCGateNodeForm;

implementation

{$R *.dfm}

function Get_ComputerName: RtcString;
var
  buf: array [0 .. 256] of AnsiChar;
  len: DWord;
begin
  len := sizeof(buf);
  GetComputerNameA(@buf, len);
  Result := RtcString(PAnsiChar(@buf));
end;

function Get_UserName: RtcString;
var
  buf: array [0 .. 256] of AnsiChar;
  len: DWord;
begin
  len := sizeof(buf);
  GetUserNameA(@buf, len);
  Result := RtcString(PAnsiChar(@buf));
end;

function LeftStr(const str:String; len:integer):String;
  var
    a:integer;
  begin
  if length(str)>=len then
    Result:=Copy(str,1,len)
  else
    begin
    Result:=str;
    SetLength(Result,len);
    for a:=length(str)+1 to len do
      Result[a]:=' ';
    end;
  end;

function KSeparate(const s:String):String;
  var
    i,len:integer;
  begin
  Result:='';
  i:=0;len:=length(s);
  while i<len do
    begin
    Result:=s[len-i]+Result;
    Inc(i);
    if (i mod 3=0) and (i<len) then Result:='.'+Result;
    end;
  end;

function USeparate(const s:String):String;
  var
    i,len:integer;
  begin
  Result:='';
  i:=0;len:=length(s);
  while i<len do
    begin
    Result:=s[len-i]+Result;
    Inc(i);
    if (i mod 3=0) and (i<len) then Result:='-'+Result;
    end;
  end;

function NiceUserAddr(const uaddr:String):String;
  var
    GateAddr,GatePort:RtcString;
    uid:TGateUID;
  begin
  uid:=SplitUserAddr(uaddr,GateAddr,GatePort);
  Result:=GateAddr+':'+GatePort+'/'+USeparate(Int2Str(uid));
  end;

procedure TRTCGateNodeForm.AddClient(const UserAddr,AccID:RtcString);
  var
    info:RtcWideString;
  begin
  info:=CombineUserAcc(UserAddr,AccID);
  if FClients.search(info)<=0 then
    begin
    case MultiCli.isType[AccID] of
      gat_Linked: FClients.insert(info,31);
      gat_Private: FClients.insert(info,21);
      else FClients.insert(info,10);
      end;
    eUsers.Items.Add(info);
    end;
  end;

procedure TRTCGateNodeForm.VerifiedClient(const UserAddr,AccID:RtcString);
  var
    info:RtcWideString;
    k:integer;
  begin
  info:=CombineUserAcc(UserAddr,AccID);
  k:=FClients.search(info);
  case k of
    30,31: FClients.change(info,35);
    20,21: FClients.change(info,25);
    end;
  eUsers.Repaint;
  end;

procedure TRTCGateNodeForm.UnVerifiedClient(const UserAddr,AccID:RtcString);
  var
    info:RtcWideString;
    k:integer;
  begin
  info:=CombineUserAcc(UserAddr,AccID);
  k:=FClients.search(info);
  case k of
    31,35: FClients.change(info,30);
    21,25: FClients.change(info,20);
    end;
  eUsers.Repaint;
  end;

function TRTCGateNodeForm.ClientStatus(const UserAddr,AccID:RtcString):integer;
  var
    info:RtcWideString;
  begin
  info:=CombineUserAcc(UserAddr,AccID);
  Result:=FClients.search(info);
  end;

procedure TRTCGateNodeForm.RemClient(const UserAddr,AccID:RtcString);
  var
    k:integer;
    info:RtcWideString;
  begin
  info:=CombineUserAcc(UserAddr,AccID);
  if FClients.search(info)>0 then
    begin
    FClients.remove(info);
    k:=eUsers.Items.IndexOf(info);
    if k>=0 then
      eUsers.Items.Delete(k);
    end;
  end;

procedure TRTCGateNodeForm.RemGateClient(const GateAddr,GatePort:RtcString);
  var
    UserAddr,ga,gp:RtcString;
    i,k:longint;
    info:RtcWideString;
  begin
  info:=FClients.search_min(i);
  while i>0 do
    begin
    SplitUserAcc(info,UserAddr);
    SplitUserAddr(UserAddr,ga,gp);
    if (ga=GateAddr) and (gp=GatePort) then
      begin
      FClients.remove(info);
      k:=eUsers.Items.IndexOf(info);
      if k>=0 then
        eUsers.Items.Delete(k);
      end;
    info:=FClients.search_g(info,i);
    end;
  end;

function TRTCGateNodeForm.BestClientStatus(const UserAddr:RtcString):integer;
  var
    GateAddr,GatePort:RtcString;
    uaddr,ga,gp:RtcString;
    UserID,uid:TGateUID;
    i:longint;
    info:RtcWideString;
  begin
  Result:=0;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  info:=FClients.search_min(i);
  while i>0 do
    begin
    SplitUserAcc(info,uaddr);
    uid:=SplitUserAddr(uaddr,ga,gp);
    if (uid=UserID) and (ga=GateAddr) and (gp=GatePort) then
      if i>Result then Result:=i;
    info:=FClients.search_g(info,i);
    end;
  end;

function TRTCGateNodeForm.OnlyClientText(const UInfo:RtcString):RtcString;
  var
    l:integer;
  begin
  l:=Pos('>',UInfo);
  if l>0 then
    Result:=Copy(UInfo,1,l-1)
  else
    Result:=UInfo;
  end;

function TRTCGateNodeForm.CombineAccID(const AccID:RtcString):RtcWideString;
  begin
  if length(AccID)>0 then
    begin
    case MultiCli.isType[AccID] of
      gat_Private: Result:='1';
      gat_Linked: Result:='2';
      else Result:='3';
      end;
    Result:=Result+LeftStr(MultiCli.DisplayName[accID],100);
    Result:=Result+AccID;
    end
  else
    Result:='';
  end;

function TRTCGateNodeForm.SplitAccID(const AccInfo:RtcWideString):RtcString;
  begin
  if length(AccInfo)>101 then
    Result:=Copy(AccInfo,102,length(AccInfo)-101)
  else
    Result:='';
  end;

function TRTCGateNodeForm.CombineUserAcc(const UserAddr,AccID:RtcString):RtcWideString;
  begin
  if (UserAddr<>'') and (AccID<>'') then
    begin
    case MultiCli.isType[AccID] of
      gat_Private: Result:='1';
      gat_Linked: Result:='2';
      else Result:='3';
      end;
    Result:=Result+LeftStr(MultiCli.DisplayName[accID],100);
    Result:=Result+LeftStr(OnlyClientText(MultiCli.UserInfo(UserAddr)),100);
    Result:=Result+AccID+'?'+UserAddr;
    end
  else
    Result:='';
  end;

function TRTCGateNodeForm.SplitUserAcc(const UserAcc:RtcWideString; var UserAddr:RtcString):RtcString;
  var
    j,x:integer;
  begin
  if length(UserAcc)>201 then
    begin
    x:=202;
    j:=PosEx('?',UserAcc,x);
    if j>0 then
      begin
      Result:=Copy(UserAcc,x,j-x);
      UserAddr:=Copy(UserAcc,j+1,length(UserAcc)-j);
      end
    else
      Result:='';
    end
  else
    Result:='';
  end;

procedure TRTCGateNodeForm.FormCreate(Sender: TObject);
  var
    Client:TRtcHttpGateClient;
    acc:RtcString;
  begin
  FClients:=tStringIntList.Create(8);

  GatePanHig:=pGateways.Height;
  InvitePanHig:=pInviteCap.Height;
  CfgPanWid:=pConfig.Width;
  WorkPanWid:=pWorkspace.Width;

  LastGateAddr:=''; // used for "btnConnect" after "btnDisconnect"
  ChangingGateway:=False; // used for "ShowMessage" only if manually starting the Gateway

  // We are using Gateway events to update the "btnLocalGateway" button
  with Gateway do
    begin
    OnListenStart:=GatewayListenStart;
    OnListenError:=GatewayListenError;
    OnListenStop:=GatewayListenStop;
    end;

  MultiLink.MultiClient:=MultiCli;

  // Load last saved configuration and activate all connections
  LoadConfig;

  { Update Gateway list using active
    connections on the MultiClient component ... }
  eGateways.Clear;
  Client:=MultiCli.FirstClient;
  while assigned(Client) do
    begin
    if Client.AutoLogin then
      if Client=MultiCli.Client then
        eGateways.ItemIndex:=eGateways.Items.Add(Client.GateAddr+':'+Client.GatePort)
      else
        eGateways.Items.Add(Client.GateAddr+':'+Client.GatePort);
    Client:=MultiCli.NextClient(Client);
    end;

  if eGateways.Items.Count>0 then
    begin
    eGateways.ItemIndex:=0;
    MultiCli.SelectClient(eGateways.Items[eGateways.ItemIndex]);
    end;

  { Update Account list using Accounts
    registered on the MultiClient component ... }
  eAccounts.Clear;
  acc:=MultiCli.FirstID;
  while acc<>'' do
    begin
    eAccounts.ItemIndex:=eAccounts.Items.Add(CombineAccID(acc));
    acc:=MultiCli.NextID(acc);
    end;

  if eAccounts.Items.Count>0 then
    eAccounts.ItemIndex:=0;

  // Activate Status Timer - used to redraw the "eGateways"
  sTimer.Enabled:=True;
  end;

procedure TRTCGateNodeForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  SaveConfig;
  sTimer.Enabled:=False; // disable Status Timer

  MultiCli.LogOutAndCleanUp; // close all connections to Gateways
  Gateway.Active:=False; // stop the Gateway

  RtcFreeAndNil(FClients);
  end;

procedure TRTCGateNodeForm.eGatewaysDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  LRect: TRect;
  LBackground, LColor: TColor;
  Client: TRtcHttpGateClient;
  txt: String;
  cx,wid:integer;
begin
  // Paint the "eGateway" item based on the current connection status
  Client:=MultiCli.FindClient(eGateways.Items[Index],True); // Find and lock Client component
  if assigned(Client) then with eGateways.Canvas do
    try
      // Draw item background ...
      FillRect(Rect);
      LBackground := Brush.Color;
      LColor:=Pen.Color;

      // Box width = Item height
      wid := Rect.Bottom - Rect.Top;

      // Calculate Input State Box rect ...
      LRect := Rect;
      LRect.Right := LRect.Left + wid;
      InflateRect(LRect, -2, -2);

      // Paint Input State box ...
      case Client.State.InputState of
        ins_Closed:     Brush.Color:=clRed;
        ins_Connecting: Brush.Color:=clYellow;
        ins_Prepare:    Brush.Color:=clBlue;
        ins_Start:      Brush.Color:=clGreen;
        ins_Idle:       Brush.Color:=clGreen;
        ins_Recv:       Brush.Color:=clLime;
        ins_Done:       Brush.Color:=clNavy;
        end;
      Pen.Color:=clWhite;
      Pen.Width:=1;
      Ellipse(LRect);

      if Client.State.InputState>=ins_Prepare then
        begin
        // Draw Input PING line ...
        InflateRect(LRect, -1, -1);
        Pen.Width:=2;
        case Client.State.InputState of
          ins_Prepare: Pen.Color:=clNavy;
          ins_Start:   Pen.Color:=clBlack;
          ins_Idle:    Pen.Color:=clBlack;
          ins_Recv:    Pen.Color:=clGreen;
          ins_Done:    Pen.Color:=clLime;
          end;
        MoveTo((LRect.Left+LRect.Right) div 2,(LRect.Top+LRect.Bottom) div 2);
        case Client.State.PingInCnt of
          0:LineTo(LRect.Right,LRect.Top);
          1:LineTo(LRect.Right,LRect.Bottom);
          2:LineTo(LRect.Left,LRect.Bottom);
          3:LineTo(LRect.Left,LRect.Top);
          end;
        InflateRect(LRect, 1, 1);
        end;

      // Calculate Output State Box rect ...
      LRect.Left := LRect.Right+4;
      LRect.Right := LRect.Right + wid;

      // Paint Output state box ...
      case Client.State.OutputState of
        outs_Connecting: Brush.Color:=clYellow;
        outs_Closed:     Brush.Color:=clRed;
        outs_Prepare:    Brush.Color:=clBlue;
        outs_Start:      Brush.Color:=clGreen;
        outs_Idle:       Brush.Color:=clGreen;
        outs_Send:       Brush.Color:=clLime;
        outs_Done:       Brush.Color:=clNavy;
        end;
      Pen.Color:=clWhite;
      Pen.Width:=1;
      Ellipse(LRect);

      if Client.State.OutputState>=outs_Prepare then
        begin
        // Draw Output PING line ...
        InflateRect(LRect, -1, -1);
        Pen.Width:=2;
        case Client.State.OutputState of
          outs_Prepare: Pen.Color:=clNavy;
          outs_Start:   Pen.Color:=clBlack;
          outs_Idle:    Pen.Color:=clBlack;
          outs_Send:    Pen.Color:=clGreen;
          outs_Done:    Pen.Color:=clLime;
          end;
        MoveTo((LRect.Left+LRect.Right) div 2,(LRect.Top+LRect.Bottom) div 2);
        case Client.State.PingOutCnt of
          0:LineTo(LRect.Right,LRect.Top);
          1:LineTo(LRect.Right,LRect.Bottom);
          2:LineTo(LRect.Left,LRect.Bottom);
          3:LineTo(LRect.Left,LRect.Top);
          end;
        InflateRect(LRect, 1, 1);
        end;

      // Prepare Display Text ...
      txt:=Client.GateAddr;

      // Use default background and text color ...
      Brush.Color := LBackground;
      Pen.Color := LColor;

      // Calculate Text box Rect ...
      Rect.Left := LRect.Right + 4;

      // Write Text ...
      Font.Style:=[fsBold];
      TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);

      Rect.Left:=Rect.Left+TextWidth(txt)+2;
      if Rect.Left<Rect.Right then
        begin
        txt:=':'+Client.GatePort;
        if Client.Ready then
          txt:=txt+'/'+USeparate(Int2Str(Client.MyUID));
        Font.Style:=[];
        TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);

        Rect.Left:=Rect.Left+TextWidth(txt)+10;
        if Rect.Left<Rect.Right then
          begin
          if Client.UseWinHTTP then
            txt:='H'
          else if Client.UseProxy then
            txt:='E'
          else if Client.UseBlocking then
            txt:='B'
          else
            txt:='A';
          if Client.StreamBlockSizeIn>0 then txt:=txt+'i';
          if Client.StreamBlockSizeOut>0 then txt:=txt+'o';

          txt:=txt+' '+KSeparate(Int2Str(Client.State.TotalReceived div 1024))+'Ki '+
                       KSeparate(Int2Str(Client.State.TotalSent div 1024))+'Ko';
          Font.Style:=[];
          cx:=Rect.Right-TextWidth(txt)-4;
          if Rect.Left<cx then Rect.Left:=cx;

          TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);
          end;
        end;

    finally
      // Unlock Client
      MultiCli.UnLockClient(Client);
      end;
  end;

procedure TRTCGateNodeForm.sTimerTimer(Sender: TObject);
  var
    v:integer;
  begin
  // Force a repaint to update connection states ...
  eGateways.Repaint;
  if pAuthCode.Visible then
    begin
    // Auth.Code counter update ...
    v:=MultiCli.AuthCodeValid;
    if v>0 then
      begin
      eAuthCode.Caption:='Auth.Code ('+IntToStr(v)+'s): '+USeparate(MultiCli.AuthCode);
      if MultiCli.Ready then
        begin
        eAuthGate.Caption:='Gateway: '+MultiCli.GateAddr+':'+MultiCli.GatePort;
        eAuthUser.Caption:='Receiver ID: '+USeparate(Int2Str(MultiCli.MyUID));
        end
      else
        begin
        eAuthGate.Caption:='NOT Connected to Gateway';
        eAuthUser.Caption:='Gateway connection required';
        end;
      end
    else
      pAuthCode.Visible:=False;
    end;
  end;

procedure TRTCGateNodeForm.btnConnectClick(Sender: TObject);
  var
    GateAddr:String;
  begin
  GateAddr:=LastGateAddr; // use last disconnected Address
  if InputQuery('Connect to Gateway','Gateway Address:Port',GateAddr) then
    begin
    GateAddr:=Trim(GateAddr);
    if GateAddr<>'' then
      begin
      { "SelectClient" method creates a new connection component
        if none of the existing Client components use GateAddr ... }
      MultiCli.SelectClient(GateAddr);
      if MultiCli.AutoLogin=False then // Not logging in = new component
        begin
        LastGateAddr:=''; // clear last address
        eGateways.ItemIndex:=eGateways.Items.Add(GateAddr); // Add this Gateway item to the List and select it
        MultiCli.AutoLogin:=True; // Start connecting
        end;
      end;
    end;
  end;

procedure TRTCGateNodeForm.btnDisconnectClick(Sender: TObject);
  var
    GateAddr:String;
  begin
  if (eGateways.Items.Count>0) and (eGateways.ItemIndex>=0) then
    begin
    GateAddr:=Trim(eGateways.Items[eGateways.ItemIndex]);
    if MessageDlg('Disconnect from Gateway "'+GateAddr+'" ?',mtWarning,[mbYes,mbNo],0)=mrYes then
      begin
      LastGateAddr:=GateAddr; // remember Gateway address, so we can use Connect if disconnected by mistake
      MultiCli.SelectClient(GateAddr); // Select Client component to Disconnect from
      if MultiCli.AutoLogin then
        begin
        MultiCli.AutoLogin:=False; // Disconnect
        eGateways.Items.Delete(eGateways.ItemIndex); // Remove it from the List of Gateways
        if eGateways.Items.Count>0 then // there are more Gateways in the list?
          begin
          eGateways.ItemIndex:=0; // select the top Gateway in the list
          MultiCli.SelectClient(eGateways.Items[0]); // select connection in the MultiClient
          end;
        end;
      end;
    end;
  end;

procedure TRTCGateNodeForm.eGatewaysClick(Sender: TObject);
  var
    GateAddr:String;
  begin
  if (eGateways.Items.Count>0) and (eGateways.ItemIndex>=0) then
    begin
    GateAddr:=Trim(eGateways.Items[eGateways.ItemIndex]);
    MultiCli.SelectClient(GateAddr); // select the current Gateway item in the MultiClient
    end;
  end;

procedure TRTCGateNodeForm.eGatewaysDblClick(Sender: TObject);
  var
    GateAddr:String;
  begin
  if (eGateways.Items.Count>0) and (eGateways.ItemIndex>=0) then
    begin
    GateAddr:=Trim(eGateways.Items[eGateways.ItemIndex]);
    MultiCli.SelectClient(GateAddr); // select the current Gateway item in the MultiClient
    RTCPModule.ChangeProvider; // Request a Provider change for the currently selected Client
    end;
  end;

procedure TRTCGateNodeForm.btnLocalGatewayClick(Sender: TObject);
  var
    GatePort:String;
  begin
  if Gateway.Active then
    begin
    if MessageDlg('Stop Local Gateway'#13#10'running on Port '+Gateway.GatePort+'?',mtWarning,[mbYes,mbNo],0)=mrYes then
      begin
      ChangingGateway:=True; // manually changing Gateway parameters
      Gateway.Active:=False; // Stop the Gateway
      end;
    end
  else
    begin
    GatePort:=Gateway.GatePort;
    if InputQuery('Start Local Gateway','Gateway Port',GatePort) then
      begin
      GatePort:=Trim(GatePort);
      if GatePort<>'' then
        begin
        ChangingGateway:=True; // manually changing Gateway parameters
        Gateway.GatePort:=GatePort; // set Gateway Port
        Gateway.Active:=True; // start the Gateway
        end;
      end;
    end;
  end;

procedure TRTCGateNodeForm.GatewayListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then // Synchronize with the Main Thread ...
    Sender.Sync(GatewayListenError,E)
  else
    begin
    // Update button ...
    cLocalGateway.Brush.Color:=clRed;
    btnLocalGateway.Caption:='Start Gate';
    btnLocalGateway.Hint:='Start Local Gateway';
    cLocalGateway.Hint:=btnLocalGateway.Hint;
    if ChangingGateway then
      begin
      // Show detailed message to the user ONLY if the Gateway was started manually ...
      ChangingGateway:=False;
      ShowMessage('Error starting Local Gateway on Port '+Sender.ServerPort+#13#10+E.Message);
      end;
    end;
  end;

procedure TRTCGateNodeForm.GatewayListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then // Synchronize with the Main Thread ...
    Sender.Sync(GatewayListenStart)
  else
    begin
    // Update button ...
    cLocalGateway.Brush.Color:=clGreen;
    btnLocalGateway.Caption:='on Port '+Sender.LocalPort;
    btnLocalGateway.Hint:='Local Gateway is running'#13#10+
                          'on Port '+Sender.LocalPort+', click to STOP';
    cLocalGateway.Hint:=btnLocalGateway.Hint;
    if ChangingGateway then
      begin
      MultiCli.SelectClient('localhost:'+Sender.LocalPort);
      if MultiCli.AutoLogin=False then
        begin
        MultiCli.AutoLogin:=True; // Connect to the Gateway
        eGateways.ItemIndex:=eGateways.Items.Add(MultiCli.MyAddr); // Add Gateway to the list
        end;
      ChangingGateway:=False;
      end;
    end;
  end;

procedure TRTCGateNodeForm.GatewayListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then // Synchronize with the Main Thread ...
    Sender.Sync(GatewayListenStop)
  else
    begin
    // Update button ...
    cLocalGateway.Brush.Color:=clMaroon;
    btnLocalGateway.Caption:='Start Gate';
    btnLocalGateway.Hint:='Start Local Gateway';
    cLocalGateway.Hint:=btnLocalGateway.Hint;
    if ChangingGateway then
      ChangingGateway:=False;
    end;
  end;

function TRTCGateNodeForm.MultiCli: TRtcHttpMultiGateClient;
  begin
  Result:=RTCPModule.MultiClient;
  end;

function TRTCGateNodeForm.Gateway: TRtcHttpGateway;
  begin
  Result:=RTCPModule.Gateway;
  end;

procedure TRTCGateNodeForm.LoadConfig;
  var
    r:TRtcRecord;
    s:RtcString;
  begin
  s:=Read_File(ExtractFilePath(AppFileName)+ChangeFileExt(ExtractFileName(AppFileName),'.inf'));
  if s<>'' then
    begin
    try
      r:=TRtcRecord.FromCode(s);
      try
        Position:=poDesigned;
        Left:=r.asInteger['Main.X'];
        Top:=r.asInteger['Main.Y'];
        ClientWidth:=r.asInteger['Main.W'];
        ClientHeight:=r.asInteger['Main.H'];
        if r.asBoolean['Work.R'] then
          begin
          pWorkspace.Align:=alRight;
          WorkSplitter.Align:=alRight;
          pConfig.Align:=alClient;
          end;
        pConfig.Width:= r.asInteger['Cfg.W'];
        pGateways.Height:=r.asInteger['Cfg.H'];
        pInvitations.Height:=r.asInteger['Work.H'];
        pWorkspace.Width:=r.asInteger['Work.W'];
        CfgPanWid:=r.asInteger['Cfg.SW'];
        GatePanHig:=r.asInteger['Cfg.SH'];
        InvitePanHig:=r.asInteger['Work.SH'];
        WorkPanWid:=r.asInteger['Work.SW'];
        MyUserName:=r.asText['Name'];
      finally
        r.Free;
        end;
    except
      // ignore bad cfg file exceptions
      end;
    end;

  if MyUserName='' then
    MyUserName:=Get_UserName+'/'+Get_ComputerName;

  eUserName.Caption:=MyUserName;
  MultiCli.GateUserInfo:=MyUserName+'>'+MultiCli.PublicID;

  // Load configuration (connections to Gateways, Local Gateway status and Account info) from file ...
  s:=Read_File(ExtractFilePath(AppFileName)+ChangeFileExt(ExtractFileName(AppFileName),'.ini'));
  if s<>'' then
    try
      RTCPModule.Config:=s;
    except
      // ignore bad file exceptions
      end;
  end;

procedure TRTCGateNodeForm.SaveConfig;
  var
    r:TRtcRecord;
  begin
  // Save configuration (connections to Gateways, Local Gateway status and Account info) to file ...
  Write_File(ExtractFilePath(AppFileName)+ChangeFileExt(ExtractFileName(AppFileName),'.ini'), RTCPModule.Config);
  r:=TRtcRecord.Create;
  try
    r.asInteger['Main.X']:=Left;
    r.asInteger['Main.Y']:=Top;
    r.asInteger['Main.W']:=ClientWidth;
    r.asInteger['Main.H']:=ClientHeight;
    r.asInteger['Cfg.W']:=pConfig.Width;
    r.asInteger['Cfg.SW']:=CfgPanWid;
    r.asInteger['Cfg.H']:=pGateways.Height;
    r.asInteger['Cfg.SH']:=GatePanHig;
    r.asInteger['Work.H']:=pInvitations.Height;
    r.asInteger['Work.SH']:=InvitePanHig;
    r.asInteger['Work.W']:=pWorkspace.Width;
    r.asInteger['Work.SW']:=WorkPanWid;
    r.asBoolean['Work.R']:=WorkSplitter.Align=alRight;
    r.asText['Name']:=MyUserName;
    Write_File(ExtractFilePath(AppFileName)+ChangeFileExt(ExtractFileName(AppFileName),'.inf'), r.toCode);
  finally
    r.Free;
    end;
  end;

procedure TRTCGateNodeForm.btnNewKeyClick(Sender: TObject);
  var
    LocalName,RemoteName:RtcWideString;
    AccID:RtcString;
  begin
  LocalName:=Get_UserName;
  RemoteName:='';
  if MakeAccountQuery('Create a new KEY Account','Create KEY',LocalName,RemoteName) then
    begin
    AccID:=MultiCli.GeneratePrivate(RemoteName,True);
    MultiCli.LocalName[AccID]:=LocalName;
    if eAccounts.Items.IndexOf(CombineAccID(AccID))<0 then
      eAccounts.ItemIndex:=eAccounts.Items.Add(CombineAccID(AccID)); // Add this Account to the List and select it
    end;
  end;

procedure TRTCGateNodeForm.btnJoinPublicClick(Sender: TObject);
  var
    DispName:String;
    AccID:RtcString;
  begin
  DispName:='';
  if InputQuery('Join a Public Group','Group Name',DispName) then
    begin
    DispName:=Trim(DispName);
    if DispName<>'' then
      begin
      if MultiCli.FindPublic(DispName)='' then
        begin
        AccID:=MultiCli.RegisterPublic(DispName,True);
        if eAccounts.Items.IndexOf(CombineAccID(AccID))<0 then
          eAccounts.ItemIndex:=eAccounts.Items.Add(CombineAccID(AccID)); // Add this Account to the List and select it
        end;
      end;
    end;
  end;

procedure TRTCGateNodeForm.eAccountsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  LRect: TRect;
  LBackground, LColor: TColor;
  accID:RtcString;
  txt: String;
  imgid,wid:integer;
begin
  accID:=SplitAccID(eAccounts.Items[Index]);
  if accID='' then Exit;
  // Paint the "eAccounts" item based on the Account information
  if (MultiCli.isType[accID]<>gat_None) then // Check if the Account exists
    with eAccounts.Canvas do
      begin
      // Draw item background ...
      FillRect(Rect);
      LBackground := Brush.Color;
      LColor:=Pen.Color;

      // Box width = Item height
      wid := Rect.Bottom - Rect.Top+5;

      // Calculate Input State Box rect ...
      LRect := Rect;
      LRect.Right := wid + LRect.Left;
      InflateRect(LRect, -2, -2);

      // Paint Account state (ON=Green/OFF=Red) ...
      if MultiCli.isActive[accID] then
        begin
        Images.Draw(eAccounts.Canvas,LRect.Left,LRect.Top,1);
        case MultiCli.isType[accID] of
          gat_Public: imgid:=3;
          gat_Private: imgid:=5;
          gat_Linked: imgid:=7;
          else imgid:=0;
          end;
        end
      else
        begin
        Images.Draw(eAccounts.Canvas,LRect.Left,LRect.Top,0);
        case MultiCli.isType[accID] of
          gat_Public: imgid:=2;
          gat_Private: imgid:=4;
          gat_Linked: imgid:=6;
          else imgid:=0;
          end;
        end;

      LRect.Left:=LRect.Right;
      LRect.Right:=LRect.Right+wid;

      Images.Draw(eAccounts.Canvas,LRect.Left,LRect.Top,imgid);

      // Prepare Display Text ...
      txt:=MultiCli.RemoteName[accID];

      // Use default background and text color ...
      Brush.Color := LBackground;
      Pen.Color := LColor;

      // Calculate Text box Rect ...
      Rect.Left := LRect.Right + 2;

      // Write Text ...
      Font.Style:=[fsBold];
      TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);

      if MultiCli.LocalName[accID]<>MultiCli.RemoteName[accID] then
        begin
        Rect.Left := Rect.Left + TextWidth(txt) + 10;
        if Rect.Left < Rect.Right then
          begin
          case MultiCli.isType[accID] of
            gat_Private: txt:='by '+MultiCli.LocalName[accID];
            gat_Linked:  txt:='for '+MultiCli.LocalName[accID];
            else txt:='';
            end;
          Font.Style:=[];
          TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);
          end;
        end;
      end;
  end;

procedure TRTCGateNodeForm.eAccountsDblClick(Sender: TObject);
  var
    AccID:RtcString;
    wasActive:boolean;
  begin
  if (eAccounts.Items.Count>0) and (eAccounts.ItemIndex>=0) then
    begin
    AccID:=SplitAccID(eAccounts.Items[eAccounts.ItemIndex]);
    wasActive:=MultiCli.isActive[AccID];
    MultiCli.isActive[AccID]:=not wasActive; // change Account's "Active" state
    eAccounts.Repaint;
    eUsers.Repaint;
    end;
  end;

procedure TRTCGateNodeForm.btnDeleteClick(Sender: TObject);
  var
    AccID,AccName,AccType:RtcString;
  begin
  if (eAccounts.Items.Count>0) and (eAccounts.ItemIndex>=0) then
    begin
    AccID:=SplitAccID(eAccounts.Items[eAccounts.ItemIndex]);
    AccName:=MultiCli.DisplayName[AccID];
    if AccName<>'' then
      begin
      case MultiCli.isType[AccID] of
        gat_Public: AccType:='Leave Public Group';
        gat_Private: AccType:='Delete KEY Account';
        gat_Linked: AccType:='Delete Account LINK';
        end;
      if MessageDlg(AccType+' "'+AccName+'" ?',mtWarning,[mbYes,mbNo],0)=mrYes then
        begin
        eAccounts.Items.Delete(eAccounts.ItemIndex); // Remove it from the List of Accounts
        MultiCli.DeleteID(AccID); // Delete Account
        if eAccounts.Items.Count>0 then // there are more Accounts in the list?
          eAccounts.ItemIndex:=0; // select the top Account in the list
        end;
      end;
    end;
  end;

procedure TRTCGateNodeForm.eUsersDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  var
    LRect: TRect;
    LBackground, LColor: TColor;
    UserAddr,AccID:RtcString;
    txt: String;
    wid:integer;
    cx,imgid:integer;
  begin
  txt:=eUsers.Items[Index];
  if txt='' then Exit;
  AccID:=SplitUserAcc(txt,UserAddr);
  // Paint the "eUsers" item based on User information
  if (MultiCli.isType[accID]<>gat_None) then // Check if the Account exists
    with eUsers.Canvas do
      begin
      // Draw item background ...
      FillRect(Rect);
      LBackground := Brush.Color;
      LColor:=Pen.Color;

      // Box width = Item height
      wid := Rect.Bottom - Rect.Top+5;

      // Calculate Input State Box rect ...
      LRect := Rect;
      LRect.Right := wid + LRect.Left;
      InflateRect(LRect, -2, -2);

      // Paint Account type icon
      case ClientStatus(UserAddr,AccID) of
        31:imgid:=4; // unverified Private
        35:imgid:=5; // verified Private
        21:imgid:=6; // unverified Link
        25:imgid:=7; // verified Link
        10:imgid:=3; // Public
        else imgid:=0; // Bad Verification
        end;

      Images.Draw(eUsers.Canvas,LRect.Left,LRect.Top,imgid);

      // Use default background and text color ...
      Brush.Color := LBackground;
      Pen.Color := LColor;

      // Calculate Text box Rect ...
      Rect.Left := LRect.Right + 5;

      // Write Text ...
      Font.Style:=[fsBold];
      if MultiCli.ActiveCount>1 then
        begin
        txt:=MultiCli.RemoteName[accID];
        TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);
        Rect.Left:=Rect.Left+TextWidth(txt)+10;
        end;

      if Rect.Left<Rect.Right then
        begin
        Font.Style:=[];
        txt:=OnlyClientText(MultiCli.UserInfo(UserAddr));
        TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);
        Rect.Left:=Rect.Left+TextWidth(txt)+10;
        if Rect.Left<Rect.Right then
          begin
          txt:='> '+NiceUserAddr(UserAddr);
          cx:=Rect.Right-TextWidth(txt)-4;
          if Rect.Left<cx then Rect.Left:=cx;
          TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(txt)) div 2, txt);
          end;
        end;
      end;
  end;

procedure TRTCGateNodeForm.eAuthUserClick(Sender: TObject);
  begin
  MultiCli.AuthCodeReset;
  sTimerTimer(nil);
  end;

procedure TRTCGateNodeForm.btnSendLinkClick(Sender: TObject);
  var
    AccID:RtcString;
    AccName:RtcWideString;
    GateAddr:RtcString;
    RecID,RecAuth:RtcString;
    res:boolean;
  begin
  if (eAccounts.Items.Count>0) and (eAccounts.ItemIndex>=0) and MultiCli.Ready then
    begin
    AccID:=SplitAccID(eAccounts.Items[eAccounts.ItemIndex]);
    AccName:=MultiCli.DisplayName[AccID];
    GateAddr:=MultiCli.GateAddr+':'+MultiCli.GatePort;
    if AccName<>'' then
      begin
      RecID:='';
      RecAuth:='';
      case MultiCli.isType[AccID] of
        gat_Public:  res:=SendAccountQuery('Send Group Invite','INVITE',AccName,GateAddr,RecAuth,RecID);
        gat_Private,
        gat_Linked:  res:=SendAccountQuery('Send LINK (Invite)','Send LINK',AccName,GateAddr,RecAuth,RecID);
        else res:=False;
        end;
      if res and (length(RecID)>0) and (length(RecAuth)>0) then
        MultiCli.SendAccountData(RecID+'@'+GateAddr,RecAuth,MultiCli.PrepareLink(AccID));
      end;
    end;
  end;

procedure TRTCGateNodeForm.btnSendCopyClick(Sender: TObject);
  var
    AccID:RtcString;
    AccName:RtcWideString;
    GateAddr:RtcString;
    RecID,RecAuth:RtcString;
    res:boolean;
  begin
  if (eAccounts.Items.Count>0) and (eAccounts.ItemIndex>=0) and MultiCli.Ready then
    begin
    AccID:=SplitAccID(eAccounts.Items[eAccounts.ItemIndex]);
    AccName:=MultiCli.DisplayName[AccID];
    GateAddr:=MultiCli.GateAddr+':'+MultiCli.GatePort;
    if AccName<>'' then
      begin
      RecID:='';
      RecAuth:='';
      case MultiCli.isType[AccID] of
        gat_Public:  res:=SendAccountQuery('Send Group (Copy)','Send Group',AccName,GateAddr,RecAuth,RecID);
        gat_Private: res:=SendAccountQuery('Send KEY (Copy)','Send KEY',AccName,GateAddr,RecAuth,RecID);
        gat_Linked:  res:=SendAccountQuery('Send LINK (Copy)','Send LINK',AccName,GateAddr,RecAuth,RecID);
        else res:=False;
        end;
      if res and (length(RecID)>0) and (length(RecAuth)>0) then
        MultiCli.SendAccountData(RecID+'@'+GateAddr,RecAuth,MultiCli.PrepareCopy(AccID,True));
      end;
    end;
  end;

procedure TRTCGateNodeForm.pInfoPanelClick(Sender: TObject);
  begin
  pInfoPanel.Visible:=False;
  end;

procedure TRTCGateNodeForm.btnReceiveClick(Sender: TObject);
  begin
  if pAuthCode.Visible then
    pAuthCode.Visible:=False
  else
    begin
    MultiCli.AuthCodeReset;
    pAuthCode.Visible:=True;
    if pAccounts.Height<pAuthCode.Top+pAuthCode.Height then
      pAccounts.Height:=pAuthCode.Top+pAuthCode.Height;
    sTimerTimer(nil);
    end;
  end;

procedure TRTCGateNodeForm.eUsersClick(Sender: TObject);
  var
    txt,AccID,UserAddr:RtcString;
  begin
  if (eUsers.Items.Count>0) and (eUsers.ItemIndex>=0) then
    begin
    txt:=eUsers.Items[eUsers.ItemIndex];
    if txt<>'' then
      begin
      AccID:=SplitUserAcc(txt,UserAddr);
      if MultiCli.AccountNotVerified(UserAddr,AccID) then
        MultiCli.RequestAccountVerification(UserAddr,AccID);
      end;
    end;
  end;

procedure TRTCGateNodeForm.cLocalGatewayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  btnLocalGateway.Click;
  end;

procedure TRTCGateNodeForm.eUserNameClick(Sender: TObject);
  var
    Client:TRtcHttpGateClient;
  begin
  if InputQuery('Change Name (Reset Connections)','My New Name',MyUserName) then
    begin
    if MyUserName='' then
      MyUserName:=Get_UserName+'/'+Get_ComputerName;
    eUserName.Caption:=MyUserName;
    MultiCli.GateUserInfo:=MyUserName+'>'+MultiCli.PublicID;
    Client:=MultiCli.FirstClient(True);
    while assigned(Client) do
      begin
      if Client.AutoLogin then
        begin
        Client.GateUserInfo:=MyUserName+'>'+MultiCli.PublicID;
        Client.Active:=False;
        Client.Active:=True;
        repeat
          Sleep(1);
          Application.ProcessMessages;
          until Client.Ready;
        end;
      MultiCli.UnLockClient(Client);
      Client:=MultiCli.NextClient(Client,True);
      end;
    end;
  end;

procedure TRTCGateNodeForm.btnSaveConfigClick(Sender: TObject);
  begin
  SaveConfig;
  end;

procedure TRTCGateNodeForm.pInviteCapClick(Sender: TObject);
  begin
  if pInvitations.Height>pInviteCap.Height+pInviteCap.Top+20 then
    begin
    InvitePanHig:=pInvitations.Height;
    pInvitations.Height:=pInviteCap.Height+pInviteCap.Top;
    end
  else
    pInvitations.Height:=InvitePanHig;
  end;

procedure TRTCGateNodeForm.pUserCapClick(Sender: TObject);
  var
    turned:boolean;
  begin
  turned:=False;
  if WorkSplitter.Align<>alRight then
    begin
    turned:=True;
    pWorkspace.Align:=alRight;
    WorkSplitter.Align:=alRight;
    pConfig.Align:=alClient;
    end;
  if pWorkspace.Width>50 then
    begin
    if not turned then
      WorkPanWid:=pWorkspace.Width;
    pWorkspace.Width:=50;
    end
  else if WorkPanWid<ClientWidth-50 then
    pWorkspace.Width:=WorkPanWid
  else
    pWorkspace.Width:=ClientWidth-50;
  end;

procedure TRTCGateNodeForm.pGatewayCapClick(Sender: TObject);
  begin
  if pGateways.Height>pGatewayCap.Height+pGatewayCap.Top+20 then
    begin
    GatePanHig:=pGateways.Height;
    pGateways.Height:=pGatewayCap.Height+pGatewayCap.Top;
    end
  else
    pGateways.Height:=GatePanHig;
  end;

procedure TRTCGateNodeForm.pAccountCapClick(Sender: TObject);
  var
    turned:boolean;
  begin
  turned:=False;
  if WorkSplitter.Align<>alLeft then
    begin
    pConfig.Align:=alLeft;
    WorkSplitter.Align:=alLeft;
    pWorkspace.Align:=alClient;
    turned:=True;
    end;
  if pConfig.Width>50 then
    begin
    if not turned then
      CfgPanWid:=pConfig.Width;
    pConfig.Width:=50;
    end
  else if CfgPanWid<ClientWidth-50 then
    pConfig.Width:=CfgPanWid
  else
    pConfig.Width:=ClientWidth-50;
  end;

procedure TRTCGateNodeForm.pGatewaysResize(Sender: TObject);
  begin
  if pGateways.Width<100 then
    pGatewayCap.Caption:='Gate'
  else
    pGatewayCap.Caption:='Gateways';
  tbGateways.Visible:=(pGateways.Height>tbGateways.Top+tbGateways.Height+42);
  eGateways.Repaint;
  end;

procedure TRTCGateNodeForm.pAccountsResize(Sender: TObject);
  begin
  if pAccounts.Width<100 then
    pAccountCap.Caption:='Acc.'
  else
    pAccountCap.Caption:='Accounts';
  if pAuthCode.Visible then
    pAuthCode.Visible:=(pAccounts.Height>=pAuthCode.Top+pAuthCode.Height);
  tbAccounts.Visible:=(pAccounts.Height>tbAccounts.Top+tbAccounts.Height+42) and
                      (pAccounts.Width>=100);
  eAccounts.Repaint;
  end;

procedure TRTCGateNodeForm.pInvitationsResize(Sender: TObject);
  begin
  if pInvitations.Width<100 then
    pInviteCap.Caption:='Inv.'
  else
    pInviteCap.Caption:='Invitations';
  eInvite.Repaint;
  end;

procedure TRTCGateNodeForm.pUsersResize(Sender: TObject);
  begin
  if pUsers.Width<100 then
    pUserCap.Caption:='Con.'
  else
    pUserCap.Caption:='Connections';
  eUsers.Repaint;
  end;

procedure TRTCGateNodeForm.MultiLinkAfterLogOutGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  RemGateClient(State.GateAddr,State.GatePort);
  end;

procedure TRTCGateNodeForm.MultiLinkInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  case Data.Command of
    gc_AccountLogIn,
    gc_AccountLogOut,
    gc_AccountReceived,
    gc_AccountTransferred,
    gc_AccountBadReceive,
    gc_AccountBadTransfer,
    gc_AccountVerified,
    gc_AccountBadVerify: Wanted:=True;
    end;
  end;

procedure TRTCGateNodeForm.MultiLinkInfoReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  var
    s:RtcWideString;
    i:integer;
  begin
  case Data.Command of
    gc_AccountLogIn:
      AddClient(Data.UserAddr,Data.AccountID);
    gc_AccountLogOut:
      RemClient(Data.UserAddr,Data.AccountID);
    gc_AccountReceived:
      begin
      s:=CombineAccID(Data.AccountID);
      i:=eAccounts.Items.IndexOf(s);
      if i<0 then
        eAccounts.ItemIndex:=eAccounts.Items.Add(s);

      case MultiCli.isType[Data.AccountID] of
        gat_Public:  s:='Group';
        gat_Private: s:='KEY';
        gat_Linked:  s:='LINK';
        end;
      eInfoPanel.Font.Color:=clGreen;
      eInfoPanel.Caption:=s+' "'+MultiCli.DisplayName[Data.AccountID]+'"'#13#10+
                         'received from '+Data.UserAddr;
      pInfoPanel.Visible:=True;

      // Generate a new AuthCode to avoid code re-use
      MultiCli.AuthCodeReset;
      pAuthCode.Visible:=False;
      end;
    gc_AccountTransferred:
      begin
      s:=CombineAccID(Data.AccountID);
      eAccounts.ItemIndex:=eAccounts.Items.IndexOf(s);
      eInfoPanel.Font.Color:=clGreen;
      eInfoPanel.Caption:='Account "'+MultiCli.DisplayName[Data.AccountID]+'"'#13#10+
                          'sent to '+Data.UserAddr;
      pInfoPanel.Visible:=True;
      end;
    gc_AccountBadReceive:
      begin
      eInfoPanel.Font.Color:=clRed;
      eInfoPanel.Caption:='BAD Account data received'#13#10+
                          'from '+Data.UserAddr;
      pInfoPanel.Visible:=True;
      end;
    gc_AccountBadTransfer:
      begin
      eInfoPanel.Font.Color:=clRed;
      eInfoPanel.Caption:='FAILED Account transfer'#13#10+
                          'to '+Data.UserAddr;
      pInfoPanel.Visible:=True;
      end;
    gc_AccountVerified:
      begin
      VerifiedClient(Data.UserAddr,Data.AccountID);
      end;
    gc_AccountBadVerify:
      begin
      UnVerifiedClient(Data.UserAddr,Data.AccountID);
      end;
    end;
  end;

procedure TRTCGateNodeForm.MultiLinkStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  RemGateClient(State.GateAddr,State.GatePort);
  end;

end.
