unit MainGateClientForm3;

interface

{$include rtcDefs.inc}

{ This Chat Client example works with the "RTCSimpleGateway" example Project and
  shows how "TRtcHttpMultiGateClient" and "TRtcMultiGateClientLink" components can be
  used to work with multiple RTC Gateways (each Gateway running on a different Address).

  For ONLINE/OFFLINE notifications about other CHAT users, this Chat Client uses
  the "MultiCli:TRtcHttpMultiGateClient" component to register and activate a Public Account
  with the Display Name "Lobby". Public Accounts are used for global notifications.
  Because all Chat Clients will be registering and activating the same Public Account,
  they will all be notified about each other during login/logout.

  Please note that ChatClients will NOT see their own ID in the "ONLINE" List after Login.
  You need at least 2 ChatClients running to see how ONLINE/OFFLINE notification works. 

  This Chat implementation allows free creation of Rooms.
  Any user inside a Chat Room can invite any other user by using
  the "INVITE" button and entering the ID of that user. }

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  ExtCtrls, Buttons,

  rtcSystem, rtcInfo, rtcLog, rtcConn,

  rtcGateConst,
  rtcGateCli,
  rtcDataCli,

  rtcSrcList,
  rtcCrypt,

  ChatHostForm,
  ChatCIDs;

type
  TMsgType=(msg_Input,msg_Output,msg_Speed,msg_Error,msg_Status,msg_Group);

type
  TGateClientForm = class(TForm)
    MainPanel: TPanel;
    StatusUpdate: TTimer;
    InfoPanel: TPanel;
    l_Status1: TLabel;
    l_Status2: TLabel;
    Panel1: TPanel;
    shInput: TShape;
    lblRecvBufferSize: TLabel;
    lblSendBuffSize: TLabel;
    shOutput: TShape;
    eYourID: TEdit;
    Label1: TLabel;
    eMyGroup: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    eInGroup: TListBox;
    btnCLR: TLabel;
    l_Groups: TLabel;
    l_Status3: TLabel;
    Bevel1: TBevel;
    btnHostChat: TButton;
    Bevel2: TBevel;
    eChatUsers: TListBox;
    btnReset: TSpeedButton;
    lblInvites: TLabel;
    lblGroups: TLabel;
    eOnlineUsers: TListBox;
    lblOnline: TLabel;
    MultiCli: TRtcHttpMultiGateClient;
    ChatLink: TRtcMultiGateClientLink;
    eGateways: TListBox;
    Label4: TLabel;
    btnNewGateway: TButton;
    btnRemoveGateway: TButton;
    
    procedure btnCLRClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure StatusUpdateTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure eMyGroupDblClick(Sender: TObject);
    procedure eInGroupDblClick(Sender: TObject);
    procedure btnHostChatClick(Sender: TObject);

    procedure eChatUsersDblClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure eOnlineUsersDblClick(Sender: TObject);
    procedure eOnlineUsersClick(Sender: TObject);
    procedure btnRemoveGatewayClick(Sender: TObject);
    procedure eGatewaysClick(Sender: TObject);
    procedure btnNewGatewayClick(Sender: TObject);
    procedure eGatewaysDblClick(Sender: TObject);
    procedure MultiCliAfterLoggedInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure MultiCliAfterLoginFailGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure MultiCliAfterLogOutGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure MultiCliBeforeLogInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure MultiCliDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure MultiCliInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure MultiCliInfoReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
    procedure MultiCliReadyToSend(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure MultiCliStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure ChatLinkDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure ChatLinkDataReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
    procedure MultiCliInfoReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);

  public
    FCS:TRtcCritSec;
    sStatus1,
    sStatus2,
    sStatus3,
    sGroups:String;

    LobbyAccount:RtcString;

    FLoginStart:TAppRunTime;
    CntReset:integer;

    InGroupCnt,OutGroupCnt:integer;

    FChatUsers,
    FScreenUsers,
    FFileUsers:TStrList;

    NeedProviderChange:boolean;

    procedure PrintMsg(const s:String; t:TMsgType);

    procedure CleanUpLists(cli:TRtcHttpGateClient);
  end;

var
  GateClientForm: TGateClientForm;

implementation

{$R *.dfm}

function FillZero(const s:RtcString;len:integer):RtcString;
  begin
  Result:=s;
  while length(Result)<len do
    Result:='0'+Result;
  end;

function Time2Str(v:TDateTime):RtcString;
  var
    hh,mm,ss,ms:word;
  begin
  DecodeTime(v, hh,mm,ss,ms);
  Result:=FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2);
  end;

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

procedure TGateClientForm.FormCreate(Sender: TObject);
  var
    MyAcc,MyLink:RtcString;
    MyData,RndSig,MySign,MyRes:RtcByteArray;
    a:integer;
  {$IFDEF RTC_RSA}
    Rnd:TRtcISAAC;
    i:integer;
  {$ENDIF}
  begin
  StartLog;

  NeedProviderChange:=False;

  FCS:=TRtcCritSec.Create;
  sStatus1:='';
  sStatus2:='';
  sStatus3:='';
  sGroups:='';

  FChatUsers:=tStrList.Create(16);
  FScreenUsers:=tStrList.Create(16);
  FFileUsers:=tStrList.Create(16);

  SetLength(MyData,0);
  SetLength(RndSig,0);
  SetLength(MyRes,0);
  SetLength(MySign,0);
  MyAcc:='';
  // Load Accounts data from a file if it exists (for testing)
  if File_Exists('accounts.txt') then
    begin
    MultiCli.RegisterFromCode(Read_FileEx('accounts.txt'),True,True);
    // Find Public "Lobby" Account
    LobbyAccount := MultiCli.FindPublic('Lobby');
    end
  else
    begin
    // Register and activate Public "Lobby" Account
    LobbyAccount := MultiCli.RegisterPublic('Lobby',True);

    // Register and activate Public "Hallway" Account
    MultiCli.RegisterPublic('Hallway',True);

    // Testing Private and Linked Accounts ...
    // Generate and activate Private account "Me"
    MyAcc := MultiCli.GeneratePrivate('Me',True);

    // Prepare a Link to our Private account
    MyData := MultiCli.PrepareLink(MyAcc);
    // Normally, CheckAccount and RegisterAccount would be
    // used by a remote Client to establish a link to this Private account
    MultiCli.CheckAccount(MyData);
    MyLink := MultiCli.RegisterAccount(MyData,True,True);

    // Store Account data to a local file (for testing)
    Write_FileEx('accounts.txt',MultiCli.SaveToCode);

    { Testing Private Signature, linked signature Verification,
      Private and Public Encryption and Decryprion routines ... }

  {$IFDEF RTC_RSA}
    // Generate a random array of bytes
    Rnd:=TRtcISAAC.Create(True);
    RndSig:=Rnd.RND(1024);
    Rnd.Free;

    // Make Private Account Signature (used for remote verification)
    MySign:=MultiCli.MakeSignature(MyAcc,RndSig);
    // Verify Private Account Signature using the Linked account
    if not MultiCli.VerifySignature(MyLink,RndSig,MySign) then
      raise Exception.Create('Signature verification failed!');

    // Encrypt Random Bytes using Public key from a linked Private account
    MySign:=MultiCli.PrivateEncrypt(MyLink,RndSig);
    // Decrypt bytes using Private key from our Private account
    MyRes:=MultiCli.PrivateDecrypt(MyAcc,MySign);
    // Check if original content matches decrypted content
    if length(MyRes)<>length(RndSig) then
      raise Exception.Create('Private Encrypt/Decrypt failed! Size missmatch');
    for i:=0 to length(MyRes)-1 do
      if MyRes[i]<>RndSig[i] then
        raise Exception.Create('Private Encrypt/Decrypt error at Byte '+IntToStr(i)+'!');

    // Encrypt Random Bytes using our temporary Public key
    MySign:=MultiCli.PublicEncrypt(MultiCli.PublicKey, RndSig);
    // Decrypt bytes using our temporary Private key
    MyRes:=MultiCli.PublicDecrypt(MySign);
    // Check if original content matches decrypted content
    if length(MyRes)<>length(RndSig) then
      raise Exception.Create('Public Encrypt/Decrypt failed! Size missmatch');
    for i:=0 to length(MyRes)-1 do
      if MyRes[i]<>RndSig[i] then
        raise Exception.Create('Public Encrypt/Decrypt error at Byte '+IntToStr(i)+'!');
  {$ENDIF}
    end;

  eMyGroup.Clear;
  eInGroup.Clear;
  eOnlineUsers.Clear;
  eChatUsers.Clear;

  // Test for sending "UserInfo" ...
  MultiCli.GateUserInfo := Get_UserName+'/'+Get_ComputerName;
  
  for a:=eGateways.Items.Count-1 downto 0 do
    begin
    MultiCli.SelectClient(eGateways.Items[a]);
    MultiCli.AutoLogin:=True;
    end;

  StatusUpdate.Enabled:=True;
  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.FormDestroy(Sender: TObject);
  begin
  FreeAndNil(FChatUsers);
  FreeAndNil(FScreenUsers);
  FreeAndNil(FFileUsers);

  FreeAndNil(FCS);
  end;

procedure TGateClientForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  StatusUpdate.Enabled:=False;
  MultiCli.LogOutAndCleanUp;
  CanClose:=True;
  end;

procedure TGateClientForm.btnCLRClick(Sender: TObject);
  begin
  CntReset:=0;
  btnCLR.Color:=clWhite;
  btnCLR.Font.Color:=clNavy;
  btnCLR.Caption:='CLR';
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

procedure TGateClientForm.StatusUpdateTimer(Sender: TObject);
  begin
  //Display status for the currently selected Client ...
  if MultiCli.MyUID>0 then
    begin
    if eYourID.Text<>LWord2Str(MultiCli.MyUID) then
      eYourID.Text:=LWord2Str(MultiCli.MyUID);
    end
  else
    eYourID.Text:='??????';

  if MultiCli.UseWinHTTP then
    btnReset.Caption:='HT'
  else if MultiCli.UseProxy then
    btnReset.Caption:='IE'
  else if MultiCli.UseBlocking then
    btnReset.Caption:='BS'
  else
    btnReset.Caption:='AS';

  case MultiCli.State.InputState of
    ins_Connecting: shInput.Brush.Color:=clYellow;
    ins_Closed:     shInput.Brush.Color:=clRed;
    ins_Prepare:    shInput.Brush.Color:=clBlue;
    ins_Start:      shInput.Brush.Color:=clGreen;
    ins_Recv:       shInput.Brush.Color:=clLime;
    ins_Idle:       shInput.Brush.Color:=clGreen;
    ins_Done:       shInput.Brush.Color:=clNavy;
    end;
  if MultiCli.State.InputState=ins_Closed then
    shInput.Pen.Color:=shInput.Brush.Color
  else
    case MultiCli.State.PingInCnt of
      0:shInput.Pen.Color:=clWhite;
      1:shInput.Pen.Color:=clGreen;
      2:shInput.Pen.Color:=clLime;
      3:shInput.Pen.Color:=clBlack;
      end;

  case MultiCli.State.OutputState of
    outs_Connecting:  shOutput.Brush.Color:=clYellow;
    outs_Closed:      shOutput.Brush.Color:=clRed;
    outs_Prepare:     shOutput.Brush.Color:=clBlue;
    outs_Start:       shOutput.Brush.Color:=clGreen;
    outs_Send:        shOutput.Brush.Color:=clLime;
    outs_Idle:        shOutput.Brush.Color:=clGreen;
    outs_Done:        shOutput.Brush.Color:=clNavy;
    end;
  if MultiCli.State.OutputState=outs_Closed then
    shOutput.Pen.Color:=shOutput.Brush.Color
  else
    case MultiCli.State.PingOutCnt of
      0:shOutput.Pen.Color:=clWhite;
      1:shOutput.Pen.Color:=clGreen;
      2:shOutput.Pen.Color:=clLime;
      3:shOutput.Pen.Color:=clBlack;
      end;
  lblSendBuffSize.Caption:=KSeparate(Int2Str(MultiCli.State.TotalSent div 1024))+'K';
  lblRecvBufferSize.Caption:=KSeparate(Int2Str(MultiCli.State.TotalReceived div 1024))+'K';

  FCS.Acquire;
  try
    l_Status1.Caption:=sStatus1;
    l_Status2.Caption:=sStatus2;
    l_Status3.Caption:=sStatus3;
    l_Groups.Caption:=sGroups;
  finally
    FCS.Release;
    end;
  end;

procedure TGateClientForm.PrintMsg(const s: String; t:TMsgType);
  begin
  FCS.Acquire;
  try
    case t of
      msg_Input:
        sStatus1:=Time2Str(Now)+' '+s;
      msg_Output:
        sStatus2:=Time2Str(Now)+' '+s;
      msg_Group:
        sStatus1:=Time2Str(Now)+' '+s;
      msg_Speed:
        sStatus3:=s;
      msg_Status:
        begin
        sStatus1:=Time2Str(Now)+' '+s;
        sStatus2:='';
        sStatus3:='';
        end;
      msg_Error:
        sStatus2:=Time2Str(Now)+' '+s;
      end;
  finally
    FCS.Release;
    end;

  case t of
    msg_Input,msg_Output,msg_Group:
      Log(s,MultiCli.PublicID+'_DATA');
    msg_Speed,msg_Status,msg_Error:
      Log(s,MultiCli.PublicID+'_CONN');
    end;
  end;

procedure TGateClientForm.eMyGroupDblClick(Sender: TObject);
  var
    UID,GID:String;
    i:integer;
    Client:TRtcHttpGateClient;
  begin
  if (eMyGroup.Items.Count>0) and (eMyGroup.ItemIndex>=0) then
    begin
    UID:=Trim(eMyGroup.Items.Strings[eMyGroup.ItemIndex]);
    i:=Pos('/',UID);
    GID:=Copy(UID,i+1,length(UID));
    UID:=Copy(UID,1,i-1);
    if MessageDlg('Remove User '+UID+' from My Group '+GID+'?',mtConfirmation,[mbYes,mbNo],0)=mrYes then
      begin
      Client:=MultiCli.FindClient(UID,True);
      if assigned(Client) then
        try
          Client.RemUserFromGroup(Client.MyUID,StrToInt(GID),ExtractUserID(UID));
        finally
          MultiCli.UnLockClient(Client);
          end;
      end;
    end;
  end;

procedure TGateClientForm.eInGroupDblClick(Sender: TObject);
  var
    UID,GID:String;
    i:integer;
    Client:TRtcHttpGateClient;
  begin
  if (eInGroup.Items.Count>0) and (eInGroup.ItemIndex>=0) then
    begin
    UID:=Trim(eInGroup.Items.Strings[eInGroup.ItemIndex]);
    i:=Pos('/',UID);
    GID:=Copy(UID,i+1,length(UID));
    UID:=Copy(UID,1,i-1);
    if MessageDlg('Leave Group '+GID+' Hosted by User '+UID+' ?',mtConfirmation,[mbYes,mbNo],0)=mrYes then
      begin
      Client:=MultiCli.FindClient(UID,True);
      if assigned(Client) then
        try
          Client.RemUserFromGroup(ExtractUserID(UID),StrToInt(GID),Client.MyUID);
        finally
          MultiCli.UnLockClient(Client);
          end;
      end;
    end;
  end;

procedure TGateClientForm.btnHostChatClick(Sender: TObject);
  begin
  if MultiCli.Client.Ready then
    NewChatHostForm(MultiCli.Client);
  end;

procedure TGateClientForm.eChatUsersDblClick(Sender: TObject);
  var
    UID,Key:String;
    UserAddr:RtcString;
    UserID,GroupID:TGateUID;
    i:integer;
    Frm:TChatHostFrm;
    Client:TRtcHttpGateClient;
  begin
  if (eChatUsers.Items.Count>0) and (eChatUsers.ItemIndex>=0) then
    begin
    UID:=Trim(eChatUsers.Items.Strings[eChatUsers.ItemIndex]);
    eChatUsers.Items.Delete(eChatUsers.ItemIndex);

    lblInvites.Visible:=eChatUsers.Count > 0;

    Key:=FChatUsers.search(UID);
    if Key<>'' then
      begin
      FChatUsers.remove(UID);
      i:=Pos('/',UID);
      UserAddr:=Copy(UID,1,i-1);
      GroupID:=StrToInt(Copy(UID,i+1,length(UID)-i));

      Client:=MultiCli.FindClient(UserAddr,True);
      if assigned(Client) then
        try
          // Open a new Chat Room
          Frm:=NewChatHostForm(Client);
          UserID:=ExtractUserID(UserAddr);
          if (Client.MyUID<>UserID) or (Frm.MyGroupID<>GroupID) then
            begin
            Frm.UserIsPassive(UserID,0);
            // Add user to "passive" list
            Frm.Link.Groups.SetStatus(UserID,0,1);
            // Add user as Friend, so the User can add us to his User Group
            Client.AddFriend(UserID);
            // Send invitation Key back with our Key
            Client.SendBytes(UserID,GroupID,cid_ChatAccept,RtcStringToBytes(Key + RtcBytesToString(Frm.InviteKey)));
            end;
        finally
          MultiCli.UnLockClient(Client);
          end;
      end;
    end;
  end;

procedure TGateClientForm.btnResetClick(Sender: TObject);
  begin
  NeedProviderChange:=True;
  MultiCli.Client.ResetStreams;
  end;

procedure TGateClientForm.eOnlineUsersDblClick(Sender: TObject);
  var
    UID:String;
    Client:TRtcHttpGateClient;
  begin
  if (eOnlineUsers.Items.Count>0) and (eOnlineUsers.ItemIndex>=0) then
    begin
    UID:=Trim(eOnlineUsers.Items.Strings[eOnlineUsers.ItemIndex]);
    Client:=MultiCli.FindClient(UID,True);
    if assigned(Client) then
      try
        NewChatHostForm(Client).InviteUserToChat(ExtractUserID(UID),0,True);
      finally
        MultiCli.UnLockClient(Client);
        end;
    end;
  end;

procedure TGateClientForm.eOnlineUsersClick(Sender: TObject);
  var
    UID:String;
    Client:TRtcHttpGateClient;
  begin
  // Testing requests for manual User Account verifications ..
  if (eOnlineUsers.Items.Count>0) and (eOnlineUsers.ItemIndex>=0) then
    begin
    UID:=Trim(eOnlineUsers.Items.Strings[eOnlineUsers.ItemIndex]);
    Client:=MultiCli.FindClient(UID,True);
    if assigned(Client) then
      try
        if Client.RequestUserVerifications(ExtractUserID(UID)) then
          MessageBeep(1);
      finally
        MultiCli.UnLockClient(Client);
        end;
    end;
  end;

procedure TGateClientForm.btnRemoveGatewayClick(Sender: TObject);
  var
    UID:RtcString;
  begin
  if (eGateways.Items.Count>0) and (eGateways.ItemIndex>=0) then
    begin
    UID:=Trim(eGateways.Items.Strings[eGateways.ItemIndex]);
    MultiCli.SelectClient(UID);
    MultiCli.AutoLogin:=False;
    eGateways.Items.Delete(eGateways.ItemIndex);
    if eGateways.Items.Count>0 then
      begin
      eGateways.ItemIndex:=0;
      MultiCli.SelectClient(eGateways.Items.Strings[0]);
      end;
    StatusUpdateTimer(nil);
    end;
  end;

procedure TGateClientForm.eGatewaysClick(Sender: TObject);
  var
    UID:RtcString;
  begin
  if (eGateways.Items.Count>0) and (eGateways.ItemIndex>=0) then
    begin
    UID:=Trim(eGateways.Items.Strings[eGateways.ItemIndex]);
    MultiCli.SelectClient(UID);
    StatusUpdateTimer(nil);
    end;
  end;

procedure TGateClientForm.btnNewGatewayClick(Sender: TObject);
  var
    GateAddr:String;
  begin
  if InputQuery('Add New Gateway','Gateway Address:Port',GateAddr) then
    begin
    MultiCli.SelectClient(GateAddr);
    if MultiCli.AutoLogin=False then
      eGateways.ItemIndex:=eGateways.Items.Add(GateAddr);
    MultiCli.AutoLogin:=True;
    StatusUpdateTimer(nil);
    end;
  end;

procedure TGateClientForm.eGatewaysDblClick(Sender: TObject);
  var
    UID:RtcString;
  begin
  if (eGateways.Items.Count>0) and (eGateways.ItemIndex>=0) then
    begin
    UID:=Trim(eGateways.Items.Strings[eGateways.ItemIndex]);
    MultiCli.SelectClient(UID);
    NeedProviderChange:=True;
    MultiCli.Client.ResetStreams;
    StatusUpdateTimer(nil);
    end;
  end;

procedure TGateClientForm.CleanUpLists(cli:TRtcHttpGateClient);
  var
    a,i:integer;
    UID:RtcString;
  begin
  a:=0;
  while a<eMyGroup.Count do
    begin
    UID:=Trim(eMyGroup.Items.Strings[a]);
    i:=Pos('/',UID);
    UID:=Copy(UID,1,i-1);
    if MultiCli.FindClient(UID)=cli then
      begin
      eMyGroup.Items.Delete(a);
      if OutGroupCnt>0 then
        Dec(OutGroupCnt);
      end
    else
      Inc(a);
    end;
  a:=0;
  while a<eInGroup.Count do
    begin
    UID:=Trim(eInGroup.Items.Strings[a]);
    i:=Pos('/',UID);
    UID:=Copy(UID,1,i-1);
    if MultiCli.FindClient(UID)=cli then
      begin
      eInGroup.Items.Delete(a);
      if InGroupCnt>0 then
        Dec(InGroupCnt);
      end
    else
      Inc(a);
    end;
  a:=0;
  while a<eOnlineUsers.Count do
    begin
    UID:=Trim(eOnlineUsers.Items.Strings[a]);
    if MultiCli.FindClient(UID)=cli then
      eOnlineUsers.Items.Delete(a)
    else
      Inc(a);
    end;
  a:=0;
  while a<eChatUsers.Count do
    begin
    UID:=Trim(eChatUsers.Items.Strings[a]);
    i:=Pos('/',UID);
    UID:=Copy(UID,1,i-1);
    if MultiCli.FindClient(UID)=cli then
      eChatUsers.Items.Delete(a)
    else
      Inc(a);
    end;
  lblGroups.Visible:=(eMyGroup.Count>0) or (eInGroup.Count>0);
  lblOnline.Visible:=eOnlineUsers.Count>0;
  lblInvites.Visible:=eChatUsers.Count>0;

  FCS.Acquire;
  try
    sGroups:=IntToStr(InGroupCnt)+'/'+IntToStr(OutGroupCnt);
  finally
    FCS.Release;
    end;

  if btnCLR.Caption<>'CLR' then
    begin
    btnCLR.Color:=clRed;
    btnCLR.Font.Color:=clYellow;
    end;

  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.MultiCliAfterLoggedInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  PrintMsg(Client.MyAddr+' Logged IN ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Status);

  StatusUpdateTimer(nil);

  // Testing manual Subscribe calls ...
  Client.Subscribe(rtcMakePublicKey('Test'),50);
  end;

procedure TGateClientForm.MultiCliAfterLoginFailGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  if Client.UseWinHTTP then // WinHTTP -> async WinSock
    begin
    btnReset.Caption:='AS';
    Client.UseBlocking:=False;
    Client.UseProxy:=False;
    Client.UseWinHTTP:=False;
    end
  else if Client.UseProxy then // WinInet -> WinHTTP
    begin
    btnReset.Caption:='HT';
    Client.UseWinHTTP:=True;
    end
  else if Client.UseBlocking then // blocking WinSock -> WinInet
    begin
    btnReset.Caption:='IE';
    Client.UseProxy:=True;
    end
  else // async WinSock -> blocking WinSock
    begin
    btnReset.Caption:='BS';
    Client.UseBlocking:=True;
    end;

  PrintMsg(Client.MyAddr+' Login attempt FAILED.',msg_Status);
  if Client.State.LastError<>'' then
    PrintMsg(Client.State.LastError, msg_Error);

  CleanUpLists(Client);
  end;

procedure TGateClientForm.MultiCliAfterLogOutGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  PrintMsg(Client.MyAddr+' Logged OUT.',msg_Status);
  if Client.State.LastError<>'' then
    PrintMsg(Client.State.LastError,msg_Error);

  CleanUpLists(Client);
  end;

procedure TGateClientForm.MultiCliBeforeLogInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  FLoginStart:=GetAppRunTime;

  if Client=MultiCli.Client then
    begin
    CntReset:=0;
    btnCLR.Color:=clWhite;
    btnCLR.Font.Color:=clNavy;
    btnCLR.Caption:='CLR';

    shInput.Brush.Color:=clYellow;
    shInput.Pen.Color:=clWhite;
    shOutput.Brush.Color:=clYellow;
    shOutput.Pen.Color:=clWhite;
    end;

  PrintMsg(Client.MyAddr+' Logging in ...',msg_Status);

  CleanUpLists(Client);
  end;

procedure TGateClientForm.MultiCliDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  if Data.Footer or not Data.ToBuffer then
    PrintMsg('<'+IntToStr(Length(Data.Content) div 1024)+'K id '+Data.UserAddr, msg_Input)
  end;

procedure TGateClientForm.MultiCliReadyToSend(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  PrintMsg('Ready ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Output);
  FLoginStart:=GetAppRunTime;
  end;

procedure TGateClientForm.MultiCliStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  if NeedProviderChange and (Client=MultiCli.Client) then
    begin
    NeedProviderChange:=False;
    if Client.UseWinHTTP then // WinHTTP -> async WinSock
      begin
      btnReset.Caption:='AS';
      Client.UseBlocking:=False;
      Client.UseProxy:=False;
      Client.UseWinHTTP:=False;
      end
    else if Client.UseProxy then // WinInet -> WinHTTP
      begin
      btnReset.Caption:='HT';
      Client.UseWinHTTP:=True;
      end
    else if Client.UseBlocking then // blocking WinSock -> WinInet
      begin
      btnReset.Caption:='IE';
      Client.UseProxy:=True;
      end
    else // async WinSock -> blocking WinSock
      begin
      btnReset.Caption:='BS';
      Client.UseBlocking:=True;
      end;
    end;

  FLoginStart:=GetAppRunTime;

  Inc(CntReset);
  btnCLR.Color:=clYellow;
  btnCLR.Font.Color:=clRed;
  btnCLR.Caption:=IntToStr(CntReset);

  if Client.Active then
    PrintMsg(Client.MyAddr+' #LOST ('+FloatToStr(Client.State.InputResetTime/RUN_TIMER_PRECISION)+'s / '+FloatToStr(Client.State.OutputResetTime/RUN_TIMER_PRECISION)+'s)',msg_Status)
  else
    PrintMsg(Client.MyAddr+' #FAIL ('+FloatToStr(Client.State.InputResetTime/RUN_TIMER_PRECISION)+'s / '+FloatToStr(Client.State.OutputResetTime/RUN_TIMER_PRECISION)+'s)',msg_Status);
  if Client.State.LastError<>'' then
    PrintMsg(Client.State.LastError, msg_Error);

  CleanUpLists(Client);
  end;

procedure TGateClientForm.ChatLinkDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  if (Data.CallID=cid_ChatInvite) and (Data.ToGroupID>0) then
    begin
    if Data.Footer then
      Wanted:=True
    else if Data.Header then
      Data.ToBuffer:=True;
    end
  else if (Data.CallID=cid_ChatLeft) and (Data.GroupID>0) then
    begin
    if Data.Footer then
      Wanted:=True
    else if Data.Header then
      Data.ToBuffer:=True;
    end;
  end;

procedure TGateClientForm.ChatLinkDataReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  var
    UID:RtcString;
    i:integer;
  begin
  if (Data.CallID=cid_ChatInvite) and (Data.ToGroupID>0) then
    begin
    UID:=Data.UserAddr+'/'+Int2Str(Data.ToGroupID);
    if FChatUsers.search(UID)='' then
      begin
      // Add UserID+GroupID to Chat Users invitation list
      eChatUsers.Items.Add(UID);
      // Store Invitation Key for Chat with UserID+GroupID
      FChatUsers.insert(UID,RtcBytesToString(Data.Content));

      if GetActiveWindow<>Handle then MessageBeep(0);
      lblInvites.Visible:=True;
      end;
    end
  else if (Data.CallID=cid_ChatLeft) and (Data.GroupID>0) then
    begin
    UID:=Data.UserAddr+'/'+Int2Str(Data.GroupID);
    if FChatUsers.search(UID)<>'' then
      begin
      i:=eChatUsers.Items.IndexOf(UID);
      eChatUsers.Items.Delete(i);
      FChatUsers.remove(UID);
      lblInvites.Visible:=eChatUsers.Count > 0;
      end;
    end;
  end;

procedure TGateClientForm.MultiCliInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  case Data.Command of
    gc_UserOnline,
    gc_UserOffline,
    gc_SubscribeLogIn,
    gc_SubscribeLogOut,
    gc_AccountLogIn,
    gc_AccountLogOut,
    gc_AccountVerified,
    gc_AccountBadVerify,
    gc_UserJoined,
    gc_UserLeft,
    gc_JoinedUser,
    gc_LeftUser,
    gc_Error:       Wanted:=True;
    end;
  end;

procedure TGateClientForm.MultiCliInfoReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);
  var
    s:String;
  begin
  case Data.Command of
    gc_UserOnline:  PrintMsg(Data.UserAddr+' ON-Line',msg_Group);
    gc_UserOffline: PrintMsg(Data.UserAddr+' OFF-Line',msg_Group);

    gc_SubscribeLogIn:   PrintMsg(Data.UserAddr+' "'+Data.UserInfo+'" ON-Channel ('+IntToStr(Data.GroupID)+')',msg_Group);
    gc_SubscribeLogOut:  PrintMsg(Data.UserAddr+' OFF-Channel ('+IntToStr(Data.GroupID)+')',msg_Group);

    gc_AccountLogIn:  begin
                      S:=Data.AccountID;
                      if S=LobbyAccount then WantGUI:=True;

                      case MultiCli.isType[S] of
                        gat_Public: PrintMsg(Data.UserAddr+' "'+Data.UserInfo+'" ON-Public "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Private:PrintMsg(Data.UserAddr+' "'+Data.UserInfo+'" ON-Private "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Linked: PrintMsg(Data.UserAddr+' "'+Data.UserInfo+'" ON-Linked "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        end;
                      if Client.VerifiedAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' Verified-ON "'+MultiCli.DisplayName[S]+'"',msg_Group)
                      else if Client.UserAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' Normal-ON "'+MultiCli.DisplayName[S]+'"',msg_Group)
                      else if Client.UnVerifiedAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' UnVerified-ON "'+MultiCli.DisplayName[S]+'"',msg_Group);
                      end;
    gc_AccountLogOut: begin
                      S:=Data.AccountID;
                      if S=LobbyAccount then WantGUI:=True;

                      case MultiCli.isType[S] of
                        gat_Public: PrintMsg(Data.UserAddr+' OFF-Public "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Private:PrintMsg(Data.UserAddr+' OFF-Private "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Linked: PrintMsg(Data.UserAddr+' OFF-Linked "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        end;
                      if Client.VerifiedAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' Verified-OFF "'+MultiCli.DisplayName[S]+'"',msg_Group)
                      else if Client.UserAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' Normal-OFF "'+MultiCli.DisplayName[S]+'"',msg_Group)
                      else if Client.UnVerifiedAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' UnVerified-OFF "'+MultiCli.DisplayName[S]+'"',msg_Group);
                      end;
    gc_AccountVerified:
                      begin
                      S:=Data.AccountID;
                      case MultiCli.isType[S] of
                        gat_Public: PrintMsg(Data.UserAddr+' OK-Public "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Private:PrintMsg(Data.UserAddr+' OK-Private "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Linked: PrintMsg(Data.UserAddr+' OK-Linked "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        end;
                      if Client.VerifiedAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' NOW Verified "'+MultiCli.DisplayName[S]+'"',msg_Group)
                      else if Client.UserAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' NOW Normal "'+MultiCli.DisplayName[S]+'"',msg_Group)
                      else if Client.UnVerifiedAccount[Data.UserID,S] then
                        PrintMsg(Data.UserAddr+' NOW UnVerified "'+MultiCli.DisplayName[S]+'"',msg_Group);
                      end;
    gc_AccountBadVerify:
                      begin
                      S:=Data.AccountID;
                      case MultiCli.isType[S] of
                        gat_Public: PrintMsg(Data.UserAddr+' BAD-Public "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Private:PrintMsg(Data.UserAddr+' BAD-Private "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        gat_Linked: PrintMsg(Data.UserAddr+' BAD-Linked "'+MultiCli.DisplayName[S]+'"',msg_Group);
                        end;
                      end;
    gc_Error:       PrintMsg('ERR #'+IntToStr(Data.ErrCode)+' from '+Data.UserAddr,msg_Group);

    gc_UserJoined,
    gc_UserLeft,
    gc_JoinedUser,
    gc_LeftUser:  WantGUI:=True;
    end;
  end;

procedure TGateClientForm.MultiCliInfoReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  var
    s:String;
    i:integer;
  begin
  case Data.Command of
    gc_AccountLogIn:  begin
                      S:=Data.UserAddr;
                      PrintMsg('CHAT +'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                      if eOnlineUsers.Items.IndexOf(S)<0 then
                        eOnlineUsers.Items.Add(S);
                      lblOnline.Visible:=eOnlineUsers.Count>0;
                      end;
    gc_AccountLogOut: begin
                      S:=Data.UserAddr;
                      PrintMsg('CHAT -'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                      i:=eOnlineUsers.Items.IndexOf(S);
                      if i>=0 then eOnlineUsers.Items.Delete(i);
                      lblOnline.Visible:=eOnlineUsers.Count>0;
                      end;
    gc_UserJoined:  begin
                    Inc(OutGroupCnt);
                    S:=Data.UserAddr+'/'+INtToStr(Data.GroupID);
                    PrintMsg('OUT +'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                    if eMyGroup.Items.IndexOf(S)<0 then
                      eMyGroup.Items.Add(S);
                    lblGroups.Visible:=eMyGroup.Count + eInGroup.Count > 0;
                    end;
    gc_UserLeft:    begin
                    Dec(OutGroupCnt);
                    S:=Data.UserAddr+'/'+IntToStr(Data.GroupID);
                    PrintMsg('OUT -'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                    i:=eMyGroup.Items.IndexOf(S);
                    if i>=0 then eMyGroup.Items.Delete(i);
                    lblGroups.Visible:=eMyGroup.Count + eInGroup.Count > 0;
                    end;
    gc_JoinedUser:  begin
                    Inc(InGroupCnt);
                    S:=Data.UserAddr+'/'+IntToStr(Data.GroupID);
                    PrintMsg('IN +'+S+' ('+IntToStr(InGroupCnt)+')',msg_Group);
                    if eInGroup.Items.IndexOf(S)<0 then
                      eInGroup.Items.Add(S);
                    end;
    gc_LeftUser:    begin
                    Dec(InGroupCnt);
                    S:=Data.UserAddr+'/'+IntToStr(Data.GroupID);
                    PrintMsg('IN -'+S+' ('+IntToStr(InGroupCnt)+')',msg_Group);
                    i:=eInGroup.Items.IndexOf(S);
                    if i>=0 then eInGroup.Items.Delete(i);
                    end;
    end;
  FCS.Acquire;
  try
    sGroups:=IntToStr(InGroupCnt)+'/'+IntToStr(OutGroupCnt);
  finally
    FCS.Release;
    end;
  end;

end.
