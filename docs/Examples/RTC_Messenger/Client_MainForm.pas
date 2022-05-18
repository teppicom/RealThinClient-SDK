unit Client_MainForm;

interface

uses
  Windows, Messages, SysUtils, 
  Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  ImgList, Menus,

  rtcTypes, rtcSystem, rtcConn, rtcDataCli, rtcHttpCli,
  rtcInfo, rtcCliModule, rtcFunction,

  tools,

  Client_ChatForm;

const
  MSG_STATUS_UNKNOWN=0;
  MSG_STATUS_OFFLINE=1;
  MSG_STATUS_ONLINE=2;
  MSG_STATUS_IGNORE=3;

type
  TForm1 = class(TForm)
    ClientModule: TRtcClientModule;
    Client: TRtcHttpClient;
    MessengerTab: TPageControl;
    tabLogin: TTabSheet;
    tabFriends: TTabSheet;
    tabIgnore: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    btnLogout: TBitBtn;
    panLogin: TPanel;
    Panel4: TPanel;
    eServerAddr: TEdit;
    Label1: TLabel;
    eServerPort: TEdit;
    Label2: TLabel;
    eModulePath: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    eLoginUser: TEdit;
    Label5: TLabel;
    eLoginPass: TEdit;
    btnLogin: TBitBtn;
    btnRegister: TBitBtn;
    btnAddFriend: TBitBtn;
    Panel5: TPanel;
    Panel6: TPanel;
    btnLogout2: TBitBtn;
    btnAddIgnore: TBitBtn;
    tvFriends: TTreeView;
    tvIgnore: TTreeView;
    resLogin: TRtcResult;
    lblLogin: TLabel;
    resUpdate: TRtcResult;
    resTimer: TRtcResult;
    resSendText: TRtcResult;
    pmFriends: TPopupMenu;
    SendMessage1: TMenuItem;
    N1: TMenuItem;
    btnDelFriend: TMenuItem;
    AddNewFriend1: TMenuItem;
    pmIgnore: TPopupMenu;
    AddnewIgnore1: TMenuItem;
    N2: TMenuItem;
    btnDelIgnore: TMenuItem;
    resLogout: TRtcResult;
    TimerClient: TRtcHttpClient;
    TimerModule: TRtcClientModule;
    resTimerLogin: TRtcResult;
    msgImages: TImageList;
    pingTimer: TTimer;
    resPing: TRtcResult;
    xUseProxy: TCheckBox;
    xUseIPv6: TCheckBox;
    rgFormat: TRadioGroup;
    procedure btnLogoutClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure resLoginReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure ClientModuleResponseAbort(Sender: TRtcConnection);
    procedure FormCreate(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure resUpdateReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure btnAddFriendClick(Sender: TObject);
    procedure btnAddIgnoreClick(Sender: TObject);
    procedure btnSendMessageClick(Sender: TObject);
    procedure msgTimerTimer(Sender: TObject);
    procedure resTimerReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure resSendTextReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDelFriendClick(Sender: TObject);
    procedure btnDelIgnoreClick(Sender: TObject);
    procedure resLogoutReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure resTimerLoginReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure pingTimerTimer(Sender: TObject);
    procedure resPingReturn(Sender: TRtcConnection; Data,
      Result: TRtcValue);
  private
    { Private declarations }
  public
    { Public declarations }
    do_notify:boolean;

    oldUserName,
    myUserName:string;
    myCheckTime:TDateTime;

    procedure StartLogin;

    procedure FriendList_Clear;
    procedure IgnoreList_Clear;

    procedure FriendList_Add(uname:string);
    procedure FriendList_Del(uname:string);
    procedure IgnoreList_Add(uname:string);
    procedure IgnoreList_Del(uname:string);

    function FriendList_Selected:string;
    function IgnoreList_Selected:string;

    procedure SendText(to_user,msg:string);

    function isFriend(uname:string):boolean;
    function isIgnore(uname:string):boolean;

    procedure FriendList_Status(uname:string; status:integer);

    procedure make_notify(uname, ntype :string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  // Width:=206;

  do_notify:=False;

  myUserName:='';
  oldUserName:='';

  // Assign the method for sending a Text message (used by Chat_Form)
  SendMsg:=SendText;

  MessengerTab.ActivePage:=tabLogin;
  tabLogin.TabVisible:=True;
  tabFriends.TabVisible:=False;
  tabIgnore.TabVisible:=False;
  panLogin.Enabled:=True;
  end;

procedure TForm1.FormShow(Sender: TObject);
  begin
  if tabLogin.TabVisible and tabLogin.Visible then
    eLoginUser.SetFocus;
  end;

procedure TForm1.StartLogin;
  begin
  Client.Disconnect;
  Client.ServerAddr:=RtcString(eServerAddr.Text);
  Client.ServerPort:=RtcString(eServerPort.Text);
  Client.ServerIPV:=GetRtcIPV(xUseIPv6.Checked);
  Client.UseProxy:=xUseProxy.Checked;
  Client.SkipRequests;
  Client.Session.Close;
  with ClientModule do
    begin
    case rgFormat.ItemIndex of
      1: DataFormat:=fmt_XMLRPC;
      2: DataFormat:=fmt_JSONrpc1;
      3: DataFormat:=fmt_JSONrpc2;
    else
      DataFormat:=fmt_RTC;
      end;
    ModuleFileName:=RtcString(eModulePath.Text);
    if copy(ModuleFileName,length(ModuleFileName),1)<>'/' then
      ModuleFileName:=ModuleFileName+'/';
    ModuleFileName:=ModuleFileName+'$MSG';

    ModuleHost:=RtcString(eServerAddr.Text);
    end;

  TimerClient.ServerAddr:=RtcString(eServerAddr.Text);
  TimerClient.ServerPort:=RtcString(eServerPort.Text);
  TimerClient.ServerIPV:=GetRtcIPV(xUseIPv6.Checked);
  TimerClient.UseProxy:=xUseProxy.Checked;
  TimerClient.SkipRequests;
  TimerClient.Session.Close;
  with TimerModule do
    begin
    case rgFormat.ItemIndex of
      1: DataFormat:=fmt_XMLRPC;
      2: DataFormat:=fmt_JSONrpc1;
      3: DataFormat:=fmt_JSONrpc2;
    else
      DataFormat:=fmt_RTC;
      end;
    ModuleFileName:=RtcString(eModulePath.Text);
    if copy(ModuleFileName,length(ModuleFileName),1)<>'/' then
      ModuleFileName:=ModuleFileName+'/';
    ModuleFileName:=ModuleFileName+'$MSG';

    ModuleHost:=RtcString(eServerAddr.Text);
    end;

  Client.Connect;

  do_notify:=False;

  btnLogin.Enabled:=False;
  btnRegister.Enabled:=False;
  lblLogin.Visible:=True;

  panLogin.Enabled:=False;
  end;

procedure TForm1.btnLoginClick(Sender: TObject);
  begin
  StartLogin;
  with ClientModule, Data.NewFunction('login') do
    begin
    asText['User']:=eLoginUser.Text;
    asText['Pass']:=eLoginPass.Text;
    Call(resLogin);
    end;
  with TimerModule, Data.NewFunction('login2') do
    begin
    asText['User']:=eLoginUser.Text;
    asText['Pass']:=eLoginPass.Text;
    Call(resTimerLogin);
    end;
  end;

procedure TForm1.btnRegisterClick(Sender: TObject);
  begin
  StartLogin;
  with ClientModule, Data.NewFunction('register') do
    begin
    asText['User']:=eLoginUser.Text;
    asText['Pass']:=eLoginPass.Text;
    Call(resLogin);
    end;
  with TimerModule, Data.NewFunction('login2') do
    begin
    asText['User']:=eLoginUser.Text;
    asText['Pass']:=eLoginPass.Text;
    Call(resTimerLogin);
    end;
  end;

procedure TForm1.btnLogoutClick(Sender: TObject);
  begin
  if Sender<>nil then
    begin
    do_notify:=False;
    pingTimer.Enabled:=False;
    with ClientModule, Data.NewFunction('Logout') do
      begin
      asText['User']:=myUserName;
      myUserName:='';
      Call(resLogout);
      end;
    end
  else
    begin
    ClientModule.SkipRequests;
    TimerModule.SkipRequests;

    do_notify:=False;
    pingTimer.Enabled:=False;

    myUserName:='';

    panLogin.Enabled:=True;

    lblLogin.Visible:=False;
    btnLogin.Enabled:=True;
    btnRegister.Enabled:=True;

    tabLogin.TabVisible:=True;
    tabFriends.TabVisible:=False;
    tabIgnore.TabVisible:=False;

    TChatForm.disableAllForms;

    eLoginUser.SetFocus;

    Client.Disconnect;
    TimerClient.Disconnect;
    end;
  end;

procedure TForm1.ClientModuleResponseAbort(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    begin
    btnLogoutClick(nil);
    MessageBeep(0);
    ShowMessage('Error sending a request to the Messenger Server.'#13#10+'Connection to Server lost.');
    end;
  end;

procedure TForm1.resLoginReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  var
    i:integer;
  begin
  if Result.isType=rtc_Exception then
    begin
    btnLogoutClick(nil);
    MessageBeep(0);
    ShowMessage(Result.asException);
    end
  else if Result.isType<>rtc_Record then
    begin
    btnLogoutClick(nil);
    MessageBeep(0);
    ShowMessage('Invalid Server Response.');
    end
  else
    begin
    TChatForm.enableAllForms;

    myUserName:=Data.asFunction.asText['user'];
    if oldUserName<>myuserName then
      TChatForm.closeAllForms;

    oldUserName:=myUserName;
    with Result.asRecord do
      begin
      FriendList_Clear;
      IgnoreList_Clear;

      if isType['friends']=rtc_Record then
        with asRecord['friends'] do
          for i:=0 to Count-1 do
            if asBoolean[FieldName[i]] then
              FriendList_Add(FieldName[i]);

      if Result.asRecord.isType['ignore']=rtc_Record then
        with asRecord['ignore'] do
          for i:=0 to Count-1 do
            if asBoolean[FieldName[i]] then
              IgnoreList_Add(FieldName[i]);
      end;

    lblLogin.Visible:=False;
    tabFriends.TabVisible:=True;
    tabIgnore.TabVisible:=True;
    tabLogin.TabVisible:=False;
    MessengerTab.ActivePage:=tabFriends;

    TimerClient.Connect;
    msgTimerTimer(nil);

    pingTimer.Enabled:=True;
    end;
  end;

procedure TForm1.resUpdateReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType=rtc_Exception then
    begin
    if Sender<>nil then
      PostInteractive; // ShowMessage would block the connection, need to post interactive

    if UpperCase(Data.asFunction.FunctionName)='ADDFRIEND' then
      FriendList_Del(Data.asFunction.asText['name'])
    else if UpperCase(Data.asFunction.FunctionName)='ADDIGNORE' then
      IgnoreList_Add(Data.asFunction.asText['name'])
    else
      btnLogoutClick(nil);

    MessageBeep(0);
    ShowMessage(Result.asException);
    end;
  end;

procedure TForm1.FriendList_Clear;
  begin
  tvFriends.Items.Clear;
  end;

procedure TForm1.IgnoreList_Clear;
  begin
  tvIgnore.Items.Clear;
  end;

procedure TForm1.FriendList_Add(uname: string);
  var
    node:TTreeNode;
  begin
  if isFriend(uname) then Exit;

  uname:=LowerCase(uname);
  node:=tvFriends.Items.Add(nil,uname);
  node.ImageIndex:=MSG_STATUS_UNKNOWN;
  node.SelectedIndex:=MSG_STATUS_UNKNOWN;
  node.StateIndex:=MSG_STATUS_UNKNOWN;
  tvFriends.Update;
  end;

procedure TForm1.IgnoreList_Add(uname: string);
  var
    node:TTreeNode;
  begin
  if isIgnore(uname) then Exit;

  uname:=LowerCase(uname);
  node:=tvIgnore.Items.Add(nil,uname);
  node.ImageIndex:=MSG_STATUS_IGNORE;
  node.SelectedIndex:=MSG_STATUS_IGNORE;
  node.StateIndex:=MSG_STATUS_IGNORE;
  tvIgnore.Update;
  end;

procedure TForm1.FriendList_Del(uname: string);
  var
    i:integer;
    node:TTreeNode;
  begin
  uname:=LowerCase(uname);
  for i:=0 to tvFriends.Items.Count-1 do
    begin
    node:=tvFriends.Items.Item[i];
    if LowerCase(node.Text)=uname then
      begin
      node.Delete;
      Break;
      end;
    end;
  end;

procedure TForm1.IgnoreList_Del(uname: string);
  var
    i:integer;
    node:TTreeNode;
  begin
  uname:=LowerCase(uname);
  for i:=0 to tvIgnore.Items.Count-1 do
    begin
    node:=tvIgnore.Items.Item[i];
    if LowerCase(node.Text)=uname then
      begin
      node.Delete;
      Break;
      end;
    end;
  end;

function TForm1.isFriend(uname: string): boolean;
  var
    i:integer;
    node:TTreeNode;
  begin
  uname:=LowerCase(uname);
  Result:=False;
  for i:=0 to tvFriends.Items.Count-1 do
    begin
    node:=tvFriends.Items.Item[i];
    if LowerCase(node.Text)=uname then
      begin
      Result:=True;
      Break;
      end;
    end;
  end;

function TForm1.isIgnore(uname: string): boolean;
  var
    i:integer;
    node:TTreeNode;
  begin
  uname:=LowerCase(uname);
  Result:=False;
  for i:=0 to tvIgnore.Items.Count-1 do
    begin
    node:=tvIgnore.Items.Item[i];
    if LowerCase(node.Text)=uname then
      begin
      Result:=True;
      Break;
      end;
    end;
  end;

procedure TForm1.FriendList_Status(uname:string; status:integer);
  var
    i:integer;
    node:TTreeNode;
  begin
  uname:=LowerCase(uname);
  for i:=0 to tvFriends.Items.Count-1 do
    begin
    node:=tvFriends.Items.Item[i];
    if LowerCase(node.Text)=uname then
      begin
      node.ImageIndex:=status;
      node.SelectedIndex:=status;
      node.StateIndex:=status;
      Break;
      end;
    end;
  tvFriends.Update;
  end;

function TForm1.FriendList_Selected: string;
  begin
  if tvFriends.Selected<>nil then
    Result:=tvFriends.Selected.Text
  else
    Result:='';
  end;

function TForm1.IgnoreList_Selected: string;
  begin
  if tvIgnore.Selected<>nil then
    Result:=tvIgnore.Selected.Text
  else
    Result:='';
  end;

procedure TForm1.btnAddFriendClick(Sender: TObject);
  var
    s:string;
  begin
  s:='';
  if InputQuery('Add Friend','Please enter your Friend''s username:',s) then
    if s<>'' then
      if (UpperCase(s)=UpperCase(myUserName)) then
        ShowMessage('You can not add yourself to friends list.')
      else if isFriend(s) then
        ShowMessage(s+' is already on your Friends list.')
      else
        begin
        with ClientModule, Data.NewFunction('AddFriend') do
          begin
          asText['User']:=myUserName;
          asText['Name']:=s;
          Call(resUpdate);
          end;
        FriendList_Add(s);
        end;
  end;

procedure TForm1.btnAddIgnoreClick(Sender: TObject);
  var
    s:string;
  begin
  s:='';
  if InputQuery('Add User to Ignore','Please enter the name of the user:',s) then
    if s<>'' then
      if (UpperCase(s)=UpperCase(myUserName)) then
        ShowMessage('You can not add yourself to ignore list.')
      else if isIgnore(s) then
        ShowMessage(s+' is already on your Ignore list.')
      else
        begin
        with ClientModule, Data.NewFunction('AddIgnore') do
          begin
          asText['User']:=myUserName;
          asText['Name']:=s;
          Call(resUpdate);
          end;
        IgnoreList_Add(s);
        end;
  end;

procedure TForm1.btnDelFriendClick(Sender: TObject);
  var
    fname:string;
  begin
  fname:=FriendList_Selected;
  if fname='' then Exit;

  if MessageDlg('Delete "'+fname+'" from your Friends list?',
                mtWarning,[mbYes,mbNo],0)=mrYes then
    begin
    with ClientModule, Data.NewFunction('DelFriend') do
      begin
      asText['User']:=myUserName;
      asText['Name']:=fname;
      Call(resUpdate);
      end;
    FriendList_Del(fname);
    end;
  end;

procedure TForm1.btnDelIgnoreClick(Sender: TObject);
  var
    fname:string;
  begin
  fname:=IgnoreList_Selected;
  if fname='' then Exit;

  if MessageDlg('Delete "'+fname+'" from your IGNORE list?',
                mtWarning,[mbYes,mbNo],0)=mrYes then
    begin
    with ClientModule, Data.NewFunction('DelIgnore') do
      begin
      asText['user']:=myUserName;
      asText['name']:=fname;
      Call(resUpdate);
      end;
    IgnoreList_Del(fname);
    end;
  end;

procedure TForm1.btnSendMessageClick(Sender: TObject);
  var
    s:string;
  begin
  s:=FriendList_Selected;
  if isIgnore(s) then
    ShowMessage('This user is on your IGNORE list.'#13#10+
                'You can not send messages to users on your IGNORE list.')
  else if isFriend(s) then
    TChatForm.getForm(s).BringToFront
  else
    MessageBeep(0);
  end;

procedure TForm1.SendText(to_user, msg: string);
  var
    chat:TChatForm;
  begin
  chat:=TChatForm.getForm(to_user);
  chat.AddMessage(myUserName, msg, RTC_ADDMSG_SELF);

  with ClientModule, Data.NewFunction('SendText') do
    begin
    asText['user']:=myUserName;
    asText['to']:=to_user;
    asText['text']:=msg;
    Call(resSendText);
    end;
  end;

procedure TForm1.msgTimerTimer(Sender: TObject);
  begin
  with TimerModule, Data.NewFunction('GetData') do
    begin
    asText['user']:=myUserName;
    asDateTime['check']:=myCheckTime;
    Call(resTimer);
    end;
  end;

procedure TForm1.resTimerReturn(Sender: TRtcConnection; Data,Result: TRtcValue);
  var
    chat:TChatForm;
    i:integer;
    fname:string;
  begin
  if Result.isType=rtc_Exception then
    begin
    if myUserName<>'' then
      begin
      btnLogoutClick(nil);
      MessageBeep(0);
      ShowMessage(Result.asException);
      end;
    end
  else if not Result.isNull then // data arrived
    begin
    if Sender<>nil then
      begin
      with Result.asRecord do
        myCheckTime:=asDateTime['check'];
      // Check for new messages
      msgTimerTimer(nil);
      // User interaction is needed, need to Post the event for user interaction
      PostInteractive;
      end;

    with Result.asRecord do
      begin
      if isType['data']=rtc_Array then with asArray['data'] do
        for i:=0 to Count-1 do
          if isType[i]=rtc_Record then with asRecord[i] do
            if not isNull['text'] then // Text message
              begin
              fname:=asText['from'];
              if not isIgnore(fname) then
                begin
                if isFriend(fname) then
                  begin
                  chat:=TChatForm.getForm(fname,do_notify);
                  chat.AddMessage(fname, asText['text'], RTC_ADDMSG_FRIEND);
                  end
                else
                  begin
                  MessageBeep(0);
                  if MessageDlg('User "'+fname+'" has sent you a message,'#13#10+
                                   'but he/she is not on your Friends list.'#13#10+
                                   'Accept the message and add "'+fname+'" to your Friends list?',
                              mtConfirmation,[mbYes,mbNo],0)=mrYes then
                    begin
                    chat:=TChatForm.getForm(fname,do_notify);
                    chat.AddMessage(fname, asText['text'], RTC_ADDMSG_FRIEND);
                    with ClientModule, Data.NewFunction('AddFriend') do
                      begin
                      asText['user']:=myUserName;
                      asText['name']:=fname;
                      Call(resUpdate);
                      end;
                    FriendList_Add(fname);
                    end
                  else if MessageDlg('Add "'+fname+'" to your IGNORE list?',
                                     mtWarning,[mbYes,mbNo],0)=mrYes then
                    begin
                    with ClientModule, Data.NewFunction('AddIgnore') do
                      begin
                      asText['user']:=myUserName;
                      asText['name']:=fname;
                      Call(resUpdate);
                      end;
                    IgnoreList_Add(fname);
                    end;
                  end;
                end;
              end
            else if not isNull['login'] then // Friend logging in
              begin
              fname:=asText['login'];
              make_notify(fname, 'login');
              if isFriend(fname) then
                FriendList_Status(fname,MSG_STATUS_ONLINE);
              end
            else if not isNull['logout'] then // Friend logging out
              begin
              fname:=asText['logout'];
              make_notify(fname, 'logout');
              if isFriend(fname) then
                FriendList_Status(fname,MSG_STATUS_OFFLINE);
              end
            else if not isNull['addfriend'] then // Added as Friend
              begin
              fname:=asText['addfriend'];
              if not isIgnore(fname) then
                begin
                MessageBeep(0);
                if isFriend(fname) then
                  ShowMessage('User "'+fname+'" added you as a Friend.')
                else
                  begin
                  MessageBeep(0);
                  if MessageDlg('User "'+fname+'" added you as a Friend.'#13#10+
                              'Add "'+fname+'" to your Friends list?',
                              mtConfirmation,[mbYes,mbNo],0)=mrYes then
                    begin
                    with ClientModule, Data.NewFunction('AddFriend') do
                      begin
                      asText['user']:=myUserName;
                      asText['name']:=fname;
                      Call(resUpdate);
                      end;
                    FriendList_Add(fname);
                    end
                  else if MessageDlg('Add "'+fname+'" to your IGNORE list?',
                                     mtWarning,[mbYes,mbNo],0)=mrYes then
                    begin
                    with ClientModule, Data.NewFunction('AddIgnore') do
                      begin
                      asText['user']:=myUserName;
                      asText['name']:=fname;
                      Call(resUpdate);
                      end;
                    IgnoreList_Add(fname);
                    end;
                  end;
                end;
              end
            else if not isNull['addignore'] then // Added as Ignore
              begin
              fname:=asText['addignore'];
              if not isIgnore(fname) then
                begin
                MessageBeep(0);
                if MessageDlg('User "'+fname+'" has chosen to IGNORE you.'#13#10+
                              'Add "'+fname+'" to your IGNORE list?',
                              mtWarning,[mbYes,mbNo],0)=mrYes then
                  begin
                  with ClientModule, Data.NewFunction('AddIgnore') do
                    begin
                    asText['user']:=myUserName;
                    asText['name']:=fname;
                    Call(resUpdate);
                    end;
                  IgnoreList_Add(fname);
                  end;
                end;
              end
            else if not isNull['delfriend'] then // Removed as Friend
              begin
              fname:=asText['delfriend'];
              if isFriend(fname) and not isIgnore(fname) then
                begin
                MessageBeep(0);
                if MessageDlg('User "'+fname+'" removed you as a Friend.'#13#10+
                              'Remove "'+fname+'" from your Friends list?',
                              mtConfirmation,[mbYes,mbNo],0)=mrYes then
                  begin
                  with ClientModule, Data.NewFunction('DelFriend') do
                    begin
                    asText['user']:=myUserName;
                    asText['name']:=fname;
                    Call(resUpdate);
                    end;
                  FriendList_Del(fname);
                  end;
                end;
              end
            else if not isNull['delignore'] then // Removed as Ignore
              begin
              fname:=asText['delignore'];
              if not isIgnore(fname) then
                begin
                MessageBeep(0);
                ShowMessage('User "'+fname+'" has removed you from his IGNORE list.');
                end;
              end;
      end;
    do_notify:=True;
    end
  else
    begin
    if Sender<>nil then
      begin
      // Check for new messages
      myCheckTime:=0;
      msgTimerTimer(nil);
      // We don't want to set do_notify to TRUE if user interaction is in progress
      PostInteractive;
      end;
    do_notify:=True;
    end;
  end;

procedure TForm1.resSendTextReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  var
    chat:TChatForm;
  begin
  if Result.isType=rtc_Exception then
    begin
    chat:=TChatForm.getForm(Data.asFunction.asText['to'],do_notify);
    chat.AddMessage('ERROR!', Result.asText+#13#10+'Message not delivered ...', RTC_ADDMSG_ERROR);
    chat.AddMessage(myUserName, Data.asFunction.asText['text'], RTC_ADDMSG_ERROR);
    end;
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  if myUserName<>'' then
    begin
    btnLogout.Click;

    // wait up to 2 second for any pending requests to complete
    Client.WaitForCompletion(False,2);

    CanClose:=True;
    end
  else
    CanClose:=True;
  end;

procedure TForm1.resLogoutReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  btnLogoutClick(nil);
  end;

procedure TForm1.resTimerLoginReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType=rtc_Exception then
    begin
    if myUserName<>'' then
      begin
      btnLogoutClick(nil);
      MessageBeep(0);
      ShowMessage(Result.asException);
      end;
    end;
  end;

procedure TForm1.make_notify(uname, ntype :string);
  var
    chat:TChatForm;
  begin
  if TChatForm.isFormOpen(uname) then
    begin
    chat:=TChatForm.getForm(uname, do_notify);
    if ntype='login' then
      chat.AddMessage(uname, '<LOGGED IN>', RTC_ADDMSG_LOGIN)
    else if ntype='logout' then
      chat.AddMessage(uname, '<LOGGED OUT>', RTC_ADDMSG_LOGOUT);
    end;
  end;

procedure TForm1.pingTimerTimer(Sender: TObject);
  begin
  pingTimer.Enabled:=False;
  with ClientModule, Data.NewFunction('Ping') do
    Call(resPing);
  end;

procedure TForm1.resPingReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType=rtc_Exception then
    begin
    btnLogoutClick(nil);
    MessageBeep(0);
    ShowMessage(Result.asException);
    end
  else
    pingTimer.Enabled:=True;
  end;

end.
