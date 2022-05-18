unit rtcMessengerProvider;

interface

{$include rtcDefs.inc}

uses
  SysUtils, Classes, 

  {$IFDEF VER120}
  Forms, // D4
  {$ENDIF}
  {$IFDEF VER130}
  Forms, // D5
  {$ENDIF}

  rtcSystem, rtcInfo, rtcCrypt,
  rtcFunction, rtcSrvModule,

  rtcConn, rtcDataSrv,

  rtcMessenger;

type
  TMessenger_Provider = class(TDataModule)
    Module: TRtcServerModule;
    MsgFunctions: TRtcFunctionGroup;
    MsgLogin: TRtcFunction;
    MsgRegister: TRtcFunction;
    MsgSendText: TRtcFunction;
    MsgGetData: TRtcFunction;
    ServerLink: TRtcDataServerLink;
    MsgAddFriend: TRtcFunction;
    MsgDelFriend: TRtcFunction;
    MsgAddIgnore: TRtcFunction;
    MsgDelIgnore: TRtcFunction;
    MsgLogOut: TRtcFunction;
    MsgLogin2: TRtcFunction;
    MsgPing: TRtcFunction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ModuleSessionClose(Sender: TRtcConnection);

    procedure MsgLoginExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgRegisterExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgSendTextExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgGetDataExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgAddFriendExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgDelFriendExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgAddIgnoreExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgDelIgnoreExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgLogOutExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgLogin2Execute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MsgPingExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  protected
    procedure CheckLogin(Sender:TRtcConnection; const uname:string);
  public
    { Public declarations }
    user:TRtcMessengerUsers;
  end;

function GetMessengerProvider:TMessenger_Provider;

implementation

{$R *.dfm}

var
  Messenger_Provider: TMessenger_Provider;

function GetMessengerProvider:TMessenger_Provider;
  begin
  if not assigned(Messenger_Provider) then
    TMessenger_Provider.Create(nil);
  Result:=Messenger_Provider;
  end;

procedure TMessenger_Provider.DataModuleCreate(Sender: TObject);
  begin
  Messenger_Provider:=self;
  user:=TRtcMessengerUsers.Create;
  end;

procedure TMessenger_Provider.DataModuleDestroy(Sender: TObject);
  begin
  user.Free;
  Messenger_Provider:=nil;
  end;

procedure TMessenger_Provider.ModuleSessionClose(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if (Session<>nil) then
      if (Session['$MSG:Login']='OK') then
        user.LogOut(Session['$MSG:User'], Session.ID);
  end;

procedure TMessenger_Provider.MsgLoginExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  with TRtcDataServer(Sender) do
    begin
    user.Login(Param.asText['user'], Param.asText['pass'], Session.ID);

    Result.asObject:=User.LoadInfo(Param.asText['user']);

    Session['$MSG:Login']:='OK';
    Session['$MSG:User']:=Param.asText['User'];
    end;
  end;

procedure TMessenger_Provider.MsgLogin2Execute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  with TRtcDataServer(Sender) do
    begin
    user.Login2(Param.asText['user'], Param.asText['pass'], Session.ID);

    Session['$MSG:Login']:='OK';
    Session['$MSG:User']:=Param.asText['User'];
    end;
  end;

procedure TMessenger_Provider.MsgRegisterExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  with TRtcDataServer(Sender) do
    begin
    User.RegUser(Param.asText['user'], Param.asText['pass'], Session.ID);

    Result.asObject:=User.LoadInfo(Param.asText['user']);

    Session['$MSG:Login']:='OK';
    Session['$MSG:User']:=Param.asText['User'];
    end;
  end;

procedure TMessenger_Provider.CheckLogin(Sender:TRtcConnection; const uname:string);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Session=nil then
      raise Exception.Create('No session for this client.')
    else if (Session['$MSG:Login']<>'OK') then
      raise Exception.Create('Not logged in.')
    else if Session['$MSG:User']<>uname then
      raise Exception.Create('Login error. Wrong username.')
    else if not User.isLoggedIn(uname, Session.ID) then
      raise Exception.Create('Logged out.');
    end;
  end;

procedure TMessenger_Provider.MsgSendTextExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  CheckLogin(Sender, Param.asText['user']);
  user.SendText(Param.asText['user'], Param.asText['to'], Param.asText['text']);
  end;

procedure TMessenger_Provider.MsgGetDataExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  var
    thischeck:TDateTime;
    arr:TRtcArray;
    cb:TRtcDelayedCall;
  begin
  cb:=nil;

  CheckLogin(Sender, Param.asText['user']);

  if not Param.asBoolean['delayed'] then
    begin
    { Set "delayed" parameter to TRUE, before preparing the call,
      because only changes we do to Param before we send it to
      the PrepareDelayedCall function will be memorized, while
      any changes we do to Param afterwards will be discarded. }
    Param.asBoolean['delayed']:=true;
    { Prepare delayed call, which will be triggered in 10 seconds
      in case the callback function is not used until then. }
    cb:=PrepareDelayedCall(10000, Param, MsgGetDataExecute);
    user.SetCallback(Param.asText['user'],cb);
    end;

  arr:=user.GetData(Param.asText['user'],Param.asDateTime['check'],thischeck);
  if assigned(arr) then
    begin
    // don't need delayed call, new data is ready to be sent now!
    user.SetCallback(Param.asText['user'],nil);
    if assigned(cb) then
      CancelDelayedCall(cb);

    with Result.NewRecord do
      begin
      asObject['data']:=arr;
      asDateTime['check']:=thischeck;
      end;
    end
  else if assigned(cb) then
    PostDelayedCall(cb)
  else
    user.SetCallback(Param.asText['user'],nil);
  end;

procedure TMessenger_Provider.MsgAddFriendExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  CheckLogin(Sender, Param.asText['user']);
  user.AddFriend(Param.asText['user'], Param.asText['name']);
  end;

procedure TMessenger_Provider.MsgDelFriendExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  CheckLogin(Sender, Param.asText['user']);
  user.DelFriend(Param.asText['user'], Param.asText['name']);
  end;

procedure TMessenger_Provider.MsgAddIgnoreExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  CheckLogin(Sender, Param.asText['user']);
  user.AddIgnore(Param.asText['user'], Param.asText['name']);
  end;

procedure TMessenger_Provider.MsgDelIgnoreExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  CheckLogin(Sender, Param.asText['user']);
  user.DelIgnore(Param.asText['user'], Param.asText['name']);
  end;

procedure TMessenger_Provider.MsgLogOutExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  with TRtcDataServer(Sender) do
    if Session<>nil then
      if Session['$MSG:Login']='OK' then
        if Session['$MSG:User']=Param.asText['user'] then
          if User.isLoggedIn(Param.asText['user'], Session.ID) then
            begin
            user.LogOut(Session['$MSG:User'], Session.ID);
            Session['$MSG:Login']:='';
            Session['$MSG:User']:='';
            end;
  end;

procedure TMessenger_Provider.MsgPingExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  // Nothing to do.
  end;

initialization
finalization
if assigned(Messenger_Provider) then
  begin
  Messenger_Provider.Free;
  Messenger_Provider:=nil;
  end;
end.
