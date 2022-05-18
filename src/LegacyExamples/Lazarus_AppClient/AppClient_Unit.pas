unit AppClient_Unit;

interface

{$include rtcDefs.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, Buttons,

  rtcSystem, rtcLog,
  rtcFunction, rtcDataCli, rtcCliModule, rtcInfo, rtcConn,
  rtcHttpCli, rtcThrPool,  ComCtrls,
  rtcMemory;

type

  { TForm1 }

  TForm1 = class(TForm)
    myPanel: TPanel;
    Panel1: TPanel;
    myBox: TPaintBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    eModule: TEdit;
    eServer: TEdit;
    xProxy: TCheckBox;
    ePort: TEdit;
    xBlocking: TCheckBox;
    xSSL: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lblCount: TLabel;
    lblTotal: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    btnFlood: TButton;
    xFlood: TCheckBox;
    xRepost: TCheckBox;
    btnConnect: TButton;
    xAutoConnect: TCheckBox;
    xMultiThreaded: TCheckBox;
    TabSheet3: TTabSheet;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    lblFlood: TLabel;
    lblClients: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    eConCnt: TEdit;
    eReqCnt: TEdit;
    btnMultiFlood: TButton;
    xReqAutoRepeat: TCheckBox;
    eUpdCnt: TEdit;
    xAutoDisconnect: TCheckBox;
    xReqAutoConnect: TCheckBox;
    Label22: TLabel;
    Label27: TLabel;
    btnMultiCreate: TButton;
    btnMultiSend: TButton;
    btnMultiDisconnect: TButton;
    Label28: TLabel;
    Label29: TLabel;
    btnMultiFree: TButton;
    btnMultiConnect: TButton;
    xReqMultiThread: TCheckBox;
    btnConnDisconn: TButton;
    xEncrypt: TCheckBox;
    xEncrypt2: TCheckBox;
    Label30: TLabel;
    eModuleHost: TEdit;
    xCompress: TCheckBox;
    xCompress2: TCheckBox;
    Label31: TLabel;
    lblMemTotal: TLabel;
    Timer1: TTimer;
    Label32: TLabel;
    eThreads: TEdit;
    xExtensiveTest: TCheckBox;
    xUseXML: TCheckBox;
    procedure Edit1Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure RtcResult2Return(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure RtcResult3Return(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure RtcClientConnect(Sender: TRtcConnection);
    procedure RtcClientDisconnect(Sender: TRtcConnection);
    procedure RtcClientModule1ResponseAbort(Sender: TRtcConnection);
    procedure eModuleChange(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure eServerChange(Sender: TObject);
    procedure btnFloodClick(Sender: TObject);
    procedure xBlockingClick(Sender: TObject);
    procedure xProxyClick(Sender: TObject);
    procedure RtcClientModule1ResponseDone(Sender: TRtcConnection);
    procedure FormCreate(Sender: TObject);
    procedure xRepostClick(Sender: TObject);
    procedure btnMultiFloodClick(Sender: TObject);
    procedure MultiResultReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure eMaxConExit(Sender: TObject);
    procedure eMaxReadExit(Sender: TObject);
    procedure eMaxWriteExit(Sender: TObject);
    
    procedure xLimitConnClick(Sender: TObject);
    procedure xSSLClick(Sender: TObject);
    procedure xMultiThreadedClick(Sender: TObject);
    procedure xAutoConnectClick(Sender: TObject);
    procedure btnMultiCreateClick(Sender: TObject);
    procedure btnMultiSendClick(Sender: TObject);
    
    procedure btnMultiFreeClick(Sender: TObject);

    procedure btnMultiDisconnectClick(Sender: TObject);
    procedure btnMultiConnectClick(Sender: TObject);
    procedure btnConnDisconnClick(Sender: TObject);
    procedure xEncryptClick(Sender: TObject);
    procedure xCompressClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure xUseXMLClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    FloodCnt:int64;
    ClientCnt,
    TotalCnt:int64;
    SimpleCall:boolean;
    CliCon:array of TRtcHttpClient;
    CliMod:array of TRtcClientModule;

  
    RtcClient: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    RtcResult1: TRtcResult;
    RtcResult2: TRtcResult;
    RtcResult3: TRtcResult;
    MultiResult: TRtcResult;
    SendResult: TRtcResult;


    procedure SendResultReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure MultiClientConnect(Sender: TRtcConnection);
    procedure MultiClientDisconnect(Sender: TRtcConnection);

  end;

var
  Form1: TForm1;

implementation

procedure TForm1.RtcClientConnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcClientConnect)
  else
    begin
    btnConnect.Caption:='Disconnect';
    Label10.Caption:='Connected to '+Sender.PeerAddr+':'+Sender.PeerPort;
    end;
  end;

procedure TForm1.RtcClientDisconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcClientDisconnect)
  else
    begin
    btnConnect.Caption:='Connect';
    Label10.Caption:='Not connected.';
    Label10.Refresh;
    end;
  end;

procedure TForm1.Edit1Change(Sender: TObject);
  begin
  with RtcClientModule1 do
    begin
    StartCalls;
    try
      // Call the Add function
      with Data.NewFunction('add') do
        begin
        if Edit1.Text<>'' then Value['A']:=StrToFloat(Edit1.Text);
        if Edit2.Text<>'' then Value['B']:=StrToFloat(Edit2.Text);
        end;
      Call(RtcResult1);
      // Call Add and Mul in combination
      with Data.NewFunction
      ('add') do
        begin
        // Call function "add" and store the value into "A" parameter
        with NewFunction('A','add') do
          begin
          if Edit1.Text<>'' then Value['A']:=StrToFloat(Edit1.Text);
          if Edit2.Text<>'' then Value['B']:=StrToFloat(Edit2.Text);
          end;
        // Call function "mul" and store the value into "B" parameter
        with NewFunction('B','mul') do
          begin
          if Edit4.Text<>'' then Value['A']:=StrToFloat(Edit4.Text);
          if Edit5.Text<>'' then Value['B']:=StrToFloat(Edit5.Text);
          end;
        // Parameters "A" and "B" are passed down to the function "add"
        // to calculate the final result.
        end;
      Call(RtcResult3);
    finally
      // Post all prepared calls
      Post;
      end;
    end;
  end;

procedure TForm1.Edit4Change(Sender: TObject);
  begin
  with RtcClientModule1 do
    begin
    StartCalls;
    try
      // Call the Mull function ...
      with Data.NewFunction('mul') do
        begin
        if Edit4.Text<>'' then Value['A']:=StrToFloat(Edit4.Text);
        if Edit5.Text<>'' then Value['B']:=StrToFloat(Edit5.Text);
        end;
      Call(RtcResult2);
      // Call Add and Mul in combination ...
      with Data.NewFunction('add') do
        begin
        // Call function "add" and store the value into "A" parameter
        with NewFunction('A','add') do
          begin
          if Edit1.Text<>'' then Value['A']:=StrToFloat(Edit1.Text);
          if Edit2.Text<>'' then Value['B']:=StrToFloat(Edit2.Text);
          end;
        // Call function "mul" and store the value into "B" parameter
        with NewFunction('B','mul') do
          begin
          if Edit4.Text<>'' then Value['A']:=StrToFloat(Edit4.Text);
          if Edit5.Text<>'' then Value['B']:=StrToFloat(Edit5.Text);
          end;
        // Parameters "A" and "B" are passed down to the function "add"
        // to calculate the final result.
        end;
      Call(RtcResult3);
    finally
      // Post all prepared calls
      Post;
      end;
    end;
  end;

procedure TForm1.RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
var
  s: string;
  begin
  if Result.isType=rtc_Exception then
    begin
    Edit3.Text:='Error';
    Label3.Caption:=Result.asString;
    end
  else if Result.isType=rtc_Null then
    begin
    Edit3.Text:='NULL';
    Label3.Caption:='Failed';
    end
  else
    begin
    s := Result.Value;
    Edit3.Text:=s;
    Label3.Caption:='OK';
    end;
  end;

procedure TForm1.RtcResult2Return(Sender: TRtcConnection; Data, Result: TRtcValue);
var
  s : String;
  begin
  if Result.isType=rtc_Exception then
    begin
    Edit6.Text:='Error';
    Label4.Caption:=Result.asString;
    end
  else if Result.isType=rtc_Null then
    begin
    Edit6.Text:='NULL';
    Label4.Caption:='Failed';
    end
  else
    begin
    s := Result.Value;
    Edit6.Text:=s;
    Label4.Caption:='OK';
    end;
  end;

procedure TForm1.RtcResult3Return(Sender: TRtcConnection; Data, Result: TRtcValue);
var
  s: String;
  begin
  if Result.isType=rtc_Exception then
    begin
    Edit7.Text:='Error';
    Label9.Caption:=Result.asString;
    end
  else if Result.isType=rtc_Null then
    begin
    Edit7.Text:='NULL';
    Label9.Caption:='Failed';
    end
  else
    begin
    s:=Result.Value;
    Edit7.Text:=s;
    Label9.Caption:='OK';
    end;
  end;

procedure TForm1.RtcClientModule1ResponseAbort(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    if MessageDlg('Request reposted '+IntToStr(Request.Reposted)+' times without success.'#13#10+
                  'Try to repost the request again?'#13#10+
                  'If you choose "No", connection will be Closed.', mtError, [mbYes,mbNo],0)=mrYes then
      Request.Repost
    else
      begin
      SkipRequests;
      Disconnect;
      end;
  end;

procedure TForm1.eModuleChange(Sender: TObject);
  begin
  if (RtcClientModule1.ModuleHost<>eModuleHost.Text) or
     (RtcClientModule1.ModuleFileName<>eModule.Text)then
    begin
    RtcClient.SkipRequests;

    RtcClientModule1.ModuleFileName:=eModule.Text;
    RtcClientModule1.ModuleHost:=eModuleHost.Text;

    eModule.Text:=RtcClientModule1.ModuleFileName;
    eModuleHost.Text:=RtcClientModule1.ModuleHost;
    end;
  end;

procedure TForm1.btnConnectClick(Sender: TObject);
  begin
  RtcClient.ServerPort:=ePort.Text;
  if RtcClient.isConnected then
    RtcClient.Disconnect
  else
    RtcClient.Connect;
  end;

procedure TForm1.eServerChange(Sender: TObject);
  begin
  if (RtcClient.ServerAddr<>eServer.Text) or
     (RtcClient.ServerPort<>ePort.Text) then
    begin
    eModuleHost.Text:=eServer.Text;

    RtcClient.Disconnect;
    RtcClient.ServerAddr:=eServer.Text;
    RtcClient.ServerPort:=ePort.Text;
    RtcClientModule1.ModuleHost:=eModuleHost.Text;

    eServer.Text:=RtcClient.ServerAddr;
    ePort.Text:=RtcClient.ServerPort;
    eModuleHost.Text:=RtcClientModule1.ModuleHost;
    end;
  end;

procedure TForm1.RtcClientModule1ResponseDone(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    begin
    TotalCnt:=TotalCnt+1;
    lblTotal.Caption:=IntToStr(TotalCnt);
    lblCount.Caption:=IntToStr(RequestCount-1);
    if RequestCount=1 then
      if xFlood.Checked then
        btnFlood.Click;
    end;
  end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
  RtcClient:= TRtcHttpClient.Create(Self);
  RtcClientModule1:= TRtcClientModule.Create(Self);
  RtcResult1:= TRtcResult.Create(Self);
  RtcResult2:= TRtcResult.Create(Self);
  RtcResult3:= TRtcResult.Create(Self);
  MultiResult:= TRtcResult.Create(Self);
  SendResult:= TRtcResult.Create(Self);
  

  with RtcClient do begin
    ServerAddr := 'localhost';
    ServerPort := '81';
    OnConnect := RtcClientConnect;
    OnDisconnect := RtcClientDisconnect;
    ReconnectOn.ConnectError := True;
    ReconnectOn.ConnectLost := True;
    ReconnectOn.ConnectFail := True;
    AutoConnect := True;
  end;
  with RtcClientModule1 do begin
    AutoSyncEvents := True;
    Client := RtcClient;
    SecureKey := 'This is a test.';
    AutoRepost := 2;
    ModuleHost := 'LOCALHOST';
    ModuleFileName := '/TEST';
    OnResponseDone := RtcClientModule1ResponseDone;
    OnResponseAbort := RtcClientModule1ResponseAbort;
  end;
  RtcResult1.OnReturn := RtcResult1Return;
  RtcResult2.OnReturn := RtcResult2Return;
  RtcResult3.OnReturn := RtcResult3Return;
  MultiResult.OnReturn := MultiResultReturn;
  SendResult.OnReturn := SendResultReturn;
  
  StartLog;

  {$IFDEF RtcTest}
  LOG_THREAD_EXCEPTIONS:=True;
  LOG_EXCEPTIONS:=True;

  eServer.Text:='server';
  eModuleHost.Text:='server';
  xReqAutoRepeat.Checked:=True;
  PageControl1.ActivePage:=TabSheet3;
  {$endif}

  { If we wanted to test how fast compression works,
    we could force data compression, even though we know there will
    be no gain in compressing the data, only CPU usage increase. }
  //RTC_MIN_COMPRESS_SIZE:=0;

  TotalCnt:=0;
  Timer1Timer(Timer1);
  end;

procedure TForm1.xRepostClick(Sender: TObject);
  begin
  if xRepost.Checked then
    RtcClientModule1.AutoRepost:=-1 // unlimited repost
  else
    RtcClientModule1.AutoRepost:=2; // report up to 2 times.
  end;

(***************************************************)
(** Methods used by MULTI-CONNECTION FLOOD TEST! ***)
(***************************************************)

procedure TForm1.btnFloodClick(Sender: TObject);
  var
    a:integer;
  begin
  for a:=1 to 250 * 4 do // x 4 = 1.000
    begin
    Edit1.Text:=IntToStr(random(1000000));
    Edit2.Text:=IntToStr(random(1000000));
    Edit4.Text:=IntToStr(random(1000000));
    Edit5.Text:=IntToStr(random(1000000));
    end;
  end;

procedure TForm1.xBlockingClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.Blocking:=xBlocking.Checked;
  finally
    xBlocking.Checked:=RtcClient.Blocking;
    end;
  end;

procedure PrepareBigFunction(Func:TRtcFunctionInfo);
  var
    a:integer;
  begin
  with Func do
    begin
    with newRecord('data') do
      begin
      with NewFunction('func','add') do
        begin
        with NewFunction('A','add') do
          begin
          asInteger['A']:=random(1000000);
          asInteger['B']:=random(1000000);
          end;
        with NewFunction('B','mul') do
          begin
          asInteger['A']:=random(1000000);
          asInteger['B']:=random(1000000);
          end;
        end;
      with newArray('arr') do
        begin
        asBoolean[0]:=true;
        asBoolean[1]:=false;
        asInteger[2]:=random(123456789);
        asLargeInt[3]:=random(1234567890);
        asFloat[4]:=random(123456789)/1000;
        asCurrency[5]:=random(123456789)/100;
        asDateTime[6]:=Now;
        asException[7]:='Test Exception message';
        asVarName[8]:='Test Variable name';
        asWideString[9]:='Test Wide String';
        asText[10]:='Test Text';
        asString[11]:='Test String';
        with NewRecord(12) do
          begin
          asBoolean['bool1']:=true;
          asBoolean['bool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=random(1234567890);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          end;
        end;
      with newRecord('rec') do
        begin
        asBoolean['bool1']:=true;
        asBoolean['bool2']:=false;
        asInteger['int']:=random(123456789);
        asLargeInt['lint']:=random(1234567890);
        asFloat['float']:=random(123456789)/1000;
        asCurrency['curr']:=random(123456789)/100;
        asDateTime['dat']:=Now;
        asException['exc']:='Test Exception message';
        asVarName['var']:='Test Variable name';
        asWideString['wstr']:='Test Wide String';
        asText['txt']:='Test Text';
        asString['str']:='Test String';
        with NewRecord('rec') do
          begin
          asBoolean['bool1']:=true;
          asBoolean['bool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=random(1234567890);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          end;
        end;
      with NewDataSet('dset') do
        begin
        for a:=1 to 10 do // 10 complex records
          begin
          Append;
          asBoolean['bool1']:=true;
          asBoolean['bool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=random(1234567890)*random(100000);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          with newArray('arr') do
            begin
            asBoolean[0]:=true;
            asBoolean[1]:=false;
            asInteger[2]:=random(123456789);
            asLargeInt[3]:=random(1234567890)*random(100000);
            asFloat[4]:=random(123456789)/1000;
            asCurrency[5]:=random(123456789)/100;
            asDateTime[6]:=Now;
            asException[7]:='Test Exception message';
            asVarName[8]:='Test Variable name';
            asWideString[9]:='Test Wide String';
            asText[10]:='Test Text';
            asString[11]:='Test String';
            with NewRecord(12) do
              begin
              asBoolean['bool1']:=true;
              asBoolean['bool2']:=false;
              asInteger['int']:=random(123456789);
              asLargeInt['lint']:=random(1234567890)*random(100000);
              asFloat['float']:=random(1234567890)/10000;
              asCurrency['curr']:=random(1234567890)/100;
              asDateTime['dat']:=Now;
              asException['exc']:='Test Exception message';
              asVarName['var']:='Test Variable name';
              asWideString['wstr']:='Test Wide String';
              asText['txt']:='Test Text';
              asString['str']:='Test String';
              end;
            end;
          with newRecord('rec') do
            begin
            asBoolean['bool1']:=true;
            asBoolean['bool2']:=false;
            asInteger['int']:=random(1234567890);
            asLargeInt['lint']:=random(1234567890)*random(100000);
            asFloat['float']:=random(1234567890)/(random(10000)+1);
            asCurrency['curr']:=random(1234567890)/(random(100)+1);
            asDateTime['dat']:=Now;
            asException['exc']:='Test Exception message';
            asVarName['var']:='Test Variable name';
            asWideString['wstr']:='Test Wide String';
            asText['txt']:='Test Text';
            asString['str']:='Test String';
            with NewRecord('rec') do
              begin
              asBoolean['bool1']:=true;
              asBoolean['bool2']:=false;
              asInteger['int']:=random(123456789);
              asLargeInt['lint']:=random(1234567890);
              asFloat['float']:=random(123456789)/(random(1000)+1);
              asCurrency['curr']:=random(123456789)/(random(100)+1);
              asDateTime['dat']:=Now;
              asException['exc']:='Test Exception message';
              asVarName['var']:='Test Variable name';
              asWideString['wstr']:='Test Wide String';
              asText['txt']:='Test Text';
              asString['str']:='Test String';
              end;
            end;
          end;
        end;
      end;
    end;
  end;

procedure TForm1.btnMultiFloodClick(Sender: TObject);
  var
    cnt,req,upd,a:integer;
  begin
  // DO NOT TRY TO CHANGE THIS WHEN CONNECTIONS ARE ACTIVE!
  try
    RTC_THREAD_POOL_MAX:=StrToInt(Trim(eThreads.Text));
  except
    Showmessage('Invalid value for Max Threads');
    Exit;
    end;

  myPanel.Refresh;

  SimpleCall:=not xExtensiveTest.Checked;

  ClientCnt:=0;
  FloodCnt:=0;
  lblClients.Caption:='0';
  lblFlood.Caption:='0';

  try
    cnt:=StrToInt(eConCnt.Text);
  except
    ShowMessage('Invalid value for Connection count');
    Exit;
    end;
  try
    req:=StrToInt(eReqCnt.Text);
  except
    ShowMessage('Invalid value for Request count');
    Exit;
    end;
  try
    upd:=StrToInt(eUpdCnt.Text);
  except
    ShowMessage('Invalid value for Update count');
    Exit;
    end;

  btnMultiCreate.Enabled:=False;

  btnMultiFlood.Enabled:=False;
  eConCnt.Enabled:=False;

  SetLength(CliCon,cnt);
  SetLength(CliMod,cnt);
  // Create new connection and client module components ...
  for a:=0 to cnt-1 do
    begin
    CliCon[a]:=TRtcHttpClient.Create(nil);
    CliMod[a]:=TRtcClientModule.Create(nil);
    with CliCon[a] do
      begin
      ServerAddr:=eServer.Text;
      ServerPort:=ePort.Text;
      MultiThreaded:=xReqMultiThread.Checked;

      AutoConnect:=xReqAutoConnect.Checked;
      UseProxy:=xProxy.Checked;
      UseSSL:=xSSL.Checked;
      Blocking:=xBlocking.Checked;

      ReconnectOn.ConnectError:=True;
      ReconnectOn.ConnectLost:=True;
      ReconnectOn.ConnectFail:=True;
      OnConnecting:=MultiClientConnect;
      OnDisconnecting:=MultiClientDisconnect;
      Timeout.AfterConnecting:=RtcClient.Timeout.AfterConnecting;
      Timeout.AfterConnect:=RtcClient.Timeout.AfterConnect;
      end;
    with CliMod[a] do
      begin
      AutoRepost:=-1; // unlimited
      AutoSessions:=True;
      if xEncrypt2.Checked then
        EncryptionKey:=16;
      {$IFDEF COMPRESS}
      if xCompress2.Checked then
        Compression:=cFast;
      {$ENDIF}
      ModuleFileName:=RtcClientModule1.ModuleFileName;
      ModuleHost:=RtcClientModule1.ModuleHost;
      SecureKey:=RtcClientModule1.SecureKey;
      DataFormat:=RtcClientModule1.DataFormat;
      Client:=CliCon[a];
      end;
    with CliCon[a].Info do
      begin
      asPtr['MOD']:=CliMod[a];
      asInteger['CNT']:=req;
      asInteger['LEFT']:=req;
      asInteger['MAX']:=req;
      asInteger['UPD']:=upd;
      asInteger['ID']:=a;
      end;
    end;
  myBox.Canvas.Brush.Color:=clBtnFace;
  myBox.Canvas.FillRect(Rect(0,0,myBox.Width,myBox.Height));
  myBox.Canvas.Brush.Color:=clNone;
  for a:=0 to Length(CliMod)-1 do
    begin
    myBox.Canvas.Pen.Color:=clMaroon;
    myBox.Canvas.MoveTo(a,0);
    myBox.Canvas.LineTo(a,myBox.Height-1);
    if not CliCon[a].AutoConnect then
      CliCon[a].Connect;
    with CliMod[a] do
      begin
      // Call the Add function
      if SimpleCall then
        begin
        with Data.NewFunction('add') do
          begin
          with NewFunction('A','add') do
            begin
            asInteger['A']:=random(1000000);
            asInteger['B']:=random(1000000);
            end;
          with NewFunction('B','mul') do
            begin
            asInteger['A']:=random(1000000);
            asInteger['B']:=random(1000000);
            end;
          end;
        end
      else
        PrepareBigFunction(Data.NewFunction('loopo'));
      Call(MultiResult);
      end;
    end;
  end;

procedure TForm1.MultiResultReturn(Sender: TRtcConnection; Data,Result: TRtcValue);
  var
    x,y,a,req,upd:integer;
  begin
  { It is not usual for one RTC application to dinamicaly create
    connection and clientmodule components at runtime.
    It is also adviseable to NOT do this, because it makes your code
    more err-prone, since you will also have to take care about
    releasing the components when they are no longer needed.
    If any normal client application, you should use only 1
    ClientModule (which is best to be placed on your Form)
    for sending remote function calls to a specific ServerModule. }
  if Sender=nil then // posted interactive
    begin
    { We will browse through all connection and
      ClientModule components to release them, one-by-one. }
    for a:=0 to length(CliCon)-1 do
      begin
      { Remove pointer to ClientModule from Connection's Info property.
        It is important to remove all pointers to objects
        stored in the Info property before the object is
        destroyed, to avoid geting access violations if
        component tries to access those objects later. }
      CliCon[a].Info.asPtr['MOD']:=nil;

      { Remove the ClientModule component from memory. }
      if CliMod[a].Client=nil then
        { "Release" only works for the component which posted
        the event interactively, so we will use it only if
        this even was posted interactively from component's event
        (check the end of this event). }
        CliMod[a].Release
      else
        begin
        { For all other ClientModule's, we will call "Free" directly,
          since we KNOW that those ClientModule's are not being used anymore. }
        CliMod[a].Free;
        end;

      // When using "Release", OnDisconnect event will NOT be called.
      CliCon[a].Release;
      end;
    ClientCnt:=0;
    lblClients.Caption:='0';
    end
  else if Result.isNull then
    begin
    Log('No Result from Server!');
    end
  else if Result.isType=rtc_Exception then
    begin
    Log('Exception from Server!'#13#10+Result.asException);
    end
  else with TRtcDataClient(Sender) do
    begin
    if Info.asInteger['CNT']>0 then
      begin
      if (Info.asInteger['LEFT']-Info.asInteger['CNT']>=Info.asInteger['UPD']) then
        if not inMainThread then
          Sync(MultiResultReturn,Data,Result)
        else
          begin
          x:=Info.asInteger['ID'];
          y:=round(Info.asInteger['CNT']/Info.asInteger['MAX']*myBox.Height);
          if Info.asInteger['CNT'] mod (Info.asInteger['UPD']*2) >= Info.asInteger['UPD'] then
            myBox.Canvas.Pen.Color:=clRed
          else
            myBox.Canvas.Pen.Color:=clMaroon;
          myBox.Canvas.MoveTo(x,0);
          myBox.Canvas.LineTo(x,y);
          myBox.Canvas.Pen.Color:=clLime;
          myBox.Canvas.LineTo(x,myBox.Height-1);

          FloodCnt:=FloodCnt+Info.asInteger['UPD'];
          lblFlood.Caption:=Format('%.0n',[FloodCnt*1.0]);
          lblFlood.Refresh;

          Info.asInteger['LEFT']:=Info.asInteger['LEFT']-Info.asInteger['UPD'];
          { If MultiThreaded, we had to do a Sync(), so ...
            Event execution will continue in background thread,
            directly after Sync() ... }
          if MultiThreaded then Exit;
          end;
      Info.asInteger['CNT']:=Info.asInteger['CNT']-1;
      with TRtcClientModule(Info.asPtr['MOD']) do
        begin
      { WARNING!
        I am calling a remote function from the result event ONLY to avoid filling
        a large number of identical requests into memory, since this is a flood-test.

        NOTE that I am using TRUE as 2nd parameter (FromInsideEvent) for this to work. }

        if SimpleCall then
          begin
          with Data.NewFunction('add') do
            begin
            with NewFunction('A','add') do
              begin
              asInteger['A']:=random(1000000);
              asInteger['B']:=random(1000000);
              end;
            with NewFunction('B','mul') do
              begin
              asInteger['A']:=random(1000000);
              asInteger['B']:=random(1000000);
              end;
            end;
          end
        else
          PrepareBigFunction(Data.NewFunction('loopo'));
        Call(MultiResult,True,Sender);
        end;
      end
    else if not inMainThread then
      // need to call the event synchronized, since we are accessing the GUI
      Sync(MultiResultReturn, Data,Result)
    else
      begin
      // We've gone through all requested interrations,
      // time to update the counter display and disconnect,
      // since we probably won't be using this connection anymore.
      x:=Info.asInteger['ID'];
      myBox.Canvas.Pen.Color:=clAqua;
      myBox.Canvas.MoveTo(x,0);
      myBox.Canvas.LineTo(x,myBox.Height-1);
      eConCnt.Text:=IntToStr(StrToInt(eConCnt.Text)-1);

      FloodCnt:=FloodCnt+Info.asInteger['UPD'];
      lblFlood.Caption:=Format('%.0n',[FloodCnt*1.0]);
      lblFlood.Refresh;

      if xAutoDisconnect.Checked then
        begin
        Info.asBoolean['closed']:=True;
        Disconnect;
        end;

      lblMemTotal.Caption:=Format('%.0n KB',[Get_AddressSpaceUsed*1.0]);

      // Are all connections finished?
      if eConCnt.Text='0' then
        begin
        btnMultiFlood.Enabled:=True;

        btnMultiCreate.Enabled:=True;

        eConCnt.Enabled:=True;
        eConCnt.Text:=IntToStr(length(CliCon));
        // Update our MAX, LEFT and CNT values.
        try
          req:=StrToInt(eReqCnt.Text);
        except
          ShowMessage('Invalid value for Request count');
          Exit;
          end;
        try
          upd:=StrToInt(eUpdCnt.Text);
        except
          ShowMessage('Invalid value for Update frequency');
          Exit;
          end;
        // Do we have to repeat the whole thing once again?
        if xReqAutoRepeat.Checked then
          begin
          btnMultiCreate.Enabled:=False;

          btnMultiFlood.Enabled:=False;
          eConCnt.Enabled:=False;
          { This loop will Post one request using each Client Module
            and re-open each connection by calling "Connect".
            We have to open the connections here again, because
            we chose to close them when they have finished their jobs. }
          myBox.Canvas.Brush.Color:=clBtnFace;
          myBox.Canvas.FillRect(Rect(0,0,myBox.Width,myBox.Height));
          for a:=0 to Length(CliMod)-1 do
            begin
            myBox.Canvas.Pen.Color:=clMaroon;
            myBox.Canvas.MoveTo(a,0);
            myBox.Canvas.LineTo(a,myBox.Height-1);
            with CliMod[a] do
              begin
              // Call the Add function
              if SimpleCall then
                begin
                with Data.NewFunction('add') do
                  begin
                  with NewFunction('A','add') do
                    begin
                    asInteger['A']:=random(1000000);
                    asInteger['B']:=random(1000000);
                    end;
                  with NewFunction('B','mul') do
                    begin
                    asInteger['A']:=random(1000000);
                    asInteger['B']:=random(1000000);
                    end;
                  end;
                end
              else
                PrepareBigFunction(Data.NewFunction('loopo'));
              Call(MultiResult, CliCon[a]=Sender, CliCon[a]);
              end;
            with CliCon[a] do
              begin
              with Info do
                begin
                asInteger['MAX']:=req;
                asInteger['CNT']:=req;
                asInteger['LEFT']:=req;
                asInteger['UPD']:=upd;
                if asBoolean['closed'] then
                  begin
                  asBoolean['closed']:=False;
                  if not AutoConnect then
                    Connect;
                  end;
                end;
              end;
            end;
          end
        else
          begin
          with TRtcClientModule(Info.asPtr['MOD']) do
          { Looks like we won't be needing those connection and
            ClientModule components, it's time to release them from memory.
            Since we are not allowed to release the ClientModule from
            inside that module's result event, we need to post this
            request to be executed interactive (check start of this event).

            We will also clear the "Client" property of our ClientModule,
            to know that we posted Interactive from here. }
            Client:=nil;
          PostInteractive;
          end;
        end;
      end;
    end;
  end;

procedure TForm1.eMaxConExit(Sender: TObject);
begin

end;

procedure TForm1.eMaxReadExit(Sender: TObject);
begin

end;

procedure TForm1.eMaxWriteExit(Sender: TObject);
begin

end;

procedure TForm1.xLimitConnClick(Sender: TObject);
begin

end;

procedure TForm1.xProxyClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.UseProxy:=xProxy.Checked;
  finally
    xProxy.Checked:=RtcClient.UseProxy;
    end;
  end;

procedure TForm1.xSSLClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.UseSSL:=xSSL.Checked;
  finally
    xSSL.Checked:=RtcClient.UseSSL;
    if xSSL.Checked then
      ePort.Text:='443'
    else
      ePort.Text:='81';
    end;
  end;

procedure TForm1.xMultiThreadedClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.MultiThreaded:=xMultiThreaded.Checked;
  finally
    xMultiThreaded.Checked:=RtcClient.MultiThreaded;
    end;
  end;

procedure TForm1.xAutoConnectClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.AutoConnect:=xAutoConnect.Checked;
  finally
    xAutoConnect.Checked:=RtcClient.AutoConnect;
    end;
  end;

procedure TForm1.btnMultiCreateClick(Sender: TObject);
  var
    cnt,req,a:integer;
  begin
  myPanel.Refresh;
  SimpleCall:=not xExtensiveTest.Checked;

  ClientCnt:=0;
  FloodCnt:=0;
  lblClients.Caption:='0';
  lblFlood.Caption:='0';

  try
    req:=StrToInt(eReqCnt.Text);
  except
    ShowMessage('Invalid value for Request count');
    Exit;
    end;
  try
    cnt:=StrToInt(eConCnt.Text);
  except
    ShowMessage('Invalid value for Connection count');
    Exit;
    end;

  btnMultiFlood.Enabled:=False;

  btnMultiCreate.Enabled:=False;

  btnMultiConnect.Enabled:=True;
  btnMultiSend.Enabled:=True;
  btnMultiDisconnect.Enabled:=True;
  btnMultiFree.Enabled:=True;

  eConCnt.Enabled:=False;

  SetLength(CliCon,cnt);
  SetLength(CliMod,cnt);
  // Create new connection and client module components ...
  for a:=0 to cnt-1 do
    begin
    CliCon[a]:=TRtcHttpClient.Create(nil);
    CliMod[a]:=TRtcClientModule.Create(nil);
    with CliCon[a] do
      begin
      ServerAddr:=eServer.Text;
      ServerPort:=ePort.Text;
      MultiThreaded:=xReqMultiThread.Checked;
      AutoConnect:=xReqAutoConnect.Checked;
      UseProxy:=xProxy.Checked;
      UseSSL:=xSSL.Checked;

      ReconnectOn.ConnectError:=True;
      ReconnectOn.ConnectLost:=True;
      ReconnectOn.ConnectFail:=True;
      OnConnecting:=MultiClientConnect;
      OnDisconnecting:=MultiClientDisconnect;
      Timeout.AfterConnecting:=RtcClient.Timeout.AfterConnecting;
      Timeout.AfterConnect:=RtcClient.Timeout.AfterConnect;
      Info.asInteger['ID']:=a;
      Info.asInteger['CNT']:=0;
      Info.asInteger['MAX']:=req;
      end;
    with CliMod[a] do
      begin
      AutoRepost:=-1; // unlimited
      AutoSessions:=True;
      if xEncrypt2.Checked then
        EncryptionKey:=16;
      {$IFDEF COMPRESS}
      if xCompress.Checked then
        Compression:=cFast;
      {$ENDIF}

      ModuleFileName:=RtcClientModule1.ModuleFileName;
      ModuleHost:=RtcClientModule1.ModuleHost;
      SecureKey:=RtcClientModule1.SecureKey;
      DataFormat:=RtcClientModule1.DataFormat;
      Client:=CliCon[a];
      end;
    end;
  myBox.Canvas.Brush.Color:=clBtnFace;
  myBox.Canvas.FillRect(Rect(0,0,myBox.Width,myBox.Height));
  myBox.Canvas.Brush.Color:=clNone;
  for a:=0 to Length(CliMod)-1 do
    begin
    myBox.Canvas.Pen.Color:=clMaroon;
    myBox.Canvas.MoveTo(a,0);
    myBox.Canvas.LineTo(a,myBox.Height-1);
    if not CliCon[a].AutoConnect then
      CliCon[a].Connect;
    end;
  end;

procedure TForm1.btnMultiSendClick(Sender: TObject);
  var
    a:integer;
  begin
  SimpleCall:=not xExtensiveTest.Checked;
  for a:=0 to Length(CliMod)-1 do
    begin
    with CliMod[a] do
      begin
      // Call the Add function
      if SimpleCall then
        begin
        with Data.NewFunction('add') do
          begin
          with NewFunction('A','add') do
            begin
            asInteger['A']:=random(1000000);
            asInteger['B']:=random(1000000);
            end;
          with NewFunction('B','mul') do
            begin
            asInteger['A']:=random(1000000);
            asInteger['B']:=random(1000000);
            end;
          end;
        end
      else
        PrepareBigFunction(Data.NewFunction('loopo'));
      Call(SendResult);
      end;
    end;
  end;

procedure TForm1.SendResultReturn(Sender: TRtcConnection; Data,Result: TRtcValue);
  var
    x,y:integer;
  begin
  with TRtcDataClient(Sender) do
    if not inMainThread then
      Sync(SendResultReturn,Data,Result)
    else
      begin
      Info.asInteger['CNT']:=Info.asInteger['CNT']+1;
      if Info.asInteger['CNT']>=Info.asInteger['MAX'] then
        Info.asInteger['CNT']:=1;

      x:=Info.asInteger['ID'];
      y:=myBox.Height-round(Info.asInteger['CNT']/Info.asInteger['MAX']*myBox.Height);

      if Info.asInteger['CNT'] mod 2=0 then
        myBox.Canvas.Pen.Color:=clMaroon
      else
        myBox.Canvas.Pen.Color:=clRed;

      myBox.Canvas.MoveTo(x,0);
      myBox.Canvas.LineTo(x,y);
      myBox.Canvas.Pen.Color:=clLime;
      myBox.Canvas.LineTo(x,myBox.Height);

      FloodCnt:=FloodCnt+1;
      lblFlood.Caption:=Format('%.0n',[FloodCnt*1.0]);
      lblFlood.Refresh;
      end;
  end;

procedure TForm1.btnMultiFreeClick(Sender: TObject);
  var
    a:integer;
  begin
  // We are releasing all connection components.
  // This will free them from memory,
  // without calling "OnDisconnect".
  // This is something that you should NOT do
  // from a normal client application.
  for a:=0 to length(CliCon)-1 do
    begin
    CliCon[a].Release;
    CliMod[a].Free;
    end;
  eConCnt.Enabled:=True;
  eConCnt.Text:=IntToStr(length(CliCon));

  SetLength(CliCon,0);
  SetLength(CliMod,0);

  ClientCnt:=0;
  lblClients.Caption:='0';

  btnMultiFlood.Enabled:=True;

  btnMultiCreate.Enabled:=True;
  btnMultiConnect.Enabled:=False;
  btnMultiSend.Enabled:=False;
  btnMultiDisconnect.Enabled:=False;
  btnMultiFree.Enabled:=False;
  end;

procedure TForm1.MultiClientConnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(MultiClientConnect)
  else
    begin
    Inc(ClientCnt);
    lblClients.Caption:=IntToStr(ClientCnt);
    end;
  end;

procedure TForm1.MultiClientDisconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(MultiClientDisconnect)
  else
    begin
    Dec(ClientCnt);
    lblClients.Caption:=IntToStr(ClientCnt);
    end;
  end;

procedure TForm1.btnMultiConnectClick(Sender: TObject);
  var
    a:integer;
  begin
  for a:=0 to Length(CliMod)-1 do
    CliCon[a].Connect;
  end;

procedure TForm1.btnMultiDisconnectClick(Sender: TObject);
  var
    a:integer;
  begin
  for a:=0 to length(CliCon)-1 do
    CliCon[a].Disconnect;
  end;

procedure TForm1.btnConnDisconnClick(Sender: TObject);
  var
    a:integer;
  begin
  for a:=1 to 10 do
    begin
    RtcClient.Connect;
    RtcClient.Disconnect;
    end;
  end;

procedure TForm1.xEncryptClick(Sender: TObject);
  begin
  if xEncrypt.Checked then
    RtcClientModule1.EncryptionKey:=16
  else
    begin
    RtcClientModule1.EncryptionKey:=0;
    RtcClientModule1.AutoSessions:=False;
    end;
  end;

procedure TForm1.xCompressClick(Sender: TObject);
  begin
  {$IFDEF COMPRESS}
  if xCompress.Checked then
    RtcClientModule1.Compression:=cFast
  else
    RtcClientModule1.Compression:=cNone;
  {$ENDIF}
  end;

procedure TForm1.Timer1Timer(Sender: TObject);
  begin
  lblMemTotal.Caption:=Format('%.0n KB',[Get_AddressSpaceUsed*1.0]);
  end;

procedure TForm1.xUseXMLClick(Sender: TObject);
  begin
  if xUseXML.Checked then
    RtcClientModule1.DataFormat:=fmt_XMLRPC
  else
    RtcClientModule1.DataFormat:=fmt_RTC;
  end;

initialization
{$i AppClient_Unit.lrs}

end.
