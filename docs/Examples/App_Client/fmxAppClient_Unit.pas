unit fmxAppClient_Unit;

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{ To compile the project with StreamSec Tools 2.1+ components, declare
  the StreamSecII compiler directive below or in the "rtcDeploy.inc" file. }

{.$DEFINE StreamSecII}

interface

uses
  SysUtils, Classes, Types, UITypes,

  FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.ExtCtrls, FMX.Types,
  FMX.Edit, FMX.TabControl, FMX.Objects,
  {$IFDEF IDE_XE3up} FMX.StdCtrls, {$ENDIF}
  {$IFDEF IDE_XE8up} FMX.Controls.Presentation, {$ENDIF}

{$IF Defined(StreamSecII)}
  rtcSSecTest,
{$IFEND}

  rtcLog, rtcSystem,
  rtcFunction, rtcDataCli,
  rtcCliModule, rtcInfo, rtcConn,
  rtcHttpCli, rtcThrPool, rtcPlugins;

const
  LOOP_MAX_AVG=20;

type
  TForm1 = class(TForm)
    RtcClient: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    RtcResult1: TRtcResult;
    RtcResult2: TRtcResult;
    RtcResult3: TRtcResult;
    MultiResult: TRtcResult;
    Panel1: TPanel;
    PageControl1: TTabControl;
    TabSheet1: TTabItem;
    TabSheet2: TTabItem;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    eModule: TEdit;
    eServer: TEdit;
    xProxy: TCheckBox;
    ePort: TEdit;
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
    TabSheet3: TTabItem;
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
    SendResult: TRtcResult;
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
    UpdateTimer: TTimer;
    Label32: TLabel;
    eThreads: TEdit;
    xExtensiveTest: TCheckBox;
    xUseXML: TCheckBox;
    xWinHTTP: TCheckBox;
    xBlocking: TCheckBox;
    xCryptPlugin: TCheckBox;
    lblPluginState: TLabel;
    xRTCTimeouts: TCheckBox;
    xHTTP10: TCheckBox;
    StopFloodTimer: TTimer;
    btnMultiSkip: TButton;
    xAPITimeouts: TCheckBox;
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
    procedure xProxyClick(Sender: TObject);
    procedure RtcClientModule1ResponseDone(Sender: TRtcConnection);
    procedure FormCreate(Sender: TObject);
    procedure xRepostClick(Sender: TObject);
    procedure btnMultiFloodClick(Sender: TObject);
    procedure MultiResultReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure xSSLClick(Sender: TObject);
    procedure xMultiThreadedClick(Sender: TObject);
    procedure xAutoConnectClick(Sender: TObject);
    procedure btnMultiCreateClick(Sender: TObject);
    procedure btnMultiSendClick(Sender: TObject);
    procedure SendResultReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure btnMultiFreeClick(Sender: TObject);
    procedure MultiClientConnect(Sender: TRtcConnection);
    procedure MultiClientDisconnect(Sender: TRtcConnection);
    procedure btnMultiDisconnectClick(Sender: TObject);
    procedure btnMultiConnectClick(Sender: TObject);
    procedure btnConnDisconnClick(Sender: TObject);
    procedure xEncryptClick(Sender: TObject);
    procedure xCompressClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure xUseXMLClick(Sender: TObject);
    procedure xWinHTTPClick(Sender: TObject);
    procedure xBlockingClick(Sender: TObject);
    procedure xCryptPluginClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure xRTCTimeoutsClick(Sender: TObject);
    procedure xHTTP10Click(Sender: TObject);
    procedure RtcClientModule1EncryptWrongKey(Sender: TRtcConnection);
    procedure RtcCliModEncryptWrongKey(Sender: TRtcConnection);
    procedure StopFloodTimerTimer(Sender: TObject);
    procedure btnMultiSkipClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure xAPITimeoutsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FloodCnt,
    TotalCnt,
    FloodIgn:int64;
    FloodCounter,
    FCliOpen,FCliClose:longint;
    FCS:TRtcCritSec;
    SimpleCall:boolean;

    // used for performance logging
    LoopCnt,LoopMult:int64;
    LoopStart,
    LoopTime,
    LoopDiff:longword;
    // used for averaging over the last 10 logs
    LoopPerf,
    LoopAvg:double;
    LoopMax,LoopLoc:byte;
    LoopArr:array[1..LOOP_MAX_AVG] of double;

    CliCon:array of TRtcHttpClient;
    CliMod:array of TRtcClientModule;
    RtcCryptPlugin1: TRtcCryptPlugin;

    procedure StopFloodTest;
    procedure UpdateLogs(Sender:TRtcConnection);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  StartLog;

  FCliOpen:=0;
  FCliClose:=0;
  FloodCounter:=0;

  FCS:=TRtcCritSec.Create;

{$IFDEF StreamSecII}
  { This is a simple DEMO, so we do not care
    if the Server is using an expired certificate: }
  AllowExpiredCertificates(True);

  if not File_Exists('root.cer') then
    begin
    { This is a simple DEMO, so we do not care about security.
      If we do not have a root certificate on the client,
      we will use a HACK to allow the client to work with ANY Server: }
    BeGullableAndTrustAnythingSentToYou(True);
    { This is a simple DEMO, so we will accept any certificate: }
    AddCertificateNameMap('*','*');
    end
  else
    begin
    // We have a root certificate, let's load it ...
    AddClientRootCertFile('root.cer');
    AddClientPFXFile('client.pfx','abc');
    { This is a simple DEMO, so we will accept our "locahost" certificate,
      even if our test Server is running on a remote PC and not locally: }
    AddCertificateNameMap('*','localhost');
    end;

  RtcCryptPlugin1:=GetClientCryptPlugin;
  if assigned(RtcCryptPlugin1) then
    begin
    xCryptPlugin.Caption:='CryptPlugin (use SSL)';
    xCryptPlugin.Font.Style:=[fsBold];
    lblPluginState.Caption:='CryptPlugin using SSL: Enabled though StreamSec Tools II.';
    end
  else
{$ENDIF}
    begin
    // "self" parameter in Create() is important to have the component destroyed when the form is destroyed.
    RtcCryptPlugin1 := TRtcDummyCryptPlugin.Create(self);
    lblPluginState.Text:='Using CrytPlugin dummy: SSL with CryptPlugin is disabled!';
    end;

  {$IFDEF RtcTest}
  eServer.Text:='server';
  eModuleHost.Text:='server';
  eReqCnt.Text:='50';
  xRTCTimeouts.Checked:=True;
  xReqAutoRepeat.Checked:=True;

  { If we wanted to test how fast compression works,
    we could force data compression, even though we know there will
    be no gain in compressing the data, only CPU usage increase. }
  // RTC_MIN_COMPRESS_SIZE:=0;
  {$endif}

  TotalCnt:=0;
  lblCount.Text:='0';

  UpdateTimerTimer(UpdateTimer);
  end;

procedure TForm1.RtcClientConnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcClientConnect)
  else
    begin
    btnConnect.Text:='Disconnect';
    Label10.Text:='Connected to '+Sender.sPeerAddr+':'+Sender.sPeerPort;
    end;
  end;

procedure TForm1.RtcClientDisconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcClientDisconnect)
  else
    begin
    btnConnect.Text:='Connect';
    Label10.Text:='Not connected.';
    Label10.Repaint;
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
  begin
  if Result.isType=rtc_Exception then
    begin
    Edit3.Text:='Error';
    Label3.Text:=Result.asException;
    end
  else if Result.isType=rtc_Null then
    begin
    Edit3.Text:='NULL';
    Label3.Text:='Failed';
    end
  else
    begin
    Edit3.Text:=Result.Value;
    Label3.Text:='OK';
    end;
  end;

procedure TForm1.RtcResult2Return(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType=rtc_Exception then
    begin
    Edit6.Text:='Error';
    Label4.Text:=Result.asException;
    end
  else if Result.isType=rtc_Null then
    begin
    Edit6.Text:='NULL';
    Label4.Text:='Failed';
    end
  else
    begin
    Edit6.Text:=Result.Value;
    Label4.Text:='OK';
    end;
  end;

procedure TForm1.RtcResult3Return(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType=rtc_Exception then
    begin
    Edit7.Text:='Error';
    Label9.Text:=Result.asException;
    end
  else if Result.isType=rtc_Null then
    begin
    Edit7.Text:='NULL';
    Label9.Text:='Failed';
    end
  else
    begin
    Edit7.Text:=Result.Value;
    Label9.Text:='OK';
    end;
  end;

procedure TForm1.RtcClientModule1ResponseAbort(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    if MessageDlg('Request reposted '+IntToStr(Request.Reposted)+' times without success.'#13#10+
                  'Try to repost the request again?'#13#10+
                  'If you choose "No", connection will be Closed.',
                  TMsgDlgType.mtError, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],0) = mrYes then
      Request.Repost
    else
      begin
      SkipRequests;
      Disconnect;
      end;
  end;

procedure TForm1.eModuleChange(Sender: TObject);
  begin
  if (RtcClientModule1.ModuleHost<>RtcString(eModuleHost.Text)) or
     (RtcClientModule1.ModuleFileName<>RtcString(eModule.Text))then
    begin
    RtcClient.SkipRequests;

    RtcClientModule1.ModuleFileName:=RtcString(eModule.Text);
    RtcClientModule1.ModuleHost:=RtcString(eModuleHost.Text);

    eModule.Text:=String(RtcClientModule1.ModuleFileName);
    eModuleHost.Text:=String(RtcClientModule1.ModuleHost);
    end;
  end;

procedure TForm1.btnConnectClick(Sender: TObject);
  begin
  if RtcClient.isConnected then
    RtcClient.Disconnect
  else
    RtcClient.Connect;
  end;

procedure TForm1.eServerChange(Sender: TObject);
  begin
  if (RtcClient.sServerAddr<>eServer.Text) or
     (RtcClient.sServerPort<>ePort.Text) then
    begin
    eModuleHost.Text:=eServer.Text;

    RtcClient.Disconnect;
    RtcClient.ServerAddr:=RtcString(eServer.Text);
    RtcClient.ServerPort:=RtcString(ePort.Text);
    RtcClientModule1.ModuleHost:=RtcString(eModuleHost.Text);

    eServer.Text:=RtcClient.sServerAddr;
    ePort.Text:=RtcClient.sServerPort;
    eModuleHost.Text:=String(RtcClientModule1.ModuleHost);
    end;
  end;

procedure TForm1.RtcClientModule1ResponseDone(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    begin
    TotalCnt:=TotalCnt+1;
    lblTotal.Text:=IntToStr(TotalCnt);
    lblCount.Text:=IntToStr(RequestCount-1);
    if RequestCount=1 then
      if xFlood.isChecked then
        btnFloodClick(btnFlood);
    end;
  end;

procedure TForm1.xRepostClick(Sender: TObject);
  begin
  if xRepost.isChecked then
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
  for a:=1 to 250 do // x 4 = 1.000
    begin
    Edit1.Text:=IntToStr(random(1000000));
    Edit2.Text:=IntToStr(random(1000000));
    Edit4.Text:=IntToStr(random(1000000));
    Edit5.Text:=IntToStr(random(1000000));
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
      with NewFunction('A','add') do
        begin
        asInteger['A']:=random(1000000);
        asInteger['B']:=random(1000000);
        end;
      with NewFunction('B','mul') do
        begin
        asCardinal['A']:=random(1000000);
        asCardinal['B']:=random(1000000);
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
        asCardinal[12]:=random(1234567890);
        asOID[13]:=random(1234567890);
        with NewRecord(14) do
          begin
          asBoolean['abool1']:=true;
          asBoolean['abool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=random(123456789);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          asCardinal['card']:=random(1234567890);
          asOID['oid']:=random(1234567890);
          end;
        with newArray(15) do
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
          asCardinal[12]:=random(1234567890);
          asOID[13]:=random(1234567890);
          end;
        end;
      with newRecord('rec') do
        begin
        asBoolean['bbool1']:=true;
        asBoolean['bbool2']:=false;
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
        asCardinal['card']:=random(1234567890);
        asOID['oid']:=random(1234567890);
        with NewRecord('rec') do
          begin
          asBoolean['cbool1']:=true;
          asBoolean['cbool2']:=false;
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
          asCardinal['card']:=random(1234567890);
          asOID['oid']:=random(1234567890);
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
          asCardinal[12]:=random(1234567890);
          asOID[13]:=random(1234567890);
          end;
        end;
      with NewDataSet('dset') do
        begin
        for a:=1 to 11 do // 11 complex records
          begin
          Append;
          asBoolean['dbool1']:=true;
          asBoolean['dbool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=int64(random(1234567890))*random(100000);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          asCardinal['card']:=random(1234567890);
          asOID['oid']:=random(1234567890);
          with newArray('arr') do
            begin
            asBoolean[0]:=true;
            asBoolean[1]:=false;
            asInteger[2]:=random(123456789);
            asLargeInt[3]:=int64(random(1234567890))*random(100000);
            asFloat[4]:=random(123456789)/1000;
            asCurrency[5]:=random(123456789)/100;
            asDateTime[6]:=Now;
            asException[7]:='Test Exception message';
            asVarName[8]:='Test Variable name';
            asWideString[9]:='Test Wide String';
            asText[10]:='Test Text';
            asString[11]:='Test String';
            asCardinal[12]:=random(1234567890);
            asOID[13]:=random(1234567890);
            with NewRecord(14) do
              begin
              asBoolean['ebool1']:=true;
              asBoolean['ebool2']:=false;
              asInteger['int']:=random(123456789);
              asLargeInt['lint']:=int64(random(1234567890))*random(100000);
              asFloat['float']:=random(1234567890)/10000;
              asCurrency['curr']:=random(1234567890)/100;
              asDateTime['dat']:=Now;
              asException['exc']:='Test Exception message';
              asVarName['var']:='Test Variable name';
              asWideString['wstr']:='Test Wide String';
              asText['txt']:='Test Text';
              asString['str']:='Test String';
              asCardinal['car']:=random(1234567890);
              asOID['oid']:=random(1234567890);
              end;
            with newArray(15) do
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
              asCardinal[12]:=random(1234567890);
              asOID[13]:=random(1234567890);
              end;
            end;
          with newRecord('rec') do
            begin
            asBoolean['fbool1']:=true;
            asBoolean['fbool2']:=false;
            asInteger['int']:=random(1234567890);
            asLargeInt['lint']:=int64(random(1234567890))*random(100000);
            asFloat['float']:=random(1234567890)/(random(10000)+1);
            asCurrency['curr']:=random(1234567890)/(random(100)+1);
            asDateTime['dat']:=Now;
            asException['exc']:='Test Exception message';
            asVarName['var']:='Test Variable name';
            asWideString['wstr']:='Test Wide String';
            asText['txt']:='Test Text';
            asString['str']:='Test String';
            asCardinal['card']:=random(1234567890);
            asOID['oid']:=random(1234567890);
            with NewRecord('rec') do
              begin
              asBoolean['gbool1']:=true;
              asBoolean['gbool2']:=false;
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
              asCardinal['card']:=random(1234567890);
              asOID['OID']:=random(1234567890);
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
              asCardinal[12]:=random(1234567890);
              asOID[13]:=random(1234567890);
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
  SimpleCall:=not xExtensiveTest.isChecked;

  if assigned(Sender) then
    begin
    // DO NOT TRY TO CHANGE THIS WHEN CONNECTIONS ARE ACTIVE!
    RTC_THREAD_POOL_MAX:=StrToIntDef(Trim(eThreads.Text),16);

    FCliOpen:=0;
    FCliClose:=0;
    FloodCounter:=0;

    FloodCnt:=0;
    FloodIgn:=0;
    LoopCnt:=0;
    LoopMax:=0;
    LoopTime:=0;
    LoopMult:=1;
    lblClients.Text:='0';
    lblFlood.Text:='0';
    Log('Time (sec); Mem (KB); Conn+ ; Conn- ; Conn= ; Results; Avg/Sec; 10Avg/Sec; Curr/Sec','PERF');

    cnt:=StrToIntDef(eConCnt.Text,1);
    end
  else
    cnt:=length(CliCon);

  req:=StrToIntDef(Trim(eReqCnt.Text),1);
  upd:=StrToIntDef(Trim(eUpdCnt.Text),1);

  btnMultiCreate.Enabled:=False;

  btnMultiFlood.Enabled:=False;
  eConCnt.Enabled:=False;

  if assigned(Sender) then
    begin
    SetLength(CliCon,cnt);
    SetLength(CliMod,cnt);
    end
  else
    begin
    for a:=0 to cnt-1 do
      begin
      with CliCon[a] do
        begin
        FixupRequest:=RtcClient.FixupRequest;
        if (ServerAddr<>RtcString(eServer.Text)) or
           (ServerPort<>RtcString(ePort.Text)) or
           (MultiThreaded<>xReqMultiThread.isChecked) or
           (CryptPlugin<>RtcClient.CryptPlugin) or
           (AutoConnect<>xReqAutoConnect.isChecked) or
           (UseProxy<>xProxy.isChecked) or
           (UseWinHTTP<>xWinHTTP.isChecked) or
           (UseSSL<>xSSL.isChecked) or
           (Blocking<>xBlocking.isChecked) then
          begin
          Disconnect;

          ServerAddr:=RtcString(eServer.Text);
          ServerPort:=RtcString(ePort.Text);
          MultiThreaded:=xReqMultiThread.isChecked;
          CryptPlugin:=RtcClient.CryptPlugin;
          FixupRequest:=RtcClient.FixupRequest;

          AutoConnect:=xReqAutoConnect.isChecked;
          UseProxy:=xProxy.isChecked;
          UseWinHTTP:=xWinHTTP.isChecked;
          UseSSL:=xSSL.isChecked;
          Blocking:=xBlocking.isChecked;

          TimeoutsOfAPI.ReceiveTimeout:=RtcClient.TimeoutsOfAPI.ReceiveTimeout;
          TimeoutsOfAPI.SendTimeout:=RtcClient.TimeoutsOfAPI.SendTimeout;
          Timeout.AfterConnecting:=RtcClient.Timeout.AfterConnecting;
          Timeout.AfterConnect:=RtcClient.Timeout.AfterConnect;
          end;
        end;
      end;
    end;

  // Create and/or Setup new connection and client module components ...
  for a:=0 to cnt-1 do
    begin
    if assigned(Sender) then
      begin
      CliCon[a]:=TRtcHttpClient.Create(nil);
      CliMod[a]:=TRtcClientModule.Create(nil);
      with CliCon[a] do
        begin
        ServerAddr:=RtcString(eServer.Text);
        ServerPort:=RtcString(ePort.Text);
        MultiThreaded:=xReqMultiThread.isChecked;
        CryptPlugin:=RtcClient.CryptPlugin;
        FixupRequest:=RtcClient.FixupRequest;

        AutoConnect:=xReqAutoConnect.isChecked;
        UseProxy:=xProxy.isChecked;
        UseWinHTTP:=xWinHTTP.isChecked;
        UseSSL:=xSSL.isChecked;
        Blocking:=xBlocking.isChecked;

        ReconnectOn.ConnectError:=True;
        ReconnectOn.ConnectLost:=True;
        ReconnectOn.ConnectFail:=True;
        OnConnecting:=MultiClientConnect;
        OnDisconnecting:=MultiClientDisconnect;
        TimeoutsOfAPI.ReceiveTimeout:=RtcClient.TimeoutsOfAPI.ReceiveTimeout;
        TimeoutsOfAPI.SendTimeout:=RtcClient.TimeoutsOfAPI.SendTimeout;
        Timeout.AfterConnecting:=RtcClient.Timeout.AfterConnecting;
        Timeout.AfterConnect:=RtcClient.Timeout.AfterConnect;
        end;
      CliMod[a].Client:=CliCon[a];
      end;
    with CliMod[a] do
      begin
      AutoRepost:=-1; // unlimited
      AutoSessions:=True;
      if xEncrypt2.isChecked then
        EncryptionKey:=16;
      if xCompress2.isChecked then
        Compression:=cFast;
      ModuleFileName:=RtcClientModule1.ModuleFileName;
      ModuleHost:=RtcClientModule1.ModuleHost;
      SecureKey:=RtcClientModule1.SecureKey;
      DataFormat:=RtcClientModule1.DataFormat;
      OnEncryptWrongKey:=RtcCliModEncryptWrongKey;
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

  for a:=0 to Length(CliMod)-1 do
    begin
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
            asCardinal['A']:=random(1000000);
            asCardinal['B']:=random(1000000);
            end;
          end;
        end
      else
        PrepareBigFunction(Data.NewFunction('loopo'));
      Call(MultiResult);
      end;
    end;
  end;

procedure TForm1.UpdateLogs(Sender:TRtcConnection);
  var
    a:integer;
  begin
  if LoopCnt>=1000 then // log max once every 1000 requests
    begin
    LoopDiff:=GetTickTime;
    if LoopTime=0 then
      begin // ignore the 1st result, server might have not been listening immediately.
      FloodIgn:=FloodCnt;
      LoopTime:=LoopDiff;
      LoopStart:=LoopTime;
      LoopMult:=1;
      end
    else
      begin
      LoopTime:=LoopDiff-LoopTime; // time difference
      if LoopTime<1000 then // do not log more often than once every 5 seconds
        begin
        // return our "LoopTime" back to the original value:
        LoopTime:=LoopDiff-LoopTime; // LoopDif-(LoopDif-LoopTime) = LoopDif-LoopDif+LoopTime = LoopTime
        Inc(LoopMult); // incrase our loop count multiplicator
        end
      else
        begin
        // Calculate current requests per second:
        LoopPerf:=(1000*LoopCnt*LoopMult)/LoopTime;
        // Update our performance array:
        if LoopMax<LOOP_MAX_AVG then
          begin
          Inc(LoopMax);
          LoopLoc:=LoopMax;
          LoopArr[LoopLoc]:=LoopPerf;
          end
        else
          begin
          LoopLoc:=LoopLoc+1;
          if LoopLoc>LOOP_MAX_AVG then LoopLoc:=1;
          LoopArr[LoopLoc]:=LoopPerf;
          end;
        // Calculate average requests per second over the last 10 loops:
        LoopAvg:=0;
        for a:=1 to LoopMax do
          LoopAvg:=LoopAvg+LoopArr[a];
        LoopAvg:=LoopAvg/LoopMax;
        // Log ...
        Log(RtcString( IntToStr((LoopDiff-LoopStart) div 1000 )+ // Time (sec)
              '; '+Format('%.0n', [FCliOpen*1.0] )+ // Opened Connections
              '; '+Format('%.0n', [FCliClose*1.0] )+ // Closed Connections
              '; '+Format('%.0n', [Sender.TotalClientConnectionCount*1.0] )+ // Current Connections
              '; '+Format('%.0n', [(FloodCnt-FloodIgn)*1.0] )+ // Results
              '; '+Format('%.1n', [(1000*(FloodCnt-FloodIgn))/(LoopDiff-LoopStart)] )+ // Average req/sec
              '; '+Format('%.1n', [LoopAvg] )+ // Current Average req/sec (last 20 logs)
              '; '+Format('%.1n', [LoopPerf] )), 'PERF'); // Current req/sec
        LoopTime:=LoopDiff; // set LoopTime to current time
        LoopMult:=1; // reset multiplicator
        end;
      end;
    LoopCnt:=0; // reset loop count (wait for at least 1000 more requests)
    end;
  end;

procedure TForm1.MultiResultReturn(Sender: TRtcConnection; Data,Result: TRtcValue);
  begin
  if Result.isNull then
    begin
    Log('No Result from Server!');
    end
  else if Result.isType=rtc_Exception then
    begin
    Log('Exception from Server!'#13#10+RtcString(Result.asException));
    end
  else with TRtcDataClient(Sender) do
    begin
    if Info.asInteger['CNT']>0 then
      begin
      FCS.Acquire;
      try
        Inc(FloodCounter);
        if (Info.asInteger['LEFT']-Info.asInteger['CNT']>=Info.asInteger['UPD']) then
          begin
          LoopCnt:=LoopCnt+Info.asInteger['UPD'];
          FloodCnt:=FloodCnt+Info.asInteger['UPD'];
          UpdateLogs(Sender);

          Info.asInteger['LEFT']:=Info.asInteger['LEFT']-Info.asInteger['UPD'];
          end;
      finally
        FCS.Release;
        end;

      Info.asInteger['CNT']:=Info.asInteger['CNT']-1;
      if TRtcDataClient(Sender).isConnecting then // stop looping if closing
        with TRtcClientModule(Info.asPtr['MOD']) do
          begin
        { WARNING!
          I am calling a remote function from the result event ONLY to avoid filling
          a large number of identical requests into memory, since this is a flood-test. }

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
                asCardinal['A']:=random(1000000);
                asCardinal['B']:=random(1000000);
                end;
              end;
            end
          else
            PrepareBigFunction(Data.NewFunction('loopo'));
          { I am using TRUE as a 2nd parameter (FromInsideEvent)
            for the remote Call to work from inside the event: }
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
      eConCnt.Text:=IntToStr(StrToInt(eConCnt.Text)-1);

      FCS.Acquire;
      try
        LoopCnt:=LoopCnt+Info.asInteger['UPD'];
        FloodCnt:=FloodCnt+Info.asInteger['UPD'];
        UpdateLogs(Sender);
      finally
        FCS.Release;
        end;

      if xAutoDisconnect.isChecked or not xReqAutoRepeat.isChecked then
        begin
        Info.asBoolean['closed']:=True;
        Disconnect;
        end;

      // Are all connections finished?
      if eConCnt.Text='0' then
        StopFloodTimer.Enabled:=True;
      end;
    end;
  end;

procedure TForm1.xProxyClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.UseProxy:=xProxy.isChecked;
  finally
    xProxy.isChecked:=RtcClient.UseProxy;
    end;
  end;

procedure TForm1.xSSLClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.UseSSL:=xSSL.isChecked;
  finally
    xSSL.isChecked:=RtcClient.UseSSL;
    end;
  end;

procedure TForm1.xMultiThreadedClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.MultiThreaded:=xMultiThreaded.isChecked;
  finally
    xMultiThreaded.isChecked:=RtcClient.MultiThreaded;
    end;
  end;

procedure TForm1.xAutoConnectClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.AutoConnect:=xAutoConnect.isChecked;
  finally
    xAutoConnect.isChecked:=RtcClient.AutoConnect;
    end;
  end;

procedure TForm1.btnMultiCreateClick(Sender: TObject);
  var
    cnt,req,a:integer;
  begin
  SimpleCall:=not xExtensiveTest.isChecked;

  FloodCnt:=0;
  FCliOpen:=0;
  FloodCounter:=0;

  FCliClose:=0;
  lblClients.Text:='0';
  lblFlood.Text:='0';

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
  btnMultiSkip.Enabled:=True;
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
      ServerAddr:=RtcString(eServer.Text);
      ServerPort:=RtcString(ePort.Text);
      MultiThreaded:=xReqMultiThread.isChecked;
      AutoConnect:=xReqAutoConnect.isChecked;
      CryptPlugin:=RtcClient.CryptPlugin;

      UseProxy:=xProxy.isChecked;
      UseSSL:=xSSL.isChecked;
      UseWinHTTP:=xWinHTTP.isChecked;
      Blocking:=xBlocking.isChecked;

      ReconnectOn.ConnectError:=True;
      ReconnectOn.ConnectLost:=True;
      ReconnectOn.ConnectFail:=True;
      OnConnecting:=MultiClientConnect;
      OnDisconnecting:=MultiClientDisconnect;
      Timeout.AfterConnecting:=RtcClient.Timeout.AfterConnecting;
      Timeout.AfterConnect:=RtcClient.Timeout.AfterConnect;
      TimeoutsOfAPI.ReceiveTimeout:=RtcClient.TimeoutsOfAPI.ReceiveTimeout;
      TimeoutsOfAPI.SendTimeout:=RtcClient.TimeoutsOfAPI.SendTimeout;
      Info.asInteger['ID']:=a;
      Info.asInteger['CNT']:=0;
      Info.asInteger['MAX']:=req;
      end;
    with CliMod[a] do
      begin
      AutoRepost:=-1; // unlimited
      AutoSessions:=True;
      if xEncrypt2.isChecked then
        EncryptionKey:=16;
      if xCompress.isChecked then
        Compression:=cFast;
      ModuleFileName:=RtcClientModule1.ModuleFileName;
      ModuleHost:=RtcClientModule1.ModuleHost;
      SecureKey:=RtcClientModule1.SecureKey;
      DataFormat:=RtcClientModule1.DataFormat;
      OnEncryptWrongKey:=RtcCliModEncryptWrongKey;
      Client:=CliCon[a];
      end;
    end;
  for a:=0 to Length(CliMod)-1 do
    begin
    if not CliCon[a].AutoConnect then
      CliCon[a].Connect;
    end;
  end;

procedure TForm1.btnMultiSendClick(Sender: TObject);
  var
    a:integer;
  begin
  SimpleCall:=not xExtensiveTest.isChecked;
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
            asCardinal['A']:=random(1000000);
            asCardinal['B']:=random(1000000);
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
  begin
  with TRtcDataClient(Sender) do
    if not inMainThread then
      Sync(SendResultReturn,Data,Result)
    else
      begin
      Info.asInteger['CNT']:=Info.asInteger['CNT']+1;
      if Info.asInteger['CNT']>=Info.asInteger['MAX'] then
        Info.asInteger['CNT']:=1;
      FloodCnt:=FloodCnt+1;
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
    CliCon[a].Disconnect;

  for a:=0 to length(CliCon)-1 do
    begin
    CliMod[a].Client:=nil;
    FreeAndNil(CliMod[a]);
    end;

  for a:=0 to length(CliCon)-1 do
    CliCon[a].Release;

  eConCnt.Enabled:=True;
  eConCnt.Text:=IntToStr(length(CliCon));

  SetLength(CliCon,0);
  SetLength(CliMod,0);

  lblClients.Text:='0';
  btnMultiFlood.Enabled:=True;

  btnMultiCreate.Enabled:=True;
  btnMultiConnect.Enabled:=False;
  btnMultiSend.Enabled:=False;
  btnMultiDisconnect.Enabled:=False;
  btnMultiSkip.Enabled:=False;
  btnMultiFree.Enabled:=False;
  end;

procedure TForm1.MultiClientConnect(Sender: TRtcConnection);
  begin
  FCS.Acquire;
  try
    Inc(FCliOpen);
  finally
    FCS.Release;
    end;
  end;

procedure TForm1.MultiClientDisconnect(Sender: TRtcConnection);
  begin
  FCS.Acquire;
  try
    Inc(FCliClose);
  finally
    FCS.Release;
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

procedure TForm1.btnMultiSkipClick(Sender: TObject);
  var
    a:integer;
  begin
  for a:=0 to length(CliCon)-1 do
    CliCon[a].SkipRequests;
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
  if xEncrypt.isChecked then
    RtcClientModule1.EncryptionKey:=16
  else
    begin
    RtcClientModule1.EncryptionKey:=0;
    RtcClientModule1.AutoSessions:=False;
    end;
  end;

procedure TForm1.xCompressClick(Sender: TObject);
  begin
  if xCompress.isChecked then
    RtcClientModule1.Compression:=cFast
  else
    RtcClientModule1.Compression:=cNone;
  end;

procedure TForm1.xCryptPluginClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    if xCryptPlugin.isChecked then
      RtcClient.CryptPlugin:=RtcCryptPlugin1
    else
      RtcClient.CryptPlugin:=nil;
  finally
    xCryptPlugin.isChecked:=assigned(RtcClient.CryptPlugin);
    end;
  end;

procedure TForm1.UpdateTimerTimer(Sender: TObject);
  begin
  FCS.Acquire;
  try
    lblClients.Text:=IntToStr(RtcClient.TotalClientConnectionCount)+
                     ' ('+IntToStr(FCliOpen)+' - '+IntToStr(FCliClose)+')';
    lblFlood.Text:=Format('%.0n',[FloodCounter*1.0]);
  finally
    FCS.Release;
    end;
  lblClients.Repaint;
  lblFlood.Repaint;
  end;

procedure TForm1.xUseXMLClick(Sender: TObject);
  begin
  if xUseXML.isChecked then
    RtcClientModule1.DataFormat:=fmt_XMLRPC
  else
    RtcClientModule1.DataFormat:=fmt_RTC;
  end;

procedure TForm1.xWinHTTPClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.UseWinHTTP:=xWinHTTP.isChecked;
  finally
    xWinHTTP.isChecked:=RtcClient.UseWinHTTP;
    end;
  end;

procedure TForm1.xBlockingClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.Blocking:=xBlocking.isChecked;
  finally
    xBlocking.isChecked:=RtcClient.Blocking;
    end;
  end;

procedure TForm1.xRTCTimeoutsClick(Sender: TObject);
  begin
  if xRTCTimeouts.isChecked then
    begin
    // we want timeouts to be used only in extreme cases ...
    RtcClient.Timeout.AfterConnecting:=300; // max 5 minutes to connect
    RtcClient.Timeout.AfterConnect:=300; // max 5 minutes idle time between each packet when connected
    end
  else
    begin
    RtcClient.Timeout.AfterConnecting:=0; // no timeout
    RtcClient.Timeout.AfterConnect:=0; // no timeout
    end;
  end;

procedure TForm1.xAPITimeoutsClick(Sender: TObject);
  begin
  if xAPITimeouts.isChecked then
    begin
    // we want timeouts to be used only in extreme cases ...
    RtcClient.TimeoutsOfAPI.ReceiveTimeout:=120; // max 2 minutes to receive data
    RtcClient.TimeoutsOfAPI.SendTimeout:=120; // max 2 minutes to send data
    end
  else
    begin
    RtcClient.TimeoutsOfAPI.ReceiveTimeout:=0; // no timeout
    RtcClient.TimeoutsOfAPI.SendTimeout:=0; // no timeout
    end;
  end;

procedure TForm1.xHTTP10Click(Sender: TObject);
  begin
  RtcClient.FixupRequest.ForceOldHttp10:=xHTTP10.isChecked;
  end;

procedure TForm1.RtcClientModule1EncryptWrongKey(Sender: TRtcConnection);
  begin
  ShowMessage('Wrong Encryption key!');
  end;

procedure TForm1.RtcCliModEncryptWrongKey(Sender: TRtcConnection);
  begin
  Log('Encryption Error ('+Sender.PeerAddr+':'+Sender.PeerPort+')','ENCRYPT');
  end;

procedure TForm1.StopFloodTimerTimer(Sender: TObject);
  begin
  StopFloodTimer.Enabled:=False;

  btnMultiFlood.Enabled:=True;
  btnMultiCreate.Enabled:=True;

  eConCnt.Enabled:=True;
  eConCnt.Text:=IntToStr(length(CliCon));

  // Do we have to repeat the whole thing once again?
  if xReqAutoRepeat.isChecked then
    btnMultiFloodClick(nil)
  else
    StopFloodTest;
  end;

procedure TForm1.FormDestroy(Sender: TObject);
  begin
{$IFDEF StreamSecII}
  RtcClient.CryptPlugin:=nil;
  ReleaseCryptPlugins;
{$ENDIF}
  FCS.Free;
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  StopFloodTest;
  CanClose:=True;
  end;

procedure TForm1.StopFloodTest;
  var
    a:integer;
  begin
  StopFloodTimer.Enabled:=False;
  xReqAutoRepeat.isChecked:=False;
  xAutoConnect.isChecked:=False;

  RtcClient.AutoConnect:=False;
  RtcClient.Disconnect;

  if length(CliCon)>0 then
    for a:=0 to length(CliCon)-1 do
      begin
      CliCon[a].AutoConnect:=False;
      CliCon[a].Disconnect;
      end;

  // Wait for all connections to close
  while RtcClient.TotalClientConnectionCount>0 do
    begin
    Application.ProcessMessages;
    Sleep(10);
    end;
    
  if length(CliCon)>0 then
    begin
    // Destroy all objects used in the test ...
    for a:=0 to length(CliCon)-1 do
      begin
      CliMod[a].Client:=nil;
      FreeAndNil(CliMod[a]);

      CliCon[a].Info.asPtr['MOD']:=nil;
      FreeAndNil(CliCon[a]);
      end;
    SetLength(CliCon,0);
    SetLength(CliMod,0);
    end;
  end;

type
  TForcedMemLeakTest=class(TObject);

procedure ForceMemLeak;
  begin
  // We will create one object of this kind to test memory leak detection.
  TForcedMemLeakTest.Create;
  // "TMemLeakTest" is the only object that should remain in memory when app terminates.
  end;

initialization
{$IFDEF RtcDeploy}
ForceMemLeak;
{$ENDIF}
finalization
Log('*****************************************','DEBUG');
Log('AppClient_Unit finalized.','DEBUG');
end.
