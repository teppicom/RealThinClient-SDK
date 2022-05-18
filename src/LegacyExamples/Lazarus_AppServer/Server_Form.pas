unit Server_Form;

{$include rtcDefs.inc}

interface

uses
  LResources, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,

  rtcLog, rtcSystem,
  rtcInfo, rtcConn,
  rtcDataSrv, rtcHttpSrv,

  AppServerModule, ExtCtrls,

  rtcThrPool,
  rtcMemory;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    btnListen: TButton;
    Label2: TLabel;
    ePort: TEdit;
    Label3: TLabel;
    lblCliCnt: TLabel;
    xEncrypt: TCheckBox;
    xMultiThreaded: TCheckBox;
    xCompress: TCheckBox;
    Timer1: TTimer;
    Label6: TLabel;
    lblTotalMem: TLabel;
    eThreads: TEdit;
    Label5: TLabel;
    Label7: TLabel;
    xMonitorDataInOut: TCheckBox;
    lblDataInOut: TLabel;
    xBlocking: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnListenClick(Sender: TObject);
    procedure xBlockingClick(Sender: TObject);
    procedure xEncryptClick(Sender: TObject);
    procedure xMultiThreadedClick(Sender: TObject);
    procedure xCompressClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    RtcDataServer1: TRtcHttpServer;
    CS:TRtcCritSec;
    ResponseDoneCount:int64;
    LastTime,StartTime:longword;
    TotalDataIn,TotalDataOut:int64;

    procedure RtcDataServer1ClientConnect(Sender: TRtcConnection);
    procedure RtcDataServer1ClientDisconnect(Sender: TRtcConnection);
    procedure RtcDataServer1ResponseDone(Sender: TRtcConnection);
    procedure RtcDataServer1DataIn(Sender: TRtcConnection);
    procedure RtcDataServer1DataOut(Sender: TRtcConnection);
    procedure RtcDataServer1ListenStart(Sender: TRtcConnection);
    procedure RtcDataServer1ListenError(Sender: TRtcConnection; E: Exception);
    procedure RtcDataServer1ListenStop(Sender: TRtcConnection);
  end;

var
  Form1: TForm1;

implementation

procedure TForm1.FormCreate(Sender: TObject);
  begin
  RtcDataServer1:= TRtcHttpServer.Create(Self);
  RtcDataServer1.OnClientConnect := RtcDataServer1ClientConnect;
  RtcDataServer1.OnClientDisconnect := RtcDataServer1ClientDisconnect;
  RtcDataServer1.OnListenStart := RtcDataServer1ListenStart;
  RtcDataServer1.OnListenStop := RtcDataServer1ListenStop;
  RtcDataServer1.OnListenError := RtcDataServer1ListenError;
  RtcDataServer1.OnResponseDone := RtcDataServer1ResponseDone;
  RtcDataServer1.OnDataOut := RtcDataServer1DataOut;
  RtcDataServer1.OnDataIn := RtcDataServer1DataIn;
  RtcDataServer1.MultiThreaded := False;
  RtcDataServer1.ServerPort := '81';


  CS:=TRtcCritSec.Create;

  StartLog;

  {$ifdef rtcTest}
  LOG_THREAD_EXCEPTIONS:=True;
  LOG_EXCEPTIONS:=True;
  {$endif}

  { If we wanted to test how fast compression works,
    we could force data compression even when we know there will
    be no gain in compressing the data (only CPU usage increase). }
  //RTC_MIN_COMPRESS_SIZE:=0;

  AppSrv_Module.ServerLink.Server:=RtcDataServer1;

  // Using properties set up by form
  xCompressClick(xCompress);
  xEncryptClick(xEncrypt);
  xMultiThreadedClick(xMultiThreaded);

  ResponseDoneCount:=0;
  Timer1Timer(Timer1);
  end;

procedure TForm1.RtcDataServer1ListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ListenStart)
  else
    begin
    StartTime:=GetTickTime;
    LastTime:=StartTime;


    XLog('Time (sec); Memory (KB); Requests; Connections; Avg req/sec; Curr req/sec; Listener started!');
    XLog('0; '+Format('%.0n', [Get_AddressSpaceUsed*1.0]));
    btnListen.Caption:='Stop';
    Label1.Caption:='Listening on Port '+Sender.LocalPort;
    end;
  end;

procedure TForm1.RtcDataServer1ListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ListenError,E)
  else
    Label1.Caption:='Listening Error: '+E.Message;
  end;

procedure TForm1.RtcDataServer1ListenStop(Sender: TRtcConnection);
  var
    cnt:int64;
    m:string;
    tm:longword;
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ListenStop)
  else
    begin
    tm:=GetTickTime;

    cnt:=ResponseDoneCount;
    m:=Format('%.0n', [Get_AddressSpaceUsed*1.0]);
    lblTotalMem.Caption:=m+' KB';

    XLog(IntToStr((tm-StartTime) div 1000)+
        '; '+Format('%.0n', [Get_AddressSpaceUsed*1.0] )+
        '; '+Format('%.0n', [cnt*1.0] )+
        '; '+Format('%.0n', [Sender.TotalServerConnectionCount*1.0] )+
        '; '+Format('%.1n', [cnt/(tm-StartTime)*1000.0] )+
        '; '+Format('%.1n', [(cnt mod $2000)/(tm-LastTime)*1000.0] ) +
        '; Listener STOPPED!');

    ResponseDoneCount:=0;

    btnListen.Caption:='Listen';
    Label1.Caption:='Stopped listening.';
    end;
  end;

procedure TForm1.btnListenClick(Sender: TObject);
  begin
  if RtcDataServer1.isListening then
    RtcDataServer1.StopListen
  else
    begin
    TotalDataIn:=0;
    TotalDataOut:=0;

    // DO NOT CHANGE THIS WHEN CLIENT OR SERVER ARE RUNNING !!!
    RTC_THREAD_POOL_MAX:=StrToInt(eThreads.Text);

    lblDataInOut.Caption:='???';

    if xMonitorDataInOut.Checked then
      begin
      RtcDataServer1.OnDataIn:=RtcDataServer1DataIn;
      RtcDataServer1.OnDataOut:=RtcDataServer1DataOut;
      end
    else
      begin
      RtcDataServer1.OnDataIn:=nil;
      RtcDataServer1.OnDataOut:=nil;
      end;
    RtcDataServer1.ServerPort:=ePort.Text;
    RtcDataServer1.Listen;
    end;
end;

procedure TForm1.xBlockingClick(Sender: TObject);
begin
  RtcDataServer1.Blocking:=xBlocking.Checked;
end;

procedure TForm1.RtcDataServer1ClientConnect(Sender: TRtcConnection);
begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ClientConnect)
  else
    begin
    lblCliCnt.Caption:=IntToStr(RtcDataServer1.TotalServerConnectionCount);
    lblCliCnt.Refresh;
    end;

end;

procedure TForm1.RtcDataServer1ClientDisconnect(Sender: TRtcConnection);
begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ClientDisconnect)
  else
    begin
    lblCliCnt.Caption:=IntToStr(RtcDataServer1.TotalServerConnectionCount);
    lblCliCnt.Refresh;
    end;
end;

procedure TForm1.xEncryptClick(Sender: TObject);
  begin
  if xEncrypt.Checked then
    AppSrv_Module.ServerModule.EncryptionKey:=16
  else
    AppSrv_Module.ServerModule.EncryptionKey:=0;
  end;

procedure TForm1.xMultiThreadedClick(Sender: TObject);
  begin
  RtcDataServer1.MultiThreaded:=xMultiThreaded.Checked;
  end;

procedure TForm1.xCompressClick(Sender: TObject);
begin
if xCompress.Checked then
  AppSrv_Module.ServerModule.Compression:=cFast
else
  AppSrv_Module.ServerModule.Compression:=cNone;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
  var
    m:string;
  begin
  m:=Format('%.0n KB', [Get_AddressSpaceUsed * 1.0]);
  lblTotalMem.Caption:=m;
  end;

procedure TForm1.RtcDataServer1ResponseDone(Sender: TRtcConnection);
  var
    tm:longword;
    cnt:int64;
  begin
  CS.Enter;
    ResponseDoneCount:=ResponseDoneCount+1;
    cnt:=ResponseDoneCount;
  CS.Leave;
  if cnt and $1FFF=0 then
    begin
    tm:=GetTickTime;
    XLog(IntToStr((tm-StartTime) div 1000)+
        '; '+Format('%.0n', [Get_AddressSpaceUsed*1.0] )+
        '; '+Format('%.0n', [cnt*1.0] )+
        '; '+Format('%.0n', [Sender.TotalServerConnectionCount*1.0] )+
        '; '+Format('%.1n', [cnt/(tm-StartTime)*1000.0] )+
        '; '+Format('%.1n', [$2000/(tm-LastTime)*1000.0] ) );
    LastTime:=tm;
    end;
  end;

procedure TForm1.RtcDataServer1DataIn(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1DataIn)
  else
    begin
    TotalDataIn:=TotalDataIn+Sender.DataIn;
    lblDataInOut.Caption:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

procedure TForm1.RtcDataServer1DataOut(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1DataOut)
  else
    begin
    TotalDataOut:=TotalDataOut+Sender.DataOut;
    lblDataInOut.Caption:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

procedure TForm1.FormDestroy(Sender: TObject);
  begin
  CS.Destroy;
  end;

initialization
  {$I Server_Form.lrs}


end.
