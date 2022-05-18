unit MainGateForm;

interface

{$include rtcDeploy.inc}

{ This example demonstrates how to set up and use a TRtcHttpGateway component. }

{ To compile the project with StreamSec Tools 2.1+ components, declare
  the StreamSecII compiler directive below or in the "rtcDeploy.inc" file. }

{.$DEFINE StreamSecII}

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  rtcTypes,
  rtcInfo,
  rtcConn,

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

  rtcLog,
  rtcDataSrv,
  rtcHttpSrv,
  rtcGateSrv,
  rtcGateConst,

  rtcThrPool,
  rtcSystem,

  ExtCtrls, Spin;

type
  TGateForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    shGateway4: TShape;
    ePort: TEdit;
    btnStart: TButton;
    lblStatus: TLabel;
    lblConnect: TLabel;
    StatusTimer: TTimer;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label10: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    eInPack: TSpinEdit;
    eInBuff: TSpinEdit;
    eInSpeed: TSpinEdit;
    Label2: TLabel;
    Label13: TLabel;
    eInRead: TSpinEdit;
    Label14: TLabel;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    eOutPack: TSpinEdit;
    eOutBuff: TSpinEdit;
    eOutSpeed: TSpinEdit;
    Label18: TLabel;
    Label19: TLabel;
    eOutWrite: TSpinEdit;
    Label20: TLabel;
    xMultiThreaded: TCheckBox;
    xIPv6: TCheckBox;
    xBlockingAPI: TCheckBox;
    lblThreads: TLabel;
    xIPv4: TCheckBox;
    MyGate: TRtcGateway;
    MyLink: TRtcDualDataServerLink;
    Server6: TRtcHttpServer;
    Server4: TRtcHttpServer;
    shGateway6: TShape;
    Label5: TLabel;
    eThreads: TSpinEdit;
    procedure btnStartClick(Sender: TObject);
    procedure MyGateListenStart(Sender: TRtcConnection);
    procedure MyGateListenStop(Sender: TRtcConnection);
    procedure MyGateListenError(Sender: TRtcConnection; E: Exception);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
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
  if not (Server6.isListening or Server4.isListening) then
    begin
    RTC_THREAD_POOL_OVERSIZE:=eThreads.Value;
    RTC_THREAD_POOL_LIMIT:=eThreads.Value;
    RTC_THREAD_POOL_MAX:=eThreads.Value div 2;

    rtcSetupReceivingParams(eInPack.Value,eInBuff.Value,eInRead.Value);
    rtcSetupSendingParams(eOutPack.Value,eOutBuff.Value,eOutWrite.Value);

    MyGate.ReceiveSpeedLimit:=eInSpeed.Value;
    MyGate.StreamSpeedLimit:=eOutSpeed.Value;
    if xIPv4.Checked then
      begin
      Server4.MultiThreaded:=xMultiThreaded.Checked;
      Server4.ServerPort:=ePort.Text;
      Server4.Blocking:=xBlockingAPI.Checked;
      Server4.Listen();
      end;
    if xIPv6.Checked then
      begin
      Server6.MultiThreaded:=xMultiThreaded.Checked;
      Server6.ServerPort:=ePort.Text;
      Server6.Blocking:=xBlockingAPI.Checked;
      Server6.Listen();
      end;
    end
  else
    begin
    Server4.StopListenNow();
    Server6.StopListenNow();
    StatusTimerTimer(nil);
    btnStart.Caption:='START';
    end;
  end;

procedure TGateForm.MyGateListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(MyGateListenStart)
  else
    begin
    if Sender=Server6 then
      shGateway6.Brush.Color:=clGreen
    else
      shGateway4.Brush.Color:=clGreen;
    lblStatus.Caption:='Listening on Port '+Sender.LocalPort;
    btnStart.Caption:='STOP';
    end;
  end;

procedure TGateForm.MyGateListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(MyGateListenStop)
  else
    begin
    if Sender=Server6 then
      shGateway6.Brush.Color:=clRed
    else
      shGateway4.Brush.Color:=clRed;
    lblStatus.Caption:='Stopped listening';
    btnStart.Caption:='DONE';
    end;
  end;

procedure TGateForm.MyGateListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then
    Sender.Sync(MyGateListenError,E)
  else
    lblStatus.Caption:=E.Message;
  end;

procedure TGateForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  Server4.StopListenNow();
  Server6.StopListenNow();
  end;

procedure TGateForm.FormCreate(Sender: TObject);
  begin
  StartLog;
  StartLogBuffers(512000);

{$IFDEF StreamSecII}
  AddServerRootCertFile('root.cer');
  AddServerPFXFile('server.pfx','abc');

  Server4.CryptPlugin := GetServerCryptPlugin;
  Server6.CryptPlugin := GetServerCryptPlugin;

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
  DumpLogBuffers;
  lblConnect.Caption:=IntToStr(rtcTotalConnectionCount);
  lblThreads.Caption:=IntToStr(RtcTotalThreadsIdle)+' + '+IntToStr(RtcTotalThreadsBusy)+' + '+IntToStr(RtcTotalJobsQueued);
  end;

end.
