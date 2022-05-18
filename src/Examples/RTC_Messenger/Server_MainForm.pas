unit Server_MainForm;

interface

uses
  Windows, Messages, SysUtils, 
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcTypes, rtcSystem, rtcInfo, rtcConn,
  rtcHttpSrv, rtcDataSrv,

  rtcMessengerProvider;

type
  TServerMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    Server: TRtcHttpServer;
    pInfo: TPanel;
    Label1: TLabel;
    ePort: TEdit;
    xIPv6: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServerMain: TServerMain;

implementation

{$R *.dfm}

procedure TServerMain.btnStartClick(Sender: TObject);
  begin
  GetMessengerProvider.ServerLink.Server:=Server;
  Server.ServerIPV:=GetRtcIPV(xIPv6.Checked);
  Server.ServerPort:=RtcString(ePort.Text);
  Server.Listen;
  end;

procedure TServerMain.btnStopClick(Sender: TObject);
  begin
  Server.StopListen;
  end;

procedure TServerMain.ServerListenStart(Sender: TRtcConnection);
  begin
  btnStart.Enabled:=False;
  btnStop.Enabled:=True;
  pInfo.Caption:='Server Ready.';
  end;

procedure TServerMain.ServerListenStop(Sender: TRtcConnection);
  begin
  btnStart.Enabled:=True;
  btnStop.Enabled:=False;
  pInfo.Caption:='Server not Listening.';
  end;

procedure TServerMain.ServerRequestNotAccepted(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      Response.Status(301,'Invalid request');
      Write;
      end;
  end;

end.
