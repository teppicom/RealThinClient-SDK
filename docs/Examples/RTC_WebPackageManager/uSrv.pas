unit uSrv;

{$INCLUDE defines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, rtcConn, StdCtrls, ExtCtrls;

type
  TfrmServer = class(TForm)
    Panel1: TPanel;
    lblCliCon: TLabel;
    btStop: TButton;
    btStart: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btStartClick(Sender: TObject);
    procedure AppException(Sender : TObject; E: Exception);
  private
    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerDisconnecting(Sender: TRtcConnection);
  public
    fNeedClose : boolean;
  end;

var
  frmServer: TfrmServer;

implementation

uses HTTP_Module, rtcLog
  {$IFDEF LOG_THREAD_EXCEPTIONS}, rtcThrPool{$ENDIF};

{$R *.dfm}

procedure TfrmServer.FormCreate(Sender: TObject);
begin
  fNeedClose := False;

  {$IFDEF LOG_EXCEPTIONS}
  LOG_EXCEPTIONS := True;
  {$ENDIF}

  {$IFDEF LOG_THREAD_EXCEPTIONS}
  LOG_THREAD_EXCEPTIONS := True;
  {$ENDIF}

  StartLog;

  Application.CreateForm(THTTP_Server, HTTP_Server);

  Http_Server.OnStart:=ServerListenStart;
  Http_Server.OnStop:=ServerListenStop;
  Http_Server.OnError:=ServerListenError;
  Http_Server.OnConnect:=ServerConnecting;
  Http_Server.OnDisconnect:=ServerDisconnecting;
  
  Http_Server.Start;
end;

procedure TfrmServer.ServerConnecting(Sender: TRtcConnection);
begin
  if not Sender.InMainThread then
    Sender.Sync(ServerConnecting)
  else
    lblCliCon.Caption:=IntToStr(Sender.TotalClientConnectionCount)+' client(s).';
end;

procedure TfrmServer.ServerDisconnecting(Sender: TRtcConnection);
begin
  if not Sender.inMainThread then
    Sender.Sync(ServerDisconnecting)
  else
    lblCliCon.Caption:=IntToStr(Sender.TotalClientConnectionCount)+' client(s).';
end;

procedure TfrmServer.ServerListenError(Sender: TRtcConnection;
  E: Exception);
begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenError,E)
  else
    ShowMessage('Error starting WebServer!'#13#10 + E.ClassName+'>'+E.Message);
end;

procedure TfrmServer.ServerListenStart(Sender: TRtcConnection);
begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStart)
  else
    lblCliCon.Caption:='Server started.';
  btStart.Enabled := False;
  btStop.Enabled := True;
end;

procedure TfrmServer.ServerListenStop(Sender: TRtcConnection);
begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStop)
  else
    begin
      lblCliCon.Caption:='Server not listening.';
      btStart.Enabled := True;
      btStop.Enabled := False;
      if fNeedClose then
        PostMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TfrmServer.btStopClick(Sender: TObject);
begin
  Http_Server.Stop;
end;

procedure TfrmServer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Http_Server.Server.isListening then begin
    fNeedClose := True;

    Http_Server.Stop;
    StopLog;

    CanClose := False;
  end;
end;

procedure TfrmServer.btStartClick(Sender: TObject);
begin
  Http_Server.Start;
end;

procedure TfrmServer.AppException(Sender: TObject; E: Exception);
begin
  XLog(Format('Exception [App]: class: %s; message: %s',[E.ClassName, E.Message])); 
end;

end.
