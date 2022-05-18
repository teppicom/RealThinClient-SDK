unit Server_Form;

interface

uses                          
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcLog, rtcSystem,
  rtcInfo, rtcConn,
  rtcDataSrv,

  HTTP_Module;

type
  TRtcFileServer = class(TForm)
    Panel4: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Label8: TLabel;
    eIndexPages: TMemo;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label6: TLabel;
    Label1: TLabel;
    Label7: TLabel;
    eVirtualHosts: TMemo;
    Splitter2: TSplitter;
    Panel7: TPanel;
    btnListen: TButton;
    btnStop: TButton;
    lblCliCon: TLabel;
    Label9: TLabel;
    eStreamExtensions: TEdit;
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure btnListenClick(Sender: TObject);
    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    CliCnt:integer;
  end;

var
  RtcFileServer: TRtcFileServer;

implementation

{$R *.dfm}

procedure TRtcFileServer.FormCreate(Sender: TObject);
  var
    s:string;
    a,i:integer;
    f:TextFile;
  begin
  StartLog;

  CliCnt:=0;

  // Read configuration file ...
  AssignFile(f,AppFileName+'.cfg');
  {$I-}
  Reset(f);
  {$I+}
  if IOResult=0 then
    begin
    try
      eIndexPages.Clear;
      ReadLn(f, s); s:=Trim(s); i:=StrToInt(s);
      for a:=0 to i-1 do
        begin
        ReadLn(f, s);
        eIndexPages.Lines.Add(s);
        end;

      eVirtualHosts.Clear;
      ReadLn(f, s); s:=Trim(s); i:=StrToInt(s);
      for a:=0 to i-1 do
        begin
        ReadLn(f, s);
        eVirtualHosts.Lines.Add(s);
        end;

      ReadLn(f, s); s:=Trim(s); eStreamExtensions.Text := s;
    except
      // ignore errors while reading the CFG file
      end;
    CloseFile(f);
    end;
  end;

procedure TRtcFileServer.btnListenClick(Sender: TObject);
  var
    f:TextFile;
    a:integer;
  begin
  // Write new configuration file ...
  AssignFile(f,AppFileName+'.cfg');
  {$I-}
  Rewrite(f);
  {$I+}
  if IOResult=0 then
    begin
    try
      WriteLn(f, IntToStr(eIndexPages.Lines.Count));
      for a:=0 to eIndexPages.Lines.Count-1 do
        WriteLn(f, eIndexPages.Lines[a]);

      WriteLn(f, IntToStr(eVirtualHosts.Lines.Count));
      for a:=0 to eVirtualHosts.Lines.Count-1 do
        WriteLn(f, eVirtualHosts.Lines[a]);

      WriteLn(f, eStreamExtensions.Text);
    finally
      CloseFile(f);
      end;
    end;

  Http_Server.OnStart:=ServerListenStart;
  Http_Server.OnStop:=ServerListenStop;
  Http_Server.OnError:=ServerListenError;
  Http_Server.OnConnect:=ServerConnecting;
  Http_Server.OnDisconnect:=ServerDisconnecting;
  Http_Server.Start;
  end;

procedure TRtcFileServer.btnStopClick(Sender: TObject);
  begin
  Http_Server.Stop;
  end;

procedure TRtcFileServer.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenError,E)
  else
    ShowMessage('Error starting WebServer!'#13#10 + E.ClassName+'>'+E.Message);
  end;

procedure TRtcFileServer.ServerListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStart)
  else
    begin
    lblCliCon.Caption:='Server started.';

    // Disable buttons and edit fields ...
    btnStop.Enabled:=True;
    if Visible then
      btnStop.SetFocus;

    btnListen.Enabled:=False;
    eStreamExtensions.Enabled:=False;
    eVirtualHosts.Enabled:=False;
    eIndexPages.Enabled:=False;
    end;
  end;

procedure TRtcFileServer.ServerListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStop)
  else
    begin
    lblCliCon.Caption:='Server not listening.';

    // Enable buttons and Edit fields ...
    eStreamExtensions.Enabled:=True;
    eVirtualHosts.Enabled:=True;
    eIndexPages.Enabled:=True;
    btnListen.Enabled:=True;
    btnListen.SetFocus;

    btnStop.Enabled:=False;
    end;
  end;

procedure TRtcFileServer.ServerConnecting(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerConnecting)
  else
    lblCliCon.Caption:=IntToStr(Sender.TotalServerConnectionCount)+' client(s).';
  end;

procedure TRtcFileServer.ServerDisconnecting(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerDisconnecting)
  else
    lblCliCon.Caption:=IntToStr(Sender.TotalServerConnectionCount-1)+' client(s).';
  end;

procedure TRtcFileServer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  HTTP_Server.Stop;
  end;

end.
