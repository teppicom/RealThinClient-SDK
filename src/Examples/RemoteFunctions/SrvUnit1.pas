unit SrvUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, rtcInfo, rtcConn, rtcDataSrv, rtcHttpSrv, rtcFunction,
  rtcSrvModule, StdCtrls, ExtCtrls, rtcSystem;

{$include rtcDefs.inc}

type
  TForm1 = class(TForm)
    RtcHttpServer1: TRtcHttpServer;
    RtcFunctionGroup1: TRtcFunctionGroup;
    RtcServerModule1: TRtcServerModule;
    RtcFunction1: TRtcFunction;
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    xfmtRTC: TCheckBox;
    xfmtXMLRPC: TCheckBox;
    xfmtJSONrpc1: TCheckBox;
    xfmtJSON: TCheckBox;
    xfmtJSONrpc2: TCheckBox;
    xreqContentBodyALL: TCheckBox;
    xreqContentBodyParams: TCheckBox;
    xreqDirectJSON: TCheckBox;
    xreqQueryJSON: TCheckBox;
    xreqQueryNameJSON: TCheckBox;
    xreqQueryNameText: TCheckBox;
    xreqUriParamsJSON: TCheckBox;
    xreqUriParamsText: TCheckBox;
    CheckBox1: TCheckBox;
    xAutoSessionCheck: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RtcFunction1Execute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure Button1Click(Sender: TObject);
    procedure RtcHttpServer1ListenStart(Sender: TRtcConnection);
    procedure RtcHttpServer1ListenStop(Sender: TRtcConnection);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure xfmtClick(Sender: TObject);
    procedure xreqClick(Sender: TObject);
    procedure xAutoSessionCheckClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  RtcHttpServer1.Listen;
  end;

procedure TForm1.RtcFunction1Execute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  // Map params received in the "params" array to named parameters ...
  Param.map(0,'name');

  // Here is a simple example for manual session handling ...
  with TRtcDataServer(Sender) do
    if not assigned(Session) then
      begin
      if Param.asText['name']='hi' then // user sends "hi" to start a new session
        begin
        OpenSession;
        Session.KeepAlive:=30; // allow max 30 seconds idle time between requests, or session dies
        Result.asText:='Hi! Your NEW ID = '+Session.ID;
        end
      else // no session, tell the user that
        Result.asText:=Param.asText['name']+' does NOT have a Session';
      end
    else if Param.asText['name']='bye' then // user sends "bye" to close a session
      begin
      Result.asText:='Bye, bye! Your ID was '+Session.ID;
      Session.Close;
      end
    else // tell the user his session ID
      begin
      Result.asText:=Param.asText['name']+', your OLD ID = '+Session.ID;
      end;
  Result.asText:=
    'File='+Sender.Request.FileName+#13#10+
    'Method='+Sender.Request.Method+#13#10+
    'Params='+Param.toJSON+#13#10+
    'Result='+Result.asText;
  end;

procedure TForm1.Button1Click(Sender: TObject);
  begin
  if RtcHttpServer1.isListening then
    RtcHttpServer1.StopListen
  else
    RtcHttpServer1.Listen;
  end;

procedure TForm1.RtcHttpServer1ListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpServer1ListenStart)
  else
    Button1.Caption:='Stop';
  end;

procedure TForm1.RtcHttpServer1ListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpServer1ListenStop)
  else
    Button1.Caption:='Listen';
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  // Call "StopListenNow" before closing your Server App
  RtcHttpServer1.StopListenNow();
  end;

procedure TForm1.xfmtClick(Sender: TObject);
  var
    fmt:TRtcDataFormat;
  begin
  fmt:=TRtcDataFormat(TComponent(Sender).Tag);
  if TCheckBox(Sender).Checked then
    RtcServerModule1.DataFormats:=RtcServerModule1.DataFormats+[fmt]
  else
    RtcServerModule1.DataFormats:=RtcServerModule1.DataFormats-[fmt];
  end;

procedure TForm1.xreqClick(Sender: TObject);
  var
    rmod:TRtcDataReqMode;
  begin
  rmod:=TRtcDataReqMode(TComponent(Sender).Tag);
  if TCheckBox(Sender).Checked then
    RtcServerModule1.DataReqModes:=RtcServerModule1.DataReqModes+[rmod]
  else
    RtcServerModule1.DataReqModes:=RtcServerModule1.DataReqModes-[rmod];
  end;

procedure TForm1.xAutoSessionCheckClick(Sender: TObject);
  begin
  RtcServerModule1.AutoSessionCheck:=xAutoSessionCheck.Checked;
  end;

end.
