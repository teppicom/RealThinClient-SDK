unit CliUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, rtcFunction, rtcDataCli, rtcCliModule, rtcInfo,
  rtcConn, rtcHttpCli,

  loFileCli, rtcSystem;

{$include rtcDefs.inc}

type
  TForm1 = class(TForm)
    RtcHttpClient1: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RtcHttpClient1Connect(Sender: TRtcConnection);
    procedure RtcHttpClient1Disconnect(Sender: TRtcConnection);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  RtcHttpClient1.Disconnect;
  end;

procedure TForm1.RtcHttpClient1Connect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpClient1Connect)
  else
    Label1.Caption:='Connected';
  end;

procedure TForm1.RtcHttpClient1Disconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpClient1Disconnect)
  else
    Label1.Caption:='NOT Connected';
  end;

procedure TForm1.Button1Click(Sender: TObject);
  begin
  { RtcClientModule1.ObjectLinks = ol_Manual, so we need to call "ActivateObjectManager" manually.
    Setting RtcClientModule1.ObjectLinks to ol_AutoClient or ol_AutoBoth would eliminate
    this requirement, but only one TRtcClientModule can have that setting. }
  RtcClientModule1.ActivateObjectManager(True);
  { Now that the Object Manager is active, we can create Linked Objects
    and they will be created automatically on the Server side if the TRtcServerModule
    on the Server has its ObjectLinks property set to ol_AutoClient or ol_AutoBoth. }
  TloFileClient.Create(nil).Show;
  end;

procedure TForm1.Button2Click(Sender: TObject);
  begin
  { We can also call a remote function on the Server and let the Server
    create the Linked Object(s), in which case our TRtcClientModule's ObjectLinks
    property needs to be set to one of the ol_Auto* settings (at least ol_AutoServer),
    for data from the Server to be accepted and executed locally on the Client. }
  RtcClientModule1.Prepare('run');
  RtcClientModule1.Call(nil); // in this case, we are not interested in the actual result
  end;

end.
