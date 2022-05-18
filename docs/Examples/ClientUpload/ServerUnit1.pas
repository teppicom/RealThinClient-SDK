unit ServerUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  rtcTypes, rtcSystem,
  rtcDataSrv, rtcInfo,
  rtcConn, rtcHttpSrv;

type
  TForm2 = class(TForm)
    RtcHttpServer1: TRtcHttpServer;
    RtcDataProvider1: TRtcDataProvider;
    Label1: TLabel;
    eServerPort: TEdit;
    Label2: TLabel;
    eUploadFolder: TEdit;
    btnListen: TButton;
    procedure btnListenClick(Sender: TObject);
    procedure RtcHttpServer1ListenStart(Sender: TRtcConnection);
    procedure RtcHttpServer1ListenStop(Sender: TRtcConnection);
    procedure RtcDataProvider1CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider1DataReceived(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnListenClick(Sender: TObject);
  begin
  with RtcHttpServer1 do
    begin
    if not isListening then
      begin
      ServerPort:=RtcString(eServerPort.Text);
      Listen;
      end
    else
      StopListen;
    end;
  end;

procedure TForm2.RtcHttpServer1ListenStart(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if not inMainThread then
      Sync(RtcHttpServer1ListenStart)
    else
      btnListen.Caption:='Stop Listen';
  end;

procedure TForm2.RtcHttpServer1ListenStop(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if not inMainThread then
      Sync(RtcHttpServer1ListenStop)
    else
      btnListen.Caption:='Listen';
  end;

procedure TForm2.RtcDataProvider1CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if (Request.Method='PUT') and
       (UpperCase(Request.FileName)='/UPLOAD') and
       (Request.Query['file']<>'') then
      begin
      Request.Info.asText['file']:=eUploadFolder.Text + '\'+Utf8Decode(URL_Decode(Request.Query['file']));
      Accept;
      end;
  end;

procedure TForm2.RtcDataProvider1DataReceived(Sender: TRtcConnection);
  var
    s:RtcString;
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.Started then
      begin
      if not DirectoryExists(eUploadFolder.Text) then
        CreateDir(eUploadFolder.Text);
      Delete_File(Request.Info.asText['file']);
      end;
    s:=Read;
    Write_File(Request.Info.asText['file'], s, Request.ContentIn-length(s));

    if Request.Complete then
      begin
      { We have the complete request content here (the complete file).
        We can could process the file (now stored locally) and send a response
        to the Client letting the client know how our processing went. }
      Response.Status(200,'OK'); // Set response status to a standard "OK"
      Write('Thanks for the file.'); // Send a short message to let the client know how we did.
      end;
    end;
  end;

end.
