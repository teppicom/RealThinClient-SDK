unit ClientUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, rtcDataCli, rtcInfo, rtcConn, rtcHttpCli, StdCtrls, ExtCtrls,
  rtcSystem;

type
  TForm1 = class(TForm)
    eLocalFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    eRequestFileName: TEdit;
    Label4: TLabel;
    eServerPort: TEdit;
    Label5: TLabel;
    eServerAddr: TEdit;
    btnConnect: TButton;
    btnPutFile: TButton;
    btnOpen: TButton;
    OpenDialog1: TOpenDialog;
    RtcHttpClient1: TRtcHttpClient;
    RtcDataRequest1: TRtcDataRequest;
    pInfo: TPanel;
    procedure btnConnectClick(Sender: TObject);
    procedure RtcHttpClient1Connect(Sender: TRtcConnection);
    procedure RtcHttpClient1Disconnect(Sender: TRtcConnection);
    procedure btnPutFileClick(Sender: TObject);
    procedure RtcDataRequest1BeginRequest(Sender: TRtcConnection);
    procedure btnOpenClick(Sender: TObject);
    procedure RtcDataRequest1DataOut(Sender: TRtcConnection);
    procedure RtcDataRequest1DataSent(Sender: TRtcConnection);
    procedure RtcDataRequest1DataReceived(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnConnectClick(Sender: TObject);
  begin
  with RtcHttpClient1 do
    begin
    if not isConnected then
      begin
      ServerAddr:=eServerAddr.Text;
      ServerPort:=eServerPort.Text;
      Connect;
      end
    else
      Disconnect;
    end;
  end;

procedure TForm1.RtcHttpClient1Connect(Sender: TRtcConnection);
  begin
  btnConnect.Caption:='Disconnect';
  end;

procedure TForm1.RtcHttpClient1Disconnect(Sender: TRtcConnection);
  begin
  btnConnect.Caption:='Connect';
  btnPutFile.Caption:='Upload';
  end;

procedure TForm1.btnPutFileClick(Sender: TObject);
  begin
  btnPutFile.Caption:='Clicked ...';
  with RtcDataRequest1 do
    begin
    // File Name on Server (need to URL_encode all Query parameters)
    Request.Query['file'] := URL_Encode(Utf8Encode(eRequestFileName.Text));
    // Local File Name
    Request.Info.asText['file'] := eLocalFileName.Text;
    Post;
    end;
  end;

procedure TForm1.RtcDataRequest1BeginRequest(Sender: TRtcConnection);
  begin
  btnPutFile.Caption:='Sending ...';
  with TRtcDataClient(Sender) do
    begin
    Request.Method:='PUT';
    Request.FileName:='/UPLOAD';
    Request.Host:=ServerAddr;
    Request.ContentLength:=File_Size(Request.Info.asText['file']);
    WriteHeader;
    end;
  end;

procedure TForm1.btnOpenClick(Sender: TObject);
  begin
  if OpenDialog1.Execute then
    begin
    eLocalFileName.Text:=OpenDialog1.FileName;
    eRequestFileName.Text:=ExtractFileName(eLocalFileName.Text);
    end;
  end;

procedure TForm1.RtcDataRequest1DataOut(Sender: TRtcConnection);
  begin
  with Sender as TRtcDataClient do
    begin
    pInfo.Caption:='Sending: '+
       IntToStr(Request.ContentOut)+'/'+
       IntToStr(Request.ContentLength)+' ['+
       IntToStr(round(Request.ContentOut/Request.ContentLength*100))+'%]';
    end;
  end;

procedure TForm1.RtcDataRequest1DataSent(Sender: TRtcConnection);
  var
    bSize:int64;
  begin
  with TRtcDataClient(Sender) do
    begin
    if Request.ContentLength>Request.ContentOut then
      begin
      bSize:=Request.ContentLength-Request.ContentOut;
      if bSize>64000 then bSize:=64000;
      Write(Read_File(Request.Info.asText['file'], Request.ContentOut, bSize));
      end;
    end;
  end;

procedure TForm1.RtcDataRequest1DataReceived(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    begin
    { We do not expect a long response,
      so we can wait for the response to be Done before reading it. }
    if Response.Done then
      begin
      { We can use "Read" here to get the complete content sent from the Server
        if we expect the Server to send a short content. If the Server is expected
        to send a long response, you should use Read before Response.Done and
        write the read content to a file as it arrives to avoid flooding your memory. }
      btnPutFile.Caption:='Done, Status = '+
          IntToStr(Response.StatusCode)+' '+Response.StatusText;
      end;
    end;
  end;

end.
