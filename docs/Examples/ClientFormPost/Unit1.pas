unit Unit1;

{ This example demonstrates sending of FORM POST data to a Web Server
  using the "x-www-form-url-encoded" content type. }

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  // RTC SDK units ...
  rtcDataCli, rtcInfo, rtcConn, rtcHttpCli;

type
  TForm1 = class(TForm)
    RtcHttpClient1: TRtcHttpClient;
    RtcDataRequest1: TRtcDataRequest;
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    procedure RtcDataRequest1BeginRequest(Sender: TRtcConnection);
    procedure RtcDataRequest1DataReceived(Sender: TRtcConnection);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
  begin
  RtcDataRequest1.Request.Method:='POST'; // Use the "HTTP POST" method
  RtcDataRequest1.Request.FileName:='/x/index.php'; // Set the "FileName" to receive the request
  RtcDataRequest1.Request.Host:=RtcHttpClient1.ServerAddr; // Set the "Host" HTTP header
  RtcDataRequest1.Post(); // Post the request to the request queue
  end;

// The "OnBeginRequest" event is called after "RtcDataRequest1.Post" ...
procedure TForm1.RtcDataRequest1BeginRequest(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
  begin
  Cli.Request.ContentType:='application/x-www-form-urlencoded'; // important!!!
  // Set all POST variables (in our case, "username" and "pwd")
  Cli.Request.Params.Value['username']:=URL_Encode('TestUser');
  Cli.Request.Params.Value['pwd']:=URL_Encode('XYZ123ABC');
  // Use the "Write" method to send "Params.Text" ("FORM-URLENCODED" values) to the Server
  Cli.Write(Cli.Request.Params.Text);
  end;

// The "OnDataReceived" event is called once the request was sent to the Server
// and we start receiving a response. Here, we will simply print the response out.
procedure TForm1.RtcDataRequest1DataReceived(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    mycontent:RtcString;
  begin
  // We want to wait for the whole response ...
  if Cli.Response.Done then
    begin
    // Since we have the whole response,
    // we can use a single "Read" call to get the complete content body
    mycontent:=Cli.Read;

    Memo1.Lines.Text:=IntToStr(Cli.Response.StatusCode)+' '+Cli.Response.StatusText+ #13#10
                     + mycontent;
    end;
  end;

end.
