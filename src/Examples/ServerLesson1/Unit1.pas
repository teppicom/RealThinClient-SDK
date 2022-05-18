{ Quick-Start: RTC Server Lesson 1

Here how you can create a new project, just like this one ...

1. Open a new Project
2. from the RTC Server tab, put RtcHttpServer on your form:
3. for RtcHttpServer1, set ServerPort = 80
4. for your Form, define OnCreate event:

  RtcHttpServer1.Listen;

5. from the RTC Server tab, put RtcDataProvider on your form:
6. for RtcDataProvider1, set Server = RtcHttpServer1
7. for RtcDataProvider1, define OnCheckRequest event:
  with Sender as TRtcDataServer do
    if UpperCase(Request.FileName)='/TIME' then
      Accept;

8. for RtcDataProvider1, define OnDataReceived event:
  with Sender as TRtcDataServer do
    if Request.Complete then
      Write('Current time is: '+TimeToStr(Now));

9. Compile and Run the project.

When you run the project, your Server will immediately start to Listen on Port 80
(if you used the Port number from this lesson). In case your Server doesn't start
normaly and you end-up with "Error 10048 in function bind: Address already in use."
exception, it means that you have another Web Server running on Port 80 allready.
Since only one application can listen on a specific port, you either have to
change the ServerPort in this example (maybe use Port 81) and compile the project
again, or stop the other WebServer before running this example, so you can use
Port 80.

10. Using a web browser, go to http://localhost/time

NOTE: If you have changed the Port number to something different from 80
(standard HTTP port), you will also have to use this port number in your browser.
For example, if you used Port 81, your URL should look like this:
http://localhost:81/time

You should get a response like "Current time is: 23:15:38".

-----

For a visual lesson, please go to:
http://www.realthinclient.com
}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, rtcInfo, rtcConn, rtcDataSrv, rtcHttpSrv, rtcSystem;

type
  TForm1 = class(TForm)
    RtcHttpServer1: TRtcHttpServer;
    RtcDataProvider1: TRtcDataProvider;
    procedure FormCreate(Sender: TObject);
    procedure RtcDataProvider1CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider1DataReceived(Sender: TRtcConnection);
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

procedure TForm1.RtcDataProvider1CheckRequest(Sender: TRtcConnection);
  begin
  with Sender as TRtcDataServer do
    // We want to process anything that starts with "TIME", so we are using FilePath.Equal()
    // to compare the element at FilePath[0] with "TIME" while ignoring character casing.
    if Request.FilePath.Equal(0,'TIME') then
      Accept;
  end;

procedure TForm1.RtcDataProvider1DataReceived(Sender: TRtcConnection);
  var
    a:integer;
  begin
  with Sender as TRtcDataServer do
    if Request.Complete then
      begin
      Write('<html><body>');
      Write('Request from '+Sender.PeerAddr+':'+Sender.PeerPort+'<br>');
      Write('Server time: '+TimeToStr(Now)+'<br>');

      Write('<br>URL = '+Request.URL+'<br>');

      Write('<br>URI = '+Request.URI+'<br>');

      Write('<br>FileName = '+Request.FileName+'<br>');

      Write('<br>FilePath.Count = '+IntToStr(Request.FilePath.Count)+'<br>');
      for a:=0 to Request.FilePath.Count-1 do
        Write('FilePath['+IntToStr(a)+'] = "'+Request.FilePath[a]+'"<br>');

      Write('<br>Query.ItemCount = '+IntToStr(Request.Query.ItemCount)+'<br>');
      for a:=0 to Request.Query.ItemCount-1 do
        Write('Query Item '+IntToStr(a)+': '+
              'Name = "'+Request.Query.ItemName[a]+'"; '+
              'Value = "'+Request.Query.ItemValue[a]+'"<br>');

      Write('</body></html>');
      end;
  end;

end.
