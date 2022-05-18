{ Quick-Start: RTC Server Lesson 2

Here is how you can create a new project, just like this one ...

For small files or files where you feel can fit safely in your server's memory
(for example, up to 32K) or to prepare web site output, there's no need to split
the transfer. You can write the file out directly from the OnDataReceived event
of your RtcDataProvider component. You can also use the Write method consecutively.

Let's add a new RtcDataProvider component to our server app, which will produce
a site with a list of all square values from 1 to 100.

1. Open the Project created in Server Lesson 1.
2. from the "RTC Server" tab, put a new RtcDataProvider on the form:
3. for RtcDataProvider2, set Server = RtcHttpServer1
4. for RtcDataProvider2, define the OnCheckRequest event:
  // we want to access this request with "/square"
  with TRtcDataServer(Sender) do
    if UpperCase(Request.FileName)='/SQUARE' then Accept;

5. for RtcDataProvider2, define the OnDataReceived event:
  var
    line:integer;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      Write('<html><body>');
      Write('Here comes a table of square values ... <br>');
      for line:=1 to 100 do
        begin
        // I will use 3 write calls here,
        // you can use one (makes no difference).
        Write('Square of '+IntToStr(line)+' = ');
        Write(IntToStr(line*line));
        Write('<br>');
        end;
      Write('......... done.');
      Write('</body></html>');
      end;
  end;

6. Compile and Run the project.
7. Start your Browser and go to http://localhost/square

If everything went ok, you should see a list of square values.
You can also see that the other data provider is till working by going to
http://localhost/time

The same way, you can send binary data out.
The only thing you need to do is read the data into a string from the file.
I will show you how to do this, in the next lesson.

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
    RtcDataProvider2: TRtcDataProvider;
    procedure FormCreate(Sender: TObject);
    procedure RtcDataProvider1CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider1DataReceived(Sender: TRtcConnection);
    procedure RtcDataProvider2CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider2DataReceived(Sender: TRtcConnection);
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
    if UpperCase(Request.FileName)='/TIME' then
      Accept;
  end;

procedure TForm1.RtcDataProvider1DataReceived(Sender: TRtcConnection);
  begin
  with Sender as TRtcDataServer do
    if Request.Complete then
      Write('Current time is: '+TimeToStr(Now));
  end;

procedure TForm1.RtcDataProvider2CheckRequest(Sender: TRtcConnection);
  begin
  // we want to access this request with "/square"
  with TRtcDataServer(Sender) do
    if UpperCase(Request.FileName)='/SQUARE' then Accept;
  end;

procedure TForm1.RtcDataProvider2DataReceived(Sender: TRtcConnection);
  var
    line:integer;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      Write('<html><body>');
      Write('Here comes a table of square values ... <br>');
      for line:=1 to 100 do
        begin
        // I will use 3 write calls here,
        // you can use one (makes no difference).
        Write('Square of '+IntToStr(line)+' = ');
        Write(IntToStr(line*line));
        Write('<br>');
        end;
      Write('......... done.');
      Write('</body></html>');
      end;
  end;

end.
