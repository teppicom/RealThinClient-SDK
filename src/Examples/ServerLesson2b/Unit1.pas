{ Quick-Start: RTC Server Lesson 2b

Here is how you can create a new project, just like this one ...

This lesson is an extension of Lesson 2.
You will learn in this lesson how to use Request Query parameters,
which give your data providers the possibility to use simple parameters
as part of the request. Most dynamic sites use Query parameters (like Google)
to generate dynamic content.

1. Open the Project created in Server Lesson 2.
2. for RtcDataProvider2, change the OnDataReceived event to:
  var
    cnt,line:integer;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      Write('<html><body>');
      Write('Here comes a table of square values ... <br>');
      cnt:=0;
      if Request.Query['cnt']<>'' then
        try
          cnt:=StrToInt(Request.Query['cnt']);
        except
          end;
      if (cnt<1)or (cnt>1000) then
        begin
        cnt:=10;
        Write('Wrong "cnt" parameter.');
        Write('Using default value of 10.<br>');
        end;
      for line:=1 to cnt do
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

3. Compile and Run the project.
4. Start your Browser and go to http://localhost/square?cnt=27

If everything went ok, you should see a
list of square values from 1 to 27.

You can also try any other value for cnt, like ...
http://localhost/square?cnt=5392

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
    cnt,line:integer;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      Write('<html><body>');
      Write('Here comes a table of square values ... <br>');
      cnt:=0;
      if Request.Query['cnt']<>'' then
        try
          cnt:=StrToInt(Request.Query['cnt']);
        except
          end;
      if (cnt<1)or (cnt>1000) then
        begin
        cnt:=10;
        Write('Wrong "cnt" parameter.');
        Write('Using default value of 10.<br>');
        end;
      for line:=1 to cnt do
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
