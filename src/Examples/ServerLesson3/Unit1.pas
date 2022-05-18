{ Quick-Start: RTC Server Lesson 3

Here is how you can create a new project, just like this one ...

Let's say we want to write a simple Web Server, which will provider access
to small files in a specific folder.

1. Open the Project created in Lesson 1 or Lesson 2 (either one will do).
2. from "RTC Server" tab, put a new RtcDataProvider on your form:
3. for RtcDataProvider3, set Server = RtcHttpServer1
4. for RtcDataProvider3, set CheckOrder = 900

Note: We want to send a file out ONLY if other data providers didn't accept
      the request. To achieve this, we have defined our place in the check
      order using the CheckOrder property. Our assumption here is that
      all other RtcDataProvider components have a lower CheckOrder.

5. We need a function to convert our Request.FileName
   to a local file name. All files are in a sub-folder
   "/data", placed in the same folder as our server app.

   Copy this function into your main form's unit:

  function GetFullFileName(fname:string):string;
    var
      DocRoot:string;
    begin
    // Get the executable file's folder name ...
    DocRoot:=ExtractFilePath(AppFileName);
    // Make sure the file doesn't end with '\' ...
    if Copy(DocRoot,length(DocRoot),1)='\' then
      Delete(DocRoot,length(DocRoot),1);
    // We want to use a sub-folder named '\data' ...
    DocRoot:=DocRoot+'\data';
    // Requests use "/" instead of "\" ...
    fname:=StringReplace(fname,'/','\',[rfreplaceall]);
    // Create file name containing full path
    Result:=ExpandFileName(DocRoot+fname);
    // Check if the file is inside our folder
    // (don't want people to use "..\" to
    //  move out of this folder)
    if UpperCase(Copy(Result,1,length(DocRoot)))<>
       UpperCase(DocRoot) then
      Result:=''; // return empty file name.
    end;

6. for RtcDataProvider3, define the OnCheckRequest event:

  var
    fname:string;
  begin
  with TRtcDataServer(Sender) do
    begin
    // This time, we will accept any Request for which we can find
    //  a file in our folder, specified in "Request.FileName"
    fname:=GetFullFileName(Request.FileName);
    if (fname<>'') and (File_Exists(fname)) then
      begin
      Accept;
      // We will store the file name in our request,
      //  so we don't have to recreate it again later.
      Request.Info['fname']:=fname;
      end;
    end;
  end;

7. We assume that the files will be short
   and we will be sending them out in one run.
   In that case, we can simply send the whole
   file out from our OnDataReceived event.

   For RtcDataProvider3, define the OnDataReceived event:

  var
    fname:string;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      fname:=Request.Info['fname'];
      if File_Exists(fname) then
        Write(Read_File(fname))
      else
        // 'Write' will ensure that
        //  response is sent, even if empty.
        Write;
      end;
  end;

8. Compile and Run the Project.

This lesson got a bit complicated, because I wanted to use a folder relative
to our exe file and there has to be some file handling so clients don't get
access to our whole drive. To test how the server works, use Windows Explorer
to create a sub-folder named 'data' inside the folder where your project's
executable file is created, then put a file named 'test.txt' there. Once you
have the folder with a file and the server is running, open your Internet
Browser and download the file, using this link:
http://localhost/test.txt

If everything went ok, you should get the file in your Browser window.
If you place other files in the 'data' folder, you can download them by
using their name. It makes no difference whether the file is a text file,
image or an executable file.

I assumed that the file is small enough to be read in one call and sent
out directly, without splitting the thing in multiple events.
With larger files, there is a small modification we will have to do.
I will come to this in the next lesson.

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
    RtcDataProvider3: TRtcDataProvider;
    procedure FormCreate(Sender: TObject);
    procedure RtcDataProvider1CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider1DataReceived(Sender: TRtcConnection);
    procedure RtcDataProvider2CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider2DataReceived(Sender: TRtcConnection);
    procedure RtcDataProvider3CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider3DataReceived(Sender: TRtcConnection);
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

function GetFullFileName(fname:string):string;
  var
    DocRoot:string;
  begin
  // Get the executable file's folder name ...
  DocRoot:=ExtractFilePath(AppFileName);
  // Make sure the file doesn't end with '\' ...
  if Copy(DocRoot,length(DocRoot),1)='\' then
    Delete(DocRoot,length(DocRoot),1);
  // We want to use a sub-folder named '\data' ...
  DocRoot:=DocRoot+'\data';
  // Requests use "/" instead of "\" ...
  fname:=StringReplace(fname,'/','\',[rfreplaceall]);
  // Create file name containing full path
  Result:=ExpandFileName(DocRoot+fname);
  // Check if the file is inside our folder
  // (don't want people to use "..\" to
  //  move out of this folder)
  if UpperCase(Copy(Result,1,length(DocRoot)))<>
     UpperCase(DocRoot) then
    Result:=''; // return empty file name.
  end;

procedure TForm1.RtcDataProvider3CheckRequest(Sender: TRtcConnection);
{ This time, we will accept any Request for which we can find
    a file in our folder, specified in "Request.FileName"  }
  var
    fname:string;
  begin
  with TRtcDataServer(Sender) do
    begin
    fname:=GetFullFileName(Request.FileName);
    if (fname<>'') and (File_Exists(fname)) then
      begin
      Accept;
      { We will store the file name in our request,
        so we don't have to recreate it again later. }
      Request.Info['fname']:=fname;
      end;
    end;
  end;

procedure TForm1.RtcDataProvider3DataReceived(Sender: TRtcConnection);
  { We assume that the files will be short
    and we will be sending them out in one run.
    In that case, we can simply send the whole
    file out from our OnDataReceived event. }
  var
    fname:string;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      fname:=Request.Info['fname'];
      if File_Exists(fname) then
        Write(Read_File(fname))
      else
        { 'Write' will ensure that
          response is sent, even if empty. }
        Write;
      end;
  end;

end.
