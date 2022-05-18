{ Quick-Start: RTC Server Lesson 4

To be able to send large files out, without using up all our server's
memory to hold the file, we can split the sending procedure in several
event calls. There is a small difference between the 2 methods:

A) Sending all out at once

When you want to send all out at once, which is the easiest and the best
way to do if you are preparing web sites or other dynamically generated
content, you will only need the implement the OnCheckRequest and
OnDataReceived events and use the Write method from OnDataReceived event
to write the whole content out. This is what we did in the last few examples.

B) Sending a large response in several turns

To be able to send large content out (like large files), additionally to the
events we used on out RtcDataProvider components, we will need the
OnDataSent event. This event will be called after all data prepared with
prior calls to Write have been sent out to the client and all sending buffers
are empty. Other than that, we will need some insight into our
TRtcDataServer's Response property.

Here is how you can create a new project, just like this one ...

Let's go and change the example from Lesson 3 a bit, so we can also send
large files with it. We will define a memory limit of 16K for a single
event call, to keep our memory usage as low as 16K for any client
requesting a file from our server.

1. We will change the OnCheckRequest event for our last RtcDataProvider
   component, to set the Response.ContentLength value to the file size.
   To do this, double-click on the last RtcDataProvider component to jump
   to the OnCheckRequest event provider. There is only a small addition to
   the event, but to keep it all in one place,

   Here is the complete new event implementation:

  var
    fname:string;
  begin
  with TRtcDataServer(Sender) do
    begin
    fname:=GetFullFileName(Request.FileName);
    if (fname<>'') and (File_Exists(fname)) then
      begin
      Accept;
      // We will store the file name in our request,
      // so we don't have to recreate it again later.
      Request.Info['fname']:=fname;
      // We need to set the Response.ContentLength,
      // to tell the RtcDataServer how large the content
      // (data) in our response will be.
      // If we do not set the Response.ContentLength,
      // RtcDataServer will assume that the first event which
      // calls Write has prepared the complete response and
      // will calculate the ContentLength for us.
      Response.ContentLength:=File_Size(fname);
      // We will send the Response Header out,
      // so we don't have to call Write
      //  in case our File size is zero.
      WriteHeader;
      end;
    end;
  end;

2. Then, we will update the OnDataReceived event of our RtcDataProvider,
   to send only limited amount of data out (let's set that limit to 16KB).

   And here is the updated OnDataReceived event handler:

  var
    fname:string;
    len:cardinal;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      // Check if we have to send more data.
      if Response.ContentLength>Response.ContentOut then
        begin
        fname:=Request.Info['fname'];
        // Only continue if the file hasn't changed in size.
        if File_Size(fname)=Response.ContentLength then
          begin
          // calculate how much we still need to send.
          len:=Response.ContentLength - Response.ContentOut;
          // Limit the length to send at once to 16KB
          if len>16000 then len:=16000;
          // Send 'len' bytes from our file,
          // starting at position "Response.ContentOut"
          Write( Read_File(fname, Response.ContentOut, len) );
          end
        else
          // Disconnect the client, because our file
          // has changed and we have sent
          // the wrong header and file beginning out.
          Disconnect;
        end;
      end;
  end;

3. We will use the same event handler method we defined for OnDataReceived
   for our OnDataSent event. To do this, from the Object Inspector's Window,
   we can simply copy the event name for the OnDataReceived event to the
   OnDataSent event. Warning: do not remove the OnDataReceived event.
   Both events (OnDataReceived and OnDataSent) should use the same implementation.

That's all there is to it.
What will happen exactly?

First, the "OnCheckRequest" event will be called, which will check if a file
with requested name exists, then accept the request and set the
"Response.ContentLength" property (our file size) and write the response header
out (by calling "WriteHeader"). Then, "OnDataReceived" event will be called,
but we won't react to it until we receive the complete request body
(Request.Complete). After we get the complete request, we will send up to
16K of our file out. If the file is larger than 16K, "OnDataSent" event will
be called every time our data has been sent out and it is safe to continue
sending the next package. On each call to our "OnDataSent" event, we will
send up to 16K bytes of data out, until our
"Response.ContentOut=Response.ContentLength".

To test how this works, you can place a large file into the "/data" sub-folder,
open your favorite Internet browser and request the file. You will not see
the difference at the client side between the old and the new implementation,
but the new implementation will only use up to 16K of memory, while the old
one will put the whole file into memory before sending.

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
      { We need to set the Response.ContentLength, 
        to tell the RtcDataServer how large the content 
        (data) in our response will be. 
        If we do not set the Response.ContentLength,
        RtcDataServer will assume that the first event which 
        calls Write has prepared the complete response and 
        will calculate the ContentLength for us. } 
      Response.ContentLength:=File_Size(fname); 
      { We will send the Response Header out, 
        so we don't have to call Write
        in case our File size is zero. } 
      WriteHeader;
      end;
    end;
  end;

procedure TForm1.RtcDataProvider3DataReceived(Sender: TRtcConnection);
  var
    fname:string; 
    len:cardinal; 
  begin 
  with TRtcDataServer(Sender) do 
    if Request.Complete then 
      begin
      // Check if we have to send more data. 
      if Response.ContentLength>Response.ContentOut then
        begin 
        fname:=Request.Info['fname']; 
        // Only continue if the file hasn't changed in size.         
        if File_Size(fname)=Response.ContentLength then
          begin 
          // calculate how much we still need to send. 
          len:=Response.ContentLength - Response.ContentOut; 
          // Limit the length to send at once to 16KB 
          if len>16000 then len:=16000; 
          // Send 'len' bytes from our file,
          // starting at position "Response.ContentOut"
          Write( Read_File(fname, Response.ContentOut, len) ); 
          end 
        else 
          { Disconnect the client, because our file 
            has changed and we have sent 
            the wrong header and file beginning out. }
          Disconnect;  
        end; 
      end; 
  end;

end.
