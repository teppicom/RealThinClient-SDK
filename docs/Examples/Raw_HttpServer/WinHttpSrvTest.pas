unit WinHttpSrvTest;
{ This is an example of using the Microsoft Http.sys Server API with the
  "TRtcQuickJob" component to write a balanced Multi-Threaded HTTP Server.

  As an alternative to using the "TRtcQuickJob" component, as shown in
  this short Example Project, you could also use any other Thread Pool,
  or create your own Threads and implement a loop in the "Execute" method
  to wait for and process Requests, prepare and send Responses, then Exit
  the loop and close the Thread when "WHttp_NextRequest" returns FALSE, but ...
  using the "TRtcQuickJob" component, which utilizes the RTC Thread Pool to
  dinamically create "virtual" threads and post completely independent "jobs"
  makes things A LOT easier, because you do NOT have to worry about Threads.

  For details about "Http.sys" Server API functions in the RTC SDK,
  check the "rtcWinHttpSys.inc" file and the "rtcWinHttpSys.pas" unit.

  NOTE: By default, only Windows Administrators have the right to use
  the Http.sys Server API to start the Server for a specified URL,
  but Administrators can use the "WHttp_SetUrlPermissions" function
  on the "tRtcWinHttpApiServer" class to set permissions for an URL
  and allow other users to start the Server on that URL as well.

  Before you can start the Server as a normal User, you will need to compile this
  Project, then start it by right-clicking the EXE and using "Run as Administrator",
  then use the "Set Permissions" button to add URLs to the "allowed for all" list. }
interface

uses
  Windows, Messages, SysUtils,
  Variants, Classes, Graphics,
  Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  rtcTypes,
  rtcSystem,
  rtcWinHttpSys,

  rtcInfo,
  rtcConn,
  rtcCrypt,
  rtcThrPool,
  rtcThrJobs;


const
  { Security Descriptor to Allow General Access to ALL USERS.
    See description of the "WHttp_SetUrlACL" function for details. }
  SD_AllowAll = 'D:(A;;GA;;;S-1-1-0)';

  { MAX_WORKING, MIN_WAITING and MAX_WAITING parameters are used to
    balance the Server Load by starting a new Request Handler,
    IF ...
      A) New Request was juast received, while
         less than MAX_WORKING handlers are processing Requests, and
         less than MAX_WAITING handlers are waiting for new Requests,
    OR ...
      B) We have just finished processing a Request (Response sent), and now
         less than MIN_WAITING handlers are left waiting for new Requests. }

  MAX_WORKING:Integer = 5; // Should be > 0

  MIN_WAITING:Integer = 5; // Should be > 0

  MAX_WAITING:Integer = 10; // Should be >= MIN_WAITING

type
  TForm1 = class(TForm)
    btnSartServer: TButton;
    btnStopServer: TButton;
    Memo1: TMemo;
    RtcQuickJob1: TRtcQuickJob;
    btnAddRequestHandler: TButton;
    Timer1: TTimer;
    Label1: TLabel;
    btnAddURL: TButton;
    btnRemovePermissions: TButton;
    eURL: TEdit;
    btnSetPermissions: TButton;
    btnClearPermissions: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    eFolder: TEdit;
    procedure btnSartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure RtcQuickJob1Execute(Data: TRtcValue);
    procedure btnAddRequestHandlerClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnAddURLClick(Sender: TObject);
    procedure btnRemovePermissionsClick(Sender: TObject);
    procedure btnSetPermissionsClick(Sender: TObject);
    procedure btnClearPermissionsClick(Sender: TObject);
    procedure eFolderChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    { All you need to initialize and configure the Server
      is a variable holding a "THandle" for the Request Queue. }
    Queue:THandle;

    { Two variables are used in this example, to keep track of
      "Working" and "Waiting" Jobs, so we know when to start a
      new Job, can monitor how "Busy" our Server is and can
      wait for all Jobs to finish before closing the Server. }
    CliWaiting,
    CliWorking:Integer;

    WWW_ROOT_FOLDER:RtcWideString;

    procedure StopServer;
    function AddReqHandler(Queue:THandle; MaxWaiting:Integer):boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  CliWaiting:=0;
  CliWorking:=0;
  Queue:=0;
  eFolderChange(eFolder);
  end;

procedure TForm1.btnSartServerClick(Sender: TObject);
  begin
  // Open a Request Queue ...
  if Queue=0 then
    Memo1.Lines.Add('OPEN Queue  => #'+
      Int2Str(WHttp_OpenQueue(Queue)))
  else
    Memo1.Lines.Add('Queue was already Open.');
  end;

procedure TForm1.btnAddURLClick(Sender: TObject);
  begin
  // Add URL to the Request Queue ...
  Memo1.Lines.Add('Add URL '+eURL.Text+'  => #'+
    Int2Str(WHttp_AddUrl(Queue,eURL.Text)));
  // Add a new Request Handler, if less than "MAX_WAITING" are already waiting ...
  btnAddRequestHandler.Click;
  end;

procedure TForm1.btnRemovePermissionsClick(Sender: TObject);
  begin
  // Remove URL from the Request Queue ...
  Memo1.Lines.Add('Remove URL '+eURL.Text+'  => #'+
    Int2Str(WHttp_RemoveUrl(Queue,eURL.Text)));
  end;

procedure TForm1.btnSetPermissionsClick(Sender: TObject);
  begin
  // Set Permission to allows ALL USERS to Start the Server for this URL ...
  Memo1.Lines.Add('Set URL Access Level '+eURL.Text+'  => #'+
    Int2Str(WHttp_SetUrlACL(eURL.Text,SD_AllowAll)));
  end;

procedure TForm1.btnClearPermissionsClick(Sender: TObject);
  begin
  // Removes all custom-set Permissions for this URL ...
  Memo1.Lines.Add('Clear URL Permissions '+eURL.Text+'  => #'+
    Int2Str(WHttp_ClearUrlACL(eURL.Text)));
  end;

procedure TForm1.btnStopServerClick(Sender: TObject);
  begin
  if Queue<>0 then
    begin
    Memo1.Lines.Add('CLOSING Request Queue ...');
    StopServer;
    Memo1.Lines.Add('Request Queue closed.');

    eFolderChange(eFolder);
    end;
  end;

procedure TForm1.StopServer;
  function  Done:boolean;
    begin
    { We are using "CliWaiting" and "CliWorking" variables
      to keep track of Working and Waiting Request Handlers.
      After a Request Queue is closed, all Request Handlers
      started for that "Request Queue" will exit, so we
      just need to wait until CliWaiting and CliWorking
      are both set to 0 to know that all Jobs are finished. }
    Result:=(CliWaiting or CliWorking)=0;
    end;
  begin
  // Close the Request Queue ...
  WHttp_CloseQueue(Queue);
  // Wait for all "Request Handlers" to finish ...
  repeat Sleep(0) until Done;
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  StopServer;
  end;

procedure TForm1.Timer1Timer(Sender: TObject);
  begin
  Label1.Caption:='Handlers: Idle='+Int2Str(CliWaiting)+
                          ', Busy='+Int2Str(CliWorking);

  Label2.Caption:='Threads: Idle='+Int2Str(RtcTotalThreadsIdle)+
                         ', Busy='+Int2Str(RtcTotalThreadsBusy)+
                 '; Jobs: Queued='+Int2Str(RtcTotalJobsQueued);
  end;

function TForm1.AddReqHandler(Queue:THandle; MaxWaiting:Integer):boolean;
  begin
  { This function starts a new request handler if
    there are less than "MAX" handlers waiting. }
  if Queue=0 then
    Result:=False
  { Simply post the Request Queue Handle to the RtcQuickJob component.
    This will trigger the "OnExecute" event in a background thread,
    calling the "RtcQuickJob1Execute" method (see code below) using
    the "Data.asLargeInt" parameter as the "Request Queue" handle ... }
  else if InterlockedIncrement(CliWaiting)<=MaxWaiting then
    begin
    RtcQuickJob1.Post(TRtcLargeIntValue.Create(Queue));
    Result:=True;
    end
  else if InterlockedDecrement(CliWaiting)<MIN_WAITING then
    begin
    InterlockedIncrement(CliWaiting);
    RtcQuickJob1.Post(TRtcLargeIntValue.Create(Queue));
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TForm1.btnAddRequestHandlerClick(Sender: TObject);
  begin
  if AddReqHandler(Queue,MIN_WAITING) then
    Memo1.Lines.Add('Request Handler added');
  end;

procedure TForm1.eFolderChange(Sender: TObject);
  begin
  // Only update the WWW folder if the Request Queue is closed!
  if Queue=0 then
    WWW_ROOT_FOLDER:=eFolder.Text;
  end;

procedure TForm1.RtcQuickJob1Execute(Data: TRtcValue);
  var
    { Request Queue Handle, local to this event!
      We are using the same name for this local variable as for
      the global "Queue" variable to avoid mixing them up in here. }
    Queue:THandle;
    { Request Headers will be populated by the "WHttp_NextRequest" function
      we are using in this example, it the function returns >0 as a Result. }
    Request:WHTTP_Request;

  function GetContentType(const Ext:RtcWideString):RtcString;
    begin
    if Same_Text('htm',Ext) or Same_Text('html',Ext) then
      Result:='Content-Type: text/html'
    else
      Result:='';
    end;

  function SendFile(const FName:RtcWideString):boolean;
    var
      fs:int64;
    begin
    { To keep this example simple, we are ignoring the "Bytes" HTTP Request header
      (if it was present) and always sending the complete File back to the Client. }
    Result:=File_Exists(FName);
    if Result then
      begin
      fs:=File_Size(FName);
      if WHttp_SendHeader(
            Request,
            // Set Content-Length header (required) ...
            'Content-Length:'+Int2Str(fs)+#13#10+
            // Set Content-Type for known file extensions (optional) ...
            GetContentType(ExtractFileExt(FName)),
            // Status Code = 200, Status Text = OK
            200,'OK',
            // Set ResponseFinished parameter (are we done?)
            (fs=0) or (Request.Method='HEAD')
            )>0 then // we have a positive result, the header was sent
        if (fs>0) and (Request.Method<>'HEAD') then
          // Send the File contents as the Response Content Body  ...
          WHttp_SendFile(Request,
                         FName, // pass the File Name here!
                         True); // "ResponseFinished=True"!
      end;
    end;

  function SendFromFolder(const WebFolder:RtcWideString):boolean;
    var
      FName:RtcWideString;
    begin
    Result:=False;
    // We only send files for 'GET' and 'HEAD' requests ...
    if (Request.Method<>'GET') and (Request.Method<>'HEAD') then Exit;

    FName:=ExpandUNCFileName(WebFolder +
                             StringReplace(URL_Decode(Request.URI),'/','\',[rfReplaceAll])
                             );
    if Same_Text(Copy(FName,1,length(WebFolder)),WebFolder) then
      if Copy(FName,length(FName),1)<>'\' then
        Result:=SendFile(FName)
      else if SendFile(FName+'index.html') then
        Result:=True
      else if SendFile(FName+'index.htm') then
        Result:=True;
    end;

  procedure SendFromMemory;
    var
      { Buffer used for reading the Request Content Body }
      Buff:RtcByteArray;
      { Used to prepare the Response Header Text }
      Response:TRtcServerResponse;
      { Used as a Buffer when preparing the Response Content Body }
      outData:TRtcHugeByteArray;
    begin
    { Here, we are preparing a dynamic Response using
      Request Headers (which we have already received) and the
      Request Content Body (received using the code below). }

    { We can use a "tRtcHugeByteArray" as a Buffer,
      to prepare the Response Content Body in Memory ... }
    outData:=TRtcHugeByteArray.Create;

    { We could be manually creating a Response Header Text, but
      using the "TRtcServerResponse" class makes it easier ... }
    Response:=TRtcServerResponse.Create;
    try
    { Let's check what we have received in Request Headers ... }

    { To keep this example simple, we will simply collect all the information
      we have received in the Request and use it to prepare a response containing
      that data, so we can use a Web Browser to see what our Server has received. }

      outData.Add('<html><body>');

      outData.Add('Time = '+TimeToStr(Now)+'<br>');
      outData.Add('Date = '+DateToStr(Now)+'<br><br>');

      outData.Add('Queue HDL = '+Int2Str(Queue)+'<br>');
      outData.Add('Request ID = '+Int2Str(Request.RequestID)+'<br>');
      outData.Add('Connection ID = '+Int2Str(Request.ConnectionID)+'<br>');
      outData.Add('Raw Connection ID = '+Int2Str(Request.RawConnectionID)+'<br><br>');

      outData.Add('Remote Addr = '+Request.RemoteAddr+'<br>');
      outData.Add('Local Addr = '+Request.LocalAddr+'<br><br>');

      outData.Add('Header Size = '+IntToStr(Request.HeaderSize)+'<br>');
      outData.Add('Flags = '+Int2Str(Request.Flags)+'<br><br>');

      outData.Add('Method = '+Request.Method+'<br>');
      outData.Add('URI = '+Request.URI+'<br>');
      outData.Add('Version = '+Int2Str(Request.Ver.MajorVersion)+'.'+Int2Str(Request.Ver.MinorVersion)+'<br><br>');

      outData.Add(StringReplace(Request.HeaderText,#13#10,'<br>',[rfReplaceAll]));

      outData.Add('<br><br>');

      {The following 3 functions ...

        "WHttp_ReadContent" // receive request content body
        "WHttp_SendHeader"  // send response header
        "WHttp_SendContent" // send response content body

        ... are also available in versions which do NOT need a "Request:WHttp_Reuest" as a parameter,
        but instead require a "ReqQueueHandle:THandle" and "RequestID:HTTP_REQUEST_ID" to work,
        both of which are 64-bit integer values, which makes them easy to send and received,
        but ... since we have a "Request:WHttp_Request" in this event and will be processing
        the entire Request and sending out the Response from this event, we use the shorter,
        overloaded versions of these functions, which accept a "Request" record as parmeter. }

      { Read the entire Request Content Body (if present) ... }
      while WHttp_ReadContent(Request,Buff)>0 do // Data received
        begin
        { We have the next chunk of the request content body in "Buff".
          We don't want to send the same data back, but ... since this
          is just an Example and we do want to see on the Client how
          the Server was receiving the Data, we will add a line for every
          chunk of data received, containing the size of the chunk and a
          MD5-Hash encoded with '0'-'9', 'a'-'z', 'A'-'Z', '@' and '$'. }
        outData.Add('Received: '+Int2Str(length(Buff))+' Bytes,');
        outData.Add('Hash = '+MD5Code6Bit(RtcBytesToString(Buff))+'<br>');
        end;

      { Add HTTML closing tags to our Response Content Body (still in the buffer) ... }
      outData.Add('</body></html>');

      { IMPORTANT! Need to set the "Content-Length" HTTP Header for the Response
        to the number of bytes we plan to send out in the Response Content Body.

        Since our Content Body is now ready and waiting in "outData" object,
        which has a "Size" method to return the size of data stored there,
        we can set our Content Length to that value ... }
      Response.ContentLength:=outData.Size;

      { This sets Response.StatusCode and StatusText in a single call ... }
      Response.Status(200,'OK');

      { Setting "Content-Type" is required if the content body
        will have to be displayed in a "Web Browser" ... }
      Response.ContentType:='text/html';

      { Http.sys automatically sets the "Server" header to "Microsoft HTTPAPI/2.0",
        or appends that after anything we set in the "Server" Response Header. }
      Response['Server']:='RTC Test Server'; // just an example

      { A dummy Header value, used to test custom Headers ... }
      Response['Duffy']:='Duck'; // another example

      { Now, we need to call "WHttp_SendHeader" to send the Response Header ... }
      if WHttp_SendHeader(Request,
                          Response.HeaderText,
                          Response.StatusCode,
                          Response.StatusText
                          { We have Content Body waiting, so we have to call
                            this function with "ResponseFinished=FALSE" (default)!
                            If we did NOT have a Content Body,
                            we would need to call this function with
                            TRUE as the next parameter ("ResponseFinished=TRUE").

                            The last parameter is "ForceDisconnect",
                            which would force-close this connection
                            after sending the response Header,
                            but that is NOT want that now. }
                          )>0 then // Header sent (no error) ...
        begin
        { To keep it simple, we will be sending the entire
          Response Content Body in a single API call ... }
        WHttp_SendContent(Request,
                          outData.GetEx, // <- our Response Content Body
                          True // "ResponseFinished=TRUE"!
                          { Because we have set the "Content-Length" header,
                            we do NOT have to Force-Close the connection,
                            so we will call this function with the default
                            "ForceClose" parameter value (=FALSE) }
                          );
        end;

    finally
      { Free Objects we've used while processing this Request
        and preparing a Response, we no longer need them ... }
      Response.Free;
      outData.Free;
      end;

    end;

  begin

(***** This event implements our Request and Response Handler, as follows:
   1. Wait for a new Request. If waiting failed (=FALSE), jump to 7 (skip points 2 - 6).
   2. Increase "Working" and decrease "Waiting" Request Handler count.
   3. If the number of "Waiting" Request Handlers is below Maximum, start a new one.
   4. Process the Request received, prepare a Response and send it back to the Client.
   5. If the number of "Waiting" Request Handlers is below Minimum, start a new one.
   6. Reduce Working Request Handler count and Exit (skip point 7)
   7. Reduce Waiting Request Handler count and Exit.
******)

  { The "AddReqHandler" method sends the Request Queue as a "TRtcLargeInt",
    so we can access it here to get the Request Queue ... }
  Queue:=Data.asLargeInt;

  { Wait for the Next Request. This is a blocking function.
    If it returns a positive value, it means that a "Request" was received.
    If it returns a negative value, it means that there was an Error. }
  if WHttp_NextRequest(Queue,Request)>0 then // We have received Request data ...
    begin
    { We will start a new Request Handler if less than MAX_WORKING
      Threads are currently busy processing Requests and less than
      MAX_WAITING jobs are currently waiting for new Requests ... }
    try
      if InterlockedIncrement(CliWorking)>MAX_WORKING then
        InterlockedDecrement(CliWaiting) // too many threads busy, do NOT start a new one!
      else if InterlockedDecrement(CliWaiting)<MAX_WAITING then
        AddReqHandler(Queue,MAX_WAITING); // we are below our top limit, let's start a new job

    (************ Processing the Request *************)

      if not SendFromFolder(WWW_ROOT_FOLDER) then
        SendFromMemory;

    (************** Finished with this Request *************)

      { Start a new Request Handler if we are below MIN_WAITING ... }
      AddReqHandler(Queue,MIN_WAITING);

    finally
      { Make sure we reduce the "Working" counter, even if
        there was an exception while processing this request ... }
      InterlockedDecrement(CliWorking);
      end;
    end
  else // "WHttp_NextRequest" returned FALSE!
    begin
    { There was an Error waiting for the next Request,
      which either means that the Request Queue has been Closed,
      or that there is a Problem with the Server accepting Requests.

      In either case, there is no point in re-trying, so we
      will simply reduce the "Waiting" counter and exit ... }
    InterlockedDecrement(CliWaiting);
    end;
  end;

end.
