program MyHttpServer;

{$APPTYPE CONSOLE}

{
When you compile and run this Project on your local machine, you will see a Console open up,
with a message “Server started”. To test the Server, open any Web Browser and try one of these URLs  …

http://localhost/html
http://localhost/json
http://localhost/xml
http://localhost/code

If the Server does NOT start, you will see an error message like
“Server Error: Address already in use”, which means that
another Server is already running on port 80 (which was used in this example),
so you will either need to modify your code to use another Port
(see "MyServer:=TMyServer.Create('80');" line below), or stop the
other Server before you start this Project. Because all the links
above are accessing a Server running on the default HTTP port (80),
if your Server is running on any other Port, you also need to include
the Port number in each URL. For example, if the Server is running on
Port 8080 on your local machine, URLs would look like this …

http://localhost:8080/html
http://localhost:8080/json
http://localhost:8080/xml
http://localhost:8080/code

What is happening there?

Well … if you take a look at the “DataProv_CheckRequest” method, you will see
that it will accept “/html”, “/json”, “/xml” or “/code” as the request URI.

And if you then look at the “DataProv_DataReceived” method, you will see how
the Server handles each of these requests to prepare a response for the Client.

There are also methods to display a simple message on the Console if there is
an error starting the Server, or when the Server is started and stopped.
There is also a method which handles all requests not accepted or processed
by any Data Provider linked to youor Server, which will (in this case) respond
with an 404 page that says “Bad command.”

Ofcourse, the code above is not limited to Console Applications.

The exact same code as I’ve used here for the “TMyServer” class would work
without any modifications if compiled into a Windows or Android Service, a
Windows VCL Application, or cross-platform FireMonkey Application
for Windows, MacOSX, iOS or Android.

And … naturally … because RTC SDK is a set of non-visual components for Delphi,
all the components install into the Delphi component palette, so you can simply
drop them on any Form or DataModule in Delphi, set properties in the Delphi’s
Object Inspector and implement all the events you want directly,
without the need to implement your own classes.

Now, someone could ask:
  “What if I want to add more functionality? Do I add more code to
   DataProv_CheckRequest and DataProv_DataReceived events
   to handle everything I want my Server to do?”

While you could do that too, it is better to keep your functionality separated
by using a different TRtcDataProvider component for each set of requests you want
your Server to handle. To make your “data” provided by each TRtcDataProvider
component accessible through the same Server, you only have to set the “Server”
of every TRtcDataProvider component to the same TRtcHttpServer component
– just like I’ve done in the example above for the “DataProv” component.

Since RTC SDK also comes with components for writing Remote Functions,
Load Balancers and Gateways (as well as some other stuff), you can always
add more functionality to your Server, simply by adding more components from
the RTC SDK and linking them to the TRtcHttpServer.

RTC SDK has full support for IPv4 as well as IPv6, but you need to specify
which IP version you want your Server to use. By default, RTC will be using IPv4.

To have your Server running on IPv6, simply use rtc_IPv6 as the 2nd parameter
when creating an object using the TMyServer class.

On Windows, if you want to have a Server listening on the same Port using
IPv4 and IPv6, you will need two TRtcHttpServer components. One will have
“ServerIPV:=rtc_IPv4? and the other “rtc_IPv6?. For all your Data Providers
to be accessible from both HTTP Servers, you will use a TRtcDualDataServerLink
component, which has 2 Server properties, so it can be assigned both TRtcHttpServer
components, and then you will assign that TRtcDualDataServerLink component to the
“Link” property of each TRtcDataProvider component – instead of using the “Server” property.

I didn’t bother using a TRtcDataServerLink component in my example above to keep it simple
and give you an easy start, but keep in mind that you can also use TRtcDataServerLink and
TRtcDualDataServerLink components to completely separate your “Data Providers” and all your
Application logic from the actual Server component being used.

If you want to see a full-blown Console Web Server running on IPv4 and IPv6
using the TRtcDataServerLink to separate all functionality into modules,
take a look at the "RTConWebServer" Project in the "SDKDemos_CON" Project Group.

And if you need to host your Application as an ISAPI extension, for example …
if your customers already have IIS or Apache running and you want your Server
code to be accessible through their existing Web Server, by separating your code
into modules and using a TRtcDataServerLink, you can compile your Server code into
ISAPI DLLs by linking your Data Providers and Remote Functions to a TRtcISAPIServer
component and compile it into the ISAPI example Project, included in the RTC SDK “Lib” folder.

You can find ISAPI DLL examples in the "SDKDemos_CON" Project Group.

PS. If you are using Delphi 2009 or later and would like to see
    the same Project where all RTC events are implemented using
    anonymous methods instead of a class with methods, check the
    "RtcHttpServer2" Project, available in the same folder.

And now, take a closer look at the implementation below ... }

uses
  SysUtils,
  rtcTypes,
  rtcConn,
  rtcDataSrv,
  rtcHttpSrv,
  rtcInfo;

type
// This is our self-contained HTTP Server class ...
  TMyServer=class(TObject)
  protected
    HttpSrv:TRtcHttpServer;
    DataProv:TRtcDataProvider;

  public
    constructor Create(PortNumber:String='80';
                       IPVer:RtcIPV=rtc_IPVDefault);
    destructor Destroy; override;

    procedure DataProv_CheckRequest(Sender: TRtcConnection);
    procedure DataProv_DataReceived(Sender: TRtcConnection);

    procedure HttpSrv_ListenStart(Sender: TRtcConnection);
    procedure HttpSrv_ListenError(Sender: TRtcConnection; E:Exception);
    procedure HttpSrv_ListenStop(Sender: TRtcConnection);
    procedure HttpSrv_RequestNotAccepted(Sender: TRtcConnection);
    end;

constructor TMyServer.Create(PortNumber:String='80'; IPVer:RtcIPV=rtc_IPVDefault);
  begin
  // Create HttpServer and DataProvider components ...
  HttpSrv:=TRtcHttpServer.Create(nil);
  DataProv:=TRtcDataProvider.Create(nil);

  // Assign Server for our Data Provider ...
  DataProv.Server:=HttpSrv;

  // Assign Data Provider Events (handles Valid Requests) ...
  DataProv.OnCheckRequest:=DataProv_CheckRequest;
  DataProv.OnDataReceived:=DataProv_DataReceived;

  // Assign Server Events (handles the rest) ...
  HttpSrv.OnRequestNotAccepted:=HttpSrv_RequestNotAccepted;
  HttpSrv.OnListenStart:=HttpSrv_ListenStart;
  HttpSrv.OnListenStop:=HttpSrv_ListenStop;
  HttpSrv.OnListenError:=HttpSrv_ListenError;

  // Configure the Server ...
  HttpSrv.ServerPort:=PortNumber;
  HttpSrv.ServerIPV:=IPVer;
  HttpSrv.MultiThreaded:=True;

  // Start the Server listener ...
  HttpSrv.Listen();
  end;

destructor TMyServer.Destroy;
  begin
  // Stop the Server listener ...
  HttpSrv.StopListenNow();

  // Destroy the components ...
  HttpSrv.Free;
  DataProv.Free;
  end;

procedure TMyServer.DataProv_CheckRequest(Sender: TRtcConnection);
  begin
  // Check Request headers and "Accept" all Requests
  // we want to handle with our Data Provider ...
  with TRtcDataServer(Sender) do
    if (Request.Method='GET') and  // we only want "GET" requests
       (Request.ContentLength=0) then // ... with no content body
        if (Request.URI='/html') or
           (Request.URI='/json') or
           (Request.URI='/xml') or
           (Request.URI='/code') then
          Accept; // Accept the Request
  end;

procedure TMyServer.DataProv_DataReceived(Sender: TRtcConnection);
  var
    t:TRtcRecord;
  begin
  with TRtcDataServer(Sender) do
  // We will start processing the request only if
  // we have received the complee request content body ...
    if Request.Complete then
      if Request.URI='/html' then
        begin
        // We can use multiple "Write" calls
        // to prepare our HTML response ...
        Response.ContentType:='text/html';
        Write('<html><body>');
        Write('Your IP: '+PeerAddr+'<br>');
        Write('Your Port: '+PeerPort+'<br>');
        Write('Date & Time: <b>'+DateTimeToStr(Now)+'</b><br>');
        Write('Agent: <i>'+Request['User-Agent']+'</i><br>');
        Write('</body></html>');
        // All "Write" calls will be buffered,
        // RTC will calculate the "Content-Length" for us
        // and send the whole content body out as a single
        // Response - when we are finished with our event.
        end
      else
        begin
        // Using TRtcRecord to prepare our response Object ...
        t:=TRtcRecord.Create;
        try
          t.asText['agent']:=Request['User-Agent'];
          t.asText['ip']:=PeerAddr;
          t.asText['port']:=PeerPort;
          t.asDateTime['now']:=Now;
          if Request.URI='/json' then
            begin
            // Serialize to "JSON" ...
            Response.ContentType:='application/json';
            Write(t.toJSON);
            end
          else if Request.URI='/xml' then
            begin
            // Serialize to "XML-RPC" ...
            Response.ContentType:='text/xml';
            Write(t.toXMLrpc);
            end
          else if Request.URI='/code' then
            begin
            // Serialize to "Code" (RTC format) ...
            Response.ContentType:='text/plain';
            Write(t.toCode);
            end;
        finally
          t.Free;
          end;
        end
  end;

procedure TMyServer.HttpSrv_RequestNotAccepted(Sender: TRtcConnection);
  begin
  // Request wasn't accepted ...
  with TRtcDataServer(Sender) do
    begin
    // Send "404" status code back ...
    Response.Status(404,'Not Found');
    Response.ContentType:='text/plain';
    // Something to show in the Web Browser ...
    Write('Bad command.');
    // And ... Disconnect the Client.
    Disconnect;
    end;
  end;

procedure TMyServer.HttpSrv_ListenError(Sender: TRtcConnection; E: Exception);
  begin
  Writeln('Server Error: '+E.Message);
  end;

procedure TMyServer.HttpSrv_ListenStart(Sender: TRtcConnection);
  begin
  Writeln('Server started.');
  end;

procedure TMyServer.HttpSrv_ListenStop(Sender: TRtcConnection);
  begin
  Writeln('Server stopped.');
  end;

var
  MyServer:TMyServer;

begin
  try
    // Create and start our Server ...
    MyServer:=TMyServer.Create('80');
    try
      // Since this is a console application and our
      // Server is Multi-Threaded, we can do whatever
      // we want here. For simplicity reasons, we will
      // just use "ReadLn" to allow the Server to run
      // while we wait for the user to press <Enter>.

      Writeln('Press <Enter> to Quit ...');
      ReadLn;

      // User has pressed <Enter> - time to kill our Server.
    finally
      MyServer.Free;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
