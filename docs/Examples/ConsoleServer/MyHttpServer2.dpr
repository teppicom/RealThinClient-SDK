program MyHttpServer2;

{ You need Delphi 2010 or later to compile this Project!

  This Example uses anonymous methods to implement RTC events.

  Check the "MyHttpServer" project if you are looking for an
  example which is compatible with all Delphi versions and does
  the same as this Project, but uses a custom class with methods
  instead of anonymous methods to implement all RTC events. }

{$APPTYPE CONSOLE}

{$include rtcDefs.inc}

{$IFNDEF RTC_ANON_METHODS}
begin
  Writeln('This Project uses Anonymous methods and requires Delphi 2010 or later.');
  Readln;
{$ELSE}

uses
  SysUtils,
  rtcTypes,
  rtcConn,
  rtcDataSrv,
  rtcHttpSrv,
  rtcInfo;

var
  HttpSrv:TRtcHttpServer;
  DataProv:TRtcDataProvider;

begin
  try
    // Create HttpServer and DataProvider components ...
    HttpSrv:=TRtcHttpServer.Create(nil);
    DataProv:=TRtcDataProvider.Create(nil);
    try
      // Assign Server to Data Provider ...
      DataProv.Server:=HttpSrv;

      // Assign Data Provider Events (handle Valid Requests) ...

      { "Anon" methods are available ONLY with Delphi 2010 or later! }
      DataProv.OnCheckRequest:=DataProv.Anon(
        procedure(Sender: TRtcConnection)
          begin
          // Check Request headers and "Accept" all Requests
          // we want to handle with our Data Provider ...
          with Sender do
            if (Request.Method='GET') and  // we only want "GET" requests
               (Request.ContentLength=0) then // ... with no content body
                if (Request.URI='/html') or
                   (Request.URI='/json') or
                   (Request.URI='/xml') or
                   (Request.URI='/code') then
                  Accept; // Accept the Request
          end);
      DataProv.OnDataReceived:=DataProv.Anon(
        procedure(Sender: TRtcConnection)
          var
            t:TRtcRecord;
          begin
          with Sender do
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
          end);

      // Assign Server Events (handle the rest) ...
      HttpSrv.OnRequestNotAccepted:=HttpSrv.Anon(
        procedure(Sender: TRtcConnection)
          begin
          // Request wasn't accepted ...
          with Sender do
            begin
            // Send "404" status code back ...
            Response.Status(404,'Not Found');
            Response.ContentType:='text/plain';
            // Something to show in the Web Browser ...
            Write('Bad command.');
            // And ... Disconnect the Client.
            Disconnect;
            end;
          end);
      HttpSrv.OnListenStart:=HttpSrv.Anon(
        procedure(Sender: TRtcConnection)
          begin
          Writeln('Server started.');
          end);
      HttpSrv.OnListenStop:=HttpSrv.Anon(
        procedure(Sender: TRtcConnection)
          begin
          Writeln('Server stopped.');
          end);
      HttpSrv.OnListenError:=HttpSrv.Anon(
        procedure(Sender: TRtcConnection; E: Exception)
          begin
          Writeln('Server Error: '+E.Message);
          end);

      // Configure the Server ...
      HttpSrv.ServerPort:='80';
      HttpSrv.ServerIPV:=rtc_IPv4;
      HttpSrv.MultiThreaded:=True;

      // Start the Server listener ...
      HttpSrv.Listen();

      // Since this is a console application and our
      // Server is Multi-Threaded, we can do whatever
      // we want here. For simplicity reasons, we will
      // just use "ReadLn" to allow the Server to run
      // while we wait for the user to press <Enter>.
      Writeln('Press <Enter> to Quit ...');
      ReadLn;

      // User has pressed <Enter> - time to kill our Server.
    finally
      // Stop the Server listener ...
      HttpSrv.StopListenNow();
      // Destroy the components ...
      HttpSrv.Free;
      DataProv.Free;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.

