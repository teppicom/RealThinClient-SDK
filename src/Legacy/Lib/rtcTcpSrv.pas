{
  @html(<b>)
  TCP/IP Server Connection (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  Introducing the @html(<b>) @Link(TRtcTcpServer) @html(</b>) component:
  @html(<br>)
  Low-level Server connection component for TCP/IP communication using raw data.
  There will be no special pre-set formatting when sending or receiving
  data through this server connection component.
}
unit rtcTcpSrv;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTypes,

  rtcSockets,
  rtcSocketSrvProv, // Socket Server Provider
  
  rtcConn;

type
  { @Abstract(Server Connection component for TCP/IP communication using raw data)

    There is no predefined formatting when sending and receiving
    data through @Link(TRtcTcpServer) connection component.
    Everything that comes through the connection, will be
    received exactly as it was sent (byte-wise). The same goes
    for sending data out through the component. This makes the
    component universal, so it can be used to write virtualy
    any TCP/IP Server application.
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Local Address to bind the server to (leave empty for ALL)
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to listen on and wait for connections
    @html(<br><br>)

    Methods to check first:
    Most important TRtcTcpServer's methods are:
    @html(<br>)
    @Link(TRtcServer.Listen) - Start server
    @html(<br>)
    @Link(TRtcConnection.Read) - Read data
    @html(<br>)
    @Link(TRtcConnection.Write) - Write data
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect client
    @html(<br>)
    @Link(TRtcServer.StopListen) - Stop server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Server started
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - new Client connected
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data received from client (need to read)
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to client (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - one Client disconnected
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Server stopped
    @html(<br><br>)

    @html(<b>function ReadEx:RtcByteArray;</b><br>)
      Use ReadEx to get all the data that is waiting
      for you in this connection component's receiving buffer.
      A call to Read will also clear the buffer, which means that
      you have to store the RtcByteString received from Read, before
      you start to process it.
      @html(<br><br>)

      Keep in mind that when using TCP/IP,
      data is received as a stream (or rather, peaces of it),
      without pre-defined end-marks for each package sent or received.
      This means that the client could have sent a big chunk of data in
      just one call, but the server will receive several smaller packages
      of different sizes. It could also happen that client sends multiple
      smaller packages, which your server connection could receive
      as one big package. A combination of those circumstances is also possible.
      @html(<br><br>)

      So, before you start processing the data you receive, make sure that
      you have received everything you need. You could create a buffer for
      storing temporary data, so you can react on multiple OnDataReceived
      events and put all data received inside your buffer, before you
      actually start processing the data.
      @html(<br><br>)

      IMPORTANT: ONLY CALL Read from OnDataReceived event handler.
      OnDataReceived will be automatically triggered by the connection
      component, every time new data becomes available. If your Server
      has to work on non-Windows platforms, check the "NeedMoreData" method.
    @html(<br><br>)

    @html(<b>procedure WriteEx(const s:RtcByteArray);</b><br>)
      Use WriteEx to send data out. Write will not block your
      code execution, it will only put the RtcByteString into sending buffer
      and return immediatelly.
      @html(<br><br>)

      Keep in mind that all the data you put into the sending
      buffer will remain there until it was sent out or the
      connection through it was to be sent closed. To avoid filling
      your buffer with data that will not be sent out for some time,
      try sending a small peace at a time and then react on the
      @Link(TRtcConnection.OnDataSent) event to continue and send the next one.
      Packages you send out at once shouldn't be larger that 64 KB.

    @html(<br><br>)

    Check @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  TRtcTcpServer = class(TRtcServer)
  protected
    { Creates a new connection provider
      @exclude}
    function CreateProvider:TObject; override;
    // @exclude
    procedure SetParams; override;

  public
    { Use this class function to create a new TRtcTcpServer
      component at runtime. DO NOT USE the Create constructor. }
    class function New:TRtcTcpServer;

    { In order to make your implementation cross-platform, you need to call
      this method to signal that you are waiting for data from the Client.
      Only after calling "NeedMoreData" will the low-level TCP socket class
      enter a "reading loop" and start waiting for incoming data. Without
      calling "NeedMoreData", the "OnDataReceived" event will NOT be called
      even if the Client has sent data to the Server. }
    procedure NeedMoreData;

  published
    { This event will be triggered every time this connection component's
      buffer is completely empty and the other side has just become ready to
      accept new data. It is good to wait for this event before starting
      to send data out, even though you can start sending data directly
      from the @Link(TRtcConnection.OnConnect) event.
      @html(<br><br>)

      By responding to this event and sending the data only after it was
      triggered, you avoid keeping the data in the send buffer, especially
      if the data you are sending is being read from a file on disk,
      which wouldn't occupy any memory until loaded. }
    property OnReadyToSend;
    { This event will be triggered every time a chunk of your data
      prepared for sending has just been sent out. To know
      exactly how much of it is on the way, use the @Link(TRtcConnection.DataOut) property.
      @html(<br><br>)

      NOTE: Even though data has been sent out, it doesn't mean that
      the other side already received it. It could also be that connection will
      break before this package reaches the other end. }
    property OnDataOut;
    { This event will be triggered when all data prepared for sending
      has been sent out and the sending buffer has become empty again.
      @html(<br><br>)

      When sending large data blocks, try slicing them in small chunks,
      sending a chunk at a time and responding to this event to prepare
      and send the next chunk. This will keep your memory needs low. }
    property OnDataSent;
    { When this event triggers, it means that the other side has sent you
      some data and you can now read it. Check the connection component's
      description to see which properties and methods you can use
      to read the data received. }
    property OnDataReceived;

    { You can set all timeout parameters for the clients underlying API connection or
      default timeout periods for all client connections of the server connection component
      using this property. Check @Link(TRtcTimeoutsOfAPI) for more information. }
    property TimeoutsOfAPI;
    end;

implementation

type
  TSockProv = TRtcSocketServerProvider;

class function TRtcTcpServer.New: TRtcTcpServer;
  begin
  Result:=Create(nil);
  end;

function TRtcTcpServer.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TSockProv.Create;
    TSockProv(Con).SocketClass:=DefaultRtcSocketClass;
    TSockProv(Con).Proto:=proTCP;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcTcpServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    TSockProv(Con).TimeoutsOfAPI:=TimeoutsOfAPI;
  end;

procedure TRtcTcpServer.NeedMoreData;
  begin
  if assigned(Con) then
    TSockProv(Con).NeedMoreData;
  end;

end.
