{
  @html(<b>)
  UDP Server Connection (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.

  Introducing the @html(<b>) @Link(TRtcUdpServer) @html(</b>) component:
  @html(<br>)
  Low-level Server connection component for UDP communication using raw data.
  There will be no special pre-set formatting when sending or receiving
  data through this server connection component.
}
unit rtcUdpSrv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  rtcTypes,
  rtcConn;

type
  { @Abstract(Server Connection component for UDP communication using raw data)

    There is no predefined formatting when sending and receiving
    data through @Link(TRtcUdpServer) connection component.
    Everything that comes through the connection, will be
    received exactly as it was sent (byte-wise). The same goes
    for sending data out through the component. This makes the
    component universal, so it can be used to write virtualy
    any UDP Server application.
    @html(<br><br>)

    There are BIG differences between the UDP Server component
    and the TCP/IP Server connection components:
    @html(<br><br>)

    1. UDP protocol is connectionless and all datagrams are received
    and accepted by the TRtcUdpServer component itself, without triggering
    events like OnConnect and without creating a separate connection
    component for the 'client connection'. Treat each Datagram as a
    separate connection. Simply read the datagram and reply to the
    client, if required. After each received Datagram, TRtcUdpServer
    will be prepared for sending to the client from which the last
    datagram was received. This makes it easy for you to send the
    answer datagram to the correct client. You can only send ONE (1)
    datagram as the answer to one received datagram.
    @html(<br><br>)

    2. UDP server is processing everything in a single thread,
    even when you use the MultiThreaded mode. The only difference in
    MultiThreaded mode with UDP is that processing will be done in
    a background thread, not the Main Thread. This means that, when
    using MultiThreaded mode, the Main Thread will not be blocked
    if UDP servers needs longer to process one UDP datagram. But,
    only one datagram can be processed at a time by one UDP Server
    and there is only ONE component that does all the processing.
    @html(<br><br>)

    3. Since there is no connection kept open after one UDP client
    sends a datagram to the UDP Server, your server can only respond
    to each datagram with one 'returning' datagram. But, UDP server
    can not send a serie of datagrams to the same client, without
    receiving a serie of datagrams from that UDP Client.
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Local Address to bind the server to (leave empty for ALL)
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to listen on and wait for connections
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcServer.Listen) - Start server
    @html(<br>)
    @Link(TRtcConnection.Read) - Read datagram
    @html(<br>)
    @Link(TRtcConnection.Write) - Write datagram (reply)
    @html(<br>)
    @Link(TRtcServer.StopListen) - Stop server
    @html(<br><br>)

    Envents to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Server started
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Datagram received (need to read)
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Datagram sent (buffer now empty)
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Server stopped
    @html(<br><br>)

    @html(<b>function ReadEx:RtcByteArray;</b><br>)
      Use ReadEx to get the next UDP Datagram that is waiting
      for you in this connection component's receiving buffer.
      A call to Read will also clear the buffer, which means that
      you have to store the RtcByteArray received from ReadEx, before
      you start to process it.
      @html(<br><br>)

      When using UDP, data is received in datagrams (data packages).
      This means that your server will receive one OnDataReceived event
      for every datagram (data package) sent out by clients. Datagrams
      will also be read in one peace (not sliced as with TCP/IP)
      when using the @Link(TRtcConnection.Read) function.
      This makes the UDP protocol easier to use for simple message
      communication that TCP/IP. But, by using UDP, there is no guarantee
      that a datagram will be sent. It could be that you will be notified
      with OnDataLost event when your datagram doesnt reach the destination,
      but it could also be that you will not be notified at all and
      the datagram will simply be lost.
      @html(<br><br>)

      IMPORTANT: ONLY CALL Read from OnDataReceived event handler.
      OnDataReceived will be automatically triggered by the server connection
      component, every time new datagram becomes available.
      @html(<br><br>)

    @html(<b>procedure WriteEx(const s:RtcByteArray);</b><br>)
      Use WriteEx to send one datagram out. Write for UDP will put the
      datagram RtcByteArray into the sending buffer and return immediatelly.
      @html(<br><br>)

      There is a limit to the maximum size of a datagram, which differs
      from system to system. In case your datagram doesn't fit in this
      UDP sending buffer, an exception will be raised from Write.
      @html(<br><br>)

    Check @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  TRtcUdpServer = class(TRtcServer)
  protected
    { @exclude }
    FMultiCast          : Boolean;
    { @exclude }
    FMultiCastAddrStr   : RtcString;
    { @exclude }
    FReuseAddr          : Boolean;

    { @exclude }
    FOnDataLost:TRtcNotifyEvent;

    { Copies properties introduced by this class to the new component.
      @exclude }
    procedure CopyFrom(Dup:TRtcServer); override;

    { Creates a new connection provider
      @exclude}
    function CreateProvider:TObject; override;

    { DataLost event handler,
      which will be mapped to the connection provider.
      @exclude }
    procedure TriggerDataLost; override;
    // @exclude
    procedure CallDataLost; override;

    { Set the parameters introduced by TRtcUdpServer.
      @exclude }
    procedure SetParams; override;

  public
    { @exclude }
    constructor Create(AOwner: TComponent); override;

    { Use this class function to create a new TRtcUdpServer
      component at runtime. DO NOT USE the Create constructor. }
    class function New:TRtcUdpServer;

    { Disconnect for the UDP Server component does NOTHING. }
    procedure Disconnect; override;

  published
    { Set this property to True and the @Link(TRtcUdpServer.UdpMultiCastAddr)
      to a multicast IP address, if you want to receive multicast datagrams }
    property UdpMultiCast:Boolean read  FMultiCast write FMultiCast default False;
    { Set this property to the multicast IP address and the
      @Link(TRtcUdpServer.UdpMultiCast) to TRUE,
      if you want to receive multicast datagrams }
    property UdpMultiCastAddr:RtcString read FMultiCastAddrStr write FMultiCastAddrStr;
    { Set this to True if you want more UDP servers on this PC to
      be able to listen on the same address at the same time.
      @html(<br><br>)

      Be careful when changing this property, because it could easily happen
      that, even though you start your listener without errors, you will not
      be receiving any datagrams, because the other app will 'snatch them' from you.
      To be sure that you will receive the datagrams, leave this property False
      and expect your UDP server to be the only one listening on the port. }
    property UdpReuseAddr:Boolean read FReuseAddr write FReuseAddr default False;

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
    { This event will be triggered every time a chunk of data
      has just been read in. To know exactly how much of it
      was read, use the @Link(TRtcConnection.DataIn) property. }
    property OnDataIn;
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
    { OnDataLost event will be triggerd if you send a datagram out
      and your UDP connection provider receives a notification
      that the datagram could not be sent. This notification will most
      likely not function over internet, but it does work in some LANs.
      @html(<br><br>)

      If you have to be 100% sure that your datagrams reached the destination,
      your destination should reply to each datagram and you should set
      a timeout to disconnect after X seconds if there is no reply AfterDataSent.
      Check the @Link(TRtcConnection.Timeout) property for more information
      on defining timeouts for a connection. }
    property OnDataLost:TRtcNotifyEvent read FOnDataLost write FOnDataLost;
    end;

implementation

uses
  rtcSockets,
  rtcSocketSrvProv;

type
  TSockProv = TRtcSocketServerProvider;

constructor TRtcUdpServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FOnDataLost:=nil;
  end;

class function TRtcUdpServer.New: TRtcUdpServer;
  begin
  Result:=Create(nil);
  end;

function TRtcUdpServer.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TSockProv.Create;
    TSockProv(Con).SocketClass:=DefaultRtcSocketClass;
    TSockProv(Con).Proto:=proUDP;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcUdpServer.TriggerDataLost;
  begin
  Timeout.DataLost;
  EnterEvent;
  try
    CallDataLost;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcUdpServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    with TSockProv(Con) do
      begin
      UdpMultiCast:=Self.UdpMultiCast;
      UdpMultiCastAddr:=Self.UdpMultiCastAddr;
      UdpReuseAddr:=Self.UdpReuseAddr;
      end;
  end;

procedure TRtcUdpServer.CallDataLost;
  begin
  if assigned(OnDataLost) then
    OnDataLost(self);
  end;

procedure TRtcUdpServer.Disconnect;
  begin
  // ignore !
  end;

procedure TRtcUdpServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);

  OnDataLost:=TRtcUdpServer(Dup).OnDataLost;
  end;

end.
