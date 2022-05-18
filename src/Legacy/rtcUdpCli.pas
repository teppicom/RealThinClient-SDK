{
  @html(<b>)
  UDP Client Connection (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  Introducing the @html(<b>) @Link(TRtcUdpClient) @html(</b>) component:
  @html(<br>)
  Low-Level Client connection component for UDP communication using raw data.
  There will be no special pre-set formatting when sending or receiving
  data through this client connection component.
}
unit rtcUdpCli;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  
  rtcTypes,
  rtcConn;

type
  { @Abstract(Client Connection component for UDP communication using raw data)

    There is no predefined formatting when sending and receiving
    data through @Link(TRtcUdpClient) connection component.
    Everything that comes through the connection, will be
    received exactly as it was sent (byte-wise). The same goes
    for sending data out through the component. This makes the
    component universal, so it can be used to write virtualy
    any UDP Client application.
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Address to connect to
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to connect to
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcClient.Connect) - Connect to server
    @html(<br>)
    @Link(TRtcConnection.Write) - Write datagram (send request to server)
    @html(<br>)
    @Link(TRtcConnection.Read) - Read datagram (get result from server)
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect from server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - Connected to server
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Datagram sent (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Datagram received (need to read)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - Disconnected from server
    @html(<br><br>)

    @html(<b>function ReadEx:RtcByteArray;</b><br>)
      Use ReadEx to get the next UDP Datagram that is waiting
      for you in this connection component's receiving buffer.
      A call to Read will also clear the buffer, which means that
      you have to store the RtcString received from Read, before
      you start to process it.
      @html(<br><br>)

      When using UDP, data is received in datagrams (data packages).
      This means that the client will receive one OnDataReceived event
      for every datagram (data package) sent out by server. Datagrams
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
      OnDataReceived will be automatically triggered by the connection
      component, every time new datagram becomes available.
      @html(<br><br>)

    @html(<b>procedure WriteEx(const s:RtcByteArray);</b><br>)
      Use WriteEx to send one datagram out. Write for UDP will put the
      datagram RtcString into the sending buffer and return immediatelly.
      @html(<br><br>)

      There is a limit to the maximum size of a datagram, which differs
      from system to system. In case your datagram doesn't fit in this
      UDP sending buffer, an exception will be raised from Write.
      @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }
  TRtcUdpClient=class(TRtcClient)
  protected
    { @exclude }
    FMultiCast          : Boolean;
    { @exclude }
    FMultiCastIpTTL     : Integer;
    { @exclude }
    FReuseAddr          : Boolean;

    { @exclude }
    FOnDataLost:TRtcNotifyEvent;

    { Creates a new connection provider
      @exclude}
    function CreateProvider:TObject; override;

    { DataLost event handler,
      which will be mapped to the connection provider.
      @exclude }
    procedure TriggerDataLost; override;
    // @exclude
    procedure CallDataLost; override;

    { Set the parameters introduced by TRtcUdpClient.
      @exclude }
    procedure SetParams; override;

  public
    { @exclude }
    constructor Create(AOwner: TComponent); override;

    { Use this class function to create a new TRtcUdpClient
      component at runtime. DO NOT USE the Create constructor. }
    class function New:TRtcUdpClient;

  published
    { Set this property to True and the @Link(TRtcConnection.ServerAddr) to a
      multicast IP address, if you want to multicast your datagrams
      with @Link(TRtcConnection.Write) to multiple IP addresses. }
    property UdpMultiCast:Boolean read  FMultiCast write FMultiCast default False;
    { Set this property to the maximum number of Hops (IP addresses on
      the route to destination) which one multicasted datagram may go
      through before it reaches has to reach the destination IP address.
      IP addresses which would require a datagrams to make more Hops
      to reach it, will not reach their destination. }
    property UdpMultiCastMaxHops:Integer read FMultiCastIpTTL write FMultiCastIpTTL default 1;
    { Set this to True if you want more UDP clients on this PC to
      be able to multicast to the same multicast address at the same time.
      Standard value is FALSE. This property is only used if multicasting. }
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
      has been just read in. To know exactly how much of it was read,
      use the @Link(TRtcConnection.DataIn) property. }
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
      and your UDP connection provider receives a notification from
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
  rtcSocketCliProv;

type
  TSockProv = TRtcSocketClientProvider;

constructor TRtcUdpClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FMultiCastIpTTL:=1;
  end;

class function TRtcUdpClient.New: TRtcUdpClient;
  begin
  Result:=Create(nil);
  end;

function TRtcUdpClient.CreateProvider:TObject;
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

procedure TRtcUdpClient.TriggerDataLost;
  begin
  Timeout.DataLost;
  EnterEvent;
  try
    CallDataLost;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcUdpClient.SetParams;
  begin
  inherited;
  if assigned(Con) then
    with TSockProv(Con) do
      begin
      UdpMultiCast:=Self.UdpMultiCast;
      UdpMultiCastMaxHops:=Self.UdpMultiCastMaxHops;
      UdpReuseAddr:=Self.UdpReuseAddr;
      end;
  end;

procedure TRtcUdpClient.CallDataLost;
  begin
  if assigned(OnDataLost) then
    OnDataLost(self);
  end;

end.
