{
  @html(<b>)
  Plugable Message Server component
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcMessageServer) @html(</b>) component: @html(<br>)
  Plugable Server component can be used for direct client-server in-memory connections, or
  for "plugging" RTC functions/applications into third-party Servers (like NexusDB Server).
}
unit rtcMsgSrv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcConn,
  rtcDataSrv,
  rtcTransports;

type
  { @Abstract(Message Server Connection component)

    Methods to check first:
    @html(<br>)
    @Link(TRtcDataServer.Request), @Link(TRtcConnection.Read) - Read client request
    @html(<br>)
    @Link(TRtcDataServer.Response), @Link(TRtcMessageServer.WriteHeader), @Link(TRtcMessageServer.Write) - Write result to client
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Listener Started
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - new Client connected
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data available from client (check @Link(TRtcDataServer.Request))
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to client (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - one Client disconnected
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Listener Stopped
    @html(<br><br>)

    Check @Link(TRtcDataServer), @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcMessageServer = class(TRtcDataServer, IRTCMessageReceiver)
  private
    ConnPool:TRtcObjectList;
    FCS:TRtcCritSec;

    FWritten:boolean;
    FWriteBuffer:TRtcHugeByteArray;

    procedure CloseAllConnections;
    
  protected
    // @exclude
    procedure CopyFrom(Dup: TRtcServer); override;

    // @exclude
    procedure SetParams; override;

    // @exclude
    function CreateProvider:TObject; override;

    // @exclude
    procedure TriggerDataSent; override;
    // @exclude
    procedure TriggerDataReceived; override;
    // @exclude
    procedure TriggerDataOut; override;

    // @exclude
    procedure SetSrvRequest(const Value: TRtcServerRequest); override;
    // @exclude
    procedure SetSrvResponse(const Value: TRtcServerResponse); override;

  public

    // This will always return TRUE for TRtcMessageServer.
    function isExtension:boolean; override;

    { Get a new replica of the TRtcMessageServer component,
      which will represent a new physical connection.
      This can be used in combination with ProcessData and
      PutConnection to process request data as it arrives
      instead of preparing the complete request in memory
      and making a single "ProcessMessage" call. }
    function GetConnection:TObject;

    { aRequest contains data which needs to be processed,
      aReply will contain the response (if any).

      This method is called directly on this component and can
      be used to process data as it arrives (partial content),
      but it is NOT to be used from more than one thread at a time,
      nor for processing more than one physical connection,
      since it has an internal state specific to the connection. }
    procedure ProcessData(aRequest, aReply: TStream);

    { Return a connection object (received using GetConnection)
      to the connection object pool, so it can be freed.
      PutConnection has to be used once for each GetConnection call. }
    procedure PutConnection(conn:TObject);

    { aRequest is a stream containing a complete HTTP request,
      aReply is a stream which will receive a complete HTTP response.
      This method is tread-safe and can be called from all threads at the same time,
      because a new replica of the connection component will be created internaly
      and used for processing the request, and ... destroyed before the method returns. }
    procedure ProcessMessage(aRequest, aReply: TStream);

    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    // Constructor
    class function New:TRtcMessageServer;

    { Flush all buffered data.
      @html(<br>)
      When using 'Write' without calling 'WriteHeader' before, all data
      prepared by calling 'Write' will be buffered until your event
      returns to its caller (automatically upon your event completion) or
      when you first call 'Flush'. Flush will check if Response.ContentLength is set
      and if not, will set the content length to the number of bytes buffered.
      @html(<br>)
      Flush does nothing if WriteHeader was called for this response.

      @exclude}
    procedure Flush; override;

    // You can call WriteHeader to send the Response header out.
    procedure WriteHeader(SendNow:boolean=True); overload; override;
    { You can call WriteHeader with empty 'HeaderText' parameter to
      tell the component that you do not want any HTTP header to be sent. }
    procedure WriteHeader(const HeaderText: RtcString; SendNow:boolean=True); overload; override;

    // Use Write to send the Content (document body) out.
    procedure WriteEx(const s:RtcByteArray); override;
    // Use Write to send the Content (document body) out.
    procedure Write(const s:RtcString); override;
    end;

implementation

uses
  rtcConnProv,
  rtcMsgSrvProv;

type
  TMyProvider = TRtcMessageServerProvider;

{ TRtcMessageServer }

constructor TRtcMessageServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FCS:=TRtcCritSec.Create;

  ConnPool:=nil;

  FWriteBuffer:=TRtcHugeByteArray.Create;
  FWritten:=False;
  end;

destructor TRtcMessageServer.Destroy;
  begin
  try
    CloseAllConnections;

    RtcFreeAndNil(FWriteBuffer);
    RtcFreeAndNil(FCS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcMessageServer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcMessageServer.GetConnection:TObject;
  begin
  Result:=nil;
  FCS.Acquire;
  try
    if assigned(ConnPool) then
      begin
      if ConnPool.Count > 0 then
        begin
        Result:= ConnPool.items[ConnPool.Count-1];
        ConnPool.Delete(ConnPool.Count-1);
        end;
      end;
  finally
    FCS.Release;
    end;
  { Now we either have the connection,
     or we need to create one. }
  if Result=nil then
    begin
    TriggerConnectionAccepting;

    Result:=TRtcMessageServer(self.copyOf);
    end;

  TMyProvider(TRtcMessageServer(Result).Con).Connect;
  end;

procedure TRtcMessageServer.PutConnection(conn:TObject);
  begin
  TMyProvider(TRtcMessageServer(conn).Con).Disconnect;

  FCS.Acquire;
  try
    if not assigned(ConnPool) then
      ConnPool:=TRtcObjectList.Create;
    ConnPool.Add(conn);
  finally
    FCS.Release;
    end;
  end;

procedure TRtcMessageServer.CloseAllConnections;
  var
    i    :integer;
    mycon:TRtcMessageServer;
  begin
  FCS.Acquire;
  try
    if assigned(ConnPool) then
      begin
      for i:= 0 to ConnPool.count - 1 do
        begin
        mycon:= TRtcMessageServer(ConnPool.items[i]);
        mycon.Release;
        end;
      ConnPool.Clear;
      RtcFreeAndNil(ConnPool);
      end;
  finally
    FCS.Release;
    end;
  end;

class function TRtcMessageServer.New: TRtcMessageServer;
  begin
  Result:=Create(nil);
  end;

function TRtcMessageServer.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TMyProvider.Create;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcMessageServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);
  end;

procedure TRtcMessageServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    begin
    TMyProvider(Con).Request:=Request;
    TMyProvider(Con).Response:=Response;
    TMyProvider(Con).FixupRequest:=FixupRequest;
    end;
  end;

procedure TRtcMessageServer.WriteHeader(SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader;
    end;
  end;

procedure TRtcMessageServer.WriteHeader(const HeaderText: RtcString; SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader(HeaderText);
    end;
  end;

procedure TRtcMessageServer.WriteEx(const s: RtcByteArray);
  begin
  if State>=conActive then
    begin
    if Response.Sent then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if Response.Sending then
      begin
      { Header is out }

      if Response.ValueCS['CONTENT-LENGTH']<>'' then
        if Response.ContentLength - Response.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the RtcByteString out, without buffering }

      Con.WriteEx(s);
      end
    else
      begin
      if (Response.ValueCS['CONTENT-LENGTH']<>'') and not FWritten then // Direct writing if header was sent out.
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Response.ContentLength - Response.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');
        Con.WriteEx(s);
        end
      else
        begin
        { Header is not out.
          Buffer all Write() operations,
          so we can determine content size and write it all out in a flush. }
        FWritten:=True;
        FWriteBuffer.AddEx(s);
        end;
      end;
    end;
  end;

procedure TRtcMessageServer.Write(const s: RtcString);
  begin
  if State>=conActive then
    begin
    if Response.Sent then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if Response.Sending then
      begin
      { Header is out }

      if Response.ValueCS['CONTENT-LENGTH']<>'' then
        if Response.ContentLength - Response.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the RtcByteString out, without buffering }

      Con.Write(s);
      end
    else
      begin
      if (Response.ValueCS['CONTENT-LENGTH']<>'') and not FWritten then // Direct writing if header was sent out.
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Response.ContentLength - Response.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');
        Con.Write(s);
        end
      else
        begin
        { Header is not out.
          Buffer all Write() operations,
          so we can determine content size and write it all out in a flush. }
        FWritten:=True;
        FWriteBuffer.Add(s);
        end;
      end;
    end;
  end;

procedure TRtcMessageServer.Flush;
  var
    Temp:RtcByteArray;
  begin
  if not FWritten then
    Exit
  else
    FWritten:=False; // so we don't re-enter this method.

  if State>=conActive then
    begin
    Timeout.DataSending;

    if Response.Sent then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if not Response.Sending then
      begin
      if Response.ValueCS['CONTENT-LENGTH']='' then // length not specified
        Response.ContentLength:=FWriteBuffer.Size;

      TMyProvider(Con).WriteHeader;
      end;

    if FWriteBuffer.Size>0 then
      begin
      Temp:= FWriteBuffer.GetEx;
      FWriteBuffer.Clear;

      Con.WriteEx(Temp);
      SetLength(Temp,0);
      end;
    end;
  end;

procedure TRtcMessageServer.TriggerDataReceived;
  begin
  inherited;
  Flush;
  end;

procedure TRtcMessageServer.TriggerDataSent;
  begin
  if FWriteCount>0 then
    Timeout.DataSent;
  EnterEvent;
  try
    if FWriteCount>0 then
      begin
      CallDataSent;
      Flush;

      if Response.Done then
        if Request.Close then
          Disconnect; // make sure we close the connection, as requested by the client.
      end;

    if not isClosing then
      begin
      CallReadyToSend;
      Flush;

      if (FWriteCount>0) and Response.Done then
        if Request.Close then
          Disconnect; // make sure we close the connection, as requested by the client.
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcMessageServer.TriggerDataOut;
  begin
  inherited;
  Flush;
  end;

procedure TRtcMessageServer.SetSrvRequest(const Value: TRtcServerRequest);
  begin
  inherited SetSrvRequest(Value);
  if assigned(Con) then
    TMyProvider(Con).Request:=Request;
  end;

procedure TRtcMessageServer.SetSrvResponse(const Value: TRtcServerResponse);
  begin
  inherited SetSrvResponse(Value);
  if assigned(Con) then
    TMyProvider(Con).Response:=Response;
  end;

procedure TRtcMessageServer.ProcessMessage(aRequest, aReply: TStream);
  var
    Server:TRtcMessageServer;
  begin
  Server:=TRtcMessageServer(GetConnection);
  try
    Server.ProcessData(aRequest, aReply);
    if not Server.Response.Sent then
      raise Exception.Create('Error! A complete response has to be sent from ProcessMessage!');
  finally
    PutConnection(Server);
    end;
  end;

procedure TRtcMessageServer.ProcessData(aRequest, aReply: TStream);
  begin
  EnterEvent;
  try
    TMyProvider(Con).ExecuteRequest(aRequest, aReply);
  finally
    LeaveEvent;
    end;
  end;

function TRtcMessageServer.isExtension: boolean;
  begin
  Result:=True;
  end;

end.
