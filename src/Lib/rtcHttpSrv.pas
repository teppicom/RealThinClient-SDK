{
  @html(<b>)
  HTTP Server Connection
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcHttpServer) @html(</b>) component:
  @html(<br>)
  Server connection component for TCP/IP communication using HTTP requests.
  Received data will be processed to gather Request information and
  make it easily accessible through the @Link(TRtcDataServer.Request) property.
  The same way, your response will be packed into a HTTP result header
  and sent out as a valid HTTP result, readable by any Web Browser.
  @Link(TRtcHttpServer) also makes sure that you receive requests one by one,
  even if the client side sends all requests at once (as one big request list).
}
unit rtcHttpSrv;

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

  rtcPlugins,
  rtcDataSrv;

type
  { @Abstract(Server Connection component for direct TCP/IP communication using the HTTP protocol)

    Received data will be processed by TRtcHttpServer to gather Request
    information and make it easily accessible through the
    @Link(TRtcDataServer.Request) property.
    The same way, your response will be packed into a HTTP response header
    and sent out as a valid HTTP response, readable by any Web Browser.
    @html(<br>)
    @Link(TRtcHttpServer) also makes sure that you receive requests one by one
    and get the chance to answer them one-by-one, even if the client side
    sends all the requests at once (as one big request list), so
    you can relax and process all incomming requests, without worrying
    about overlapping your responses for different requests.
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
    @Link(TRtcDataServer.Request), @Link(TRtcConnection.Read) - Read client request
    @html(<br>)
    @Link(TRtcDataServer.Response), @Link(TRtcHttpServer.WriteHeader), @Link(TRtcHttpServer.Write) - Write result to client
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
    @Link(TRtcConnection.OnDisconnect) - one Client disconnected
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Server stopped
    @html(<br><br>)

    Check @Link(TRtcDataServer), @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcHttpServer = class(TRtcDataServer)
  private
    FProvType:byte;
    // User Parameters
    FMaxRequestSize:cardinal;
    FMaxHeaderSize:cardinal;
    FOnInvalidRequest:TRtcNotifyEvent;
    FCryptPlugin:TRtcCryptPlugin;

    FBeforeDestroy:TRtcNotifyEvent;
    FProtoHttp:RtcProtoHttp;

    FWritten:boolean;
    FWriteBuffer:TRtcHugeByteArray;
    FBlocking: boolean;

    function GetCryptObject: TObject;
    procedure SetBlocking(const Value: boolean);
    procedure SetCryptPlugin(const Value: TRtcCryptPlugin);
    procedure SetProtoHttp(const Value: RtcProtoHttp);

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure CopyFrom(Dup: TRtcServer); override;

    // @exclude
    procedure SetTriggers; override;
    // @exclude
    procedure ClearTriggers; override;
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
    procedure TriggerInvalidRequest; virtual;
    // @exclude
    procedure CallInvalidRequest; virtual;

    // @exclude
    procedure SetSrvRequest(const Value: TRtcServerRequest); override;
    // @exclude
    procedure SetSrvResponse(const Value: TRtcServerResponse); override;

  public
    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    // Constructor
    class function New:TRtcHttpServer;

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

    // Use WriteEx to send the Content (document body) out.
    procedure WriteEx(const s:RtcByteArray); override;

    // Use Write to send the Content (document body) out.
    procedure Write(const s:RtcString); override;

    { Encryption object associated with this physical connection.
      The actual type of the object depends on the CryptPlugin component used.
      Will be NIL if Encryption components are not used,
      not initialized or did not update this property. }
    property CryptObject:TObject read GetCryptObject;

  published
    { Server HTTP "Protocol" prefference. @html(<br>)
      With ServerProtocol=rtc_Http1, a connection provider with support for 
      HTTP/1.0 and HTTP/1.1 will be used, while support for HTTP/2 stays disabled. @html(<br>)
      With ServerProtocol=rtc_Http2, a connection provider with support for HTTP/2 
      will be used, with a fall-back to HTTP/1 for Clients which do NOT support HTTP/2. }
    property ServerProtocol:RtcProtoHttp read FProtoHttp write SetProtoHttp default rtc_HttpDefault;

    { Setting Blocking to TRUE will make sure that a blocking
      low-level connection provider will be used for all communication. @html(<br><br>)

      When used from console applications or other types of applications which
      does NOT have a message queue, in addition to setting Blocking to TRUE,
      it is also recommended to set the MultiThreaded property to TRUE and
      make sure that your code written for RTC component events is thread-safe. @html(<br><br>)

      When used from standard windowed applications which DO HAVE a message queue,
      because all the communication has to be handled from background threads
      for the Server to work correctly, if you leave the MultiThreaded property
      as FALSE (so you do NOT have to worry about threading), all events triggered
      by this and all linked RTC SDK components will be called synchronized. @html(<br><br>)

      WARNING: Setting Blocking to TRUE but leaving MultiThreaded as FALSE will
      ONLY work in normal WINDOWED applications. When using "Blocking=TRUE" in
      console or service applications, make sure to also set MultiThreaded to TRUE. }
    property Blocking:boolean read FBlocking write SetBlocking default False;

    { Maximum allowed size of the request, without header (0 = no limit).
      This is the first line in a HTTP request and includes Request.Method and Request.URI }
    property MaxRequestSize:cardinal read FMaxRequestSize write FMaxRequestSize default 0;
    { Maximum allowed size of each request's header size (0 = no limit).
      This are all the remaining header lines in a HTTP request,
      which come after the first line and end with an empty line,
      after which usually comes the content (document body). }
    property MaxHeaderSize:cardinal read FMaxHeaderSize write FMaxHeaderSize default 0;

    { This event will be called if the received request exceeds your defined
      maximum request or header size. If both values are 0, this event will never be called. }
    property OnInvalidRequest:TRtcNotifyEvent read FOnInvalidRequest write FOnInvalidRequest;

    { To use SSL/TLS encryption, assign the encryption plug-in here,
      before you first start the Server Listener. }
    property CryptPlugin:TRtcCryptPlugin read FCryptPlugin write SetCryptPlugin;

    { You can set all timeout parameters for the clients underlying API connection or
      default timeout periods for all client connections of the server connection component
      using this property. Check @Link(TRtcTimeoutsOfAPI) for more information. }
    property TimeoutsOfAPI;

    { Event triggered just before the component is destroyed }
    property BeforeDestroy:TRtcNotifyEvent read FBeforeDestroy write FBeforeDestroy;
    end;

implementation

{$IFNDEF RTC_noAsynSock} {$DEFINE RTC_useAsynSock} {$DEFINE RTC_useSockets} {$ENDIF}
{$IFNDEF RTC_noSynSock}  {$DEFINE RTC_useSynSock}  {$DEFINE RTC_useSockets} {$ENDIF}

uses
  {$IFNDEF RTC_noAsynSock} rtcWinSocket, {$ENDIF}
  {$IFNDEF RTC_noSynSock} rtcSynSocket, {$ENDIF}
  {$IFDEF RTC_useSockets} {$DEFINE RTC_ProvOK} rtcSocketHttpSrvProv, {$ENDIF}
  rtcConnProv;

{$IFNDEF RTC_ProvOK}
  {$MESSAGE WARN 'TRtcHttpServer component unusable: You have disabled all Server API support options!'}
{$ENDIF}

const cpt_None=0;

{$IFDEF RTC_useSockets}
type  TSocketProv  = TRtcSocketHttpServerProvider;
const cpt_Sock1=10;

type  TSocketProv2 = TRtcSocketHttp2ServerProvider;
const cpt_Sock2=20;
{$ENDIF}

class function TRtcHttpServer.New: TRtcHttpServer;
  begin
  Result:=Create(nil);
  end;

{ TRtcHttpServer }

constructor TRtcHttpServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FProtoHttp:=rtc_HttpDefault;
  FWriteBuffer:=TRtcHugeByteArray.Create;
  FWritten:=False;
  end;

destructor TRtcHttpServer.Destroy;
  begin
  try
    FNotSilent:=False;
    FAmSilent:=True;

    if assigned(FBeforeDestroy) then
      FBeforeDestroy(self);

    RtcFreeAndNil(FWriteBuffer);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcHttpServer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcHttpServer.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    ws_Clear;

  {$IFDEF RTC_useSockets}
    if FProtoHttp=rtc_Http1 then
      begin
      FProvType:=cpt_Sock1;
      Con:=TSocketProv.Create;
      if Blocking then
        begin
      {$IFDEF RTC_useSynSock}
        TSocketProv(Con).SocketClass:=TRtcSynSocket;
      {$ELSE}
        TSocketProv(Con).SocketClass:=TRtcWinSocket;
      {$ENDIF}
        end
      else
        begin
      {$IFDEF RTC_useAsynSock}
        TSocketProv(Con).SocketClass:=TRtcWinSocket;
      {$ELSE}
        TSocketProv(Con).SocketClass:=TRtcSynSocket;
      {$ENDIF}
        end;
      end
    else // rtc_Http2
      begin
      FProvType:=cpt_Sock2;
      Con:=TSocketProv2.Create;
      if Blocking then
        begin
      {$IFDEF RTC_useSynSock}
        TSocketProv2(Con).SocketClass:=TRtcSynSocket;
      {$ELSE}
        TSocketProv2(Con).SocketClass:=TRtcWinSocket;
      {$ENDIF}
        end
      else
        begin
      {$IFDEF RTC_useAsynSock}
        TSocketProv2(Con).SocketClass:=TRtcWinSocket;
      {$ELSE}
        TSocketProv2(Con).SocketClass:=TRtcSynSocket;
      {$ENDIF}
        end;
      end;
  {$ELSE}
    raise Exception.Create('TRtcHttpServer.CreateProvider: No API support. Can not initialize connection.');
  {$ENDIF}
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcHttpServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);

  MaxRequestSize:=TRtcHttpServer(Dup).MaxRequestSize;
  MaxHeaderSize:=TRtcHttpServer(Dup).MaxHeaderSize;
  OnInvalidRequest:=TRtcHttpServer(Dup).OnInvalidRequest;
  FCryptPlugin:=TRtcHttpServer(Dup).CryptPlugin;
  FProtoHttp:=TRtcHttpServer(Dup).ServerProtocol;
  FBlocking:=TRtcHttpServer(Dup).Blocking;
  end;

procedure TRtcHttpServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1:
        begin
        TSocketProv(Con).CryptPlugin:=CryptPlugin;
        TSocketProv(Con).Request:=Request;
        TSocketProv(Con).Response:=Response;
        TSocketProv(Con).MaxRequestSize:=MaxRequestSize;
        TSocketProv(Con).MaxHeaderSize:=MaxHeaderSize;
        TSocketProv(Con).FixupRequest:=FixupRequest;
        TSocketProv(Con).TimeoutsOfAPI:=TimeoutsOfAPI;
        end;
      cpt_Sock2:
        begin
        TSocketProv2(Con).CryptPlugin:=CryptPlugin;
        TSocketProv2(Con).Request:=Request;
        TSocketProv2(Con).Response:=Response;
        TSocketProv2(Con).MaxRequestSize:=MaxRequestSize;
        TSocketProv2(Con).MaxHeaderSize:=MaxHeaderSize;
        TSocketProv2(Con).FixupRequest:=FixupRequest;
        TSocketProv2(Con).TimeoutsOfAPI:=TimeoutsOfAPI;
        end;
    {$ENDIF}
      cpt_None: raise Exception.Create('TRtcHttpServer.SetParams: Connection provider not set.');
      else      raise Exception.Create('TRtcHttpServer.SetParams: Connection Provider not recognized.');
      end;
  end;

function TRtcHttpServer.GetCryptObject: TObject;
  begin
  if Con=nil then
    Result:=nil
  else
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1: Result:=TSocketProv(Con).CryptObject;
      cpt_Sock2: Result:=TSocketProv2(Con).CryptObject;
    {$ENDIF}
      cpt_None: Result:=nil;
      else      Result:=nil;
      end;
  end;

procedure TRtcHttpServer.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1: TSocketProv(Con).SetTriggerInvalidRequest(TriggerInvalidRequest);
      cpt_Sock2: TSocketProv2(Con).SetTriggerInvalidRequest(TriggerInvalidRequest);
    {$ENDIF}
      cpt_None: raise Exception.Create('TRtcHttpServer.SetTriggers: Connection Provider not set.');
      else      raise Exception.Create('TRtcHttpServer.SetTriggers: Connection Provider not recognized.');
      end;
  end;

procedure TRtcHttpServer.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1: TSocketProv(Con).SetTriggerInvalidRequest(nil);
      cpt_Sock2: TSocketProv2(Con).SetTriggerInvalidRequest(nil);
    {$ENDIF}
      cpt_None: raise Exception.Create('TRtcHttpServer.ClearTriggers: Connection Provider not set.');
      else      raise Exception.Create('TRtcHttpServer.ClearTriggers: Connection Provider not recognized.');
      end;
  end;

procedure TRtcHttpServer.WriteHeader(SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1: TSocketProv(Con).WriteHeader(SendNow);
      cpt_Sock2: TSocketProv2(Con).WriteHeader(SendNow);
    {$ENDIF}
      cpt_None:  raise Exception.Create('TRtcHttpServer.WriteHeader: Connection Provider not set.');
      else       raise Exception.Create('TRtcHttpServer.WriteHeader: Connection Provider not recognized.');
      end;
    end;
  end;

procedure TRtcHttpServer.WriteHeader(const HeaderText: RtcString; SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1: TSocketProv(Con).WriteHeader(HeaderText, SendNow);
      cpt_Sock2: TSocketProv2(Con).WriteHeader(HeaderText, SendNow);
    {$ENDIF}
      cpt_None: raise Exception.Create('TRtcHttpServer.WriteHeader: Connection Provider not set.');
      else      raise Exception.Create('TRtcHttpServer.WriteHeader: Connection Provider not recognized.');
      end;
    end;
  end;

procedure TRtcHttpServer.WriteEx(const s: RtcByteArray);
  begin
  if State>=conActive then
    if isWebSocket then
      Con.WriteEx(s)
    else
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
          We can just write the RtcByteArray out, without buffering }

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
          if assigned(Con) then Con.WriteEx(s);
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

procedure TRtcHttpServer.Write(const s: RtcString);
  begin
  if State>=conActive then
    if isWebSocket then
      Con.Write(s)
    else
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
          We can just write the RtcByteArray out, without buffering }

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
          if assigned(Con) then Con.Write(s);
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

procedure TRtcHttpServer.Flush;
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
      raise Exception.Create('Error! Response allready sent for this Request.');

    if not Response.Sending then
      begin
      if Response.ValueCS['CONTENT-LENGTH']='' then // length not specified
        Response.ContentLength:=FWriteBuffer.Size;

      case FProvType of
      {$IFDEF RTC_useSockets}
        cpt_Sock1: TSocketProv(Con).WriteHeader(False);
        cpt_Sock2: TSocketProv2(Con).WriteHeader(False);
      {$ENDIF}
        cpt_None: raise Exception.Create('TRtcHttpServer.Flush: Connection Provider not set.');
        else      raise Exception.Create('TRtcHttpServer.Flush: Connection Provider not recognized.');
        end;
      end;

    if FWriteBuffer.Size>0 then
      if assigned(Con) then
        begin
        Temp:=FWriteBuffer.GetEx;
        FWriteBuffer.Clear;
        Con.WriteEx(Temp);
        SetLength(Temp,0);
        end
      else
        FWriteBuffer.Clear;
    end;
  end;

procedure TRtcHttpServer.CallInvalidRequest;
  begin
  if assigned(OnInvalidRequest) then
    OnInvalidRequest(self);
  end;

procedure TRtcHttpServer.TriggerDataReceived;
  begin
  if State>conPrepared then
    begin
    inherited;
    Flush;
    end;
  end;

procedure TRtcHttpServer.TriggerDataSent;
  begin
  if State>conPrepared then
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
  end;

procedure TRtcHttpServer.TriggerDataOut;
  begin
  if State>conPrepared then
    begin
    inherited;
    Flush;
    end;
  end;

procedure TRtcHttpServer.TriggerInvalidRequest;
  begin
  if State>conPrepared then
    begin
    EnterEvent;
    try
      CallInvalidRequest;
      Flush;

      Disconnect;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcHttpServer.SetSrvRequest(const Value: TRtcServerRequest);
  begin
  inherited SetSrvRequest(Value);
  if assigned(Con) then
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1: TSocketProv(Con).Request:=Request;
      cpt_Sock2: TSocketProv2(Con).Request:=Request;
    {$ENDIF}
      cpt_None: raise ERtcConnection.Create('TRtcHttpServer.SetRequest: Connection Provider not set.');
      else      raise ERtcConnection.Create('TRtcHttpServer.SetRequest: Connection Provider not recognized.');
      end;
  end;

procedure TRtcHttpServer.SetSrvResponse(const Value: TRtcServerResponse);
  begin
  inherited SetSrvResponse(Value);
  if assigned(Con) then
    case FProvType of
    {$IFDEF RTC_useSockets}
      cpt_Sock1: TSocketProv(Con).Response:=Response;
      cpt_Sock2: TSocketProv2(Con).Response:=Response;
    {$ENDIF}
      cpt_None: raise ERtcConnection.Create('TRtcHttpServer.SetResponse: Connection Provider not set.');
      else      raise ERtcConnection.Create('TRtcHttpServer.SetResponse: Connection Provider not recognized.');
      end;
  end;

procedure TRtcHttpServer.SetBlocking(const Value: boolean);
  begin
  if FBlocking<>Value then
    begin
    if assigned(Con) then
      if isListening then
        Error('Can not change Blocking when Server is listening.')
      else
        ReleaseProvider;
    FBlocking:=Value;
    end;
  end;

procedure TRtcHttpServer.SetProtoHttp(const Value: RtcProtoHttp);
  begin
  if FProtoHttp<>Value then
    begin
    if assigned(Con) then
      if isListening then
        Error('Can not change ServerProtocol when Server is listening.')
      else
        ReleaseProvider;
    {$IFNDEF RTC_HTTP20}
    if Value<>rtc_Http1 then
      Error('Only HTTP/1 is currently supported!');
    {$ENDIF}
    FProtoHttp:=Value;
    end;
  end;

procedure TRtcHttpServer.SetCryptPlugin(const Value: TRtcCryptPlugin);
  begin
  if FCryptPlugin<>Value then
    begin
    if assigned(Con) then
      if isListening then
        Error('Can not change CryptPlugin when Server is listening.')
      else
        ReleaseProvider;
    FCryptPlugin := Value;
    end;
  end;

procedure TRtcHttpServer.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FCryptPlugin then
      SetCryptPlugin(nil);
  end;

end.
