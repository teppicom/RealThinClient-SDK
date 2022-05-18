{
  @html(<b>)
  Plugable Message Client component
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcMessageClient) @html(</b>) component:
  @html(<br>)
  Plugable Client component can be used for direct client-server in-memory connections, or
  for "plugging" RTC Clients into third-party connection components (like NexusDB).
}
unit rtcMsgCli;

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
  rtcDataCli;

type
  { @Abstract(Plugable Message Client Connection component)

    Received data will be processed by TRtcMessageClient to gather Request
    information and make it easily accessible through the
    @Link(TRtcDataClient.Request) property.
    The same way, your response will be packed into a HTTP result header
    and sent out as a valid HTTP result, readable by any Web Browser.
    @html(<br>)
    @Link(TRtcMessageClient) also makes sure that you receive requests one by one
    and get the chance to answer them one-by-one, even if the client side
    sends all the requests at once (as one big request list), so
    you can relax and process all incomming requests, without worrying
    about overlapping your responses for different requests.
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TRtcMessageClient.Server) - Server connection component (where our requests are sent for processing)
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcDataClient.Request), @Link(TRtcMessageClient.WriteHeader), @Link(TRtcMessageClient.Write) - Write (send) Request to Server
    @html(<br>)
    @Link(TRtcDataClient.Response), @Link(TRtcConnection.Read) - Read Server's Response
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to server (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data available from server (check @Link(TRtcDataClient.Response))
    @html(<br>)
    @Link(TRtcMessageClient.OnInvalidResponse) - Received invalid response from Server
    @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcMessageClient = class(TRtcDataClient)
  private
    FServer:TComponent;

    // User Parameters
    FMaxResponseSize:cardinal;
    FMaxHeaderSize:cardinal;
    FOnInvalidResponse:TRtcNotifyEvent;

    // Internal variables
    FWritten:boolean;
    FWriteBuffer:TRtcHugeByteArray;

    procedure SetServer(const Value: TComponent);

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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
    procedure TriggerInvalidResponse; virtual;
    // @exclude
    procedure CallInvalidResponse; virtual;

    // @exclude
    procedure SetCliRequest(const Value: TRtcClientRequest); override;
    // @exclude
    procedure SetCliResponse(const Value: TRtcClientResponse); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function New:TRtcMessageClient;

    // @exclude
    procedure LeaveEvent; override;

    { Flush all buffered data.
      @html(<br>)
      When using 'Write' without calling 'WriteHeader' before, all data
      prepared by calling 'Write' will be buffered until your event
      returns to its caller (automatically upon your event completion) or
      when you first call 'Flush'. Flush will check if Request.ContentLength is set
      and if not, will set the content length to the number of bytes buffered.
      @html(<br>)
      Flush does nothing if WriteHeader was called for this response.

      @exclude}
    procedure Flush; override;

    // You can call WriteHeader to send the Request header out.
    procedure WriteHeader(SendNow:boolean=True); overload; override;
    { You can call WriteHeader with empty 'HeaderText' parameter to
      tell the component that you do not want any HTTP header to be sent. }
    procedure WriteHeader(const HeaderText:RtcString; SendNow:boolean=True); overload; override;

    // Use Write to send any Content (document body) out.
    procedure WriteEx(const s:RtcByteArray); override;
    // Use Write to send any Content (document body) out.
    procedure Write(const s:RtcString); override;

  published
    { Maximum allowed size of the first response line, without header (0 = no limit).
      This is the first line in a HTTP response and includes Response.StatusCode and Response.StatusText }
    property MaxResponseSize:cardinal read FMaxResponseSize write FMaxResponseSize default 0;
    { Maximum allowed size of each response's header size (0 = no limit).
      This are all the remaining header lines in a HTTP response,
      which come after the first line and end with an empty line,
      after which usually comes the content (document body). }
    property MaxHeaderSize:cardinal read FMaxHeaderSize write FMaxHeaderSize default 0;

    { This event will be called if the received response exceeds your defined
      maximum response or header size. If both values are 0, this event will never be called. }
    property OnInvalidResponse:TRtcNotifyEvent read FOnInvalidResponse write FOnInvalidResponse;

    { TRtcMsgServer or any other component implementing the IRTCMessageReceiver interface. }
    property Server:TComponent read FServer write SetServer;
    end;

implementation

uses
  rtcConnProv,
  rtcTransports,
  rtcMsgCliProv;
  
type
  TMyProvider = TRtcMessageClientProvider; // Message Client Provider

{ TRtcMessageClient }

constructor TRtcMessageClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FWriteBuffer:=TRtcHugeByteArray.Create;
  FWritten:=False;
  end;

destructor TRtcMessageClient.Destroy;
  begin
  try
    RtcFreeAndNil(FWriteBuffer);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcMessageClient.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

class function TRtcMessageClient.New: TRtcMessageClient;
  begin
  Result:=Create(nil);
  end;

function TRtcMessageClient.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TMyProvider.Create;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcMessageClient.SetParams;
  begin
  inherited;
  if assigned(Con) and (Con is TMyProvider) then
    begin
    TMyProvider(Con).Request:=Request;
    TMyProvider(Con).Response:=Response;
    TMyProvider(Con).MaxResponseSize:=MaxResponseSize;
    TMyProvider(Con).MaxHeaderSize:=MaxHeaderSize;
    TMyProvider(Con).FixupRequest:=FixupRequest;
    end;
  end;

procedure TRtcMessageClient.SetTriggers;
  var
    MR:IRTCMessageReceiver;
  begin
  inherited;
  if assigned(Con) and (Con is TMyProvider) then
    begin
    if not assigned(Server) then
      raise Exception.Create('Server NOT assigned!')
    else if Server.GetInterface(IRTCMessageReceiverGUID, MR) then
      TMyProvider(Con).Server:=MR
    else
      raise Exception.Create('Server does NOT support the IRTCMessageReceiver interface!');

    TMyProvider(Con).SetTriggerInvalidResponse(TriggerInvalidResponse);
    end;
  end;

procedure TRtcMessageClient.ClearTriggers;
  var
    MyCon:TMyProvider;
  begin
  if assigned(Con) and (Con is TMyProvider) then
    begin
    MyCon:=TMyProvider(Con);
    inherited;
    MyCon.SetTriggerInvalidResponse(nil);
    MyCon.Server:=nil;
    end;
  end;

procedure TRtcMessageClient.WriteHeader(SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Request.Active then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader;
    end;
  end;

procedure TRtcMessageClient.WriteHeader(const HeaderText: RtcString; SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Request.Active then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader(HeaderText);
    end;
  end;

procedure TRtcMessageClient.WriteEx(const s: RtcByteArray);
  begin
  if State>=conActive then
    begin
    if Request.Complete then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if Request.Active then
      begin
      { Header is out }

      if Request.ValueCS['CONTENT-LENGTH']<>'' then
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the RtcByteString out, without buffering }

      Con.WriteEx(s);
      end
    else
      begin
      if (Request.ValueCS['CONTENT-LENGTH']<>'') and not FWritten then
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Request.ContentLength - Request.ContentOut < length(s) then
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

procedure TRtcMessageClient.Write(const s: RtcString);
  begin
  if State>=conActive then
    begin
    if Request.Complete then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if Request.Active then
      begin
      { Header is out }

      if Request.ValueCS['CONTENT-LENGTH']<>'' then
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the RtcByteString out, without buffering }

      Con.Write(s);
      end
    else
      begin
      if (Request.ValueCS['CONTENT-LENGTH']<>'') and not FWritten then
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Request.ContentLength - Request.ContentOut < length(s) then
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

procedure TRtcMessageClient.Flush;
  var
    Temp:RtcByteArray;
  begin
  if State>=conActive then
    begin
    if not FWritten then
      Exit
    else
      FWritten:=False; // so we don't re-enter this method.

    Timeout.DataSending;

    if Request.Complete then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if not Request.Active then
      begin
      if Request.ValueCS['CONTENT-LENGTH']='' then // length not specified
        begin
        Request.AutoLength:=True;
        Request.ContentLength:=FWriteBuffer.Size;
        end;

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

procedure TRtcMessageClient.CallInvalidResponse;
  begin
  if assigned(OnInvalidResponse) then
    OnInvalidResponse(self);
  end;

procedure TRtcMessageClient.TriggerDataReceived;
  begin
  if State>=conActive then
    begin
    inherited;
    Flush;
    end;
  end;

procedure TRtcMessageClient.TriggerDataSent;
  begin
  if State>=conActive then
    begin
    if FWriteCount>0 then
      Timeout.DataSent;
    EnterEvent;
    try
      if FWriteCount>0 then
        begin
        CallDataSent;
        Flush;
        end;

      if not isClosing then
        begin
        CallReadyToSend;
        Flush;
        end;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcMessageClient.TriggerDataOut;
  begin
  if State>=conActive then
    begin
    inherited;
    Flush;
    end;
  end;

procedure TRtcMessageClient.TriggerInvalidResponse;
  begin
  if State>=conActive then
    begin
    EnterEvent;
    try
      CallInvalidResponse;
      Flush;

      Disconnect;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcMessageClient.SetCliRequest(const Value: TRtcClientRequest);
  begin
  inherited SetCliRequest(Value);
  if assigned(Con) then
    TMyProvider(Con).Request:=Request;
  end;

procedure TRtcMessageClient.SetCliResponse(const Value: TRtcClientResponse);
  begin
  inherited SetCliResponse(Value);
  if assigned(Con) then
    TMyProvider(Con).Response:=Response;
  end;

procedure TRtcMessageClient.LeaveEvent;
  begin
  inherited;
    if not InsideEvent then
      if assigned(Con) then
        TMyProvider(Con).LeavingEvent;
  end;

procedure TRtcMessageClient.SetServer(const Value: TComponent);
  var
    MR:IRTCMessageReceiver;
    MyCon:TMyProvider;
  begin
  if Value<>FServer then
    begin
    if not assigned(Value) then
      begin
      FServer:=nil;
      if assigned(Con) then
        begin
        MyCon:=TMyProvider(Con);
        MyCon.Server:=nil;
        end;
      end
    else if assigned(Value) then
      begin
      if Value.GetInterface(IRTCMessageReceiverGUID, MR) then
        begin
        FServer:=Value;
        if assigned(Con) then
          begin
          MyCon:=TMyProvider(Con);
          MyCon.Server:=MR;
          end;
        end
      else
        raise Exception.Create('Component does NOT support the IRTCMessageReceived interface!');
      end;
    end;
  end;

procedure TRtcMessageClient.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FServer then
      SetServer(nil);
  end;

end.
