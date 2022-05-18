{
  @html(<b>)
  ISAPI Server Connection
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcISAPIServer) @html(</b>) component: @html(<br>)
  Server connection component for ISAPI communication using HTTP requests.
  
  This component is ONLY for MS Windows.  
}
unit rtcISAPISrv;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WINDOWS}

uses
  Windows, Classes, SysUtils,

  {$IFNDEF FPC}isapi2,{$ENDIF}

  rtcTypes,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcConn,
  rtcDataSrv,

  rtcConnProv,
  rtcISAPISrvProv; // ISAPI Server Provider

type
  { @Abstract(ISAPI Server Connection component)

    Methods to check first:
    @html(<br>)
    @Link(TRtcDataServer.Request), @Link(TRtcConnection.Read) - Read client request
    @html(<br>)
    @Link(TRtcDataServer.Response), @Link(TRtcISAPIServer.WriteHeader), @Link(TRtcISAPIServer.Write) - Write result to client
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Module Loaded
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - new Client connected
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data available from client (check @Link(TRtcDataServer.Request))
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to client (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - one Client disconnected
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Module Unloading
    @html(<br><br>)

    Check @Link(TRtcDataServer), @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidWindows)]
  {$ENDIF}
  TRtcISAPIServer = class(TRtcDataServer)
  private
    FCS:TRtcCritSec;
    ConnPool:TRtcObjectList;

    FWritten:boolean;
    FWriteBuffer:TRtcHugeByteArray;
    FForce1Thread: boolean;

    function GetConnection:TRtcISAPIServer;
    procedure PutConnection(conn:TRtcISAPIServer);
    procedure FreeConnection(conn:TRtcISAPIServer);
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

    // This will always return TRUE for TRtcISAPIServer.
    function isExtension:boolean; override;

    // Direct Access to the "GetServerVariable" API on the Server
    function GetServerVariable(const VariableName:RtcString; Size:cardinal):RtcString;

    // @exclude
    procedure ExecuteRequest(var ECB: TEXTENSION_CONTROL_BLOCK);

    // @exclude
    class procedure Load;
    // @exclude
    class procedure UnLoad;
    // @exclude
    class function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD;

    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    // Constructor
    class function New:TRtcISAPIServer;

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

  published
    { Set "ForceSingleThread" to TRUE if you want your ISAPI extension to handle
      ONLY one request at a time. This is useful *ONLY* if you don't know how to
      write thread-safe code and are experiencing problems with your ISAPI extension.
      Setting this property to TRUE will force all clients to "stand in a line"
      and wait for their turn. If your ISAPI has to serve large files, it won't
      be capable of handling more than 1 client at a time, since most clients
      will disconnect themselves after a specific time-out period. }
    property ForceSingleThread:boolean read FForce1Thread write FForce1Thread default False;
    end;

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

type
  TMyProvider = TRtcISAPIServerProvider;

var
  MainISAPIServer:TRtcISAPIServer=nil;
  MainISAPICS:TRtcCritSec=nil;

{ TRtcISAPIServer }

constructor TRtcISAPIServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  // Make the first instance a global instance
  if not assigned(MainISAPIServer) then
    begin
    MainISAPIServer:=self;
    MainISAPICS:=TRtcCritSec.Create;
    end;

  FCS:=TRtcCritSec.Create;

  FWriteBuffer:=TRtcHugeByteArray.Create;
  FWritten:=False;

  FForce1Thread:=False;
  end;

destructor TRtcISAPIServer.Destroy;
  begin
  try
    // If this is the global instance, remove pointer
    if self=MainISAPIServer then
      begin
      MainISAPIServer:=nil;
      RtcFreeAndNil(MainISAPICS);
      end;

    CloseAllConnections;

    RtcFreeAndNil(FCS);
    RtcFreeAndNil(FWriteBuffer);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcISAPIServer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcISAPIServer.GetConnection:TRtcISAPIServer;
  begin
  if FForce1Thread then
    MainISAPICS.Acquire;
  try
    Result:=nil;

    FCS.Acquire;
    try
      if assigned(ConnPool) then
        begin
        if ConnPool.Count > 0 then
          begin
          Result:= TRtcISAPIServer(ConnPool.items[ConnPool.Count-1]);
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

      Result:=TRtcISAPIServer(self.copyOf);
      end;
  except
    if FForce1Thread then
      MainISAPICS.Release;
    raise;
    end;
  end;

procedure TRtcISAPIServer.PutConnection(conn:TRtcISAPIServer);
  begin
  try
    FCS.Acquire;
    try
      if not assigned(ConnPool) then
        ConnPool:=TRtcObjectList.Create;
      ConnPool.Add(conn);
    finally
      FCS.Release;
      end;
  finally
    if FForce1Thread then
      MainISAPICS.Release;
    end;
  end;

procedure TRtcISAPIServer.FreeConnection(conn:TRtcISAPIServer);
  begin
  try
    Conn.Release;
  finally
    if FForce1Thread then
      MainISAPICS.Release;
    end;
  end;

procedure TRtcISAPIServer.CloseAllConnections;
  var
    i    :integer;
    mycon  :TRtcISAPIServer;
  begin
  FCS.Acquire;
  try
    if assigned(ConnPool) then
      begin
      for i:= 0 to ConnPool.count - 1 do
        begin
        mycon:= TRtcISAPIServer(ConnPool.items[i]);
        mycon.Release;
        end;
      ConnPool.Clear;
      RtcFreeAndNil(ConnPool);
      end;
  finally
    FCS.Release;
    end;
  end;

class function TRtcISAPIServer.New: TRtcISAPIServer;
  begin
  Result:=Create(nil);
  end;

function TRtcISAPIServer.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TMyProvider.Create;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcISAPIServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);

  ForceSingleThread:=TRtcISAPIServer(Dup).ForceSingleThread;
  end;

procedure TRtcISAPIServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    begin
    TMyProvider(Con).FixupRequest:=FixupRequest;
    TMyProvider(Con).Request:=Request;
    TMyProvider(Con).Response:=Response;
    end;
  end;

function TRtcISAPIServer.GetServerVariable(const VariableName:RtcString; Size:cardinal):RtcString;
  begin
  if assigned(Con) then
    Result:=TMyProvider(Con).ServerVariable(VariableName,Size)
  else
    Result:='';
  end;

procedure TRtcISAPIServer.WriteHeader(SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader;
    end;
  end;

procedure TRtcISAPIServer.WriteHeader(const HeaderText: RtcString; SendNow:boolean=True);
  begin
  if State>=conActive then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader(HeaderText);
    end;
  end;

procedure TRtcISAPIServer.WriteEx(const s: RtcByteArray);
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
        WriteHeader;
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

procedure TRtcISAPIServer.Write(const s: RtcString);
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
        WriteHeader;
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

procedure TRtcISAPIServer.Flush;
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

procedure TRtcISAPIServer.TriggerDataReceived;
  begin
  inherited;
  Flush;
  end;

procedure TRtcISAPIServer.TriggerDataSent;
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

procedure TRtcISAPIServer.TriggerDataOut;
  begin
  inherited;
  Flush;
  end;

procedure TRtcISAPIServer.SetSrvRequest(const Value: TRtcServerRequest);
  begin
  inherited SetSrvRequest(Value);
  if assigned(Con) then
    TMyProvider(Con).Request:=Request;
  end;

procedure TRtcISAPIServer.SetSrvResponse(const Value: TRtcServerResponse);
  begin
  inherited SetSrvResponse(Value);
  if assigned(Con) then
    TMyProvider(Con).Response:=Response;
  end;

class function TRtcISAPIServer.HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD;
  var
    Server:TRtcISAPIServer;
  begin
  if assigned(MainISAPIServer) then
    begin
    Result:=HSE_STATUS_SUCCESS;

    Server:=MainISAPIServer.GetConnection;
    try
      Server.EnterEvent;
      try
        Server.ExecuteRequest(ECB);
        if not Server.Response.Sent then
          raise Exception.Create('Response not sent! Need to send complete response from ISAPI.'); // Result:=HSE_STATUS_PENDING;
      finally
        Server.LeaveEvent;
        end;
    except
      on E:Exception do
        begin
        { If an exception happens, we do not want to keep the object.
          This is to avoid future problems with this object,
          since it could now be in a "limb" state. }
        MainISAPIServer.FreeConnection(Server);
        raise;
        end;
      end;
    MainISAPIServer.PutConnection(Server);
    end
  else
    raise Exception.Create('No ISAPI Server component found.');
  end;

procedure TRtcISAPIServer.ExecuteRequest(var ECB: TEXTENSION_CONTROL_BLOCK);
  begin
  TMyProvider(Con).Connect(ECB);
  TMyProvider(Con).ExecuteRequest;
  end;

class procedure TRtcISAPIServer.Load;
  begin
  if assigned(MainISAPIServer) then
    MainISAPIServer.Listen;
  end;

class procedure TRtcISAPIServer.UnLoad;
  begin
  if assigned(MainISAPIServer) then
    MainISAPIServer.StopListen;
  end;

function TRtcISAPIServer.isExtension: boolean;
  begin
  Result:=True;
  end;

function GetModuleName(Module: HMODULE): String;
  var
    ModName: array[0..MAX_PATH] of Char;
  begin
  SetString(Result, ModName, GetModuleFileName(Module, ModName, SizeOf(ModName)));
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcISAPISrv Initializing ...','DEBUG');{$ENDIF}

AppFileName:=ExpandUNCFileName(GetModuleName(HInstance));

{$IFDEF RTC_DEBUG} Log('rtcISAPISrv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcISAPISrv Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF WINDOWS}
end.