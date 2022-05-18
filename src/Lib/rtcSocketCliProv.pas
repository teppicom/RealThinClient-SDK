{
  "Client Socket Connection Provider"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)

  @exclude
}
unit rtcSocketCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcThrPool,
  rtcLog,

  rtcPlugins,
  rtcConnProv,
  rtcSockBase;

type
  TRtcBaseSockClientProvider = class(TRtcThrClientProvider)
  private
    FCryptPlugin: TRtcCryptPlugin;
    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;

  protected
    FCryptObject:TObject;

    procedure CleanUp; override;

  public
    constructor Create; override;

    property CryptPlugin        : TRtcCryptPlugin   read FCryptPlugin
                                                    write FCryptPlugin;

    property TimeoutsOfAPI:TRtcTimeoutsOfAPI read FTimeoutsOfAPI write FTimeoutsOfAPI;

    property CryptObject:TObject read FCryptObject;
    end;

  TRtcSocketClientProvider = class;

  TRtcSocketClientProtocol = (proTCP, proUDP);

  TRtcSocketClientThread = class(TRtcThread)
  public
    RtcConn: TRtcSocketClientProvider;
    Releasing: boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenConn;
    procedure ReOpenConn;
    procedure CloseConn;

    function RunJob:boolean; override;
    end;

  TRtcSocketClientProvider = class(TRtcBaseSockClientProvider)
  private
    Conn:TRtcSocketBase;

    FSocketClass:TRtcSocketBaseClass;

    FProtocol: TRtcSocketClientProtocol;

    FRawOut,
    FPlainOut:int64;

    FReadBuff,
    FReadingBuffer:RtcByteArray;

    Client_Thread : TRtcSocketClientThread;

    FMultiCast          : Boolean;
    FMultiCastIpTTL     : Integer;
    FReuseAddr          : Boolean;

    FRealNeedData:boolean;

    FBlocking: boolean;

    procedure wsOnBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure wsOnChangeState(Sender: TObject; NewState: TRtcSocketState);

    procedure wsOnSessionClosed(Sender: TObject; ErrorCode: Word);

    procedure wsOnDataReceived(Sender: TObject; ErrCode: Word);
    procedure wsOnDataSent(Sender: TObject; ErrCode: Word);

    procedure wsOnDataOut(Sender: TObject; Len: cardinal);
    procedure wsOnDataIn(Sender: TObject; Len: cardinal);

    procedure OpenConnection(Force:boolean;Reconnecting:boolean);

  protected

    procedure CleanUp; override;

    function _Active:boolean;
    function _Visible:boolean;

    function ClientThreadNIL:boolean;
    function GetClientThread:TRtcThread; override;

    procedure CryptNeedMoreData;

    procedure DirectWriteEx(const s:RtcByteArray);
    procedure BufferWriteEx(const s:RtcByteArray);

    procedure DirectWrite(const s:RtcString);
    procedure BufferWrite(const s:RtcString);

    function CryptPluginError:boolean; virtual;
    function GetCryptProtocol:TRtcCryptPluginProtocol; virtual;
    procedure SetDataProtocol(res:TRtcCryptPluginState); virtual;

  public
    constructor Create; override;

    procedure NeedMoreData;

    function ReleaseMe(FromInside:boolean):boolean; override;

    procedure Connect(Force:boolean=False;Reconnecting:boolean=False); override;

    procedure InternalDisconnect; override;
    procedure Disconnect; override;

    procedure Check; override;

    function ReadEx: RtcByteArray; override;
    procedure WriteEx(const s: RtcByteArray; sendNow:boolean=True); override;

    function PeekEx: RtcByteArray; override;
    procedure PokeEx(const s: RtcByteArray); override;

    function Read: RtcString; override;
    procedure Write(const s: RtcString; sendNow:boolean=True); override;

    property Blocking:boolean read FBlocking write FBlocking;

    property SocketClass:TRtcSocketBaseClass read FSocketClass write FSocketClass;

    property Proto:TRtcSocketClientProtocol read FProtocol write FProtocol;

    property UdpMultiCast       : Boolean           read  FMultiCast
                                                    write FMultiCast;
    property UdpMultiCastMaxHops: Integer           read  FMultiCastIpTTL
                                                    write FMultiCastIpTTL;
    property UdpReuseAddr       : Boolean           read  FReuseAddr
                                                    write FReuseAddr;
    end;

implementation

{ TRtcBaseSockClientProvider }

constructor TRtcBaseSockClientProvider.Create;
  begin
  inherited;

  FCryptObject:=nil;

  FPeerPort:='';
  FPeerAddr:='';
  FLocalPort:='';
  FLocalAddr:='';
  end;

procedure TRtcBaseSockClientProvider.CleanUp;
  begin
  try
    try
      FPeerPort:='';
      FPeerAddr:='';
      FLocalPort:='';
      FLocalAddr:='';
    finally
      inherited;
    end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcBaseSockClientProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcWSockClientThread }

var
  Message_WSStop,
  Message_WSOpenConn,
  Message_WSReOpenConn,
  Message_WSCloseConn,
  Message_WSRelease:TRtcBaseMessage;

{ TRtcWSockClientProvider }

constructor TRtcSocketClientProvider.Create;
  begin
  inherited;

  FBlocking:=False;
  SocketClass:=nil;

  FRawOut:=0;
  FPlainOut:=0;

  Closing:=False;

  FPeerPort:='';
  FPeerAddr:='';
  FLocalPort:='';
  FLocalAddr:='';

  FProtocol:=proTCP;
  UdpMultiCastMaxHops:=1;

  SetLength(FReadBuff,0);
  SetLength(FReadingBuffer,0);

  Conn:=nil;
  end;

function TRtcSocketClientProvider.ClientThreadNIL:boolean;
  begin
  Result:=Client_Thread=nil;
  end;

procedure TRtcSocketClientProvider.CleanUp;
  begin
  try
    { Before destroying this connection object,
      we will disconnect this and all related open connections. }
    try
      Closing:=True;
      Silent:=True;
      if assigned(Conn) then InternalDisconnect;
      TriggerConnectionClosing;

      if inThread then
        begin
        if not ReleaseMe(True) then
          Log('TRtcSocketClientProvider.CleanUp - ReleaseMe(TRUE)?','ERROR');
        end
      else
        begin
        if not ReleaseMe(False) then
          Log('TRtcSocketClientProvider.CleanUp - ReleaseMe(FALSE)?','ERROR');
        end;
    finally
      try
        inherited;
      finally
        SetLength(FReadBuff,0);
        SetLength(FReadingBuffer,0);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketClientProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcSocketClientProvider.CryptPluginError:boolean;
  begin
  Result:=True;
  Lost:=True;
  InternalDisconnect;
  //raise Exception.Create('Encryption error.');
  end;

procedure TRtcSocketClientProvider.PokeEx(const s:RtcByteArray);
  begin
  if assigned(Conn) then
    FReadingBuffer:=s;
  end;

function TRtcSocketClientProvider.PeekEx: RtcByteArray;
  var
    s_in, s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not assigned(Conn) then
    begin
    SetLength(FReadBuff,0);
    SetLength(FReadingBuffer,0);
    SetLength(Result,0);
    end
  else if length(FReadingBuffer)>0 then
    Result:=FReadingBuffer
  else if (Proto=proTCP) and (State=conActive) then
    begin
    if assigned(CryptPlugin) then
      begin
      SetLength(Result,0);
      SetLength(s_in,0);
      if Conn.ReceiveEx(s_in)>0 then
        begin
        // Decrypt input data ...
        SetLength(s_out,0);
        try
          s_res:=CryptPlugin.DataReceivedEx(FCryptObject, s_in, s_out, Result);
        except
          on E:Exception do
            begin
            if LOG_PLUGIN_ERRORS then
              Log('TRtcSocketClientProvider CryptPlugin.DataReceivedEx',E,'PLUGIN');
            s_res:=cpsClosed;
            end;
          end;
        if s_res=cpsClosed then
          if CryptPluginError then Exit;
        if length(Result)>0 then
          begin
          FRealNeedData:=False;
          // Trigger the "OnDataIn" event ...
          FDataIn:=length(Result);
          TriggerDataIn;
          end;
        if length(s_out)>0 then
          DirectWriteEx(s_out);
        if (s_res=cpsWaiting) or FRealNeedData then
          CryptNeedMoreData;
        end;
      end
    else
      begin
      SetLength(Result,0);
      if Conn.ReceiveEx(Result)>0 then
        FRealNeedData:=False;
      end;
    FReadingBuffer:=Result;
    end
  else
    begin
    Result:=FReadBuff;
    SetLength(FReadBuff,0);
    FReadingBuffer:=Result;
    end;
  end;

function TRtcSocketClientProvider.ReadEx: RtcByteArray;
  var
    s_in, s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not assigned(Conn) then
    begin
    SetLength(FReadBuff,0);
    SetLength(FReadingBuffer,0);
    SetLength(Result,0);
    end
  else if length(FReadingBuffer)>0 then
    begin
    Result:=FReadingBuffer;
    SetLength(FReadingBuffer,0);
    end
  else if (Proto=proTCP) and (State=conActive) then
    begin
    if assigned(CryptPlugin) then
      begin
      SetLength(Result,0);
      SetLength(s_in,0);
      if Conn.ReceiveEx(s_in)>0 then
        begin
        // Decrypt input data ...
        SetLength(s_out,0);
        try
          s_res:=CryptPlugin.DataReceivedEx(FCryptObject, s_in, s_out, Result);
        except
          on E:Exception do
            begin
            if LOG_PLUGIN_ERRORS then
              Log('TRtcSocketClientProvider CryptPlugin.DataReceivedEx',E,'PLUGIN');
            s_res:=cpsClosed;
            end;
          end;
        if s_res=cpsClosed then
          if CryptPluginError then Exit;
        if length(Result)>0 then
          begin
          FRealNeedData:=False;
          // Trigger the "OnDataIn" event ...
          FDataIn:=length(Result);
          TriggerDataIn;
          end;
        if length(s_out)>0 then
          DirectWriteEx(s_out);
        if (s_res=cpsWaiting) or FRealNeedData then
          CryptNeedMoreData;
        end;
      end
    else
      begin
      SetLength(Result,0);
      if Conn.ReceiveEx(Result)>0 then
        FRealNeedData:=False;
      end;
    end
  else
    begin
    Result:=FReadBuff;
    SetLength(FReadBuff,0);
    end;
  end;

function TRtcSocketClientProvider.Read: RtcString;
  var
    s_in, s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
    Res:RtcByteArray;
  begin
  if not assigned(Conn) then
    begin
    SetLength(FReadBuff,0);
    SetLength(FReadingBuffer,0);
    SetLength(Result,0);
    SetLength(Res,0);
    end
  else if length(FReadingBuffer)>0 then
    begin
    Result:=RtcBytesToString(FReadingBuffer);
    SetLength(FReadingBuffer,0);
    end
  else if (Proto=proTCP) and (State=conActive) then
    begin
    if assigned(CryptPlugin) then
      begin
      SetLength(Result,0);
      SetLength(s_in,0);
      if Conn.ReceiveEx(s_in)>0 then
        begin
        // Decrypt input data ...
        SetLength(s_out,0); SetLength(Res,0);
        try
          s_res:=CryptPlugin.DataReceivedEx(FCryptObject, s_in, s_out, Res);
        except
          on E:Exception do
            begin
            if LOG_PLUGIN_ERRORS then
              Log('TRtcSocketClientProvider CryptPlugin.DataReceivedEx',E,'PLUGIN');
            s_res:=cpsClosed;
            end;
          end;
        if s_res=cpsClosed then
          if CryptPluginError then Exit;
        if length(Res)>0 then
          begin
          FRealNeedData:=False;
          // Trigger the "OnDataIn" event ...
          FDataIn:=length(Res);
          TriggerDataIn;
          end;
        if length(s_out)>0 then
          DirectWriteEx(s_out);
        if (s_res=cpsWaiting) or FRealNeedData then
          CryptNeedMoreData;
        Result:=RtcBytesToString(Res);
        SetLength(Res,0);
        end;
      end
    else
      begin
      SetLength(Result,0);
      if Conn.ReceiveStr(Result)>0 then
        FRealNeedData:=False;
      end;
    end
  else
    begin
    Result:=RtcBytesToString(FReadBuff);
    SetLength(FReadBuff,0);
    end;
  end;

procedure TRtcSocketClientProvider.WriteEx(const s: RtcByteArray; SendNow:boolean=True);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    begin
    SetLength(s_out,0);
    try
      s_res:=CryptPlugin.DataToSendEx(FCryptObject, s, s_out);
    except
      on E:Exception do
        begin
        if LOG_PLUGIN_ERRORS then
          Log('TRtcSocketClientProvider CryptPlugin.DataToSend',E,'PLUGIN');
        s_res:=cpsClosed;
        end;
      end;
    if s_res=cpsClosed then
      if CryptPluginError then Exit;
    Inc(FPlainOut, length(s));
    if length(s_out)>0 then
      DirectWriteEx(s_out);
    if s_res=cpsWaiting then
      CryptNeedMoreData;
    end
  else if SendNow then
    DirectWriteEx(s)
  else
    BufferWriteEx(s);
  end;

procedure TRtcSocketClientProvider.Write(const s: RtcString; SendNow:boolean=True);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    begin
    SetLength(s_out,0);
    try
      s_res:=CryptPlugin.DataToSendEx(FCryptObject, RtcStringToBytes(s), s_out);
    except
      on E:Exception do
        begin
        if LOG_PLUGIN_ERRORS then
          Log('TRtcSocketClientProvider CryptPlugin.DataToSend',E,'PLUGIN');
        s_res:=cpsClosed;
        end;
      end;
    if s_res=cpsClosed then
      if CryptPluginError then Exit;
    Inc(FPlainOut, length(s));
    if length(s_out)>0 then
      DirectWriteEx(s_out);
    if s_res=cpsWaiting then
      CryptNeedMoreData;
    end
  else if SendNow then
    DirectWrite(s)
  else
    BufferWrite(s);
  end;

procedure TRtcSocketClientProvider.DirectWriteEx(const s: RtcByteArray);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.SendEx(s);
  end;

procedure TRtcSocketClientProvider.DirectWrite(const s: RtcString);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.SendStr(s);
  end;

procedure TRtcSocketClientProvider.BufferWriteEx(const s: RtcByteArray);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.BuffEx(s);
  end;

procedure TRtcSocketClientProvider.BufferWrite(const s: RtcString);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.BuffStr(s);
  end;

procedure TRtcSocketClientProvider.wsOnChangeState(Sender: TObject; NewState: TRtcSocketState);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if Closing then Exit;

  if assigned(Conn) then
    if NewState=wsConnected then
      begin
      if Proto=proTCP then
        begin
        FLocalAddr:=Conn.GetLocalAddr;
        if not NoIP(FLocalAddr) then
          begin
          FLocalPort:=Conn.GetLocalPort;
          FPeerAddr:=Conn.GetPeerAddr;
          FPeerPort:=Conn.GetPeerPort;
          TriggerConnecting;
          end;
        end
      else
        begin
        FLocalAddr:='127.0.0.1';
        FLocalPort:=Conn.GetLocalPort;
        FPeerAddr:=Conn.GetPeerAddr;
        FPeerPort:=Conn.GetPeerPort;
        TriggerConnecting;

        FDataCrypt:=assigned(CryptPlugin);

        if FDataCrypt then
          begin
          State:=conHandshake;
          FDataProtocol:=cppTcp;

          SetLength(s_out,0);
          try
            s_res:=CryptPlugin.AfterConnectEx(FCryptObject, s_out, GetCryptProtocol, GetAddr, PeerAddr);
          except
            on E:Exception do
              begin
              if LOG_PLUGIN_ERRORS then
                Log('TRtcSocketClientProvider CryptPlugin.AfterConnect',E,'PLUGIN');
              s_res:=cpsClosed;
              end;
            end;

          if s_res>=cpsReady then
            begin
            State:=conActive;
            SetDataProtocol(s_res);
            end
          else if s_res=cpsClosed then
            if CryptPluginError then Exit;

          if length(s_out)>0 then
            begin
            DirectWriteEx(s_out);
            SetLength(s_out,0);
            end;
          if s_res=cpsWaiting then
            CryptNeedMoreData;
          end
        else
          begin
          State:=conActive;
          SetDataProtocol(cpsReady);
          end;

        if State=conActive then
          TriggerConnect;
        end;
      end
    else if NewState=wsClosed then
      wsOnSessionClosed(Sender, 0)
    else if NewState=wsConnectLost then
      begin
      Lost:=True;
      InternalDisconnect;
      end
    else if NewState=wsConnectError then
      begin
      Lost:=True;
      InternalDisconnect;
      end;
  end;

procedure TRtcSocketClientProvider.wsOnSessionClosed(Sender: TObject; ErrorCode:Word);
  var
    Ex:Exception;
  begin
  { Client connection closed.

    This method is called when one of the active connections get closed.
    It handles connections closing for all active connection types
    (incomming and outgoing connections). }

  TriggerDisconnecting;

  if assigned(Conn) and not Closing then // Connection object still here ?
    begin
    Closing:=True; // Let everyone know we are closing now ...

    try
      TriggerConnectionClosing;

      if (State in [conHandshake,conActive,conClosing]) then // Connection was active
        begin
        if assigned(CryptPlugin) then
          try
            CryptPlugin.AfterDisconnectEx(FCryptObject);
          except
            on E:Exception do
              if LOG_PLUGIN_ERRORS then
                Log('TRtcSocketClientProvider CryptPlugin.AfterDisconnect',E,'PLUGIN');
            end;
        if Lost then
          TriggerConnectLost // TriggerConnectLost will call TriggerDisconnect
        else
          TriggerDisconnect;
        end
      else
        begin
        if Lost then
          begin
          if Conn.GetLastErrorText<>'' then
            begin
            Ex:=ERtcSocketError.Create(Conn.GetLastErrorText);
            try
              TriggerConnectError(Ex);
            finally
              Ex.Free;
              end;
            end
          else
            TriggerConnectFail; // we have a "Failed connection" here, rather then a Disconnect.
          end
        else
          TriggerDisconnect;
        end;
    finally
      State:=conClosing;

      if assigned(Conn) then
        begin
        { We need to remove all events from this connection
          before we can actually destroy our own connection object. }
        Conn.EventsOff:=True;

        try
          Conn.Close;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketClientProvider.OnSessionClosed: Conn.Close',E,'ERROR'); // ignore all errors here
          end;
        try
          Conn.Release;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketClientProvider.OnSessionClosed: Conn.Release',E,'ERROR'); // ignore all errors here
          end;
        Conn:=nil;
        end;

      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnDataReceived(Sender: TObject; ErrCode: Word);
  var
    len:integer;
    s_in,s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if _Visible then
    begin
    if State=conActivating then // call "Connected" only after we know that we can relly send data.
      begin
      if not NoIP(FLocalAddr) then
        begin
        FDataCrypt:=assigned(CryptPlugin);

        if FDataCrypt then
          begin
          State:=conHandshake;
          FDataProtocol:=cppTcp;

          SetLength(s_out,0);
          try
            s_res:=CryptPlugin.AfterConnectEx(FCryptObject, s_out, GetCryptProtocol, GetAddr, GetPeerAddr);
          except
            on E:Exception do
              begin
              if LOG_PLUGIN_ERRORS then
                Log('CryptPlugin.AfterConnect',E,'PLUGIN');
              s_res:=cpsClosed;
              end;
            end;

          if s_res>=cpsReady then
            begin
            State:=conActive;
            SetDataProtocol(s_res);
            end
          else if s_res=cpsClosed then
            if CryptPluginError then
              begin
              TriggerReadyToRelease;
              Exit;
              end;

          if length(s_out)>0 then
            begin
            DirectWriteEx(s_out);
            SetLength(s_out,0);
            end;
          if s_res=cpsWaiting then
            CryptNeedMoreData;
          end
        else
          begin
          State:=conActive;
          SetDataProtocol(cpsReady);
          end;

        if State=conActive then
          TriggerConnect;
        end;
      end;

    if State=conHandshake then // call "Connected" only after the encryption handshake
      begin
      SetLength(s_in,0);
      if Conn.ReceiveEx(s_in)>0 then
        begin
        // Decrypt input data ...
        SetLength(FReadingBuffer,0);
        SetLength(s_out,0);
        try
          s_res:=CryptPlugin.DataReceivedEx(FCryptObject, s_in, s_out, FReadingBuffer);
        except
          on E:Exception do
            begin
            if LOG_PLUGIN_ERRORS then
              Log('TRtcSocketClientProvider CryptPlugin.DataReceivedEx',E,'PLUGIN');
            s_res:=cpsClosed;
            end;
          end;

        if s_res>=cpsReady then
          begin
          State:=conActive;
          SetDataProtocol(s_res);
          end
        else if s_res=cpsClosed then
          if CryptPluginError then Exit;

        if length(s_out)>0 then
          DirectWriteEx(s_out);
        if s_res=cpsWaiting then
          CryptNeedMoreData;

        if State=conActive then
          begin
          TriggerConnect;
          if length(FReadingBuffer)>0 then
            begin
            FRealNeedData:=False;
            // Trigger the "OnDataIn" event ...
            FDataIn:=length(FReadingBuffer);
            TriggerDataIn;
            end;
          end;
        end;
      end;

    if State=conActive then
      begin
      if Proto=proUDP then
        begin
        len:=Conn.ReceiveEx(FReadBuff);
        if (len>=0) then
          begin
          FPeerPort:=Conn.GetPeerPort;
          FPeerAddr:=Conn.GetPeerAddr;
          TriggerDataReceived;
          TriggerReadyToRelease;
          end
        else
          begin
          SetLength(FReadBuff,0);
          SetLength(FReadingBuffer,0);
          TriggerDataLost;
          TriggerReadyToRelease;
          end;
        end
      else
        begin
        TriggerDataReceived;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnDataSent(Sender: TObject; ErrCode: Word);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if _Visible then
    begin
    if State=conActivating then // call "Connected" only after we know that we can relly send data.
      begin
      if not NoIP(FLocalAddr) then
        begin
        FDataCrypt:=assigned(CryptPlugin);

        if FDataCrypt then
          begin
          State:=conHandshake;
          FDataProtocol:=cppTcp;

          SetLength(s_out,0);
          try
            s_res:=CryptPlugin.AfterConnectEx(FCryptObject, s_out, GetCryptProtocol, GetAddr, GetPeerAddr);
          except
            on E:Exception do
              begin
              if LOG_PLUGIN_ERRORS then
                Log('CryptPlugin.AfterConnect',E,'PLUGIN');
              s_res:=cpsClosed;
              end;
            end;

          if s_res>=cpsReady then
            begin
            State:=conActive;
            SetDataProtocol(s_res);
            end
          else if s_res=cpsClosed then
            if CryptPluginError then
              begin
              TriggerReadyToRelease;
              Exit;
              end;

          if length(s_out)>0 then
            begin
            DirectWriteEx(s_out);
            SetLength(s_out,0);
            end;
          if s_res=cpsWaiting then
            CryptNeedMoreData;
          end
        else
          begin
          State:=conActive;
          SetDataProtocol(cpsReady);
          end;

        if State=conActive then
          TriggerConnect;
        end;
      end;

    if State=conActive then // do not call this when it comes for the first time, if we haven't been sending anything out yet.
      begin
      TriggerDataSent;
      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnDataOut(Sender: TObject; Len: cardinal);
  begin
  if _Visible then
    begin
    if State>conActivating then
      begin
      if assigned(CryptPlugin) then
        begin
        Dec(FRawOut,Len);
        if (FRawOut=0) {and (FPlainOut>0)} then
          begin
          FDataOut:=FPlainOut;
          FPlainOut:=0;
          try
            TriggerDataOut;
          finally
            FDataOut:=0;
            end;
          end;
        TriggerReadyToRelease;
        end
      else
        begin
        FDataOut:=Len;
        try
          TriggerDataOut;
        finally
          FDataOut:=0;
          end;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnDataIn(Sender: TObject; Len: cardinal);
  begin
  if _Visible then
    begin
    if State>conActivating then
      begin
      if not assigned(CryptPlugin) then
        begin
        FDataIn:=Len;
        TriggerDataIn;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnBgException(Sender: TObject; E: Exception;
    var CanClose: Boolean);
  begin
  if (E is EClientLimitReached) then
    CanClose:=False
  else
    begin
    CanClose:=True;
    try
      TriggerException(E);
    except
      on E:Exception do
        if LOG_EVENT_ERRORS then
          Log('TRtcSocketClientProvider.OnBgException: TriggerException',E,'ERROR');
        // ignore all exceptions here
      end;
    end;
  end;

function TRtcSocketClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState>=conActivating) and assigned(Conn);
  end;

function TRtcSocketClientProvider._Visible: boolean;
  begin
  Result:=not Closing and (FState>=conActivating) and assigned(Conn);
  end;

procedure TRtcSocketClientProvider.Check;
  var
    addr:RtcString;
  begin
  if assigned(Conn) then
    begin
    addr:=Conn.GetLocalAddr;
    if NoIP(addr) then
      begin
      if LOG_SOCKET_ERRORS then
        Log('CLOSING from Check. Socket not connected to local address.','SOCK');
      Conn.Close;
      raise ERtcSocketError.Create('Socket not connected to local address.');
      end;
    addr:=Conn.GetPeerAddr;
    if NoIP(addr) then
      begin
      if LOG_SOCKET_ERRORS then
        Log('CLOSING from Check. Socket not connected to peer address.','SOCK');
      Conn.Close;
      raise ERtcSocketError.Create('Socket not connected to peer address.');
      end;
    end;
  end;

function TRtcSocketClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcSocketClientProvider.Connect(Force:boolean=False;Reconnecting:boolean=False);
  begin
  if Reconnecting then
    begin
    if assigned(Client_Thread) then
      begin
      if inThread then
        OpenConnection(Force,True)
      else
        TRtcThread.PostJob(Client_Thread, Message_WSReOpenConn);
      end
    else if GetMultiThreaded then
      begin
      Client_Thread := TRtcSocketClientThread.Create;
      Client_Thread.RtcConn:= self;
      TRtcThread.PostJob(Client_Thread, Message_WSReOpenConn);
      end
    else
      OpenConnection(Force,True);
    end
  else
    begin
    if assigned(Client_Thread) then
      begin
      if inThread then
        OpenConnection(Force,False)
      else
        TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else if GetMultiThreaded then
      begin
      Client_Thread := TRtcSocketClientThread.Create;
      Client_Thread.RtcConn:= self;
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else
      OpenConnection(Force,False);
    end;
  end;

procedure TRtcSocketClientProvider.OpenConnection(Force:boolean;Reconnecting:boolean);
  begin
  if State>=conActivating then Exit; // already connected !!!

  if not assigned(SocketClass) then
    raise Exception.Create('TRtcSocketClientProvider -> SocketClass not assigned!');

  try
    if Proto=proUDP then
      begin
      SetLength(FReadBuff,0);
      SetLength(FReadingBuffer,0);
      end;

    FRealNeedData:=False;
    Lost:=True;
    Closing:=False;
    Silent:=False;
    FRawOut:=0;
    FPlainOut:=0;
    FDataOut:=0;
    FDataIn:=0;
    FLocalAddr:='';
    FPeerAddr:='';

    TriggerConnectionOpening(Force);

    try
      Conn:=SocketClass.Create;
      Conn.Blocking:=Blocking;
      Conn.Reconnecting:=Reconnecting;
      Conn.MessageThread:=Client_Thread;

      Conn.TimeoutsOfAPI:=TimeoutsOfAPI;
      with Conn do
        begin
        case Proto of
          proTCP:Protocol:=spTcp;
          proUDP:
            begin
            Protocol:=spUdp;
            UdpMultiCast:=Self.UdpMultiCast;
            UdpMultiCastIpTTL:=Self.UdpMultiCastMaxHops;
            UdpReuseAddr:=Self.UdpReuseAddr;
            end;
          end;

        Addr:=self.GetAddr;
        Port:=self.GetPort;
        PreferIP4:=self.GetIPV in [rtc_IPvOS4, rtc_IPv4];
        PreferIPDef:=self.GetIPV in [rtc_IPvOS4, rtc_IPvOS6];

        MultiThreaded:=assigned(Client_Thread);

        OnBgException:=wsOnBgException;
        OnChangeState:=wsOnChangeState;

        OnDataReceived:=wsOnDataReceived;
        OnDataSent:=wsOnDataSent;
        OnDataOut:=wsOnDataOut;
        OnDataIn:=wsOnDataIn;
        end;

      try
        State:=conActivating;
        Conn.Connect;
      except
        on E:Exception do
          begin
          State:=conClosing;
          if _Active then
            begin
            try
              Conn.EventsOff:=True;
              Conn.Release;
            finally
              Conn:=nil;
              end;
            end;
          raise;
          end;
        end;
    except
      TriggerConnectionClosing;
      raise;
      end;
  except
    on E:EClientLimitReached do // connection limit reached
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:ERtcSocketError do // normal (expected) socket exception
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:ERtcFatalSockException do // Fatal socket exception
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:Exception do
      begin
      TriggerReadyToRelease;
      raise;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.Disconnect;
  begin
  if assigned(Client_Thread) and not inThread then
    TRtcThread.PostJob(Client_Thread, Message_WSCloseConn)
  else
    begin
    Lost:=False;
    InternalDisconnect;
    end;
  end;

procedure TRtcSocketClientProvider.InternalDisconnect;
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not assigned(Conn) then // not connected
    begin
    Closing:=True;
    Exit; // silent exit if nothing to do.
    end;

  if State>=conActivating then
    begin
    if State>conActivating then
      State:=conClosing
    else
      State:=conInactive;

    Conn.EventsOff:=True; // deactivate all events for this client connection

    if not Closing then
      begin
      if assigned(CryptPlugin) then
        begin
        SetLength(s_out,0);
        s_res:=cpsClosed;
        try
          s_res:=CryptPlugin.BeforeDisconnectEx(FCryptObject, s_out);
        except
          on E:Exception do
            if LOG_PLUGIN_ERRORS then
              begin
              Log('CryptPlugin.BeforeDisconnect',E,'PLUGIN');
              s_res:=cpsClosed;
              end;
          end;

        if not Lost then
          begin
          if length(s_out)>0 then
            begin
            DirectWriteEx(s_out);
            SetLength(s_out,0);
            end;
          if s_res=cpsWaiting then
            CryptNeedMoreData;
          end
        else
          SetLength(s_out,0);
        end;
      wsOnSessionClosed(self,0);
      end
    else
      begin
      if assigned(CryptPlugin) then
        try
          CryptPlugin.AfterDisconnectEx(FCryptObject);
        except
          on E:Exception do
            if LOG_PLUGIN_ERRORS then
              begin
              Log('CryptPlugin.AfterDisconnect',E,'PLUGIN');
              end;
          end;
      try
        Conn.Close;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketClientProvider.InternalDisconnect: Conn.Close',E,'ERROR'); // ignore all errors here
        end;
      try
        Conn.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketClientProvider.InternalDisconnect: Conn.Release',E,'ERROR'); // ignore all errors here
        end;
      Conn:=nil;
      end;
    end;
  end;

function TRtcSocketClientProvider.ReleaseMe(FromInside:boolean):boolean;
  begin
  Result:=False;
  if assigned(Client_Thread) then
    begin
    if FromInside then
      begin
      Client_Thread.Releasing:=True;
      Client_Thread.InternalKill;
      Client_Thread:=nil;
      end
    else if Silent then
      begin
      TRtcThread.PostJob(Client_Thread, Message_WSStop, True, True);
      if RtcWaitFor(ClientThreadNIL,ClientThreadNIL,RTC_WAITFOR_RELEASE_PROVIDER)<>wait_OK then
        if LOG_EXCEPTIONS then
          Log(RtcString(ClassName)+'.ReleaseMe ('+PeerAddr+':'+PeerPort+') TIMEOUT!','ERROR');
      Sleep(RTC_WAITFOR_PROVIDER_FINISH);
      Result:=True;
      end
    else
      TRtcThread.PostJob(Client_Thread, Message_WSRelease, True);
    end
  else
    Result:=inherited ReleaseMe(FromInside);
  end;

constructor TRtcSocketClientThread.Create;
  begin
  inherited;
  Releasing:=False;
  RtcConn:=nil;
  end;

procedure TRtcSocketClientThread.OpenConn;
  begin
  if assigned(RtcConn) and not Releasing then
    RtcConn.OpenConnection(False,False);
  end;

procedure TRtcSocketClientThread.ReOpenConn;
  begin
  if assigned(RtcConn) and not Releasing then
    RtcConn.OpenConnection(False,True);
  end;

procedure TRtcSocketClientThread.CloseConn;
  begin
  try
    if assigned(RtcConn) then
      begin
      RtcConn.Lost:=False;
      RtcConn.InternalDisconnect;
      end;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('TRtcSocketClientThread.CloseConn : RtConn.InternalDisconnect',E,'ERROR');
    end;
  end;

destructor TRtcSocketClientThread.Destroy;
  begin
  try
    if assigned(RtcConn) then
      begin
      if Releasing then
        begin
        RtcConn.Client_Thread:=nil;
        RtcFreeAndNil(RtcConn);
        end
      else
        begin
        RtcConn.InternalDisconnect;
        RtcConn.Client_Thread:=nil;
        RtcConn:=nil;
        end;
      end;
    inherited;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('TRtcSocketClientThread.Destroy',E,'ERROR');
    end;
  end;

function TRtcSocketClientThread.RunJob:boolean;
  begin
  Result:=False;
  try
    if Job is TRtcSockMessage then
      begin
      try
        if assigned(RtcConn) then
          if assigned(RtcConn.Conn) then
            RtcConn.Conn.DoMessage(TRtcSockMessage(Job).Msg,0);
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            try
              Log('CLI:TRtcSocketClientThread.RunJob "DoMessage('+Int2Str(TRtcSockMessage(Job).Msg)+')"',E,'ERROR');
            except
              on E2:Exception do
                Log('CLI:TRtcSocketClientThread.RunJob "DoMessage('+RtcString(E2.ClassName)+':'+RtcString(E2.Message)+')"',E,'ERROR');
              end;
          end;
        end;
      end
    else if Job is TRtcSocketMesDataJob then
      begin
      try
        if assigned(RtcConn) then
          begin
          if assigned(RtcConn.Conn) then
            Result:=inherited RunJob
          else if TRtcJob(Job).SingleUse then
            RtcFreeAndNil(Job);
          end
        else if TRtcJob(Job).SingleUse then
          RtcFreeAndNil(Job);
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            try
              Log('CLI:TRtcSocketClientThread.RunJob "TRtcSocketMesDataJob('+Int2Str(TRtcSocketMesDataJob(Job).Msg)+')"',E,'ERROR');
            except
              on E2:Exception do
                Log('CLI:TRtcSocketClientThread.RunJob "TRtcSocketMesDataJob('+RtcString(E2.ClassName)+':'+RtcString(E2.Message)+')"',E,'ERROR');
              end;
          end;
        end;
      end
    else if Job=Message_WSOpenConn then
      begin
      try
        OpenConn;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSOpenConn"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSReOpenConn then
      begin
      try
        ReOpenConn;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSReOpenConn"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSCloseConn then
      begin
      try
        CloseConn;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSCloseConn"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSStop then
      begin
      try
        if assigned(RtcConn) then
          begin
          RtcConn.Client_Thread:=nil;
          RtcConn:=nil;
          end;
        Result:=True;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSStop"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSRelease then
      begin
      try
        Releasing:=True;
        Result:=True;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSRelease"',E,'ERROR');
          end;
        end;
      end
    else
      begin
      try
        Result:=inherited RunJob;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            try
              Log('CLI:TRtcSocketClientThread.RunJob (else: "'+RtcString(Job.ClassName)+'")',E,'ERROR');
            except
              on E2:Exception do
                Log('CLI:TRtcSocketClientThread.RunJob (else: *ClassError* "'+RtcString(E2.ClassName)+':'+RtcString(E2.Message)+'")',E,'ERROR');
              end;
          end;
        end;
      end;
  except
    on E:Exception do
      begin
      Result:=True;
      if LOG_AV_ERRORS then
        Log('CLI:TRtcSocketClientThread.RunJob *AV*',E,'ERROR');
      end;
    end;
  end;

procedure TRtcSocketClientProvider.CryptNeedMoreData;
  begin
  if assigned(Conn) then Conn.NeedMoreData;
  end;

procedure TRtcSocketClientProvider.NeedMoreData;
  begin
  if assigned(Conn) then
    begin
    FRealNeedData:=True;
    Conn.NeedMoreData;
    end;
  end;

function TRtcSocketClientProvider.GetCryptProtocol: TRtcCryptPluginProtocol;
  begin
  Result:=cppTcp;
  end;

procedure TRtcSocketClientProvider.SetDataProtocol(res:TRtcCryptPluginState);
  begin
  FDataProtocol:=cppTcp;
  end;

type
  TMySocketCliProvUnit=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  mySock:TMySocketCliProvUnit;

{ TMySocketCliProv }

constructor TMySocketCliProvUnit.Create;
  begin
  inherited;
  Message_WSOpenConn:=TRtcBaseMessage.Create;
  Message_WSReOpenConn:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  end;

destructor TMySocketCliProvUnit.Destroy;
  begin
  try
    RtcFreeAndNil(Message_WSOpenConn);
    RtcFreeAndNil(Message_WSReOpenConn);
    RtcFreeAndNil(Message_WSCloseConn);
    RtcFreeAndNil(Message_WSStop);
    RtcFreeAndNil(Message_WSRelease);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TMySocketCliProv.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcSocketCliProv Initializing ...','DEBUG');{$ENDIF}

mySock:=TMySocketCliProvUnit.Create;

{$IFDEF RTC_DEBUG} Log('rtcSocketCliProv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcSocketCliProv Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;
RtcFreeAndNil(mySock);

{$IFDEF RTC_DEBUG} Log('rtcSocketCliProv Finalized.','DEBUG');{$ENDIF}
end.
