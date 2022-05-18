{
  @html(<b>)
  Connection
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit defines the basic wrappers for all RTC Connection Components
  and is used in all your units which work with any RTC connection component.
  The most-used and referenced components throughout the RTC Component Suite
  will be @Link(TRtcConnection), @Link(TRtcServer) and @Link(TRtcClient).
}
unit rtcConn;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  SysUtils,

  rtcTypes,
  rtcSrcList,
  rtcSystem,
  rtcThrPool,
  rtcTimer,

  rtcInfo,
  rtcLink,

  rtcConnProv;

var
  // Write all Timed-out disconnects to a Log file?
  LOG_TIMEOUT_DISCONNECTS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

  { @name defines the Memory limit in Bytes.
    No new connections will be accepted by any server connection
    component in this application after this limit is reached.
    @html(<br><br>)

    Each Windows installation has its memory limitations.
    By setting the memory limit for the app here, you can
    throw away new connections in case your memory usage goes
    over the normal value.
    @html(<br>)
    By setting the limit too low, new connections will be abandoned for no reason.
    @html(<br>)
    By setting the limit too high, if you don't check your free memory anywhere
    else in the application and the application needs a lot of memory, you
    could end up with Out-of-memory or Access-Violation exceptions.
    @html(<br><br>)

    One 32-bit application can not address more then 4GB of RAM,
    so the highest value you should use here is 4.000.000.000 (under 4 GB),
    in case you have more RAM then you could ever need.
    @html(<br>)
    Standard value here is 500.000.000 (under 500 MB). }
  RTC_MEMORY_LIMIT:int64=500000000;

  { @name sets the Total connection count limit (default = 0; no limit),
    limiting the number of total active connections (server+client). }
  RTC_CONNECTION_LIMIT:integer=0;

  { @name sets the connection count limit for the client (default = 0; no limit),
    limiting the number of connections that can be made using any TRtcClient
    connection component descendant. If more client connections are attempted,
    an exception will be raised from the 'Connect' method. }
  RTC_CLIENT_CONNECT_LIMIT:integer=0;

  { @name sets the limit for accepting connections on the server (default = 0; no limit)
    by using any TRtcServer connection component descendant. If more connections try
    to come, they will be abandoned (disconnected without asking or notifying). }
  RTC_SERVER_ACCEPT_LIMIT:integer=0;

type
  // @exclude
  RtcIPV = rtcTypes.RtcIPV;

  { @abstract(Used for exceptions raised by TRtcConnection)

    To stay on the safe side, you should catch ALL exceptions
    of type "Exception" in order to handle exceptions that may be
    raised from the RTC communication components. Some underlying components
    (for example: connection providers) may use their own exception types. }
  ERtcConnection = class(Exception);

  TRtcComponent = class;

  TRtcConnection = class;

  // @exclude
  TRtcSimpleEvent = procedure(Sender:TObject) of object;

  { TRtcUserEvent is the event handler type for user-defined events. }
  TRtcUserEvent = procedure(Sender:TRtcConnection; Obj:TObject) of object;

  { TRtcUserDataEvent is the event handler type for user-defined data events. }
  TRtcUserDataEvent = procedure(Sender:TRtcConnection; Obj:TObject; Data:TRtcValue) of object;

  { TRtcNotifyEvent is the standard event handler type.
    Using one basic event type makes writing event handles much easier,
    since most methods are exchangeable. }
  TRtcNotifyEvent = procedure(Sender:TRtcConnection) of object;

  { TRtcErrorEvent is the error event handler type.
    All events that react on exceptions will use handlers of this type. }
  TRtcErrorEvent = procedure(Sender:TRtcConnection; E:Exception) of object;

  { TRtcFunctionCallEvent is the function call event handler type. }
  TRtcFunctionCallEvent=procedure(Sender:TRtcConnection;
                                  Param:TRtcFunctionInfo;
                                  Result:TRtcValue) of object;

  { TRtcFunctionErrorEvent is called if an exception is raised while executing a function call. }
  TRtcFunctionErrorEvent=procedure(Sender:TRtcConnection;
                                   Param:TRtcFunctionInfo;
                                   Result:TRtcValue;
                                   E:Exception;
                                   var Handled:boolean) of object;

  { TRtcResultEvent is the event handler to receive the result of a remote function call. }
  TRtcResultEvent=procedure(Sender:TRtcConnection;
                                  Data:TRtcValue;
                                  Result:TRtcValue) of object;

  { TRtcResultErrorEvent event handler is called if an exception is raised while processing a result received from the Server. }
  TRtcResultErrorEvent=procedure(Sender:TRtcConnection;
                                  Data:TRtcValue;
                                  Result:TRtcValue;
                                  E:Exception) of object;

  { TRtcFunctionPrepareEvent is the event handler to prepare a remote function call. }
  TRtcFunctionPrepareEvent=procedure(Sender:TRtcConnection;
                                     Data:TRtcValue) of object;

  { TRtcObjectCreateEvent is the event handler to create local instances of "Linked Objects". }
  TRtcObjectCreateEvent=procedure(Sender:TRtcConnection;
                                  Param:TRtcObjectCall) of object;

  { Data event }
  TRtcDataEvent=TRtcFunctionPrepareEvent;

{$IFDEF RTC_ANON_METHODS}

  // @exclude
  TRtcSimpleAnonMethod = reference to procedure(Sender:TObject);

  { TRtcUserEvent is the event handler type for user-defined events. }
  TRtcUserAnonMethod = reference to procedure(Sender:TRtcConnection; Obj:TObject);

  { TRtcUserDataEvent is the event handler type for user-defined data events. }
  TRtcUserDataAnonMethod = reference to procedure(Sender:TRtcConnection;
                                                  Obj:TObject; Data:TRtcValue);

  { TRtcNotifyEvent is the standard event handler type.
    Using one basic event type makes writing event handles much easier,
    since most methods are exchangeable. }
  TRtcNotifyAnonMethod = reference to procedure(Sender:TRtcConnection);

  { TRtcErrorEvent is the error event handler type.
    All events that react on exceptions will use handlers of this type. }
  TRtcErrorAnonMethod = reference to procedure(Sender:TRtcConnection; E:Exception);

  { TRtcFunctionCallEvent is the function call event handler type. }
  TRtcFunctionCallAnonMethod=reference to procedure(Sender:TRtcConnection;
                                                    Param:TRtcFunctionInfo;
                                                    Result:TRtcValue);

  { TRtcFunctionErrorEvent is called if an exception is raised while executing a function call. }
  TRtcFunctionErrorAnonMethod=reference to procedure(Sender:TRtcConnection;
                                                     Param:TRtcFunctionInfo;
                                                     Result:TRtcValue;
                                                     E:Exception;
                                                     var Handled:boolean);

  { TRtcResultEvent is the event handler to receive the result of a remote function call. }
  TRtcResultAnonMethod=reference to procedure(Sender:TRtcConnection;
                                              Data:TRtcValue;
                                              Result:TRtcValue);

  { TRtcResultErrorEvent event handler is called if an exception is raised while processing a result received from the Server. }
  TRtcResultErrorAnonMethod=reference to procedure(Sender:TRtcConnection;
                                                   Data:TRtcValue;
                                                   Result:TRtcValue;
                                                   E:Exception);

  { TRtcFunctionPrepareEvent is the event handler to prepare a remote function call. }
  TRtcFunctionPrepareAnonMethod=reference to procedure(Sender:TRtcConnection;
                                                       Data:TRtcValue);

  { TRtcObjectCreateEvent is the event handler to create local instances of "Linked Objects". }
  TRtcObjectCreateAnonMethod=reference to procedure(Sender:TRtcConnection;
                                                    Param:TRtcObjectCall);

  { TRtcCustomEvent is the event handler type for custom-defined events. }
  TRtcCustomAnonMethod = reference to procedure(Sender:TObject; Obj:TObject);

  { TRtcCustomDataEvent is the event handler type for custom-defined data events. }
  TRtcCustomDataAnonMethod = reference to procedure(Sender:TObject; Obj:TObject; Data:TRtcValue);

{$ENDIF}

  { This is the base component class, which all RTC components extend. }
  TRtcComponent = class(TRtc_Component)
    protected
      FNotSilent, // NOT Silent
      FAmSilent:boolean; // am Silent
    {$IFDEF RTC_ANON_METHODS}
      FAMCS:TRtcCritSec;
      FAMethods:tObjList;
    {$ENDIF}

      function GetVersionSDK:RtcString;
      procedure SetVersionSDK(const s:RtcString);

    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;

  {$IFDEF RTC_ANON_METHODS}
      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;

      { Get the "Simple" Event handler for anonymous method
          procedure(Sender:TObject) }
      function Anon(const Event:TRtcSimpleAnonMethod):TRtcSimpleEvent; overload;

      { Get the "Notify" Event handler for anonymous method
          procedure(Sender:TRtcConnection) }
      function Anon(const Event:TRtcNotifyAnonMethod):TRtcNotifyEvent; overload;

      { Get the "Error" Event handler from anonymous method
         procedure(Sender:TRtcConnection; E:Exception) }
      function Anon(const Event:TRtcErrorAnonMethod):TRtcErrorEvent; overload;

      { Get the "FunctionCall" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Param:TRtcFunctionInfo; Result:TRtcValue) }
      function Anon(const Event:TRtcFunctionCallAnonMethod):TRtcFunctionCallEvent; overload;

      { Get the "FunctionError" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Param:TRtcFunctionInfo; Result:TRtcValue; E:Exception; var Handled:boolean) }
      function Anon(const Event:TRtcFunctionErrorAnonMethod):TRtcFunctionErrorEvent; overload;

      { Get the "Result" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue) }
      function Anon(const Event:TRtcResultAnonMethod):TRtcResultEvent; overload;

      { Get the "ResultError" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue; E:Exception) }
      function Anon(const Event:TRtcResultErrorAnonMethod):TRtcResultErrorEvent; overload;

      { Get the "FunctionPrepare" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Data:TRtcValue) }
      function Anon(const Event:TRtcFunctionPrepareAnonMethod):TRtcFunctionPrepareEvent; overload;

      { Get the "User" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Obj:TObject) }
      function Anon(const Event:TRtcUserAnonMethod):TRtcUserEvent; overload;

      { Get the "UserData" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Obj:TObject; Data:TRtcValue) }
      function Anon(const Event:TRtcUserDataAnonMethod):TRtcUserDataEvent; overload;

      { Get the "Custom" Event handler from anonymous method
         procedure(Sender:TObject; Obj:TObject) }
      function Anon(const Event:TRtcCustomAnonMethod):TRtcCustomEvent; overload;

      { Get the "CustomData" Event handler from anonymous method
         procedure(Sender:TObject; Obj:TObject; Data:TRtcValue) }
      function Anon(const Event:TRtcCustomDataAnonMethod):TRtcCustomDataEvent; overload;

      { Get the "ObjectCreate" Event handler from anonymous method
         procedure(Sender:TRtcConnection; Param:TRtcObjectCall) }
      function Anon(const Event:TRtcObjectCreateAnonMethod):TRtcObjectCreateEvent; overload;
  {$ENDIF}

    published

      { RealThinClient SDK Version (for information only) }
      property Version_SDK:RtcString read GetVersionSDK write SetVersionSDK stored False;
      end;

  { @abstract(RTC "TPersistent" class) }
  TRtcPersistent = rtcSystem.TRtcPersistent;

  { @abstract(Provides Timeout capability to all TRtcConnection components)

    TRtcTimeout is tightly coupled with TRtcConnection component.
    It encapsulates all Timeout parametes for active client
    and server connections, defining how long a connection can
    stay Idle after a specific task, before a Timeout period is
    triggered which closes idle connections (timed-out disconnect). }
  TRtcTimeout = class(TRtcPersistent)
  private
    FTimer: TRtcTimer;

    FInterval:integer;

    FAfterConnecting:integer;
    FAfterConnect:integer;
    FAfterDataReceived:integer;
    FAfterDataLost:integer;
    FAfterDataSend:integer;
    FAfterDataOut:integer;
    FAfterDataIn:integer;
    FAfterDataSent:integer;

    FConn:TRtcConnection;
    FThr:TRtcThread;
    FJob:TRtcJob;

    procedure TriggerTimeout;

    procedure TimerSet;
    procedure TimerReset;

    property Conn:TRtcConnection read FConn write FConn;

  public
    { @exclude }
    constructor Create(Con:TRtcConnection);
    { @exclude }
    destructor Destroy; override;

    { @exclude }
    procedure AssignTo(Dest: TPersistent); override;

    { Start the Timeout counter.
      @exclude }
    procedure Start(Multi_Threaded:boolean);
    { Stop the Timeout counter
      @exclude }
    procedure Stop;

    { Temporary Disable the Timeout counter.
      You can use this to disable the timeout while you are processing data. }
    procedure Disable;
    { Enable the Timeout counter and set its intervals to 'timeout' miliseconds.
      You can use this to set the next timeout period,
      overwriting the last value set by the connection component.
      This new interval will only have effect until the connection component changes it,
      which could also be immeriatelly after your call to Enable(). }
    procedure Enable(timeout:integer);

    { Connceting started, set timeout to AfterConnecting.
      @exclude }
    procedure Connecting;
    { Connect accomplished, set timeout to AfterConnect.
      @exclude }
    procedure Connect;

    { Data Sending started, set timeout to AfterDataSend.
      @exclude }
    procedure DataSending;
    { Data sent Out, set timeout to AfterDataOut.
      @exclude }
    procedure DataOut;
    { Data read In, set timeout to AfterDataIn.
      @exclude }
    procedure DataIn;
    { Data Sent, set timeout to AfterDataSent.
      @exclude }
    procedure DataSent;

    { New Data received, set timeout to AfterDataReceived.
      @exclude }
    procedure DataReceived;
    { Data Packet Lost (UDP only), set timeout to AfterDataLost.
      @exclude }
    procedure DataLost;

  published
    { Time (seconds) to wait for a Connect (Connect means that you can send and/or
      receive data through the connection) after Connecting was initiated (Connect message received,
      but the connection is not yet ready to be used). @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterConnecting:integer read FAfterConnecting write FAfterConnecting default 0;
    { Time (seconds) to wait for something to come through the connection or for user
      to start sending data through, after a Connect (Connect means that you can send and/or
      receive data through the connection). @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterConnect:integer read FAfterConnect write FAfterConnect default 0;
    { Time (seconds) to wait for the next event to trigger for this connection, after a
      data package was received (OnDataReceived event was triggered) . @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataReceived:integer read FAfterDataReceived write FAfterDataReceived default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after a sent package was lost (UDP Only timeout; OnDataLost event was triggered). @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataLost:integer read FAfterDataLost write FAfterDataLost default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after Data was prepared for sending (Write() or some other method to fill
      data into sending buffer was called). @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataSend:integer read FAfterDataSend write FAfterDataSend default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after a Data chunk was sent out (OnDataOut event was triggered). @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataOut:integer read FAfterDataOut write FAfterDataOut default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after a Data chunk was received in (OnDataIn event was triggered). @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataIn:integer read FAfterDataIn write FAfterDataIn default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after the sending buffer was completey emptied and all Data was Sent out
      (OnDataSent event was triggered). @html(<br>)
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataSent:integer read FAfterDataSent write FAfterDataSent default 0;
    end;

  TRtcReconnectParam = class;

  TRtcRestartParam = class;

  Rtc1Bit=0..1;

  Rtc4Bit=$0..$F;

  Rtc4ByteMask=array[0..3] of byte;

  { @abstract(RTC Web Socket Header record) }
  RtcWSHeader=record
    { Boolean -> FIN (1 bit):
      Indicates that this is the final fragment in a message.
      The first fragment MAY also be the final fragment. }
    Final: boolean;
    { 1 bit each:
      MUST be 0 unless an extension is negotiated that defines meanings
      for non-zero values.  If a nonzero value is received and none of
      the negotiated extensions defines the meaning of such a nonzero
      value, the receiving endpoint MUST _Fail the WebSocket Connection_. }
    RSV1, RSV2, RSV3: Rtc1Bit;
    { 4 bits:
      Defines the interpretation of the "Payload data".  If an
      unknown opcode is received, the receiving endpoint MUST
      _Fail the WebSocket Connection_. The following values are defined:

      *  0 = denotes a continuation frame

      *  1 = denotes a text frame

      *  2 = denotes a binary frame

      *  3 - 7 = are reserved for further non-control frames

      *  8 = denotes a connection close

      *  9 = denotes a ping

      *  10 = denotes a pong

      *  11 - 15 = are reserved for further control frames }
    Opcode: Rtc4Bit;
    { Boolean -> Mask (1 bit):
      Defines whether the "Payload data" is masked.  If set to 1, a
      masking key is present in masking-key, and this is used to unmask
      the "Payload data" as per Section 5.3.  All frames sent from
      client to server have this bit set to 1. }
    Masked: boolean;
    { 7 bits, 7+16 bits, or 7+64 bits:
      The length of the "Payload data", in bytes: if 0-125, that is the
      payload length.  If 126, the following 2 bytes interpreted as a
      16-bit unsigned integer are the payload length.  If 127, the
      following 8 bytes interpreted as a 64-bit unsigned integer (the
      most significant bit MUST be 0) are the payload length.  Multibyte
      length quantities are expressed in network byte order.  Note that
      in all cases, the minimal number of bytes MUST be used to encode
      the length, for example, the length of a 124-byte-long string
      can't be encoded as the sequence 126, 0, 124.  The payload length
      is the length of the "Extension data" + the length of the
      "Application data".  The length of the "Extension data" may be
      zero, in which case the payload length is the length of the
      "Application data". }
    PayloadLength: int64;
    { 0 or 4 bytes:
      All frames sent from the client to the server are masked by a
      32-bit value that is contained within the frame.  This field is
      present if the mask bit is set to 1 and is absent if the mask bit
      is set to 0.  See Section 5.3 for further information on client-
      to-server masking. }
    MaskingKey:Rtc4ByteMask;

    { Payload sent/received (calculated) }
    PayloadInOut: int64;
    { The length of this WebSocket Header (calculated):
      < 0; Header Invalid.
      = 0; Header not complete or manually created.
      > 0; number of bytes in the Header. }
    HeaderLength: Integer;
    end;

  { @abstract(RTC Web Socket Frame class) }
  TRtcWSFrame=class(TRtcRecord)
  private
    FHaveHead,
    FHeadSent,
    FFinalSent,
    FStarted,
    FFinished:boolean;
    FStartOpcode:Rtc4Bit;
    FHead:RtcWSHeader;
    FData:RtcByteArray;
    FOwner:TRtcConnection;
    FName:RtcWideString;
    FSending:boolean;
    FTotalInOut,
    FTotalLength:int64;

    procedure NeedHead;
    procedure UpdateFinished;
    function CheckMore:boolean;

    function GetTotalLength:int64;
    procedure SetTotalLength(const Value: int64);

    procedure SetMasked(const Value: Boolean);
    procedure SetFinished(const Value: Boolean);

  protected
    // @exclude
    procedure CopyFrom(pValue: TRtcValueObject); override;

    // @exclude
    procedure SetOwner(Sender:TRtcConnection; const iName:RtcWideString);

  public
    { Returns a Copy of this Web Socket Frame object (including the current Frame state). }
    function copyOf:TRtcValueObject; override;

    { Creates a new Web Socket Frame for SENDING data, using these default values:
      Final:=TRUE, PayloadLength:=-1, Masked=FALSE and everything else set to 0. }
    constructor Create; overload;

    { Creates a new Web Socket Frame for SENDING data and copies all attributes from "FromFrame"
      to the newly created Frame: Opcode, Final, RSV1-3, PayloadLength, Masked and MaskingKey.
      PayloadInOut and HeaderLength parameters are initialized to 0. }
    constructor Create(FromFrame:TRtcWSFrame); overload;

    { Creates a new Web Socket Frame for SENDING data, using these default values:
      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=-1),
      Masked=FALSE and everything else is set to 0. }
    constructor Create(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=-1); overload;

    { Creates a new Web Socket Frame for SENDING data, using these default values:
      Opcode:="vOpcode", starting Payload="vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=-1),
      Masked=FALSE and everything else is set to 0. }
    constructor Create(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1); overload;

    { Creates a new Web Socket Frame for SENDING data, using these default values:
      Opcode:="vOpcode", starting Payload="vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=-1),
      Masked=FALSE and everything else is set to 0. }
    constructor Create(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1); overload;

    { Creates a new Web Socket Frame for READING data.
      "FromData" has to be empty, or contain the first bytes received.
      If "Header" is included in "FromData", header fields will be populated.
      If "Payload" is also found, it will be written and prepared for reading.
      If "Header" is NOT complete in "FromData", then header fields will be
      populated from the "wfWrite" or "wfWriteEx" methods, once enough data
      was written to include at least the "Header" for this Web Socket Frame. }
    constructor Create(const FromData:RtcByteArray); overload;

    { Creates a new Web Socket Frame for READING data.
      "FromData" has to be empty, or contain the first bytes received.
      If "Header" is included in "FromData", header fields will be populated.
      If "Payload" is also found, it will be written and prepared for reading.
      If "Header" is NOT complete in "FromData", then header fields will
      be populated from the "wfWrite" or "wfWriteEx" methods, once enough data
      was written to include at least the "Header" for this Web Socket Frame. }
    constructor Create(const FromData:RtcString); overload;

    { Checks for a Web Socket Frame Header in "FromData" (Payload NOT required) and
      Returns the total number of bytes in the Frame (HeaderLength + PayloadLength).
      Returns -1 if the Header is corrupt (header found in "FromData", but invalid).
      Returns 0 if more data is needed (header not complete in "FromData"). }
    class function Check(const FromData:RtcByteArray):int64; overload;

    { Checks for a Web Socket Frame Header in "FromData" (Payload NOT required) and
      Returns the total number of bytes in the Frame (HeaderLength + PayloadLength).
      Returns -1 if the Header is corrupt (header found in "FromData", but invalid).
      Returns 0 if more data is needed (header not complete in "FromData"). }
    class function Check(const FromData:RtcString):int64; overload;

    // @exclude
    destructor Destroy; override;

    { Boolean (FIN) -> 1 bit, Web Socket Frame Attribute (part of the Header):
      Indicates that this is the final fragment in a message.
      The first fragment MAY also be the final fragment.
      This attribute is set in the constructor and can NOT be modified
      directly, but may be changed using the "wfFinished" property. }
    property waFinal: boolean read FHead.Final write SetFinished;

    { RSV bit #1, Web Socket Frame Attribute (part of the Header):
      MUST be 0 unless an extension is negotiated that defines meanings
      for non-zero values. If a nonzero value is received and none of
      the negotiated extensions defines the meaning of such a nonzero
      value, the receiving endpoint MUST _Fail the WebSocket Connection_. }
    property waRSV1: Rtc1Bit read FHead.RSV1 write FHead.RSV1;
    { RSV bit #2, Web Socket Frame Attribute (part of the Header):
      MUST be 0 unless an extension is negotiated that defines meanings
      for non-zero values. If a nonzero value is received and none of
      the negotiated extensions defines the meaning of such a nonzero
      value, the receiving endpoint MUST _Fail the WebSocket Connection_. }
    property waRSV2: Rtc1Bit read FHead.RSV2 write FHead.RSV2;
    { RSV bit #3, Web Socket Frame Attribute (part of the Header):
      MUST be 0 unless an extension is negotiated that defines meanings
      for non-zero values. If a nonzero value is received and none of
      the negotiated extensions defines the meaning of such a nonzero
      value, the receiving endpoint MUST _Fail the WebSocket Connection_. }
    property waRSV3: Rtc1Bit read FHead.RSV3 write FHead.RSV3;

    { 4 bits, Web Socket Frame Attribute (part of the Header):
      Defines the interpretation of the "Payload data".
      If an unknown opcode is received, the receiving endpoint MUST
      _Fail the WebSocket Connection_. The following values are defined:

      *  0 = denotes a Continuation frame
      *  1 = denotes a Text frame
      *  2 = denotes a Binary frame
      *  3 - 7 = are reserved for further non-control frames

      *  8 = denotes a connection CLOSE
      *  9 = denotes a Ping
      *  10 = denotes a Pong
      *  11 - 15 = are reserved for further control frames }
    property waOpcode: Rtc4Bit read FHead.Opcode write FHead.Opcode;

    { Boolean (MASK) -> 1 bit, Web Socket Frame Attribute (part of the Header):
      Defines whether the "Payload data" is masked.
      If TRUE, a masking key is present in MaskingKey,
      and this is used to mask (before sending) or unmask
      (after reading) the "Payload data" as per Section 5.3.
      All frames sent from Client to Server have to be masked.

      Setting "Masked:=TRUE" generates a new random MaskingKey.
      Setting "Masked:=FALSE" clears all MaskingKey bytes to 0. }
    property waMasked: Boolean read FHead.Masked write SetMasked;

    { 7 / 7+16 / 7+64 bits -> Web Socket Frame Attribute (part of the Header):
      The length of "Payload data" in bytes. The payload length
      is the length of the "Extension data" + the length of the
      "Application data".  The length of the "Extension data"
      may be zero, in which case the payload length is the
      length of the "Application data". If Payload length was
      NOT automatically extracted from data being written and was
      NOT set manually using this property, it will be calculated
      by the first "wfHead" or "wfHeadEx" method to equal the number
      of "Payload" bytes written so far using "wfWrite" or "wfWriteEx". }
    property waPayloadLength: int64 read FHead.PayloadLength write FHead.PayloadLength;

    { 0 or 4 bytes, Web Socket Frame Attribute (part of the Header):
      All frames sent from the Client to the Server are masked by a
      32-bit value that is contained within the frame.  This field is
      present if Masked=TRUE (the mask bit is set to 1) and is absent
      if Masked=FALSE (the mask bit is set to 0). See Section 5.3
      for further information on client-to-server masking. }
    property waMaskingKey:Rtc4ByteMask read FHead.MaskingKey write FHead.MaskingKey;

    { Add more data to this Frame. @html(<br><br>)

      If called after "Done=TRUE" (all data written to the Frame
      was read, no more data can be written to the same Frame),
      this method automatically re-initializes the Frame object,
      so it can be re-used for READING or SENDING the next Frame. @html(<br><br>)

      If this Frame was created for READING, this method automatically
      extracts the Frame Header and populates this objects Header info. @html(<br><br>)

      If this Frame was created for SENDING, this method automatically
      initializes the Frame Header and prepares the next Frame for sending. }
    procedure wfWrite(const MoreData:RtcString);

    { Add more data to this Frame. @html(<br><br>)

      If called after "wfDone=TRUE" (all data written and read from this Frame),
      this method automatically re-initializes the Frame object,
      so it can be re-used for READING or SENDING the next Frame. @html(<br><br>)

      If this Frame was created for READING, this method automatically
      extracts the Frame Header and populates this objects Header info. @html(<br><br>)

      If this Frame was created for SENDING, this method automatically
      initializes the Frame Header and prepares the next Frame for sending. }
    procedure wfWriteEx(const MoreData:RtcByteArray);

    { Web Socket Frame Header - for INFORMATION ONLY, do NOT SEND!
      This method is used internally by the "Read" method if
      the Frame is being sent and the Header was not Read yet. }
    function wfHead:RtcString;
    { Web Socket Frame Header - for INFORMATION ONLY, do NOT SEND!
      This method is used internally by the "ReadEx" method if
      the Frame is being sent, but the Header was not read yet. }
    function wfHeadEx:RtcByteArray;
    { Human-readable Header info - for INFORMATION ONLY, do NOT SEND! }
    function wfHeadInfo:RtcString;

    { Has the Header for this Frame been sent? }
    property wfHeaderSent:boolean read FHeadSent;

    { Read more data from this Frame. The first call to "Read" on a
      Frame being sent includes the Frame Header. }
    function wfRead:RtcString;
    { Read more data from this Frame. The first call to ReadEx on a
      Frame being sent includes the Frame Header. }
    function wfReadEx:RtcByteArray;

    { Web Socket Frame Header length:
      Calculated in class constructors using "FromData", or ...
      using "FromData" in "wfWrite" or "wfWriteEx" method, or ...
      in the 1st "wfHead" or "wfHeadEx" call based on header fields:
      < 0; Header is Invalid.
      = 0; Header manually created or incomplete.
      > 0; Number of bytes in the Header. }
    property wfHeaderLength: Integer read FHead.HeaderLength;

    { Number of bytes already written *and* read from this Frame's "Payload". }
    property wfPayloadInOut: int64 read FHead.PayloadInOut;

    { Total number of bytes already written *and* read from all Frames in this set. }
    property wfTotalInOut: int64 read FTotalInOut;

    { Total Payload length of all Frames in this set.
      This property is calculated automatically based on Payload data being written,
      but ... it can also be used to explicitly set the maximum combined "Payload" 
      length allowed for all Frames being sent using this Frame object. By manually 
      setting this property to a value greated or equal to zero, the "wfFinished" 
      flag will be set automatically as soon as the number of bytes written 
      using this Frame object reaches the total length specified. }
    property wfTotalLength: int64 read GetTotalLength write SetTotalLength;

    { Number of bytes ready to be Read from this Frame.
      Use the "Read" or "ReadEx" method to read. }
    function wfPayloadReady: int64;

    { If this is a continuation Frame, wfOpcode returns
      the Opcode attribute of the first Frame in this Set. }
    property wfOpcode:Rtc4Bit read FStartOpcode;

    { Frame finished. No more "Payload" should be written to this Frame. }
    property wfFinished:boolean read FFinished write SetFinished;

    { Frame started. The Frame Header is ready (READING) or already sent (SENDING),
      but the "Read" or "ReadEx" methods have NOT been used to read "Payload" data. }
    property wfStarted:boolean read FStarted;

    { Frame is Complete. No more data can be written to this Frame,
      but there could still be some "Payload" data ready for Reading. }
    function wfComplete:boolean;

    { Frame is Done. Nothing more can be read or written to this Frame.

      If (Done=TRUE) and (Final=TRUE) and this Frame object has been
      assigned to a TRtcConnection component, it will be auto-freed
      by the TRtcConnection component before the next OnWSDataSent
      or the next OnWSDataReceived event (whatever comes first). }
    function wfDone:boolean;

    { The "Name" of this Frame object.
      Set when the object is used in Send() or assigned to the Frame[] property. }
    property wfName:RtcWideString read FName;

    { Returns the TRtcConnection component where this object is stored and managed,
      or NIL if this Frame object has NOT been assigned to a TRtcConnection component.

      This is set automatically when a Frame object is used in the Send() method
      or assigned to the Frame[] property of a TRtcConnection component, to ensure
      that the TRtcConnection component is notified if this object is destroyed,
      removing the object from any lists managed by the TRtcConnection component. }
    property wfOwner:TRtcConnection read FOwner;

    { Returns TRUE if this Frame was created for SENDING data.
      Returns FALSE if this Frame was created for READING data.
      This is set when the object is created and can NOT be changed! }
    property wfSending:boolean read FSending;
    end;

  { @abstract(RTC Web Socket Manager) }
  TRtcWSManager=class(TObject)
  private
    FCS:TRtcCritSec;
    FList:tObjList;
  public
    constructor Create; overload;
    destructor Destroy; override;

    // Add a new (upgraded WebSocket) connection to this Manager
    function wsAdd(Sender:TRtcConnection):RtcIntPtr;
    // Remove a connection from this Manager
    function wsRemove(Sender:TRtcConnection):RtcIntPtr;

    // First connection (returns ID, 0 = no connections)
    function wsFirst:RtcIntPtr;
    // Next connection (returns ID, 0 = no more connecitons)
    function wsNext(id:RtcIntPtr):RtcIntPtr;
    // Prior connection (returns ID, 0 = no more connections)
    function wsPrior(id:RtcIntPtr):RtcIntPtr;
    // Last connection (returns ID, 0 = no connections)
    function wsLast:RtcIntPtr;

    // Web Socket Connection count
    function wsCount:integer;

    { Add Frame "iFrame" to the SENDING QUEUE of the connection "id". @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns 1 if "iFrame" object was added to the queue, or 0 if this call fails. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSend(id:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { Add more "Payload" data to Frame "iFrame", being sent or queued for sending to connection "id".
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns 1 if "Payload" was added to the "iFrame" object, or 0 if this call failed. }
    function wSendMore(id:RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False):integer; overload;

    { Add more "Payload" data to Frame "iFrame", being sent or queued for sending to connection "id".
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns 1 if "Payload" was added to the "iFrame" object, or 0 if this call failed. }
    function wSendMore(id:RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add Frame "iFrame" to the SENDING QUEUE of the connection "id", but ONLY if NOTHING
      is being sent through connection 'id' and the sending queue of connection "id" is EMPTY. @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns 1 if "iFrame" object was added to the queue, or 0 if this call fails. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendIfIdle(id:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add Frame "iFrame" to the SENDING QUEUE of all connections managed here.
      The original "iFrame" object is destroyed automatically by this method call. @html(<br><br>)

      If called with iName<>'', a copy of "iFrame" will be created for
      every connection component and stored in the "Frame[iName]" property
      and available in all events triggered by the connection component,
      where it will be accessible using "Sender.Frame[iName]" and
      will be managed (and destroyed) by the "Sender.Frame" property.  @html(<br><br>)

      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendToAll(iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of all managed connections,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of all managed connection,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of all managed connections,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add Frame "iFrame" to the SENDING QUEUE of all connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY. @html(<br><br>)

      The original "iFrame" object is destroyed automatically by this method call. @html(<br><br>)

      If called with iName<>'', a copy of "iFrame" will be created for
      every connection component and stored in the "Frame[iName]" property
      and available in all events triggered by the connection component,
      where it will be accessible using "Sender.Frame[iName]" and
      will be managed (and destroyed) by the "Sender.Frame" property.  @html(<br><br>)

      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendToIdle(iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0. @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add Frame "iFrame" to the SENDING QUEUE of all connections managed by this
      Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"). @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns the number of connections where the "iFrame" object was
      added to the queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendToOthers(xid:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0. @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { Disconnect the Web Socket connection "id".
      Returns 1 if a connection with "id" was found (and will be closed),
      or 0 if this call has failed (connection could not be found). }
    function wsDisconnect(id:RtcIntPtr):integer;

    { Disconnect all Web Socket connections managed here.
      Returns the number of connections found (and now being closed),
      or 0 if this call has failed (no connections found). }
    function wsDisconnectAll:integer;

    { Clear the SENDING QUEUE for the Web Socket connection "id".
      Returns 1 if a connection with "id" was found (queued Frames will be removed),
      or 0 if this call has failed (connection could not be found).
      This does NOT cancel Web Socket Frames already being SENT out.
      It only removes Web Socket Frames waiting in the queue! }
    function wsClearSendingQueue(id:RtcIntPtr):integer;

    { Clear SENDING QUEUES of all Web Socket connections managed here.
      Returns 1 if a connection with "id" was found (queued Frames will be removed),
      or 0 if this call has failed (connection could not be found).
      This does NOT cancel Web Socket Frames already being SENT out.
      It only removes Web Socket Frames waiting in the queue! }
    function wsClearAllSendingQueues:integer;

    { Returns the "ID" of a "Sender:TRtcConnection" component if the
      "Sender:TRtcConnection" is managed by this Web Socket Manager.
      The "ID" returned by "wsGetID" can be used with all methods of
      this Web Socket Manager which require "id:RtcIntPtr" as parameter
      (like "wSendTo", "wsClearSendingQueue" or "wsDisconnect" methods). }
    function wsGetID(const Sender:TRtcConnection):RtcIntPtr;
    end;

  { @abstract(Basic connection component wrapper)
    TRtcConnection is the Basic connection component wrapper (client & server).
    It publishes methods and handles all the tasks that are common to all
    connection components. All connection components are indirect descendants
    of TRtcConnection and therefor inherit all its methods and properties.

    VERY IMPORANT!!! If you create connection components at runtime:
      * NEVER! use Create(), Free or Destroy on ANY connection component.
      * Allways use the 'NEW' class function (implemented by all TRtcConnection
        components) to create the connection object and
      * ONLY use the 'RELEASE' method to free the connection object.

    @html(<b>)
    MULTITHREADED CLIENT/SERVER:
    @html(</b><br><br>)

    When @Link(TRtcConnection.MultiThreaded) property for this connection is True,
    your events will be called from within a Thread inside a Thread pool.
    If you decide to use your component(s) in a multithreaded environment,
    you have to keep in mind that all Data is being sent and received in
    the background, from threads belonging to the RTC Thread Pool.
    This implementation is thoroughly tested and works perfectly in
    any mulithreaded environment (tested on a dual-processor machine with
    thousands of clients accessing the server at the same time). But,
    to keep the code working for you, you will have to follow a few simple rules.

    1. Accessing the component in MultiThreaded mode:

      A) After a connection is initiated ('Connect' was called),
         your code which needs to access any methods or properties of
         that component, HAS TO BE synchronized with any background threads
         that might be using the component to process background requests.

         The easiest way to work with any TRtcConnection component
         in a multithreaded mode is to implement everything as events.
         There are enough event handlers declared, so that most of
         the things can be done this way.

         The mechanism integrated into TRtcConnection components makes sure that
         events triggered by the connection component DO NOT OVERLAP with callback
         functions and messages called by Windows. This means that you can safely
         use the component from ANY EVENT you define for that component.
         When the event is triggered, you are the owner of the component
         and nothing will access the component during your event execution.

      B) But, there will probably be situations where you do not want to
         wait for a connection event in order to do something with your connection.

         To be able to access the connection components from anywhere else
         than your event handlers (for example, from a Button click event),
         even when they are running in multithreaded mode, you will use
         TRtcConnection components ability to POST A JOB to itself.

         All actions that TRtcConnection components do, are defined by
         small jobs that have to be executed. When there are no jobs waiting
         for execution, there are no Threads or CPU resources used.

         Jobs are objects that contain DATA AND EXECUTION CODE. You can
         create jobs by extending the TRtcJob class and implementing
         your classes Run and SingleUse methods (defined as abstract by TRtcJob).
         After creating an instance of your TRtcJob class and initializing
         it with the data you need to execute the Run method, you can
         post your job object to your connection component by using
         the PostJob method (also implemented by TRtcConnection).

    2. Accessing the GUI from events in MultiThreaded mode:

      All events which have to access the GUI (Graphical User Interface,
      containing any visual component) and use components in Multi-Threaded mode
      HAVE TO be synchronized with the Main Thread by using the 'Sync' method.

      If the "Sync" method is called from any RTC Thread, the Event passed as a
      parameter will be called from the Main Thread, puting the background thread
      (from which you called Sync) in a waiting state, until the Event finishes.
      When finished, the "Sync" method will return TRUE.

      If the "Sync" method is called from the Main Thread,
      it will do nothing and immediately return TRUE.

      You can also use the "inMainThread" method to check if
      your code is currently running inside the Main Thread.

      Here is one 'Access GUI from OnDataReceived event' example:

        procedure TMyForm.ServerOnDataReceived(Sender:TRtcConnection);
          begin
          // If we are currently inside any RTC background Thread,
          // the "Sync" method will post the event we specify in the 1st parameter
          // to the Main Thread, wait for that Event to finish and then return TRUE.
          // If we are already in the Main Thread,
          // the Sync method does nothing and returns FALSE.

          // By using the following construct at the top of our event, we can
          // make sure that any code executed below will be running in the Main Thread.

          if Sender.Sync(ServerOnDataReceived) then Exit;

          // Since we are calling EXIT in case the "Sync" method returned TRUE
          // and we using our own even as the 1st parameter in the "Sync" method,
          // any code executed from this point will be running in the Main Thread.
          end;

      This OnDataReceived event handler example wants to have GUI access
      throughout the event execution, so it will use the "Sync" method to
      call itself synchronized from the Main Thread in case it was called
      from a background RTC thread, then EXIT if the call was successful.

      Naturaly, for events which do not need to access the GUI, you can
      simply implement the event without using the "Sync" method. }
  TRtcConnection = class(TRtcComponent)
  private // CLIENT & SERVER

    FInfo:TRtcInfo;

    FRecTimer:TRtcTimer;

    FMultiThread:boolean;

    FFree:boolean;
    FReadyToRelease:boolean;

    FActive:boolean;
    FClosing:boolean;

    FParent: TRtcConnection;

    FTimeout: TRtcTimeout;
    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;

    FPort:RtcString;
    FAddr:RtcString;
    FIPV:RtcIPV;
    FOverLimit:boolean;

    FMyEvent:TRtcNotifyEvent;

    FMyErrMsg:Exception;
    FMyErrEvent:TRtcErrorEvent;
    FMyHandled:boolean;

    FMyUserMsg:TObject;
    FMyUserEvent:TRtcUserEvent;

    FMyUserData:TRtcValue;
    FMyUserDataEvent:TRtcUserDataEvent;

    FMyCustMsg:TObject;
    FMyCustEvent:TRtcCustomEvent;

    FMyCustData:TRtcValue;
    FMyCustDataEvent:TRtcCustomDataEvent;

    FMyFunc:TRtcFunctionInfo;
    FMyFuncResult:TRtcValue;
    FMyFuncEvent:TRtcFunctionCallEvent;
    FMyFuncError:TRtcFunctionErrorEvent;
    FMyFuncData:TRtcValue;
    FMyResultEvent:TRtcResultEvent;
    FMyResultErrorEvent:TRtcResultErrorEvent;
    FMyFuncPrepEvent:TRtcFunctionPrepareEvent;

    FMyObjCreateEvent:TRtcObjectCreateEvent;
    FMyObjCreateParams:TRtcObjectCall;

    FInsideEvent:integer;
    FConnectOp:boolean;
    FListening:boolean;
    FDisconnecting:boolean;
    FStopping:boolean;
    FReleasing,
    FReleaseCall:boolean;

    FConnectTriggered:boolean;
    FConnectingTriggered:boolean;

    FOnConnect:TRtcNotifyEvent;
    FOnConnecting:TRtcNotifyEvent;
    FOnDisconnect:TRtcNotifyEvent;
    FOnDisconnecting:TRtcNotifyEvent;

    FOnException:TRtcErrorEvent;

    FOnDataOut:TRtcNotifyEvent;
    FOnDataIn:TRtcNotifyEvent;
    FOnDataSent:TRtcNotifyEvent;
    FOnDataReceived:TRtcNotifyEvent;
    FOnReadyToSend:TRtcNotifyEvent;

    function GetParent:TRtcConnection;

    { Check Parent connection. All connections connected to a local listener
      have this listener as their Parent. }
    property Parent:TRtcConnection read GetParent;

    procedure SetTimeout(const Value: TRtcTimeout);
    procedure SetTimeoutsOfAPI(const Value: TRtcTimeoutsOfAPI);

  protected // CLIENT & SERVER

    FWantConnect:boolean;
    FReconnecting:boolean;
    FConnecting:boolean;
    FTimedOut:boolean;

    { @exclude }
    FDataOut:cardinal;
    { @exclude }
    FDataIn:cardinal;
    { @exclude }
    FReadCount:int64;
    { @exclude }
    FWriteCount:int64;

    { Connection provider component,
      set by TRtcConnection after calling the CreateProvider method.
      @exclude }
    Con:TRtcConnectionProvider;

    // @exclude
    function AutoRelease:boolean; virtual;

    // @exclude
    procedure SetMultiThread(const Value: boolean); virtual; abstract;

    { Create a new connection provider and return it as a result.
      @exclude }
    function CreateProvider:TObject; virtual; abstract;

    { Returns TRUE if the connection provider was released
     @exclude }
    function CheckProviderReleased:boolean;

    { The connection is closed. Returns TRUE if State=conInactive.
      @exclude }
    function ConnectionClosed:boolean;

    { @name is called by TRtcConnection before Connect or Listen,
      to copy parameters from the Connection component to
      the Connection Provider component. This is the method in which
      the connection component should initialize the connection provider's
      parameters with the ones set for the connection component.
      @exclude }
    procedure SetParams; virtual;

    { @name is called by TRtcConnection before Connect or Listen,
      to set connection provider's triggers to connection component's
      events, so that connection provider and connection component
      can work together as a team.
      @exclude }
    procedure SetTriggers; virtual;

    { @name is called by TRtcConnection after a connection was closed,
      to clear all connection provider's triggers, so that he connection
      provider doesn't cause an access violation in case the connection
      component is being released after disconnect, while the connection
      provider is still receiving old events or finalizing the connection.
      @exclude }
    procedure ClearTriggers; virtual;

    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event) method.
      @exclude }
    procedure CallMyEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Err) method.
      @exclude }
    procedure CallMyError;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj) method.
      @exclude }
    procedure CallMyUserEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj,Data) method.
      @exclude }
    procedure CallMyUserDataEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj) method.
      @exclude }
    procedure CallMyCustomEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj,Data) method.
      @exclude }
    procedure CallMyCustomDataEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Par,Res) method.
      @exclude }
    procedure CallMyFuncEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Res) method.
      @exclude }
    procedure CallMyFuncError;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Res,E,Handled) method.
      @exclude }
    procedure CallMyResultEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Res,E) method.
      @exclude }
    procedure CallMyResultErrorEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Data) method.
      @exclude }
    procedure CallMyFuncPrepEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Params) method.
      @exclude }
    procedure CallMyObjCreateEvent;

    { AfterDestroy event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerAfterDestroy; virtual;

    { BeforeCreate event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerBeforeCreate; virtual;

    { ReadyToRelease event,
      needs to be implemented by connection component,
      so it can be mapped to a connection provider's trigger.
      @exclude}
    procedure TriggerReadyToRelease; virtual; abstract;

    { Connecting event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerConnecting; virtual;
    // @exclude
    procedure CallConnecting; virtual;

    { Connect event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerConnect; virtual;
    // @exclude
    procedure CallConnect; virtual;

    { Disconnecting event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDisconnecting; virtual;
    // @exclude
    procedure CallDisconnecting; virtual;

    { Disconnect event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDisconnect; virtual;
    // @exclude
    procedure CallDisconnect; virtual;

    { DataOut event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataOut; virtual;
    // @exclude
    procedure CallDataOut; virtual;

    { DataIn event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataIn; virtual;
    // @exclude
    procedure CallDataIn; virtual;

    { LastWrite event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerLastWrite; virtual;
    // @exclude
    procedure CallLastWrite; virtual;

    { DataSent event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataSent; virtual;
    // @exclude
    procedure CallDataSent; virtual;
    // @exclude
    procedure CallReadyToSend; virtual;

    { DataReceived event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataReceived; virtual;
    // @exclude
    procedure CallDataReceived; virtual;

    // @exclude
    procedure CallAfterManualRead; virtual;

    { DataLost event (used by UDP only),
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataLost; virtual;
    // @exclude
    procedure CallDataLost; virtual;

    { Exception event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerException(E:Exception); virtual;
    // @exclude
    procedure CallException(E:Exception); virtual;

    { Called to raise exceptions
      @exclude }
    procedure Error(const Err:String);

    { Used by the Timeout component to trigger an internal disconnect,
      which will result in a reconnect, if reconnect parameters are set.
      @exclude }
    procedure InternalDisconnect; virtual;

    // @exclude
    function InsideEvent:boolean;

    // @exclude
    function LeavingEvent:boolean;

    // @exclude
    function GetRequestInserted:boolean; virtual;

    // @exclude
    function GetRequest:TRtcRequest; virtual;

    // @exclude
    function GetResponse:TRtcResponse; virtual;

    // @exclude
    function GetSession:TRtcSession; virtual;

    // @exclude
    procedure SetRequest(const Value:TRtcRequest); virtual;

    // @exclude
    procedure SetResponse(const Value:TRtcResponse); virtual;

    // @exclude
    procedure SetReconnectOn(const Value: TRtcReconnectParam); virtual;

    // @exclude
    function GetReconnectOn:TRtcReconnectParam; virtual;

    // @exclude
    procedure SetRestartOn(const Value: TRtcRestartParam); virtual;

    // @exclude
    function GetRestartOn:TRtcRestartParam; virtual;

    { This event will be triggered every time this connection component's
      buffer is completely empty and the other side has just become ready to
      accept new data. It is good to wait for this event before starting
      to send data out, even though you can start sending data directly
      from the @Link(OnConnect) event.
      @html(<br><br>)

      By responding to this event and sending the data only after it was
      triggered, you avoid keeping the data in the send buffer, especially
      if the data you are sending is being read from a file on disk,
      which wouldn't occupy any memory until loaded. }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;

    { This event will be triggered every time a chunk of your data
      prepared for sending has just been sent out. To know
      exactly how much of it is on the way, use the @Link(DataOut) property.
      @html(<br><br>)

      NOTE: Even though data has been sent out, it doesn't mean that
      the other side already received it. It could also be that connection will
      break before this package reaches the other end. }
    property OnDataOut:TRtcNotifyEvent read FOnDataOut write FOnDataOut;

    { This event will be triggered every time a chunk of data
      has just come in (received). To know exactly how much of it
      has just arrived, use the @Link(DataIn) property. }
    property OnDataIn:TRtcNotifyEvent read FOnDataIn write FOnDataIn;

    { This event will be triggered when all data prepared for sending
      has been sent out and the sending buffer has become empty again.
      @html(<br><br>)

      When sending large data blocks, try slicing them in small chunks,
      sending a chunk at a time and responding to this event to prepare
      and send the next chunk. This will keep your memory needs low. }
    property OnDataSent:TRtcNotifyEvent read FOnDataSent write FOnDataSent;

    { When this event triggers, it means that the other side has sent you
      some data and you can now read it. Check the connection component's
      description to see which properties and methods you can use
      to read the data received. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

  public // CLIENT & SERVER

    { DO NOT CALL THIS CONSTRUCTOR DIRECTLY!!!
      Use the 'NEW' class function to create a new object!!! }
    constructor Create(AOwner:TComponent); override;

    { DO NOT CALL THIS DESTRUCTOR DIRECTLY!!!
      DO NOT CALL 'FREE' DIRECTLY!!!
      Use the @Link(Release) method to destroy this object. }
    destructor Destroy; override;

    { Release the connection provider }
    procedure ReleaseProvider;

    { Returns TRUE if connection was closed because of a Timeout (set using the "Timeout" property). }
    function TimedOut: boolean;

    { ISAPI SERVER: Returns TRUE if this is a Server connection running as an extension or a plug-in to another Server.  @html(<br><br>)
      OTHER: Always returns FALSE. }
    function isExtension:boolean; virtual;

    { CLIENT: Returns TRUE if Client is CONNECTED and 
              ready to send/receive data. @html(<br><br>)
      SERVER: Always returns FALSE. }
    function isConnected:boolean; virtual;

    { CLIENT: Returns TRUE if Client is Connected, Connecting or
      waiting to start the next RECONNECT attempt. @html(<br><br>)
      SERVER: Always returns FALSE. }
    function isConnecting:boolean; virtual;

    { SERVER: Returns TRUE if this is a Server listener and it is listening. @html(<br><br>) 
      CLIENT: Always returns FALSE. }
    function isListening:boolean; virtual;

    { SERVER: Returns TRUE. @html(<br><br>)
      CLIENT: Returns FALSE. }
    function isServer:boolean; virtual;

    { SERVER: Returns TRUE if this is a Server-side connection opened by a CLIENT,
              Returns FALSE if this is the Server listener. @html(<br><br>)
      CLIENT: Always returns FALSE. }
    function isClient:boolean; virtual;

    { Needs to be called by connection components to signalize
      that component is entering a state where a user-defined
      event will be called. This is to prevent users from
      destroying the connection component from inside an event
      handler and to handle the Disconnect, Connect, Relase and
      other methods that affect connection status.
      @exclude }
    procedure EnterEvent; virtual;

    { Needs to be called by connection components to signalize
      that component is leaving a state where a user-defined
      event was called. This is to prevent users from
      destroying the connection component from inside an event
      handler and to handle the Disconnect, Connect, Relase and
      other methods that affect connection status.
      @exclude }
    procedure LeaveEvent; virtual;

    { If you create connection components at runtime,
      allways use the 'NEW' class function to create the connection
      object and 'RELEASE' procedure to free the connection object.
      @html(<br><br>)

      To make the code you write compatible with all new versions of Delphi,
      and to make sure that object is not released while it is still being used
      by internal connection component mechanisms,
      ONLY USE this 'Release' procedure to free any connection component and
      release all the resources that are used by the component.
      @html(<br><br>)

      After calling Release, you should also set the variable to NIL,
      in which you have stored the connection component. }
    procedure Release;

    { All native RTC connection components know when they get
      disconnected from peer and also trigger the events required.
      @html(<br><br>)

      But, there may be some third-party connection components coming,
      which will not know when a connection was closed by peer
      (for example, different implementations of blocking TCP/IP).
      @html(<br><br>)

      The @name method is here to cover such cases and give the connection
      provider means to write a method for checking if the connection is
      still active. In such cases, after calling the Check procedure,
      if the connection is still active, nothing will happen. But if the
      connection is dead, connection will be closed by the connection provider. }
    procedure Check; virtual;

    { When using "Request.ManualRead=TRUE" or "Response.ManualRead=TRUE",
      make sure to call this method as the last line of the event where
      the Read method was used - if it was not called from "OnDataReceived". }
    procedure AfterManualRead; virtual;

    { *1.) If this is a listening connection,
      stop listening and close all connections open to this listener (gracefuly).
      All events that need to be triggered will be triggered.
      @html(<br><br>)

      *2.) If this is a client connection (initiated by this or the other side),
      close the connection (gracefuly).
      All events that need to be triggered will be triggered. }
    procedure Disconnect; virtual;

    { Is the connection in the process of closing (disconnecting)?
      Do not try to send any more data out if this is @true. }
    function isClosing:boolean; virtual;

    { Get this connection's Peer Address (to which Address/IP are we connected?) }
    function PeerAddr:RtcString; virtual;

    { Get this connection's Peer Port (to which Port are we connected?) }
    function PeerPort:RtcString; virtual;

    { Get this connection's Local Address (on which local IP are we?) }
    function LocalAddr:RtcString; virtual;

    { Get this connection's Local Port (on which local Port are we?) }
    function LocalPort:RtcString; virtual;

    { (String) Get this connection's Peer Address. }
    function sPeerAddr:String; deprecated;

    { (String) Get this connection's Peer Port. }
    function sPeerPort:String; deprecated;

    { (String) Get this connection's Local Address. }
    function sLocalAddr:String; deprecated;

    { (String) Get this connection's Local Port. }
    function sLocalPort:String; deprecated;

    { HTTP/DATA CLIENT: Skip all requests
      (RequestAborted events will NOT BE triggered!!!). @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure SkipRequests; virtual;

    { HTTP/DATA CLIENT: Cancel all requests
      (will fire all RequestAborted events). @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure CancelRequests; virtual;

    { HTTP/DATA CLIENT: Check request count
      (number of requests waiting to be processed). @html(<br><br>)
      OTHER: Not supported - always returns 0. }
    function RequestCount:integer; virtual;

    { @exclude
      HTTP/DATA CLIENT - Internal function: Post a new Request Object.
      When posting from inside a RTC event or a remote function,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption. @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure PostRequest(var myReq; FromInsideEvent:boolean=False); virtual;

    { @exclude
      HTTP/DATA CLIENT - Internal function: Insert a Request before active request.
      This procedure may ONLY be called from BeginRequest event
      to place another request before the active request.  @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure InsertRequest(const Req:TObject); virtual;

    { Return the total number of active connections, counting all connections that
      are opened to this process by any TRtcConnection class (all servers and clients). }
    function TotalConnectionCount:integer; virtual;

    { Return the number of active connections,
      open by our Client connection components. }
    function TotalClientConnectionCount:integer; virtual;

    { Return the number of active connections,
      open to our Servers connection components. }
    function TotalServerConnectionCount:integer; virtual;

    { Post a job to connection thread's job queue.
      @param(Job - If using existing connection components (not extenting them),
      'Job' object has to be of @Link(TRtcJob) type,
      or else the 'Unknown Job' exception will be raised
      from within the thread, which will close the connection.) }
    function PostJob(var myJob; HighPriority:boolean=False; ForceThread:boolean=False):boolean; virtual;

    { Post a job to "Thr" thread's job queue.
      @param(Job - If using existing connection components (not extenting them),
      'Job' object has to be of @Link(TRtcJob) type,
      or else the 'Unknown Job' exception will be raised
      from within the thread, which will close the connection.) }
    function PostJobTo(Thr:TRtcThread; var myJob; HighPriority:boolean=False; ForceThread:boolean=False):boolean; virtual;

    { Post a job to call the envent "Evnt" from connection thread's job queue,
      passing this connection component as the "Sender" parameter to the event. }
    function PostEvent(Evnt:TRtcNotifyEvent; HighPriority:boolean=False; ForceThread:boolean=False):boolean; virtual;

    { Post a job to call the envent "Evnt" from "Thr" thread's job queue,
      passing this connection component as the "Sender" parameter to the event. }
    function PostEventTo(Thr:TRtcThread; Evnt:TRtcNotifyEvent; HighPriority:boolean=False; ForceThread:boolean=False):boolean; virtual;

    { Get connection component's virtual thread }
    function Thread:TRtcThread; virtual;

    { If we are currently inside any RTC background thread, the "Sync" method will call the 'Event'
      synchronized from the Main Thread, wait for the event to finish and then returns TRUE.
      If we are already in the Main Thread, the "Sync" method does nothing and returns FALSE.
      You can use this method at the top of any RTC events which have to
      access the GUI (Graphical User Interface, everything visual).
      If you want to check wether your event is being executed from
      inside the Main Thread, you can use the @Link(inMainThread) method.
      @param(Event - TRtcNotifyEvent method to be called synchronized, from the Main Thread.)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcNotifyEvent):boolean; overload;

    { Same as Sync(Event), with a difference that you can synchronize @Link(TRtcErrorEvent)
      events with this call and 'Err' will be used as the Exception parameter when calling the 'Event'.
      @param(Event - TRtcErrorEvent method to be called synchronized, from the MainThread)
      @param(Err - Exception object to be passed to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcErrorEvent; Err:Exception):boolean; overload;

    { Same as Sync(Event), with a difference that you can synchronize @Link(TRtcFunctionCallEvent)
      events with this call and 'Param' and 'Res' will be passed as parameters when calling the 'Event'.
      @param(Event - TRtcFunctionCallEvent method to be called synchronized, from the MainThread)
      @param(Par - TRtcFunctionCall object, containing all function call information)
      @param(Res - TRtcValue object, ready to receive the result information)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcFunctionCallEvent; Par:TRtcFunctionInfo; Res:TRtcValue):boolean; overload;

    { Same as Sync(Event), with a difference that you can synchronize @Link(TRtcFunctionErrorEvent)
      events with this call and 'Param', 'Res', 'E' and 'Handled' will be passed as parameters.
      @param(Event - TRtcFunctionErrorEvent method to be called synchronized, from the MainThread)
      @param(Par - TRtcFunctionCall object, containing all function call information)
      @param(Res - TRtcValue object, ready to receive the result information)
      @param(E - Exception object)
      @param(Handled - set to TRUE if the Exception is now handled)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcFunctionErrorEvent; Par:TRtcFunctionInfo; Res:TRtcValue; E:Exception; var Handled:boolean):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcResultEvent)
      events with this call and 'Res' will be passed as the result parameter when calling the 'Event'.
      @param(Event - TRtcResultEvent method to be called synchronized, from the MainThread)
      @param(Data - TRtcValue object, containing the information which was sent to the server, which has produced the "Res")
      @param(Res - TRtcValue object, containing the result information)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcResultEvent; Data:TRtcValue; Res:TRtcValue):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcResultEvent)
      events with this call and 'Res' will be passed as the result parameter when calling the 'Event'.
      @param(Event - TRtcResultErrorEvent method to be called synchronized, from the MainThread)
      @param(Data - TRtcValue object, containing the information which was sent to the server, which has produced the "Res")
      @param(Res - TRtcValue object, containing the result information)
      @param(E - Exception object, containing the exception raised)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcResultErrorEvent; Data:TRtcValue; Res:TRtcValue; E:Exception):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcFunctionPrepareEvent)
      events with this call and 'Data' will be passed as the Data parameter when calling the 'Event'.
      @param(Event - TRtcFunctionPrepareEvent method to be called synchronized, from the MainThread)
      @param(Data - TRtcValue object, which you should use to prepare the remote function call)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcFunctionPrepareEvent; Data:TRtcValue):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcUserEvent)
      events with this call and the 'Obj' parameter will be passed to the 'Event'.
      @param(Event - TRtcUserEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcUserEvent; Obj:TObject):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcUserDataEvent)
      events with this call and the 'Obj' and 'Data' parameters will be passed to the 'Event'.
      @param(Event - TRtcUserDataEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @param(Data - Value object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcUserDataEvent; Obj:TObject; Data:TRtcValue):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcCustomEvent)
      events with this call and the 'Obj' parameter will be passed to the 'Event'.
      @param(Event - TRtcCustomEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcCustomEvent; Obj:TObject):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcCustomDataEvent)
      events with this call and the 'Obj' and 'Data' parameters will be passed to the 'Event'.
      @param(Event - TRtcCustomDataEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @param(Data - Value object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcCustomDataEvent; Obj:TObject; Data:TRtcValue):boolean; overload;

    { Same as Sync(Event), with the difference that you can synchronize @Link(TRtcObjectCreateEvent)
      events with this call and 'Param' parameter will be passed to the 'Event'.
      @param(Event - TRtcObjectCreateEvent method to be called synchronized, from the Main Thread.)
      @param(Param - Object Constructor parameters)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcObjectCreateEvent;
                  Param:TRtcObjectCall):boolean; overload;

    { Check current connection state.
      There is no real need to use this property to check your connection state,
      since events will be triggered when the state changes. }
    function State:TRtcConnectionState;

    { You can use this method to check if you are inside the Main thread,
      from which drawing and writing to the GUI is allowed. If true,
      you can directly access the GUI. If false, you have to use the
      Sync() method to call this or some other event synchronized,
      from where you can use the drawing routines. }
    function inMainThread:boolean;

    { You can use this method to check if your connection object is currently
      inside its Thread. If true, jobs do not have to be posted,
      they can be called directly. To check if you are allowed to
      access GUI (drawing or writing to the screen or canvas),
      use the @Link(inMainThread) function. }
    function inThread:boolean; virtual;

    { HTTP/DATA SERVER: Request handler has to call Accept before it starts
      processing the request, so that all further events remain mapped to the
      active event handlers and don't switch before a reply has been sent. @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure Accept; virtual;

    { HTTP/DATA SERVER: Find an existing Session with this ID.
      If Session with this ID does not exist,
      or session has expired or session is currently locked,
      returns FALSE. Otherwise, prepares the Session variable
      for use and returns TRUE. @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    function FindSession(const ID:RtcString):boolean; virtual;

    { HTTP/DATA SERVER: If there is a session with this ID, returns TRUE,
      even if that session is locked. @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    function HaveSession(const ID:RtcString):boolean; virtual;

    { HTTP/DATA SERVER: If you do not need the Session anymore and do not want
      to keep the session locked until request completes, you can release the
      Session Lock by calling "UnLockSession". After this call, you will no longer
      have access to the Session object, until you lock it again using FindSession.  @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure UnLockSession; virtual;

    { HTTP/DATA SERVER: Create a new Session, with a new and unique Session ID.  @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure OpenSession(LockType:TRtcSessionLockType=sesFwdLock); virtual;

    { HTTP/DATA SERVER: If there is a Session with this "ID",
      returns TRUE and closes the session.  @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    function CloseSession(const ID:RtcString):boolean; virtual;

    { HTTP/DATA SERVER: Total number of open Sessions. @html(<br><br>)
      OTHER: Not supported - always returns 0. }
    function TotalSessionsCount:cardinal; virtual;

    { HTTP/DATA SERVER: Total number of Sessions currently locked.
      A session is locked after a call to FindSession() and
      unlocked after the Event is done executing.   @html(<br><br>)
      OTHER: Not supported - always returns 0. }
    function TotalSessionsLocked:cardinal; virtual;

    { HTTP/DATA SERVER: Total number of Sessions currently unlocked.
      A session is locked after a call to FindSession() and
      unlocked after the Event is done executing.   @html(<br><br>)
      OTHER: Not supported - always returns 0. }
    function TotalSessionsUnlocked:cardinal; virtual;

    { HTTP/DATA SERVER: Activate current Session's Object Manager for use in the current Thread.
      Call with "False" as parameter if the Object Manager should already exist,
      or with "True" if a new Object Manager should be created if it wasn't
      already and a new Session opened if no Session is currently active.  @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure ActivateObjectManager(xCreate:boolean=True); virtual;

    { HTTP/DATA SERVER: Returns the "Linked Objects" Manager of the currently active Session.
      Returns NIL if the active Session does NOT have an "Object Manager"
      assigned or if no Session was activated for the current request.  @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    function GetObjectManager:TRtcRemoteObjectManager; virtual;

    { Read function used to read a String of byte-sized characters received.
      It may ONLY be called from inside your @Link(OnDataReceived) event handler.
      Its behavior depends on the connection component that implements it. }
    function Read:RtcString; virtual;

    { Write function used to send a String of byte-sized characters out.
      Its behavior depends on the connection component that implements it. }
    procedure Write(const s:RtcString); overload; virtual;

    { Write function used to force sending HTTP header out. }
    procedure Write; overload;

    { ReadEx function is used to read raw byte data received. It may ONLY be called
      from inside your @Link(OnDataReceived) event handler.
      Its behavior depends on the connection component that implements it. }
    function ReadEx:RtcByteArray; virtual;

    { WriteEx function used to send raw byte data out.
      Its behavior depends on the connection component that implements it. }
    procedure WriteEx(const s:RtcByteArray); overload; virtual;

    { PeekEx function can be used to access reading buffers containing raw
      content byte data received, without removing the content body from
      receiving buffers, allowing PeekEx to be used any number of times
      without breaking any code which requires access to that content and
      making it useful for content logging and monitoring, but ... the
      PeekEx method may ONLY be use from inside OnDataReiceved, OnPeekRequest
      and OnPeekResponse events. It also HAS TO be used BEFORE any calls to
      the Read or ReadEx, because these methods will clear reading buffers! }
    function PeekEx:RtcByteArray; virtual;

    { "PokeEx" procedure can be used to REPLACE the content currently
      stored in reading buffers, making this new content accessible
      to anyone calling the next "PeekEx", "Read" or "ReadEx" method.
      "PokeEx" method may ONLY be use from inside "OnDataReiceved",
      "OnPeekRequest" and "OnPeekResponse" events. }
    procedure PokeEx(const s:RtcByteArray); virtual;

    { HTTP/DATA CLIENT & SERVER: Flush all buffered data. @html(<br>)
      When using 'Write' without calling 'WriteHeader' before, all data
      prepared by calling 'Write' will be buffered until your event
      returns to its caller (automatically upon your event completion) or
      when you first call 'Flush'. @html(<br>)
      Flush will check if Request/Response.ContentLength is set and if not,
      will set the content length to the number of bytes buffered. @html(<br>)
      Flush does nothing if WriteHeader was called for this response. @html(<br><br>)
      OTHER: Not supported - call will be ignored. }
    procedure Flush; virtual;

    { HTTP/DATA CLIENT & SERVER: Send Request/Response Headers out.  @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure WriteHeader(SendNow:boolean=True); overload; virtual;

    { HTTP/DATA CLIENT & SERVER: You can call WriteHeader with empty 'HeaderText' parameter
      to tell the component that you do not want any HTTP header to be sent. @html(<br><br>)
      OTHER: Not supported - raises an Exception if called! }
    procedure WriteHeader(const HeaderText: RtcString; SendNow:boolean=True); overload; virtual;

    { @name (Read-Only) can be used from the @Link(OnDataOut) event,
      to check how much data has now been sent out. The value of this
      property changes with every @Link(OnDataOut) event and should
      only be read from your @Link(OnDataOut) event handler. }
    property DataOut:cardinal read FDataOut;

    { @name (Read-Only) can be used from the @Link(OnDataIn) event,
      to check how much data has just arrived. The value of this
      property changes with every @Link(OnDataIn) event and should
      only be read from your @Link(OnDataIn) event handler. }
    property DataIn:cardinal read FDataIn;

    { Total number of bytes read from the other side (Peer/Remote) through this connection. }
    property ReadCount:int64 read FReadCount;
    { Total number of bytes sent to the other side (Peer/Remote) through this connection. }
    property WriteCount:int64 read FWriteCount;

    { This property will be set to TRUE if the connection was established,
      even though our connection limit was reached (or over the limit).
      @html(<br><br>)

      You can check the number of total conncetions open using the
      @Link(TotalConnectionCount) function. If you find this connection
      should stay open (even though the limit was reached when it connected),
      change this property to FALSE, to handle it like any other connection.
      @html(<br><br>)

      Note: This property is not being used by any TRtcConnection component
      or its descendants. It is only intended for your use. Even if
      the overLimit property is TRUE, it will not affect this connection's behavior. }
    property OverLimit:boolean read FOverLimit write FOverLimit;

    { Additional connection component information.
      @html(<br><br>)

      You can use this property as you see fit.
      The only purpose of the Info property is to give
      the component user ways to store additional
      information about this conncetion inside the
      connection component itself, without having to
      create and maintain separate data structures. }
    property Info:TRtcInfo read FInfo;

    { (String) Returns Server Address to connect to for the Client,
      or Listening address (Bind) for the Server. }
    function sServerAddr:String; deprecated;

    { (String) Returns Port on the Host to connect to for the Client,
      or Listening Port (Bind) for the Server. }
    function sServerPort:String; deprecated;

    { HTTP/DATA SERVER: Current Request's Session info.
         Before you can access the Session for the Request,
         you have to find the appropriate Session by using the
         FindSession function or create a new session by calling
         the OpenSession method. @html(<br><br>)
      HTTP/DATA CLIENT: This connection's Session info.
        If you will be using multiple client connections and
        need to store session information, you have to start a
        separate session for every client connection. @html(<br><br>)
        There is a difference between this DataClient's Session object and
        the DataServer's Session object. DataClient's Session object stays
        permamently defined all the time, from the point of object creation
        to its destruction. @html(<br><br>)
      OTHER: Not supported - raises an Exception if accessed! }
    property Session:TRtcSession read GetSession;

    { You can set all timeout parameters for the clients underlying API connection or
      default timeout periods for all client connections of the server connection component
      using this property. Check @Link(TRtcTimeoutsOfAPI) for more information. }
    property TimeoutsOfAPI: TRtcTimeoutsOfAPI read FTimeoutsOfAPI write SetTimeoutsOfAPI;

    { HTTP/DATA CLIENT & SERVER: Access to current request information.
      Use Request property to prepare the request header.
      Here you can set all header variables and parameters.
      If there is no content body to send out (only header), you will at
      least have to call 'WriteHeader' or 'Write' without parameters once. @html(<br><br>)
      OTHER: Not supported - raises an Exception if accessed! }
    property Request:TRtcRequest read GetRequest write SetRequest;

    { HTTP/DATA CLIENT & SERVER: Access to current response information.
      Use Response property to read the response information received.
      Here is all the info that was available in response header.
      To read response's body, use the Read function. @html(<br><br>)
      OTHER: Not supported - raises an Exception if accessed!}
    property Response:TRtcResponse read GetResponse write SetResponse;

    { HTTP/DATA CLIENT: Another request has just been inserted before this one?@html(<br><br>)
      OTHER: Not supported - always returns FALSE. }
    property RequestInserted:boolean read GetRequestInserted;

    { CLIENT: By setting properties in @name to True, you do not have to 
      do anything special to initiate a Reconnect after specific events occur. @html(<br><br>)
      OTHER: Not supported - raises an Exception if accessed! }
    property ReconnectOn:TRtcReconnectParam read GetReconnectOn write SetReconnectOn;

    { SERVER: By setting properties in @name to True, you do not have to 
      do anything special to initiate a renewed Listen after specific events occure.
      For more information, check @Link(TRtcRestartParam). @html(<br><br>)
      OTHER: Not supported - raises an Exception if accessed!}
    property RestartOn:TRtcRestartParam read GetRestartOn write SetRestartOn;

  private // WEB SOCKETS

    FWSFrameQueued:tXObjList;
    FWSFrameOther:tXObjList;
    FWSFrameIN,
    FWSFrameOUT:TRtcWSFrame;

    function ws_GetFrame(const iName: RtcWideString): TRtcWSFrame;
    procedure ws_SetFrame(const iName: RtcWideString; const Value: TRtcWSFrame);

    function ws_GetTemp(const iName: RtcWideString): TRtcWSFrame;
    procedure ws_SetTemp(const iName: RtcWideString; const Value: TRtcWSFrame);

    function ws_GetFrameIn: TRtcWSFrame;

    // Adds "iFrame" to the "sending Frames" list
    procedure ws_SendingFrame(iFrame:TRtcWSFrame);
    // Adds "iFrame" to the "other Frames" list
    procedure ws_OtherFrame(iFrame:TRtcWSFrame);
    // Removes "iFrame" from all our "Frames" list(s).
    procedure ws_RemoveFrame(iFrame:TRtcWSFrame);
    // Adds "iFrame" at the bottom of the sending queue.
    function ws_AddFrameToQueue(iFrame:TRtcWSFrame; const iName:RtcWideString=''):boolean;
    // Adds "iFrame" to the sending queue if NOTHING is currently being sent and the sending QUEUE is EMPTY.
    function ws_AddFrameToQueueIfIdle(iFrame:TRtcWSFrame; const iName:RtcWideString=''):boolean;

  protected // WEB SOCKETS

    FWS:TRtcWSManager;
    FWebSocket,
    FWebSocketData:boolean;

    // Clear all "finished" Temporary Frames used for Reading (called after OnDataReceived)
    function ws_ClearTempFrames:boolean;
    // Send the Web Socket Frames out from the SENDING Queue, if the last Frame is finished
    procedure ws_SendNextQueuedFrame(Cleanup:boolean);
    // Clear the Frames sending Queue if anything is waiting
    function ws_ClearSendingQueue:boolean;
    // Clear all Queued, Sending and Other Frames
    procedure ws_Clear;

  public // WEB SOCKETS

    { WEB SOCKET: Is this an upgraded Web Socket connection? }
    property isWebSocket:boolean read FWebSocket;

    { WEB SOCKET MANAGER:
      Web Socket Manager where this connection is being managed.
      Assigned automatically by the first Web Socket Manager where
      this connection was added and cleared automatically by the
      same Web Socket Manager when the connection is removed. }
    property wsManager:TRtcWSManager read FWS;

    { WEB SOCKET: Clear this Web Socket connections "Sending Queue".
      Returns TRUE if this is a Web Socket connection (sending queue will be cleared),
      or FALSE if this is NOT a Web Socket connection or the connection is already closed. }
    function wsClearSendingQueue:boolean; overload;

    { WEB SOCKET:
      Disconnect this Web Socket connection.
      If this is NOT an upgraded Web Socket connection, this method call is ignored.
      Returns TRUE if this is an established Web Socket connection (it will be closed),
      or FALSE if this call has failed (not a Web Socket connection or already closed). }
    function wsDisconnect:boolean; overload;

    { WEB SOCKET:
      Add Frame "iFrame" to this connections sending queue. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      this component and will be available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using Send, this component becomes the *Owner* of the "iFrame" object.
      The "iFrame" object will be managed and destroyed by the component!
      Any external pointers or references to this object should be set to NIL. @html(<br><br>)

      Returns TRUE if "iFrame" object was added, or FALSE if this call has failed,
      but the "iFrame" object should NOT be used after this call in either case! @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSend(iFrame:TRtcWSFrame; const iName:RtcWideString=''):boolean; overload;

    { WEB SOCKET:
      Add more "Payload" data to "iFrame", which has already been placed
      into the sending queue and is now being sent or is waiting in the queue.
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns TRUE if "vPayload" was added to "iFrame", FALSE if this call failed. }
    function wSendMore(iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False):boolean; overload;

    { WEB SOCKET:
      Add more "Payload" data to "iFrame", which has already been placed
      into the sending queue and is now being sent or is waiting in the queue.
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns TRUE if "vPayload" was added to "iFrame", FALSE if this call failed. }
    function wSendMore(iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False):boolean; overload;

    { WEB SOCKET:
      Add a new Frame to this connections sending queue, initialized with:
      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to Send( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns TRUE if the Frame was added to the sending queue, FALSE if this call failed. }
    function wSend(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):boolean; overload;

    { WEB SOCKET:
      Add a new Frame to this connections sending queue, initialized with:
      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to Send( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns TRUE if the Frame was added to the sending queue, FALSE if this call failed. }
    function wSend(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):boolean; overload;

    { WEB SOCKET:
      Add a new Frame to this connections sending queue, initialized with:
      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True),PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns TRUE if the Frame was added to the sending queue, FALSE if this call failed. }
    function wSend(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):boolean; overload;

    { WEB SOCKET:
      Add Frame "iFrame" to this connections sending queue,
      but ONLY if this connections sending queue is EMPTY and
      NOTHING is being sent out through the connection. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      this component and will be available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using Send, this component becomes the *Owner* of the "iFrame" object.
      The "iFrame" object will be managed and destroyed by the component!
      Any external pointers or references to this object should be set to NIL. @html(<br><br>)

      Returns TRUE if "iFrame" object was added, or FALSE if this call has failed,
      but the "iFrame" object should NOT be used after this call in either case! @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendIfIdle(iFrame:TRtcWSFrame; const iName:RtcWideString=''):boolean; overload;

    { WEB SOCKET:
      Add a new Frame to this connections sending queue, initialized
      with the values listed below, but ONLY if this connections sending queue
      is EMPTY and NOTHING is currently being sent out through this connection: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side
      connections(with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to Send( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns TRUE if the Frame was added to the sending queue, FALSE if this call failed. }
    function wSendIfIdle(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):boolean; overload;

    { WEB SOCKET:
      Add a new Frame to this connections sending queue, initialized
      with the values listed below, but ONLY if this connections sending queue
      is EMPTY and NOTHING is currently being sent out through this connection: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to Send( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns TRUE if the Frame was added to the sending queue, FALSE if this call failed. }
    function wSendIfIdle(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):boolean; overload;

    { WEB SOCKET:
      Add a new Frame to this connections sending queue, initialized
      with the values listed below, but ONLY if NOTHING is currently being
      sent through this connection and the sending queue is empty: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns TRUE if the Frame was added to the sending queue, FALSE if this call failed. }
    function wSendIfIdle(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):boolean; overload;

    { WEB SOCKET MANAGER:
      First connection (ID, 0 = no connections) }
    function wsFirst:RtcIntPtr;
    { WEB SOCKET MANAGER:
      Next connection (ID, 0 = no more connecitons) }
    function wsNext(id:RtcIntPtr):RtcIntPtr;
    { WEB SOCKET MANAGER:
      Prior connection (ID, 0 = no more connections) }
    function wsPrior(id:RtcIntPtr):RtcIntPtr;
    { WEB SOCKET MANAGER:
      Last connection (ID, 0 = no connections) }
    function wsLast:RtcIntPtr;

    { WEB SOCKET MANAGER:
      Connection count }
    function wsCount:integer;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of the connection "id". @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns 1 if "iFrame" object was added to the queue, or 0 if this call fails. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSend(id:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add more "Payload" data to "iFrame", which has already been placed into the
      sending queue for connection "id" and is now being sent our waiting to be sent.
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns 1 if "vPayload" was added to "iFrame", or 0 if this call failed. }
    function wSendMore(id:RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False):integer; overload;

    { WEB SOCKET MANAGER:
      Add more "Payload" data to "iFrame", which has already been placed into the
      sending queue for connection "id" and is now being sent our waiting to be sent.
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns 1 if "vPayload" was added to "iFrame", or 0 if this call failed. }
    function wSendMore(id:RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side
      connections(with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of the connection "id", but ONLY if NOTHING
      is being sent through connection 'id' and the sending queue of connection "id" is EMPTY. @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns 1 if "iFrame" object was added to the queue, or 0 if this call fails. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendIfIdle(id:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side
      connections(with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of all connections managed here.
      The original "iFrame" object is destroyed automatically by this method call. @html(<br><br>)

      If called with iName<>'', a copy of "iFrame" will be created for
      every connection component and stored in the "Frame[iName]" property
      and available in all events triggered by the connection component,
      where it will be accessible using "Sender.Frame[iName]" and
      will be managed (and destroyed) by the "Sender.Frame" property.  @html(<br><br>)

      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendToAll(iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all managed connections,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side
      connections(with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all managed connection,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all managed connections,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of all connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY. @html(<br><br>)

      The original "iFrame" object is destroyed automatically by this method call. @html(<br><br>)

      If called with iName<>'', a copy of "iFrame" will be created for
      every connection component and stored in the "Frame[iName]" property
      and available in all events triggered by the connection component,
      where it will be accessible using "Sender.Frame[iName]" and
      will be managed (and destroyed) by the "Sender.Frame" property.  @html(<br><br>)

      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendToIdle(iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side
      connections(with a randomly generated "MaskingKey"),
      with everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of all connections managed by this
      Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"). @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns the number of connections where the "iFrame" object was
      added to the queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.
      On all Frames added to the sending queue of a Server-side connections,
      the "iFrame.Masked" property will be automatically set to FALSE and the
      "iFrame.MaskingKey" property will be cleared to all zeroes. }
    function wSendToOthers(xid:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side
      connections(with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else = 0.  @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Disconnect the Web Socket connection "id".
      Returns 1 if a connection with "id" was found (and will be closed),
      or 0 if this call has failed (connection could not be found). }
    function wsDisconnect(id:RtcIntPtr):integer; overload;

    { WEB SOCKET MANAGER:
      Disconnect all Web Socket connections managed here.
      Returns the number of connections found (and now being closed),
      or 0 if this call has failed (no connections found). }
    function wsDisconnectAll:integer;

    { WEB SOCKET MANAGER:
      Clear the SENDING QUEUE for the Web Socket connection "id".
      Returns 1 if a connection with "id" was found (queued Frames will be removed),
      or 0 if this call has failed (connection could not be found).
      This does NOT cancel Web Socket Frames already being SENT out.
      It only removes Web Socket Frames waiting in the queue! }
    function wsClearSendingQueue(id:RtcIntPtr):integer; overload;

    { WEB SOCKET MANAGER:
      Clear the SENDING QUEUES of all Web Socket connections managed here.
      Returns 1 if a connection with "id" was found (queued Frames will be removed),
      or 0 if this call has failed (connection could not be found).
      This does NOT cancel Web Socket Frames already being SENT out.
      It only removes Web Socket Frames waiting in the queue! }
    function wsClearAllSendingQueues:integer;

    { WEB SOCKET MANAGER:
      If this connection is managed by a Web Socket Manager,
      the "WSGetID" method returns the "ID" of this connnection
      in our Web Socket Manager, which can be used with all "WS*"
      methods which require an "id" as the 1st parameter. }
    function wsGetID:RtcIntPtr;

    { WEB SOCKET: "wsTemp" property can be used to store pointers to TRtcWSFrame
      objects currently used for processing Web Socket Frames which are already
      stored in a connection component and should NOT be auto-freed here! }
    property wsTemp[const iName:RtcWideString]:TRtcWSFrame read ws_GetTemp write ws_SetTemp;

    { WEB SOCKET: "wsFrame" property is used to store named TRtcWSFrame
      objects currently used for sending or receiving Web Socket Frames,
      which should be auto-freed by this connection component.  }
    property wsFrame[const iName:RtcWideString]:TRtcWSFrame read ws_GetFrame write ws_SetFrame;

    { WEB SOCKET: "FrameOUT" property gives you direct access to the currently
      active Web Socket Frame object used for SENDING out Web Socket data,
      even if the Send() method was used with an empty "iName" parameter. @html(<br><br>)

      This property may ONLY be used from inside events triggered
      by this connection component. If this property returns NIL,
      it means that all Web Socket Frames have been sent and the
      Web Socket SENDING QUEUE is curently empty (done sending). }
    property wsFrameOUT:TRtcWSFrame read FWSFrameOUT;

    { WEB SOCKET: "FrameIN" property gives you direct access to the currently
      active Web Socket Frame object used for READING received Web Socket data.
      Using this property automatically creates a new Frame object on first
      read access and reads all content from receiving buffers to prepare the
      Frame for usage, eliminating the need to manually create a TRtcWSFrame
      instance for read access, or to manually use connection components
      "Read" or "ReadEx" method in combination with Frames "wfWrite" or "wfWriteEx"
      method to prepare the Frame for reading. This is an auto-created and
      auto-freed Frame object which is stored  separately without a "Name".  }
    property wsFrameIN:TRtcWSFrame read ws_GetFrameIn;

  published

    { Set the @name property to @True if you want your connection to use the
      Thread pooling mechanism, which is integrated into the RealThinClient
      library and can be used by all RTC connection components. To find out what
      more you need to keep in mind when working in multithreaded mode, check
      the @Link(TRtcConnection) description.
      @html(<br><br>)

      NOTE: This property is read only before you call 'Listen' for the server component
      or 'Connect' for the client component. Changing this property when a connection
      is open will have no effect on the component, until the next time you
      start the listener or open a new connection.
      @html(<br><br>)

      WARNING: To safely use your components in MultiThreaded mode, also check
      the Descriptions for @Link(TRtcConnection), @Link(Sync), @Link(inMainThread),
      @Link(inThread) and @Link(PostJob). }
    property MultiThreaded:boolean read FMultiThread write SetMultiThread default False;

    { You can set all timeout parameters for the client connection component or
      default timeout periods for all client connections of the server connection component
      using this property. Check @Link(TRtcTimeout) for more information. }
    property Timeout:TRtcTimeout read FTimeout write SetTimeout;

    { Server Address to connect to for the Client,
      Listening address (Bind) for the Server (leave empty to listen on all network cards). }
    property ServerAddr:RtcString read FAddr write FAddr;

    { Server Port to connect to for the Client,
      Listening Port (Bind) for the Server (never leave empty). }
    property ServerPort:RtcString read FPort write FPort;

    { Server "IP Version" prefference }
    property ServerIPV:RtcIPV read FIPV write FIPV default rtc_IPVDefault;

    { This event will be called when a new connection is waiting to be
      initialized. This is still BEFORE a connection is ready to read/send data. }
    property OnConnecting:TRtcNotifyEvent read FOnConnecting write FOnConnecting;

    { This event will be called when a succesful connection is being closed. }
    property OnDisconnecting:TRtcNotifyEvent read FOnDisconnecting write FOnDisconnecting;

    { This event will be called after a new connection
      has been succesfully established and is ready to be used.
      The event triggered will receive the client connection object as parameter.

      @html(<br><br>)
      * 1) If this was a client connection on which you called 'Connect' to
        attempt a new connection to a server, this event handler will
        be the first event to trigger after 'OnConnecting' and will
        receive the current connection object as parameter, only if the connection
        to the server was succesfull. If server was not available, 'OnConnectFail'
        event will be triggered instead. Also, in case of failure, the 'OnDisconnect'
        event will NOT be triggered.

      @html(<br><br>)
      * 2) If this was a listening connection (server), this event
        handler will receive a new connection object as parameter
        and be called directly after the 'OnClientConnect' and
        'OnConnecting' events. In case needed, you can change all the
        actual events to be triggered for every client connection from
        the 'OnClientConnect' event handler. But, it is not advisable
        to do so. It is better to have 1 event handler for all client connections
        that belong to a specific server and use the 'Sender' parameter to
        distinguish between different clients. }
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;

    { This event will be called when a prior working connection (for which you
      have allready received the @Link(OnConnect) event) just got disconnected,
      by either side:
      @html(<br>)
      1.) You closed it by calling @Link(Disconnect), or
      @html(<br>)
      2.) the other side closed it, or
      @html(<br>)
      3.) the connection simply colapsed. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;

    { This event is used to process exceptions that happen in the background,
      for example while sending data from buffer to the other side.
      @html(<br><br>)

      Normaly, no exceptions should happen in the background.
      If an exception happens in the background and you set the event handler
      for the OnException event, you would receive the exception object,
      so you can handle it if needed (for example, write it to a Log file).
      @html(<br><br>)

      If there is no handler for this event, background exceptions will be ignored. }
    property OnException:TRtcErrorEvent read FOnException write FOnException;
    end;

{ --- TRtcServer --- }

  { @Abstract(Restart Parameters for TRtcServer components)

    @name is tightly coupled with @Link(TRtcServer) component.
    It encapsulates the parameters used to define how a server listener
    should behave when there is a problem with the listening connection. }
  TRtcRestartParam = class(TRtcPersistent)
  private
    FListenLost:boolean;
    FListenError:boolean;
    FWait:integer;

  public
    { Will be created by TRtcServer component.
      @exclude }
    constructor Create;
    { Will be destroyed by TRtcServer component-
      @exclude }
    destructor Destroy; override;

    { @exclude }
    procedure AssignTo(Dest:TPersistent); override;

  published
    { Set ListenLost to TRUE if you want a restart listener on Listener Lost
        (listener closed by the system). }
    property ListenLost:boolean read FListenLost write FListenLost default False;

    { Set ListenError to TRUE if you want a restart listener on Listener Error
        (listener could not be started). }
    property ListenError:boolean read FListenError write FListenError default False;

    { Wait defines how long (in second) component should wait before it tries to restart the listener. }
    property Wait:integer read FWait write FWait default 0;
    end;

  { @Abstract(Basic Server-side connection component wrapper)

    @name publishes methods and handles all the tasks that are common to all
    Server connection components. All Server-side connection components
    are indirect descendants of TRtcServer and therefor inherit
    all its methods and properties. Since @name is also a direct descendant
    of @Link(TRtcConnection), it also inherits all methods and properties
    from @Link(TRtcConnection). For more information, check @Link(TRtcConnection).
    @html(<br><br>)

    All @Link(TRtcServer) descendant components automatically create and
    initialize a separate object (same type as the Server component) for
    every accepted Client connection. It also automatically releases the
    same object (which it created) when the client connection closes.
    This means that you will have a separate object for every client connection,
    without having to create, maintain or release it. All properties and events
    which you have defined for the TRtcServer component will be automatically
    copied into the new component, so that you do not have to do any special
    initialization to work with the connection object.
    @html(<br><br>)

    The components created automatically for each new accepted client connection
    will be of the same type as the main Server listener component. For example,
    if you created a @Link(TRtcTcpServer) component, set its properties and
    called @Link(TRtcServer.Listen), you will have access to a copy of that
    @Link(TRtcTcpServer) component, which will start its own new life from
    the second the connection is accepted by the underlying connection provider.
    @html(<br><br>)

    All events which you defined for the lisneter component, will automatically
    be copied to the new client connection component. Remember that this is
    NOT the @Link(TRtcClient) descendant component, it is a new @Link(TRtcServer)
    component, but its @Link(TRtcServer.isClient) function will return TRUE and
    @Link(TRtcServer.isListening) will return FALSE, which is a complete oposite
    to the results of same function calls on the listening server component.
    @html(<br><br>)

    ALL EVENTS for client connection components will only be called with
    the client connection components as parameter, rather than the Server
    listener component. In fact, only the 'OnListen...' events and the
    'OnRestart' event are being called with the Server listener component
    as the 'Sender' parameter. All the other events are only used by the
    connection components which are created for client connections by the
    server connection component. }
  TRtcServer = class(TRtcConnection)
  private
    FAmClient:boolean;

    FClientConnectTriggered:boolean;

    FRestartOn:TRtcRestartParam;

    FOnClientConnect:TRtcNotifyEvent;
    FOnClientDisconnect:TRtcNotifyEvent;

    FOnOpen:TRtcNotifyEvent;
    FOnClose:TRtcNotifyEvent;

    FOnListenLost:TRtcNotifyEvent;
    FOnListenError:TRtcErrorEvent;
    FOnRestart:TRtcNotifyEvent;

    procedure CallConnectionAccepting;

  protected

    // @exclude
    procedure SetRestartOn(const Value: TRtcRestartParam); override;

    // @exclude
    function GetRestartOn:TRtcRestartParam; override;

    // @exclude
    function AutoRelease:boolean; override;

    // @exclude
    function ServerConnectionsClosed: boolean;

    // @exclude
    procedure SetMultiThread(const Value: boolean); override;

    { Called by @Link(TRtcConnection) to copy values from 'Dup' connection
     (Server connection in most cases) to this one.
      This way, the new client connection is being prepared for usage.
      @html(<br><br>)

      If you should write a new connection class inherited from @Link(TRtcConnection),
      you have to overwrite this method and implement it so that it copies
      all connection properties defined by your class from 'Dup' to this one,
      (will be a wnewly created component).
      @html(<br><br>)

      CopyFrom() is being called from connection providers after a new
      client connection has been created and needs to be prepared for usage.

      @exclude}
    procedure CopyFrom(Dup:TRtcServer); virtual;

    { This is ConnectionAccepting trigger,
      ready to be mapped to a connection provider.
      Will trigger an exception if connection may not be accepted (for any reason).
      @exclude }
    procedure TriggerConnectionAccepting; virtual;

    { This is ConnectionAccepted trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectionAccepted; virtual;

    { This is ConnectionLost trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectionLost; virtual;

    { This is a trigger to create a new connection provider,
      used by all server connection providers to get a fresh
      conncetion component, which they can initialize and use.
      @exclude }
    procedure TriggerNewProvider(var Provider:TObject); virtual;

    { This is a ReadyToRelease trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerReadyToRelease; override;

    { This is a ClientConnect trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerClientConnect; virtual;
    // @exclude
    procedure CallClientConnect; virtual;

    { This is a ClientDisconnect trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerClientDisconnect; virtual;
    // @exclude
    procedure CallClientDisconnect; virtual;

    { This is a ListenStart trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenStart; virtual;
    // @exclude
    procedure CallListenStart; virtual;

    { This is a ListenStop trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenStop; virtual;
    // @exclude
    procedure CallListenStop; virtual;

    { This is a ListenLost trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenLost; virtual;
    // @exclude
    procedure CallListenLost; virtual;

    { This is a ListenError trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenError(E:Exception); virtual;
    // @exclude
    procedure CallListenError(E:Exception); virtual;

    { This is a Restart trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerRestart; virtual;
    // @exclude
    procedure CallRestart; virtual;

    { @name sets all parameters for the connection provider,
      introduced by the @Link(TRtcServer) connection component.
      It calls 'inherited' to set inherited properties.
      @exclude }
    procedure SetParams; override;
    { @name sets all triggers for the connection provider,
      introduced by the @Link(TRtcServer) connection component.
      It calls 'inherited' to set inherited triggers.
      @exclude }
    procedure SetTriggers; override;
    { @name clears all triggers for the connection provider,
      introduced by the @Link(TRtcServer) connection component.
      It calls 'inherited' to clear inherited triggers.
      @exclude }
    procedure ClearTriggers; override;

    { @name is a trigger, implementing a Timer event to
      restart the listener connection.
      @exclude }
    procedure OnRestart_Timer; virtual;

  public
    { When creating TRtcServer components at runtime,
      never use Create(). Always use the NEW class function,
      implemented by all RTC connection components. }
    constructor Create(AOwner: TComponent); override;
    { To destroy connection component created at runtime,
      never use Free or Destroy. Always use the @Link(TRtcConnection.Release) method. }
    destructor Destroy; override;

    { Creates a copy of this connection component,
      with all properties and events set,
      including a new connection provider,
      with preset properties and events. }
    function copyOf:TRtcServer; virtual;

    { Start listening for incomming connections on specified @Link(TRtcConnection.ServerPort),
      bound to a specified @Link(TRtcConnection.ServerAddr). }
    procedure Listen(Restarting:boolean=False); virtual;

    { Stop Listening for incomming connections
      and close all open connections as soon as possible. }
    procedure StopListen; virtual;

    { Calling this method will create a timer and call Listen
      after 'WaitSeconds' seconds. If you call this method with
      0 as parameter, default value for RTC_WAIT_BEFORE_RECONNECT will be used. }
    procedure Restart(WaitSeconds:integer=0); virtual;

    { Returns TRUE if this is a listener (server) and it is listening. }
    function isListening:boolean; override;

    { Returns TRUE if this is a client connection,
      FALSE if it is the listener. }
    function isClient:boolean; override;

    { Returns TRUE. }
    function isServer:boolean; override;

  published

    { By setting these properties to True, you do not have to do anything
      special to initiate a renewed Listen after specific events occure.
      For more information, check @Link(TRtcRestartParam). }
    property RestartOn;

    { This will be the first event to trigger when a new client establishes
      a new connection with your listening server component.
      Same as the 'OnConnect' event, this event will allways be called
      from the client connection's thread and with a client connection as parameter.
      @html(<br><br>)

      If all your client connections will use the same events handlers for all
      client connections coming through this server component (this is the
      prefered method to use RTC connection components), you don't need to
      implement this event handler.
      @html(<br><br>)

      But, in case you want to use different events for different clients (maybe
      depending on the IP address or some other information you get immediatelly
      after connect and before any real data has been sent or received), you can
      use this event handler to set new events, which will be used instead of
      the default event handlers, which you defined for your TRtcServer component. }
    property OnClientConnect:TRtcNotifyEvent read FOnClientConnect write FOnClientConnect;

    { This event will be the last event to trigger when a working connection that
      came from a client to your listening server gets lost.
      Same as the 'OnDisconnect' event, this event will allways be called
      from the client connection's thread and with a client connection as parameter.
      @html(<br><br>)

      This event's primary purpuse is to give you a chance to free the
      resources you occupied for this connection. There will be no more
      events triggered for this connection after 'OnClientDisconnect'.
      @html(<br><br>)

      This will be the last event where you can still use the Client connection.
      After this event, Client connection and its component will be destroyed.
       NOTE: OnClientDisconnect allways comes in pair with prior OnClientConnect.
             OnClientDisconnect will not be triggered for connections
             where OnClientConnect was not triggered before. }
    property OnClientDisconnect:TRtcNotifyEvent read FOnClientDisconnect write FOnClientDisconnect;

    { Listener was started. This means that we're waiting on a
      specific port for incomming connections. This event will be
      triggered if the call to "Listen" was succesfull. }
    property OnListenStart:TRtcNotifyEvent read FOnOpen write FOnOpen;

    { Listener stopped. This means that we're no longer waiting on
      a specific port for incomming connections. This event will be
      triggered if the call to "Disconnect" was succesfull.
       NOTE: OnListenStop allways comes in pair with proior OnListenStart.
             OnListenStop will not be triggered for server components
             where OnListenStart was not triggered before. }
    property OnListenStop:TRtcNotifyEvent read FOnClose write FOnClose;

    { This event will be called when our listener closes,
      without us calling the 'Disconnect' method. }
    property OnListenLost:TRtcNotifyEvent read FOnListenLost write FOnListenLost;

    { This event will be called when listener can not start because of an error. }
    property OnListenError:TRtcErrorEvent read FOnListenError write FOnListenError;

    { This event will be triggered just before starting a new listening
      attempt, when a listener had to be restarted (listener killed by OS). }
    property OnRestart:TRtcNotifyEvent read FOnRestart write FOnRestart;
    end;

{ --- TRtcClient --- }

  { @Abstract(Reconnect parameters for TRtcClient components)

    @name is tightly coupled with @Link(TRtcClient) component.
    It encapsulates the parameters used to define how a client
    connection should act when a connection can not be
    established, or when connection gets lost. }
  TRtcReconnectParam = class(TRtcPersistent)
  private
    FConnectError:boolean;
    FConnectFail:boolean;
    FConnectLost:boolean;
    FWait:integer;

  public
    { @class will be created by TRtcClient component.
      @exclude }
    constructor Create;
    { @class will be destroyed by TRtcClient component.
      @exclude }
    destructor Destroy; override;

    { @exclude }
    procedure AssignTo(Dest:TPersistent); override;

  published
    { Set ConnectError to TRUE if you want automatic reconnect on Connect Errr. }
    property ConnectError:boolean read FConnectError write FConnectError default False;

    { Set ConnectLost to TRUE if you want automatic reconnect on Connect Lost. }
    property ConnectLost:boolean read FConnectLost write FConnectLost default False;

    { Set ConnectFail to TRUE if you want automatic reconnect on Connect Fail. }
    property ConnectFail:boolean read FConnectFail write FConnectFail default False;

    { Wait defines how long (in seconds) component should wait before it tries to reconnect. }
    property Wait:integer read FWait write FWait default 0;
    end;

  { @Abstract(Basic Client-side connection component wrapper)

    @name publishes methods and handles all the tasks that are common to all
    Client connection components. All Client-side connection components
    are indirect descendants of TRtcClient and therefor inherit
    all its methods and properties. Since @name is also a direct descendant
    of @Link(TRtcConnection), it also inherits all methods and properties
    from @Link(TRtcConnection). For more information, check @Link(TRtcConnection). }
  TRtcClient = class(TRtcConnection)
  private
    FConLevel:integer;
    FConCnt:integer;

    FReconnectOn:TRtcReconnectParam;

    FOnConnectError:TRtcErrorEvent;
    FOnConnectFail:TRtcNotifyEvent;
    FOnConnectLost:TRtcNotifyEvent;

    FOnReconnect:TRtcNotifyEvent;

  protected

    // @exclude
    procedure SetReconnectOn(const Value: TRtcReconnectParam); override;
    
    // @exclude
    function GetReconnectOn:TRtcReconnectParam; override;

    // @exclude
    function isConnectionRequired:boolean; virtual;

    // @exclude
    procedure SetMultiThread(const Value: boolean); override;

    { This is a ConnectionOpening trigger,
      ready to be mapped to a connection provider.
      Will trigger an exception if connection may not be opened (for any reason).
      @exclude }
    procedure TriggerConnectionOpening(Force:boolean); virtual;

    { This is a ConnectionClosing trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectionClosing; virtual;

    { This is a ReadyToRelease trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerReadyToRelease; override;

    { This is a ConnectFail trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectFail; virtual;
    // @exclude
    procedure CallConnectFail; virtual;

    { Disconnect event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDisconnect; override;

    { This is a ConnectLost trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectLost; virtual;
    // @exclude
    procedure CallConnectLost; virtual;

    { This is a ConnectError trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectError(E:Exception); virtual;
    // @exclude
    procedure CallConnectError(E:Exception); virtual;

    { This is a Reconnect trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerReconnect; virtual;
    // @exclude
    procedure CallReconnect; virtual;

    { @name sets all parameters for the connection provider,
      introduced by the @Link(TRtcClient) connection component.
      It calls 'inherited' to set inherited properties.
      @exclude }
    procedure SetParams; override;
    { @name sets all triggers for the connection provider,
      introduced by the @Link(TRtcClient) connection component.
      It calls 'inherited' to set inherited triggers.
      @exclude }
    procedure SetTriggers; override;
    { @name clears all triggers for the connection provider,
      introduced by the @Link(TRtcClient) connection component.
      It calls 'inherited' to clear inherited triggers.
      @exclude }
    procedure ClearTriggers; override;

    { @name is a trigger, implementing a Timer event to
      reconnect the client connection.
      @exclude }
    procedure OnReconnect_Timer; virtual;

  public

    { When creating TRtcClient components at runtime,
      never use Create(). Always use the NEW class function,
      implemented by all RTC connection components. }
    constructor Create(AOwner: TComponent); override;
    { To destroy connection component created at runtime,
      never use Free or Destroy. Always use the @Link(TRtcConnection.Release) method. }
    destructor Destroy; override;

    { Connect to @Link(TRtcConnection.ServerAddr) on @Link(TRtcConnection.ServerPort) (a Listener has to be waiting there).
      Before you start sending data out, wait for the @Link(TRtcConnection.OnConnect) event.
      If there will be something to read from the connection, the @Link(TRtcConnection.OnDataReceived)
      event will be triggered. Do not call @Link(TRtcConnection.Read) from anywhere else that
      the @Link(TRtcConnection.OnDataReceived) event.
      @html(<br><br>)

      You can use the 'Force' parameter to force a connection attempt
      even if the connection count limit has been reached.
      @html(<br><br>)

      If the host address can not be resolved, port is invalid or something
      else is wrong with connecion data, and you have defined the 'OnConnectError' event,
      then the 'OnConnectError' event will be triggered, but there will be NO EXCEPTIONS
      raised from the 'Connect' procedure.
      @html(<br><br>)

      To initiate a new conncetion attempt after a connect error, you
      can simply call 'Connect' or 'Reconnect' from this event.
      By calling 'Reconnect', you can define how long you want to wait before
      a new connection attempt will be made, while a call to 'Connect' uses
      a default time period set up with a RTC_WAIT_BEFORE_RECONNECT constant.
      @html(<br><br>)

      If this connect attempt raises an exception, it means that either
      you have reached your connection limit or something bad has gone wrong
      and you should handle that exception as internal error.
      @html(<br><br>)

      When all is clear for the connection attempt, 'Connect' procedure will
      return immediatelly, and real connecting is done in the background. Do not
      try to send or read data immediatelly after the call to 'Connect', you will
      end up with exceptions (socket not connected or something like that).

      If there were no exceptions comming from 'Connect',
      there are three possibilities (only one can happen):

      A) If connection attempt results in an ERROR:
           "OnConnectError" event will be triggered.
           If 'ReconnectOnError' is TRUE,
             reconnect will be triggered.

      B) Connection attempt FAILS:
           "OnConnectFail" event will be triggered.
           If 'ReconnectOnFail' is TRUE,
             reconnect will be triggered.

      C) Connection attempt SUCCEEDS:
           first "OnConnecting" event is triggered,
           then "OnConnect" events will be triggered.

           When a working connection closes,
             "OnDisconnect" and "OnDisconnecting" events will be triggered.

             After that, if the connection was NOT closed on purpose,
               "OnConnectLost" event will be triggered.
               If "ReconnectOnLost" is TRUE, reconnect will be triggered.

      Connection attempts that end in ERROR or FAIL,
      only trigger the @Link(OnConnectError) or @Link(OnConnectFail) event.
      They DO NOT trigger the @Link(TRtcConnection.OnDisconnect)
      or @Link(TRtcConnection.OnDisconnecting) events. }
    procedure Connect(Force:boolean=False; Reconnecting:boolean=False); virtual;

    { Returns TRUE if this Client is connected and ready to send/receive data. }
    function isConnected:boolean; override;

    { Returns TRUE if this Client is connected, connecting, 
      trying to connect or waiting to start the next reconnect attempt. }
    function isConnecting:boolean; override;

    { Calling this method will create a timer and call the OnReconnect event
      after 'WaitSeconds' seconds. If you call this method with
      0 as parameter, default value for RTC_WAIT_BEFORE_RECONNECT will be used. }
    procedure Reconnect(WaitSeconds:integer=0); virtual;

  published

    { By setting properties in @name to True, you do not have to do anything
      special to initiate a Reconnect after specific events occur. }
    property ReconnectOn;

    { This event will be called when it is certain that a connection
      attempt using "Connect" from this object has failed.
      @html(<br><br>)

      Other implementations might first call "OnConnect" then call "OnDisconnect"
      when a connection attempt has failed, but I think it's better to keep those
      situations (connection open then closed, or connection failed) separated,
      to know what exactly happened. If "OnConnectFail" event is triggered,
      you can be sure that either something is wrong with your Addr/Port,
      or the Host (server) you're connecting to is not listening,
      or there is a firewall between the two of you that blocks the connection. }
    property OnConnectFail:TRtcNotifyEvent read FOnConnectFail write FOnConnectFail;

    { This event will be called when a connection that we have opened closes,
      without us calling the 'Disconnect' method. }
    property OnConnectLost:TRtcNotifyEvent read FOnConnectLost write FOnConnectLost;

    { This event will be called when you try to open a new connection
      and an error occured inside the 'Connect' method, saying that
      either the connection limit was reached, host address can not
      be resolved, TCP/IP is not working or anything else that can
      happen before the actual connecting process starts.
      @html(<br><br>)

      This method will be called with the connection object and the
      exception that occured as parameters.
      @html(<br><br>)

      To try reconnecting, you can change the connection parameters (if needed)
      and call the 'Reconnect' method from this event.
      You cal also @Link(TRtcConnection.Release) the connection object from here,
      in case youcreated it at runtime. }
    property OnConnectError:TRtcErrorEvent read FOnConnectError write FOnConnectError;

    { This event will be triggered just before making a new connection
      attempt after a connect error, failed connection or lost connection. }
    property OnReconnect:TRtcNotifyEvent read FOnReconnect write FOnReconnect;
    end;

  { @abstract(RTC Client Request info) }
  TRtcClientRequest = class(TRtcRequest)
  private
    FOnBegin:TRtcNotifyEvent;
    FOnData:TRtcNotifyEvent;
    FOnDone:TRtcNotifyEvent;
    FOnAbort:TRtcNotifyEvent;
    FOnReject:TRtcNotifyEvent;

  public
    // @exclude
    constructor Create; overload;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;
    { @exclude }
    procedure Init; override;

    // Skip this request, response irrelevant
    procedure Skip; override;
    // Cancel this request (ResponseAborted event should be triggered)
    procedure Cancel; override;
    // Repost this request
    procedure Repost; override;

    // Event called before the first RequestStart event
    property OnBegin:TRtcNotifyEvent read FOnBegin write FOnBegin;
    // Event called after last ResponseData event
    property OnData:TRtcNotifyEvent read FOnData write FOnData;
    // Event called after last ResponseDone event
    property OnDone:TRtcNotifyEvent read FOnDone write FOnDone;

    // Event called after Request has been terminated by peer disconnect and repost was not requested
    property OnAbort:TRtcNotifyEvent read FOnAbort write FOnAbort;
    // Event called after Response has been Rejected by Request handler (wrong server response)
    property OnReject:TRtcNotifyEvent read FOnReject write FOnReject;
    end;

  { @abstract(RTC Client Response info) }
  TRtcClientResponse = class(TRtcResponse)
  public
    // @exclude
    constructor Create; overload;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;

    // Reject this response, it is inacceptable (wrong server or server version?)
    procedure Reject; override;
    end;

  { @abstract(RTC Server Request info) }
  TRtcServerRequest = class(TRtcRequest)
  public
    // @exclude
    constructor Create; overload;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;
    { @exclude 
      Not supported - raises an Exception if called!}
    procedure Init; override;

    { @exclude 
      Not supported - raises an Exception if called! }
    procedure Skip; override;
    { @exclude 
      Not supported - raises an Exception if called! }
    procedure Cancel; override;
    { @exclude 
      Not supported - raises an Exception if called! }
    procedure Repost; override;
    end;

  TRtcServerRequestFixup=class(TRtcPersistent)
  private
    FDecodeQuery: boolean;
    FRemovePrefix: boolean;
    FDecodeFileName: boolean;
    FUpperCaseFileName: boolean;

  public
    { Will be created by TRtcDataServer component.
      @exclude }
    constructor Create;
    { Will be destroyed by TRtcDataServer component.
      @exclude }
    destructor Destroy; override;

    procedure AssignTo(Dest:TPersistent); override;

    procedure Fixup(Request:TRtcRequest);

  published
    { You can set this property to TRUE to allow the component to automatically remove
      the "http://domain.com:port/" and "https://domain.com:port/" part from Request.FileName,
      so that your Web Server can also work with HTTP Proxy Servers who do not properly
      process the URI before forwarding the request. The only reason why should should NOT set
      this property to TRUE is if you do not want your Server to work with badly implemented
      Proxy Servers or if you are using the component to implement a HTTP Proxy server and
      need the URL Prefix (http://...) to determine the destination of each request. }
    property RemovePrefix:boolean read FRemovePrefix write FRemovePrefix default False;

    { To have the HttpServer component decode the FileName part of the Request automatically,
      so you do not have to use URLDecode on "Request.FileName", set this to TRUE.
      Note that if you do this, you have to avoid using URLDecode on Request.FileName anywhere
      in your code, because doing URLDecode twice on the same string could give you wrong results. }
    property DecodeFileName:boolean read FDecodeFileName write FDecodeFileName default False;

    { To have the HttpServer component decode the Query part of the Request automatically,
      so you do not have to use URLDecode on "Request.Query" elements, set this to TRUE.
      Note that if you do this, you have to avoid using URLDecode on Request.Query anywhere
      in your code, because doing URLDecode twice on the same string could give you wrong results. }
    property DecodeQuery:boolean read FDecodeQuery write FDecodeQuery default False;

    { If you want to process all your requests by allowing the user to use mixed casing
      while you only check for the uppercase version of the string in Request.FileName,
      set this to TRUE and Request.FileName will always be converted to UpperCase.
      Note that doing this would also make any code which checks for lowercase versions
      of strings in Request.FileName non-functional, because all lowercase letters will
      be converted to uppercase before anything can access the Request.FileName property. }
    property UpperCaseFileName:boolean read FUpperCaseFileName write FUpperCaseFileName default False;
    end;

  TRtcClientRequestFixup=class(TRtcPersistent)
  private
    FEncodeQuery: boolean;
    FEncodeFileName: boolean;
    FForceHttp10: boolean;

  public
    { Will be created by TRtcDataClient component.
      @exclude }
    constructor Create;
    { Will be destroyed by TRtcDataClient component.
      @exclude }
    destructor Destroy; override;

    procedure AssignTo(Dest:TPersistent); override;

    procedure Fixup(Request:TRtcRequest);

  published
    { To have the Client component encode the FileName part of the Request automatically,
      so you do not have to use URL_Encode on "Request.FileName", set this to TRUE.
      Note that if you do this, you have to avoid using URL_Encode on Request.FileName anywhere
      in your code, because doing URL_Encode twice on the same string could give you wrong results. }
    property EncodeFileName:boolean read FEncodeFileName write FEncodeFileName default False;

    { To have the Client component encode the Query part of the Request automatically,
      so you do not have to use URL_Encode on "Request.Query" elements, set this to TRUE.
      Note that if you do this, you have to avoid using URL_Encode on Request.Query anywhere
      in your code, because doing URL_Encode twice on the same string could give you wrong results. }
    property EncodeQuery:boolean read FEncodeQuery write FEncodeQuery default False;

    { If your client needs to work with some very old Server(s) which ONLY understand the
      old HTTP/1.0 protocol and can NOT work with the new HTTP/1.1 protocol, you can either
      manually set the Request.Close property for every request to TRUE, or ... set
      Fixuprequest.ForceOldHttp10 to TRUE and have every request sent out using HTTP/1.0.

      Setting ForceOldHttp10 to TRUE will force every request to be sent using HTTP/1.0,
      which means that a connection will be open before every request, one request will
      be sent out and a single response will be received before the Server closes the
      connection. The client will have to open a new connection if it wants to send
      out another request to the same Server. This will make your client work a lot
      slower, but will also make working with very old Web Servers a lot easier. }
    property ForceOldHttp10:boolean read FForceHttp10 write FForceHttp10 default False;
    end;

  { @abstract(RTC Server Response info) }
  TRtcServerResponse = class(TRtcResponse)
  public
    // @exclude
    constructor Create; overload;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;

    { @exclude  
      Not supported - raises an Exception if called! }
    procedure Reject; override;
    end;

{ Number of active incomming RTC Connections to all RTC Server components }
function rtcServerConnectionCount:longint;

{ Number of active outgoing RTC Connections from all RTC Client components }
function rtcClientConnectionCount:longint;

{ Total number of ALL active incomming and outgoing connections to and from RTC components }
function rtcTotalConnectionCount:longint;

{ Setup Receiving Parameters:
  MRU = "Maximum Receive Unit" = Receiving Packet Size in Bytes (default = 1500 Bytes)
  ReceivingBuffer = Number of Packets that should fit into the Receiving Buffer (default = maximize input buffers)
  MaxReadPackets = Maximum number of Packets to be extracted from Receiving buffers with a single Read operation (default = entire input buffer) }
procedure rtcSetupReceivingParams(MRU:word=0;
                                  ReceivingBuffer:byte=0;
                                  MaxReadPackets:byte=0);

{ Setup Sending Parameters:
  MTU = "Maximum Transmit Unit" = Sending Packet Size in Bytes (default = 1500 Bytes)
  SendingBuffer = Number of Packets to fit into the Sending buffer (default = maximize output buffers)
  MaxWritePackets = Maximum number of Packets to be sent from Sending buffers with a single Write operation (default = entire output buffer) }
procedure rtcSetupSendingParams(MTU:word=0;
                                SendingBuffer:byte=0;
                                MaxWritePackets:byte=0);

{ Terminate all RTC Worker Threads and Timers.
  After this call, RTC components will no longer be functional.
  Call this procedure ONLY if you are closing your Application. }
procedure rtcShutDown;

{$IFDEF RTC_ANON_METHODS}
{ Get RtcIntPtr from "reference to procedure" or "reference to function" }
function MethRefToRtcPtr(const MethRef):RtcIntPtr;
{$ENDIF}

implementation

uses
{$IFDEF IDE_1}
  FileCtrl,
{$ENDIF}

{$IFDEF MEMCONTROL}
  rtcMemory,
{$ENDIF}

  rtcLog;

{$IFDEF RTC_TRIAL}
const
  LIMITED_TEXT='Limit for active connections in a Starter edition reached.'#13#10+
               'Order a RealThinClient subscription to remove this limit.';
{$ENDIF}

{ Shut down all RTC background Threads and Timers. }
procedure rtcShutDown;
  begin
  CloseTimerPool;
  end;

{ Setup Receiving Parameters:
  MRU = "Maximum Receive Unit" = Receiving Packet Size in Bytes (default = 1500 Bytes)
  ReceivingBuffer = Number of Packets that should fit into the Receiving Buffer (default = maximize input buffers)
  MaxReadPackets = Maximum number of Packets to be extracted from Receiving buffers with a single "Read" call (default = entire input buffer) }
procedure rtcSetupReceivingParams(MRU:word=0;
                                  ReceivingBuffer:byte=0;
                                  MaxReadPackets:byte=0);
  var
    PackSize:Cardinal;
  begin
  if MRU=0 then MRU:=1500
  else if MRU<60 then MRU:=60;
  PackSize:=MRU;

  if (ReceivingBuffer=0) or
     (ReceivingBuffer>trunc(65535/PackSize)) then
    ReceivingBuffer:=trunc(65535/PackSize);

  if (MaxReadPackets=0) or
     (MaxReadPackets>ReceivingBuffer) then
    MaxReadPackets:=ReceivingBuffer;

  SOCK_READ_BUFFER_SIZE:=ReceivingBuffer*PackSize;
  SOCK_MAX_READ_SIZE:=MaxReadPackets*PackSize;
  end;

{ Setup Sending Parameters:
  MTU = "Maximum Transmit Unit" = Sending Packet Size in Bytes (default = 1500 Bytes)
  SendingBuffer = Number of Packets to fit into the Sending buffer (default = maximize output buffers)
  MaxWritePackets = Maximum number of Packets to be sent from Sending buffers with a single "Write" call (default = entire output buffer) }
procedure rtcSetupSendingParams(MTU:word=0;
                                SendingBuffer:byte=0;
                                MaxWritePackets:byte=0);
  var
    PackSize:Cardinal;
  begin
  if MTU=0 then MTU:=1500
  else if MTU<100 then MTU:=100;
  PackSize:=MTU-40;

  if (SendingBuffer=0) or
     (SendingBuffer>trunc(65535/PackSize)) then
    SendingBuffer:=trunc(65535/PackSize);

  if (MaxWritePackets=0) or
     (MaxWritePackets>SendingBuffer) then
    MaxWritePackets:=SendingBuffer;

  SOCK_SEND_BUFFER_SIZE:=SendingBuffer*PackSize;
  SOCK_MAX_SEND_SIZE:=MaxWritePackets*PackSize;
  end;

type
  TRtcBaseServerClass = class of TRtcServer;

var
  ConnCS:TRtcCritSec;
  ServerConnection_Count:longint=0;
  ClientConnection_Count:longint=0;
  TotalConnection_Count:longint=0;

function rtcServerConnectionCount:longint;
  begin
  ConnCS.Acquire;
  try
    Result:=ServerConnection_Count;
  finally
    ConnCS.Release;
    end;
  end;

function rtcClientConnectionCount:longint;
  begin
  ConnCS.Acquire;
  try
    Result:=ClientConnection_Count;
  finally
    ConnCS.Release;
    end;
  end;
  
function rtcTotalConnectionCount:longint;
  begin
  ConnCS.Acquire;
  try
    Result:=TotalConnection_Count;
  finally
    ConnCS.Release;
    end;
  end;

function ClientConnection_Open(Force:boolean=False):boolean;
  begin
  Result:=False;
  if not assigned(ConnCS) then
    raise EClientLimitReached.Create('Closing application.');

  {$IFDEF MEMCONTROL}
  if Get_MemoryInUse>=RTC_MEMORY_LIMIT then
    begin
    if Force then
      begin
      ConnCS.Acquire;
      try
        Inc(ClientConnection_Count);
        Dec(TotalConnection_Count);
      finally
        ConnCS.Release;
        end;
      end
    else
      raise EClientLimitReached.Create('Memory limit reached.')
    end
  else
  {$ENDIF}
    begin
    ConnCS.Acquire;
    try
      {$IFDEF RTC_TRIAL}
      if TotalConnection_Count>=10 then
        raise EClientLimitReached.Create(LIMITED_TEXT)
      else {$ENDIF}
      if (RTC_CONNECTION_LIMIT>0) and
         (TotalConnection_Count>=RTC_CONNECTION_LIMIT) then
        begin
        if Force then
          begin
          Inc(ClientConnection_Count);
          Inc(TotalConnection_Count);
          end
        else
          raise EClientLimitReached.Create('Total connection limit reached.')
        end
      else if (RTC_CLIENT_CONNECT_LIMIT>0) and
              (ClientConnection_Count>=RTC_CLIENT_CONNECT_LIMIT) then
        begin
        if Force then
          begin
          Inc(ClientConnection_Count);
          Inc(TotalConnection_Count);
          end
        else
          raise EClientLimitReached.Create('Client connection limit reached.');
        end
      else
        begin
        Inc(ClientConnection_Count);
        Inc(TotalConnection_Count);
        Result:=True;
        end;
    finally
      ConnCS.Release;
      end;
    end;
  end;

procedure ClientConnection_Close;
  begin
  if not assigned(ConnCS) then Exit;

  ConnCS.Acquire;
  try
    Dec(ClientConnection_Count);
    Dec(TotalConnection_Count);
  finally
    ConnCS.Release;
    end;
  end;

procedure ServerConnection_CanAccept;
  begin
  if not assigned(ConnCS) then
    raise EClientLimitReached.Create('Closing application.');

  {$IFDEF MEMCONTROL}
  if Get_MemoryInUse>=RTC_MEMORY_LIMIT then
    raise EClientLimitReached.Create('Memory limit reached.')
  else
  {$ENDIF}
    begin
    ConnCS.Acquire;
    try
      {$IFDEF RTC_TRIAL}
      if TotalConnection_Count>=10 then
        raise EClientLimitReached.Create(LIMITED_TEXT)
      else {$ENDIF}
      if (RTC_CONNECTION_LIMIT>0) and
         (TotalConnection_Count>=RTC_CONNECTION_LIMIT) then
        raise EClientLimitReached.Create('Total connection limit reached.')
      else if (RTC_SERVER_ACCEPT_LIMIT>0) and
              (ServerConnection_Count>=RTC_SERVER_ACCEPT_LIMIT) then
        raise EClientLimitReached.Create('Server connection limit reached.');
    finally
      ConnCS.Release;
      end;
    end;
  end;

procedure ServerConnection_Accept;
  begin
  if not assigned(ConnCS) then
    raise EClientLimitReached.Create('Closing application.');

  ConnCS.Acquire;
  try
    Inc(ServerConnection_Count);
    Inc(TotalConnection_Count);
  finally
    ConnCS.Release;
    end;
  end;

procedure ServerConnection_Close;
  begin
  if not assigned(ConnCS) then
    Exit;

  ConnCS.Acquire;
  try
    Dec(ServerConnection_Count);
    Dec(TotalConnection_Count);
  finally
    ConnCS.Release;
    end;
  end;

{ TRtcComponent }

procedure TRtcComponent.AfterConstruction;
  begin
  FNotSilent:=True;
  FAmSilent:=False;
  end;

procedure TRtcComponent.BeforeDestruction;
  begin
  FNotSilent:=False;
  FAmSilent:=True;
  end;

function TRtcComponent.GetVersionSDK: RtcString;
  begin
  Result:=RTCSDK_VERSION;
  end;

procedure TRtcComponent.SetVersionSDK(const s: RtcString);
  begin
  // This setter method has to exist,
  // or Delphi would not show the property in the IDE
  end;

{$IFDEF RTC_ANON_METHODS}

{--- Anonymous Method support ---}

function MethRefToRtcPtr(const MethRef):RtcIntPtr;
  begin
  Result:= RtcIntPtr(Pointer(MethRef));
  end;

constructor TRtcComponent.Create(AOwner:TComponent);
  begin
  inherited;
  FAMCS:=TRtcCritSec.Create;
  FAMethods:=nil;
  end;

destructor TRtcComponent.Destroy;
  var
    o:TObject;
    i:RtcIntPtr;
  begin
  FAMCS.Acquire;
  try
    if assigned(FAMethods) then
      begin
      i:=FAMethods.search_min(o);
      while (i<>0) and assigned(o) do
        begin
        FAMethods.remove(i);
        RtcFreeAndNil(o);
        i:=FAMethods.search_min(o);
        end;
      RtcFreeAndNil(FAMethods);
      end;
  finally
    FAMCS.Release;
    end;
  RtcFreeAndNil(FAMCS);
  inherited;
  end;

type
  TRtcSimpleAMContainer = class(TObject)
  public
    MyMethod:TRtcSimpleAnonMethod;
    constructor Create(const AMethod:TRtcSimpleAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TObject);
    end;

constructor TRtcSimpleAMContainer.Create(const AMethod:TRtcSimpleAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcSimpleAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcSimpleAMContainer.MyEvent(Sender:TObject);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender);
  end;

function TRtcComponent.Anon(const Event:TRtcSimpleAnonMethod):TRtcSimpleEvent;
  var
    obj:TRtcSimpleAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcSimpleAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcNotifyAMContainer = class(TObject)
  public
    MyMethod:TRtcNotifyAnonMethod;
    constructor Create(AMethod:TRtcNotifyAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection);
    end;

constructor TRtcNotifyAMContainer.Create(AMethod:TRtcNotifyAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcNotifyAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcNotifyAMContainer.MyEvent(Sender:TRtcConnection);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender);
  end;

function TRtcComponent.Anon(const Event:TRtcNotifyAnonMethod):TRtcNotifyEvent;
  var
    obj:TRtcNotifyAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcNotifyAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcErrorAMContainer = class(TObject)
  public
    MyMethod:TRtcErrorAnonMethod;
    constructor Create(AMethod:TRtcErrorAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; E:Exception);
    end;

constructor TRtcErrorAMContainer.Create(AMethod:TRtcErrorAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcErrorAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcErrorAMContainer.MyEvent(Sender:TRtcConnection; E:Exception);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,E);
  end;

function TRtcComponent.Anon(const Event:TRtcErrorAnonMethod):TRtcErrorEvent;
  var
    obj:TRtcErrorAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcErrorAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcFunctionCallAMContainer = class(TObject)
  public
    MyMethod:TRtcFunctionCallAnonMethod;
    constructor Create(AMethod:TRtcFunctionCallAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Param:TRtcFunctionInfo; Result:TRtcValue);
    end;

constructor TRtcFunctionCallAMContainer.Create(AMethod:TRtcFunctionCallAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcFunctionCallAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcFunctionCallAMContainer.MyEvent(Sender:TRtcConnection; Param:TRtcFunctionInfo; Result:TRtcValue);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Param,Result);
  end;

function TRtcComponent.Anon(const Event:TRtcFunctionCallAnonMethod):TRtcFunctionCallEvent;
  var
    obj:TRtcFunctionCallAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcFunctionCallAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcFunctionErrorAMContainer = class(TObject)
  public
    MyMethod:TRtcFunctionErrorAnonMethod;
    constructor Create(AMethod:TRtcFunctionErrorAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Param:TRtcFunctionInfo; Result:TRtcValue; E:Exception; var Handled:boolean);
    end;

constructor TRtcFunctionErrorAMContainer.Create(AMethod:TRtcFunctionErrorAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcFunctionErrorAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcFunctionErrorAMContainer.MyEvent(Sender:TRtcConnection; Param:TRtcFunctionInfo; Result:TRtcValue; E:Exception; var Handled:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Param,Result,E,Handled);
  end;

function TRtcComponent.Anon(const Event:TRtcFunctionErrorAnonMethod):TRtcFunctionErrorEvent;
  var
    obj:TRtcFunctionErrorAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcFunctionErrorAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcResultAMContainer = class(TObject)
  public
    MyMethod:TRtcResultAnonMethod;
    constructor Create(AMethod:TRtcResultAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    end;

constructor TRtcResultAMContainer.Create(AMethod:TRtcResultAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcResultAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcResultAMContainer.MyEvent(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Data,Result);
  end;

function TRtcComponent.Anon(const Event:TRtcResultAnonMethod):TRtcResultEvent;
  var
    obj:TRtcResultAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcResultAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcResultErrorAMContainer = class(TObject)
  public
    MyMethod:TRtcResultErrorAnonMethod;
    constructor Create(AMethod:TRtcResultErrorAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue; E:Exception);
    end;

constructor TRtcResultErrorAMContainer.Create(AMethod:TRtcResultErrorAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcResultErrorAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcResultErrorAMContainer.MyEvent(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue; E:Exception);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Data,Result,E);
  end;

function TRtcComponent.Anon(const Event:TRtcResultErrorAnonMethod):TRtcResultErrorEvent;
  var
    obj:TRtcResultErrorAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcResultErrorAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcFunctionPrepareAMContainer = class(TObject)
  public
    MyMethod:TRtcFunctionPrepareAnonMethod;
    constructor Create(AMethod:TRtcFunctionPrepareAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Data:TRtcValue);
    end;

constructor TRtcFunctionPrepareAMContainer.Create(AMethod:TRtcFunctionPrepareAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcFunctionPrepareAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcFunctionPrepareAMContainer.MyEvent(Sender:TRtcConnection; Data:TRtcValue);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Data);
  end;

function TRtcComponent.Anon(const Event:TRtcFunctionPrepareAnonMethod):TRtcFunctionPrepareEvent;
  var
    obj:TRtcFunctionPrepareAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcFunctionPrepareAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcUserAMContainer = class(TObject)
  public
    MyMethod:TRtcUserAnonMethod;
    constructor Create(const AMethod:TRtcUserAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Obj:TObject);
    end;

constructor TRtcUserAMContainer.Create(const AMethod:TRtcUserAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcUserAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcUserAMContainer.MyEvent(Sender:TRtcConnection; Obj:TObject);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Obj);
  end;

function TRtcComponent.Anon(const Event:TRtcUserAnonMethod):TRtcUserEvent;
  var
    obj:TRtcUserAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcUserAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcUserDataAMContainer = class(TObject)
  public
    MyMethod:TRtcUserDataAnonMethod;
    constructor Create(AMethod:TRtcUserDataAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Obj:TObject; Data:TRtcValue);
    end;

constructor TRtcUserDataAMContainer.Create(AMethod:TRtcUserDataAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcUserDataAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcUserDataAMContainer.MyEvent(Sender:TRtcConnection; Obj:TObject; Data:TRtcValue);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Obj,Data);
  end;

function TRtcComponent.Anon(const Event:TRtcUserDataAnonMethod):TRtcUserDataEvent;
  var
    obj:TRtcUserDataAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcUserDataAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcCustomAMContainer = class(TObject)
    MyMethod:TRtcCustomAnonMethod;
    constructor Create(AMethod:TRtcCustomAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TObject; Obj:TObject);
    end;

constructor TRtcCustomAMContainer.Create(AMethod:TRtcCustomAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcCustomAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcCustomAMContainer.MyEvent(Sender:TObject; Obj:TObject);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Obj);
  end;

function TRtcComponent.Anon(const Event:TRtcCustomAnonMethod):TRtcCustomEvent;
  var
    obj:TRtcCustomAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcCustomAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcCustomDataAMContainer = class(TObject)
    MyMethod:TRtcCustomDataAnonMethod;
    constructor Create(AMethod:TRtcCustomDataAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TObject; Obj:TObject; Data:TRtcValue);
    end;

constructor TRtcCustomDataAMContainer.Create(AMethod:TRtcCustomDataAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcCustomDataAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcCustomDataAMContainer.MyEvent(Sender:TObject; Obj:TObject; Data:TRtcValue);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Obj,Data);
  end;

function TRtcComponent.Anon(const Event:TRtcCustomDataAnonMethod):TRtcCustomDataEvent;
  var
    obj:TRtcCustomDataAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcCustomDataAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

type
  TRtcObjectCreateAMContainer = class(TObject)
  public
    MyMethod:TRtcObjectCreateAnonMethod;
    constructor Create(AMethod:TRtcObjectCreateAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; Param:TRtcObjectCall);
    end;

constructor TRtcObjectCreateAMContainer.Create(AMethod:TRtcObjectCreateAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcObjectCreateAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcObjectCreateAMContainer.MyEvent(Sender:TRtcConnection; Param:TRtcObjectCall);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender,Param);
  end;

function TRtcComponent.Anon(const Event:TRtcObjectCreateAnonMethod):TRtcObjectCreateEvent;
  var
    obj:TRtcObjectCreateAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcObjectCreateAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

{$ENDIF}

{ TRtcConnection }

constructor TRtcConnection.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FTimedOut:=False;
  FReconnecting:=False;
  FRecTimer:=nil;

  FInfo:=TRtcInfo.Create;

  FMultiThread:=False;

  FFree:=False;
  FReadyToRelease:=False;

  Con:=nil;

  FActive:=False;
  FParent:=nil;

  FConnectTriggered:=False;
  FConnectingTriggered:=False;

  FTimeout:=TRtcTimeout.Create(self);
  FTimeoutsOfAPI:=TRtcTimeoutsOfAPI.Create;

  FPort:='';
  FAddr:='';
  FIPV:=rtc_IPvDefault;
  FOverLimit:=False;

  FInsideEvent:=0;
  FReleasing:=False;
  FReleaseCall:=False;
  FDisconnecting:=False;
  FStopping:=False;
  FConnecting:=False;
  FListening:=False;
  FConnectOp:=False;

  FOnConnect:=nil;
  FOnConnecting:=nil;
  FOnDisconnect:=nil;
  FOnDisconnecting:=nil;

  FOnException:=nil;

  FWSFrameQueued:=tXObjList.Create(8);
  FWSFrameOther:=tXObjList.Create(8);
  FWSFrameIN:=nil;
  FWSFrameOUT:=nil;
  FWS:=nil;
  end;

destructor TRtcConnection.Destroy;
  begin
  try
    if FReconnecting then
      begin
      FReconnecting:=False;
      TRtcTimer.Stop(FRecTimer);
      FRecTimer:=nil;
      end;

    RtcFreeAndNil(FTimeout);
    RtcFreeAndNil(FTimeoutsOfAPI);
    RtcFreeAndNil(FInfo);

    FPort:='';
    FAddr:='';
    FIPV:=rtc_IPVDefault;

    ws_Clear;

    RtcFreeAndNil(FWSFrameQueued);
    RtcFreeAndNil(FWSFrameOther);
    RtcFreeAndNil(FWSFrameOUT);
    RtcFreeAndNil(FWSFrameIN);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcConnection.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcConnection.Accept;
  begin
  raise ERtcConnection.Create('"Accept" method not supported by '+ClassName);
  end;

procedure TRtcConnection.ActivateObjectManager(xCreate:boolean=True);
  begin
  raise ERtcConnection.Create('"ActivateObjectManager" method not supported by '+ClassName);
  end;

function TRtcConnection.FindSession(const ID:RtcString):boolean;
  begin
{$IFDEF FPC} Result:=False; {$ENDIF}
  raise ERtcConnection.Create('"FindSession" method not supported by '+ClassName);
  end;

function TRtcConnection.HaveSession(const ID:RtcString):boolean;
  begin
{$IFDEF FPC} Result:=False; {$ENDIF}
  raise ERtcConnection.Create('"HaveSession" method not supported by '+ClassName);
  end;

procedure TRtcConnection.UnLockSession;
  begin
  raise ERtcConnection.Create('"UnLockSession" method not supported by '+ClassName);
  end;

procedure TRtcConnection.OpenSession(LockType:TRtcSessionLockType=sesFwdLock);
  begin
  raise ERtcConnection.Create('"OpenSession" method not supported by '+ClassName);
  end;

function TRtcConnection.CloseSession(const ID:RtcString):boolean;
  begin
{$IFDEF FPC} Result:=False; {$ENDIF}
  raise ERtcConnection.Create('"CloseSession" method not supported by '+ClassName);
  end;

procedure TRtcConnection.PostRequest(var myReq; FromInsideEvent:boolean=False);
  begin
  raise ERtcConnection.Create('"PostRequest" method not supported by '+ClassName);
  end;

procedure TRtcConnection.InsertRequest(const Req:TObject);
  begin
  raise ERtcConnection.Create('"InsertRequest" method not supported by '+ClassName);
  end;

procedure TRtcConnection.SkipRequests;
  begin
  raise ERtcConnection.Create('"SkipRequests" method not supported by '+ClassName);
  end;

procedure TRtcConnection.CancelRequests;
  begin
  raise ERtcConnection.Create('"CancelRequests" method not supported by '+ClassName);
  end;

procedure TRtcConnection.SetReconnectOn(const Value: TRtcReconnectParam);
  begin
  raise ERtcConnection.Create('"ReconnectOn" property not supported by '+ClassName);
  end;

function TRtcConnection.GetReconnectOn:TRtcReconnectParam;
  begin
{$IFDEF FPC} Result:=nil; {$ENDIF}
  raise ERtcConnection.Create('"ReconnectOn" property not supported by '+ClassName);
  end;

procedure TRtcConnection.SetRestartOn(const Value: TRtcRestartParam);
  begin
  raise ERtcConnection.Create('"RestartOn" property not supported by '+ClassName);
  end;

function TRtcConnection.GetRestartOn:TRtcRestartParam;
  begin
{$IFDEF FPC} Result:=nil; {$ENDIF}
  raise ERtcConnection.Create('"RestartOn" property not supported by '+ClassName);
  end;

function TRtcConnection.GetResponse:TRtcResponse;
  begin
{$IFDEF FPC} Result:=nil; {$ENDIF}
  raise ERtcConnection.Create('"Response" property not supported by '+ClassName);
  end;

function TRtcConnection.GetRequest:TRtcRequest;
  begin
{$IFDEF FPC} Result:=nil; {$ENDIF}
  raise ERtcConnection.Create('"Request" property not supported by '+ClassName);
  end;

function TRtcConnection.GetSession:TRtcSession;
  begin
{$IFDEF FPC} Result:=nil; {$ENDIF}
  raise ERtcConnection.Create('"Session" property not supported by '+ClassName);
  end;

procedure TRtcConnection.SetRequest(const Value:TRtcRequest);
  begin
  raise ERtcConnection.Create('"Request" property not supported by '+ClassName);
  end;

procedure TRtcConnection.SetResponse(const Value:TRtcResponse);
  begin
  raise ERtcConnection.Create('"Response" property not supported by '+ClassName);
  end;

function TRtcConnection.GetObjectManager:TRtcRemoteObjectManager;
  begin
{$IFDEF FPC} Result:=nil; {$ENDIF}
  raise ERtcConnection.Create('"GetObjectManager" method not supported by '+ClassName);
  end;

function TRtcConnection.TotalSessionsCount:cardinal;
  begin
  Result:=0;
  end;

function TRtcConnection.TotalSessionsLocked:cardinal;
  begin
  Result:=0;
  end;

function TRtcConnection.TotalSessionsUnlocked:cardinal;
  begin
  Result:=0;
  end;

function TRtcConnection.GetRequestInserted:boolean;
  begin
  Result:=False;
  end;

function TRtcConnection.isConnected:boolean;
  begin
  Result:=False;
  end;

function TRtcConnection.isListening:boolean;
  begin
  Result:=False;
  end;

function TRtcConnection.isClient:boolean;
  begin
  Result:=False;
  end;

function TRtcConnection.isServer:boolean;
  begin
  Result:=False;
  end;

function TRtcConnection.isConnecting:boolean;
  begin
  Result:=False;
  end;

function TRtcConnection.RequestCount:integer;
  begin
  Result:=0;
  end;

function TRtcConnection.sLocalAddr: String;
  begin
  Result:=String(LocalAddr);
  end;

function TRtcConnection.sLocalPort: String;
  begin
  Result:=String(LocalPort);
  end;

function TRtcConnection.sPeerAddr: String;
  begin
  Result:=String(PeerAddr);
  end;

function TRtcConnection.sPeerPort: String;
  begin
  Result:=String(PeerPort);
  end;

function TRtcConnection.sServerAddr: String;
  begin
  Result:=String(ServerAddr);
  end;

function TRtcConnection.sServerPort: String;
  begin
  Result:=String(ServerPort);
  end;

function TRtcConnection.ReadEx: RtcByteArray;
  begin
  if assigned(Con) then
    begin
    Timeout.DataReceived;
    Result:=Con.ReadEx;
    end
  else
    SetLength(Result,0);
  end;

procedure TRtcConnection.PokeEx(const s: RtcByteArray);
  begin
  if assigned(Con) then
    Con.PokeEx(s);
  end;

function TRtcConnection.PeekEx: RtcByteArray;
  begin
  if assigned(Con) then
    Result:=Con.PeekEx
  else
    SetLength(Result,0);
  end;

function TRtcConnection.Read: RtcString;
  begin
  if assigned(Con) then
    begin
    Timeout.DataReceived;
    Result:=Con.Read;
    end
  else
    SetLength(Result,0);
  end;

procedure TRtcConnection.WriteEx(const s: RtcByteArray);
  begin
  if assigned(Con) then
    begin
    Timeout.DataSending;
    Con.WriteEx(s);
    end;
  end;

procedure TRtcConnection.Write(const s: RtcString);
  begin
  if assigned(Con) then
    begin
    Timeout.DataSending;
    Con.Write(s);
    end;
  end;

procedure TRtcConnection.Write;
  begin
  WriteEx(RtcEmptyByteArray);
  end;

procedure TRtcConnection.Flush;
  begin
  // not supported? Do nothing.
  end;

procedure TRtcConnection.WriteHeader(SendNow:boolean=True);
  begin
  raise ERtcConnection.Create('"WriteHeader" method not supported by '+ClassName);
  end;

procedure TRtcConnection.WriteHeader(const HeaderText: RtcString; SendNow:boolean=True);
  begin
  raise ERtcConnection.Create('"WriteHeader" method not supported by '+ClassName);
  end;

procedure TRtcConnection.TriggerDataReceived;
  begin
  EnterEvent;
  try
    CallDataReceived;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerDataSent;
  begin
  if FWriteCount>0 then
    Timeout.DataSent;
  EnterEvent;
  try
    if FWriteCount>0 then
      CallDataSent;
    if not isClosing then
      CallReadyToSend;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerDataOut;
  begin
  Timeout.DataOut;
  EnterEvent;
  try
    if assigned(Con) then
      begin
      FDataOut:=Con.DataOut;
      FWriteCount:=FWriteCount+FDataOut;

      CallDataOut;
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerDataIn;
  begin
  Timeout.DataIn;
  EnterEvent;
  try
    if assigned(Con) then
      begin
      FDataIn:=Con.DataIn;
      FReadCount:=FReadCount+FDataIn;

      CallDataIn;
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerLastWrite;
  begin
  EnterEvent;
  try
    CallLastWrite;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.Error(const Err:String);
  begin
  raise ERtcConnection.Create(Err);
  end;

function TRtcConnection.TotalConnectionCount: integer;
  begin
  Result:=rtcTotalConnectionCount;
  end;

function TRtcConnection.TotalClientConnectionCount: integer;
  begin
  Result:=rtcClientConnectionCount;
  end;

function TRtcConnection.TotalServerConnectionCount: integer;
  begin
  Result:=rtcServerConnectionCount;
  end;

function TRtcConnection.inThread: boolean;
  begin
  if assigned(Con) then
    Result:=Con.inThread
  else if not MultiThreaded then
    Result:=True
  else
    Result:=False;
  end;

function TRtcConnection.inMainThread: boolean;
  begin
  if (self=nil) or MultiThreaded then
    Result:=InsideMainThread
  else
    Result:=True;
  end;

procedure TRtcConnection.Release;
  var
    inside:boolean;
  begin
  if self=nil then Exit;
  FReleaseCall:=True;
  if assigned(Con) then
    begin
    inside:=inThread;
    if FAmSilent then
      Con.Silent:=True;
    if Con.ReleaseMe(inside) then
      RtcFreeAndNil(Con);
    end
  else if InsideEvent then
    FReleasing:=True
  else
    {$IFDEF NEXTGEN} DisposeOf; {$ELSE} Free; {$ENDIF}
  end;

procedure TRtcConnection.Disconnect;
  begin
  if FReconnecting then
    begin
    FReconnecting:=False;
    TRtcTimer.Stop(FRecTimer);
    FRecTimer:=nil;
    end;
  FWantConnect:=False;
  FConnecting:=False;

  if assigned(Con) and MultiThreaded then
    Con.Disconnect
  else if InsideEvent then
    begin
    FReleasing:=False;
    FClosing:=True;
    FDisconnecting:=True;
    end
  else if assigned(Con) then
    Con.Disconnect;
  end;

procedure TRtcConnection.InternalDisconnect;
  begin
  if assigned(Con) then
    Con.InternalDisconnect;
  end;

function TRtcConnection.isClosing:boolean;
  begin
  if FReleasing or FClosing or FDisconnecting then
    Result:=True
  else if not assigned(Con) then
    Result:=True
  else if State<=conClosing then
    Result:=True
  else
    Result:=False;
  end;

procedure TRtcConnection.Check;
  begin
  if assigned(Con) then
    Con.Check;
  end;

function TRtcConnection.LocalAddr: RtcString;
  begin
  if assigned(Con) then
    Result:=Con.GetLocalAddr
  else
    Result:=FAddr;
  end;

function TRtcConnection.LocalPort: RtcString;
  begin
  if assigned(Con) then
    Result:=Con.GetLocalPort
  else
    Result:=FPort;
  end;

function TRtcConnection.PeerAddr: RtcString;
  begin
  if assigned(Con) then
    Result:=Con.GetPeerAddr
  else
    Result:='';
  end;

function TRtcConnection.PeerPort: RtcString;
  begin
  if assigned(Con) then
    Result:=Con.GetPeerPort
  else
    Result:='';
  end;

function TRtcConnection.PostJob(var myJob; HighPriority:boolean=False; ForceThread:boolean=False):boolean;
  var
    Job:TObject absolute myJob;
    xJob:TRtcJob absolute myJob;
  begin
  if assigned(self) and assigned(Job) then
    begin
    if assigned(Con) then
      Result:=Con.PostJob(Job,HighPriority,ForceThread)
    else if not MultiThreaded and (Job is TRtcJob) then
      begin
      if xJob.Run(nil) then
        if xJob.SingleUse then
          RtcFreeAndNil(Job);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.PostJobTo(Thr:TRtcThread; var myJob; HighPriority:boolean=False; ForceThread:boolean=False):boolean;
  var
    Job:TObject absolute myJob;
    xJob:TRtcJob absolute myJob;
  begin
  if assigned(self) and assigned(Job) then
    begin
    if assigned(Thr) then
      Result:=TRtcThread.PostJob(Thr,Job,HighPriority,ForceThread)
    else if not MultiThreaded and (Job is TRtcJob) then
      begin
      if xJob.Run(nil) then
        if xJob.SingleUse then
          RtcFreeAndNil(Job);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcNotifyEvent):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyEvent:=Event;
    Result:=TRtcThread.Sync(CallMyEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcErrorEvent; Err:Exception):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyErrEvent:=Event;
    FMyErrMsg:=Err;
    Result:=TRtcThread.Sync(CallMyError);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcUserEvent; Obj:TObject):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyUserEvent:=Event;
    FMyUserMsg:=Obj;
    Result:=TRtcThread.Sync(CallMyUserEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcUserDataEvent; Obj:TObject; Data:TRtcValue):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyUserDataEvent:=Event;
    FMyUserData:=Data;
    FMyUserMsg:=Obj;
    Result:=TRtcThread.Sync(CallMyUserDataEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcCustomEvent; Obj:TObject):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyCustEvent:=Event;
    FMyCustMsg:=Obj;
    Result:=TRtcThread.Sync(CallMyCustomEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcCustomDataEvent; Obj:TObject; Data:TRtcValue):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyCustDataEvent:=Event;
    FMyCustData:=Data;
    FMyCustMsg:=Obj;
    Result:=TRtcThread.Sync(CallMyCustomDataEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcFunctionCallEvent; Par:TRtcFunctionInfo; Res:TRtcValue):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyFuncEvent:=Event;
    FMyFunc:=Par;
    FMyFuncResult:=Res;
    Result:=TRtcThread.Sync(CallMyFuncEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcFunctionErrorEvent; Par:TRtcFunctionInfo; Res:TRtcValue; E:Exception; var Handled:boolean):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyFuncError:=Event;
    FMyFunc:=Par;
    FMyFuncResult:=Res;
    FMyErrMsg:=E;
    FMyHandled:=Handled;
    Result:=TRtcThread.Sync(CallMyFuncError);
    Handled:=FMyHandled;
    end;
  end;

function TRtcConnection.Sync(Event: TRtcResultEvent; Data:TRtcValue; Res:TRtcValue):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyResultEvent:=Event;
    FMyFuncResult:=Res;
    FMyFuncData:=Data;
    Result:=TRtcThread.Sync(CallMyResultEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcResultErrorEvent; Data:TRtcValue; Res:TRtcValue; E:Exception):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyResultErrorEvent:=Event;
    FMyFuncResult:=Res;
    FMyFuncData:=Data;
    FMyErrMsg:=E;
    Result:=TRtcThread.Sync(CallMyResultErrorEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcFunctionPrepareEvent; Data:TRtcValue):boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyFuncPrepEvent:=Event;
    FMyFuncData:=Data;
    Result:=TRtcThread.Sync(CallMyFuncPrepEvent);
    end;
  end;

function TRtcConnection.Sync(Event: TRtcObjectCreateEvent; Param: TRtcObjectCall): boolean;
  begin
  if (self=nil) or InsideMainThread or not assigned(Event) then
    Result:=False
  else
    begin
    FMyObjCreateEvent:=Event;
    FMyObjCreateParams:=Param;
    Result:=TRtcThread.Sync(CallMyObjCreateEvent);
    FMyObjCreateParams:=nil;
    end;
  end;

function TRtcConnection.GetParent: TRtcConnection;
  begin
  Result:=FParent;
  end;

procedure TRtcConnection.CallMyEvent;
  begin
  if FNotSilent then
    FMyEvent(self);
  end;

procedure TRtcConnection.CallMyError;
  begin
  if FNotSilent then
    FMyErrEvent(self,FMyErrMsg);
  end;

procedure TRtcConnection.CallMyUserEvent;
  begin
  if FNotSilent then
    FMyUserEvent(self,FMyUserMsg);
  end;

procedure TRtcConnection.CallMyUserDataEvent;
  begin
  if FNotSilent then
    FMyUserDataEvent(self,FMyUserMsg,FMyUserData);
  end;

procedure TRtcConnection.CallMyCustomEvent;
  begin
  if FNotSilent then
    FMyCustEvent(self,FMyCustMsg);
  end;

procedure TRtcConnection.CallMyCustomDataEvent;
  begin
  if FNotSilent then
    FMyCustDataEvent(self,FMyCustMsg,FMyCustData);
  end;

procedure TRtcConnection.CallMyFuncEvent;
  begin
  if FNotSilent then
    FMyFuncEvent(self,FMyFunc,FMyFuncResult);
  end;

procedure TRtcConnection.CallMyFuncError;
  begin
  if FNotSilent then
    FMyFuncError(self,FMyFunc,FMyFuncResult,FMyErrMsg,FMyHandled);
  end;

procedure TRtcConnection.CallMyResultEvent;
  begin
  if FNotSilent then
    FMyResultEvent(self,FMyFuncData,FMyFuncResult);
  end;

procedure TRtcConnection.CallMyResultErrorEvent;
  begin
  if FNotSilent then
    FMyResultErrorEvent(self,FMyFuncData,FMyFuncResult,FMyErrMsg);
  end;

procedure TRtcConnection.CallMyFuncPrepEvent;
  begin
  if FNotSilent then
    FMyFuncPrepEvent(self,FMyFuncData);
  end;

procedure TRtcConnection.CallMyObjCreateEvent;
  begin
  if FNotSilent then
    FMyObjCreateEvent(self,FMyObjCreateParams);
  end;

procedure TRtcConnection.TriggerConnecting;
  begin
  if not FConnectingTriggered then
    begin
    FConnectingTriggered:=True;

    EnterEvent;
    try
      CallConnecting;
    finally
      LeaveEvent;
      end;

    Timeout.Connecting;
    end;
  end;

procedure TRtcConnection.TriggerDisconnecting;
  begin
  if FConnectingTriggered then
    begin
    FConnectingTriggered:=False;

    EnterEvent;
    try
      CallDisconnecting;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcConnection.TriggerConnect;
  begin
  if not FConnectTriggered then
    begin
    FConnectTriggered:=True;

    EnterEvent;
    try
      CallConnect;
    finally
      LeaveEvent;
      end;

    Timeout.Connect;
    end;
  end;

procedure TRtcConnection.TriggerDisconnect;
  begin
  if FConnectTriggered then
    begin
    FConnectTriggered:=False;

    EnterEvent;
    try
      FClosing:=True;
      CallDisconnect;
    finally
      FClosing:=False;
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcConnection.TriggerException(E: Exception);
  begin
  EnterEvent;
  try
    CallException(E);
  finally
    LeaveEvent;
    end;
  end;

function TRtcConnection.State: TRtcConnectionState;
  begin
  if self=nil then
    Result:=conInactive
  else if assigned(Con) then
    Result:=Con.GetState
  else
    Result:=conInactive;
  end;

function TRtcConnection.ConnectionClosed:boolean;
  begin
  if self=nil then Result:=True
  else Result:=State<=conPrepared;
  end;

function TRtcConnection.CheckProviderReleased:boolean;
  begin
  Result:= Con=nil;
  end;

procedure TRtcConnection.ReleaseProvider;
  var
    inside:boolean;
  begin
  if self=nil then Exit;
  if assigned(Con) then
    begin
    inside:=inThread;
    if FAmSilent then
      Con.Silent:=True;
    if Con.ReleaseMe(inside) then
      RtcFreeAndNil(Con)
    else
      begin
      if RtcWaitFor(CheckProviderReleased,CheckProviderReleased,RTC_WAITFOR_RELEASE_PROVIDER)<>wait_OK then
        if LOG_EXCEPTIONS then
          Log(RtcString(ClassName)+'.ReleaseProvider Releasing ('+PeerAddr+':'+PeerPort+') TIMEOUT!','ERROR');
      Sleep(RTC_WAITFOR_PROVIDER_FINISH);
      end;
    end;
  end;

procedure TRtcConnection.SetTriggers;
  begin
  if assigned(Con) then
    with Con do
      begin
      SetError(self.Error);

      SetTriggerBeforeCreate(self.TriggerBeforeCreate);
      SetTriggerAfterDestroy(self.TriggerAfterDestroy);

      SetTriggerConnecting(self.TriggerConnecting);
      SetTriggerConnect(self.TriggerConnect);
      SetTriggerDisconnect(self.TriggerDisconnect);
      SetTriggerDisconnecting(self.TriggerDisconnecting);

      SetTriggerReadyToRelease(self.TriggerReadyToRelease);

      SetTriggerLastWrite(self.TriggerLastWrite);

      SetTriggerDataOut(self.TriggerDataOut);
      SetTriggerDataIn(self.TriggerDataIn);
      SetTriggerDataSent(self.TriggerDataSent);
      SetTriggerDataReceived(self.TriggerDataReceived);
      SetTriggerDataLost(self.TriggerDataLost);
      SetTriggerException(self.TriggerException);
      end;
  end;

procedure TRtcConnection.ClearTriggers;
  begin
  if assigned(Con) then
    with Con do
      begin
      SetError(nil);

      SetTriggerReadyToRelease(nil);

      SetTriggerBeforeCreate(nil);
      SetTriggerAfterDestroy(nil);

      SetTriggerConnecting(nil);
      SetTriggerConnect(nil);
      SetTriggerDisconnect(nil);
      SetTriggerDisconnecting(nil);

      SetTriggerLastWrite(nil);
      SetTriggerDataOut(nil);
      SetTriggerDataIn(nil);
      SetTriggerDataSent(nil);
      SetTriggerDataReceived(nil);
      SetTriggerDataLost(nil);
      SetTriggerException(nil);
      end;
  end;

procedure TRtcConnection.SetParams;
  begin
  FDataOut:=0;
  FDataIn:=0;
  FReadCount:=0;
  FWriteCount:=0;
  if assigned(Con) then
    with Con do
      begin
      SetAddr(self.ServerAddr);
      SetPort(self.ServerPort);
      SetIPV(self.ServerIPV);
      SetMultiThreaded(self.MultiThreaded);
      end;
  end;

procedure TRtcConnection.TriggerAfterDestroy;
  begin
  if assigned(Timeout) then Timeout.Stop;
  ClearTriggers;
  Con:=nil;

  if FReleaseCall or AutoRelease then
    if InsideEvent then
      FFree:=True
    else
    {$IFDEF NEXTGEN} DisposeOf; {$ELSE} Free; {$ENDIF}
  end;

procedure TRtcConnection.TriggerBeforeCreate;
  begin
  //
  end;

procedure TRtcConnection.EnterEvent;
  begin
  Inc(FInsideEvent);
  end;

procedure TRtcConnection.LeaveEvent;
  begin
  Dec(FInsideEvent);
  if FInsideEvent=0 then
    if FFree then
      {$IFDEF NEXTGEN} DisposeOf; {$ELSE} Free; {$ENDIF}
  end;

procedure TRtcConnection.CallConnect;
  begin
  if assigned(OnConnect) and FNotSilent then
    OnConnect(self);
  end;

procedure TRtcConnection.CallConnecting;
  begin
  if assigned(OnConnecting) and FNotSilent then
    OnConnecting(self);
  end;

procedure TRtcConnection.CallDataReceived;
  begin
  if assigned(OnDataReceived) and FNotSilent then
    OnDataReceived(self);
  end;

function TRtcConnection.AutoRelease: boolean;
  begin
  Result:=False;
  end;

procedure TRtcConnection.CallAfterManualRead;
  begin
  // defaults to empty implementation, has to be implemented
  // by connection components which support manual reading.
  end;

procedure TRtcConnection.CallDataOut;
  begin
  if assigned(OnDataOut) and FNotSilent then
    OnDataOut(self);
  end;

procedure TRtcConnection.CallDataIn;
  begin
  if assigned(OnDataIn) and FNotSilent then
    OnDataIn(self);
  end;

procedure TRtcConnection.CallLastWrite;
  begin
  // placeholder
  end;

procedure TRtcConnection.CallDataSent;
  begin
  if assigned(OnDataSent) and FNotSilent then
    OnDataSent(self);
  end;

procedure TRtcConnection.CallDisconnect;
  begin
  if assigned(OnDisconnect) and FNotSilent then
    OnDisconnect(self);
  end;

procedure TRtcConnection.CallDisconnecting;
  begin
  if assigned(OnDisconnecting) and FNotSilent then
    OnDisconnecting(self);
  end;

procedure TRtcConnection.CallException(E: Exception);
  begin
  if assigned(OnException) and FNotSilent then
    OnException(self,E);
  end;

procedure TRtcConnection.CallReadyToSend;
  begin
  if assigned(OnReadyToSend) and FNotSilent then
    OnReadyToSend(self);
  end;

function TRtcConnection.InsideEvent: boolean;
  begin
  Result:=inThread and (FInsideEvent>0);
  end;

function TRtcConnection.LeavingEvent: boolean;
  begin
  Result:=FInsideEvent=1;
  end;

procedure TRtcConnection.CallDataLost;
  begin
  // NO implementation
  end;

procedure TRtcConnection.TriggerDataLost;
  begin
  // NO implementation
  end;

function TRtcConnection.isExtension: boolean;
  begin
  Result:=False;
  end;

procedure TRtcConnection.SetTimeout(const Value: TRtcTimeout);
  begin
  if Value<>FTimeout then
    FTimeout.Assign(Value);
  end;

procedure TRtcConnection.SetTimeoutsOfAPI(const Value: TRtcTimeoutsOfAPI);
  begin
  if Value<>FTimeoutsOfAPI then
    FTimeoutsOfAPI.Assign(Value);
  end;

procedure TRtcConnection.AfterManualRead;
  begin
  CallAfterManualRead;
  end;

type
  TRtcConEventJob=class(TRtcJob)
  public
    con:TRtcConnection;
    ev:TRtcNotifyEvent;
    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcConEventJob.Run(Thr: TRtcThread): boolean;
  begin
  Result:=True; // auto-release object
  ev(con);
  end;

function TRtcConnection.PostEvent(Evnt: TRtcNotifyEvent; HighPriority:boolean=False; ForceThread:boolean=False): boolean;
  var
    FJob:TRtcConEventJob;
  begin
  FJob:=TRtcConEventJob.Create;
  FJob.con:=self;
  FJob.ev:=Evnt;
  if PostJob(FJob,HighPriority,ForceThread) then
    Result:=True
  else
    begin
    Result:=False;
    if FJob.SingleUse then
      RtcFreeAndNil(FJob);
    end;
  end;

function TRtcConnection.PostEventTo(Thr:TRtcThread; Evnt: TRtcNotifyEvent; HighPriority:boolean=False; ForceThread:boolean=False): boolean;
  var
    FJob:TRtcConEventJob;
  begin
  FJob:=TRtcConEventJob.Create;
  FJob.con:=self;
  FJob.ev:=Evnt;
  if PostJobTo(Thr,FJob,HighPriority,ForceThread) then
    Result:=True
  else
    begin
    Result:=False;
    if FJob.SingleUse then
      RtcFreeAndNil(FJob);
    end;
  end;

function TRtcConnection.TimedOut: boolean;
  begin
  Result:=FTimedOut;
  end;

function TRtcConnection.Thread: TRtcThread;
  begin
  if assigned(Con) then
    Result:=Con.GetThread
  else
    Result:=nil;
  end;

type
  TRtcWSFramePayloadJob=class(TRtcJob)
  public
    c:TRtcConnection;
    iFrame:TRtcWSFrame;
    iPayload:RtcByteArray;
    iFinal:boolean;
    destructor Destroy; override;
    function Run(Thr:TRtcThread):boolean; override;
    end;
  TRtcWSFrameQueueJob=class(TRtcJob)
  public
    c:TRtcConnection;
    iFrame:TRtcWSFrame;
    iName:RtcWideString;
    destructor Destroy; override;
    function Run(Thr:TRtcThread):boolean; override;
    end;
  TRtcWSFrameQueueIfIdleJob=class(TRtcJob)
  public
    c:TRtcConnection;
    iFrame:TRtcWSFrame;
    iName:RtcWideString;
    destructor Destroy; override;
    function Run(Thr:TRtcThread):boolean; override;
    end;
  TRtcWSFramesClearJob=class(TRtcJob)
  public
    c:TRtcConnection;
    destructor Destroy; override;
    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcWSFramePayloadJob.Run(Thr:TRtcThread):boolean;
  begin
  if iFinal then
    iFrame.wfFinished:=True;
  iFrame.wfWriteEx(iPayload);
  SetLength(iPayload,0);
  c.ws_SendNextQueuedFrame(False);
  Result:=True;
  end;

function TRtcWSFrameQueueJob.Run(Thr:TRtcThread):boolean;
  begin
  try
    if c.ws_AddFrameToQueue(iFrame,iName) then
      begin
      iFrame:=nil;
      c.ws_SendNextQueuedFrame(False);
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  iName:='';
  Result:=True;
  end;

function TRtcWSFrameQueueIfIdleJob.Run(Thr:TRtcThread):boolean;
  begin
  try
    if c.ws_AddFrameToQueueIfIdle(iFrame,iName) then
      begin
      iFrame:=nil;
      c.ws_SendNextQueuedFrame(False);
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  iName:='';
  Result:=True;
  end;

function TRtcWSFramesClearJob.Run(Thr:TRtcThread):boolean;
  begin
  if (c.State=conActive) and c.isWebSocket then
    c.ws_ClearSendingQueue;
  c:=nil;
  Result:=True;
  end;

destructor TRtcWSFramePayloadJob.Destroy;
  begin
  c:=nil;
  iFrame:=nil;
  SetLength(iPayload,0);
  inherited;
  end;

destructor TRtcWSFrameQueueJob.Destroy;
  begin
  c:=nil;
  RtcFreeAndNil(iFrame);
  iName:='';
  inherited;
  end;

destructor TRtcWSFrameQueueIfIdleJob.Destroy;
  begin
  c:=nil;
  RtcFreeAndNil(iFrame);
  iName:='';
  inherited;
  end;

destructor TRtcWSFramesClearJob.Destroy;
  begin
  c:=nil;
  inherited;
  end;

function TRtcConnection.wSend(iFrame:TRtcWSFrame; const iName:RtcWideString=''):boolean;
  var
    job:TRtcWSFrameQueueJob;
  begin
  Result:=False;
  if iFrame=nil then Exit;
  try
    if (Self<>nil) and isWebSocket and (State=conActive) then
      begin
      if iFrame.waMasked=isServer then
        iFrame.waMasked:=not isServer;

      if InsideEvent then
        begin
        if ws_AddFrameToQueue(iFrame,iName) then
          begin
          iFrame:=nil;
          ws_SendNextQueuedFrame(False);
          Result:=True;
          end;
        end
      else
        begin
        job:=TRtcWSFrameQueueJob.Create;
        try
          job.c:=Self;
          job.iFrame:=iFrame; iFrame:=nil;
          job.iName:=iName;
          if PostJob(job) then
            begin
            job:=nil;
            Result:=True;
            end;
        finally
          RtcFreeAndNil(job);
          end;
        end;
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  end;

function TRtcConnection.wSendMore(iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False):boolean;
  var
    job:TRtcWSFramePayloadJob;
  begin
  Result:=False;
  if (iFrame=nil) or (Self=nil) or ((vFinal=False) and (length(vPayload)=0)) then Exit;
  if isWebSocket and (State=conActive) then
    if InsideEvent then
      begin
      if vFinal then
        iFrame.wfFinished:=True;
      iFrame.wfWriteEx(vPayload);
      ws_SendNextQueuedFrame(False);
      Result:=True;
      end
    else
      begin
      job:=TRtcWSFramePayloadJob.Create;
      try
        job.c:=Self;
        job.iFrame:=iFrame;
        job.iFinal:=vFinal;
        job.iPayload:=vPayload;
        if PostJob(job) then
          begin
          job:=nil;
          Result:=True;
          end;
      finally
        RtcFreeAndNil(job);
        end;
      end;
  end;

function TRtcConnection.wSendMore(iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False):boolean;
  var
    job:TRtcWSFramePayloadJob;
  begin
  Result:=False;
  if (iFrame=nil) or (Self=nil) or ((vFinal=False) and (length(vPayload)=0)) then Exit;
  if isWebSocket and (State=conActive) then
    if InsideEvent then
      begin
      if vFinal then
        iFrame.wfFinished:=True;
      iFrame.wfWrite(vPayload);
      ws_SendNextQueuedFrame(False);
      Result:=True;
      end
    else
      begin
      job:=TRtcWSFramePayloadJob.Create;
      try
        job.c:=Self;
        job.iFrame:=iFrame;
        job.iFinal:=vFinal;
        job.iPayload:=RtcStringToBytes(vPayload);
        if PostJob(job) then
          begin
          job:=nil;
          Result:=True;
          end;
      finally
        RtcFreeAndNil(job);
        end;
      end;
  end;

function TRtcConnection.wSendIfIdle(iFrame:TRtcWSFrame; const iName:RtcWideString=''):boolean;
  var
    job:TRtcWSFrameQueueIfIdleJob;
  begin
  Result:=False;
  if iFrame=nil then Exit;
  try
    if (Self<>nil) and isWebSocket and (State=conActive) then
      begin
      if iFrame.waMasked=isServer then
        iFrame.waMasked:=not isServer;

      if InsideEvent then
        begin
        if ws_AddFrameToQueueIfIdle(iFrame,iName) then
          begin
          iFrame:=nil;
          ws_SendNextQueuedFrame(False);
          Result:=True;
          end;
        end
      else
        begin
        job:=TRtcWSFrameQueueIfIdleJob.Create;
        try
          job.c:=Self;
          job.iFrame:=iFrame; iFrame:=nil;
          job.iName:=iName;
          if PostJob(job) then
            begin
            job:=nil;
            Result:=True;
            end;
        finally
          RtcFreeAndNil(job);
          end;
        end;
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  end;

function TRtcConnection.wSend(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): boolean;
  begin
  if (Self<>nil) and isWebSocket and (State=conActive) then
    Result:=wSend(TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength))
  else
    Result:=False;
  end;

function TRtcConnection.wSend(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): boolean;
  begin
  if (Self<>nil) and isWebSocket and (State=conActive) then
    Result:=wSend(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength))
  else
    Result:=False;
  end;

function TRtcConnection.wSend(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): boolean;
  begin
  if (Self<>nil) and isWebSocket and (State=conActive) then
    Result:=wSend(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength))
  else
    Result:=False;
  end;

function TRtcConnection.wSendIfIdle(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): boolean;
  begin
  if (Self<>nil) and isWebSocket and (State=conActive) then
    Result:=wSendIfIdle(TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength))
  else
    Result:=False;
  end;

function TRtcConnection.wSendIfIdle(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): boolean;
  begin
  if (Self<>nil) and isWebSocket and (State=conActive) then
    Result:=wSendIfIdle(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength))
  else
    Result:=False;
  end;

function TRtcConnection.wSendIfIdle(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): boolean;
  begin
  if (Self<>nil) and isWebSocket and (State=conActive) then
    Result:=wSendIfIdle(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength))
  else
    Result:=False;
  end;

function TRtcConnection.wsClearSendingQueue:boolean;
  var
    job:TRtcWSFramesClearJob;
  begin
  Result:=False;
  if Self=nil then Exit;
  if InsideEvent then
    Result:=ws_ClearSendingQueue
  else if IsWebSocket and (State=conActive) then
    begin
    job:=TRtcWSFramesClearJob.Create;
    try
      job.c:=Self;
      if PostJob(job) then
        begin
        Result:=True;
        job:=nil;
        end
    finally
      RtcFreeAndNil(job);
      end;
    end;
  end;

function TRtcConnection.ws_GetFrame(const iName: RtcWideString): TRtcWSFrame;
  var
    obj:TObject;
  begin
  Result:=nil;
  obj:=Info.asObj[iName];
  if (obj<>nil) and (obj is TRtcWSFrame) then
    Result:=TRtcWSFrame(obj);
  end;

procedure TRtcConnection.ws_SetFrame(const iName: RtcWideString; const Value: TRtcWSFrame);
  var
    obj:TObject;
    oldFrame:TRtcWSFrame absolute obj;
  begin
  obj:=Info.asObj[iName];
  if obj<>Value then
    if Value=nil then
      Info.SetNil(iName)
    else if obj=nil then
      begin
      Value.SetOwner(self, iName);
      Info.asObj[iName]:=Value;
      end
    else if obj is TRtcWSFrame then
      begin
      if oldFrame.FOwner=self then
        begin
        Value.SetOwner(self, iName);
        Info.asObj[iName]:=Value;
        end
      else
        begin
        Value.SetOwner(self, iName);
        Info.asObj[iName]:=Value;
        end;
      end
    else
      begin
      Value.SetOwner(self, iName);
      Info.asObj[iName]:=Value;
      end;
  end;

function TRtcConnection.ws_GetTemp(const iName: RtcWideString): TRtcWSFrame;
  var
    obj:TObject;
  begin
  Result:=nil;
  obj:=Info.asPtr[iName];
  if (obj<>nil) and (obj is TRtcWSFrame) then
    Result:=TRtcWSFrame(obj);
  end;

procedure TRtcConnection.ws_SetTemp(const iName: RtcWideString; const Value: TRtcWSFrame);
  begin
  Info.asPtr[iName]:=Value;
  end;

procedure TRtcConnection.ws_SendingFrame(iFrame: TRtcWSFrame);
  begin
  FWSFrameOther.removeThis(iFrame);
  FWSFrameOUT:=iFrame;
  end;

procedure TRtcConnection.ws_OtherFrame(iFrame: TRtcWSFrame);
  begin
  FWSFrameOther.addLast(iFrame);
  end;

procedure TRtcConnection.ws_RemoveFrame(iFrame: TRtcWSFrame);
  begin
  if iFrame=FWSFrameIN then FWSFrameIN:=nil;
  if iFrame=FWSFrameOUT then FWSFrameOUT:=nil;
  FWSFrameOther.removeThis(iFrame);
  FWSFrameQueued.removeThis(iFrame);
  if iFrame.wfName<>'' then
    if Info.asObj[iFrame.wfName]=iFrame then
      Info.asObj[iFrame.wfName]:=nil;
  end;

function TRtcConnection.ws_AddFrameToQueue(iFrame: TRtcWSFrame; const iName: RtcWideString):boolean;
  begin
  if assigned(iFrame) and isWebSocket and (State=conActive) and assigned(FWSFrameQueued) then
    begin
    FWSFrameQueued.removeThis(iFrame);
    // Update "Name" and remove from any other Owner (if assigned) ...
    if iName='' then
      iFrame.SetOwner(nil,iFrame.wfName)
    else
      iFrame.SetOwner(nil,iName);
    FWSFrameQueued.addLast(iFrame);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcConnection.ws_AddFrameToQueueIfIdle(iFrame: TRtcWSFrame; const iName: RtcWideString):boolean;
  begin
  if assigned(iFrame) and isWebSocket and (State=conActive) and assigned(FWSFrameQueued) and
    (FWSFrameQueued.Count=0) and (FWSFrameOUT=nil) then // Queue Empty
    begin
    // Update "Name" and remove from any other Owner (if assigned) ...
    if iName='' then
      iFrame.SetOwner(nil,iFrame.wfName)
    else
      iFrame.SetOwner(nil,iName);
    FWSFrameQueued.addLast(iFrame);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcConnection.ws_ClearTempFrames:boolean;
  var
    obj:TObject;
    iFrame:TRtcWSFrame absolute obj;
  begin
  Result:=False;
  if isWebSocket and (State=conActive) then
    begin
    Result:=True;

    obj:=FWSFrameIN;
    if assigned(obj) then
      if iFrame.waFinal and iFrame.wfComplete then
        RtcFreeAndNil(iFrame);

    if assigned(FWSFrameOther) then
      while FWSFrameOther.Count>0 do
        begin
        obj:=FWSFrameOther.First;
        if iFrame.waFinal and iFrame.wfComplete then
          RtcFreeAndNil(iFrame)
        else
          Break;
        end;
    end;
  end;

procedure TRtcConnection.ws_SendNextQueuedFrame(Cleanup:boolean);
  procedure SendNext;
    var
      obj:TObject;
      iFrame:TRtcWSFrame absolute obj;
    begin
    if FWSFrameQueued.Count>0 then
      begin
      FWSFrameQueued.extractFirst(obj);
      iFrame.SetOwner(self,iFrame.wfName);
      if iFrame.wfName<>'' then
        wsFrame[iFrame.wfName]:=iFrame;
      WriteEx(iFrame.wfReadEx);
      end;
    end;
  begin
  if isWebSocket and (State=conActive) and (FWSFrameQueued<>nil) then
    begin
    if FWSFrameOUT=nil then
      SendNext
    else if FWSFrameOUT.waFinal and FWSFrameOUT.wfDone then // Final Frame and all bytes sent?
      begin
      if CleanUp then
        begin
        RtcFreeAndNil(FWSFrameOUT);
        SendNext;
        end;
      end
    else if FWSFrameOUT.wfPayloadReady>0 then
      WriteEx(FWSFrameOUT.wfReadEx);
    end;
  end;

function TRtcConnection.ws_ClearSendingQueue:boolean;
  var
    frm:TObject;
  begin
  Result:=False;
  if isWebSocket and (State=conActive) and assigned(FWSFrameQueued) then
    begin
    Result:=True;
    while FWSFrameQueued.Count>0 do
      begin
      FWSFrameQueued.extractFirst(frm);
      if frm<>nil then
        RtcFreeAndNil(frm);
      end;
    end;
  end;

procedure TRtcConnection.ws_Clear;
  var
    frm:TObject;
  begin
  FWebSocket:=False;
  FWebSocketData:=False;

  frm:=FWSFrameIN;
  if assigned(frm) then
    RtcFreeAndNil(frm);

  frm:=FWSFrameOUT;
  if assigned(frm) then
    RtcFreeAndNil(frm);

  if assigned(FWSFrameOther) then
    while FWSFrameOther.Count>0 do
      begin
      FWSFrameOther.extractFirst(frm);
      if frm<>nil then
        RtcFreeAndNil(frm);
      end;
  if assigned(FWSFrameQueued) then
    while FWSFrameQueued.Count>0 do
      begin
      FWSFrameQueued.extractFirst(frm);
      if frm<>nil then
        RtcFreeAndNil(frm);
      end;
  end;

function TRtcConnection.ws_GetFrameIn: TRtcWSFrame;
  begin
  if assigned(FWSFrameIN) then
    begin
    Result:=FWSFrameIN;
    if FWebSocketData then
      begin
      FWebSocketData:=False;
      Result.wfWriteEx(ReadEx);
      end;
    end
  else
    begin
    if FWebSocketData then
      begin
      FWSFrameIN:=TRtcWSFrame.Create(ReadEx);
      FWebSocketData:=False;
      end
    else
      FWSFrameIN:=TRtcWSFrame.Create('');
    FWSFrameIN.SetOwner(self,'');
    Result:=FWSFrameIN;
    end;
  end;

function TRtcConnection.wsGetID:RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsGetID(self);
  end;

function TRtcConnection.wsCount: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsCount;
  end;

function TRtcConnection.wsFirst: RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsFirst;
  end;

function TRtcConnection.wsLast: RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsLast;
  end;

function TRtcConnection.wsNext(id: RtcIntPtr): RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsNext(id);
  end;

function TRtcConnection.wsPrior(id: RtcIntPtr): RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsPrior(id);
  end;

function TRtcConnection.wSend(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,iFrame,iName);
  end;

function TRtcConnection.wSendMore(id: RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) or ((vFinal=False) and (length(vPayload)=0)) then
    Result:=0
  else
    Result:=FWS.wSendMore(id,iFrame,vPayload,vFinal);
  end;

function TRtcConnection.wSendMore(id: RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) or ((vFinal=False) and (length(vPayload)=0)) then
    Result:=0
  else
    Result:=FWS.wSendMore(id,iFrame,vPayload,vFinal);
  end;

function TRtcConnection.wSendIfIdle(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,iFrame,iName);
  end;

function TRtcConnection.wSendToAll(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToAll(iFrame,iName);
  end;

function TRtcConnection.wSendToIdle(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToIdle(iFrame,iName);
  end;

function TRtcConnection.wSendToOthers(xid: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToOthers(xid,iFrame,iName);
  end;

function TRtcConnection.wsDisconnect: boolean;
  begin
  if (self<>nil) and IsWebSocket and (State=conActive) then
    begin
    Result:=True;
    Disconnect;
    end
  else
    Result:=False;
  end;

function TRtcConnection.wsDisconnect(id: RtcIntPtr): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsDisconnect(id);
  end;

function TRtcConnection.wsDisconnectAll: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsDisconnectAll;
  end;

function TRtcConnection.wsClearSendingQueue(id: RtcIntPtr): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsClearSendingQueue(id);
  end;

function TRtcConnection.wsClearAllSendingQueues: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsClearAllSendingQueues;
  end;

function TRtcConnection.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToAll(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToIdle(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcConnection.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

{ RtcWSHeader utility functions }

{ A single-frame unmasked text message:
   [$81,
    $05,
    $48,$65,$6c,$6c,$6f] (contains "Hello")

 A single-frame masked text message:
   [$81,
    $85,
    $37,$fa,$21,$3d,
    $7f,$9f,$4d,$51,$58] (contains "Hello")

 A fragmented unmasked text message:
   [$01,$03,$48,$65,$6c] (contains "Hel")
   [$80,$02,$6c,$6f] (contains "lo")

 Unmasked Ping request and masked Ping response:
   [$89,$05,$48,$65,$6c,$6c,$6f] (contains a body of "Hello", but the contents of the body are arbitrary)
   [$8a,$85,$37,$fa,$21,$3d,$7f,$9f,$4d,$51,$58] (contains a body of "Hello", matching the body of the ping)

 256 bytes binary message in a single unmasked frame:
   [$82,$7E,$01,$00 + 256 bytes of binary data]

 64KiB binary message in a single unmasked frame:
   [$82,$7F,$00,$00,$00,$00,$00,$01,$00,$00 + 65536 bytes of binary data] }

(*
  RtcWSHeader=record
    { 1 bit:
      Indicates that this is the final fragment in a message.
      The first fragment MAY also be the final fragment. }
    FIN: Rtc1Bit;
    { 1 bit each:
      MUST be 0 unless an extension is negotiated that defines meanings
      for non-zero values.  If a nonzero value is received and none of
      the negotiated extensions defines the meaning of such a nonzero
      value, the receiving endpoint MUST _Fail the WebSocket Connection_. }
    RSV1, RSV2, RSV3: Rtc1Bit;
    { 4 bits:
      Defines the interpretation of the "Payload data".  If an
      unknown opcode is received, the receiving endpoint MUST
      _Fail the WebSocket Connection_. The following values are defined:

      *  0 = denotes a continuation frame

      *  1 = denotes a text frame

      *  2 = denotes a binary frame

      *  3 - 7 = are reserved for further non-control frames

      *  8 = denotes a connection close

      *  9 = denotes a ping

      *  10 = denotes a pong

      *  11 - 15 = are reserved for further control frames }
    Opcode: Rtc4Bit;
    { 1 bit:
      Defines whether the "Payload data" is masked.  If set to 1, a
      masking key is present in masking-key, and this is used to unmask
      the "Payload data" as per Section 5.3.  All frames sent from
      client to server have this bit set to 1. }
    Masked: Rtc1Bit;
    { 7 bits, 7+16 bits, or 7+64 bits:
      The length of the "Payload data", in bytes: if 0-125, that is the
      payload length.  If 126, the following 2 bytes interpreted as a
      16-bit unsigned integer are the payload length.  If 127, the
      following 8 bytes interpreted as a 64-bit unsigned integer (the
      most significant bit MUST be 0) are the payload length.  Multibyte
      length quantities are expressed in network byte order.  Note that
      in all cases, the minimal number of bytes MUST be used to encode
      the length, for example, the length of a 124-byte-long string
      can't be encoded as the sequence 126, 0, 124.  The payload length
      is the length of the "Extension data" + the length of the
      "Application data".  The length of the "Extension data" may be
      zero, in which case the payload length is the length of the
      "Application data". }
    PayloadLength: int64;
    { 0 or 4 bytes:
      All frames sent from the client to the server are masked by a
      32-bit value that is contained within the frame.  This field is
      present if the mask bit is set to 1 and is absent if the mask bit
      is set to 0.  See Section 5.3 for further information on client-
      to-server masking. }
    MaskingKey:Rtc4ByteMask;
    end;
*)

function RtcWebSockCheck(const Source:RtcByteArray):int64; overload;
  var
    need:byte;
  begin
(*
  // 1 bit => 1.1
  FIN: Rtc1Bit;
  // 1 bit each: => 1.2 - 1.4
  RSV1, RSV2, RSV3: Rtc1Bit;
  // 4 bits:     => 1.5 - 1.8
  Opcode: Rtc4Bit;

  // 1 bit:      => 2.1
  Masked: Rtc1Bit;
  // 0-125 = 7 bits = 2.2 - 2.8
  // 126 = 7+16 bits = 2.2 - 2.8, 3 - 4
  // 127 = 7+64 bits => 2.2 - 2.8, 3 - 6
  PayloadLength: int64;

  { 0 or 4 bytes:
    All frames sent from the client to the server are masked by a
    32-bit value that is contained within the frame.  This field is
    present if the mask bit is set to 1 and is absent if the mask bit
    is set to 0.  See Section 5.3 for further information on client-
    to-server masking. }
  MaskingKey:Rtc4ByteMask;
*)
  Result:=0;
  if length(Source)<2 then
    Exit
  else
    begin
    need:=2;
    if Source[1] and 128 = 128 then // masked
      Inc(need,4); // 4 bytes mask

    if (Source[1] and 127)>=126 then // length
      if (Source[1] and 127)=127 then
        Inc(need,8) // 64-bit length
      else
        Inc(need,2); // 16-bit length

    if length(Source)<need then
      Exit
    else if (Source[1] and $7F = 127) and
            (Source[2] and $80 > 0) then
      Result:=-1
    else
      begin
      if (Source[1] and $7F)<=125 then
        Result := Source[1] and $7F
      else if (Source[1] and $7F)=127 then
        begin // 64-bit length
        Result := Source[2];
        Result := Result shl 8 + Source[3];
        Result := Result shl 8 + Source[4];
        Result := Result shl 8 + Source[5];
        Result := Result shl 8 + Source[6];
        Result := Result shl 8 + Source[7];
        Result := Result shl 8 + Source[8];
        Result := Result shl 8 + Source[9];
        end
      else
        begin // 16-bit length
        Result := Source[2];
        Result := Result shl 8 + Source[3];
        end;

      Result := Result + need;
      end;
    end;
  end;

function RtcWebSockCheck(const Head:RtcWSHeader):int64; overload;
  var
    need:byte;
  begin
(*
  // 1 bit => 1.1
  FIN: Rtc1Bit;
  // 1 bit each: => 1.2 - 1.4
  RSV1, RSV2, RSV3: Rtc1Bit;
  // 4 bits:     => 1.5 - 1.8
  Opcode: Rtc4Bit;

  // 1 bit:      => 2.1
  Masked: Rtc1Bit;
  // 0-125 = 7 bits = 2.2 - 2.8
  // 126 = 7+16 bits = 2.2 - 2.8, 3 - 4
  // 127 = 7+64 bits => 2.2 - 2.8, 3 - 6
  PayloadLength: int64;

  { 0 or 4 bytes:
    All frames sent from the client to the server are masked by a
    32-bit value that is contained within the frame.  This field is
    present if the mask bit is set to 1 and is absent if the mask bit
    is set to 0.  See Section 5.3 for further information on client-
    to-server masking. }
  MaskingKey:Rtc4ByteMask;
*)
  if Head.PayloadLength<0 then
    Result:=-1
  else
    begin
    need:=2;
    if Head.PayloadLength>125 then
      if Head.PayloadLength>$FFFF then
        Inc(need,8)
      else
        Inc(need,2);
    if Head.Masked=TRUE then Inc(need,4);
    Result:=Head.PayloadLength+need;
    end;
  end;

function RtcWebSockHeadLen(const Head:RtcWSHeader):byte; overload;
  begin
(*
  // 1 bit => 1.1
  FIN: Rtc1Bit;
  // 1 bit each: => 1.2 - 1.4
  RSV1, RSV2, RSV3: Rtc1Bit;
  // 4 bits:     => 1.5 - 1.8
  Opcode: Rtc4Bit;

  // 1 bit:      => 2.1
  Masked: Rtc1Bit;
  // 0-125 = 7 bits = 2.2 - 2.8
  // 126 = 7+16 bits = 2.2 - 2.8, 3 - 4
  // 127 = 7+64 bits => 2.2 - 2.8, 3 - 6
  PayloadLength: int64;

  { 0 or 4 bytes:
    All frames sent from the client to the server are masked by a
    32-bit value that is contained within the frame.  This field is
    present if the mask bit is set to 1 and is absent if the mask bit
    is set to 0.  See Section 5.3 for further information on client-
    to-server masking. }
  MaskingKey:Rtc4ByteMask;
*)
  Result:=2;
  if Head.PayloadLength>125 then
    if Head.PayloadLength>$FFFF then
      Inc(Result,8)
    else
      Inc(Result,2);
  if Head.Masked=TRUE then Inc(Result,4);
  end;

function RtcWebSockHead(const Source:RtcByteArray):RtcWSHeader; overload;
  var
    need:byte;
  begin
(*
  // 1 bit => 1.1
  FIN: Rtc1Bit;
  // 1 bit each: => 1.2 - 1.4
  RSV1, RSV2, RSV3: Rtc1Bit;
  // 4 bits:     => 1.5 - 1.8
  Opcode: Rtc4Bit;

  // 1 bit:      => 2.1
  Masked: Rtc1Bit;
  // 0-125 = 7 bits = 2.2 - 2.8
  // 126 = 7+16 bits = 2.2 - 2.8, 3 - 4
  // 127 = 7+64 bits => 2.2 - 2.8, 3 - 6
  PayloadLength: int64;

  { 0 or 4 bytes:
    All frames sent from the client to the server are masked by a
    32-bit value that is contained within the frame.  This field is
    present if the mask bit is set to 1 and is absent if the mask bit
    is set to 0.  See Section 5.3 for further information on client-
    to-server masking. }
  MaskingKey:Rtc4ByteMask;
*)

  FillChar(Result,SizeOf(Result),0);

  // Header not extracted yet
  Result.PayloadLength:=-1;
  Result.HeaderLength:=0;

  if length(Source)<2 then
    Exit
  else
    begin
    need:=2;
    if Source[1] and 128 = 128 then // masked
      Inc(need,4); // 4 bytes mask

    if (Source[1] and 127)>=126 then // length
      if (Source[1] and 127)=127 then
        Inc(need,8) // 64-bit length
      else
        Inc(need,2); // 16-bit length

    if length(Source)<need then
      Exit
    else if (Source[1] and $7F = 127) and
            (Source[2] and $80 > 0) then
      begin
      // Header invalid
      Result.HeaderLength:=-1;
      Exit;
      end
    else
      begin
      if Source[0] and $80>0 then Result.Final:=TRUE;
      if Source[0] and $40>0 then Result.RSV1:=1;
      if Source[0] and $20>0 then Result.RSV2:=1;
      if Source[0] and $10>0 then Result.RSV3:=1;
      Result.Opcode:=Source[0] and $F;

      if (Source[1] and $7F)<=125 then
        Result.PayloadLength := Source[1] and $7F
      else if (Source[1] and $7F)=127 then
        begin // 64-bit length
        Result.PayloadLength := Source[2];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[3];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[4];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[5];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[6];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[7];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[8];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[9];
        end
      else
        begin // 16-bit length
        Result.PayloadLength := Source[2];
        Result.PayloadLength := Result.PayloadLength shl 8 + Source[3];
        end;

      if Source[1] and $80>0 then
        begin
        Result.Masked:=TRUE;
        Result.MaskingKey[0]:=Source[need-4];
        Result.MaskingKey[1]:=Source[need-3];
        Result.MaskingKey[2]:=Source[need-2];
        Result.MaskingKey[3]:=Source[need-1];
        end;

      Result.HeaderLength:=need;
      end;
    end;
  end;

function RtcWebSockHead(var Head:RtcWSHeader):RtcByteArray; overload;
  var
    need:byte;
    len:int64;
  begin
(*
  // 1 bit => 1.1
  FIN: Rtc1Bit;
  // 1 bit each: => 1.2 - 1.4
  RSV1, RSV2, RSV3: Rtc1Bit;
  // 4 bits:     => 1.5 - 1.8
  Opcode: Rtc4Bit;

  // 1 bit:      => 2.1
  Masked: Rtc1Bit;
  // 0-125 = 7 bits = 2.2 - 2.8
  // 126 = 7+16 bits = 2.2 - 2.8, 3 - 4
  // 127 = 7+64 bits => 2.2 - 2.8, 3 - 6
  PayloadLength: int64;

  { 0 or 4 bytes:
    All frames sent from the client to the server are masked by a
    32-bit value that is contained within the frame.  This field is
    present if the mask bit is set to 1 and is absent if the mask bit
    is set to 0.  See Section 5.3 for further information on client-
    to-server masking. }
  MaskingKey:Rtc4ByteMask;
*)
  need:=2;
  if Head.PayloadLength<0 then
    begin
    Head.HeaderLength:=-1;
    SetLength(Result,0);
    Exit;
    end
  else if Head.PayloadLength>125 then
    if Head.PayloadLength>$FFFF then
      Inc(need,8)
    else
      Inc(need,2);
  if Head.Masked=TRUE then Inc(need,4);

  Head.HeaderLength:=need;
  SetLength(Result,need);
  FillChar(Result[0],need,0);

  Result[0]:=Head.Opcode;
  if Head.Final=TRUE  then Result[0]:=Result[0] or $80;
  if Head.RSV1=1 then Result[0]:=Result[0] or $40;
  if Head.RSV2=1 then Result[0]:=Result[0] or $20;
  if Head.RSV3=1 then Result[0]:=Result[0] or $10;

  if Head.PayloadLength<=125 then
    Result[1]:=Head.PayloadLength
  else if Head.PayloadLength>$FFFF then
    begin
    Result[1]:=127;
    len:=Head.PayloadLength;
    Result[9]:=len and $FF; len:=len shr 8;
    Result[8]:=len and $FF; len:=len shr 8;
    Result[7]:=len and $FF; len:=len shr 8;
    Result[6]:=len and $FF; len:=len shr 8;
    Result[5]:=len and $FF; len:=len shr 8;
    Result[4]:=len and $FF; len:=len shr 8;
    Result[3]:=len and $FF; len:=len shr 8;
    Result[2]:=len and $FF;
    end
  else
    begin
    Result[1]:=126;
    len:=Head.PayloadLength;
    Result[3]:=len and $FF; len:=len shr 8;
    Result[2]:=len and $FF;
    end;

  if Head.Masked=TRUE then
    begin
    Result[1]:=Result[1] or $80;
    Result[need-4] := Head.MaskingKey[0];
    Result[need-3] := Head.MaskingKey[1];
    Result[need-2] := Head.MaskingKey[2];
    Result[need-1] := Head.MaskingKey[3];
    end;
  end;

function RtcWebSockHeadToString(const Head:RtcWSHeader):RtcString; overload;
  begin
  case Head.Opcode of
    wf_Cont:   Result:='CNT';
    wf_Text:   Result:='TXT';
    wf_Binary: Result:='BIN';
    wf_Close:  Result:='CLS';
    wf_Ping:   Result:='PIN';
    wf_Pong:   Result:='PON';
    else       if Head.Opcode<=9 then
                  Result:='#0'+Int2Str(Head.Opcode)
               else
                  Result:='#'+Int2Str(Head.Opcode);
    end;
  if Head.Final=TRUE  then Result:=Result+',FIN';
  if Head.RSV1=1 then Result:=Result+',rsv1';
  if Head.RSV2=1 then Result:=Result+',rsv2';
  if Head.RSV3=1 then Result:=Result+',rsv3';
  if Head.PayloadLength>=0 then
    Result:=Result+',len='+Int2Str(Head.PayloadLength);
  if Head.PayloadInOut>0 then
    Result:=Result+',read='+Int2Str(Head.PayloadInOut);
  if Head.Masked=TRUE then
    Result:=Result+',mask$'+
      Int2Hex(Head.MaskingKey[0],2)+
      Int2Hex(Head.MaskingKey[1],2)+
      Int2Hex(Head.MaskingKey[2],2)+
      Int2Hex(Head.MaskingKey[3],2);
  end;

function RtcWebSockHeadToString(const Source:RtcByteArray):RtcString; overload;
  begin
  Result:=RtcWebSockHeadToString(RtcWebSockHead(Source));
  end;

// Mask "Dest" using "MaskingKey", starting from key position "loc" (only the 1st "len" bytes)
procedure RtcMaskWebSockBytes(var Dest:RtcByteArray; const MaskingKey:Rtc4ByteMask; const loc:int64; len:Integer=-1);
  var
    i,j:Integer;
  begin
  j:=loc mod 4;
  if len<0 then len:=length(Dest);
  if len<=0 then Exit;
  for i:=0 to len-1 do
    begin
    Dest[i]:=Dest[i] xor MaskingKey[j];
    if j>=3 then j:=0
    else Inc(j);
    end;
  end;

function RtcGetMaskedWebSockBytes(const Source:RtcByteArray; const MaskingKey:Rtc4ByteMask; const loc:int64; len:Integer=-1):RtcByteArray;
  var
    i,j:Integer;
  begin
  j:=loc mod 4;
  if len<0 then len:=length(Source);
  SetLength(Result,len);
  if len<=0 then Exit;
  for i:=0 to len-1 do
    begin
    Result[i]:=Source[i] xor MaskingKey[j];
    if j>=3 then j:=0
    else Inc(j);
    end;
  end;

{ TRtcWSFrame }

constructor TRtcWSFrame.Create;
  begin
  inherited;
  FOwner:=nil;
  SetLength(FData,0);
  FillChar(FHead,SizeOf(FHead),0);
  FHead.Final:=True;
  FHead.PayloadLength:=-1;
  FHaveHead:=True;
  FHeadSent:=False;
  FFinalSent:=False;
  FSending:=True;
  FStarted:=FHaveHead;
  FFinished:=False;
  FStartOpcode:=0;
  FTotalLength:=-1;
  UpdateFinished;
  end;

constructor TRtcWSFrame.Create(FromFrame:TRtcWSFrame);
  begin
  inherited Create;
  FOwner:=nil;
  SetLength(FData,0);
  FillChar(FHead,SizeOf(FHead),0);
  FHaveHead:=True;
  FHeadSent:=False;
  FFinalSent:=False;
  FSending:=True;
  FStarted:=FHaveHead;
  if assigned(FromFrame) then
    begin
    FHead.Final:=FromFrame.FHead.Final;
    FHead.RSV1:=FromFrame.FHead.RSV1;
    FHead.RSV2:=FromFrame.FHead.RSV2;
    FHead.RSV3:=FromFrame.FHead.RSV3;
    FHead.Opcode:=FromFrame.FHead.Opcode;
    FHead.Masked:=FromFrame.FHead.Masked;
    FHead.PayloadLength:=FromFrame.FHead.PayloadLength;
    FHead.MaskingKey:=FromFrame.FHead.MaskingKey;
    FTotalLength:=FromFrame.FTotalLength;
    FStartOpcode:=FromFrame.FStartOpcode;
    end
  else
    begin
    FHead.Final:=True;
    FHead.PayloadLength:=-1;
    FStartOpcode:=0;
    FTotalLength:=-1;
    end;
  UpdateFinished;
  end;

constructor TRtcWSFrame.Create(vOpcode:Rtc4Bit; vFinal:Boolean=True; vPayloadLength:int64=-1);
  begin
  inherited Create;
  FOwner:=nil;
  SetLength(FData,0);
  FillChar(FHead,SizeOf(FHead),0);
  FHead.Final:=vFinal;
  FHead.Opcode:=vOpcode;
  FHead.PayloadLength:=vPayloadLength;
  FHaveHead:=True;
  FHeadSent:=False;
  FFinalSent:=False;
  FSending:=True;
  FStarted:=FHaveHead;
  FStartOpcode:=vOpcode;
  FTotalLength:=-1;
  UpdateFinished;
  end;

constructor TRtcWSFrame.Create(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1);
  begin
  inherited Create;
  FOwner:=nil;
  SetLength(FData,0);
  FillChar(FHead,SizeOf(FHead),0);
  FHead.Final:=vFinal;
  FHead.Opcode:=vOpcode;
  FHead.PayloadLength:=vPayloadLength;
  FHaveHead:=True;
  FHeadSent:=False;
  FFinalSent:=False;
  FSending:=True;
  FStarted:=FHaveHead;
  FStartOpcode:=vOpcode;
  FTotalLength:=-1;
  UpdateFinished;
  wfWrite(vPayload);
  end;

constructor TRtcWSFrame.Create(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1);
  begin
  inherited Create;
  FOwner:=nil;
  SetLength(FData,0);
  FillChar(FHead,SizeOf(FHead),0);
  FHead.Final:=vFinal;
  FHead.Opcode:=vOpcode;
  FHead.PayloadLength:=vPayloadLength;
  FHaveHead:=True;
  FHeadSent:=False;
  FFinalSent:=False;
  FSending:=True;
  FStarted:=FHaveHead;
  FStartOpcode:=vOpcode;
  FTotalLength:=-1;
  UpdateFinished;
  wfWriteEx(vPayload);
  end;

constructor TRtcWSFrame.Create(const FromData: RtcByteArray);
  begin
  inherited Create;
  FOwner:=nil;
  FData:=FromData;
  if RtcWebSockCheck(FData)<0 then
    raise ERtcInfo.Create('TRtcWSFrame.Create(FromData): Web Socket Header corrupt');

  FHead:=RtcWebSockHead(FData);
  if FHead.HeaderLength>0 then
    begin
    FHaveHead:=True;
    DelBytes(FData,FHead.HeaderLength);
    FStartOpcode:=FHead.Opcode;
    FFinalSent:=FHead.Final;
    end
  else
    begin
    FStartOpcode:=0;
    FHaveHead:=False;
    end;
  FHeadSent:=True;
  FSending:=False;
  FStarted:=FHaveHead;
  FTotalLength:=-1;
  UpdateFinished;
  end;

constructor TRtcWSFrame.Create(const FromData: RtcString);
  begin
  inherited Create;
  FOwner:=nil;
  FData:=RtcStringToBytes(FromData);
  if RtcWebSockCheck(FData)<0 then
    raise ERtcInfo.Create('TRtcWSFrame.Create(FromData): Web Socket Header corrupt');

  FHead:=RtcWebSockHead(FData);
  if FHead.HeaderLength>0 then
    begin
    FHaveHead:=True;
    DelBytes(FData,FHead.HeaderLength);
    FStartOpcode:=FHead.Opcode;
    FFinalSent:=FHead.Final;
    end
  else
    begin
    FStartOpcode:=0;
    FHaveHead:=False;
    end;
  FHeadSent:=True;
  FSending:=False;
  FStarted:=FHaveHead;
  FTotalLength:=-1;
  UpdateFinished;
  end;

destructor TRtcWSFrame.Destroy;
  begin
  if assigned(FOwner) then
    begin
    FOwner.ws_RemoveFrame(self);
    FOwner:=nil;
    end;
  FName:='';
  SetLength(FData,0);
  inherited;
  end;

procedure TRtcWSFrame.SetOwner(Sender: TRtcConnection; const iName:RtcWideString);
  begin
  if (Sender<>FOwner) or (FName<>iName) then
    begin
    if assigned(FOwner) then
      FOwner.ws_RemoveFrame(self);
    FName:=iName;
    FOwner:=Sender;
    if assigned(FOwner) then
      if FSending then
        FOwner.ws_SendingFrame(self)
      else
        FOwner.ws_OtherFrame(self);
    end;
  end;

procedure TRtcWSFrame.UpdateFinished;
  begin
  if FTotalLength>=0 then
    if FTotalInOut+length(FData)>=FTotalLength then
      begin
      FFinished:=True;
      Exit;
      end;

  if FHaveHead and FHead.Final and (FHead.PayloadLength>=0) then
    if FHead.PayloadInOut+length(FData) >= FHead.PayloadLength then
      FFinished:=True;
  end;

function TRtcWSFrame.CheckMore;
  begin
  if wfDone and
    ( (length(FData)>0) or
      (FFinished and not FHead.Final) ) then
    begin
    FHead.HeaderLength:=0;
    FHead.PayloadInOut:=0;
    FHead.PayloadLength:=-1; // this sets "Done" and "Complete" to FALSE

    if not FFinalSent then // prepare the "continuation" frame
      FHead.Opcode:=wf_Cont;

    if wfSending then
      begin
      if FFinished then
        FHead.Final:=True;
      FHeadSent:=False;
      if FHead.Masked then
        waMasked:=True; // generate new random MaskedKey
      end
    else
      FHaveHead:=False;
    FStarted:=FHaveHead;

    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TRtcWSFrame.NeedHead;
  begin
  FHead:=RtcWebSockHead(FData);
  if FHead.HeaderLength<0 then
    raise ERtcInfo.Create('TRtcWSFrame.NeedHead: Web Socket Header corrupt')
  else if FHead.HeaderLength>0 then
    begin
    if (FStartOpcode=0) or FFinalSent then
      FStartOpcode:=FHead.Opcode;
    FHaveHead:=True;
    FFinalSent:=FHead.Final;
    FStarted:=FHaveHead;
    DelBytes(FData,FHead.HeaderLength);
    if not FFinished then UpdateFinished;
    end;
  end;

procedure TRtcWSFrame.wfWriteEx(const MoreData: RtcByteArray);
  begin
  if length(MoreData)=0 then Exit;
  AddBytes(FData,MoreData);
  if not FFinished then UpdateFinished;
  if CheckMore then
    begin
    if not FHaveHead then NeedHead;
    if not FFinished then UpdateFinished;
    end;
  end;

procedure TRtcWSFrame.wfWrite(const MoreData: RtcString);
  begin
  if length(MoreData)=0 then Exit;
  AddBytes(FData,RtcStringToBytes(MoreData));
  if not FFinished then UpdateFinished;
  if CheckMore then
    begin
    if not FHaveHead then NeedHead;
    if not FFinished then UpdateFinished;
    end;
  end;

function TRtcWSFrame.wfHeadEx: RtcByteArray;
  begin
  if not FHaveHead then
    NeedHead;
  if FHaveHead then
    begin
    if not FHeadSent then
      begin
      if FHead.PayloadLength<0 then
        FHead.PayloadLength:=FHead.PayloadInOut+length(FData);
      FHeadSent:=True;
      FFinalSent:=FHead.Final;
      if FStartOpcode=0 then
        FStartOpcode:=FHead.Opcode;
      if not FFinished then UpdateFinished;
      end;
    Result:=RtcWebSockHead(FHead);
    end
  else
    SetLength(Result,0);
  end;

function TRtcWSFrame.wfHead: RtcString;
  begin
  if not FHaveHead then
    NeedHead;
  if FHaveHead then
    begin
    if not FHeadSent then
      begin
      if FHead.PayloadLength<0 then
        FHead.PayloadLength:=FHead.PayloadInOut+length(FData);
      FHeadSent:=True;
      FFinalSent:=FHead.Final;
      if FStartOpcode=0 then
        FStartOpcode:=FHead.Opcode;
      if not FFinished then UpdateFinished;
      end;
    Result:=RtcBytesToString(RtcWebSockHead(FHead));
    end
  else
    Result:='';
  end;

function TRtcWSFrame.wfHeadInfo: RtcString;
  begin
  if not FHaveHead then
    NeedHead;
  Result:=RtcWebSockHeadToString(FHead);
  end;

function TRtcWSFrame.wfPayloadReady: int64;
  begin
  CheckMore;
  if not FHaveHead then
    begin
    NeedHead;
    if not FHaveHead then
      begin
      Result:=0;
      Exit;
      end;
    end;

  if FHead.PayloadLength>=0 then
    begin
    Result:=FHead.PayloadLength-FHead.PayloadInOut;
    if Result>length(FData) then
      Result:=length(FData);
    end
  else
    Result:=length(FData);
  if not FHeadSent then
    Result:=Result + RtcWebSockHeadLen(FHead);
  end;

function TRtcWSFrame.wfReadEx: RtcByteArray;
  var
    len:int64;
  begin
  SetLength(Result,0);
  CheckMore;
  if not FHaveHead then
    begin
    NeedHead;
    if not FHaveHead then Exit;
    end;

  if FHeadSent then
    FStarted:=False
  else
    Result:=wfHeadEx;

  len:=FHead.PayloadLength-FHead.PayloadInOut;
  if len>length(FData) then
    len:=length(FData);
  if len>0 then
    begin
    if FHead.Masked then
      RtcMaskWebSockBytes(FData, FHead.MaskingKey, FHead.PayloadInOut, len);
    Inc(FHead.PayloadInOut, len);
    Inc(FTotalInOut, len);
    AddBytes(Result,FData,0,len);
    DelBytes(FData,len);
    end;
  end;

function TRtcWSFrame.wfRead: RtcString;
  var
    len:int64;
  begin
  Result:='';
  CheckMore;
  if not FHaveHead then
    begin
    NeedHead;
    if not FHaveHead then Exit;
    end;

  if FHeadSent then
    FStarted:=False
  else
    Result:=wfHead;

  len:=FHead.PayloadLength-FHead.PayloadInOut;
  if len>length(FData) then
    len:=length(FData);
  if len>0 then
    begin
    if FHead.Masked then
      RtcMaskWebSockBytes(FData, FHead.MaskingKey, FHead.PayloadInOut, len);
    Inc(FHead.PayloadInOut, len);
    Inc(FTotalInOut, len);
    Result:=Result+RtcBytesToString(FData,0,len);
    DelBytes(FData,len);
    end;
  end;

function TRtcWSFrame.wfComplete: boolean;
  begin
  if not FHaveHead then
    Result:=False
  else if FHead.PayloadLength<0 then
    Result:=False
  else
    Result:= FHead.PayloadLength<=FHead.PayloadInOut+length(FData);
  end;

function TRtcWSFrame.wfDone: boolean;
  begin
  if not (FHaveHead and FHeadSent) then // Head needs to be sent
    Result:=False
  else if FHead.PayloadLength<0 then
    Result:=False
  else
    Result:= FHead.PayloadLength=FHead.PayloadInOut;
  end;

class function TRtcWSFrame.Check(const FromData: RtcByteArray): int64;
  begin
  Result:=RtcWebSockCheck(FromData);
  end;

class function TRtcWSFrame.Check(const FromData: RtcString): int64;
  begin
  Result:=RtcWebSockCheck(RtcStringToBytes(FromData));
  end;

procedure TRtcWSFrame.CopyFrom(pValue: TRtcValueObject);
  begin
  inherited CopyFrom(pValue);
  FHaveHead:=TRtcWSFrame(pValue).FHaveHead;
  FHeadSent:=TRtcWSFrame(pValue).FHeadSent;
  FFinalSent:=TRtcWSFrame(pValue).FFinalSent;
  FFinished:=TRtcWSFrame(pValue).FFinished;
  FStarted:=TRtcWSFrame(pValue).FStarted;
  FHead:=TRtcWSFrame(pValue).FHead;
  FData:=TRtcWSFrame(pValue).FData;
  FName:=TRtcWSFrame(pValue).FName;
  FSending:=TRtcWSFrame(pValue).FSending;
  FTotalLength:=TRtcWSFrame(pValue).FTotalLength;
  FTotalInOut:=TRtcWSFrame(pValue).FTotalInOut;
  FStartOpcode:=TRtcWSFrame(pValue).FStartOpcode;
  FOwner:=nil;
  end;

function TRtcWSFrame.copyOf: TRtcValueObject;
  begin
  Result:=TRtcWSFrame.Create;
  TRtcWSFrame(Result).CopyFrom(self);
  end;

procedure TRtcWSFrame.SetTotalLength(const Value: int64);
  begin
  FTotalLength:=Value;
  if not FFinished then UpdateFinished;
  end;

function TRtcWSFrame.GetTotalLength:int64;
  begin
  if FHead.PayloadLength>=0 then
    Result:=FTotalInOut-FHead.PayloadInOut+FHead.PayloadLength
  else
    Result:=FTotalInOut+length(FData);
  if Result<FTotalLength then
    Result:=FTotalLength;
  end;

procedure TRtcWSFrame.SetFinished(const Value: Boolean);
  begin
  if Value and not FFinished then
    begin
    FFinished:=True;
    if not FHeadSent then
      FHead.PayloadLength:=-1;
    end;
  end;

procedure TRtcWSFrame.SetMasked(const Value: Boolean);
  begin
  FHead.Masked := Value;
  if Value then
    begin
    FHead.MaskingKey[0]:=Random(256) and $FF;
    FHead.MaskingKey[1]:=Random(256) and $FF;
    FHead.MaskingKey[2]:=Random(256) and $FF;
    FHead.MaskingKey[3]:=Random(256) and $FF;
    end
  else
    begin
    FHead.MaskingKey[0]:=0;
    FHead.MaskingKey[1]:=0;
    FHead.MaskingKey[2]:=0;
    FHead.MaskingKey[3]:=0;
    end;
  end;

{ TRtcWSManager }

constructor TRtcWSManager.Create;
  begin
  inherited;
  FCS:=TRtcCritSec.Create;
  FList:=tObjList.Create(16);
  end;

destructor TRtcWSManager.Destroy;
  begin
  RtcFreeAndNil(FList);
  RtcFreeAndNil(FCS);
  inherited;
  end;

function TRtcWSManager.wsAdd(Sender: TRtcConnection): RtcIntPtr;
  var
    id:RtcIntPtr absolute Sender;
  begin
  Result:=0;
  if (Self=nil) or (Sender=nil) then Exit;
  FCS.Acquire;
  try
    if FList.search(id)<>Sender then
      begin
      FList.insert(id,Sender);
      if Sender.FWS=nil then Sender.FWS:=self;
      end;
    Result:=id;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsRemove(Sender: TRtcConnection): RtcIntPtr;
  var
    id:RtcIntPtr absolute Sender;
  begin
  Result:=0;
  if (Self=nil) or (Sender=nil) then Exit;
  FCS.Acquire;
  try
    if FList.search(id)=Sender then
      begin
      if Sender.FWS=self then Sender.FWS:=nil;
      FList.remove(id);
      Result:=id;
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsGetID(const Sender:TRtcConnection):RtcIntPtr;
  var
    id:RtcIntPtr absolute Sender;
  begin
  Result:=0;
  if (Self=nil) or (Sender=nil) then Exit;
  FCS.Acquire;
  try
    if FList.search(id)=Sender then
      Result:=id;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsCount: integer;
  begin
  if Self=nil then
    begin
    Result:=0;
    Exit;
    end;
  FCS.Acquire;
  try
    Result:=FList.Count;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsFirst: RtcIntPtr;
  var
    obj:TObject;
  begin
  if Self=nil then
    begin
    Result:=0;
    Exit;
    end;
  FCS.Acquire;
  try
    Result:=FList.search_min(obj);
    if obj=nil then
      Result:=0;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsLast: RtcIntPtr;
  var
    obj:TObject;
  begin
  if Self=nil then
    begin
    Result:=0;
    Exit;
    end;
  FCS.Acquire;
  try
    Result:=FList.search_max(obj);
    if obj=nil then
      Result:=0;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsNext(id: RtcIntPtr): RtcIntPtr;
  var
    obj:TObject;
  begin
  if Self=nil then
    begin
    Result:=0;
    Exit;
    end;
  FCS.Acquire;
  try
    Result:=FList.search_g(id, obj);
    if not assigned(obj) then
      Result:=0;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsPrior(id: RtcIntPtr): RtcIntPtr;
  var
    obj:TObject;
  begin
  if Self=nil then
    begin
    Result:=0;
    Exit;
    end;
  FCS.Acquire;
  try
    Result:=FList.search_l(id, obj);
    if not assigned(obj) then
      Result:=0;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wSend(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  var
    Sender:TRtcConnection absolute id;
    tmp:TRtcWSFrame;
  begin
  Result:=0;
  if iFrame=nil then Exit;
  try
    if (Self=nil) or (id=0) then Exit;
    FCS.Acquire;
    try
      if FList.search(id)=Sender then
        begin
        tmp:=iFrame; iFrame:=nil;
        if Sender.wSend(tmp, iName) then
          Inc(Result);
        end;
    finally
      FCS.Release;
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  end;

function TRtcWSManager.wSendMore(id: RtcIntPtr; iFrame: TRtcWSFrame; const vPayload: RtcString; vFinal:boolean=False): integer;
  var
    Sender:TRtcConnection absolute id;
  begin
  Result:=0;
  if (id=0) or (iFrame=nil) or (self=nil) or ((vFinal=False) and (length(vPayload)=0)) then Exit;
  FCS.Acquire;
  try
    if FList.search(id)=Sender then
      if Sender.wSendMore(iFrame, vPayload, vFinal) then
        Inc(Result);
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wSendMore(id: RtcIntPtr; iFrame: TRtcWSFrame; const vPayload: RtcByteArray; vFinal:boolean=False): integer;
  var
    Sender:TRtcConnection absolute id;
  begin
  Result:=0;
  if (id=0) or (iFrame=nil) or (self=nil) or ((vFinal=False) and (length(vPayload)=0)) then Exit;
  FCS.Acquire;
  try
    if FList.search(id)=Sender then
      if Sender.wSendMore(iFrame, vPayload, vFinal) then
        Inc(Result);
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wSendIfIdle(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  var
    Sender:TRtcConnection absolute id;
    tmp:TRtcWSFrame;
  begin
  Result:=0;
  if iFrame=nil then Exit;
  try
    if (Self=nil) or (id=0) then Exit;
    FCS.Acquire;
    try
      if FList.search(id)=Sender then
        begin
        tmp:=iFrame; iFrame:=nil;
        if Sender.wSendIfIdle(tmp, iName) then
          Inc(Result);
        end;
    finally
      FCS.Release;
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  end;

function TRtcWSManager.wSendToAll(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  var
    obj:TObject;
    id:RtcIntPtr;
    Sender:TRtcConnection absolute id;
    tmp:TRtcWSFrame;
  begin
  Result:=0;
  if iFrame=nil then Exit;
  try
    if Self=nil then Exit;
    FCS.Acquire;
    try
      id:=FList.search_min(obj);
      while (id>0) and (obj=Sender) do
        begin
        tmp:=TRtcWSFrame(iFrame.copyOf);
        if Sender.wSend(tmp, iName) then
          Inc(Result);
        id:=FList.search_g(id,obj);
        end;
    finally
      FCS.Release;
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  end;

function TRtcWSManager.wSendToIdle(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  var
    obj:TObject;
    id:RtcIntPtr;
    Sender:TRtcConnection absolute id;
    tmp:TRtcWSFrame;
  begin
  Result:=0;
  if iFrame=nil then Exit;
  try
    if Self=nil then Exit;
    FCS.Acquire;
    try
      id:=FList.search_min(obj);
      while (id>0) and (obj=Sender) do
        begin
        tmp:=TRtcWSFrame(iFrame.copyOf);
        if Sender.wSendIfIdle(tmp, iName) then
          Inc(Result);
        id:=FList.search_g(id,obj);
        end;
    finally
      FCS.Release;
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  end;

function TRtcWSManager.wSendToOthers(xid: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  var
    obj:TObject;
    id:RtcIntPtr;
    Sender:TRtcConnection absolute id;
    tmp:TRtcWSFrame;
  begin
  Result:=0;
  if iFrame=nil then Exit;
  try
    if Self=nil then Exit;
    FCS.Acquire;
    try
      id:=FList.search_min(obj);
      while (id>0) and (obj=Sender) do
        begin
        if id<>xid then
          begin
          tmp:=TRtcWSFrame(iFrame.copyOf);
          if Sender.wSend(tmp, iName) then
            Inc(Result);
          end;
        id:=FList.search_g(id,obj);
        end;
    finally
      FCS.Release;
      end;
  finally
    RtcFreeAndNil(iFrame);
    end;
  end;

function TRtcWSManager.wsDisconnect(id: RtcIntPtr): integer;
  var
    Sender:TRtcConnection absolute id;
  begin
  Result:=0;
  if (Self=nil) or (id=0) then Exit;
  FCS.Acquire;
  try
    if FList.search(id)=Sender then
      if Sender.wsDisconnect then
        Inc(Result);
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsDisconnectAll: integer;
  var
    obj:TObject;
    id:RtcIntPtr;
    Sender:TRtcConnection absolute id;
  begin
  Result:=0;
  if Self=nil then Exit;
  FCS.Acquire;
  try
    id:=FList.search_min(obj);
    while (id>0) and (obj=Sender) do
      begin
      if Sender.wsDisconnect then
        Inc(Result);
      id:=FList.search_g(id,obj);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsClearSendingQueue(id: RtcIntPtr): integer;
  var
    Sender:TRtcConnection absolute id;
  begin
  Result:=0;
  if (Self=nil) or (id=0) then Exit;
  FCS.Acquire;
  try
    if FList.search(id)=Sender then
      if Sender.wsClearSendingQueue then
        Inc(Result);
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wsClearAllSendingQueues: integer;
  var
    obj:TObject;
    id:RtcIntPtr;
    Sender:TRtcConnection absolute id;
  begin
  Result:=0;
  if Self=nil then Exit;
  FCS.Acquire;
  try
    id:=FList.search_min(obj);
    while (id>0) and (obj=Sender) do
      begin
      if Sender.wsClearSendingQueue then
        Inc(Result);
      id:=FList.search_g(id,obj);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcWSManager.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=TRUE; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (Self=nil) then
    Result:=0
  else
    Result:=wSend(id,TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (Self=nil) then
    Result:=0
  else
    Result:=wSend(id,TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (Self=nil) then
    Result:=0
  else
    Result:=wSend(id,TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=TRUE; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (Self=nil) then
    Result:=0
  else
    Result:=wSendIfIdle(id,TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (Self=nil) then
    Result:=0
  else
    Result:=wSendIfIdle(id,TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (Self=nil) then
    Result:=0
  else
    Result:=wSendIfIdle(id,TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToAll(vOpcode: Rtc4Bit; vFinal: boolean=TRUE; vPayloadLength:int64=0): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToAll(TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToAll(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToAll(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToIdle(vOpcode: Rtc4Bit; vFinal: boolean=TRUE; vPayloadLength:int64=0): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToIdle(TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToIdle(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToIdle(TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=TRUE; vPayloadLength:int64=0): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToOthers(xid,TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToOthers(xid,TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

function TRtcWSManager.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=TRUE; vPayloadLength:int64=-1): integer;
  begin
  if Self=nil then
    Result:=0
  else
    Result:=wSendToOthers(xid,TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength));
  end;

{ TRtcServer }

constructor TRtcServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FAmClient:=False;
  FClientConnectTriggered:=False;

  FRestartOn:=TRtcRestartParam.Create;

  FOnClientConnect:=nil;
  FOnClientDisconnect:=nil;

  FOnOpen:=nil;
  FOnClose:=nil;
  end;

destructor TRtcServer.Destroy;
  begin
  try
    FNotSilent:=False;
    FAmSilent:=True;

    ReleaseProvider;

    RtcFreeAndNil(FRestartOn);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcServer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcServer.CopyFrom(Dup: TRtcServer);
  begin
  ServerAddr:=Dup.ServerAddr;
  ServerPort:=Dup.ServerPort;
  MultiThreaded:=Dup.MultiThreaded;

  Timeout.AfterConnecting:=Dup.Timeout.AfterConnecting;
  Timeout.AfterConnect:=Dup.Timeout.AfterConnect;

  Timeout.AfterDataReceived:=Dup.Timeout.AfterDataReceived;
  Timeout.AfterDataLost:=Dup.Timeout.AfterDataLost;

  Timeout.AfterDataSend:=Dup.Timeout.AfterDataSend;
  Timeout.AfterDataOut:=Dup.Timeout.AfterDataOut;
  Timeout.AfterDataIn:=Dup.Timeout.AfterDataIn;
  Timeout.AfterDataSent:=Dup.Timeout.AfterDataSent;

  OnConnecting:=Dup.OnConnecting;
  OnDisconnecting:=Dup.OnDisconnecting;
  OnConnect:=Dup.OnConnect;
  OnDisconnect:=Dup.OnDisconnect;
  OnClientConnect:=Dup.OnClientConnect;
  OnClientDisconnect:=Dup.OnClientDisconnect;

  OnListenStart:=Dup.OnListenStart;
  OnListenStop:=Dup.OnListenStop;

  OnException:=Dup.OnException;

  OnDataOut:=Dup.OnDataOut;
  OnDataIn:=Dup.OnDataIn;
  OnDataSent:=Dup.OnDataSent;
  OnDataReceived:=Dup.OnDataReceived;
  OnReadyToSend:=Dup.OnReadyToSend;
  end;

function TRtcServer.ServerConnectionsClosed: boolean;
  begin
  if self=nil then
    Result:=True
  else
    begin
    Result:=False;
    if ConnectionClosed then
      if not assigned(Con) then
        Result:=True
      else if isClient then
        Result:=True
      else
        Result:=TRtcServerProvider(Con).ClientCount=0;
    end;
  end;

procedure TRtcServer.CallConnectionAccepting;
  begin
  ServerConnection_CanAccept;
  end;

procedure TRtcServer.TriggerConnectionAccepting;
  begin
  CallConnectionAccepting;
  end;

procedure TRtcServer.TriggerConnectionAccepted;
  begin
  if not FActive then
    begin
    ServerConnection_Accept;
    FActive:=True;

    Timeout.Start(MultiThreaded);
    Timeout.Connecting;

    TriggerClientConnect;
    end;
  end;

procedure TRtcServer.TriggerConnectionLost;
  begin
  if FActive then
    begin
    Timeout.Stop;
    ServerConnection_Close;
    FActive:=False;

    TriggerClientDisconnect;
    end;
  end;

type
  TRtcRestartJob=class(TRtcJob)
  public
    Conn:TRtcServer;
    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcRestartJob.Run(Thr:TRtcThread):boolean;
  begin
  if assigned(Conn) then
    Conn.OnRestart_Timer;
  Result:=True;
  end;

procedure TRtcServer.Restart(WaitSeconds: integer=0);
  begin
  if InsideEvent then
    begin
    FReleasing:=False;
    FListening:=True;
    end
  else if not FReconnecting then
    begin
    FReconnecting:=True;
    if WaitSeconds<=0 then
      OnRestart_Timer
    else
      begin
      FRecTimer:=TRtcTimer.Create(MultiThreaded);
      TRtcTimer.Enable(FRecTimer,WaitSeconds*1000, OnRestart_Timer, True, True);
      end;
    end;
  end;

procedure TRtcServer.OnRestart_Timer;
  var
    job:TRtcRestartJob;
  begin
  if not inThread and assigned(con) and (Thread<>nil) then
    begin
    job:=TRtcRestartJob.Create;
    job.Conn:=self;
    if PostJob(job) then
      Exit
    else if job.SingleUse then
      RtcFreeAndNil(job);
    end;

  if FReconnecting then
    begin
    FReconnecting:=False;
    FRecTimer:=nil;
    try
      TriggerRestart;
    except
      on E:Exception do
        begin
        try
          TriggerException(E);
        except
          //
          end;
        end;
      end;
    end;
  end;

procedure TRtcServer.StopListen;
  begin
  if isClient then
    Parent.Disconnect
  else if InsideEvent then
    begin
    FReleasing:=False;
    FClosing:=True;
    FDisconnecting:=True;
    FStopping:=True;
    end
  else if assigned(Con) then
    Con.Disconnect;
  end;

procedure TRtcServer.Listen(Restarting:boolean=False);
  begin
  if InsideEvent then
    begin
    FReleasing:=False;
    FListening:=True;
    end
  else if isClient then
    TRtcServer(Parent).Listen(Restarting)
  else
    begin
    if not assigned(Con) then
      CreateProvider;
    SetParams;
    TRtcServerProvider(Con).Listen(Restarting);
    end;
  end;

function TRtcServer.isServer:boolean;
  begin
  Result:=True;
  end;

function TRtcServer.isClient:boolean;
  begin
  Result:=FAmClient or assigned(Parent);
  end;

function TRtcServer.isListening: boolean;
  begin
  if assigned(Con) then
    Result:=not isClient and
            not (FClosing or FDisconnecting) and
            ( (State=conListening) or
              (State=conActive) )
  else
    Result:=False;
  end;

procedure TRtcServer.TriggerClientConnect;
  begin
  if not FClientConnectTriggered then
    begin
    FClientConnectTriggered:=True;

    EnterEvent;
    try
      CallClientConnect;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcServer.TriggerClientDisconnect;
  begin
  if FClientConnectTriggered then
    begin
    FClientConnectTriggered:=False;

    EnterEvent;
    try
      CallClientDisconnect;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcServer.TriggerListenError(E: Exception);
  begin
  EnterEvent;
  try
    FClosing:=True;
    try
      CallListenError(E);
    finally
      FClosing:=False;
      end;
    if RestartOn.ListenError then
      begin
      if not (FListening or FReleasing or FReconnecting) then
        FListening:=True;
      end
    else if not assigned(OnListenError) then
      raise ERtcConnection.Create(E.Message);
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcServer.TriggerListenStart;
  begin
  EnterEvent;
  try
    CallListenStart;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcServer.TriggerListenStop;
  begin
  EnterEvent;
  try
    FClosing:=True;
    CallListenStop;
  finally
    FClosing:=False;
    LeaveEvent;
    end;
  end;

procedure TRtcServer.TriggerListenLost;
  begin
  EnterEvent;
  try
    FClosing:=True;
    CallListenLost;
  finally
    FClosing:=False;
    LeaveEvent;
    end;

  if RestartOn.ListenLost and
     not FReleasing then // not reconnecting if Release called on error.
    FListening:=True;
  end;

procedure TRtcServer.TriggerRestart;
  begin
  if State>conPrepared then
    StopListen;

  if State>conPrepared then
    begin
    if RestartOn.Wait>0 then
      Restart(RestartOn.Wait)
    else
      Restart(1);
    Exit;
    end;

  EnterEvent;
  try
    CallRestart;
  finally
    LeaveEvent;
    end;

  if not FReleasing and // Do not reconnect if released.
     not FListening then
    Listen(True);
  end;

procedure TRtcServer.TriggerReadyToRelease;
  var
    Par:TRtcServer;
  begin
  if InsideEvent then
    FReadyToRelease:=True
  else
    begin
    FReadyToRelease:=False;
    if FReleasing then
      begin
      Timeout.Stop;

      FClosing:=False;
      FReleasing:=False;
      FConnecting:=False;
      FListening:=False;
      if FStopping then
        begin
        FStopping:=False;
        if isClient then
          StopListen
        else
          Release;
        end
      else
        begin
        if FDisconnecting then
          begin
          FDisconnecting:=False;
          Disconnect;
          end;
        Release;
        end;
      end
    else if FListening then
      begin
      Timeout.Disable;

      FListening:=False;
      if FStopping or FDisconnecting then
        begin
        FDisconnecting:=False;
        FStopping:=False;
        if isClient then
          begin
          Par:=TRtcServer(Parent);
          Par.StopListen;
          Par.Restart(RestartOn.Wait);
          end
        else
          begin
          StopListen;
          Restart(RestartOn.Wait);
          end;
        end
      else
        begin
        if isClient then
          TRtcServer(Parent).Restart(RestartOn.Wait)
        else
          Restart(RestartOn.Wait);
        end;
      end
    else if FDisconnecting then
      begin
      Timeout.Disable;

      FClosing:=False;
      FDisconnecting:=False;

      if FStopping then
        begin
        FStopping:=False;
        StopListen;
        end
      else
        Disconnect;
      end;
    end;
  end;

procedure TRtcServer.TriggerNewProvider(var Provider: TObject);
  var
    cla:TRtcBaseServerClass;
    cl:TRtcServer;
  begin
  cla:=TRtcBaseServerClass(self.ClassType);
  cl:=cla.Create(nil);
  try
    cl.FParent:=self;
    Provider := cl.CreateProvider; // Con:=TRtcServer(Provider);
    cl.CopyFrom(self);
    cl.SetParams;
  except
    Provider:=nil;
    try
      RtcFreeAndNil(cl);
    except
      end;
    raise;
    end;
  end;

procedure TRtcServer.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcServerProvider(Con) do
      begin
      SetTriggerConnectionAccepting(self.TriggerConnectionAccepting);
      SetTriggerConnectionAccepted(self.TriggerConnectionAccepted);
      SetTriggerConnectionLost(self.TriggerConnectionLost);

      SetTriggerNewProvider(self.TriggerNewProvider);

      SetTriggerListenStart(self.TriggerListenStart);
      SetTriggerListenStop(self.TriggerListenStop);

      SetTriggerListenError(self.TriggerListenError);
      SetTriggerListenLost(self.TriggerListenLost);
      end;
  end;

procedure TRtcServer.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcServerProvider(Con) do
      begin
      SetTriggerConnectionAccepting(nil);
      SetTriggerConnectionAccepted(nil);
      SetTriggerConnectionLost(nil);

      SetTriggerNewProvider(nil);

      SetTriggerListenStart(nil);
      SetTriggerListenStop(nil);

      SetTriggerListenLost(nil);
      SetTriggerListenError(nil);
      end;
  end;

procedure TRtcServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    with TRtcServerProvider(Con) do
      begin
      //
      end;
  end;

function TRtcServer.AutoRelease: boolean;
  begin
  Result:=isClient;
  end;

procedure TRtcServer.CallClientConnect;
  begin
  if assigned(OnClientConnect) and FNotSilent then
    OnClientConnect(self);
  end;

procedure TRtcServer.CallClientDisconnect;
  begin
  if assigned(OnClientDisconnect) and FNotSilent then
    OnClientDisconnect(self);
  end;

procedure TRtcServer.CallListenError(E: Exception);
  begin
  if assigned(OnListenError) and FNotSilent then
    OnListenError(self,E);
  end;

procedure TRtcServer.CallListenLost;
  begin
  if assigned(OnListenLost) and FNotSilent then
    OnListenLost(self);
  end;

procedure TRtcServer.CallListenStart;
  begin
  if assigned(OnListenStart) and FNotSilent then
    OnListenStart(self);
  end;

procedure TRtcServer.CallListenStop;
  begin
  if assigned(OnListenStop) and FNotSilent then
    OnListenStop(self);
  end;

procedure TRtcServer.CallRestart;
  begin
  if assigned(OnRestart) and FNotSilent then
    OnRestart(self);
  end;

procedure TRtcServer.SetMultiThread(const Value: boolean);
  begin
  if Value<>FMultiThread then
    begin
    if not isClient and assigned(Con) then
      if isListening then
        Error('Can not change MultiThreaded after Listen.')
      else
        ReleaseProvider;
    FMultiThread := Value;
    end;
  end;

function TRtcServer.copyOf: TRtcServer;
  var
    cla:TRtcBaseServerClass;
  begin
  cla:=TRtcBaseServerClass(self.ClassType);
  Result:=cla.Create(nil);
  try
    Result.FParent:=self;
    Result.CreateProvider; // Con:=TRtcServer(Provider);
    Result.CopyFrom(self);
    Result.SetParams;
  except
    try
      RtcFreeAndNil(Result);
    except
      end;
    raise;
    end;
  end;

function TRtcServer.GetRestartOn:TRtcRestartParam;
  begin
  Result:=FRestartOn;
  end;

procedure TRtcServer.SetRestartOn(const Value: TRtcRestartParam);
  begin
  if Value<>FRestartOn then
    FRestartOn.Assign(Value);
  end;

{ TRtcClient }

constructor TRtcClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FConLevel:=0;
  FConCnt:=0;

  FReconnectOn:=TRtcReconnectParam.Create;
  end;

destructor TRtcClient.Destroy;
  begin
  try
    FNotSilent:=False;
    FAmSilent:=True;

    ReleaseProvider;

    RtcFreeAndNil(FReconnectOn);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClient.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcClient.TriggerConnectionOpening(Force: boolean);
  begin
  if not FActive then
    begin
    if not ClientConnection_Open(Force) then
      begin
      OverLimit:=True;
      end;
    FActive:=True;

    Timeout.Start(MultiThreaded);
    Timeout.Connecting;
    end;
  end;

procedure TRtcClient.TriggerConnectionClosing;
  begin
  if FActive then
    begin
    Timeout.Stop;
    ClientConnection_Close;
    FActive:=False;
    end;
  end;

type
  TRtcReconJob=class(TRtcJob)
  public
    Conn:TRtcClient;
    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcReconJob.Run(Thr:TRtcThread):boolean;
  begin
  if assigned(Conn) then
    Conn.OnReconnect_Timer;
  Result:=True;
  end;

procedure TRtcClient.Reconnect(WaitSeconds: integer=0);
  begin
  FWantConnect:=True;

  if InsideEvent then // called from inside event
    begin
    FReleasing:=False;
    FConnecting:=True;
    end
  else if not FReconnecting then
    begin
    FReconnecting:=True;
    if WaitSeconds<=0 then
      OnReconnect_Timer
    else
      begin
      FRecTimer:=TRtcTimer.Create(MultiThreaded);
      TRtcTimer.Enable(FRecTimer, WaitSeconds*1000, OnReconnect_Timer, True, True);
      end;
    end;
  end;

procedure TRtcClient.OnReconnect_Timer;
  var
    job:TRtcReconJob;
  begin
  if not inThread and assigned(Con) and (Thread<>nil) then
    begin
    job:=TRtcReconJob.Create;
    job.Conn:=self;
    if PostJob(job) then
      Exit
    else if job.SingleUse then
      RtcFreeAndNil(job);
    end;

  if FReconnecting then
    begin
    FReconnecting:=False;
    FRecTimer:=nil;
    try
      TriggerReconnect;
    except
      on E:Exception do
        try
          TriggerException(E);
        except
          //
        end;
      end;
    end;
  end;

procedure TRtcClient.Connect(Force: boolean=False; Reconnecting:boolean=False);
  begin
  FWantConnect:=True;
  FTimedOut:=False;

  Inc(FConLevel);
  try
    Inc(FConCnt);
    if FConLevel=1 then
      begin
      while FConCnt>0 do
        begin
        Dec(FConCnt);
        if assigned(Con) and MultiThreaded then
          begin
          SetParams;
          TRtcClientProvider(Con).Connect(Force,Reconnecting);
          end
        else if InsideEvent then
          begin
          FReleasing:=False;
          FConnecting:=True;
          end
        else if assigned(Con) then
          begin
          SetParams;
          TRtcClientProvider(Con).Connect(Force,Reconnecting);
          end
        else
          begin
          CreateProvider;
          SetParams;
          TRtcClientProvider(Con).Connect(Force,Reconnecting);
          end;
        end;
      end;
  finally
    Dec(FConLevel);
    end;
  end;

function TRtcClient.isConnected: boolean;
  begin
  if not (FClosing or FDisconnecting) then
    begin
    if Con=nil then
      Result:=False
    else
      Result:=Con.GetState=conActive;
    end
  else
    Result:=False;
  end;

function TRtcClient.isConnecting: boolean;
  begin
  Result:=FWantConnect or FConnecting or FReconnecting;
  end;

procedure TRtcClient.TriggerConnectError(E: Exception);
  begin
  Timeout.Disable;

  inherited TriggerDisconnect;

  EnterEvent;
  try
    FClosing:=True;
    try
      CallConnectError(E);
    finally
      FClosing:=False;
      end;
    if FWantConnect and ReconnectOn.ConnectError and isConnectionRequired then
      begin
      if not (FDisconnecting or FReleasing or FReconnecting or (E is EClientLimitReached)) then
        FConnecting:=True
      else if not (FConnecting or FReconnecting) then
        FWantConnect:=False;
      end
    else if not (FConnecting or FReconnecting) then
      FWantConnect:=False;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcClient.TriggerConnectFail;
  begin
  Timeout.Disable;

  inherited TriggerDisconnect;

  EnterEvent;
  try
    FClosing:=True;
    CallConnectFail;
  finally
    FClosing:=False;
    LeaveEvent;
    end;

  if FWantConnect and ReconnectOn.ConnectFail and
     not (FDisconnecting or FReleasing or FReconnecting) and
     isConnectionRequired then
    FConnecting:=True
  else if not (FConnecting or FReconnecting) then
    FWantConnect:=False;
  end;

procedure TRtcClient.TriggerConnectLost;
  begin
  Timeout.Disable;

  inherited TriggerDisconnect;

  EnterEvent;
  try
    FClosing:=True;

    CallConnectLost;

    if FWantConnect and ReconnectOn.ConnectLost and
       not (FDisconnecting or FReleasing or FReconnecting) and
       isConnectionRequired then
      FConnecting:=True
    else if not (FConnecting or FReconnecting) then
      FWantConnect:=False;
  finally
    FClosing:=False;
    LeaveEvent;
    end;
  end;

procedure TRtcClient.TriggerDisconnect;
  begin
  FWantConnect:=False;
  inherited TriggerDisconnect;

  if isWebSocket then
    begin
    EnterEvent;
    try
      FClosing:=True;
      CallConnectLost;
    finally
      FClosing:=False;
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcClient.TriggerReconnect;
  begin
  if State>conPrepared then
    Disconnect;

  if State>conPrepared then
    begin
    if ReconnectOn.Wait>0 then
      Reconnect(ReconnectOn.Wait)
    else
      Reconnect(1);
    Exit;
    end;

  Timeout.Disable;

  EnterEvent;
  try
    CallReconnect;
  finally
    LeaveEvent;
    end;

  if not FReleasing and // Do not reconnect if released.
     not FConnecting and
     not FListening and
     not FStopping and
     not FDisconnecting then
    Connect(False,True);
  end;

procedure TRtcClient.TriggerReadyToRelease;
  begin
  if InsideEvent then
    FReadyToRelease:=True
  else
    begin
    FReadyToRelease:=False;
    if FReleasing then
      begin
      Timeout.Stop;

      FClosing:=False;
      FReleasing:=False;
      FConnecting:=False;
      FListening:=False;
      if FDisconnecting then
        begin
        FDisconnecting:=False;
        Disconnect;
        end;
      Release;
      end
    else if FConnecting then
      begin
      Timeout.Disable;

      FClosing:=False;
      FConnecting:=False;
      if FDisconnecting then
        begin
        FClosing:=False;
        FDisconnecting:=False;
        Disconnect;
        end;
      Reconnect(ReconnectOn.Wait);
      end
    else if FDisconnecting then
      begin
      Timeout.Disable;

      FClosing:=False;
      FDisconnecting:=False;

      Disconnect;
      end;
    end;
  end;

procedure TRtcClient.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcClientProvider(Con) do
      begin
      SetTriggerConnectionOpening(self.TriggerConnectionOpening);
      SetTriggerConnectionClosing(self.TriggerConnectionClosing);

      SetTriggerConnectFail(self.TriggerConnectFail);
      SetTriggerConnectLost(self.TriggerConnectLost);
      SetTriggerConnectError(self.TriggerConnectError);
      end;
  end;

procedure TRtcClient.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcClientProvider(Con) do
      begin
      SetTriggerConnectionOpening(nil);
      SetTriggerConnectionClosing(nil);

      SetTriggerConnectFail(nil);
      SetTriggerConnectLost(nil);
      SetTriggerConnectError(nil);
      end;
  end;

procedure TRtcClient.SetParams;
  begin
  inherited;
  if assigned(Con) then
    with TRtcClientProvider(Con) do
      begin
      //
      end;
  end;

procedure TRtcClient.CallConnectError(E: Exception);
  begin
  if assigned(OnConnectError) and FNotSilent then
    OnConnectError(self,E);
  end;

procedure TRtcClient.CallConnectFail;
  begin
  if assigned(OnConnectFail) and FNotSilent then
    OnConnectFail(self);
  end;

procedure TRtcClient.CallConnectLost;
  begin
  if assigned(OnConnectLost) and FNotSilent then
    OnConnectLost(self);
  end;

procedure TRtcClient.CallReconnect;
  begin
  if assigned(OnReconnect) and FNotSilent then
    OnReconnect(self);
  end;

procedure TRtcClient.SetMultiThread(const Value: boolean);
  begin
  if Value<>FMultiThread then
    begin
    if assigned(Con) then
      if isConnecting then
        Error('Can not change MultiThreaded after Connect.')
      else
        ReleaseProvider;
    FMultiThread := Value;
    end;
  end;

function TRtcClient.GetReconnectOn:TRtcReconnectParam;
  begin
  Result:=FReconnectOn;
  end;

procedure TRtcClient.SetReconnectOn(const Value: TRtcReconnectParam);
  begin
  if Value<>FReconnectOn then
    FReconnectOn.Assign(Value);
  end;

function TRtcClient.isConnectionRequired: boolean;
  begin
  Result:=True;
  end;

{ TRtcReconnectParam }

procedure TRtcReconnectParam.AssignTo(Dest: TPersistent);
  begin
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcReconnectParam) then
    begin
    TRtcReconnectParam(Dest).ConnectError:=ConnectError;
    TRtcReconnectParam(Dest).ConnectLost:=ConnectLost;
    TRtcReconnectParam(Dest).ConnectFail:=ConnectFail;
    TRtcReconnectParam(Dest).Wait:=Wait;
    end;
  end;

constructor TRtcReconnectParam.Create;
  begin
  inherited;
  FConnectError:=False;
  FConnectLost:=False;
  FConnectFail:=False;
  FWait:=0;
  end;

destructor TRtcReconnectParam.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcReconnectParam.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcRestartParam }

procedure TRtcRestartParam.AssignTo(Dest: TPersistent);
  begin
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcRestartParam) then
    begin
    TRtcRestartParam(Dest).ListenLost:=ListenLost;
    TRtcRestartParam(Dest).ListenError:=ListenError;
    TRtcRestartParam(Dest).Wait:=Wait;
    end;
  end;

constructor TRtcRestartParam.Create;
  begin
  inherited;
  FListenLost:=False;
  FListenError:=False;
  FWait:=0;
  end;

destructor TRtcRestartParam.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcRestartParam.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtTimeoutDisconnect }

type
  TRtcTimeoutDisconnect=class(TRtcJob)
  public
    Conn:TRtcConnection;
    constructor Create(Con:TRtcConnection);

    function Run(Thr:TRtcThread):boolean; override;
    function SingleUse:boolean; override;
    end;

constructor TRtcTimeoutDisconnect.Create(Con: TRtcConnection);
  begin
  inherited Create;
  Conn:=Con;
  end;

function TRtcTimeoutDisconnect.SingleUse:boolean;
  begin
  // do not destroy
  Result:=False;
  end;

function TRtcTimeoutDisconnect.Run(Thr:TRtcThread):boolean;
  begin
  Result:=False;
  try
    if assigned(Conn) then
      begin
      if LOG_TIMEOUT_DISCONNECTS then
        Log('ABORT with Timeout. Local '+Conn.LocalAddr+':'+Conn.LocalPort+', Peer '+Conn.PeerAddr+':'+Conn.PeerPort,'TIMEOUT');

      Conn.FTimedOut:=True;
      Conn.InternalDisconnect;
      end;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('TRtcTimeoutDisconnect.Run',E,'ERROR');
    end;
  end;

{ TRtcTimeout }

procedure TRtcTimeout.TriggerTimeout;
  begin
  try
    if assigned(Conn) then
      if not Conn.FAmSilent then
        begin
        if LOG_TIMEOUT_DISCONNECTS then
          Log('ABORT with Timeout. Local '+Conn.LocalAddr+':'+Conn.LocalPort+', Peer '+Conn.PeerAddr+':'+Conn.PeerPort,'TIMEOUT');

        Conn.FTimedOut:=True;
        Conn.InternalDisconnect;
        end;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('TRtcTimeout.TriggerTimeout',E,'ERROR');
    end;
  end;

constructor TRtcTimeout.Create(Con:TRtcConnection);
  begin
  inherited Create;

  FAfterConnecting:=0;
  FAfterConnect:=0;
  FAfterDataReceived:=0;
  FAfterDataLost:=0;
  FAfterDataSend:=0;
  FAfterDataOut:=0;
  FAfterDataIn:=0;
  FAfterDataSent:=0;

  FInterval:=-1;

  FConn:=Con;
  FThr:=nil;
  FJob:=nil;
  FTimer:=nil;
  end;

destructor TRtcTimeout.Destroy;
  begin
  try
    if assigned(FTimer) then
      begin
      TRtcTimer.Stop(FTimer);
      FTimer:=nil;
      end;
    RtcFreeAndNil(FJob);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcTimeout.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcTimeout.Start(Multi_Threaded:boolean);
  begin
  if not assigned(FTimer) then
    FTimer:=TRtcTimer.Create(Multi_Threaded);
  FInterval:=-1;
  if Multi_Threaded then
    begin
    if assigned(FConn) and assigned(FConn.Con) then
      begin
      FThr:=FConn.Thread;
      if not assigned(FJob) then
        FJob:=TRtcTimeoutDisconnect.Create(FConn);
      end
    else
      FThr:=nil;
    end
  else
    FThr:=nil;
  end;

procedure TRtcTimeout.Stop;
  begin
  if assigned(FTimer) then
    begin
    TRtcTimer.Stop(FTimer);
    FTimer:=nil;
    end;
  FInterval:=-1;
  FThr:=nil;
  end;

procedure TRtcTimeout.TimerSet;
  begin
  if assigned(FThr) then
    TRtcTimer.Enable(FTimer,FInterval*1000, FJob, FThr)
  else
    TRtcTimer.Enable(FTimer,FInterval*1000, TriggerTimeout);
  end;

procedure TRtcTimeout.TimerReset;
  begin
  TRtcTimer.Reset(FTimer);
  end;

procedure TRtcTimeout.Connecting;
  begin
  if assigned(FTimer) then
    if FAfterConnecting<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterConnecting>0 then
      begin
      if FInterval<>FAfterConnecting then
        begin
        FInterval:=FAfterConnecting;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.Connect;
  begin
  if assigned(FTimer) then
    if FAfterConnect<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterConnect>0 then
      begin
      if FInterval<>FAfterConnect then
        begin
        FInterval:=FAfterConnect;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataReceived;
  begin
  if assigned(FTimer) then
    if FAfterDataReceived<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataReceived>0 then
      begin
      if FInterval<>FAfterDataReceived then
        begin
        FInterval:=FAfterDataReceived;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataLost;
  begin
  if assigned(FTimer) then
    if FAfterDataLost<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataLost>0 then
      begin
      if FInterval<>FAfterDataLost then
        begin
        FInterval:=FAfterDataLost;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataSending;
  begin
  if assigned(FTimer) then
    if FAfterDataSend<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataSend>0 then
      begin
      if FInterval<>FAfterDataSend then
        begin
        FInterval:=FAfterDataSend;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataOut;
  begin
  if assigned(FTimer) then
    if FAfterDataOut<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataOut>0 then
      begin
      if FInterval<>FAfterDataOut then
        begin
        FInterval:=FAfterDataOut;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataIn;
  begin
  if assigned(FTimer) then
    if FAfterDataIn<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataIn>0 then
      begin
      if FInterval<>FAfterDataIn then
        begin
        FInterval:=FAfterDataIn;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataSent;
  begin
  if assigned(FTimer) then
    if FAfterDataSent<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataSent>0 then
      begin
      if FInterval<>FAfterDataSent then
        begin
        FInterval:=FAfterDataSent;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.Enable(timeout:integer);
  begin
  if assigned(FTimer) then
    if timeout<0 then
      TRtcTimer.Disable(FTimer)
    else if timeout>0 then
      begin
      if FInterval<>timeout then
        begin
        FInterval:=timeout;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.Disable;
  begin
  if assigned(FTimer) then
    TRtcTimer.Disable(FTimer);
  FInterval:=-1;
  end;

procedure TRtcTimeout.AssignTo(Dest: TPersistent);
  begin
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcTimeout) then
    begin
    TRtcTimeout(Dest).AfterConnecting:=AfterConnecting;
    TRtcTimeout(Dest).AfterConnect:=AfterConnect;
    TRtcTimeout(Dest).AfterDataReceived:=AfterDataReceived;
    TRtcTimeout(Dest).AfterDataLost:=AfterDataLost;
    TRtcTimeout(Dest).AfterDataSend:=AfterDataSend;
    TRtcTimeout(Dest).AfterDataOut:=AfterDataOut;
    TRtcTimeout(Dest).AfterDataIn:=AfterDataIn;
    TRtcTimeout(Dest).AfterDataSent:=AfterDataSent;
    end;
  end;

{ TRtcClientRequest }

procedure TRtcClientRequest.Clear;
  begin
  inherited;
  Init;
  Reposted:=0;
  end;

procedure TRtcClientRequest.Init;
  begin
  Started:=False;
  Active:=False;
  Complete:=False;

  Skipped:=False;
  Cancelled:=False;
  Reposting:=False;
  if AutoLength then
    begin
    AutoLength:=False;
    SetHeader('Content-Length','');
    end;

  DataOut:=0;
  end;

procedure TRtcClientRequest.Skip;
  begin
  Skipped:=True;
  Cancelled:=False;
  end;

procedure TRtcClientRequest.Cancel;
  begin
  Cancelled:=True;
  end;

procedure TRtcClientRequest.Repost;
  begin
  if not Reposting then
    begin
    Init;

    Reposting:=True;
    Reposted:=Reposted+1;
    end;
  end;

constructor TRtcClientRequest.Create;
  begin
  inherited;
  end;

destructor TRtcClientRequest.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientRequest.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcClientResponse }

procedure TRtcClientResponse.Clear;
  begin
  inherited;
  StatusCode:=0;
  StatusText:='';
  Started:=False;
  Receiving:=False;
  Rejected:=False;
  Done:=False;
  ManualRead:=False;

  DataIn:=0;
  end;

constructor TRtcClientResponse.Create;
  begin
  inherited;
  end;

destructor TRtcClientResponse.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientResponse.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcClientResponse.Reject;
  begin
  Rejected:=True;
  end;

{ TRtcServerRequest }

procedure TRtcServerRequest.Clear;
  begin
  inherited;
  Started:=False;
  Complete:=False;

  Accepted:=False;
  ManualRead:=False;

  DataIn:=0;
  end;

procedure TRtcServerRequest.Init;
  begin
  raise ERtcConnection.Create('"Request.Init" is a Client-side method!');
  end;

procedure TRtcServerRequest.Skip;
  begin
  raise ERtcConnection.Create('"Request.Skip" is a Client-side method!');
  end;

procedure TRtcServerRequest.Cancel;
  begin
  raise ERtcConnection.Create('"Request.Cancel" is a Client-side method!');
  end;

procedure TRtcServerRequest.Repost;
  begin
  raise ERtcConnection.Create('"Request.Repost" is a Client-side method!');
  end;

constructor TRtcServerRequest.Create;
  begin
  inherited;
  end;

destructor TRtcServerRequest.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcServerRequest.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcServerResponse }

constructor TRtcServerResponse.Create;
  begin
  inherited;
  StatusCode:=200;
  StatusText:='OK';
  end;

destructor TRtcServerResponse.Destroy;
  begin
  try
    StatusText:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcServerResponse.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcServerResponse.Clear;
  begin
  inherited;
  StatusCode:=200;
  StatusText:='OK';

  Started:=False;
  Sending:=False;
  Done:=False;
  Sent:=False;

  SendContent:=True;

  DataOut:=0;

  WSUpgrade:=False;
  end;

procedure TRtcServerResponse.Reject;
  begin
  raise ERtcConnection.Create('"Response.Reject" is a Client-side method!');
  end;

{ TRtcServerRequestFixup }

procedure TRtcServerRequestFixup.AssignTo(Dest: TPersistent);
  begin
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcServerRequestFixup) then
    begin
    TRtcServerRequestFixup(Dest).DecodeQuery:=DecodeQuery;
    TRtcServerRequestFixup(Dest).DecodeFileName:=DecodeFileName;
    TRtcServerRequestFixup(Dest).UpperCaseFileName:=UpperCaseFileName;
    TRtcServerRequestFixup(Dest).RemovePrefix:=RemovePrefix;
    end;
  end;

constructor TRtcServerRequestFixup.Create;
  begin
  inherited;
  FDecodeQuery:=False;
  FDecodeFileName:=False;
  FUpperCaseFileName:=False;
  FRemovePrefix:=False;
  end;

destructor TRtcServerRequestFixup.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcServerRequestFixup.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcServerRequestFixup.Fixup(Request: TRtcRequest);
  var
    s:RtcString;
    myPos:integer;
  begin
  if RemovePrefix and (length(Request.FileName)>8) then
    begin
    s:=Upper_Case(Copy(Request.FileName,1,8));
    if s='HTTPS://' then
      myPos:=8
    else if Copy(s,1,7)='HTTP://' then
      myPos:=7
    else
      myPos:=0;

    if myPos>0 then
      begin
      s:=Copy(Request.FileName,myPos+1,length(Request.FileName)-myPos);
      myPos:=PosEx('/',s);
      if myPos>0 then
        s:=Copy(s,myPos,length(s)-myPos+1)
      else
        s:='';
      Request.FileName:=s;
      end;
    end;
  if DecodeFileName and (length(Request.FileName)>0) then
    Request.FileName:=URL_Decode(Request.FileName);
  if DecodeQuery and (length(Request.Query.Text)>0) then
    begin
    if Request.Query.ItemCount=0 then
      Request.Query.Text:=URL_Decode(Request.Query.Text)
    else
      begin
      for myPos:=0 to Request.Query.ItemCount-1 do
        begin
        Request.Query.ItemName[myPos]:=URL_Decode(Request.Query.ItemName[myPos]);
        Request.Query.ItemValue[myPos]:=URL_Decode(Request.Query.ItemValue[myPos]);
        end;
      end;;
    end;
  if UpperCaseFileName then
    Request.FileName:=Upper_Case(Request.FileName);
  end;

{ TRtcClientRequestFixup }

procedure TRtcClientRequestFixup.AssignTo(Dest: TPersistent);
  begin
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcClientRequestFixup) then
    begin
    TRtcClientRequestFixup(Dest).EncodeQuery:=EncodeQuery;
    TRtcClientRequestFixup(Dest).EncodeFileName:=EncodeFileName;
    TRtcClientRequestFixup(Dest).ForceOldHttp10:=ForceOldHttp10;
    end;
  end;

constructor TRtcClientRequestFixup.Create;
  begin
  inherited;
  FEncodeQuery:=False;
  FEncodeFileName:=False;
  FForceHttp10:=False;
  end;

destructor TRtcClientRequestFixup.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientRequestFixup.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcClientRequestFixup.Fixup(Request: TRtcRequest);
  var
    myPos:integer;
  begin
  if EncodeFileName and (length(Request.FileName)>0) then
    Request.FileName:=URL_Encode(Request.FileName);
  if EncodeQuery and (length(Request.Query.Text)>0) then
    begin
    if Request.Query.ItemCount=0 then
      Request.Query.Text:=URL_Encode(Request.Query.Text,True)
    else
      begin
      for myPos:=0 to Request.Query.ItemCount-1 do
        begin
        Request.Query.ItemName[myPos]:=URL_Encode(Request.Query.ItemName[myPos]);
        Request.Query.ItemValue[myPos]:=URL_Encode(Request.Query.ItemValue[myPos]);
        end;
      end;;
    end;
  if ForceOldHttp10 then
    Request.Close:=True;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
ConnCS:=TRtcCritSec.Create;
finalization
{$IFDEF RTC_DEBUG} Log('rtcConn Finalizing ...','DEBUG');{$ENDIF}
CloseTimerPool;
RtcFreeAndNil(ConnCS);
{$IFDEF RTC_DEBUG} Log('rtcConn Finalized.','DEBUG');{$ENDIF}
end.
