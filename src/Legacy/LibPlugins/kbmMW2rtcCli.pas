unit kbmMW2rtcCli;

(*
 This is a very simplified, experimental kbmMW 2 RTC Client Transport implementation.

 This is a LEGACY unit, which means that continued use of this unit is discouraged.
 If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
 released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.

 This transport is necessary if you want to write a Client to communicate with a 
 kbmMW Server compiled with the kbmMW 2 RTC Server Transport ("kbmMW2rtcSrv.pas" unit).

 @exclude
*)

interface

{$I kbmMW.inc}

//{$IFDEF KBMMW_RTCTRANSPORT_SUPPORT}

uses
  Classes, SysUtils,
  kbmMWCustomTransport,kbmMWClient,kbmMWGlobal,

//************************************************************************************
// IF A COMPILE ERROR OCCURS HERE, YOU NEED TO CHECK YOU HAVE RTC SDK INSTALLED      *
// kbmMWConfig.inc FILE.                                                             *
//                                                                                   *
  rtcTypes, rtcSystem, rtcInfo, rtcConn, rtcDataCli
//                                                                                   *
//************************************************************************************
  ;

type
  TkbmMW2CustomRtcClientTransport = class(TkbmMWCustomClientTransport)
  private
    { Private declarations }
    FDataRequest:TRtcDataRequest;

    MyRequestContent,
    MyResponseContent:RtcByteArray;
    MyResponseStatus:integer;
    MyResponseHeader:RtcString;
    FRtcHost: RtcString;
    FRtcFileName: RtcString;

    function GetRtcAutoRepost: integer;
    function GetRtcHyperThreading: boolean;
    function GetRtcAutoSync: boolean;
    function GetRtcClient: TRtcDataClient;
    function GetRtcLink: TRtcDataClientLink;

    procedure SetRtcAutoRepost(const Value: integer);
    procedure SetRtcHyperThreading(const Value: boolean);
    procedure SetRtcAutoSync(const Value: boolean);
    procedure SetRtcClient(const Value: TRtcDataClient);
    procedure SetRtcLink(const Value: TRtcDataClientLink);

  protected

    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection);
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection);
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection);

  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    class function IsSerializedTransport:boolean; override;
    class function IsConnectionlessTransport:boolean; override;

    procedure Assign(ATransport:TPersistent); override;

    procedure Connect; override;
    procedure Disconnect; override;
    function IsConnected:boolean; override;

    function ReceiveStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream; ALimit:integer):boolean; override;
    procedure TransmitStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream); override;

  published
    property Active;

    { "Request.Host" will be assigned this property before sending the request out. @html(<br>)
      It is not necessary to set this property if your server's kbmMW2RtcServerTransport component
      left its "RtcHost" property blank. On the other hand, for servers which serve multiple hosts, 
      mostly where kbmMW2RtcServerModule has assigned its "RtcHost" property, it is very important 
      to set this kbmMW2RtcClientTransport's "RtcHost" property to the appropriate host name. }
    property RtcHost:RtcString read FRtcHost write FRtcHost;

    { To be able to connect to the kbmMW transport on the Server, this components RtcFileName
      property has to be identical to the "RtcFileName" property of the kbmMW2RtcServerTransport
      which you want to connect to. This property will be assigned to "Request.FileName" before
      sending any request out, so you won't be preparing the Request headers manualy. @html(<br>)
      All kbmMW data will be passed to the kbmMW2RtcServerTransport through request's Content body, 
      so that kbmMW2RtcServerTransport won't need to check the request headers for anything else 
      than "Request.FileName" (RtcFileName property) to know if the request is directed at it. }
    property RtcFileName:RtcString read FRtcFileName write FRtcFileName;

     { If all events which your component implements have to access the GUI,
      to avoid checking the "Sender.inMainThread" and calling Sender.Sync(Event)
      for every event, you can se this AutoSyncEvent property to true,
      which will ensure that any event assigned to this component will
      be called from the main thread (synchronized, when needed). }
    property RtcAutoSyncEvents:boolean read GetRtcAutoSync write SetRtcAutoSync default false;

    { You can link your components (one or more) to a DataClientLink component
      by assigning your @Link(TRtcDataClientLink) component to chind component's Link property.
      Doing this, you only have to set the Client property for the master
      DataClientLink component and don't need to do it for every single
      DataRequest component. }
    property RtcLink:TRtcDataClientLink read GetRtcLink write SetRtcLink;

    { You can also link your components (one or more) directly to your
      DataClient connection component by assigning your
      @Link(TRtcDataClient) connection component to this child component's Client property.
      This is useful if you don't want to use a DataClientLink. }
    property RtcClient:TRtcDataClient read GetRtcClient write SetRtcClient;

   { If you want to enable the possibility to use this transport to send requests
      from multiple threads AT THE SAME TIME, where this component will acs as if
      it were X components, one for each thread, simply set HyperThreading to TRUE. @html(<br><br>)

      This is useful if you need to send requests to another Server from
      within your Server running in multi-threaded mode and want to use only one set of
      rtcHttpClient/transport components for all clients connected to your Server.

      Even in HyperThreading mode, only properties and methods needed to prepare and post
      the request (Request and Post) will use a separate copy for each thread, while all
      other properties and methods exist only once for all threads, so don't try to modify
      them while your application is actively using the component in multi-threaded mode. @html(<br><br>)

      Leave HyperThreading as FALSE to use this component "the stadard way" (for example,
      when you're writing a client application where requests are posted from the main thread
      or if you are creating a separate component for every thread that needs it). }
    property RtcHyperThreading:boolean read GetRtcHyperThreading write SetRtcHyperThreading default False;

    { Set this property to a value other than 0 (zero) if you want the DataRequest to
      auto-repost any request up to "AutoRepost" times, in case the connection gets lost
      while sending data to server or receiving data from server.
      AutoRepost = -1 means that request should be reposted infinitely. }
    property RtcAutoRepost:integer read GetRtcAutoRepost write SetRtcAutoRepost default 0;
  end;

  TkbmMW2RtcClientTransport = class(TkbmMW2CustomRtcClientTransport)
  published
    { Published declarations }
    property Crypt;
    property Compression;
    property Params;
    property StreamFormat;
    property VerifyTransfer;
    property TransportStateOptions;
    property MaxRetries;
    property MaxRetriesAlternative;
    property RequestTimeout;
    property OnReconnect;
    property OnException;
    property OnConnectionLost;
    property OnConnected;
    property OnDisconnected;
    property ConnectionString;
{$IFNDEF KBMMW_CODEGEAR_EDITION}
    property Plugin;
{$ENDIF}
    property ConnectTimeout;
    property StringConversion;
    property FallbackServers;
    property AutoFallback;
  end;

implementation

// RTC Client transport.
//**************************************************
constructor TkbmMW2CustomRtcClientTransport.Create(AOwner:TComponent);
  begin
  inherited;
  FDataRequest:=TRtcDataRequest.Create(nil);
  FDataRequest.OnBeginRequest:=Call_BeginRequest;
  FDataRequest.OnResponseDone:=Call_ResponseDone;
  FDataRequest.OnResponseAbort:=Call_ResponseAbort;
  end;

destructor TkbmMW2CustomRtcClientTransport.Destroy;
  begin
  if FDataRequest<>nil then
     begin
     FDataRequest.Client:=nil;
     FDataRequest.Link:=nil;
     FDataRequest.Free;
     end;
  inherited;
  end;

class function TkbmMW2CustomRtcClientTransport.IsSerializedTransport:boolean;
  begin
  Result:=true;
  end;

class function TkbmMW2CustomRtcClientTransport.IsConnectionlessTransport:boolean;
  begin
  Result:=false;
  end;

procedure TkbmMW2CustomRtcClientTransport.Assign(ATransport:TPersistent);
  begin
  inherited;
  if ATransport is TkbmMW2CustomRtcClientTransport then
     begin
     RtcClient:=TkbmMW2CustomRtcClientTransport(ATransport).RtcClient;
     RtcLink:=TkbmMW2CustomRtcClientTransport(ATransport).RtcLink;
     RtcAutoSyncEvents:=TkbmMW2CustomRtcClientTransport(ATransport).RtcAutoSyncEvents;
     RtcHyperThreading:=TkbmMW2CustomRtcClientTransport(ATransport).RtcHyperThreading;
     RtcAutoRepost:=TkbmMW2CustomRtcClientTransport(ATransport).RtcAutoRepost;
     RtcFileName:=TkbmMW2CustomRtcClientTransport(ATransport).RtcFileName;
     RtcHost:=TkbmMW2CustomRtcClientTransport(ATransport).RtcHost;
     end;
  end;

procedure TkbmMW2CustomRtcClientTransport.SetRtcAutoRepost(const Value: integer);
  begin
  if assigned(FDataRequest) then FDataRequest.AutoRepost:=Value;
  end;

procedure TkbmMW2CustomRtcClientTransport.SetRtcAutoSync(const Value: boolean);
  begin
  if assigned(FDataRequest) then FDataRequest.AutoSyncEvents:=Value;
  end;

procedure TkbmMW2CustomRtcClientTransport.SetRtcClient(const Value: TRtcDataClient);
  begin
  if assigned(FDataRequest) then FDataRequest.Client:=Value;
  end;

procedure TkbmMW2CustomRtcClientTransport.SetRtcHyperThreading(const Value: boolean);
  begin
  if Assigned(FDataRequest) then FDataRequest.HyperThreading:=Value;
  end;

procedure TkbmMW2CustomRtcClientTransport.SetRtcLink(const Value: TRtcDataClientLink);
  begin
  if assigned(FDataRequest) then FDataRequest.Link:=Value;
  end;

function TkbmMW2CustomRtcClientTransport.GetRtcAutoRepost: integer;
  begin
  if assigned(FDataRequest) then Result:=FDataRequest.AutoRepost
  else Result:=0;
  end;

function TkbmMW2CustomRtcClientTransport.GetRtcAutoSync: boolean;
  begin
  if assigned(FDataRequest) then Result:=FDataRequest.AutoSyncEvents
  else Result:=False;
  end;

function TkbmMW2CustomRtcClientTransport.GetRtcClient: TRtcDataClient;
  begin
  if assigned(FDataRequest) then Result:=FDataRequest.Client
  else Result:=nil;
  end;

function TkbmMW2CustomRtcClientTransport.GetRtcHyperThreading: boolean;
  begin
  if assigned(FDataRequest) then Result:=FDataRequest.HyperThreading
  else Result:=False;
  end;

function TkbmMW2CustomRtcClientTransport.GetRtcLink: TRtcDataClientLink;
  begin
  if assigned(FDataRequest) then Result:=FDataRequest.Link
  else Result:=nil;
  end;

procedure TkbmMW2CustomRtcClientTransport.Call_BeginRequest(Sender: TRtcConnection);
  begin
  Sender.WriteEx(MyRequestContent);
  end;

procedure TkbmMW2CustomRtcClientTransport.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  MyResponseStatus:=-1;
  end;

procedure TkbmMW2CustomRtcClientTransport.Call_ResponseDone(Sender: TRtcConnection);
  begin
  MyResponseHeader:=TRtcDataClient(Sender).Response.HeaderText;
  MyResponseStatus:=TRtcDataClient(Sender).Response.StatusCode;
  MyResponseContent:=Sender.ReadEx;
  end;

procedure TkbmMW2CustomRtcClientTransport.Connect;
  begin
  if assigned(FDataRequest) then
    if assigned(FDataRequest.Client) then
      FDataRequest.Client.Connect;
  SetState(mwtrstConnected);
  DoConnected(Info);
  end;

procedure TkbmMW2CustomRtcClientTransport.Disconnect;
  begin
  SetState(mwtrstDisconnected);
  if assigned(FDataRequest) then
    if assigned(FDataRequest.Client) then
      FDataRequest.Client.Disconnect;
  DoDisconnected(Info);
  end;

function TkbmMW2CustomRtcClientTransport.IsConnected:boolean;
  begin
  Result:=(State in [mwtrstConnected,mwtrstListening]);
  end;

function TkbmMW2CustomRtcClientTransport.ReceiveStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream; ALimit:integer):boolean;
  begin
  FDataRequest.Request.Method:='POST';
  FDataRequest.Request.FileName:=FRtcFileName;
  FDataRequest.Request.Host:=FRtcHost;
  FDataRequest.Post;
  FDataRequest.WaitForCompletionEx;
  if MyResponseStatus<>200 then
    Result:=False
  else
    begin
    if assigned(MyResponseContent) then
      begin
      if length(MyResponseContent)>0 then
        AStream.DataStream.Write(MyResponseContent[0],length(MyResponseContent));
      SetLength(MyResponseContent,0);
      end;
    Result:=true;
    end;
  end;

procedure TkbmMW2CustomRtcClientTransport.TransmitStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream);
  begin
  AStream.Rewind;
  SetLength(MyRequestContent, AStream.DataStream.Size);
  if AStream.DataStream.Size>0 then
    AStream.DataStream.Read(MyRequestContent[0],length(MyRequestContent));
  end;

//{$ELSE}
//implementation
//{$ENDIF}

end.
