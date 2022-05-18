unit kbmMW2rtcSrv;

(*
 This is a very simplified, experimental kbmMW 2 RTC Server Transport implementation.

 This is a LEGACY unit, which means that continued use of this unit is discouraged.
 If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
 released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.

 This transport is necessary if you want to integrate kbmMW Server functionality 
 into a RTC Server. This kbmMW Server can only communicate with kbmMW Clients compiled
 with kbmMW components using the kbmMW 2 RTC Client Transport ("kbmMW2rtcSrv.pas" unit).

 @exclude
*)

interface

{$I kbmMW.inc}

//{$IFNDEF KBMMW_CLIENTSIDE_ONLY}
//{$IFDEF KBMMW_RTCTRANSPORT_SUPPORT}

uses
  Classes, Sysutils,
  kbmMWCustomTransport,kbmMWServer,kbmMWGlobal,

//************************************************************************************
// IF A COMPILE ERROR OCCURS HERE, YOU NEED TO CHECK YOU HAVE RTC SDK INSTALLED.     *

  rtcTypes, rtcSystem, rtcConn, rtcDataSrv

//                                                                                   *
//************************************************************************************
  ;

type
  TkbmMW2CustomRtcServerTransport = class(TkbmMWCustomServerTransport)
  private
    { Private declarations }
    FDataProvider:TRtcDataProvider;

    FRtcFileName:RtcString;
    FRtcHost:RtcString;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection);

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection);

    function GetListenStart: TRtcNotifyEvent;
    function GetListenStop: TRtcNotifyEvent;
    function GetRtcFileName: RtcString;
    function GetRtcHost: RtcString;
    function GetRtcLink: TRtcDataServerLink;
    function GetRtcOrder: integer;
    function GetRtcServer: TRtcDataServer;

    procedure SetListenStart(const Value: TRtcNotifyEvent);
    procedure SetListenStop(const Value: TRtcNotifyEvent);
    procedure SetRtcFileName(const Value: RtcString);
    procedure SetRtcHost(const Value: RtcString);
    procedure SetRtcLink(const Value: TRtcDataServerLink);
    procedure SetRtcOrder(const Value: integer);
    procedure SetRtcServer(const Value: TRtcDataServer);

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  public
    class function IsSerializedTransport:boolean; override;
    class function IsConnectionlessTransport:boolean; override;

    procedure Assign(ATransport:TPersistent); override;
    procedure Listen; override;
    procedure Close; override;
    procedure CloseConnection(AInfo:IkbmMWCustomTransportInfo); override;

    function BeginReceive(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream):boolean; override;
    function ReceiveStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream; ALimit:integer):boolean; override;
    procedure TransmitStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream); override;

    function IsListening:boolean; override;

  published
    { You can link your components (one or more) to a DataServerLink component
      by assigning your @Link(TRtcDataServerLink) component to this Link property.
      Doing this, you only have to set the Server property for the master
      DataServerLink component and don't need to do it for every single
      DataSource component. }
    property RtcLink:TRtcDataServerLink read GetRtcLink write SetRtcLink;

    { You can also link your components (one or more) directly to your
      DataServer connection component by assigning your
      @Link(TRtcDataServer) connection component to this Server property.
      This is useful if you have some complex functionality implemented
      in a single DataSource/DataServerLink component and don't need to drag
      the component through another DataServerLink component to get to
      your DataServer. }
    property RtcServer:TRtcDataServer read GetRtcServer write SetRtcServer;

    { This is the Order in which the components will be asked to
      process a request. The smaller this Order number, the sooner
      a component will be asked to process a request, compared to
      other components connected to the DataServer at the same level.
      Since we could have more levels (more DataServerLink components
      connected to each other), Order only defines the priority
      at the same level.
      @html(<br><br>)

      For example, if DataSourceA has Order=50 and DataServerLinkB has Order=60,
      when both components are assigned to the same parent DataServerLink or directly
      to the DataServer, DataSourceA will receive requests for checking before any
      component assigned to DataServerLinkB, no matter which Order the components
      assigned to DataServerLinkB have. This is because the Order property only
      defines the order in the list of components assigned to the same parent
      component (DataServerLink or DataSource).
      @html(<br><br>)

      To make this simpler, just think of this Order as you would of TabOrder
      in a Form, where DataServer is the Form, DataServerLink are the Panels and
      DataSource are edit controls. TabOrder is defined for each control
      inside its parent (TEdit inside TPanel, child TPanel inside parent TPanel
      or master TPanel directly on the TForm).
      @html(<br><br>)

      Order is especially important for components which could handle
      same requests, like the PHP source handler and File source handler.
      If FileSource handler would have lower order number than the
      PHPSource handler, then the PHP soruce handler would never be executed,
      because all PHP files would be simply sent out in their source code form
      by the File Source handler. This is why the File Source handler has to have
      order number bigger that the PHPSource handler and (preferably) be
      connected to the same component (DataServerLink or DataServer). }
    property RtcCheckOrder:integer read GetRtcOrder write SetRtcOrder default 0;

    { If RequestHost is specified, then Request.Host will be compared to the RtcHost
      property to decide if this request should be processed by this ServerModule. @html(<br>)
      If your DataServer has to serve more different hosts, while your ServerModule
      is not supposed to react to requests from all those hosts, you can assign the
      host name to which this ServerModule belongs to. If RtcHost is left blank,
      then this ServerModule will respond to any request asking for this servermodule's
      ModuleFileName, regardless of the HOST header. @html(<br><br>)

      To process all requests for a domain and all of its sub-domains, enter domain name ONLY
      (example: "realthinclient.com" for "realthinclient.com" and any sub-domain like
      "www.realthinclient.com", "mymod.myapp.realthinclient.com", etc). @html(<br>)
      To limit the requests to a sub-domain and its sub-sub-domains, enter the name
      of the highest sub-domain (example: "myapp.realthinclient.com" for
      "myapp.realthinclient.com" and any of its sub-domains like
      "mymod.myapp.realthinclient.com"). @html(<br>)
      To process ONLY requests pointed exactly to ONE HOST, add "." in front of your
      host name (example: ".realthinclient.com" will ONLY react to requests with
      "realthinclient.com" in their HOST header). }
    property RtcHost:RtcString read GetRtcHost write SetRtcHost;

    { This property will be compared to Request.FileName to decide if the
      request we just received was pointed to this Transport. Any request asking
      for this FileName will be processed by this Transport component.
      Since parameters are passed to the transport through request's Content
      body (rather than headers), we do not need to check the request for anything
      else than it's FileName to know if the request is directed to this transport. }
    property RtcFileName:RtcString read GetRtcFileName write SetRtcFileName;

    { This event will be mapped as @Link(TRtcServer.OnListenStart) event
      to the assigned Server component and called AFTER the Server's
      OnListenStart event, for all components. This event can be used
      to initialize the component after server starts listening. }
    property OnListenStart:TRtcNotifyEvent read GetListenStart write SetListenStart;
    { This event will be mapped as @Link(TRtcServer.OnListenStop) event
      to the assigned Server component and called BEFORE the Server's
      OnListenStop event, for all components. This event can be used
      to de-initialize the component before server stops listening. }
    property OnListenStop:TRtcNotifyEvent read GetListenStop write SetListenStop;
  end;

  TkbmMW2RtcServerTransport = class(TkbmMW2CustomRtcServerTransport)
  published
    { Published declarations }
    property Crypt;
    property Compression;
    property StreamFormat;
    property VerifyTransfer;
    property TransportStateOptions;
{$IFNDEF KBMMW_CODEGEAR_EDITION}
    property Plugin;
{$ENDIF}
    property Params;
    property StringConversion;
  end;

implementation

// RTC Server transport.
//******************************************************************
constructor TkbmMW2CustomRtcServerTransport.Create(AOwner:TComponent);
  begin
  inherited;
  FDataProvider:=TRtcDataProvider.Create(nil);
  FDataProvider.OnCheckRequest:=Call_CheckRequest;
  FDataProvider.OnDataReceived:=Call_DataReceived;
  end;

destructor TkbmMW2CustomRtcServerTransport.Destroy;
  begin
  FDataProvider.Server:=nil;
  FDataProvider.Link:=nil;
  FreeAndNil(FDataProvider);
  inherited;
  end;

class function TkbmMW2CustomRtcServerTransport.IsSerializedTransport:boolean;
  begin
  Result:=true;
  end;

class function TkbmMW2CustomRtcServerTransport.IsConnectionlessTransport:boolean;
  begin
  Result:=true;
  end;

function TkbmMW2CustomRtcServerTransport.GetRtcFileName: RtcString;
  begin
  Result:=FRtcFileName;
  end;

function TkbmMW2CustomRtcServerTransport.GetRtcHost: RtcString;
  begin
  Result:=FRtcHost;
  end;

function TkbmMW2CustomRtcServerTransport.GetRtcLink: TRtcDataServerLink;
  begin
  if assigned(FDataProvider) then Result:=FDataProvider.Link
  else Result:=nil;
  end;

function TkbmMW2CustomRtcServerTransport.GetRtcOrder: integer;
  begin
  if assigned(FDataProvider) then Result:=FDataProvider.CheckOrder
  else Result:=0;
  end;

function TkbmMW2CustomRtcServerTransport.GetRtcServer: TRtcDataServer;
  begin
  if assigned(FDataProvider) then Result:=FDataProvider.Server
  else Result:=nil;
  end;

procedure TkbmMW2CustomRtcServerTransport.SetRtcFileName(const Value: RtcString);
  begin
  FRtcFileName:=Value;
  end;

procedure TkbmMW2CustomRtcServerTransport.SetRtcHost(const Value: RtcString);
  begin
  FRtcHost:=Value;
  end;

procedure TkbmMW2CustomRtcServerTransport.SetRtcLink(const Value: TRtcDataServerLink);
  begin
  if assigned(FDataProvider) then FDataProvider.Link:=Value;
  end;

procedure TkbmMW2CustomRtcServerTransport.SetRtcOrder(const Value: integer);
  begin
  if assigned(FDataProvider) then FDataProvider.CheckOrder:=Value;
  end;

procedure TkbmMW2CustomRtcServerTransport.SetRtcServer(const Value: TRtcDataServer);
  begin
  if assigned(FDataProvider) then FDataProvider.Server:=Value;
  end;

procedure TkbmMW2CustomRtcServerTransport.Assign(ATransport:TPersistent);
  begin
  inherited;
  if ATransport is TkbmMW2CustomRtcServerTransport then
     begin
     RtcServer:=TkbmMW2CustomRtcServerTransport(ATransport).RtcServer;
     RtcLink:=TkbmMW2CustomRtcServerTransport(ATransport).RtcLink;
     RtcCheckOrder:=TkbmMW2CustomRtcServerTransport(ATransport).RtcCheckOrder;
     RtcHost:=TkbmMW2CustomRtcServerTransport(ATransport).RtcHost;
     RtcFileName:=TkbmMW2CustomRtcServerTransport(ATransport).RtcFileName;
     end;
  end;

// Will be executed for each accepted request.
procedure TkbmMW2CustomRtcServerTransport.Call_DataReceived(Sender:TRtcConnection);
  var
    Info:IkbmMWServerTransportInfo;
  begin
  if TRtcDataServer(Sender).Request.Complete then
    begin
    Info:=TkbmMWServerTransportInfo.Create;
    try
      Info.Client:=Sender;
      DoConnected(Info);
      Server.ServeRequest(Self,Info);
      finally
        DoDisconnected(Info);
      end;
    end;
  end;

procedure TkbmMW2CustomRtcServerTransport.SetListenStart(const Value: TRtcNotifyEvent);
  begin
  if assigned(FDataProvider) then FDataProvider.OnListenStart:=Value;
end;

procedure TkbmMW2CustomRtcServerTransport.SetListenStop(const Value: TRtcNotifyEvent);
  begin
  if assigned(FDataProvider) then FDataProvider.OnListenStop:=Value;
  end;

function TkbmMW2CustomRtcServerTransport.GetListenStart: TRtcNotifyEvent;
  begin
  if assigned(FDataProvider) then Result:=FDataProvider.OnListenStart
  else Result:=nil;
  end;

function TkbmMW2CustomRtcServerTransport.GetListenStop: TRtcNotifyEvent;
  begin
  if assigned(FDataProvider) then Result:=FDataProvider.OnListenStop
  else Result:=nil;
  end;

procedure TkbmMW2CustomRtcServerTransport.Listen;
  begin
  if assigned(FDataProvider) then
    if assigned(FDataProvider.Server) then
      FDataProvider.Server.Listen;
  SetState(mwtrstListening);
  end;

procedure TkbmMW2CustomRtcServerTransport.Close;
  begin
  if assigned(FDataProvider) then
    if assigned(FDataProvider.Server) then
      FDataProvider.Server.StopListen;
  SetState(mwtrstDisconnected);
  end;

function TkbmMW2CustomRtcServerTransport.IsListening:boolean;
  begin
  Result:=(State = mwtrstListening);
  end;

procedure TkbmMW2CustomRtcServerTransport.Call_CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.FileName=FRtcFileName then
      begin
      if FRtcHost<>'' then
        begin
        if copy(FRtcHost,1,1)<>'.' then // accepting domain with any sub-domain
          begin
          if (Upper_Case(Request.Host)=FRtcHost) then // host = domain name
            Accept
          else if ( (length(Request.Host)>length(FRtcHost)) and // could be sub-domain
                    (Copy(Request.Host,length(Request.Host)-length(FRtcHost),1)='.') and // has '.' in the right place
                    ( Upper_Case(Copy(Request.Host,length(Request.Host)-length(FRtcHost)+1,length(FRtcHost)))
                      = FRtcHost) ) then // is sub-domain
            Accept;
          end
        else if Upper_Case(Request.Host)=Copy(FRtcHost,2,length(FRtcHost)-1) then
          Accept; // accepting a specific sub-domain only
        end
      else // Accept the request. It has our ModuleFileName as Request.FileName, we accept all hosts
        Accept;
      end;
    end;
  end;

procedure TkbmMW2CustomRtcServerTransport.CloseConnection(AInfo:IkbmMWCustomTransportInfo);
  var
    conn:TRtcDataServer;
  begin
  conn:=TRtcDataServer((AInfo as IkbmMWServerTransportInfo).Client);
  conn.Disconnect;
  end;

function TkbmMW2CustomRtcServerTransport.BeginReceive(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream):boolean;
  var
    conn:TRtcDataServer;
  begin
  inherited BeginReceive(AInfo,AStream);
  conn:=TRtcDataServer((AInfo as IkbmMWServerTransportInfo).Client);
  (AStream as IkbmMWCustomRequestTransportStream).RemoteLocation:=conn.PeerAddr+':'+conn.PeerPort;
  Result:=true;
  end;

// Recieve data from socket.
function TkbmMW2CustomRtcServerTransport.ReceiveStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream; ALimit:integer):boolean;
  var
    conn:TRtcDataServer;
    data:RtcByteArray;
  begin
  AStream.Rewind;
  conn:=TRtcDataServer((AInfo as IkbmMWServerTransportInfo).Client);
  data:=nil;
  data:=conn.ReadEx;
  if length(data)>0 then
    AStream.DataStream.Write(data[0],length(data));
  Result:=true;
  end;

// Add data through socket.
procedure TkbmMW2CustomRtcServerTransport.TransmitStream(AInfo:IkbmMWCustomTransportInfo; const AStream:IkbmMWCustomTransportStream);
  var
    conn:TRtcDataServer;
    arr:RtcByteArray;
  begin
  AStream.Rewind;
  conn:=TRtcDataServer((AInfo as IkbmMWServerTransportInfo).Client);
  SetLength(arr, AStream.DataStream.Size);
  AStream.DataStream.Read(arr[0],length(arr));
  conn.WriteEx(arr);
  end;

//{$ELSE}
//implementation
//{$ENDIF}
//{$ELSE}
//implementation
//{$ENDIF}

end.
