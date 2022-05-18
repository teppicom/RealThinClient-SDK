{
  @html(<b>)
  RTC Load Balancer Demo Project
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This Project shows how to use multiple TRtcLoadBalancer components with
  the RealThinClient SDK to write a HTTP Load Balancer capable of handling
  multiple different Web Applications at different Virtual Hosts and Paths.
}

unit MainUnit3;

interface

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{ To compile the project with StreamSec Tools 2.1+ components using demo SSL certificates,
  declare the StreamSecII compiler directive below, in your project or in the "rtcDeploy.inc" file.

  When StreamSecII compiler directive is declared,
  the "SSL" option for the Load Balancer will be enabled. }
{.$DEFINE StreamSecII}

{ Declare SERVER_SSL if your Servers are using SSL,
  so the Load Balancer will also use SSL to communicate with the Servers.

  !This option requires 3rd-party encryption components (StreamSecII)! }
{.$DEFINE SERVER_SSL}

{ Declare DEBUG_SSL to use small socket buffer sizes, emulating slow connections }
{.$DEFINE DEBUG_SSL}

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, ComCtrls,

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

  rtcTypes, rtcSystem, rtcConn,
  rtcDataCli, rtcDataSrv,
  rtcHttpCli, rtcHttpSrv,
  rtcInfo, rtcLog, rtcThrPool,
  rtcDataRoute, rtcLoadBalance;

type
  TRtcLoadBalancerMainForm = class(TForm)
    Server: TRtcHttpServer;
    StatProvider: TRtcDataProvider;
    DumpProvider: TRtcDataProvider;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label9: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Label5: TLabel;
    Label10: TLabel;
    Bevel5: TBevel;
    xServerMulti: TCheckBox;
    xServerBlocking: TCheckBox;
    xClientMulti: TCheckBox;
    xClientBlocking: TCheckBox;
    xForceHttp10: TCheckBox;
    xResponseBuffer: TCheckBox;
    xRequestBuffer: TCheckBox;
    xRequestInTimeouts: TCheckBox;
    xResponseOutTimeout: TCheckBox;
    xRequestOutTimeout: TCheckBox;
    xResponseInTimeout: TCheckBox;
    eRequestInTime: TSpinEdit;
    eResponseOutTime: TSpinEdit;
    eResponseInTime: TSpinEdit;
    eRequestOutTime: TSpinEdit;
    xPostReturnBeforeResponseSent: TCheckBox;
    Label4: TLabel;
    eFromPort: TEdit;
    TabSheet3: TTabSheet;
    Label7: TLabel;
    xDebugLog: TCheckBox;
    eLogFolder: TEdit;
    xBuffLog: TCheckBox;
    bDumpLog: TButton;
    Label2: TLabel;
    bConnect: TButton;
    Label3: TLabel;
    eThrCount: TSpinEdit;
    Bevel1: TBevel;
    Label8: TLabel;
    eServerList: TMemo;
    Label1: TLabel;
    eVirtualHost: TEdit;
    Label13: TLabel;
    Label15: TLabel;
    eVirtualPath: TEdit;
    eServerAddr: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    eServerPort: TEdit;
    Label19: TLabel;
    Label20: TLabel;
    eServerURI: TEdit;
    eMaxConn: TSpinEdit;
    Label21: TLabel;
    eMaxSess: TSpinEdit;
    eSessTimeout: TSpinEdit;
    Label22: TLabel;
    Label23: TLabel;
    Bevel2: TBevel;
    Label24: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    bAddServer: TButton;
    bAddWebApp: TButton;
    Label29: TLabel;
    Label6: TLabel;
    Bevel6: TBevel;
    cAppType: TComboBox;
    Label11: TLabel;
    cBalanceType: TComboBox;
    Label25: TLabel;
    cReqOrder: TComboBox;
    Label30: TLabel;
    xSSL: TCheckBox;
    xServerIP6: TCheckBox;
    xClientIP6: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bConnectClick(Sender: TObject);
    procedure eLogFolderChange(Sender: TObject);
    procedure StatProviderCheckRequest(Sender: TRtcConnection);
    procedure StatProviderDataReceived(Sender: TRtcConnection);
    procedure xBuffLogClick(Sender: TObject);
    procedure bDumpLogClick(Sender: TObject);
    procedure DumpProviderCheckRequest(Sender: TRtcConnection);
    procedure DumpProviderDataReceived(Sender: TRtcConnection);

    procedure eServerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure eServerListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure eServerDetailsChange(Sender: TObject);
    procedure bAddServerClick(Sender: TObject);
    procedure bAddWebAppClick(Sender: TObject);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure xSSLClick(Sender: TObject);
    { Private declarations }

  public
    { Public declarations }
    FEventLog:boolean;

    FCliMultiThreaded:boolean;
    FCliBlocking:boolean;
    FCliFixupRequest:boolean;

    BalancersActive:boolean;

    Balancers:array of TRtcLoadBalancer;

    procedure CreateBalancers;
    procedure DestroyBalancers;

    procedure ServerInfoFromLine(lin:RtcString;
                                 var VirtualHost,VirtualPath,
                                 ServerAddr,ServerPort,ServerURI:RtcString;
                                 var MaxConn,MaxSess,SessTimeout,
                                 AppType,BalanceType,RequestOrder:integer);

    function ServerLineFromInfo(VirtualHost,VirtualPath,
                                ServerAddr,ServerPort,ServerURI:RtcString;
                                MaxConn,MaxSess,SessTimeout,
                                AppType,BalanceType,RequestOrder:integer):RtcString;

    procedure UpdateServerDetails;
    procedure UpdateServerList;

    function SetupBalancer(const MyVirtualHost, MyVirtualPath:RtcString;
                           MyAppType, MyBlanceType, MyReqOrder:integer):TRtcLoadBalancer;

    procedure LoadBalancer_PrepareConnection(Sender: TRtcDataClient);
    procedure LoadBalancer_DebugLog(Info: TRtcRouterDebugInfo);

    procedure IntraWebApp_RequestBodyHasSessionID(Sender: TRtcDataServer; var NeedContent: Boolean);
    procedure IntraWebApp_RequestPrepareSessionID(Sender: TRtcDataServer; Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
    procedure IntraWebApp_ResponseBodyHasSessionID(Sender: TRtcDataClient; var NeedContent: Boolean);
    procedure IntraWebApp_ResponsePrepareSessionID(Sender: TRtcDataClient; Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);

    procedure IntraWeb_ResponseBegin(Sender: TRtcDataClient);
    procedure IntraWeb_ResponseReceived(Sender: TRtcDataClient; Content: TRtcRouterContentBody);

    procedure RTCApp_RequestPrepareSessionID(Sender: TRtcDataServer; Content: TRtcRouterContentBody;Session: TRtcLoadBalancerSession);
    procedure RTCApp_ResponsePrepareSessionID(Sender: TRtcDataClient; Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
  end;

var
  RtcLoadBalancerMainForm: TRtcLoadBalancerMainForm;

implementation

{$R *.dfm}

(*********************************************************************************************)
(*                                                                                           *)
(*   Methods required for Session handling (extract SessionIDs from Requests and Responses)  *)
(*                                                                                           *)
(*********************************************************************************************)

{ RTC Session handling }

procedure TRtcLoadBalancerMainForm.RTCApp_RequestPrepareSessionID(Sender: TRtcDataServer; Content: TRtcRouterContentBody;Session: TRtcLoadBalancerSession);
  begin
  Session.ID:=Sender.Request.Query.ValueCS['ID'];
  if length(Session.ID)<>32 then
    Session.ID:='';
  end;

procedure TRtcLoadBalancerMainForm.RTCApp_ResponsePrepareSessionID(Sender: TRtcDataClient; Content: TRtcRouterContentBody;Session: TRtcLoadBalancerSession);
  begin
  Session.ID := Sender.Request.Query.ValueCS['ID'];
  if length(Session.ID)=32 then
    begin
    if Sender.Response.StatusCode=410 then
      Session.Close:=True // Session Expired
    else if Sender.Response.StatusCode=200 then
      Session.Close:=False // Update Session Timeout
    else
      Session.ID:='';
    end
  else if assigned(Sender.Response.Cookie) then
    begin
    Session.ID:=Sender.Response.Cookie.ValueCS['ID'];
    if length(Session.ID)=32 then
      Session.Close := Sender.Response.StatusCode=410
    else
      Session.ID:='';
    end
  else
    Session.ID:='';
  end;

{ IntraWeb Session handling }

procedure TRtcLoadBalancerMainForm.IntraWebApp_RequestBodyHasSessionID(Sender: TRtcDataServer; var NeedContent: Boolean);
  begin
  NeedContent := Sender.Request.Method='POST';
  end;

procedure TRtcLoadBalancerMainForm.IntraWebApp_RequestPrepareSessionID(Sender: TRtcDataServer;
    Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
  var
    s:RtcString;
  begin
  if assigned(Content) and (Sender.Request.Method='POST') then
    begin
    { "Request.Params" can be used to extract parameters from
       "x-www-for-urlencoded" and "multipart/form-data" encoded content body. }
    Sender.Request.Params.AddText(Content.Body);

    Session.ID:=Sender.Request.Params['IW_SessionID_'];
    if (length(Session.ID)<20) or (length(Session.ID)>40) then
      Session.ID:='';

    Sender.Request.Params.Clear;
    end;

  if Session.ID='' then
    begin
    s:=Upper_Case(Sender.Request.FilePath[0]);
    if s='CALLBACK' then
      begin
      Session.ID:=Sender.Request.FilePath[Sender.Request.FilePath.Count-1];
      if length(Session.ID)<20 then // SessionID will have at least 20 characters
        Session.ID:='';
      end
    else if s='CACHE' then
      begin
      Session.ID:=Sender.Request.FilePath[2];
      if length(Session.ID)<20 then // SessionID will have at least 20 characters
        Session.ID:='';
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.IntraWebApp_ResponseBodyHasSessionID(Sender: TRtcDataClient; var NeedContent: Boolean);
  begin
  // When using Hidden Sessions, SessionID will be received in the "/" or "/EXE" HTML page
  if (Sender.Request.FileName='/') or
     (Upper_Case(Sender.Request.FileName)='/EXEC') then
    NeedContent := Copy(Sender.Response.ContentType,1,9)='text/html';
  end;

procedure TRtcLoadBalancerMainForm.IntraWebApp_ResponsePrepareSessionID(Sender: TRtcDataClient;
      Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
  var
    loc:integer;
  begin
  if assigned(Content) and
    ( (Sender.Request.FileName='/') or
      (Upper_Case(Sender.Request.FileName)='/EXEC') ) then
    begin
    // SessionID will be in a hidden variable "IW_SessionID_"
    Session.ID:='name="IW_SessionID_" value="';

    loc:=Pos(Session.ID,Content.Body);
    if loc>0 then
      begin
      // Session ID will be less than 40 characters, terminated with "
      Session.ID:=Copy(Content.Body, loc+length(Session.ID), 40);
      loc:=Pos('"',Session.ID);
      if loc>0 then
        Session.ID:=Copy(Session.ID,1,loc-1)
      else
        Session.ID:='';
      end
    else
      Session.ID:='';

    // Not receiving a SessionID
    if Session.ID='' then
      { If we are NOT receiving a SessionID or if we got a different SessionID,
        we should close the SessionID we have been using for this Request. }
      Session.Close:=True;
    end;
  end;

procedure TRtcLoadBalancerMainForm.IntraWeb_ResponseBegin(Sender: TRtcDataClient);
  begin
  if Copy(Sender.Response.ContentType,1,9)='text/html' then
    Sender.Response.ManualRead:=False;
  end;

procedure TRtcLoadBalancerMainForm.IntraWeb_ResponseReceived(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
  var
    root,path:RtcString;
  begin
  if assigned(Content) then
    begin
    path:=TRtcLoadBalancer.GetVirtualPath(Sender);
    root:=TRtcLoadBalancer.GetRootURI(Sender);
    if path='' then path:='/';
    if root='' then root:='/';
    if path<>root then
      begin
      if root='/' then
        begin
        Content.Body:=StringReplace(Content.Body,'var GURLBase="','var GURLBase="'+path,[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'action="/','action="'+path+'/',[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'src="/','src="'+path+'/',[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'href="/','href="'+path+'/',[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'<submit>/','<submit>'+path+'/',[rfReplaceAll,rfIgnoreCase]);
        end
      else
        begin
        Content.Body:=StringReplace(Content.Body,'var GURLBase="'+root,'var GURLBase="'+path,[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'action="'+root,'action="'+path,[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'src="'+root,'src="'+path,[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'href="'+root,'href="'+path,[rfReplaceAll,rfIgnoreCase]);
        Content.Body:=StringReplace(Content.Body,'<submit>'+root,'<submit>'+path,[rfReplaceAll,rfIgnoreCase]);
        end;
      end;
    end;
  end;

(*************************************************************************************************)
(*                                                                                               *)
(*   Methods and Events required to implement a Load Balancer (mostly Application-independent)   *)
(*                                                                                               *)
(*************************************************************************************************)

{ TRtcDataRouter methods and events }

procedure TRtcLoadBalancerMainForm.LoadBalancer_PrepareConnection(Sender: TRtcDataClient);
  begin
  if Sender is TRtcHttpClient then
    with Sender as TRtcHttpClient do
      begin
    {$IFDEF SERVER_SSL}
      {$IFDEF StreamSecII}
        { This is a simple DEMO, so we do not care if the Servers we are connecting to have valid certificates.
          To make this possible, we will use a HACK allowing us to work with ANY Server, regardless of its certificate: }
        BeGullableAndTrustAnythingSentToYou(True);
        { We will also allow our client to accept expired certificates: }
        AllowExpiredCertificates(True);
        { We want to ignore Host names inside certificates and simply accept anything we get: }
        AddCertificateNameMap('*','*'); // comment this out to work only with Servers using valid certificates
        if File_Exists('root.cer') then // we have a root certificate?
          begin
          // Load our demo certificates ...
          AddClientPFXFile('client.pfx','abc'); // our "demo" client PFX file
          AddClientRootCertFile('root.cer'); // our "demo" root certificate

          { Need to have "NO_DISABLEMD5SIGN" compiler directive declared
            if we want to be able to load root certificates from "p7b" files.
            If this compiler directive is NOT declared for this project,
            windows root certificates will NOT be loaded and we will be
            getting FATAL errors for certificates unless we call:
            BeGullibleAndTrustAnythingSentToYou(TRUE); }
          {$IFDEF NO_DISABLEMD5SIGN}
          AddClientRootCertFile('rootX.p7b'); // Standard Windows Root certificates (exported from IE)
          AddClientRootCertFile('rootY.p7b'); // Additional Windows certificates (exported from IE)
          {$ENDIF}
          end;
        CryptPlugin:=GetClientCryptPlugin;
      {$ENDIF}
    {$ENDIF}
      MultiThreaded:=FCliMultiThreaded;
      Blocking:=FCliBlocking;
      FixupRequest.ForceOldHttp10:=FCliFixupRequest;
      end;
  end;

procedure TRtcLoadBalancerMainForm.LoadBalancer_DebugLog(Info: TRtcRouterDebugInfo);
  begin
  Log(Info.Text,Info.Name);
  end;

(****************************************************************************)
(*                                                                          *)
(*   Methots used to Set-up, Create and Destroy Load Balancer components    *)
(*                                                                          *)
(****************************************************************************)

procedure TRtcLoadBalancerMainForm.ServerInfoFromLine(lin: RtcString;
    var VirtualHost, VirtualPath, ServerAddr, ServerPort, ServerURI: RtcString;
    var MaxConn, MaxSess, SessTimeout, AppType, BalanceType, RequestOrder: integer);
  var
    tmp:RtcString;
    lastDelim:boolean;

  function GetToken(delim:RtcChar):RtcString;
    var
      i:integer;
    begin
    i:=PosEx(delim,lin);
    if i>0 then
      begin
      lastDelim:=True;
      Result:=Trim(Copy(lin,1,i-1));
      Delete(lin,1,i);
      if delim=' ' then
        lin:=Trim(lin);
      end
    else
      begin
      lastDelim:=False;
      Result:=Trim(lin);
      lin:='';
      end;
    end;

  begin
  VirtualHost:='';
  VirtualPath:='';
  ServerAddr:='';
  ServerPort:='';
  ServerURI:='';
  MaxConn:=0;
  MaxSess:=0;
  SessTimeout:=0;
  BalanceType:=0;
  RequestOrder:=0;
  AppType:=0;

  lin:=Trim(lin);
  VirtualHost:=GetToken('/');
  if lastDelim then
    begin
    VirtualPath:='/'+GetToken(' ');

    tmp:=GetToken(' '); AppType:=StrToIntDef(tmp,0);
    tmp:=GetToken(' '); BalanceType:=StrToIntDef(tmp,0);
    tmp:=GetToken(' '); RequestOrder:=StrToIntDef(tmp,0);

    ServerAddr:=GetToken(':');
    if lastDelim then
      begin
      ServerPort:=GetToken('/');
      if lastDelim then
        begin
        ServerURI:='/'+GetToken(' ');

        tmp:=GetToken(' '); MaxConn:=StrToIntDef(tmp,0);
        tmp:=GetToken(' '); MaxSess:=StrToIntDef(tmp,0);
        tmp:=GetToken(' '); SessTimeout:=StrToIntDef(tmp,0);

        end
      else
        ServerPort:='';
      end
    else
      ServerAddr:='';
    end
  else
    VirtualHost:='';
  end;

function TRtcLoadBalancerMainForm.ServerLineFromInfo(VirtualHost, VirtualPath,
    ServerAddr, ServerPort, ServerURI: RtcString;
    MaxConn, MaxSess, SessTimeout,
    AppType, BalanceType, RequestOrder: integer): RtcString;
  begin
  if (ServerAddr='') or (ServerPort='') then
    Result:=''
  else
    begin
    if Copy(VirtualPath,1,1)<>'/' then VirtualPath:='/'+VirtualPath;
    if Copy(ServerURI,1,1)<>'/' then ServerURI:='/'+ServerURI;
    Result:=VirtualHost+VirtualPath+' '+
            IntToStr(AppType)+' '+IntToStr(BalanceType)+' '+IntToStr(RequestOrder)+' '+
            ServerAddr+':'+ServerPort+ServerURI+' '+
            IntToStr(MaxConn)+' '+IntToStr(MaxSess)+' '+IntToStr(SessTimeout);
    end;
  end;

function TRtcLoadBalancerMainForm.SetupBalancer(const MyVirtualHost, MyVirtualPath: RtcString;
    MyAppType, MyBlanceType, MyReqOrder: integer): TRtcLoadBalancer;
  var
    a:integer;
  begin
  Result:=nil;

  for a:=0 to length(Balancers)-1 do
    if ( Upper_Case(Balancers[a].VirtualHost) = Upper_Case(MyVirtualHost) ) and
       ( Upper_Case(Balancers[a].VirtualPath) = Upper_Case(MyVirtualPath) ) then
    Result:=Balancers[a];

  if not assigned(Result) then
    begin
    Result := TRtcLoadBalancer.Create(nil);
    SetLength(Balancers, length(Balancers)+1);
    Balancers[length(Balancers)-1]:= Result;

    Result.Server:=Server;
    Result.CheckOrder := 10+length(Balancers);

    Result.VirtualHost:=MyVirtualHost;
    Result.VirtualPath:=MyVirtualPath;

    Result.BalanceType:=TRtcLoadBalanceType(MyBlanceType);
    Result.RequestOrder:=TRtcLoadBalanceOrder(MyReqOrder);

    { All of these Settings could be different for each Web Application, but we will
      use a single Setup here to simplify the configuration process in this Demo. }
    with Result do
      begin
      PostReturnBeforeResponseSent:=xPostReturnBeforeResponseSent.Checked;
      RequestBuffer:=xRequestBuffer.Checked;
      ResponseBuffer:=xResponseBuffer.Checked;

      if xDebugLog.Checked then
        OnDebugLog:=LoadBalancer_DebugLog
      else
        OnDebugLog:=nil;

      PrepareConnection:=LoadBalancer_PrepareConnection;

      case MyAppType of
        1:begin // RTC using Auto-Sessions
          RequestPrepareSessionID:=RTCApp_RequestPrepareSessionID;
          ResponsePrepareSessionID:=RTCApp_ResponsePrepareSessionID;
          end;
        2:begin // IntraWeb using hidden Sessions
          RequestBodyHasSessionID:=IntraWebApp_RequestBodyHasSessionID;
          RequestPrepareSessionID:=IntraWebApp_RequestPrepareSessionID;
          ResponseBodyHasSessionID:=IntraWebApp_ResponseBodyHasSessionID;
          ResponsePrepareSessionID:=IntraWebApp_ResponsePrepareSessionID;
          end;
        3:begin // IntraWeb using hidden Sessions + absolute paths
          RequestBodyHasSessionID:=IntraWebApp_RequestBodyHasSessionID;
          RequestPrepareSessionID:=IntraWebApp_RequestPrepareSessionID;
          ResponseBodyHasSessionID:=IntraWebApp_ResponseBodyHasSessionID;
          ResponsePrepareSessionID:=IntraWebApp_ResponsePrepareSessionID;
          OnResponseBeginO:=IntraWeb_ResponseBegin;
          OnResponseReceivedO:=IntraWeb_ResponseReceived;
          end;
        end;

      if xRequestInTimeouts.Checked then
        begin
        TimeoutAfterCheckRequestI:=5+eRequestInTime.Value*4;
        TimeoutAfterPostNewRequestI:=5+eRequestInTime.Value;
        TimeoutAfterPostOldRequestI:=5+eRequestInTime.Value;
        TimeoutAfterQueuedRequestI:=5+eRequestInTime.Value*4;
        TimeoutAfterRequestDataIn:=eRequestInTime.Value;
        TimeoutAfterRequestReceivedI:=-1; // Disable Timeouts while waiting for a Response
        end
      else
        begin
        TimeoutAfterCheckRequestI:=0;
        TimeoutAfterPostNewRequestI:=0;
        TimeoutAfterPostOldRequestI:=0;
        TimeoutAfterQueuedRequestI:=0;
        TimeoutAfterRequestDataIn:=0;
        TimeoutAfterRequestReceivedI:=0;
        end;

      if xRequestOutTimeout.Checked then
        begin
        TimeoutAfterRequestBeginO:=5+eResponseOutTime.Value*2;
        TimeoutAfterRequestDataOut:=eResponseOutTime.Value;
        TimeoutAfterRequestSentO:=-1; // Disable Timeouts while waiting for a Response
        end
      else
        begin
        TimeoutAfterRequestBeginO:=0;
        TimeoutAfterRequestDataOut:=0;
        TimeoutAfterRequestSentO:=0;
        end;

      if xResponseInTimeout.Checked then
        begin
        TimeoutAfterResponseBeginO:=5+eResponseInTime.Value*2;
        TimeoutAfterResponseDataIn:=eResponseInTime.Value;
        TimeoutAfterResponseReceivedO:=-1; // Disable Timeouts after receiving a Response
        end
      else
        begin
        TimeoutAfterResponseBeginO:=0;
        TimeoutAfterResponseDataIn:=0;
        TimeoutAfterResponseReceivedO:=0;
        end;

      if xResponseOutTimeout.Checked then
        begin
        TimeoutAfterResponseDataOut:=eResponseOutTime.Value;
        TimeoutAfterResponseSentI:=-1; // Disable timeouts after sending a Response
        end
      else
        begin
        TimeoutAfterResponseDataOut:=0;
        TimeoutAfterResponseSentI:=0;
        end;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.CreateBalancers;
  var
    lin:RtcString;
    i:integer;
    tmpHost,tmpPath,
    tmpAddr,tmpPort,tmpUri:RtcString;
    tmpConn,tmpSess,tmpTim,
    tmpApp,tmpBal,tmpReq:integer;
    MyBalancer:TRtcLoadBalancer;
  begin
  for i:=0 to eServerList.Lines.Count-1 do
    begin
    lin:=eServerList.Lines[i];
    ServerInfoFromLine(lin,tmpHost,tmpPath,tmpAddr,tmpPort,tmpUri,
                           tmpConn,tmpSess,tmpTim,
                           tmpApp,tmpBal,tmpReq);
    if (tmpAddr<>'') and (tmpPort<>'') then
      begin
      MyBalancer:=SetupBalancer(tmpHost,tmpPath,tmpApp,tmpBal,tmpReq);
      MyBalancer.AddServer(tmpAddr,tmpPort,tmpUri,
                           tmpConn,tmpSess,tmpTim/24/60/60,
                           GetRtcIPV(xClientIP6.Checked));
      end;
    end;
  BalancersActive:=True;
  end;

procedure TRtcLoadBalancerMainForm.DestroyBalancers;
  var
    a,i:integer;
  begin
  if not BalancersActive then Exit;

  BalancersActive:=False;

  for i:=0 to length(Balancers)-1 do
    Balancers[i].Active:=False;

  try
    { This method is used for closing all connections and destroying all
      TRtcDataRequest and TRtcHttpClient components used with the TRtcDataRouter component. }

    { We want to dump all buffers here, so any errors can be
      spotted in case this method should not complete as expected. }
    if xBuffLog.Checked then StopLogBuffers;

    Log('CloseClient begin','main');

    Log('  *** Closing '+IntToStr(rtcServerConnectionCount)+' incoming connections ...','main');
    Server.StopListenNow;
    Log('  * All incoming connections closed.','main');

    a := 0;
    for i:=0 to length(Balancers)-1 do
      a:=a+Balancers[i].ActiveClients;

    if a>0 then
      begin
      Log('  *** Waiting for '+Int2Str(a)+' requests to become idle ...','main');
      repeat
        Application.ProcessMessages;
        Sleep(10);
        a := 0;
        for i:=0 to length(Balancers)-1 do
          a:=a+Balancers[i].ActiveClients;
        until a=0;
      Log('  * All requests are now idle.','main');
      end;

    { From here on, all connections should be idle
      and no new connections should arrive ... }

    a := 0;
    for i:=0 to length(Balancers)-1 do
      a:=a+Balancers[i].IdleClients;

    if a>0 then
      begin
      Log('  *** Closing '+IntToStr(a)+' outgoing connections ...','main');

      for i:=0 to length(Balancers)-1 do
        Balancers[i].ClosePool;

      while rtcClientConnectionCount>0 do
        begin
        Application.ProcessMessages;
        Sleep(10);
        end;
      Log('  * All outgoing connections closed.','main');

      Sleep(1000);
      end;

    Log('  *** Releasing Connection Pools ...','main');
    for i:=0 to length(Balancers)-1 do
      Balancers[i].ReleasePool;

    Log('  *** Releasing Load Balancer objects ...','main');
    for i:=0 to length(Balancers)-1 do
      begin
      Balancers[i].Server:=nil;
      Balancers[i].Free;
      end;

    SetLength(Balancers,0);

    Log('  . All objects released.','main');

    Log('CloseClient end.','main');
  except
    on E:Exception do
      begin
      Log('TForm1.DestroyBalancers',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.UpdateServerDetails;
  var
    MyLine:integer;
    lin:RtcString;
    tmpHost,tmpPath,
    tmpAddr,tmpPort,tmpUri:RtcString;
    tmpConn,tmpSess,tmpTim,
    tmpApp,tmpBal,tmpReq:integer;
  begin
  With eServerList do
    MyLine := Perform(EM_LINEFROMCHAR, SelStart, 0);
  if MyLine>=0 then
    begin
    lin:=eServerList.Lines[MyLine];
    ServerInfoFromLine(lin,tmpHost,tmpPath,tmpAddr,tmpPort,tmpUri,
                           tmpConn,tmpSess,tmpTim,tmpApp,tmpBal,tmpReq);
    eVirtualHost.Text:=tmpHost;
    eVirtualPath.Text:=tmpPath;
    eServerAddr.Text:=tmpAddr;
    eServerPort.Text:=tmpPort;
    eServerURI.Text:=tmpUri;
    eMaxConn.Value:=tmpConn;
    eMaxSess.Value:=tmpSess;
    eSessTimeout.Value:=tmpTim;
    cBalanceType.ItemIndex:=tmpBal;
    cReqOrder.ItemIndex:=tmpReq;
    cAppType.ItemIndex:=tmpApp;
    end;
  end;

procedure TRtcLoadBalancerMainForm.UpdateServerList;
  var
    MyLine:integer;
    lin:RtcString;
    tmpHost,tmpPath,
    tmpAddr,tmpPort,tmpUri:RtcString;
    tmpConn,tmpSess,tmpTim,
    tmpApp,tmpBal,tmpReq:integer;
  begin
  With eServerList do
    MyLine := Perform(EM_LINEFROMCHAR, SelStart, 0);
  if MyLine>=0 then
    begin
    tmpHost:=eVirtualHost.Text;
    tmpPath:=eVirtualPath.Text;
    tmpAddr:=eServerAddr.Text;
    tmpPort:=eServerPort.Text;
    tmpUri:=eServerURI.Text;
    tmpConn:=eMaxConn.Value;
    tmpSess:=eMaxSess.Value;
    tmpTim:=eSessTimeout.Value;
    tmpApp:=cAppType.ItemIndex;
    tmpBal:=cBalanceType.ItemIndex;
    tmpReq:=cReqOrder.ItemIndex;
    lin:=ServerLineFromInfo(tmpHost,tmpPath,tmpAddr,tmpPort,tmpUri,
                            tmpConn,tmpSess,tmpTim,
                            tmpApp,tmpBal,tmpReq);
    eServerList.Lines[MyLine]:=lin;
    end;
  end;

(*************************************************)
(*                                               *)
(*   "/$tat" and "/$dump" Data Provider events   *)
(*                                               *)
(*************************************************)

procedure TRtcLoadBalancerMainForm.StatProviderCheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/$tat' then
      Accept;
  end;

procedure TRtcLoadBalancerMainForm.StatProviderDataReceived(Sender: TRtcConnection);
  var
    i:integer;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      Response.ContentType:='text/html';
      Write('<html><body>');

      Write('Connections to all Servers: '+Int2Str(rtcClientConnectionCount)+'<br>');
      Write('Connections from all Clients: '+Int2Str(rtcServerConnectionCount)+'<br><br>');

      if BalancersActive then
        for i:=0 to length(Balancers)-1 do
          begin
          Write('WebApp ('+IntToStr(i)+') <b>'+Balancers[i].VirtualHost+Balancers[i].VirtualPath+'</b>'+
                ' : Active='+Int2Str(Balancers[i].ActiveClients)+
                ', Idle='+Int2Str(Balancers[i].IdleClients)+
                ', Sessions='+Int2Str(Balancers[i].ActiveSessions)+'<br>');
          Write(Balancers[i].GetDebugInfo+'<br>');
          Write(Balancers[i].GetBalanceDebugInfo+'<br><br>');
          end;

      Write('</body></html>');
      end;
  end;

procedure TRtcLoadBalancerMainForm.DumpProviderCheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/$dump' then
      Accept;
  end;

procedure TRtcLoadBalancerMainForm.DumpProviderDataReceived(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      DumpLogBuffers;
      Response.ContentType:='text/html';
      Write('<html><body>');
      Write('LOG entries written to files.');
      Write('</body></html>');
      end;
  end;

(***************************************************************)
(*                                                             *)
(*   Graphical User Interface (Load Balancer configuration)    *)
(*                                                             *)
(***************************************************************)

procedure TRtcLoadBalancerMainForm.FormCreate(Sender: TObject);
  begin
// Used for debugging purposes ...

{$IFDEF DEBUG_SSL}
  // Buffer used by WinSock to buffer outgoing data
  SOCK_SEND_BUFFER_SIZE:=1024;
  // Buffer used by WinSock to buffer incoming data
  SOCK_READ_BUFFER_SIZE:=1024;

  // Max packet size sent out at once
  SOCK_MAX_SEND_SIZE:=1024;
  // Max packet size read at once
  SOCK_MAX_READ_SIZE:=1024;
{$ENDIF}

  Randomize;

  StartLog;
  xBuffLogClick(xBuffLog);

  eServerList.SelLength:=0;
  eServerList.SelStart:=0;
  UpdateServerDetails;
  end;

procedure TRtcLoadBalancerMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  bConnect.Enabled:=false;

  DestroyBalancers;
  end;

procedure TRtcLoadBalancerMainForm.bConnectClick(Sender: TObject);
  begin
  bConnect.Enabled:=False;
  try
    if not Server.isListening then
      begin
      // Enable log buffers if "LOG buffers" was checked (dumps old buffers and starts fresh)
      if xBuffLog.Checked then xBuffLogClick(nil);

      RTC_THREAD_POOL_MAX:=eThrCount.Value;

      Randomize;

      Server.MultiThreaded:=xServerMulti.Checked;
      Server.Blocking:=xServerBlocking.Checked;
      Server.ServerPort:=RtcString(eFromPort.Text);
      Server.ServerIPV:=GetRtcIPV(xServerIP6.Checked);

      FCliMultiThreaded:=xClientMulti.Checked;
      FCliBlocking:=xClientBlocking.Checked;
      FCliFixupRequest:=xForceHttp10.Checked;

      CreateBalancers;

      Server.Listen;
      bConnect.Caption:='STOP';
      end
    else
      begin
      bConnect.Enabled:=False;

      DestroyBalancers;

      bConnect.Enabled:=True;
      bConnect.Caption:='START';
      end;
  finally
    bConnect.Enabled:=True;
    end;
  end;

procedure TRtcLoadBalancerMainForm.eLogFolderChange(Sender: TObject);
  begin
  if eLogFolder.Text<>'' then
    RTC_LOG_FOLDER:=IncludeTrailingPathDelimiter(eLogFolder.Text)
  else
    RTC_LOG_FOLDER:='';
  end;

procedure TRtcLoadBalancerMainForm.xBuffLogClick(Sender: TObject);
  begin
  if xBuffLog.Checked then
    StartLogBuffers(128000000) // 128 MB buffers
  else
    StopLogBuffers;
  end;

procedure TRtcLoadBalancerMainForm.bDumpLogClick(Sender: TObject);
  begin
  DumpLogBuffers;
  end;

procedure TRtcLoadBalancerMainForm.eServerListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
  UpdateServerDetails;
  end;

procedure TRtcLoadBalancerMainForm.eServerListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  UpdateServerDetails;
  end;

procedure TRtcLoadBalancerMainForm.eServerDetailsChange(Sender: TObject);
  begin
  if Sender=ActiveControl then
    UpdateServerList;
  end;

procedure TRtcLoadBalancerMainForm.bAddServerClick(Sender: TObject);
  var
    VirtualHost,
    VirtualPath:RtcString;
    AppType, BalType, ReqOrder:integer;
  begin
  VirtualHost:=eVirtualHost.Text;
  VirtualPath:=eVirtualPath.Text;
  AppType:=cAppType.ItemIndex;
  BalType:=cBalanceType.ItemIndex;
  ReqOrder:=cReqOrder.ItemIndex;

  eServerList.Lines.Add('');
  eServerList.SelLength := 0;
  eServerList.SelStart := length(eServerList.Lines.Text);

  UpdateServerDetails;

  eVirtualHost.Text:=VirtualHost;
  eVirtualPath.Text:=VirtualPath;
  cAppType.ItemIndex:=AppType;
  cBalanceType.ItemIndex:=BalType;
  cReqOrder.ItemIndex:=ReqOrder;

  eServerAddr.SetFocus;
  end;

procedure TRtcLoadBalancerMainForm.bAddWebAppClick(Sender: TObject);
  begin
  eServerList.Lines.Add('');
  eServerList.SelLength := 0;
  eServerList.SelStart := length(eServerList.Lines.Text);
  UpdateServerDetails;
  eVirtualHost.SetFocus;
  end;

procedure TRtcLoadBalancerMainForm.ServerRequestNotAccepted(Sender: TRtcConnection);
  begin
  Sender.Disconnect;
  end;

procedure TRtcLoadBalancerMainForm.xSSLClick(Sender: TObject);
  begin
  if xSSL.Checked then
    begin
  {$IFDEF StreamSecII}
    // Set to false to disable RC4 and enable AES-CBC.
    // ExpectOldBrowsers := false;

      //  Files will be IGNORED if they do not exist.
    AddServerRootCertFile(ExtractFilePath(AppFileName) + 'root.cer');
    AddServerPFXFile(ExtractFilePath(AppFileName) + 'server.pfx', 'abc');
    try
      Server.CryptPlugin := GetServerCryptPlugin;
    except
      On E: Exception do XLog(E.Message);
      end;

    eFromPort.Text:='443';
  {$ELSE}
    xSSL.Checked:=False;
  {$ENDIF}
    end
  else
    begin
    Server.CryptPlugin:=nil;
    eFromPort.Text:='80';
    end;
  end;

end.
