{
  @html(<b>)
  RTC Load Balancer Demo Project
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This Project shows how to use the TRtcLoadBalancer component
  with the RealThinClient SDK to write a HTTP Load Balancer.
}

unit MainUnit2;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls,

  rtcTypes, rtcSystem, rtcConn,
  rtcDataCli, rtcDataSrv,
  rtcHttpCli, rtcHttpSrv,
  rtcInfo, rtcLog,
  rtcThrPool,
  rtcDataRoute, rtcLoadBalance;

type
  TRtcLoadBalancerMainForm = class(TForm)
    Server: TRtcHttpServer;
    bConnect: TButton;
    eFromPort: TEdit;
    Label4: TLabel;
    xServerMulti: TCheckBox;
    xServerBlocking: TCheckBox;
    xClientMulti: TCheckBox;
    xClientBlocking: TCheckBox;
    xChangeHost: TCheckBox;
    xChangeURLs: TCheckBox;
    xForceHttp10: TCheckBox;
    xResponseBuffer: TCheckBox;
    xDebugLog: TCheckBox;
    xRequestBuffer: TCheckBox;
    eLogFolder: TEdit;
    Label7: TLabel;
    DataBalancer: TRtcLoadBalancer;
    StatProvider: TRtcDataProvider;
    cReqOrder: TRadioGroup;
    xEventLog: TCheckBox;
    eToURI: TEdit;
    Label8: TLabel;
    xBuffLog: TCheckBox;
    bDumpLog: TButton;
    xRequestInTimeouts: TCheckBox;
    xResponseOutTimeout: TCheckBox;
    xRequestOutTimeout: TCheckBox;
    xResponseInTimeout: TCheckBox;
    Bevel2: TBevel;
    eRequestInTime: TSpinEdit;
    Label9: TLabel;
    eResponseOutTime: TSpinEdit;
    Label12: TLabel;
    eResponseInTime: TSpinEdit;
    Label14: TLabel;
    eRequestOutTime: TSpinEdit;
    Label16: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    DumpProvider: TRtcDataProvider;
    eThrCount: TSpinEdit;
    Label3: TLabel;
    xPostReturnBeforeResponseSent: TCheckBox;
    Label5: TLabel;
    Label10: TLabel;
    Bevel5: TBevel;
    eServerList: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    cBalanceType: TRadioGroup;
    xServerIP6: TCheckBox;
    xClientIP6: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bConnectClick(Sender: TObject);
    procedure eLogFolderChange(Sender: TObject);
    procedure DataBalancerCheckRequestI(Sender: TRtcDataServer);
    procedure DataBalancerRequestBeginO(Sender: TRtcDataClient);
    procedure DataBalancerResponseBeginO(Sender: TRtcDataClient);
    procedure DataBalancerPostReturn(DataRequest: TRtcDataRequest);
    procedure DataBalancerRequestSentO(Sender: TRtcDataClient);
    procedure DataBalancerResponseSentI(Sender: TRtcDataServer);
    procedure DataBalancerRequestReceiveAbortI(Sender: TRtcDataServer);
    procedure DataBalancerRequestSendAbortO(Sender: TRtcDataClient);
    procedure DataBalancerResponseReceiveAbortO(Sender: TRtcDataClient);
    procedure DataBalancerResponseSendAbortI(Sender: TRtcDataServer);
    procedure StatProviderCheckRequest(Sender: TRtcConnection);
    procedure StatProviderDataReceived(Sender: TRtcConnection);
    procedure DataBalancerPostNewRequestI(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
    procedure DataBalancerPostOldRequestI(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
    procedure eToURIExit(Sender: TObject);
    procedure DataBalancerDebugLog(Info: TRtcRouterDebugInfo);
    procedure DataBalancerResponseReceivedO(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
    procedure DataBalancerRequestReceivedI(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
    procedure xBuffLogClick(Sender: TObject);
    procedure bDumpLogClick(Sender: TObject);
    procedure DumpProviderCheckRequest(Sender: TRtcConnection);
    procedure DumpProviderDataReceived(Sender: TRtcConnection);
    procedure xPostReturnBeforeResponseSentClick(Sender: TObject);
    procedure DataBalancerPrepareConnection(Sender: TRtcDataClient);
    procedure DataBalancerRequestBodyHasSessionID(Sender: TRtcDataServer; var NeedContent: Boolean);
    procedure DataBalancerRequestPrepareSessionID(Sender: TRtcDataServer; Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
    procedure DataBalancerResponseBodyHasSessionID(Sender: TRtcDataClient; var NeedContent: Boolean);
    procedure DataBalancerResponsePrepareSessionID(Sender: TRtcDataClient; Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
    { Private declarations }
  public
    { Public declarations }
    FChangeHost:boolean;
    FToURI:RtcString;
    FChangeURLs:boolean;
    FEventLog:boolean;

    FCliMultiThreaded:boolean;
    FCliBlocking:boolean;
    FCliFixupRequest:boolean;

    procedure CreatePool;
    procedure CloseClients;

    procedure LogStatus(Sender:TRtcDataServer; const Stat:RtcString); overload;
    procedure LogStatus(Sender:TRtcDataClient; const Stat:RtcString); overload;
    procedure LogStatus(const Stat:RtcString); overload;
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

procedure TRtcLoadBalancerMainForm.DataBalancerRequestBodyHasSessionID(Sender: TRtcDataServer; var NeedContent: Boolean);
  begin
(* RTC AppClient+AppServer demo *)
  if Sender.Request.FileName='/TEST' then
    NeedContent:=False
  else
(* Comicino Web App *)
    NeedContent := Sender.Request.Method='POST';
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerRequestPrepareSessionID(Sender: TRtcDataServer;
    Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
  var
    s:RtcString;
  begin
(* Comicino Web App *)
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
  (* RTC AppClient+AppServer Demo *)
    if Sender.Request.FileName='/TEST' then
      begin
      Session.ID:=Sender.Request.Query.ValueCS['ID'];
      if length(Session.ID)<>32 then
        Session.ID:='';
      end
    else
  (* Comicino Web App *)
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
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerResponseBodyHasSessionID(Sender: TRtcDataClient; var NeedContent: Boolean);
  begin
(* Comicino Web App *)
  // When using Hidden Sessions, SessionID will be received in the "/" or "/EXE" HTML page
  if (Sender.Request.FileName='/') or
     (Upper_Case(Sender.Request.FileName)='/EXEC') then
    NeedContent := Copy(Sender.Response.ContentType,1,9)='text/html';
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerResponsePrepareSessionID(Sender: TRtcDataClient;
      Content: TRtcRouterContentBody; Session: TRtcLoadBalancerSession);
  var
    loc:integer;
  begin
(* RTC AppClient+AppServer Demo *)
  if Sender.Request.FileName='/TEST' then
    begin
    Session.ID:=Sender.Response.Cookie.ValueCS['ID'];
    if length(Session.ID)=32 then
      Session.Close := Sender.Response.StatusCode=410
    else
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
      else
        Session.ID:='';
      end;
    end
  else // ->
(* Comicino Web App *)
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

(*************************************************************************************************)
(*                                                                                               *)
(*   Methods and Events required to implement a Load Balancer (mostly Application-independent)   *)
(*                                                                                               *)
(*************************************************************************************************)

{ TRtcDataRouter methods and events }

procedure TRtcLoadBalancerMainForm.DataBalancerPrepareConnection(Sender: TRtcDataClient);
  begin
  if Sender is TRtcHttpClient then
    with Sender as TRtcHttpClient do
      begin
      MultiThreaded:=FCliMultiThreaded;
      Blocking:=FCliBlocking;
      FixupRequest.ForceOldHttp10:=FCliFixupRequest;
      end;
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerCheckRequestI(Sender: TRtcDataServer);
  begin
  try
    Sender.Accept;
    { If all Requests are Accepted (like in this example), then all Requests will be processed which weren't
      processed using other rtcDataProvider or rtcDataRouter or rtcServerModule components linked to the same
      rtcHttpServer (meaning all components which have a "higher CheckOrder position" / "smaller CheckOrder value"
      like for example the "StartProvider" component which has "CheckOrder=0" while DataBalancer has "CheckOrder=10"). }

    { For Requests which we have accepted (all requests in this example), we can modify Request Headers here ... }

    { We should also set "Sender.Request.ManualRead" to TRUE if we do NOT intend
      to modify Request Content Body before it is forwarded to our Server (outgoing connection). }
    Sender.Request.ManualRead:=True;

    LogStatus(Sender,'OnCheckRequest');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterCheckRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerRequestBeginO(Sender: TRtcDataClient);
  begin
  try
    LogStatus(Sender,'OnRequestBegin');

    if FChangeHost then
      Sender.Request.Host:=Sender.ServerAddr;

    if FChangeURLs then
      if Sender.Request.Referer<>'' then
        if UpperCase(Copy(Sender.Request.Referer,1,8+9))='HTTP://LOCALHOST/' then
          Sender.Request.Referer:=RtcString(StringReplace(String(Sender.Request.Referer),'http://localhost/',
                                                           String('http://'+Sender.ServerAddr+'/'),[rfReplaceAll,rfIgnoreCase]));
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterRequestBegin',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerResponseBeginO(Sender: TRtcDataClient);
  begin
  try
    LogStatus(Sender,'OnResponseBegin');

    if FChangeHost then
      begin
      if Sender.Response['LOCATION']<>'' then
        Sender.Response['LOCATION']:=RtcString(StringReplace(String(Sender.Response['LOCATION']),
                                                              String('http://'+Sender.ServerAddr+FToURI+'/'),'/',[rfReplaceAll,rfIgnoreCase]));
      if Sender.Response['REFRESH']<>'' then
        Sender.Response['REFRESH']:=RtcString(StringReplace(String(Sender.Response['REFRESH']),
                                                             String('http://'+Sender.ServerAddr+FToURI+'/'),'/',[rfReplaceAll,rfIgnoreCase]));
      end;

    { To enable Response Content buffering for this particular Response, set "Sender.Response.ManualRead" to FALSE.

      To *disable* Response Content buffering for this particular Response and thus allow the TRtcDataRouter
      component to forward the Content directly to the Client, set "Sender.Response.ManualRead" to TRUE. }

    if FChangeURLs and
       (Copy(UpperCase(Sender.Response.ContentType),1,5)='TEXT/') and
       (UpperCase(Sender.Response['CONTENT-ENCODING'])<>'GZIP') then
      Sender.Response.ManualRead:=False; // ENABLE Response Content Buffering (NOT recommended for long responses!!!!)
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterResponseBegin',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerResponseReceivedO(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
  var 
    Body:RtcString;
  begin
  try
    if assigned(Content) then // Response Content Buffering was enabled in "DataRouterResponseBegin" event
      begin
      LogStatus(Sender,'OnResponseReceived Body=' + Int2Str(length(Content.Body)) );

      if FChangeURLs and (length(Content.Body)>0) then
        if (Copy(UpperCase(Sender.Response.ContentType),1,5)='TEXT/') and
           (UpperCase(Sender.Response['CONTENT-ENCODING'])<>'GZIP') then
          begin
          Body:=StringReplace(Content.Body,'http://'+Sender.ServerAddr+FToURI+'"','/"',[rfReplaceAll,rfIgnoreCase]);
          Body:=StringReplace(Body,'http://'+Sender.ServerAddr+FToURI+'/','/' ,[rfReplaceAll,rfIgnoreCase]);
          Body:=StringReplace(Body,'http://'+Sender.ServerAddr+FToURI+'<','/<',[rfReplaceAll,rfIgnoreCase]);
          Body:=StringReplace(Body,'http://'+Sender.ServerAddr+FToURI+' ','/ ',[rfReplaceAll,rfIgnoreCase]);
          Content.Body:=StringReplace(Body,'http://'+Sender.ServerAddr+FToURI+#13,'/'#13,[rfReplaceAll,rfIgnoreCase]);
          end;
      end
    else // Response Content Buffering was disabled
      LogStatus(Sender,'OnResponseReceived');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterResponseReceived',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerPostNewRequestI(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
  begin
  if assigned(DataRequest) then
    LogStatus(Sender,'POST NEW Request')
  else
    LogStatus(Sender,'QUEUE NEW Request');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerPostOldRequestI(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
  begin
  if assigned(DataRequest) then
    LogStatus(Sender,'POST OLD Request')
  else
    LogStatus(Sender,'QUEUE OLD Request');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerPostReturn(DataRequest: TRtcDataRequest);
  begin
  if assigned(DataRequest) then
    LogStatus('POST Return')
  else
    LogStatus('POST NEXT Request');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerRequestReceiveAbortI(Sender: TRtcDataServer);
  begin
  LogStatus(Sender,'!!! OnRequestReceiveAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerRequestReceivedI(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
  begin
  if assigned(Content) then
    LogStatus(Sender,'OnRequestReceived Body='+Int2Str(length(Content.Body)))
  else
    LogStatus(Sender,'OnRequestReceived');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerRequestSendAbortO(Sender: TRtcDataClient);
  begin
  LogStatus(Sender,'!!! OnRequestSendAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerRequestSentO(Sender: TRtcDataClient);
  begin
  LogStatus(Sender,'OnRequestSent');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerResponseReceiveAbortO(Sender: TRtcDataClient);
  begin
  LogStatus(Sender,'!!! OnResponseReceiveAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerResponseSendAbortI(Sender: TRtcDataServer);
  begin
  LogStatus(Sender,'!!! OnResponseSendAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerResponseSentI(Sender: TRtcDataServer);
  begin
  LogStatus(Sender,'OnResponseSent');
  end;

procedure TRtcLoadBalancerMainForm.DataBalancerDebugLog(Info: TRtcRouterDebugInfo);
  begin
  Log(Info.Text,Info.Name);
  end;

(***********************************************************)
(*                                                         *)
(*   Logging Methods used by Load Balancer events (above)  *)
(*                                                         *)
(***********************************************************)

procedure TRtcLoadBalancerMainForm.LogStatus(Sender: TRtcDataServer; const Stat: RtcString);
  begin
  if not FEventLog then Exit;
  Log('['+Int2Str(DataBalancer.ActiveClients)+'+'+Int2Str(DataBalancer.IdleClients)+'] '+Sender.PeerAddr+':'+Sender.PeerPort+
      ' > '+Stat+' Len='+Int2Str(Sender.Request.ContentIn)+'/'+Int2Str(Sender.Response.ContentOut)+
      ' '+Sender.Request.URI,'main');
  end;

procedure TRtcLoadBalancerMainForm.LogStatus(Sender: TRtcDataClient; const Stat: RtcString);
  begin
  if not FEventLog then Exit;
  Log('['+Int2Str(DataBalancer.ActiveClients)+'+'+Int2Str(DataBalancer.IdleClients)+'] '+Sender.PeerAddr+':'+Sender.PeerPort+
      ' > '+Stat+' Len='+Int2Str(Sender.Request.ContentOut)+'/'+Int2Str(Sender.Response.ContentIn)+
      ' "'+Int2Str(Sender.Response.StatusCode)+' '+Sender.Response.StatusText+'" '+Sender.Request.URI,'main');
  end;

procedure TRtcLoadBalancerMainForm.LogStatus(const Stat: RtcString);
  begin
  if not FEventLog then Exit;
  Log('['+Int2Str(DataBalancer.ActiveClients)+'+'+Int2Str(DataBalancer.IdleClients)+'] > '+Stat,'main');
  end;

(***********************************************************)
(*                                                         *)
(*   Methots used to Prepare and Stop the Load Balancer    *)
(*                                                         *)
(***********************************************************)

procedure TRtcLoadBalancerMainForm.CreatePool;
  var
    lin,txt:RtcString;
    tmpAddr,
    tmpPort,
    tmpCon,
    tmpSes,
    tmpSesTim:RtcString;
    l:integer;

  function GetToken(delim:RtcChar):RtcString;
    var
      i:integer;
    begin
    i:=PosEx(delim,lin);
    if i>0 then
      begin
      Result:=Trim(Copy(lin,1,i-1));
      Delete(lin,1,i);
      lin:=Trim(lin);
      end
    else
      begin
      Result:=Trim(lin);
      lin:='';
      end;
    end;

  begin
  txt:=RtcString(eServerList.Text);
  repeat
    l:=PosEx(#13#10,txt);
    if l>0 then
      begin
      lin:=Copy(txt,1,l-1);
      Delete(txt,1,l+1);
      txt:=Trim(txt);
      end
    else
      begin
      lin:=Trim(txt);
      txt:='';
      end;
    tmpAddr:=GetToken(':');
    if tmpAddr<>'' then
      begin
      tmpPort:=GetToken(' ');
      tmpCon:=GetToken(' ');
      tmpSes:=GetToken(' ');
      tmpSesTim:=GetToken(' ');
      DataBalancer.AddServer(tmpAddr, tmpPort, FToURI,
                             StrToIntDef(tmpCon,0),
                             StrToIntDef(tmpSes,0),
                             StrToIntDef(tmpSesTim,0)/24/60/60,
                             GetRtcIPV(xClientIP6.Checked));
      end;
    until txt='';
  end;

procedure TRtcLoadBalancerMainForm.CloseClients;
  var
    a:integer;
  begin
  if not DataBalancer.Active then Exit;

  DataBalancer.Active:=False;

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

    a := DataBalancer.ActiveClients;
    if a>0 then
      begin
      Log('  *** Waiting for '+Int2Str(a)+' requests to become idle ...','main');
      repeat
        Application.ProcessMessages;
        Sleep(10);
        a := DataBalancer.ActiveClients;
        until a=0;
      Log('  * All requests are now idle.','main');
      end;

    { From here on, all connections should be idle
      and no new connections should arrive ... }

    if DataBalancer.IdleClients>0 then
      begin
      Log('  *** Closing '+IntToStr(DataBalancer.IdleClients)+' outgoing connections ...','main');
      DataBalancer.ClosePool;

      while rtcClientConnectionCount>0 do
        begin
        Application.ProcessMessages;
        Sleep(10);
        end;
      Log('  * All outgoing connections closed.','main');

      Sleep(1000);

      Log('  *** Releasing Connection Pool ...','main');
      DataBalancer.ReleasePool;
      Log('  . All connection objects released.','main');
      end
    else
      DataBalancer.ReleasePool;

    Log('CloseClient end.','main');
  except
    on E:Exception do
      begin
      Log('TForm1.CloseClients',E,'ERROR');
      raise;
      end;
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
    r_count,r_idle,r_sess:integer;
    details:RtcString;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      if DataBalancer.Active then
        begin
        r_count:=DataBalancer.ActiveClients;
        r_idle:=DataBalancer.IdleClients;
        r_sess:=DataBalancer.ActiveSessions;
        details:=DataBalancer.GetBalanceDebugInfo;
        end
      else
        begin
        r_count:=0;
        r_idle:=0;
        r_sess:=0;
        details:='';
        end;
      Response.ContentType:='text/html';
      Write('<html><body>');
      Write('Connections to all Servers: '+Int2Str(rtcClientConnectionCount)+
            ' Clients (Active='+Int2Str(r_count)+', Idle='+Int2Str(r_idle)+') / '+Int2Str(r_sess)+' Sessions<br>');
      Write('Connections from all Clients: '+Int2Str(rtcServerConnectionCount)+'<br>');
      Write('Router info: '+DataBalancer.GetDebugInfo+'<br>');
      Write('<br>');
      Write('Connections per Server ...<br>');
      Write(details);
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
  Randomize;
  StartLog;
  xBuffLogClick(xBuffLog);
  end;

procedure TRtcLoadBalancerMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  bConnect.Enabled:=false;

  if DataBalancer.Active then
    CloseClients;
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

      { Make sure to set ALL parameters
        before starting the listener ... }

      with DataBalancer do
        begin
        PostReturnBeforeResponseSent:=xPostReturnBeforeResponseSent.Checked;

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

      Server.MultiThreaded:=xServerMulti.Checked;
      Server.Blocking:=xServerBlocking.Checked;
      Server.ServerPort:=RtcString(eFromPort.Text);
      Server.ServerIPV:=GetRtcIPV(xServerIP6.Checked);

      FEventLog:=xEventLog.Checked;
      if xDebugLog.Checked then
        DataBalancer.OnDebugLog:=DataBalancerDebugLog
      else
        DataBalancer.OnDebugLog:=nil;

      DataBalancer.RequestOrder := TRtcLoadBalanceOrder(cReqOrder.ItemIndex);
      DataBalancer.BalanceType := TRtcLoadBalanceType(cBalanceType.ItemIndex);
      DataBalancer.RequestBuffer:=xRequestBuffer.Checked;
      DataBalancer.ResponseBuffer:=xResponseBuffer.Checked;

      FChangeHost:=xChangeHost.Checked;
      FChangeURLs:=xChangeURLs.Checked;
      FToURI:=RtcString(eToURI.Text);

      FCliMultiThreaded:=xClientMulti.Checked;
      FCliBlocking:=xClientBlocking.Checked;
      FCliFixupRequest:=xForceHttp10.Checked;
      
      CreatePool;

      DataBalancer.Active:=True;

      Server.Listen;
      bConnect.Caption:='STOP';
      end
    else
      begin
      bConnect.Enabled:=False;

      CloseClients;

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

procedure TRtcLoadBalancerMainForm.eToURIExit(Sender: TObject);
  begin
  if eToURI.Text<>'' then
    begin
    if eToURI.Text='/' then
      eToURI.Text:=''
    else if Copy(eToURI.Text,1,1)<>'/' then
      eToURI.Text:='/'+eToURI.Text;
    end;
  end;

procedure TRtcLoadBalancerMainForm.xPostReturnBeforeResponseSentClick(Sender: TObject);
  begin
  { It is safe to change this property while the Router is active }
  DataBalancer.PostReturnBeforeResponseSent:=xPostReturnBeforeResponseSent.Checked;
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

end.
