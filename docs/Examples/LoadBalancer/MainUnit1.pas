{
  @html(<b>)
  RTC Load Balancer Demo Project
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This Project shows how to use the TRtcDataRouter component
  with the RealThinClient SDK to write a HTTP Load Balancer.
}

unit MainUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls,

  rtcTypes, rtcConn,
  rtcDataCli, rtcDataSrv,
  rtcHttpCli, rtcHttpSrv,
  rtcInfo, rtcLog,
  rtcThrPool, rtcSystem,
  rtcDataRoute;

type
  TRtcClientConPool=class(TRtcClientPool)
  public
    FCliServerAddr:RtcString;
    FCliServerPort:RtcString;
    FCliServerIPV:RtcIPV;
    FCliMultiThreaded:boolean;
    FCliBlocking:boolean;
    FCliFixupRequest:boolean;

    SessionTimeout:TDateTime;
    MaxSessionCount,
    MaxReqCount:integer;

    function NewClient:TRtcDataClient; override;
    end;

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
    DataRouter: TRtcDataRouter;
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
    procedure DataRouterCheckRequest(Sender: TRtcDataServer);
    procedure DataRouterRequestBegin(Sender: TRtcDataClient);
    procedure DataRouterResponseBegin(Sender: TRtcDataClient);
    procedure DataRouterPostReturn(DataRequest: TRtcDataRequest);
    procedure DataRouterRequestSent(Sender: TRtcDataClient);
    procedure DataRouterResponseSent(Sender: TRtcDataServer);
    procedure FormDestroy(Sender: TObject);
    procedure DataRouterRequestReceiveAbort(Sender: TRtcDataServer);
    procedure DataRouterRequestSendAbort(Sender: TRtcDataClient);
    procedure DataRouterResponseReceiveAbort(Sender: TRtcDataClient);
    procedure DataRouterResponseSendAbort(Sender: TRtcDataServer);
    procedure StatProviderCheckRequest(Sender: TRtcConnection);
    procedure StatProviderDataReceived(Sender: TRtcConnection);
    procedure DataRouterPostNewRequest(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
    procedure DataRouterPostOldRequest(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
    procedure eToURIExit(Sender: TObject);
    procedure DataRouterQueuedRequest(Sender: TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean);
    procedure DataRouterDebugLog(Info: TRtcRouterDebugInfo);
    procedure DataRouterResponseReceived(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
    procedure DataRouterRequestReceived(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
    procedure xBuffLogClick(Sender: TObject);
    procedure bDumpLogClick(Sender: TObject);
    procedure DumpProviderCheckRequest(Sender: TRtcConnection);
    procedure DumpProviderDataReceived(Sender: TRtcConnection);
    procedure xPostReturnBeforeResponseSentClick(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    Req:TRtcMultiClientPool;
    ReqCS:TRtcCritSec;
    ReqClosing:boolean;

    FChangeHost:boolean;
    FToURI:RtcString;
    FChangeURLs:boolean;
    FRequestBuffer:boolean;
    FResponseBuffer:boolean;
    FEventLog:boolean;

    FLastIDX:integer;
    FReqOrder:integer;
    FBalanceType:integer;

    function FindDataRequestID(Sender:TRtcDataServer; var sid:RtcString; var AddToQueue:integer):integer;
    procedure GetDataRequest(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; const tip:RtcString);
    procedure GetDataRequestQueue(Sender:TRtcDataServer; var AddToQueue:integer);

    function NeedRequestContentBody(Sender:TRtcDataServer):boolean;
    procedure PrepareRequestSessionID(Sender:TRtcDataServer; Content:TRtcRouterContentBody);
    function GetRequestSessionID(Sender:TRtcDataServer):RtcString;

    function NeedResponseContentBody(Sender:TRtcDataClient):boolean;
    function GetResponseSessionID(Sender:TRtcDataClient; Content:TRtcRouterContentBody; var IsClosing:boolean):RtcString;

    procedure CreatePool;

    procedure LogStatus(Sender:TRtcDataServer; const Stat:RtcString); overload;
    procedure LogStatus(Sender:TRtcDataClient; const Stat:RtcString); overload;
    procedure LogStatus(const Stat:RtcString); overload;

    procedure CloseClients;
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

function TRtcLoadBalancerMainForm.NeedResponseContentBody(Sender: TRtcDataClient): boolean;
  begin
  { Return TRUE if you need the complete Response content body to find out the Session ID. }
  Result:=False;

(* Comicino Web App *)
  // When using Hidden Sessions, SessionID will be received in the "/" or "/EXE" HTML page
  if (Sender.Request.FileName='/') or
     (Upper_Case(Sender.Request.FileName)='/EXEC') then
    Result:= Copy(Sender.Response.ContentType,1,9)='text/html';
  end;

function TRtcLoadBalancerMainForm.GetResponseSessionID(Sender: TRtcDataClient; Content:TRtcRouterContentBody; var IsClosing:boolean): RtcString;
  var
    loc:integer;
  begin
  { Here, we have complete Request and Response headers. Using either headers, we should be able to
    find out if the Request/Response contains a SessionID and if it does, extract and return the SessionID.
    The method responsible for taking a DataRequest out of the Pool will also set "Sender.Info.asString['sid']"
    to the SessionID used to send the Request, so we can use it here to check if this Request has a SessionID.
    If the Session will be closed after this Request/Response, set "IsClosing" to TRUE to have it removed here also. }

(* RTC AppClient+AppServer Demo *)
  if Sender.Request.FileName='/TEST' then
    begin
    Result:=Sender.Response.Cookie.ValueCS['ID'];
    if length(Result)=32 then
      IsClosing:=Sender.Response.StatusCode=410
    else
      begin
      Result:=Sender.Request.Query.ValueCS['ID'];
      if length(Result)=32 then
        begin
        if Sender.Response.StatusCode=410 then
          IsClosing:=True // Session Expired
        else if Sender.Response.StatusCode=200 then
          IsClosing:=False // Update Session Timeout
        else
          Result:='';
        end
      else
        Result:='';
      end;
    end
  else // ->

(* Comicino Web App *)
  if assigned(Content) and
    ( (Sender.Request.FileName='/') or
      (Upper_Case(Sender.Request.FileName)='/EXEC') ) then
    begin
    // SessionID will be in a hidden variable "IW_SessionID_"
    Result:='name="IW_SessionID_" value="';

    loc:=Pos(Result,Content.Body);
    if loc>0 then
      begin
      // Session ID will be less than 40 characters, terminated with "
      Result:=Copy(Content.Body,loc+length(Result),40);
      loc:=Pos('"',Result);
      if loc>0 then
        begin
        Result:=Copy(Result,1,loc-1);
        // Check if this SessionID is available ...
        ReqCS.Acquire;
        try
          loc:=Req.HaveSession(Result);
          // If Session is already active, check if it's on THIS Server
          if (loc>=0) and (loc<>Req.GetPoolIndex(Sender)) then
            { When every "Server" entry is different, this should never happen. But, it is a good way
              to check if Session IDs are always correctly sent when using a single Server instance. }
            begin
            Content.Body:='<html><body>Session ID "'+Result+'" is already in use by another Server.</body></html>';
            Sender.Response.StatusCode:=500;
            Sender.Response.StatusText:='Load Balancing error';
            Result:='';
            end
          else if Sender.Info.asString['sid']<>'' then // We already have a SessionID?
            begin
            if Result<>Sender.Info.asString['sid'] then // We have had a different SessionID.
              begin
              { Something isn't right here.
                The Server should NOT simply switch SessionIDs. }
              Content.Body:='<html><body>Our Session ID is "'+Sender.Info.asString['sid']+'",'+
                            'but the Server has responsed with Session ID "'+Result+'".</body></html>';
              Sender.Response.StatusCode:=500;
              Sender.Response.StatusText:='Load Balancing error';
              Result:='';
              end;
            end;
        finally
          ReqCS.Release;
          end;
        end
      else
        Result:='';
      end
    else
      Result:='';

    // Not receiving a SessionID
    if Result='' then
      { If we are NOT receiving a SessionID or if we got a different SessionID,
        we should close the SessionID we have been using for this Request. }
      if Sender.Info.asString['sid']<>'' then
        begin
        { We have had a SessionID. Not receiving it back means the Session has expired. }
        Result:=Sender.Info.asString['sid'];
        IsClosing:=True;
        end;
    end;
  end;

function TRtcLoadBalancerMainForm.NeedRequestContentBody(Sender: TRtcDataServer): boolean;
  begin
  { If we need the complete Request Content Body to extract the SessionID,
    we have to return TRUE from this method. If we return TRUE here,
    "PrepareRequestSessionID" method will be called once the complete
    Request Content Body was received, giving us access to Content body. }

(* RTC AppClient+AppServer demo *)
  if Sender.Request.FileName='/TEST' then
    Result:=False
  else

(* Comicino Web App *)
  Result := Sender.Request.Method='POST';
  end;

procedure TRtcLoadBalancerMainForm.PrepareRequestSessionID(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
  var
    sid:RtcString;
  begin
  { This method will ONLY be called if a call to "NeedRequestContentBody" for this request returned TRUE.
    Here, we will get the complete Request Content Body in the "Content.Body" parameter, which we
    should use to extract the SessionID and store it inside the Request so it can be used from
    "GetRequestSessionID" when a decision has to be made to which Server the Request has to be sent. }

(* Comicino Web App *)
  if assigned(Content) and (Sender.Request.Method='POST') then
    begin
    { "Request.Params" can be used to extract parameters from
       "x-www-for-urlencoded" and "multipart/form-data" encoded content body. }
    Sender.Request.Params.AddText(Content.Body);

    sid:=Sender.Request.Params['IW_SessionID_'];
    if (length(sid)>20) and (length(sid)<40) then
      { We will store the SessionID to a temporary variable
        which can be accessed from "GetRequestSessionID" when needed. }
      Sender.Request.Info.asString['sid']:=sid;

    Sender.Request.Params.Clear;
    end;
  end;

function TRtcLoadBalancerMainForm.GetRequestSessionID(Sender: TRtcDataServer): RtcString;
  var
    s:RtcString;
  begin
  { Here, we only have Request headers. Using Request headers, we should be able to find out
    if the Request contains a SessionID and if it does, extract and return the SessionID.
    If this is not possible, use "NeedRequestContentBody" to mark Requests where the
    complete Request Content Body needs to be analyzed to extract the SessionID.  }

  Result:='';

(* RTC AppClient+AppServer Demo *)
  if Sender.Request.FileName='/TEST' then
    begin
    Result:=Sender.Request.Query.ValueCS['ID'];
    if length(Result)<>32 then
      Result:='';
    end
  else

(* Comicino Web App *)
  if (Sender.Request.Method='POST') then
    Result:=Sender.Request.Info.asString['sid'];

  if Result='' then
    begin
    s:=Upper_Case(Sender.Request.FilePath[0]);
    if s='CALLBACK' then
      begin
      Result:=Sender.Request.FilePath[Sender.Request.FilePath.Count-1];
      if length(Result)<20 then // SessionID will have at least 20 characters
        Result:='';
      end
    else if s='CACHE' then
      begin
      Result:=Sender.Request.FilePath[2];
      if length(Result)<20 then // SessionID will have at least 20 characters
        Result:='';
      end;
    end;
  end;

(*************************************************************************************************)
(*                                                                                               *)
(*   Methods and Events required to implement a Load Balancer (mostly Application-independent)   *)
(*                                                                                               *)
(*************************************************************************************************)

{ TRtcClientConPool methods }

function TRtcClientConPool.NewClient: TRtcDataClient;
  begin
  if (MaxReqCount>0) and (ActiveClients>=MaxReqCount) then
    Result:=nil
  else if (MaxSessionCount>0) and (ActiveSessions>=MaxSessionCount) then
    Result:=nil
  else
    begin
    Result:=TRtcHttpClient.Create(nil);
    with Result as TRtcHttpClient do
      begin
      ServerAddr:=FCliServerAddr;
      ServerPort:=FCliServerPort;
      ServerIPV:=FCliServerIPV;
      MultiThreaded:=FCliMultiThreaded;
      Blocking:=FCliBlocking;
      FixupRequest.ForceOldHttp10:=FCliFixupRequest;
      end;
    end;
  end;

{ TRtcDataRouter methods and events }

procedure TRtcLoadBalancerMainForm.GetDataRequestQueue(Sender:TRtcDataServer; var AddToQueue:integer);
  var
    idx:integer;
    sid:RtcString;
  begin
  if Sender<>nil then
    begin
    sid:=GetRequestSessionID(Sender);
    if sid<>'' then
      begin
      ReqCS.Acquire;
      try
        idx:=Req.HaveSession(sid);
      finally
        ReqCS.Release;
        end;
      if idx>=0 then
        AddToQueue:=idx+1;
      end;
    end;
  end;

function TRtcLoadBalancerMainForm.FindDataRequestID(Sender:TRtcDataServer; var sid:RtcString; var AddToQueue:integer):integer;
  var
    a,idx,
    cnt1,cnt2,
    cnt3,cnt4:integer;
    found:boolean;
    con:TRtcClientConPool;
  begin
  Result:=-1;
  try
    if Sender<>nil then
      begin
      sid:=GetRequestSessionID(Sender);
      if sid<>'' then
        begin
        idx:=Req.HaveSession(sid);
        if idx>=0 then
          begin
          AddToQueue:=idx+1;
          Con:=TRtcClientConPool(Req.Pool[idx]);
          if ((Con.MaxSessionCount=0) or (Con.ActiveSessions<Con.MaxSessionCount)) and
             ((Con.MaxReqCount=0) or (Con.ActiveClients<Con.MaxReqCount)) then
            begin
            if Con.SessionTimeout>0 then
              Con.UpdateSessionIfExpiresBefore(sid, Now + Con.SessionTimeout, Now + Con.SessionTimeout*2);
            Result:=idx;
            end
          else
            Exit; // Result:=-1
          end;
        end
      else
        idx:=-1;
      end
    else
      begin
      sid:='';
      idx:=-1;
      end;

    if idx<0 then
      begin
      idx:=FLastIDX+1;
      if idx>=Req.PoolCount then idx:=0;

      Con:=TRtcClientConPool(Req.Pool[idx]);
      if ((Con.MaxSessionCount=0) or (Con.ActiveSessions<Con.MaxSessionCount)) and
         ((Con.MaxReqCount=0) or (Con.ActiveClients<Con.MaxReqCount)) then
        Result:=idx
      else
        Result:=-1;

      case FBalanceType of
        1:begin // Balance Requests
          Con:=TRtcClientConPool(Req.Pool[idx]);
          cnt1:=Con.ActiveClients;
          cnt2:=Con.MaxReqCount;

          for a:=0 to Req.PoolCount-1 do
            begin
            found:=False;
            Con:=TRtcClientConPool(Req.Pool[a]);

            if (Con.MaxReqCount=0) or
               (Con.ActiveClients<Con.MaxReqCount) then
              begin
              if (cnt2=0) or (Con.MaxReqCount=0) then
                found := cnt1>Con.ActiveClients
              else
                found:= (cnt1/cnt2)>(Con.ActiveClients/Con.MaxReqCount);
              end;

            if found then
              begin
              idx:=a;
              Result:=idx;
              cnt1:=Con.ActiveClients;
              cnt2:=Con.MaxReqCount;
              end;
            end;

          end;

        2:begin // Balance Sessions
          Con:=TRtcClientConPool(Req.Pool[idx]);
          cnt1:=Con.ActiveClients;
          cnt2:=Con.MaxReqCount;
          cnt3:=Con.ActiveSessions;
          cnt4:=Con.MaxSessionCount;

          for a:=0 to Req.PoolCount-1 do
            begin
            found:=False;
            Con:=TRtcClientConPool(Req.Pool[a]);

            if (Con.MaxSessionCount=0) or
               (Con.ActiveSessions<Con.MaxSessionCount) then
              begin
              if (cnt4=0) or (Con.MaxSessionCount=0) then
                found := cnt3>Con.ActiveSessions
              else
                found:= (cnt3/cnt4)>(Con.ActiveSessions/Con.MaxSessionCount);

              if not found and (cnt3=Con.ActiveSessions) then
                begin
                if (Con.MaxReqCount=0) or
                   (Con.ActiveClients<Con.MaxReqCount) then
                  begin
                  if cnt2=0 then
                    found := cnt1>Con.ActiveClients
                  else
                    begin
                    if Con.MaxReqCount=0 then
                      found:= cnt1>Con.ActiveClients
                    else
                      found:= (cnt1/cnt2)>(Con.ActiveClients/Con.MaxReqCount);
                    end;
                  end;
                end;
              end;

            if found then
              begin
              idx:=a;
              Result:=idx;
              cnt1:=Con.ActiveClients;
              cnt2:=Con.MaxReqCount;
              cnt3:=Con.ActiveSessions;
              cnt4:=Con.MaxSessionCount;
              end;
            end;

          end;
        end;
      end;
  except
    on E:Exception do
      begin
      Log('TForm1.FindDataRequestID',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.GetDataRequest(Sender:TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; const tip:RtcString);
  var
    idx:integer;
    con:TRtcClientConPool;
    sid:RtcString;
  begin
  try
    { This method is used from "DataRouterPostNewRequest" and "DataRouterPostOldRequest" events and is
      responsible for returning an old or creating a new TRtcDataRequest + TRtcHttpClient component pair
      so it can be passed on to the TRtcDataRequest component and used for forwarding the "Sender.Request".

      If different requests have to be sent to different Servers, you can use the "Sender" parameter
      here to access the "Sender.Request" property and check all Request Headeder values. }

    if ReqClosing then Exit;

    ReqCS.Acquire;
    try
      idx:=FindDataRequestID(Sender,sid,AddToQueue);
      if idx>=0 then
        begin
        FLastIDX:=idx;
        Con:=TRtcClientConPool(Req.Pool[idx]);
        DataRequest:=Con.GetDataRequest;
        if assigned(DataRequest) then
          { Store the Session ID in the "sid" variable,
            so we can use it when we receive the Response
            (for example, to close the Session if expired). }
          DataRequest.Client.Info.asString['sid']:=sid;
        end;

      if assigned(DataRequest) then
        LogStatus(Sender,tip+' OK')
      else
        LogStatus(Sender,tip+' WAIT');
    finally
      ReqCS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TForm1.GetDataRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataRouterPostNewRequest(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
  begin
  try
    if ReqClosing then Exit;

    if FReqOrder=3 then
      GetDataRequest(Sender, DataRequest, AddToQueue, 'POST NEW Request')
    else
      begin
      case FReqOrder of
        0: MoveToBottom:=True; // Standard order (FIFO)
        1: MoveToBottom:=False; // Reverse order (LIFO)
        2: MoveToBottom:=random(2)=1; // Randomized
        end;
      if not MoveToBottom then
        GetDataRequest(Sender, DataRequest, AddToQueue, 'POST NEW Request')
      else
        begin
        GetDataRequestQueue(Sender,AddToQueue);
        if DataRouter.WaitingRequests(AddToQueue)=0 then
          begin
          if AddToQueue=0 then
            GetDataRequest(Sender, DataRequest, AddToQueue, 'POST NEW Request')
          else if DataRouter.WaitingRequests(0)=0 then
            GetDataRequest(Sender, DataRequest, AddToQueue, 'POST NEW Request')
          else
            LogStatus(Sender,'QUEUE Request');
          end
        else
          LogStatus(Sender,'QUEUE Request');
        end;
      end;
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterPostNewRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataRouterPostOldRequest(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
  begin
  try
    if ReqClosing then Exit;

    GetDataRequest(Sender, DataRequest, AddToQueue, 'POST OLD Request');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterPostOldRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataRouterQueuedRequest(Sender: TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean);
  var
    ok:boolean;
    sid:RtcString;
  begin
  try
    if ReqClosing then Exit;

    ReqCS.Acquire;
    try
      ok:= FindDataRequestID(Sender,sid,AddedToQueue)>=0;
    finally
      ReqCS.Release;
      end;

    if ok then // Server is now ready to post this request
      DataRouter.ReadyForNextRequest(AddedToQueue);
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterQueuedRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataRouterPostReturn(DataRequest: TRtcDataRequest);
  var
    FromQueue,FromQueue2:integer;
  begin
  try
    if ReqClosing then
      begin
      ReqCS.Acquire;
      try
        Req.PutDataRequest(DataRequest);
        Req.RemoveExpiredSessions(Now);
        LogStatus('POST Return');
      finally
        ReqCS.Release;
        end;
      end
    else if FReqOrder=3 then
      begin
      // We will be using "random" to avoid prioritizing sticky sessions to
      // give non-Sessioned Requests a chance to get through during "peek times"
      if random(2)>0 then
        begin
        FromQueue:=Req.GetPoolIndex(DataRequest.Client)+1;
        FromQueue2:=0;
        end
      else
        begin
        FromQueue2:=Req.GetPoolIndex(DataRequest.Client)+1;
        FromQueue:=0;
        end;

      ReqCS.Acquire;
      try
        Req.PutDataRequest(DataRequest);
        Req.RemoveExpiredSessions(Now);
        LogStatus('POST Return');
      finally
        ReqCS.Release;
        end;

      if not DataRouter.ReadyForNextRequest(FromQueue) then
        DataRouter.ReadyForNextRequest(FromQueue2);
      end
    else
      begin
      // We will be using "random" to avoid prioritizing sticky sessions to
      // give non-Sessioned Requests a chance to get through during "peek times"
      if random(2)>0 then
        begin
        FromQueue:=Req.GetPoolIndex(DataRequest.Client)+1;
        FromQueue2:=0;
        end
      else
        begin
        FromQueue2:=Req.GetPoolIndex(DataRequest.Client)+1;
        FromQueue:=0;
        end;

      { DataRequest object returned, check if there are Requests
        waiting and Post one using this DataRequest object. }
      if DataRouter.PostNextRequest(DataRequest,FromQueue) then
        LogStatus('POST NEXT Request')
      else if DataRouter.PostNextRequest(DataRequest,FromQueue2) then
        LogStatus('POST NEXT Request')
      else
        begin
        // No requests waiting, place the DataRequest object back into our DataRequest object Pool ...
        ReqCS.Acquire;
        try
          Req.PutDataRequest(DataRequest);
          Req.RemoveExpiredSessions(Now);
          LogStatus('POST Return');
        finally
          ReqCS.Release;
          end;
        if not DataRouter.ReadyForNextRequest(FromQueue) then
          DataRouter.ReadyForNextRequest(FromQueue2);
        end;
      end;
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterPostReturn',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataRouterCheckRequest(Sender: TRtcDataServer);
  begin
  try
    Sender.Accept;
    { If all Requests are Accepted (like in this example), then all Requests will be processed which weren't
      processed using other rtcDataProvider or rtcDataRouter or rtcServerModule components linked to the same
      rtcHttpServer (meaning all components which have a "higher CheckOrder position" / "smaller CheckOrder value"
      like for example the "StartProvider" component which has "CheckOrder=0" while DataRouter has "CheckOrder=10"). }

    { For Requests which we have accepted (all requests in this example), we can modify Request Headers here ... }
    if FToURI<>'' then
      Sender.Request.FileName:=FToURI+Sender.Request.FileName;

    { We should also set "Sender.Request.ManualRead" to TRUE if we do NOT intend
      to modify Request Content Body before it is forwarded to our Server (outgoing connection). }
    if NeedRequestContentBody(Sender) then
      Sender.Request.ManualRead:=False
    else
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

procedure TRtcLoadBalancerMainForm.DataRouterRequestBegin(Sender: TRtcDataClient);
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

procedure TRtcLoadBalancerMainForm.DataRouterRequestReceiveAbort(Sender: TRtcDataServer);
  begin
  LogStatus(Sender,'!!! OnRequestReceiveAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataRouterRequestReceived(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
  begin
  try
    if assigned(Content) then
      begin
      LogStatus(Sender,'OnRequestReceived Body='+Int2Str(length(Content.Body)));
      PrepareRequestSessionID(Sender,Content);
      end
    else
      LogStatus(Sender,'OnRequestReceived');

  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterRequestReceived',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataRouterRequestSendAbort(Sender: TRtcDataClient);
  begin
  LogStatus(Sender,'!!! OnRequestSendAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataRouterRequestSent(Sender: TRtcDataClient);
  begin
  LogStatus(Sender,'OnRequestSent');
  end;

procedure TRtcLoadBalancerMainForm.DataRouterResponseBegin(Sender: TRtcDataClient);
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

    if NeedResponseContentBody(Sender) then // We are expecting a Session ID in Content Body?
      Sender.Response.ManualRead:=False
    else if FChangeURLs and
       (Copy(UpperCase(Sender.Response.ContentType),1,5)='TEXT/') and
       (UpperCase(Sender.Response['CONTENT-ENCODING'])<>'GZIP') then
      Sender.Response.ManualRead:=False // ENABLE Response Content Buffering (NOT recommended for long responses!!!!)
    else
      Sender.Response.ManualRead:=True; // DISABLE Response Content Buffering (lower memory requirements)
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterResponseBegin',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcLoadBalancerMainForm.DataRouterResponseReceiveAbort(Sender: TRtcDataClient);
  begin
  LogStatus(Sender,'!!! OnResponseReceiveAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataRouterResponseReceived(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
  var
    sid,body:RtcString;
    clos:boolean;
    idx:integer;
    Con:TRtcClientConPool;
  begin
  try
    { Here, we have received the complete Response from the Server and should check if the
      Response contained a SessionID and if that Session is now being opened or closed. }
    clos:=False;
    sid:=GetResponseSessionID(Sender,Content,clos);
    if sid<>'' then
      begin
      ReqCS.Acquire;
      try
        idx:=Req.HaveSession(sid);

        if idx<0 then // SessionID does not exist yet
          idx:=Req.GetPoolIndex(Sender) // we will link Session ID to our Pool
        else if idx<>req.GetPoolIndex(Sender) then // SessionID exists on a different Server?
          idx:=-1; // That can't be right. SessionIDs have to be unique across all Servers.

        if idx>=0 then
          begin
          Con:=TRtcClientConPool(Req.Pool[idx]);
          if assigned(Con) then
            begin
            if clos then
              Con.CloseSession(sid)
            else if Con.SessionTimeout=0 then
              Con.OpenSession(sid,Now+1) // Sessions with no timeout will expire 1 day after being created
            else if not Con.OpenSession(sid,Now+Con.SessionTimeout) then
              Con.UpdateSessionIfExpiresBefore(sid, Now+Con.SessionTimeout, Now+Con.SessionTimeout*2);
            end;
          end;
      finally
        ReqCS.Release;
        end;
      end;

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

procedure TRtcLoadBalancerMainForm.DataRouterResponseSendAbort(Sender: TRtcDataServer);
  begin
  LogStatus(Sender,'!!! OnResponseSendAbort');
  end;

procedure TRtcLoadBalancerMainForm.DataRouterResponseSent(Sender: TRtcDataServer);
  begin
  LogStatus(Sender,'OnResponseSent');
  end;

procedure TRtcLoadBalancerMainForm.DataRouterDebugLog(Info: TRtcRouterDebugInfo);
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
  ReqCS.Acquire;
  try
    Log('['+Int2Str(Req.ActiveClients)+'+'+Int2Str(Req.IdleClients)+'] '+Sender.PeerAddr+':'+Sender.PeerPort+
        ' > '+Stat+' Len='+Int2Str(Sender.Request.ContentIn)+'/'+Int2Str(Sender.Response.ContentOut)+
        ' '+Sender.Request.URI,'main');
  finally
    ReqCS.Release;
    end;
  end;

procedure TRtcLoadBalancerMainForm.LogStatus(Sender: TRtcDataClient; const Stat: RtcString);
  begin
  if not FEventLog then Exit;
  ReqCS.Acquire;
  try
    Log('['+Int2Str(Req.ActiveClients)+'+'+Int2Str(Req.IdleClients)+'] '+Sender.PeerAddr+':'+Sender.PeerPort+
        ' > '+Stat+' Len='+Int2Str(Sender.Request.ContentOut)+'/'+Int2Str(Sender.Response.ContentIn)+
        ' "'+Int2Str(Sender.Response.StatusCode)+' '+Sender.Response.StatusText+'" '+Sender.Request.URI,'main');
  finally
    ReqCS.Release;
    end;
  end;

procedure TRtcLoadBalancerMainForm.LogStatus(const Stat: RtcString);
  begin
  if not FEventLog then Exit;
  ReqCS.Acquire;
  try
    Log('['+Int2Str(Req.ActiveClients)+'+'+Int2Str(Req.IdleClients)+'] > '+Stat,'main');
  finally
    ReqCS.Release;
    end;
  end;

(***********************************************************)
(*                                                         *)
(*   Methots used to Prepare and Stop the Load Balancer    *)
(*                                                         *)
(***********************************************************)

procedure TRtcLoadBalancerMainForm.CreatePool;
  var
    con:TRtcClientConPool;
    lin,txt:RtcString;
    tmp:RtcString;
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
  Req:=TRtcMultiClientPool.Create;

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
    tmp:=GetToken(':');
    if tmp<>'' then
      begin
      Con:=TRtcClientConPool.Create(128);
      Con.FCliServerAddr:=tmp;
      Con.FCliServerPort:=GetToken(' ');
      Con.FCliServerIPV:=GetRtcIPV(xClientIP6.Checked);
      Con.MaxReqCount:=StrToIntDef(GetToken(' '),0);
      Con.MaxSessionCount:=StrToIntDef(GetToken(' '),0);
      tmp:=GetToken(' ');
      if tmp='' then
        Con.SessionTimeout:=0
      else
        Con.SessionTimeout:=StrToIntDef(tmp,0)/24/60/60;
      Con.FCliMultiThreaded:=xClientMulti.Checked;
      Con.FCliBlocking:=xClientBlocking.Checked;
      Con.FCliFixupRequest:=xForceHttp10.Checked;
      Req.AddPool(Con);
      end;
    until txt='';
  end;

procedure TRtcLoadBalancerMainForm.CloseClients;
  var
    a:integer;
    Temp:TRtcMultiClientPool;
  begin
  if not assigned(Req) then Exit;
  
  try
    { This method is used for closing all connections and destroying all
      TRtcDataRequest and TRtcHttpClient components used with the TRtcDataRouter component. }

    { We want to dump all buffers here, so any errors can be
      spotted in case this method should not complete as expected. }
    if xBuffLog.Checked then StopLogBuffers;

    Log('CloseClient begin','main');
    ReqClosing:=True;

    Log('  *** Closing '+IntToStr(rtcServerConnectionCount)+' incoming connections ...','main');
    Server.StopListenNow;
    Log('  * All incoming connections closed.','main');

    reqCS.Acquire;
    try
      a := Req.ActiveClients;
    finally
      reqCS.Release;
      end;
    if a>0 then
      begin
      Log('  *** Waiting for '+Int2Str(a)+' requests to become idle ...','main');
      repeat
        Application.ProcessMessages;
        Sleep(10);
        reqCS.Acquire;
        try
          a := Req.ActiveClients;
        finally
          reqCS.Release;
          end;
        until a=0;
      Log('  * All requests are now idle.','main');
      end;

    { From here on, all connections should be idle
      and no new connections should arrive ... }

    Temp:=Req;
    Req:=nil;

    if Temp.IdleClients>0 then
      begin
      Log('  *** Closing '+IntToStr(Temp.IdleClients)+' outgoing connections ...','main');
      Temp.CloseAll;
      while rtcClientConnectionCount>0 do
        begin
        Application.ProcessMessages;
        Sleep(10);
        end;
      Log('  * All outgoing connections closed.','main');

      Sleep(1000);

      Log('  *** Releasing connection objects ...','main');
      FreeAndNil(Temp);
      Log('  . All connection objects released.','main');
      end
    else
      FreeAndNil(Temp);

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
    r_count,r_idle,r_sess,a:integer;
    Con:TRtcClientConPool;
    details:RtcString;
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      begin
      if assigned(Req) then
        begin
        ReqCS.Acquire;
        try
          r_count:=Req.ActiveClients;
          r_idle:=Req.IdleClients;
          r_sess:=Req.ActiveSessions;
          details:='';
          for a:=0 to Req.PoolCount-1 do
            begin
            Con:=TRtcClientConPool(Req.Pool[a]);
            details:=details+'Server['+Int2Str(a)+'] '+Con.FCliServerAddr+':'+Con.FCliServerPort+FToURI+'/ = '+
                     Int2Str(Con.ActiveClients+Con.IdleClients)+
                     ' Clients (Max='+Int2Str(Con.MaxReqCount)+', Active='+Int2Str(Con.ActiveClients)+', Idle='+Int2Str(Con.IdleClients)+') / '+
                     Int2Str(Req.Pool[a].ActiveSessions)+' Sessions (Max='+Int2Str(Con.MaxSessionCount)+') <br>';
            end;
        finally
          ReqCS.Release;
          end;
        end
      else
        begin
        r_count:=0;
        r_idle:=0;
        r_sess:=0;
        end;
      Response.ContentType:='text/html';
      Write('<html><body>');
      Write('Connections to all Servers: '+Int2Str(rtcClientConnectionCount)+
            ' Clients (Active='+Int2Str(r_count)+', Idle='+Int2Str(r_idle)+') / '+Int2Str(r_sess)+' Sessions<br>');
      Write('Connections from all Clients: '+Int2Str(rtcServerConnectionCount)+'<br>');
      Write('Router info: '+DataRouter.GetDebugInfo+'<br>');
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

  ReqCS:=TRtcCritSec.Create;
  ReqClosing:=False;
  Req:=nil;
  end;

procedure TRtcLoadBalancerMainForm.FormDestroy(Sender: TObject);
  begin
  FreeAndNil(Req);
  FreeAndNil(ReqCS);
  end;

procedure TRtcLoadBalancerMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  bConnect.Enabled:=false;

  if not ReqClosing then
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

      ReqClosing:=False;
      Randomize;

      { Make sure to set ALL parameters
        before starting the listener ... }

      with DataRouter do
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
        DataRouter.OnDebugLog:=DataRouterDebugLog
      else
        DataRouter.OnDebugLog:=nil;

      FReqOrder:=cReqOrder.ItemIndex;
      FBalanceType:=cBalanceType.ItemIndex;

      FChangeHost:=xChangeHost.Checked;
      FChangeURLs:=xChangeURLs.Checked;
      FRequestBuffer:=xRequestBuffer.Checked;
      FResponseBuffer:=xResponseBuffer.Checked;
      FToURI:=RtcString(eToURI.Text);

      CreatePool;

      Server.Listen;
      bConnect.Caption:='STOP';
      end
    else
      begin
      ReqClosing:=True;
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
  DataRouter.PostReturnBeforeResponseSent:=xPostReturnBeforeResponseSent.Checked;
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
