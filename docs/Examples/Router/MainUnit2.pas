{
  @html(<b>)
  RTC Router Demo Project
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This Project shows how to use the TRtcDataRouter component
  with the RealThinClient SDK to write a HTTP Router.

  This is the same example as the "RtcRouter" Project, with the only difference
  that this example uses the "rtcCliPool.pas" unit for a Client connection pool.
}

unit MainUnit2;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls,

  rtcTypes, rtcConn,
  rtcDataCli, rtcDataSrv,
  rtcHttpCli, rtcHttpSrv,
  rtcInfo, rtcLog,
  rtcThrPool, rtcDataRoute,
  rtcSystem;

type
  TRtcClientConPool=class(TRtcClientPool)
  public
    FCliServerAddr:RtcString;
    FCliServerPort:RtcString;
    FCliServerIPV:RtcIPV;
    FCliMultiThreaded:boolean;
    FCliBlocking:boolean;
    FCliFixupRequest:boolean;

    MaxReqCount:integer;

    function NewClient:TRtcDataClient; override;
    end;

  TRtcRouter2MainForm = class(TForm)
    Server: TRtcHttpServer;
    bConnect: TButton;
    eFromPort: TEdit;
    Label4: TLabel;
    xServerMulti: TCheckBox;
    xServerBlocking: TCheckBox;
    xClientMulti: TCheckBox;
    xClientBlocking: TCheckBox;
    xChangeHost: TCheckBox;
    eToHost: TEdit;
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
    Label6: TLabel;
    eConCount: TSpinEdit;
    Label1: TLabel;
    eToAddr: TEdit;
    eToPort: TEdit;
    Label2: TLabel;
    Label11: TLabel;
    xClientIP6: TCheckBox;
    xServerIP6: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bConnectClick(Sender: TObject);
    procedure eToAddrChange(Sender: TObject);
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
    Req:TRtcClientConPool;
    ReqCS:TRtcCritSec;
    ReqClosing:boolean;

    FChangeHost:boolean;
    FToHost:RtcString;
    FToURI:RtcString;
    FChangeURLs:boolean;
    FRequestBuffer:boolean;
    FResponseBuffer:boolean;
    FEventLog:boolean;

    FReqOrder:integer;

    procedure GetDataRequest(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; const tip:RtcString);

    procedure LogStatus(Sender:TRtcDataServer; const Stat:RtcString); overload;
    procedure LogStatus(Sender:TRtcDataClient; const Stat:RtcString); overload;
    procedure LogStatus(const Stat:RtcString); overload;

    procedure CloseClients;
  end;

var
  RtcRouter2MainForm: TRtcRouter2MainForm;

implementation

{$R *.dfm}

{ TRtcClientConPool }

function TRtcClientConPool.NewClient: TRtcDataClient;
  begin
  if (MaxReqCount>0) and (ActiveClients>=MaxReqCount) then
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

{ TRtcRouterMainForm }

procedure TRtcRouter2MainForm.FormCreate(Sender: TObject);
  begin
  try
    StartLog;
    xBuffLogClick(xBuffLog);

    ReqCS:=TRtcCritSec.Create;
    ReqClosing:=False;
    Req:=nil;
  except
    on E:Exception do
      begin
      Log('TForm1.FormCreate',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.FormDestroy(Sender: TObject);
  begin
  try
    FreeAndNil(Req);
    FreeAndNil(ReqCS);
  except
    on E:Exception do
      begin
      Log('TForm1.FormDestroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  try
    bConnect.Enabled:=false;

    if not ReqClosing then
      CloseClients;
  except
    on E:Exception do
      begin
      Log('TForm1.FormCloseQuery',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.bConnectClick(Sender: TObject);
  begin
  try
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
        FChangeHost:=xChangeHost.Checked;
        FToHost:=RtcString(eToHost.Text);
        FChangeURLs:=xChangeURLs.Checked;
        FRequestBuffer:=xRequestBuffer.Checked;
        FResponseBuffer:=xResponseBuffer.Checked;
        FToURI:=RtcString(eToURI.Text);

        Req:=TRtcClientConPool.Create(128);
        Req.FCliServerAddr:=RtcString(eToAddr.Text);
        Req.FCliServerPort:=RtcString(eToPort.Text);
        Req.FCliServerIPV:=GetRtcIPV(xClientIP6.Checked);
        Req.FCliMultiThreaded:=xClientMulti.Checked;
        Req.FCliBlocking:=xClientBlocking.Checked;
        Req.FCliFixupRequest:=xForceHttp10.Checked;
        Req.MaxReqCount:=eConCount.Value;

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
  except
    on E:Exception do
      begin
      Log('TForm1.bConnectClick',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.eToAddrChange(Sender: TObject);
  begin
  try
    eToHost.Text:=eToAddr.Text;
  except
    on E:Exception do
      begin
      Log('TForm1.eToAddChange',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.eLogFolderChange(Sender: TObject);
  begin
  try
    if eLogFolder.Text<>'' then
      RTC_LOG_FOLDER:=IncludeTrailingPathDelimiter(eLogFolder.Text)
    else
      RTC_LOG_FOLDER:='';
  except
    on E:Exception do
      begin
      Log('TForm1.eLogFolderChange',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.eToURIExit(Sender: TObject);
  begin
  try
    if eToURI.Text<>'' then
      begin
      if eToURI.Text='/' then
        eToURI.Text:=''
      else if Copy(eToURI.Text,1,1)<>'/' then
        eToURI.Text:='/'+eToURI.Text;
      end;
  except
    on E:Exception do
      begin
      Log('TForm1.eToURIExit',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.CloseClients;
  var
    a:integer;
    Temp:TRtcClientConPool;
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

    if Server.isListening then
      Server.StopListen;

    Log('  *** Closing '+IntToStr(rtcServerConnectionCount)+' incoming connections ...','main');
    while Server.isListening or (rtcServerConnectionCount>0) do
      begin
      Application.ProcessMessages;
      Sleep(10);
      end;
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
      Temp.Close;
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

procedure TRtcRouter2MainForm.GetDataRequest(Sender:TRtcDataServer; var DataRequest: TRtcDataRequest; const tip:RtcString);
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
      DataRequest:=Req.GetDataRequest;
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

procedure TRtcRouter2MainForm.DataRouterPostNewRequest(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
  begin
  { A new Request is ready for posting to the Server and the TRtcDataRouter component is
    now asking for a TRtcDataRequest component linked to a TRtcHttpClient, which should be
    used for posting the Request.

    If the Request should NOT be posted yet, simply leave DataRequest unchanged (NIL) and
    set "MoveToBottom" to TRUE if you want this "Sender.Request" to be moved to the BOTTOM
    of the Request Queue, or set it to FALSE if the Request should be placed at the TOP.

    Requests are always picked up from the TOP of the request Queue when "ReadyForNextRequest"
    or "PostNextRequest" methods are called on the "TRtcDataRouter" component.

    Default "MoveToBottom" value for NEW Requests is always TRUE, so you do NOT need to
    change it if you want to use the default (first-in, first-out) behavior. }

  try
    if ReqClosing then Exit;

    if FReqOrder=3 then
      GetDataRequest(Sender, DataRequest, 'POST NEW Request')
    else
      begin
      case FReqOrder of
        0: MoveToBottom:=True; // Standard order (FIFO)
        1: MoveToBottom:=False; // Reverse order (LIFO)
        2: MoveToBottom:=random(2)=1; // Randomized
        end;
      if not MoveToBottom or (DataRouter.WaitingRequests(AddToQueue)=0) then
        GetDataRequest(Sender, DataRequest, 'POST NEW Request')
      else
        LogStatus(Sender,'QUEUE Request');
      end;
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterPostNewRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterPostOldRequest(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer; var MoveToBottom: Boolean);
  begin
  { After "ReadyForNextRequest" method was called (for example: from the "DataRouterQueuedRequest" event)
    and TRtcDataRouter has found a Request waiting in its Request Queue, THIS event is called to allow
    you to check if a DataRequest component is *still* available (outgoing Server is ready for requests)
    and return a "DataRequest" component if "Sender.Request" should be sent to the Server now.

    If a DataRequest component is available and should be used to post this Request, this event has to
    return a "TRtcDataRequest" linked to a "TRtcHttpClient" component (similar to the "OnPostNewRequest" event).

    If a DataRequest component is NOT available now, simply leave the "DataRequest" parameter
    unchanged (NIL) and set the "MoveToBottom" parameter to TRUE if you want the Request to be
    moved to the BOTTOM of the Request queue, or leave "MoveToBottom" unchanged (FALSE) if you
    want the Request to remain at the TOP of the Request queue, so it can be picked up first when
    "ReadyForNextRequest" or "PostNextRequest" method is called again on this TRtcDataRouter component.

    Default "MoveToBottom" value for OLD Requests is always FALSE, so you do NOT need to change
    it if you want OLD Requests to remain at the TOP of to request queue if they can't be sent. }

  try
    if ReqClosing then Exit;

    GetDataRequest(Sender, DataRequest, 'POST OLD Request');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterPostOldRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterQueuedRequest(Sender: TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean);
  var
    ok:boolean;
  begin
  { This event is called immediately after the Request was placed in the Request Queue,
    which happened either after a call to "DataRouterPostNewRequest" or "DataRouterPostOldRequest"
    in case the event did NOT return a valid "TRtcDataRouter" object and thus the Request
    could NOT be sent to the Server (outgoing connection) at that time.

    The purpose of this event (QueuedRequest) is to allow you to check if a connection
    (DataRequest + HttpClient component pair) is now available, in which case you should
    use the "ReadyForNextRequest" or "PostNextRequest" method to signal the TRtcDataRouter
    that you are now ready.

    If you use "PostNextRequest" on the TRtcDataRouter component, topmost Request from the Request
    Queue will be sent using the "DataRequest" component passed to the "PostNextRequest" method,
    without giving you the chance to check Request Headers.

    You should ONLY use the "PostNextRequest" method if your TRtcDataRouter component is working
    with a single Server, or if all the Web Applications handled with this TRtcDataRouter component
    are stateless so that any Request can be send to any Server.

    If your TRtcDataRouter component has to work with multiple Servers, but - depending on Request
    headers - some Requests need to be sent to a specific Server and you need to check Request
    Headers before you can decide to which Server which Request should be sent to, always use
    the "ReadyForNextRequest" method with that particular TRtcDataRouter component.

    If you use the "ReadyForNextRequest" method, "DataRouterPostOldRequest" event will be
    triggered for the topmost Request in the Request Queue of that TRtcDataRouter component,
    allowing you to check Request Headers before deciding where the Request should be sent.

    NOTE: You can use any number of TRtcDataRouter components in the same application if
    request headers are enough for you to decide where which request should be forwarded to.
    For example, if by checking Request Headers you can already know if a Request is being
    sent to a Stateful Web Application, because of which all future Requests from that
    Client should be forwarded to the same Web Application, you can use a separate
    TRtcDataRouter component for every Stateful Server and move the decision-making
    process directly into the "OnCheckRequest" event. }

  try
    if ReqClosing then Exit;

    ReqCS.Acquire;
    try
      ok:= Req.ActiveClients<Req.MaxReqCount;
    finally
      ReqCS.Release;
      end;
    if ok then
      DataRouter.ReadyForNextRequest(AddedToQueue);
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterQueuedRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterPostReturn(DataRequest: TRtcDataRequest);
  begin
  { This event will be called after a DataRequest component is no longer used
    and can now be used for posting other Requests to the same Server. }

  try
    if ReqClosing then
      begin
      ReqCS.Acquire;
      try
        Req.PutDataRequest(DataRequest);
        LogStatus('POST Return');
      finally
        ReqCS.Release;
        end;
      end
    else if FReqOrder=3 then
      begin
      ReqCS.Acquire;
      try
        Req.PutDataRequest(DataRequest);
        LogStatus('POST Return');
      finally
        ReqCS.Release;
        end;
      DataRouter.ReadyForNextRequest(0);
      end
    else
      begin
      { DataRequest object returned, check if there are Requests
        waiting and Post one using this DataRequest object. }
      if DataRouter.PostNextRequest(DataRequest,0) then
        LogStatus('POST NEXT Request')
      else
        begin
        // No requests waiting, place the DataRequest object back into our DataRequest object Pool ...
        ReqCS.Acquire;
        try
          Req.PutDataRequest(DataRequest);
          LogStatus('POST Return');
        finally
          ReqCS.Release;
          end;
        DataRouter.ReadyForNextRequest(0);
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

procedure TRtcRouter2MainForm.DataRouterCheckRequest(Sender: TRtcDataServer);
  begin
  { You can check request headers here and Accept the Request ONLY if you want to forward it to your Server.
    If all Requests are Accepted (like in this example), then all Requests will be processed which weren't
    processed using other rtcDataProvider or rtcDataRouter or rtcServerModule components linked to the same
    rtcHttpServer (meaning all components which have a "higher CheckOrder position" / "smaller CheckOrder value"
    like for example the "StartProvider" component which has "CheckOrder=0" while DataRouter has "CheckOrder=10"). }

  try
    Sender.Accept;

    { For Requests which we have accepted (all requests in this specific example),
      we can modify Request Headers here if needed. }

    if FToURI<>'' then
      Sender.Request.FileName:=FToURI+Sender.Request.FileName;

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

procedure TRtcRouter2MainForm.DataRouterRequestBegin(Sender: TRtcDataClient);
  begin
  { This event is called immediately before the "Sender.Request" starts being forwarded to the Server.
    You can use this event to make final adjustments to Request Headers. }

  try
    LogStatus(Sender,'OnRequestBegin');

    if FChangeHost then
      Sender.Request.Host:=FToHost;

    if FChangeURLs then
      if Sender.Request.Referer<>'' then
        if UpperCase(Copy(Sender.Request.Referer,1,8+9))='HTTP://LOCALHOST/' then
          Sender.Request.Referer:=RtcString(StringReplace(String(Sender.Request.Referer),'http://localhost/',String('http://'+FToHost+'/'),[rfReplaceAll,rfIgnoreCase]));
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterRequestBegin',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterRequestReceiveAbort(Sender: TRtcDataServer);
  begin
  { This event is called in case a connection to the Client was lost before the complete Request was received.
    You can use this event for decision-making or logging or anything else you might need, but it does NOT
    necessarily have to be implemented for the TRtcDataRouter component to function. }

  try
    LogStatus(Sender,'!!! OnRequestReceiveAbort');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterRequestReceiveAbort',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterRequestReceived(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
  begin
  { This event is called after a complete Request has been received from the Client (incomming connection).

    If you have enabled Request Content buffering for this particular Request, "Content" parameter
    will be assigned and will contain the complete Request Content Body in its "Content.Body" variable,
    which you can modify here if you want a different Request Content to be sent to the Server.

    If the "Content" parameter is assigned, you can also modify Request Headers. If the "Content"
    parameter is NOT assigned, you should NOT make any modifications to the Request in this event,
    but you can use the event for logging and other non-intrusive operations. }

  try
    if assigned(Content) then
      LogStatus(Sender,'OnRequestReceived Body='+Int2Str(length(Content.Body)))
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

procedure TRtcRouter2MainForm.DataRouterRequestSendAbort(Sender: TRtcDataClient);
  begin
  { This event is called in case a connection to the Server was lost before the complete Request was sent.
    You can use this event for decision-making or logging or anything else you might need, but it does NOT
    necessarily have to be implemented for the TRtcDataRouter component to function. }

  try
    LogStatus(Sender,'!!! OnRequestSendAbort');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterRequestSendAbort',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterRequestSent(Sender: TRtcDataClient);
  begin
  { This event is called after the complete Request was sent to the Server.
    You can use this event for decision-making or logging or anything else you might need,
    but it does NOT have to be implemented for the TRtcDataRouter component to function. }

  try
    LogStatus(Sender,'OnRequestSent');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterRequestSent',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterResponseBegin(Sender: TRtcDataClient);
  begin
  { This event is called once Response Headers have been received from the Server.

    This event is almost as important as the "DataRouterCheckRequest" event, because you get to
    decide here whether you want to buffer the Response Content Body so you can modify it before
    it is forwarded to the Client, or DISABLE Response Content buffering in case you do NOT need
    to modify the Response Content body.

    You can also make changes to Response Headers here, regardless of
    whether you enable or disable Response Content buffering. }

  try
    LogStatus(Sender,'OnResponseBegin');

    if FChangeHost then
      begin
      if Sender.Response['LOCATION']<>'' then
        Sender.Response['LOCATION']:=RtcString(StringReplace(String(Sender.Response['LOCATION']),String('http://'+FToHost+FToURI+'/'),'/',[rfReplaceAll,rfIgnoreCase]));
      if Sender.Response['REFRESH']<>'' then
        Sender.Response['REFRESH']:=RtcString(StringReplace(String(Sender.Response['REFRESH']),String('http://'+FToHost+FToURI+'/'),'/',[rfReplaceAll,rfIgnoreCase]));
      end;

    { To enable Response Content buffering, set "Sender.Response.ManualRead" to FALSE. And to disable Response
      Content buffering for this particular Response, allowing the TRtcDataRouter component to forward the Content
      directly to the Client without having to buffer it into memory, set "Sender.Response.ManualRead" to TRUE. }

    if FChangeURLs and
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

procedure TRtcRouter2MainForm.DataRouterResponseReceiveAbort(Sender: TRtcDataClient);
  begin
  { This event is called in case a connection to the Server was lost before the complete Response was received.
    You can use this event for decision-making or logging or anything else you might need, but it does NOT
    necessarily have to be implemented for the TRtcDataRouter component to function. }

  try
    LogStatus(Sender,'!!! OnResponseReceiveAbort');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterResponseReceiveAbort',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterResponseReceived(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
  var
    Body:RtcString;
  begin
  { This event is called after a complete Response has been received from the Server (outgoing connection).

    If you have enabled Response Content buffering for this particular Response, "Content" parameter
    will be assigned and will contain the complete Response Content Body in its "Content.Body" variable,
    which you can modify here if you want a different Response Content to be sent to the Client.

    If the "Content" parameter is assigned, you can also modify Response Headers. If the "Content"
    parameter is NOT assigned, you should NOT make any modifications to the Response in this event,
    but you can use the event for logging and other non-intrusive operations. }

  try
    if assigned(Content) then // Response Content Buffering was enabled in "DataRouterResponseBegin" event
      begin
      LogStatus(Sender,'OnResponseReceived Body=' + Int2Str(length(Content.Body)) );

      if FChangeURLs and (length(Content.Body)>0) then
        if (Copy(UpperCase(Sender.Response.ContentType),1,5)='TEXT/') and
           (UpperCase(Sender.Response['CONTENT-ENCODING'])<>'GZIP') then
          begin
          Body:=StringReplace(Content.Body,'http://'+FToHost+FToURI+'"','/"',[rfReplaceAll,rfIgnoreCase]);
          Body:=StringReplace(Body,'http://'+FToHost+FToURI+'/','/',[rfReplaceAll,rfIgnoreCase]);
          Body:=StringReplace(Body,'http://'+FToHost+FToURI+'<','/<',[rfReplaceAll,rfIgnoreCase]);
          Body:=StringReplace(Body,'http://'+FToHost+FToURI+' ','/ ',[rfReplaceAll,rfIgnoreCase]);
          Content.Body:=StringReplace(Body,'http://'+FToHost+FToURI+#13,'/'#13,[rfReplaceAll,rfIgnoreCase]);
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

procedure TRtcRouter2MainForm.DataRouterResponseSendAbort(Sender: TRtcDataServer);
  begin
  { This event is called in case a connection to the Client was lost before the complete Response was sent.
    You can use this event for decision-making or logging or anything else you might need, but it does NOT
    necessarily have to be implemented for the TRtcDataRouter component to function. }

  try
    LogStatus(Sender,'!!! OnResponseSendAbort');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterResponseSendAbort',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterResponseSent(Sender: TRtcDataServer);
  begin
  { This event is called after the complete Response was sent to the Client.
    You can use this event for decision-making or logging or anything else you might need,
    but it does NOT have to be implemented for the TRtcDataRouter component to function. }

  try
    LogStatus(Sender,'OnResponseSent');
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterResponseSent',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DataRouterDebugLog(Info: TRtcRouterDebugInfo);
  begin
  { This event is used by "TRtcDataRouter" for Debug Logging. This event does NOT need
    to be implemented if you do NOT need detailed TRtcDataRouter Debug LOGs to be created.

    Debug Logging takes time. When LOG is written into a File, it takes a LOT of time.
    If Debug logging is NOT required, this event should NOT be assigned to improve performance. }

  try
    Log(Info.Text,Info.Name);
  except
    on E:Exception do
      begin
      Log('TForm1.DataRouterDebugLog',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.xBuffLogClick(Sender: TObject);
  begin
  try
    if xBuffLog.Checked then
      StartLogBuffers(128000000) // 128 MB buffers
    else
      StopLogBuffers;
  except
    on E:Exception do
      begin
      Log('TForm1.xBuffLogClick',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.bDumpLogClick(Sender: TObject);
  begin
  try
    DumpLogBuffers;
  except
    on E:Exception do
      begin
      Log('TForm1.bDumpLogClick',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.StatProviderCheckRequest(Sender: TRtcConnection);
  begin
  try
    with TRtcDataServer(Sender) do
      if Request.FileName='/$tat' then
        Accept;
  except
    on E:Exception do
      begin
      Log('TForm1.StatProviderCheckRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.StatProviderDataReceived(Sender: TRtcConnection);
  var
    r_count,r_idle,r_max:integer;
  begin
  try
    with TRtcDataServer(Sender) do
      if Request.Complete then
        begin
        if assigned(Req) then
          begin
          ReqCS.Acquire;
          try
            r_count:=Req.ActiveClients;
            r_idle:=Req.IdleClients;
            r_max:=Req.MaxReqCount;
          finally
            ReqCS.Release;
            end;
          end
        else
          begin
          r_count:=0;
          r_idle:=0;
          r_max:=0;
          end;
        Response.ContentType:='text/html';
        Write('<html><body>');
        Write('Connections to Server: '+Int2Str(rtcClientConnectionCount)+' (max='+Int2Str(r_max)+', active='+Int2Str(r_count)+', idle='+Int2Str(r_idle)+') <br>');
        Write('Connections from Clients: '+Int2Str(rtcServerConnectionCount)+'<br>');
        Write('Router: '+DataRouter.GetDebugInfo);
        Write('</body></html>');
        end;
  except
    on E:Exception do
      begin
      Log('TForm1.StatProviderDataReceived',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcRouter2MainForm.DumpProviderCheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/$dump' then
      Accept;
  end;

procedure TRtcRouter2MainForm.DumpProviderDataReceived(Sender: TRtcConnection);
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

procedure TRtcRouter2MainForm.xPostReturnBeforeResponseSentClick(Sender: TObject);
  begin
  // Test changing the property while the Router is active
  DataRouter.PostReturnBeforeResponseSent:=xPostReturnBeforeResponseSent.Checked;
  end;

procedure TRtcRouter2MainForm.LogStatus(Sender: TRtcDataServer; const Stat: RtcString);
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

procedure TRtcRouter2MainForm.LogStatus(Sender: TRtcDataClient; const Stat: RtcString);
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

procedure TRtcRouter2MainForm.LogStatus(const Stat: RtcString);
  begin
  if not FEventLog then Exit;
  ReqCS.Acquire;
  try
    Log('['+Int2Str(Req.ActiveClients)+'+'+Int2Str(Req.IdleClients)+'] > '+Stat,'main');
  finally
    ReqCS.Release;
    end;
  end;

end.
