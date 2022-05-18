unit WebStressTool_Unit;

{$include rtcDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellApi,

  rtcLog, rtcTypes,
  rtcFunction, rtcDataCli, rtcInfo, rtcConn,
  rtcHttpCli, rtcThrPool, ComCtrls,

  rtcMemory, rtcSystem;

type
  TForm1 = class(TForm)
    RtcClient: TRtcHttpClient;
    myPanel: TPanel;
    Panel1: TPanel;
    myBox: TPaintBox;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    lblFlood: TLabel;
    lblClients: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label32: TLabel;
    eConCnt: TEdit;
    eReqCnt: TEdit;
    xReqAutoRepeat: TCheckBox;
    eUpdCnt: TEdit;
    xAutoDisconnect: TCheckBox;
    xReqAutoConnect: TCheckBox;
    eThreads: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    eServer: TEdit;
    xProxy: TCheckBox;
    ePort: TEdit;
    xSSL: TCheckBox;
    Memo1: TMemo;
    Bevel1: TBevel;
    DataRequest: TRtcDataRequest;
    btnMultiFlood: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    lblRepost: TLabel;
    xBlockingWinSock: TCheckBox;
    Label2: TLabel;
    eMaxLoops: TEdit;
    Label3: TLabel;
    lblReqSec: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    eConnPlus: TEdit;
    procedure eServerChange(Sender: TObject);
    procedure xProxyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure xRepostClick(Sender: TObject);
    procedure btnMultiFloodClick(Sender: TObject);
    procedure xSSLClick(Sender: TObject);
    procedure MultiClientConnect(Sender: TRtcConnection);
    procedure MultiClientDisconnect(Sender: TRtcConnection);
    procedure DataRequestBeginRequest(Sender: TRtcConnection);
    procedure DataRequestDataReceived(Sender: TRtcConnection);
    procedure DataRequestResponseDone(Sender: TRtcConnection);
    procedure Panel2Click(Sender: TObject);
    procedure DataRequestRepostCheck(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
    FloodCnt,RepostCnt,LoopCnt:int64;
    ClientCnt:longint;
    RequestLines:TStringList;
    TestTime,LoopTime,LoopReq:Cardinal;
    TestReqCount:int64;
    CliCon:array of TRtcHttpClient;
    CliReq:array of TRtcDataRequest;

    procedure SetupCliCon(a,req,upd:integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.eServerChange(Sender: TObject);
  begin
  if (RtcClient.ServerAddr<>eServer.Text) or
     (RtcClient.ServerPort<>ePort.Text) then
    begin
    RtcClient.Disconnect;
    RtcClient.ServerAddr:=RtcString(eServer.Text);
    RtcClient.ServerPort:=RtcString(ePort.Text);

    eServer.Text:=RtcClient.ServerAddr;
    ePort.Text:=RtcClient.ServerPort;
    end;
  end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
  StartLog;

  {$IFDEF RtcTest}
  LOG_THREAD_EXCEPTIONS:=True;
  LOG_EXCEPTIONS:=True;
  eServer.Text:='server';
  {$endif}

  RequestLines:=TStringList.Create;
  end;

procedure TForm1.xRepostClick(Sender: TObject);
  begin
  end;

(***************************************************)
(** Methods used by MULTI-CONNECTION FLOOD TEST! ***)
(***************************************************)

procedure TForm1.SetupCliCon(a, req, upd: integer);
begin
  if length(CliCon)<a+1 then
    begin
    SetLength(CliCon,a+1);
    SetLength(CliReq,a+1);
    end;
  CliCon[a]:=TRtcHttpClient.Create(nil);
  CliReq[a]:=TRtcDataRequest.Create(nil);
  with CliCon[a] do
    begin
    ServerAddr:=RtcString(eServer.Text);
    ServerPort:=RtcString(ePort.Text);

    MultiThreaded:=True;
    Blocking:=xBlockingWinSock.Checked;

    AutoConnect:=xReqAutoConnect.Checked;
    UseProxy:=xProxy.Checked;
    UseSSL:=xSSL.Checked;

    ReconnectOn.ConnectError:=True;
    ReconnectOn.ConnectLost:=True;
    ReconnectOn.ConnectFail:=True;
    OnConnecting:=MultiClientConnect;
    OnDisconnecting:=MultiClientDisconnect;
    Timeout.AfterConnecting:=RtcClient.Timeout.AfterConnecting;
    Timeout.AfterConnect:=RtcClient.Timeout.AfterConnect;
    end;
  with CliReq[a] do
    begin
    OnBeginRequest:=DataRequestBeginRequest;
    OnDataReceived:=DataRequestDataReceived;
    OnResponseDone:=DataRequestResponseDone;
    OnRepostCheck:=DataRequestRepostCheck;
    Client:=CliCon[a];
    end;
  with CliCon[a].Info do
    begin
    asPtr['REQ']:=CliReq[a];
    asInteger['CNT']:=req;
    asInteger['LEFT']:=req;
    asInteger['MAX']:=req;
    asInteger['UPD']:=upd;
    asInteger['ID']:=a;
    end;
end;

procedure TForm1.btnMultiFloodClick(Sender: TObject);
  var
    cnt,req,upd,a:integer;
    s:string;
  begin
  if eServer.Text='' then
    begin
    ShowMessage('No Server address specified!');
    Exit;
    end;
  RequestLines.Clear;
  for a:=0 to Memo1.Lines.count-1 do
    begin
    s:=Trim(Memo1.Lines.Strings[a]);
    if s<>'' then
      begin
      if Copy(s,1,1)<>'/' then s:='/'+s;
      s:=StringReplace(s,' ','+',[rfReplaceAll]);
      RequestLines.Add(s);
      end;
    end;
  if RequestLines.Count<=0 then
    begin
    ShowMessage('Request List empty, nothing to send to the Server.');
    Exit;
    end;

  // DO NOT TRY TO CHANGE THIS WHEN CONNECTIONS ARE ACTIVE!
  try
    RTC_THREAD_POOL_MAX:=StrToInt(Trim(eThreads.Text));
  except
    Showmessage('Invalid value for Max Threads');
    Exit;
    end;

  { We will browse through all connection and
    DataRequest components to release them, one-by-one. }
  for a:=0 to length(CliCon)-1 do
    begin
    CliCon[a].Info.asPtr['REQ']:=nil;
    CliReq[a].Free;
    CliCon[a].Free;
    end;
  SetLength(CliCon,0);
  SetLength(CliReq,0);

  lblClients.Caption:='0';
  lblFlood.Caption:='0';
  lblRepost.Caption:='0';
  myPanel.Refresh;

  btnMultiFlood.Enabled:=False;

  RepostCnt:=0;
  ClientCnt:=0;
  FloodCnt:=0;
  LoopCnt:=0;

  try
    cnt:=StrToInt(eConCnt.Text);
  except
    ShowMessage('Invalid value for Connection count');
    Exit;
    end;
  try
    req:=StrToInt(eReqCnt.Text);
  except
    ShowMessage('Invalid value for Request count');
    Exit;
    end;
  try
    upd:=StrToInt(eUpdCnt.Text);
  except
    ShowMessage('Invalid value for Update count');
    Exit;
    end;

  eConCnt.Enabled:=False;

  Log( 'Loop No; '+
       'Clients; '+
       'Requests; '+
       'Req/Sec; '+
       'Req/Sec/Clients; '+
       'Total Requests; '+
       'Avg Req/Sec; '+
       'Failed Req; '+
       'Failure%',
       eServer.Text+'_'+ePort.Text );

  SetLength(CliCon,cnt);
  SetLength(CliReq,cnt);
  // Create new HttpConnection and DataRequest components ...
  for a:=0 to cnt-1 do
    SetupCliCon(a,req,upd);

  myBox.Canvas.Brush.Color:=clBtnFace;
  myBox.Canvas.FillRect(Rect(0,0,myBox.Width,myBox.Height));
  myBox.Canvas.Brush.Color:=clNone;

  for a:=0 to Length(CliReq)-1 do
    begin
    myBox.Canvas.Pen.Color:=clMaroon;
    myBox.Canvas.MoveTo(a,0);
    myBox.Canvas.LineTo(a,myBox.Height-1);
    if not CliCon[a].AutoConnect then
      CliCon[a].Connect;
    end;

  TestReqCount:=cnt*req;
  TestTime:=GetTickTime;
  LoopTime:=TestTime;
  LoopReq:=cnt*req;

  for a:=0 to Length(CliReq)-1 do
    begin
    with CliReq[a] do
      begin
      // Post the request
      Request.Method:='GET';
      Request.Host:=Client.ServerAddr;
      Request.FileName:=RtcString(RequestLines.Strings[Client.Info.asInteger['CNT'] mod RequestLines.Count]);
      Post;
      end;
    end;
  end;

procedure TForm1.xProxyClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.UseProxy:=xProxy.Checked;
  finally
    xProxy.Checked:=RtcClient.UseProxy;
    end;
  end;

procedure TForm1.xSSLClick(Sender: TObject);
  begin
  RtcClient.Disconnect;
  try
    RtcClient.UseSSL:=xSSL.Checked;
  finally
    xSSL.Checked:=RtcClient.UseSSL;
    if xSSL.Checked then
      ePort.Text:='443'
    else
      ePort.Text:='80';
    end;
  end;

procedure TForm1.MultiClientConnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(MultiClientConnect)
  else
    begin
    Inc(ClientCnt);
    lblClients.Caption:=IntToStr(ClientCnt);
    end;
  end;

procedure TForm1.MultiClientDisconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(MultiClientDisconnect)
  else
    begin
    Dec(ClientCnt);
    lblClients.Caption:=IntToStr(ClientCnt);
    end;
  end;

procedure TForm1.DataRequestBeginRequest(Sender: TRtcConnection);
  begin
  Sender.Write; // Simply send out what was prepared (no content body)
  end;

procedure TForm1.DataRequestDataReceived(Sender: TRtcConnection);
  begin
  Sender.Read; // we are not interested in the data we receive
  end;

procedure TForm1.DataRequestResponseDone(Sender: TRtcConnection);
  var
    x,y,a,req,upd,maxLoops:integer;
    MyTime, MyLoop:Cardinal;
  begin
  with TRtcDataClient(Sender) do
    begin
    if Info.asInteger['CNT']>0 then
      begin
      if (Info.asInteger['LEFT']-Info.asInteger['CNT']>=Info.asInteger['UPD']) then
        if not inMainThread then
          Sync(DataRequestResponseDone)
        else
          begin
          x:=Info.asInteger['ID'];
          y:=round(Info.asInteger['CNT']/Info.asInteger['MAX']*myBox.Height);
          if Info.asInteger['CNT'] mod (Info.asInteger['UPD']*2) >= Info.asInteger['UPD'] then
            myBox.Canvas.Pen.Color:=clRed
          else
            myBox.Canvas.Pen.Color:=clMaroon;
          myBox.Canvas.MoveTo(x,0);
          myBox.Canvas.LineTo(x,y);
          myBox.Canvas.Pen.Color:=clLime;
          myBox.Canvas.LineTo(x,myBox.Height-1);

          FloodCnt:=FloodCnt+Info.asInteger['UPD'];
          lblFlood.Caption:=IntToStr(FloodCnt);
          lblFlood.Refresh;

          Info.asInteger['LEFT']:=Info.asInteger['LEFT']-Info.asInteger['UPD'];
          Exit; { Event execution will continue in background thread,
                  directly after Sync() ... }
          end;

      Info.asInteger['CNT']:=Info.asInteger['CNT']-1;

      with TRtcDataRequest(Info.asPtr['REQ']) do
        begin
        Request.Method:='GET';
        Request.Host:=Client.ServerAddr;
        Request.FileName:=RtcString(RequestLines.Strings[Info.asInteger['CNT'] mod RequestLines.Count]);
        // Need to call with TRUE as parameter since we are posting
        // a new request from within our connection event.
        Post(True, Sender);
        end;
      end
    else if not inMainThread then
      // need to call the event synchronized, since we are accessing the GUI
      Sync(DataRequestResponseDone)
    else
      begin
      // We've gone through all requested interrations,
      // time to update the counter display and disconnect,
      // since we probably won't be using this connection anymore.
      x:=Info.asInteger['ID'];
      myBox.Canvas.Pen.Color:=clAqua;
      myBox.Canvas.MoveTo(x,0);
      myBox.Canvas.LineTo(x,myBox.Height-1);
      eConCnt.Text:=IntToStr(StrToInt(eConCnt.Text)-1);

      FloodCnt:=FloodCnt+Info.asInteger['UPD'];
      lblFlood.Caption:=IntToStr(FloodCnt);
      lblFlood.Refresh;

      if xAutoDisconnect.Checked or
         not xReqAutoRepeat.Checked then
        begin
        Info.asBoolean['closed']:=True;
        Disconnect;
        end;

      // Are all connections finished?
      if eConCnt.Text='0' then
        begin
        btnMultiFlood.Enabled:=True;

        LoopCnt := LoopCnt+1;
        maxLoops := StrToIntDef(eMaxLoops.Text,0);
        if maxLoops<=0 then maxLoops:=LoopCnt+1; // 0 = unlimited loops

        MyTime := GetTickTime - TestTime;
        MyLoop := GetTickTime - LoopTime;
        if (MyTime>0) and (MyLoop>0) then
          begin
          Log( IntToStr(LoopCnt)+'; '+
               IntToStr(length(CliCon))+'; '+
               IntToStr(LoopReq)+'; '+
               FloatToStr(Round(LoopReq/MyLoop*1000))+'; '+
               FloatToStr(Round((LoopReq/length(CliCon))/MyLoop*1000))+'; '+
               IntToStr(TestReqCount)+'; '+
               FloatToStr(Round(TestReqCount/MyTime*1000))+'; '+
               IntToStr(RepostCnt)+'; '+
               FloatToStr(Round(RepostCnt/TestReqCount*10000)/100),
               eServer.Text+'_'+ePort.Text );
          lblReqSec.Caption:=FloatToStr(Round(TestReqCount/MyTime*100000)/100);
          end
        else
          lblReqSec.Caption:='+++';

        // Do we have to repeat the whole thing once again?
        if xReqAutoRepeat.Checked and (LoopCnt<maxLoops) then
          begin
          // Update our MAX, LEFT and CNT values.
          try
            req:=StrToInt(eReqCnt.Text);
          except
            ShowMessage('Invalid value for Request count');
            Exit;
            end;
          try
            upd:=StrToInt(eUpdCnt.Text);
          except
            ShowMessage('Invalid value for Update frequency');
            Exit;
            end;

          for a := 1 to StrToIntDef(eConnPlus.Text,0) do
            SetupCliCon(length(CliCon),req,upd);

          eConCnt.Text:=IntToStr(length(CliCon));
          eConCnt.Enabled:=True;

          btnMultiFlood.Enabled:=False;
          eConCnt.Enabled:=False;
          { This loop will Post one request using each Client Module
            and re-open each connection by calling "Connect".
            We have to open the connections here again, because
            we chose to close them when they have finished their jobs. }
          myBox.Canvas.Brush.Color:=clBtnFace;
          myBox.Canvas.FillRect(Rect(0,0,myBox.Width,myBox.Height));

          LoopTime := GetTickCount;
          LoopReq := Length(CliReq) * req;
          TestReqCount := TestReqCount + LoopReq;

          for a:=0 to Length(CliReq)-1 do
            begin
            myBox.Canvas.Pen.Color:=clMaroon;
            myBox.Canvas.MoveTo(a,0);
            myBox.Canvas.LineTo(a,myBox.Height-1);
            with CliReq[a] do
              begin
              Request.Method:='GET';
              Request.Host:=Client.ServerAddr;
              Request.FileName:=RtcString(RequestLines.Strings[Info.asInteger['CNT'] mod RequestLines.Count]);
              // Need to Post with TRUE as parameter from our connection event
              Post(CliCon[a]=Sender, CliCon[a]);
              end;
            with CliCon[a] do
              begin
              with Info do
                begin
                asInteger['MAX']:=req;
                asInteger['CNT']:=req;
                asInteger['LEFT']:=req;
                asInteger['UPD']:=upd;
                if asBoolean['closed'] then
                  begin
                  asBoolean['closed']:=False;
                  if not AutoConnect then
                    Connect;
                  end;
                end;
              end;
            end;
          end
        else
          begin
          eConCnt.Enabled:=True;
          eConCnt.Text:=IntToStr(length(CliCon));

          MyTime := GetTickTime - TestTime;

          for a:=0 to length(CliCon)-1 do
            CliCon[a].Disconnect;

          if (MyTime>0) then
            ShowMessage('Test completed in '+IntToStr(MyTime)+' ms!'+#13#10+
                        'Speed = '+FloatToStr(Round(TestReqCount/MyTime*100000)/100)+' Req/sec'+#13#10+
                        'Error = '+FloatToStr(RepostCnt/TestReqCount*100)+' % ')
          else
            ShowMessage('Test completed in less than 1 ms!'+#13#10+
                        'Speed > '+FloatToStr(TestReqCount)+' Req/sec'+#13#10+
                        'Error = '+FloatToStr(Round(RepostCnt/TestReqCount*10000)/100)+' % ')
          end;
        end;
      end;
    end;
  end;

procedure TForm1.Panel2Click(Sender: TObject);
  begin
  ShellExecute(HInstance,'open','http://www.realthinclient.com','','',SW_NORMAL);
  end;

procedure TForm1.DataRequestRepostCheck(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(DataRequestRepostCheck)
  else
    begin
    TRtcDataClient(Sender).Request.Repost;
    RepostCnt:=RepostCnt+1;
    lblRepost.Caption:=IntToStr(RepostCnt);
    lblRepost.Refresh;
    end;
  end;

end.
