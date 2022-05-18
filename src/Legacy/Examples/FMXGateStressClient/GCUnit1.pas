unit GCUnit1;

(* This is a simple cross-platform Tool (created with Delphi 10.1 Belin using FireMonkey), 
   providing a simple way to test RTC Gate Clients running on all devices and stress-test 
   Applications built using TRtcGateway or TRtcHttpGateway components by dynamically 
   creating multiple TRtcHttpGateClient components (based on parameters), with an 
   easy-to-use Graphical User Interface and Graphical Status display for all Gate Clients.

   If you want to stress-test a standard RTC Gateway (like the "RtcSimpleGateway" Demo),
   simply update "MainGC" component properties (on the Form) to point at your Gateway.

   If you want to test a custom-built RTC Gateway with a custom login, sending
   something else than random data, you can modify events implemented on
   the "MainGC" component and update the "GCJobExecute" method as needed.

   After configuring the Client to work with your Gateway, simply start the App,
   click "GO" to create Clients and log in, then click "Send" to start sending
   and receiving data through the Gateway. Check button "Hint"s for more info.

   NOTE: If you click the "Close" button of the main form while a test is running,
   a clean-up job will be started in the background and the App will close automatically
   after that clean-up job has finished. That is OK and is intended as a clean shut-down.

   WARNING! Do NOT click the "Close" button more than once, unless you want to
   terminate the Client without closing connections, in which case you will most
   likely get to see Access Violations because you will be destroying objects which
   are still in use by background threads. You will also see Memory Leaks because
   background threads will be terminated before releasing the objects they've created.

   If you do NOT want to see Access Violations or Memory Leaks on App shut-down,
   awlays close all connections and allow background threads to finish before you Exit. *)

interface

{$include rtcDefs.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMX.StdCtrls,
  FMX.Controls.Presentation,

  rtcTypes,
  rtcSystem,

  rtcGateConst,
  rtcInfo,
  rtcConn,
  rtcGateCli,
  rtcDataCli,
  rtcLog,
  rtcTimer,

  rtcCrypt,
  rtcThrPool;

const
  cid_StressTest=999;

type
  TGCData=record
    iSent, iSDraw,
    iRead, iDraw,
    iErr, iEDraw,
    iErr2, iEDraw2:int64;
    iFin, iHave:boolean;
    OutBytes:RtcByteArray;
    iCS:TRtcCritSec;
    iGC:TRtcHttpGateClient;
    end;

  TGCLine=record
    id,y1,y2:integer;
    clr:TAlphaColor;
    end;

  TGCLineArr=array of TGCLine;

  TGCDataArr=array of TGCData;

  TGCMainForm = class(TForm)
    eClients: TLabel;
    MainGC: TRtcHttpGateClient;
    btnDec: TButton;
    btnInc: TButton;
    ePaintBox: TPaintBox;
    btnGo: TButton;
    btnRun: TButton;
    btnAPIs: TButton;
    btnPack: TButton;
    eStatus: TLabel;
    eError: TLabel;
    btnInOut: TButton;
    eSpeed: TLabel;
    GCJob: TRtcQuickJob;
    btnReset: TButton;
    ePackets: TLabel;
    procedure ePaintBoxResize(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure MainGCReadyToSend(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure MainGCDataFilter(Client: TRtcHttpGateClient;
      Data: TRtcGateClientData; var Wanted: Boolean);
    procedure QuickJobExecute(Data: TRtcValue);
    procedure btnIncClick(Sender: TObject);
    procedure btnDecClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ePaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MainGCAfterLoggedIn(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure MainGCAfterLoginFail(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure MainGCAfterLogOut(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure MainGCBeforeLogIn(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure MainGCDataReceivedGUI(Client: TRtcHttpGateClient;
      Data: TRtcGateClientData);
    procedure MainGCDataReceived(Client: TRtcHttpGateClient;
      Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);
    procedure MainGCReadyAfterReset(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure MainGCStreamReset(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
    procedure btnRunClick(Sender: TObject);
    procedure btnAPIsClick(Sender: TObject);
    procedure btnPackClick(Sender: TObject);
    procedure btnInOutClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NumClients,
    NumPackets,
    PACKET_SIZE:integer;
    Running:boolean;
    useAPIs:integer;
    useIO:integer;

    StartSend,
    TotalSent, UpdSent, FromRead,
    TotalRead, UpdRead,
    TotalErr,  UpdErr:int64;

    UpdActive:boolean;

    EV:TRtcEvent;
    CS:TRtcCritSec;
    Lines,LinesCopy:TGCLineArr;
    MaxLines,LineCount,LinePos,CopyCount,CopyPos:integer;
    Bmp:TBitmap;
    Active:boolean;

    GCData:TGCDataArr;
    Divisor,
    GCWid,GCHig:integer;

    LoopCount,
    TotalDone:integer;

    DataBytes,
    SendBytes,
    TmpBytes:RtcByteArray;
    RtcTim:TRtcTimer;

    procedure UpdateTimerEvent;
    procedure SendDataNow(id:integer);
    function BmpGetLines:boolean;
    procedure BmpDrawLines;
    procedure DoItAgain;
    procedure UpdateCount;
    procedure PaintLine(id,y1,y2:integer;clr:TAlphaColor; incSent:integer=0; incRead:integer=0; incErr:integer=0);
    procedure StatusUpdate(id:integer; clr:TAlphaColor; resetRead:boolean);
    procedure InOutUpdate(id:integer);
  end;

var
  GCMainForm: TGCMainForm;

implementation

{$R *.fmx}

procedure TGCMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  if not Active or (btnGo.Text='!!') then
    begin
    TRtcTimer.Stop(RtcTim);
    CanClose:=True;
    end
  else
    begin
    CanClose:=False;
    btnGOClick(nil);
    end;
  end;

procedure TGCMainForm.FormCreate(Sender: TObject);
  var
    rnd:TRtcISAAC;
  begin
  {$IFNDEF WINDOWS}
  btnAPIS.Visible:=False;
  {$ENDIF}
  Running:=False;
  Active:=False;
  UpdActive:=False;
  Divisor:=10;
  useAPIs:=0;
  useIO:=0;

  TotalDone:=0;
  TotalSent:=0;
  TotalRead:=0;
  TotalErr:=0;
  UpdSent:=-1;
  UpdRead:=-1;
  UpdErr:=-1;
  FromRead:=0;
  StartSend:=0;

  EV:=TRtcEvent.Create(True,True);
  CS:=TRtcCritSec.Create;
  MaxLines:=10000;
  SetLength(Lines,MaxLines);
  SetLength(LinesCopy,MaxLines);
  LinePos:=0;
  LineCount:=0;
  CopyPos:=0;
  CopyCount:=0;
  Bmp:=nil;

  { Uncomment the next 2 lines if you want to Limit
    the size of TCP/IP packets used by each Client.
    If no limits are set, maximum buffer sizes are used. }
  // rtcSetupReceivingParams(1500,1,1);
  // rtcSetupSendingParams(1500,1,1);

  rnd:=TRtcISAAC.Create(True);
  try
    DataBytes:=rnd.RND($FFFFFF);
  finally
    rnd.Free;
    end;

  PACKET_SIZE:=1000;
  btnPack.Text:=IntToStr(PACKET_SIZE div 1000)+' KB';
  UpdateCount;

  RtcTim:=TRtcTimer.Create(False);
  TRtcTimer.Enable(RtcTim,100,UpdateTimerEvent);
  end;

procedure TGCMainForm.FormDestroy(Sender: TObject);
  begin
  FreeAndNil(Bmp);
  SetLength(Lines,0);
  SetLength(LinesCopy,0);
  FreeAndNil(CS);
  FreeAndNil(EV);
  end;

procedure TGCMainForm.UpdateCount;
  begin
  if Divisor<1 then Divisor:=1;
  GCWid:=Divisor;
  if Divisor<2 then
    GCHig:=Divisor
  else if Divisor>20 then
    GCHig:=10
  else
    GCHig:=Divisor div 2;
  NumClients:=trunc(ePaintBox.Width) div GCWid;
  if NumClients<1 then NumClients:=1;
  NumPackets:=trunc(ePaintBox.Height) div GCHig - 6;
  if NumPackets<1 then NumPackets:=1;
  eClients.Text:=IntToStr(NumClients)+' Cli';
  ePackets.Text:=IntToStr(NumPackets)+' Pkg';
  end;

procedure TGCMainForm.UpdateTimerEvent;
  var
    mySpeed:int64;
  begin
  if BmpGetLines then
    ePaintBox.Repaint;
  if (TotalSent>UpdSent) or (TotalRead>UpdRead) then
    begin
    UpdSent:=TotalSent;
    UpdRead:=TotalRead;
    if (StartSend>0) and (GetTickTime64>StartSend) and (UpdRead>FromRead) then
      begin
      mySpeed:=((UpdRead-FromRead)*PACKET_SIZE*8) div (GetTickTime64-StartSend);
      if mySpeed<=10000 then
        eSpeed.Text:=IntToStr(mySpeed)+' KBits'
      else
        eSpeed.Text:=Float2Str((mySpeed div 10)/100)+' MBits';
      end
    else
      eSpeed.Text:='--';
    if UpdSent>0 then
      eStatus.Text:=IntToStr(UpdRead)+'/'+IntToStr(UpdSent)+'='+IntToStr(100*UpdRead div UpdSent)+'%'
    else
      eStatus.Text:='--';
    end;
  if TotalErr>UpdErr then
    begin
    UpdErr:=TotalErr;
    eError.Text:=IntToStr(UpdErr);
    end;
  if UpdActive<>Active then
    begin
    if Active=True then
      begin
      UpdActive:=True;
      btnGo.Text:='STOP'
      end
    else if length(GCData)=0 then
      begin
      UpdActive:=False;
      btnGo.Text:='GO';
      end;
    end;
  Invalidate;
  end;

procedure TGCMainForm.btnPackClick(Sender: TObject);
  begin
  PACKET_SIZE:=PACKET_SIZE*2;
  if PACKET_SIZE>$FFFFF0 then PACKET_SIZE:=1000;
  btnPack.Text:=IntToStr(PACKET_SIZE div 1000)+' KB';
  if Active then GCJob.Post(TRtcIntegerValue.Create(7));
  end;

procedure TGCMainForm.ePaintBoxPaint(Sender: TObject; Canvas: TCanvas);
  begin
  if assigned(Bmp) then
    begin
    BmpDrawLines;
    if Canvas.BeginScene then
      try
        Canvas.DrawBitmap(Bmp,RectF(0,0,Bmp.Width,Bmp.Height),RectF(0,0,Bmp.Width,Bmp.Height),1,True);
      finally
        Canvas.EndScene;
      end;
    end;
  end;

procedure TGCMainForm.ePaintBoxResize(Sender: TObject);
  begin
  if Visible and not Active then
    UpdateCount;
  end;

procedure TGCMainForm.btnIncClick(Sender: TObject);
  begin
  if not Active and (Divisor>1) then
    begin
    Divisor:=trunc(ePaintBox.Width) div (NumClients+1);
    if Divisor<1 then Divisor:=1;
    UpdateCount;
    end;
  end;

procedure TGCMainForm.btnInOutClick(Sender: TObject);
  begin
  Inc(useIO);
  if useIO>4 then
    useIO:=0;
  case useIO of
    0:btnInOut.Text:='IXO';
    1:btnInOut.Text:='IN';
    2:btnInOut.Text:='OUT';
    3:btnInOut.Text:='FULL';
    4:btnInOut.Text:='NONE';
    end;
  GCJob.Post(TRtcIntegerValue.Create(4));
  end;

procedure TGCMainForm.btnResetClick(Sender: TObject);
  begin
  if Active then
    GCJob.Post(TRtcIntegerValue.Create(7));
  end;

procedure TGCMainForm.btnRunClick(Sender: TObject);
  begin
  Running:=not Running;
  if Running then
    begin
    btnRun.Text:='Wait';
    GCJob.Post(TRtcIntegerValue.Create(3));
    end
  else
    btnRun.Text:='Send';
  end;

procedure TGCMainForm.btnAPIsClick(Sender: TObject);
  begin
  Inc(useAPIs);
  if useAPIs>3 then
    useAPIs:=0;
  case useAPIs of
    0:btnAPIs.Text:='MIX';
    1:btnAPIs.Text:='AS';
    2:btnAPIs.Text:='BS';
    3:btnAPIs.Text:='HT';
    end;
  GCJob.Post(TRtcIntegerValue.Create(4));
  end;

procedure TGCMainForm.btnDecClick(Sender: TObject);
  var
    NewNumClients:integer;
  begin
  if not Active and (NumClients>1) then
    begin
    NewNumClients:=NumClients;
    repeat
      Dec(NewNumClients);
      Divisor:=trunc(ePaintBox.Width) div NewNumClients;
      UpdateCount;
      until NumClients<=NewNumClients;
    end;
  end;

procedure TGCMainForm.btnGoClick(Sender: TObject);
  var
    i:integer;
  begin
  if not Active then
    begin
    if btnGO.Text<>'>>' then
      begin
      btnGO.Text:='>>';

      if assigned(Bmp) then FreeAndNil(Bmp);
      Bmp:=TBitmap.Create(trunc(ePaintBox.Width),trunc(ePaintBox.Height));
      Bmp.Clear(TAlphaColors.Lightgray);

      for I := 0 to NumClients do
        begin
        PaintLine(I,-4,NumPackets,TAlphaColors.Lightgray);
        PaintLine(I,NumPackets,NumPackets+4,TAlphaColors.Darkgray);
        end;
      GCJob.Post(TRtcIntegerValue.Create(0));
      end;
    end
  else
    begin
    if btnGO.Text<>'!!' then
      begin
      btnGO.Text:='!!';
      if Sender=nil then
        GCJob.Post(TRtcIntegerValue.Create(10))
      else
        GCJob.Post(TRtcIntegerValue.Create(1));
      end
    else if btnGO.Text<>'>>' then
      begin
      btnGO.Text:='>>';
      GCJob.Post(TRtcIntegerValue.Create(0));
      end;
    end;
  end;

procedure TGCMainForm.DoItAgain;
  begin
  if not Active then Exit;
  Inc(LoopCount);
  btnGO.Text:=IntToStr(LoopCount);
  GCJob.Post(TRtcIntegerValue.Create(2));
  end;

procedure TGCMainForm.PaintLine(id, y1,y2: integer; clr: TAlphaColor; incSent:integer=0; incRead:integer=0; incErr:integer=0);
  begin
  EV.WaitFor(INFINITE);
  CS.Acquire;
  try
    Lines[LinePos].id:=id;
    Lines[LinePos].y1:=y1+4;
    Lines[LinePos].y2:=y2+4;
    Lines[LinePos].clr:=clr;
    Inc(TotalSent,incSent);
    Inc(TotalRead,incRead);
    Inc(TotalErr,incErr);
    Inc(LinePos);
    if LineCount<LinePos then LineCount:=LinePos;
    if LinePos>=MaxLines then LinePos:=0;
  finally
    CS.Release;
    end;
  end;

function TGCMainForm.BmpGetLines:boolean;
  var
    LinesTmp:TGCLineArr;
  begin
  Result:=CopyCount>0;
  if not Result then
    begin
    EV.ResetEvent;
    CS.Acquire;
    try
      if LineCount=0 then Exit;

      LinesTmp:=Lines;
      Lines:=LinesCopy;
      LinesCopy:=LinesTmp;

      CopyCount:=LineCount;
      CopyPos:=LinePos;
      LineCount:=0;
      LinePos:=0;
    finally
      CS.Release;
      EV.SetEvent;
      end;
    Result:=CopyCount>0;
    end;
  end;

procedure TGCMainForm.BmpDrawLines;
  var
    i:integer;
    p1,p2:TPointF;
  begin
  if CopyCount=0 then Exit;

  with Bmp.Canvas do
    if BeginScene then
      try
        if GCWid<=GCHig*2 then
          Stroke.Thickness:=GCWid
        else
          Stroke.Thickness:=GCHig*2;
        Stroke.Kind:=TBrushKind.Solid;
        Stroke.Dash:=TStrokeDash.Solid;
        if CopyPos<CopyCount then
          for i := CopyPos to CopyCount-1 do
            begin
            Stroke.Color:=LinesCopy[i].clr;
            p1.X:=LinesCopy[i].id*GCWid+GCWid/2;
            p2.X:=p1.X;
            p1.Y:=LinesCopy[i].y1*GCHig;
            p2.Y:=LinesCopy[i].y2*GCHig;
            {$IFDEF WINDOWS}
              p1.Y:=p1.Y+Stroke.Thickness/2;
              p2.Y:=p2.Y-Stroke.Thickness/2;
            {$ENDIF}
            DrawLine(p1,p2,1);
            end;
        if CopyPos>0 then
          for i := 0 to CopyPos-1 do
            begin
            Stroke.Color:=LinesCopy[i].clr;
            p1.X:=LinesCopy[i].id*GCWid+GCWid/2;
            p2.X:=p1.X;
            p1.Y:=LinesCopy[i].y1*GCHig;
            p2.Y:=LinesCopy[i].y2*GCHig;
            {$IFDEF WINDOWS}
              p1.Y:=p1.Y+Stroke.Thickness/2;
              p2.Y:=p2.Y-Stroke.Thickness/2;
            {$ENDIF}
            DrawLine(p1,p2,1);
            end;
        EV.ResetEvent;
        CS.Acquire;
        try
          CopyPos:=0;
          CopyCount:=0;
        finally
          CS.Release;
          EV.SetEvent;
          end;
      finally
        EndScene;
        end;
  end;

procedure TGCMainForm.SendDataNow(id: integer);
  var
    t:boolean;
  begin
  InOutUpdate(id);
  with GCData[id] do
    begin
    if not iGC.State.ReadyToSend then
      begin
      PaintLine(id,iSent,iSent+1,TAlphaColors.Magenta);
      end
    else if iFin then
      begin
      PaintLine(id,iSent,iSent+1,TAlphaColors.White);
      end
    else if iSent>=NumPackets then
      begin
      PaintLine(id,iSent,iSent+1,TAlphaColors.Orange);
      end
    else if not Running then
      begin
      PaintLine(id,iSent,iSent+1,TAlphaColors.Black);
      end
    else
      begin
      if iGC.Tag mod 2=0 then
        t:=iSent mod 2=0
      else
        t:=iSent mod 2=1;
      if t then
        begin
        if OutBytes<>SendBytes then OutBytes:=SendBytes;
        if iGC.SendToMyGroup(1,cid_StressTest,OutBytes) then
          begin
          PaintLine(id,iSent,iSent+1,TAlphaColors.Blue,1);
          iCS.Acquire;
          try
            Inc(iSent);
          finally
            iCS.Release;
            end;
          end
        else if iGC.UsersInMyGroup(1)=0 then
          begin
          if not iGC.AddFriend(iGC.MyUID) then
            PaintLine(id,iSent,iSent+1,TAlphaColors.Yellow,1)
          else if not iGC.AddUserToMyGroup(1,iGC.MyUID) then
            PaintLine(id,iSent,iSent+1,TAlphaColors.White,1)
          else
            PaintLine(id,iSent,iSent+1,TAlphaColors.Red);
          end
        else
          PaintLine(id,iSent,iSent+1,TAlphaColors.Blueviolet);
        end
      else
        begin
        if OutBytes<>SendBytes then OutBytes:=SendBytes;
        if iGC.SendBytes(iGC.MyUID,0,cid_StressTest,OutBytes) then
          begin
          PaintLine(id,iSent,iSent+1,TAlphaColors.Mediumblue,1);
          iCS.Acquire;
          try
            Inc(iSent);
          finally
            iCS.Release;
            end;
          end
        else
          PaintLine(id,iSent,iSent+1,TAlphaColors.Blueviolet);
        end;
      end;
    end;
  end;

procedure TGCMainForm.InOutUpdate(id: integer);
  begin
  with GCData[id] do
    begin
    case iGC.State.InputState of
      ins_Closed: PaintLine(id,-3,-2,TAlphaColors.Red);
      ins_Connecting: PaintLine(id,-3,-2,TAlphaColors.Yellow);
      ins_Prepare: PaintLine(id,-3,-2,TAlphaColors.White);
      ins_Start: PaintLine(id,-3,-2,TAlphaColors.Gray);
      ins_Idle: PaintLine(id,-3,-2,TAlphaColors.Black);
      ins_Recv: PaintLine(id,-3,-2,TAlphaColors.Lightgreen);
      ins_Done: PaintLine(id,-3,-2,TAlphaColors.Green);
      else PaintLine(id,-3,-2,TAlphaColors.Pink);
      end;

    case iGC.State.OutputState of
      outs_Closed: PaintLine(id,-2,-1,TAlphaColors.Red);
      outs_Connecting: PaintLine(id,-2,-1,TAlphaColors.Yellow);
      outs_Prepare: PaintLine(id,-2,-1,TAlphaColors.White);
      outs_Start: PaintLine(id,-2,-1,TAlphaColors.Gray);
      outs_Idle: PaintLine(id,-2,-1,TAlphaColors.Black);
      outs_Send: PaintLine(id,-2,-1,TAlphaColors.LightBlue);
      outs_Done: PaintLine(id,-2,-1,TAlphaColors.Blue);
      else PaintLine(id,-2,-1,TAlphaColors.Pink);
      end;

    if not iGC.State.LoggedIn then
      PaintLine(id,-1,0,TAlphaColors.Red)
    else if iGC.State.InputReady then
      begin
      if iGC.State.OutputReady then
        PaintLine(id,-1,0,TAlphaColors.Lightgreen)
      else
        PaintLine(id,-1,0,TAlphaColors.Blue);
      end
    else if iGC.State.OutputReady then
      PaintLine(id,-1,0,TAlphaColors.Darkgreen)
    else
      PaintLine(id,-1,0,TAlphaColors.Maroon);
    end;
  end;

procedure TGCMainForm.StatusUpdate(id: integer; clr: TAlphaColor; resetRead:boolean);
  begin
  InOutUpdate(id);
  with GCData[id] do
    begin
    if resetRead then
      begin
      iCS.Acquire;
      try
        iSent:=iRead;
        iSDraw:=iDraw;
      finally
        iCS.Release;
        end;
      end;
    if iRead<NumPackets then
      PaintLine(id,iRead,NumPackets+1,clr)
    else
      PaintLine(id,NumPackets,NumPackets+2,clr);
    end;
  end;

procedure TGCMainForm.MainGCAfterLoggedIn(Client: TRtcHttpGateClient;
    State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  StatusUpdate(Client.Tag,TAlphaColors.Green,True);
  if not Client.AddFriend(Client.MyUID) then
    StatusUpdate(Client.Tag,TAlphaColors.Blueviolet,False)
  else if not Client.AddUserToMyGroup(1,Client.MyUID) then
    StatusUpdate(Client.Tag,TAlphaColors.Chocolate,False);
  end;

procedure TGCMainForm.MainGCAfterLoginFail(Client: TRtcHttpGateClient;
    State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  StatusUpdate(Client.Tag,TAlphaColors.Pink,False);
  end;

procedure TGCMainForm.MainGCAfterLogOut(Client: TRtcHttpGateClient;
    State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  StatusUpdate(Client.Tag,TAlphaColors.Red,False);
  end;

procedure TGCMainForm.MainGCBeforeLogIn(Client: TRtcHttpGateClient;
    State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  StatusUpdate(Client.Tag,TAlphaColors.Yellow,False);
  end;

procedure TGCMainForm.MainGCDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  if Data.CallID=cid_StressTest then
    if Data.Footer then
      begin
      with GCData[Client.Tag] do
        begin
        Wanted:=True;
        if not iFin then
          begin
          Inc(iRead);
          if (length(Data.Content)>length(DataBytes)) then
            Inc(iErr)
          else if not CompareMem(@Data.Content[0],@DataBytes[0],length(Data.Content)) then
            begin
            Inc(iErr);
            Inc(iErr2);
            end;
          end;
        end;
      end
    else if Data.Header then
      Data.ToBuffer:=True;
  end;

procedure TGCMainForm.MainGCDataReceived(Client: TRtcHttpGateClient;
    Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);
  begin
  with GCData[Client.Tag] do
    if iFin then
      PaintLine(Client.Tag,iDraw,iDraw+1,TAlphaColors.Lightskyblue)
    else if iRead>iDraw then
      begin
      iDraw:=iRead;
      if iErr>iEDraw then
        begin
        iEDraw:=iErr;
        iEDraw2:=iErr2;
        PaintLine(Client.Tag,0,iEDraw2,TAlphaColors.Magenta,0,0,1);
        PaintLine(Client.Tag,iEDraw2,iEDraw,TAlphaColors.Red);
        end;
      if iSent>iSDraw then
        begin
        iSDraw:=iSent;
        if iSDraw>iDraw then
          PaintLine(Client.Tag,iSDraw-1,iSDraw,TAlphaColors.Aqua);
        end;
      if iDraw>=NumPackets then
        begin
        iFin:=True;
        PaintLine(Client.Tag,iEDraw,iDraw+1,TAlphaColors.Lightgreen,0,1);
        WantGUI:=True;
        end
      else if Data.GroupID=1 then
        PaintLine(Client.Tag,iDraw-1,iDraw,TAlphaColors.Darkseagreen,0,1)
      else
        PaintLine(Client.Tag,iDraw-1,iDraw,TAlphaColors.Mediumseagreen,0,1);
      end;
  end;

procedure TGCMainForm.MainGCDataReceivedGUI(Client: TRtcHttpGateClient;
    Data: TRtcGateClientData);
  begin
  Inc(TotalDone);
  if TotalDone>=NumClients then
    DoItAgain;
  end;

procedure TGCMainForm.MainGCReadyAfterReset(Client: TRtcHttpGateClient;
    State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  StatusUpdate(Client.Tag,TAlphaColors.Darkgreen,True);
  if not Client.AddFriend(Client.MyUID) then
    StatusUpdate(Client.Tag,TAlphaColors.Blueviolet,False)
  else if not Client.AddUserToMyGroup(1,Client.MyUID) then
    StatusUpdate(Client.Tag,TAlphaColors.Chocolate,False);
  end;

procedure TGCMainForm.MainGCReadyToSend(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  SendDataNow(Client.Tag);
  end;

procedure TGCMainForm.MainGCStreamReset(Client: TRtcHttpGateClient;
    State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  StatusUpdate(Client.Tag,TAlphaColors.Maroon,False);
  end;

procedure TGCMainForm.QuickJobExecute(Data: TRtcValue);
  var
    i:integer;
    inStream,
    outStream:boolean;
  begin
  case Data.asInteger of
    0:begin
      if Active then Exit;

      if length(GCData)<>0 then Exit;

      SetLength(GCData,NumClients);

      if Running then StartSend:=GetTickTime64;
      FromRead:=0;
      LoopCount:=0;
      TotalDone:=0;
      TotalSent:=0;
      TotalRead:=0;
      TotalErr:=0;
      UpdSent:=-1;
      UpdRead:=-1;
      UpdErr:=-1;

      TmpBytes:=Copy(DataBytes,0,PACKET_SIZE);
      SendBytes:=TmpBytes;

      for I := 0 to length(GCData)-1 do
        begin
        with GCData[i] do
          begin
          if not iHave then
            begin
            SetLength(OutBytes,0);
            if iCS=nil then iCS:=TRtcCritSec.Create;
            if IGC=nil then iGC:=TRtcHttpGateClient.Create(self);
            with iGC do
              begin
              Tag:=i;
              {$IFDEF WINDOWS}
              case useAPIs of
                0:begin
                  if Tag=0 then
                    UseProxy:=True
                  else case (Tag-1) mod 3 of
                    1:UseBlocking:=True;
                    2:UseWinHTTP:=True;
                    end;
                  end;
                2:useBlocking:=True;
                3:UseWinHTTP:=True;
                end;
              {$ENDIF}

              inStream:=False;
              outStream:=False;
              case useIO of
                0:case Tag mod 4 of // IXO
                    0:inStream:=True;
                    1:outStream:=True;
                    2:begin
                      inStream:=True;
                      outStream:=True;
                      end;
                    end;
                1: inStream:=True;
                2: outStream:=True;
                3: begin
                   inStream:=True;
                   outStream:=True;
                   end;
                end;

              if not inStream then
                StreamBlockSizeIn:=1;
              if not outStream then
                StreamBlockSizeOut:=1;

              GateAddr:=MainGC.GateAddr;
              GatePort:=MainGC.GatePort;
              GateFileName:=MainGC.GateFileName;
              GatePrimaryKey:=MainGC.GatePrimaryKey;
              GateSecondaryKey:=MainGC.GateSecondaryKey;
              GateUserAuth:=MainGC.GateUserAuth;
              GateUserInfo:=MainGC.GateUserInfo;

              AfterLoggedIn:=MainGC.AfterLoggedIn;
              AfterLoginFail:=MainGC.AfterLoginFail;
              AfterLogOut:=MainGC.AfterLogOut;
              BeforeLogIn:=MainGC.BeforeLogIn;

              OnDataFilter:=MainGC.OnDataFilter;
              OnDataReceived:=MainGC.OnDataReceived;
              OnDataReceivedGUI:=MainGC.OnDataReceivedGUI;
              OnReadyAfterReset:=MainGC.OnReadyAfterReset;
              OnReadyToSend:=MainGC.OnReadyToSend;
              OnStreamReset:=MainGC.OnStreamReset;

              iHave:=True;

              AutoLogin:=True;
              end;
            end;
          end;
        end;
      Active:=True;
      end;
    1,10:begin
      if not Active then Exit;
      Active:=False;
      for I := 0 to length(GCData)-1 do
        with GCData[i] do
          if iHave then
            begin
            iHave:=False;
            iFin:=True;
            if assigned(iGC) then
              begin
              iGC.AutoLogin:=False;
              FreeAndNil(GCData[i].iGC);
              FreeAndNil(GCData[i].iCS);
              end;
            SetLength(OutBytes,0);
            end;
      SetLength(GCData,0);
      if Data.asInteger=10 then
        Close;
      end;
    7:begin
      if not Active then Exit;

      if length(SendBytes)<>PACKET_SIZE then
        begin
        TmpBytes:=Copy(DataBytes,0,PACKET_SIZE);
        SendBytes:=TmpBytes;
        end;
      if Running then StartSend:=GetTickTime64;
      TotalSent:=0;
      TotalRead:=0;
      TotalErr:=0;
      UpdSent:=-1;
      UpdRead:=-1;
      UpdErr:=-1;
      FromRead:=0;

      for I := 0 to length(GCData)-1 do
        with GCData[i] do
          if assigned(iGC) then
            iGC.ResetStreams(False);

      end;
    8:begin
      if not Active then Exit;

      if length(SendBytes)<>PACKET_SIZE then
        begin
        TmpBytes:=Copy(DataBytes,0,PACKET_SIZE);
        SendBytes:=TmpBytes;
        end;
      if Running then StartSend:=GetTickTime64;
      TotalSent:=0;
      TotalRead:=0;
      TotalErr:=0;
      UpdSent:=-1;
      UpdRead:=-1;
      UpdErr:=-1;
      FromRead:=0;

      for I := 0 to length(GCData)-1 do
        with GCData[i] do
          if assigned(iGC) then
            iGC.ResetStreams(True);
      end;
    9:begin
      if not Active then Exit;

      if length(SendBytes)<>PACKET_SIZE then
        begin
        TmpBytes:=Copy(DataBytes,0,PACKET_SIZE);
        SendBytes:=TmpBytes;
        end;
      if Running then StartSend:=GetTickTime64;
      TotalSent:=0;
      TotalRead:=0;
      TotalErr:=0;
      UpdSent:=-1;
      UpdRead:=-1;
      UpdErr:=-1;
      FromRead:=0;

      for I := 0 to length(GCData)-1 do
        with GCData[i] do
          if assigned(iGC) then
            begin
            iGC.AutoLogin:=False;
            iGC.AutoLogin:=True;
            end;
      end;
    2:begin
      if not Active then Exit;
      for I := 0 to length(GCData)-1 do
        with GCData[i] do if iHave then
          begin
          if assigned(iCS) then
            begin
            iCS.Acquire;
            try
              iRead:=0; iDraw:=0;
              iErr:=0;  iEDraw:=0;
              iErr2:=0; iEDraw2:=0;
              iSent:=0; iSDraw:=0;
            finally
              iCS.Release;
              end;
            end;
          PaintLine(I,-4,NumPackets,TAlphaColors.Lightgray);
          PaintLine(I,NumPackets,NumPackets+2,TAlphaColors.Darkgray);
          end;
      for I := 0 to length(GCData)-1 do
        with GCData[i] do if iHave then
          begin
          if assigned(iGC) and iGC.Ready then
            begin
            if iGC.State.ReadyToSend then
              PaintLine(I,1,NumPackets,TAlphaColors.Green)
            else
              PaintLine(I,1,NumPackets,TAlphaColors.Darkgreen);
            end
          else
            PaintLine(I,1,NumPackets,TAlphaColors.Maroon);
          end;
      TotalDone:=0;
      for I := 0 to length(GCData)-1 do
        with GCData[i] do
          if iHave then
            begin
            iFin:=False;
            SendDataNow(i);
            end;
      end;
    3:begin
      if not (Active and Running) then Exit;
      StartSend:=GetTickTime64;
      FromRead:=UpdRead;
      for I := 0 to length(GCData)-1 do
        if GCData[i].iHave then SendDataNow(i);
      end;
    4:begin
      if not Active then Exit;

      if length(SendBytes)<>PACKET_SIZE then
        begin
        TmpBytes:=Copy(DataBytes,0,PACKET_SIZE);
        SendBytes:=TmpBytes;
        end;
      if Running then StartSend:=GetTickTime64;
      TotalSent:=0;
      TotalRead:=0;
      TotalErr:=0;
      UpdSent:=-1;
      UpdRead:=-1;
      UpdErr:=-1;
      FromRead:=0;

      for I := 0 to length(GCData)-1 do
        if GCData[i].iHave and assigned(GCData[i].iGC) then
        with GCData[i].iGC do
          begin
          {$IFDEF WINDOWS}
          UseProxy:=False;
          UseWinHTTP:=False;
          UseBlocking:=False;
          case useAPIs of
            0:begin
              if Tag=0 then
                UseProxy:=True
              else case (Tag-1) mod 3 of
                1:UseBlocking:=True;
                2:UseWinHTTP:=True;
                end;
              end;
            2:useBlocking:=True;
            3:UseWinHTTP:=True;
            end;
          {$ENDIF}
          inStream:=False;
          outStream:=False;
          case useIO of
            0:case Tag mod 4 of // IXO
                0:inStream:=True;
                1:outStream:=True;
                2:begin
                  inStream:=True;
                  outStream:=True;
                  end;
                end;
            1: inStream:=True;
            2: outStream:=True;
            3: begin
               inStream:=True;
               outStream:=True;
               end;
            end;
          if inStream then
            StreamBlockSizeIn:=0
          else
            StreamBlockSizeIn:=1;
          if outStream then
            StreamBlockSizeOut:=0
          else
            StreamBlockSizeOut:=1;
          StatusUpdate(i,TAlphaColors.Yellow,False);
          ResetStreams;
          end;
      end;
    end;
  end;

initialization
StartLog;
Log('Starting up!');
finalization
Log('Shutting down ...');
end.
