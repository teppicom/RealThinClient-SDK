unit MainGateClientForm;

{ This Client is configured to work with the "RTCSimpleGateway" running on Localhost and Port 80.

  Check the "GateCli:TRtcHttpGateClient" component and set at least "GateAddr" and "GatePort"
  properties to your Gateway's Address and Port number if you want to access a Gateway running
  on a different PC and/or Port number.

  If you want to test this Client with the "ChatServer" Project, make sure to
  set the following two properties on the "GateCli:TRtcHttpGateClient" component:

  GatePrimaryKey := "MyPrimaryChatKey"
  GateUserAuth := "MyChatClient"

  See description in the "ChatServer" or "ChatClient" Projects for the explanation.

  Also ... if the Client has to work on a Systems with active Anti-Virus Software,
  always set the "StreamBlockSizeOut" property on the "GateCli:TRtcHttGateClient"
  component to the maximum number buffer size you want to allow Anti-Virus Software to use
  per outging stream. This limit is achieved by limiting the outgoing request size and
  forcing the Client to wait for a response from the Gateway before more data is sent. }

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ClipBrd,

  ExtCtrls, Spin, Buttons,

  rtcTypes, rtcSystem,
  rtcInfo, rtcLog, rtcConn,

  rtcGateConst,
  rtcGateCli,

  rtcUdpSrv,
  rtcUdpCli,
  rtcDataCli;

{$include rtcDeploy.inc}

{ Automatically start a new Client and fill up the Screen with Client
  instances when the user clicks "GO" for the 1st time? }
{.$DEFINE USE_AUTOSTART}

{ Use UDP to make all running clients "synchronized" by sending
  messages to all other Clients on this PC and inside LAN? }
{.$DEFINE USE_UDP}

const
  { Max number of lines to fill with new Test Clients when using Auto-Start.
    Set to a high number if you want the whole Screen to be filled. }
  AUTOSTART_LINES = 4;

  // "CallID" used in this test Client for sending a file
  cid_TestFileSend = 127;
  cid_AddMeFriend = 128;

type
  TMsgType=(msg_Input,msg_Output,msg_Speed,msg_Error,msg_Status,msg_Group);

type
  TGateClientForm = class(TForm)
    MainPanel: TPanel;
    shInput: TShape;
    shOutput: TShape;
    lblSendBuffSize: TLabel;
    lblRecvBufferSize: TLabel;
    btnLogIN: TSpeedButton;
    btnSendFile: TSpeedButton;
    StatusUpdate: TTimer;
    InfoPanel: TPanel;
    l_Status1: TLabel;
    l_Status2: TLabel;
    StartAnother: TTimer;
    btnCLR: TLabel;
    udpServer: TRtcUdpServer;
    udpClient: TRtcUdpClient;
    btnClose: TLabel;
    eYourID: TSpeedButton;
    eToID: TSpeedButton;
    btnReset: TSpeedButton;
    l_Status3: TLabel;
    GateCli: TRtcHttpGateClient;
    GCM: TRtcGateClientLink;
    btnHardReset: TSpeedButton;
    GCThread: TRtcGCThread;
    l_Groups: TLabel;
    shState: TShape;

    procedure btnLogINClick(Sender: TObject);
    procedure btnCLRClick(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAddUserClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StatusUpdateTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InfoPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure InfoPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure InfoPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnCloseClick(Sender: TObject);
    procedure eToIDExit(Sender: TObject);
    procedure btnSoftResetClick(Sender: TObject);
    procedure eYourIDClick(Sender: TObject);
    procedure eToIDClick(Sender: TObject);
    procedure StartAnotherTimer(Sender: TObject);
    procedure udpServerDataReceived(Sender: TRtcConnection);

    procedure GCMAfterLoggedInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure GCMAfterLoginFailGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure GCMAfterLogOutGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure GCMBeforeLogInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure GCMStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure GCMDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure GCMInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure GCMInfoReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI, WantBackThread:Boolean);
    procedure GCMReadyToSend(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread:Boolean);

    procedure btnHardResetClick(Sender: TObject);
    procedure GCMReadyToSendGUI(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo);
    procedure GCThreadDataReceived(Client: TRtcHttpGateClient;
      Data: TRtcGateClientData; var WantGUI: Boolean);
    procedure GCMDataReceived(Client: TRtcHttpGateClient;
      Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);
    procedure GCThreadInfoReceived(Client: TRtcHttpGateClient;
      Data: TRtcGateClientData; var WantGUI: Boolean);
    procedure GCThreadReadyToSend(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI: Boolean);
    procedure GCMAfterClientRemoved;
    procedure GCMAfterLoggedIn(Client: TRtcHttpGateClient;
      State: TRtcGateClientStateInfo; var WantGUI,
      WantBackThread: Boolean);

  public
    AutoStart, Used:boolean;
    xAsync,
    xRepeatSend:boolean;
    xUseAPI:byte;
    xAVFriendly:byte;

    FCS:TRtcCritSec;
    sStatus1,
    sStatus2,
    sStatus3,
    sGroups,
    sSend:String;

    LastUID:TGateUID;
    MyLOGName:String;

    MyFileName:String;

    FUserList:String;
    FUserListNew:boolean;

    FSendingFile:boolean;
    FLoginStart,
    FSendFileStart,
    FSendFileLastTime:TAppRunTime;
    FSendFileLastSize:Cardinal;
    CntReset:integer;

    FSendFileName:String;
    FSendFileLoc,
    FSendFileSize:int64;

    InGroupCnt,OutGroupCnt:integer;
    InGroupMe,OutGroupMe:boolean;

    procedure PrintMsg(const s:String; t:TMsgType);

    procedure DoSendFile;

    procedure StartNext;
  end;

var
  GateClientForm: TGateClientForm;

implementation

{$R *.dfm}

function FillZero(const s:RtcString;len:integer):RtcString;
  begin
  Result:=s;
  while length(Result)<len do
    Result:='0'+Result;
  end;

function Time2Str(v:TDateTime):RtcString;
  var
    hh,mm,ss,ms:word;
  begin
  DecodeTime(v, hh,mm,ss,ms);
  Result:=FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2);
  end;

procedure TGateClientForm.StartNext;
  var
    x,y,h,w:integer;
    nextline:boolean;
    s:String;
  begin
{$IFNDEF USE_AUTOSTART}
  Exit;
{$ENDIF}
  if Used then Exit;
  AutoStart:=False;
  Used:=True;

  nextline:=False;
  x:=Left; y:=Top;
  h:=MainPanel.Height;
  w:=MainPanel.Width;
  if x>=0 then
    begin
    if x+MainPanel.Width*2<=Screen.Width then
      x:=x+MainPanel.Width
    else
      begin
      nextline:=True;
      x:=0;
      y:=y+MainPanel.Height;
      end;
    end
  else
    begin
    if x-MainPanel.Width>=Screen.DesktopLeft then
      x:=x-MainPanel.Width
    else
      begin
      nextline:=True;
      x:=-MainPanel.Width;
      y:=y+MainPanel.Height;
      end;
    end;
  if (y>=0) and (y<=Screen.Height-MainPanel.Height) then
    begin
    if nextline then
      if ((y div MainPanel.Height) mod AUTOSTART_LINES)=0 then
        Exit; // skip the next app if we already have the expected number of lines
    s:=ParamStr(0)+' '+IntToStr(x)+' '+IntToStr(y)+' '+IntToStr(h)+' '+IntToStr(w);
    WinExec(@(RtcStringToBytesZero(s)[0]),SHOW_OPENWINDOW);
    end;
  end;

procedure TGateClientForm.FormCreate(Sender: TObject);
  var
    x,y,h,w:integer;
    GUID:TGUID;
  begin
  LastUID:=0;
  if CreateGuid(GUID) = S_OK then
    MyLOGName:=GUIDToString(GUID)
  else
    begin
    Randomize;
    MyLogName:='{'+IntToStr(RtcIntPtr(self))+'-'+IntToStr(Random(999999))+'-'+IntToStr(Random(999999))+'}';
    end;

  StartLog;
  StartLogBuffers(64000); // speed-up logging by using up to 64KB of RAM

  AutoSize:=True;

  { If you want a minimalistic client, so more can fit on the Screen,
   then uncomment the line below: }
  // MainPanel.Height:=InfoPanel.Top;

  { Uncomment the next 2 lines if you want to Limit
    the size of TCP/IP packets used by each Client.
    If no limits are set, maximum buffer sizes are used. }
  // rtcSetupReceivingParams(1500,1,1);
  // rtcSetupSendingParams(1500,1,1);

  FCS:=TRtcCritSec.Create;
  sStatus1:='';
  sStatus2:='';
  sStatus3:='';
  sGroups:='';
  sSend:='RUN';
  FUserList:='';
  FUserListNew:=False;

{$IFDEF USE_UDP}
  udpServer.Listen();
  udpClient.Connect();
{$ENDIF}

  // We will be sending the EXE file
  MyFileName:=AppFileName;
  if ParamCount>1 then
    begin
    x:=StrToInt(ParamStr(1));
    y:=StrToInt(ParamStr(2));
    h:=StrToInt(ParamStr(3));
    w:=StrToInt(ParamStr(4));
    Left:=x; Top:=y;
    MainPanel.Height:=h;
    MainPanel.Width:=w;

    { Set "xUseAPI", "xAVFriendly" and "xAsync" variable based on our Form position
      to minimize steps required to prepare a test of all APIs, streaming and processing options. }
    if X<0 then
      xUseAPI:=(-X div MainPanel.Width) mod 4
    else
      xUseAPI:=(X div MainPanel.Width) mod 4;
    xAVFriendly:=((Y div MainPanel.Height) mod 4);
    xAsync:=     ((Y div MainPanel.Height) mod 8)>=4;

    btnLogINClick(nil);
    StartAnother.Enabled:=True;
    end
  else
    begin
    AutoStart:=True;
    Left:=0;
    Top:=0;
    end;
  end;

procedure TGateClientForm.FormDestroy(Sender: TObject);
  begin
  Log('Form Destroy',MyLOGName);
  FreeAndNil(FCS);
  end;

procedure TGateClientForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  AutoStart:=False;
  StartAnother.Enabled:=False;
  GateCli.AutoLogin:=False;

  GCM.Client:=nil;
  CanClose:=GCM.Client=nil;
  if CanClose then
    Log('Close Query - OK',MyLOGName)
  else
    Log('Close Query - WAIT',MyLOGName);
  end;

procedure TGateClientForm.btnLogINClick(Sender: TObject);
  begin
  if GateCli.AutoLogin then
    begin
    if Sender=btnLogIN then
      Log('Clicked LOG OUT (Active)',MyLOGName)
    else
      Log('Clicked LOG OUT (Passive)',MyLOGName);

    StartAnother.Enabled:=False;
    AutoStart:=False;

    GateCli.AutoLogin:=False;
    try
      if Copy(Clipboard.AsText,1,1)=',' then
        Clipboard.AsText:='';
    except
      end;

    {$IFDEF USE_UDP}
    if Sender=btnLogIN then
      udpClient.Write('stop');
    {$ENDIF}
    end
  else
    begin
    if Sender=btnLogIN then
      Log('Clicked LOG IN (Active)',MyLOGName)
    else
      Log('Clicked LOG IN (Passive)',MyLOGName);

    StartAnother.Enabled:=AutoStart;
    AutoStart:=False;

    GateCli.AutoLogin:=True;

    {$IFDEF USE_UDP}
    if Sender=btnLogIN then
      udpClient.Write('start');
    {$ENDIF}
    end;
  end;

procedure TGateClientForm.btnSendFileClick(Sender: TObject);
  begin
  if FSendingFile and (Sender<>nil) then
    begin
    if Sender=btnSendFile then
      Log('Clicked STOP (Active)',MyLOGName)
    else
      Log('Clicked STOP (Passive)',MyLOGName);
    xRepeatSend:=False;
    FSendingFile:=False;
    FCS.Acquire;
    try
      sSend:='RUN';
    finally
      FCS.Release;
      end;
    PrintMsg('STOPPED.',msg_Output);

    {$IFDEF USE_UDP}
    if Sender=btnSendFile then
      udpClient.Write('nosend');
    {$ENDIF}
    end
  else if File_Exists(MyFileName) then
    begin
    if Sender=btnSendFile then
      Log('Clicked SEND (Active)',MyLOGName)
    else
      Log('Clicked SEND (Passive)',MyLOGName);
    xRepeatSend:=True;
    FSendingFile:=True;
    FCS.Acquire;
    try
      sSend:='STP';
    finally
      FCS.Release;
      end;

    FSendFileStart:=GetAppRunTime;
    FSendFileName:=MyFileName;
    FSendFileLoc:=0;
    FSendFileSize:=File_Size(FSendFileName);

    FUserListNew:=True;
    if GCM.Ready then
      btnAddUserClick(nil)
    else
      PrintMsg('!#> NOT Ready',msg_Output);

    {$IFDEF USE_UDP}
    if Sender=btnSendFile then
      udpClient.Write('send');
    {$ENDIF}
    end;
  end;

procedure TGateClientForm.btnCLRClick(Sender: TObject);
  begin
  CntReset:=0;
  btnCLR.Color:=clWhite;
  btnCLR.Font.Color:=clNavy;
  btnCLR.Caption:='CLR';

  if FUserList<>'' then
    begin
    FUserListNew:=True;
    btnAddUserClick(nil);
    end;
  end;

procedure TGateClientForm.btnAddUserClick(Sender: TObject);
  var
    aUser:TGateUID;
    p,l,cnt:integer;
    fs,ts,fAdded:String;
  begin
  try
    if not FUserListNew then
      Log('AddUser: NOT UserListNew',MyLOGName)
    else if not GCM.Ready then
      Log('AddUser: NOT GCM.Ready',MyLOGName)
    else
      begin
      FUserListNew:=False;
      cnt:=0;
      fs:=FUserList+' ';
      ts:='';
      p:=1;
      l:=length(fs);
      fAdded:='';
      // Remove all users from our receiver group ...
      if GCM.DisbandGroup then
        Log('AddUser: DisbandGroup OK',MyLOGName)
      else
        Log('AddUser: DisbandGroup FAIL',MyLOGName);
      while p<=l do
        begin
        if fs[p] in ['0'..'9'] then
          ts:=ts+fs[p]
        else if ts<>'' then
          if Pos(ts,fAdded)>0 then
            ts:=''
          else
            begin
            aUser:=Str2LWord(ts);
            // Send "cid_AddMeFriend" message to the user,
            // requesting the user to add us as their Friend,
            // after which we can add the user to our receiver Group
            if GCM.SendBytes(aUser,0,cid_AddMeFriend) then
              begin
              Log('AddUser: Invite '+IntToStr(aUser)+' OK',MyLOGName);
              if fAdded='' then fAdded:=ts
              else fAdded:=fAdded+' '+ts;
              Inc(cnt);
              end
            else
              Log('AddUser: Invite '+IntToStr(aUser)+' FAIL',MyLOGName);
            ts:='';
            end;
        Inc(p);
        end;
      FUserList:=fAdded;
      eToID.Caption:=IntToStr(cnt);
      end;
  except
    on E:Exception do
      Log('btnAddUserClick - '+E.ClassName+':'+E.Message,MyLOGName+'.ERROR');
    end;
  end;

function KSeparate(const s:String):String;
  var
    i,len:integer;
  begin
  Result:='';
  i:=0;len:=length(s);
  while i<len do
    begin
    Result:=s[len-i]+Result;
    Inc(i);
    if (i mod 3=0) and (i<len) then Result:='.'+Result;
    end;
  end;

procedure TGateClientForm.StatusUpdateTimer(Sender: TObject);
  begin
  case GateCli.State.InputState of
    ins_Connecting: shInput.Brush.Color:=clYellow;
    ins_Closed:     shInput.Brush.Color:=clRed;
    ins_Prepare:    shInput.Brush.Color:=clBlue;
    ins_Start:      shInput.Brush.Color:=clGreen;
    ins_Recv:       shInput.Brush.Color:=clLime;
    ins_Idle:       shInput.Brush.Color:=clGreen;
    ins_Done:       shInput.Brush.Color:=clNavy;
    end;
  if GateCli.State.InputState=ins_Closed then
    shInput.Pen.Color:=shInput.Brush.Color
  else
    case GateCli.State.PingInCnt of
      0:shInput.Pen.Color:=clWhite;
      1:shInput.Pen.Color:=clGreen;
      2:shInput.Pen.Color:=clLime;
      3:shInput.Pen.Color:=clBlack;
      end;

  case GateCli.State.OutputState of
    outs_Connecting:  shOutput.Brush.Color:=clYellow;
    outs_Closed:      shOutput.Brush.Color:=clRed;
    outs_Prepare:     shOutput.Brush.Color:=clBlue;
    outs_Start:       shOutput.Brush.Color:=clGreen;
    outs_Send:        shOutput.Brush.Color:=clLime;
    outs_Idle:        shOutput.Brush.Color:=clGreen;
    outs_Done:        shOutput.Brush.Color:=clNavy;
    end;
  if GateCli.State.OutputState=outs_Closed then
    shOutput.Pen.Color:=shOutput.Brush.Color
  else
    case GateCli.State.PingOutCnt of
      0:shOutput.Pen.Color:=clWhite;
      1:shOutput.Pen.Color:=clGreen;
      2:shOutput.Pen.Color:=clLime;
      3:shOutput.Pen.Color:=clBlack;
      end;
  lblSendBuffSize.Caption:=KSeparate(Int2Str(GateCli.State.TotalSent div 1024))+'K';
  lblRecvBufferSize.Caption:=KSeparate(Int2Str(GateCli.State.TotalReceived div 1024))+'K';

  if GateCli.State.LeftToSend>0 then
    shState.Brush.Color:=clLime
  else if GateCli.State.ReadyToSend then
    begin
    if GateCli.State.LastOutput+100<GetAppRunTime then // 5 sec ready but no data sent
      shState.Brush.Color:=clFuchsia
    else
      shState.Brush.Color:=clBlue;
    end
  else if GateCli.State.OutputReady and GateCli.State.InputReady then
    shState.Brush.Color:=clGreen
  else if GateCli.Ready then
    shState.Brush.Color:=clYellow
  else
    shState.Brush.Color:=clRed;

  FCS.Acquire;
  try
    l_Status1.Caption:=sStatus1;
    l_Status2.Caption:=sStatus2;
    l_Status3.Caption:=sStatus3;
    l_Groups.Caption:=sGroups;
    btnSendFile.Caption:=sSend;
  finally
    FCS.Release;
    end;

  DumpLogBuffers;
  end;

procedure TGateClientForm.DoSendFile;
  var
    sendNow,sendTime:int64;
    maxSize:integer;
  begin
  if FSendFileLoc<FSendFileSize then
    begin
    sendNow:=FSendFileSize-FSendFileLoc;
    maxSize:=random(64*1024)+32*1024;
    if sendNow>maxSize then
      sendNow:=maxSize;

    if GCM.SendToGroup(cid_TestFileSend,Read_FileEx(FSendFileName,FSendFileLoc,sendNow)) then
      begin
      PrintMsg('>'+IntToStr(sendNow div 1024)+'K',msg_Output);

      FSendFileLastTime:=GetAppRunTime;
      FSendFileLastSize:=sendNow;

      FCS.Acquire;
      try
        sSend:=IntToStr((FSendFileLoc*100) div FSendFileSize)+'%';
      finally
        FCS.Release;
        end;
      FSendFileLoc:=FSendFileLoc+sendNow;
      end
    else
      PrintMsg('!#> '+IntToStr(sendNow div 1024)+'K',msg_Output);
    end
  else
    begin
    FSendingFile:=False;
    FCS.Acquire;
    try
      sSend:='OK';
    finally
      FCS.Release;
      end;
    sendTime:=GetAppRunTime-FSendFileStart;
    if sendTime>0 then
      begin
      PrintMsg('>>'+IntToStr(FSendFileLoc div 1024)+'K in '+FloatToStr(sendTime/RUN_TIMER_PRECISION)+'s',msg_Output);
      PrintMsg(KSeparate(IntToStr(FSendFileLoc div ((sendTime*1024) div RUN_TIMER_PRECISION) ))+' Ks',msg_Speed);
      end
    else
      begin
      PrintMsg('>>'+IntToStr(FSendFileLoc div 1024)+' Ks',msg_Output);
      PrintMsg('MAX K/s',msg_Speed);
      end;

    if xRepeatSend then
      btnSendFileClick(nil);
    end;
  end;

procedure TGateClientForm.PrintMsg(const s: String; t:TMsgType);
  begin
  Log(s,MyLOGName);

  FCS.Acquire;
  try
    case t of
      msg_Input:
        sStatus1:=Time2Str(Now)+' '+s;
      msg_Output:
        sStatus2:=Time2Str(Now)+' '+s;
      msg_Group:
        sStatus1:=Time2Str(Now)+' '+s;
      msg_Speed:
        sStatus3:=s;
      msg_Status:
        begin
        sStatus1:=Time2Str(Now)+' '+s;
        sStatus2:='';
        sStatus3:='';
        end;
      msg_Error:
        sStatus2:=Time2Str(Now)+' '+s;
      end;
  finally
    FCS.Release;
    end;
  end;

var
  LMouseX,LMouseY:integer;
  LMouseD:boolean=False;

procedure TGateClientForm.InfoPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if Button=mbRight then
    begin
    if MainPanel.Height<>InfoPanel.Top then
      MainPanel.Height:=InfoPanel.Top
    else
      MainPanel.Height:=InfoPanel.Top+InfoPanel.Height;
    end
  else if Button=mbLeft then
    begin
    LMouseD:=True;
    LMouseX:=-TControl(Sender).Left; //X;
    LMouseY:=-TCOntrol(Sender).Top; // Y;
    InfoPanelMouseMove(Sender,Shift,X,Y);
    end;
  end;

procedure TGateClientForm.InfoPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    nX,nY:integer;
  begin
  if LMouseD then
    begin
    if (Left+X)>=0 then
      nX:=((Left+X-LMouseX) div MainPanel.Width) * MainPanel.Width
    else
      nX:=((Left+X-LMouseX-MainPanel.Width) div MainPanel.Width) * MainPanel.Width;
    if (Top+Y)>=0 then
      nY:=((Top+Y-LMouseY) div MainPanel.Height) * MainPanel.Height
    else
      nY:=((Top+Y-LMouseY-MainPanel.Height) div MainPanel.Height) * MainPanel.Height;
    if (nX<>Left) or (nY<>Top) then
      begin
      SetBounds(nX,nY,Width,Height);
      { Set "xUseAPI", "xAVFriendly" and "xAsync" variable based on our Form position
        to minimize steps required to prepare a test of all APIs, streaming and processing options. }
      if X<0 then
        xUseAPI:=(-nX div MainPanel.Width) mod 4
      else
        xUseAPI:=(nX div MainPanel.Width) mod 4;
      xAVFriendly:=((nY div MainPanel.Height) mod 4);
      xAsync:=     ((nY div MainPanel.Height) mod 8)>=4;
      end;
    end;
  end;

procedure TGateClientForm.InfoPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if Button=mbLeft then
    LMouseD:=False;
  end;

procedure TGateClientForm.btnCloseClick(Sender: TObject);
  begin
  AutoStart:=False;
  StartAnother.Enabled:=False;

  if GateCli.AutoLogin then
    begin
    if Sender=btnClose then
      Log('Clicked CLOSE - Logged IN (Active)',MyLOGName)
    else
      Log('Clicked CLOSE - Logged IN (Passive)',MyLOGName);
    GateCli.AutoLogin:=False;
    try
      if Copy(Clipboard.AsText,1,1)=',' then
        Clipboard.AsText:='';
    except
      end;
    end
  else
    begin
    if Sender=btnClose then
      Log('Clicked CLOSE - Logged OUT (Active)',MyLOGName)
    else
      Log('Clicked CLOSE - Logged OUT (Passive)',MyLOGName);
    {$IFDEF USE_UDP}
    if Sender=btnClose then
      udpClient.Write('close');
    {$ENDIF}
    Close;
    end;
  end;

procedure TGateClientForm.eToIDExit(Sender: TObject);
  begin
  // btnAddUserClick(nil);
  end;

procedure TGateClientForm.btnSoftResetClick(Sender: TObject);
  begin
  GateCli.ResetStreams;
  end;

procedure TGateClientForm.eYourIDClick(Sender: TObject);
  var
    s:String;
  begin
  try
    s:=Clipboard.AsText;
    if s<>'' then
      if Copy(s,1,1)<>',' then
        s:='';
    Clipboard.AsText:=s+','+eYourID.Caption;
  except
    on E:Exception do
      Log('eYourIDClick - '+E.ClassName+':'+E.Message,MyLOGName+'.ERROR');
    end;
  end;

procedure TGateClientForm.eToIDClick(Sender: TObject);
  begin
  try
    FUserList:=Clipboard.AsText;
    if FUserList<>'' then
      begin
      FUserListNew:=True;
      btnAddUserClick(nil);
      end;
  except
    on E:Exception do
      Log('eToIDClick - '+E.ClassName+':'+E.Message,MyLOGName+'.ERROR');
    end;
  end;

procedure TGateClientForm.StartAnotherTimer(Sender: TObject);
  begin
  StartAnother.Enabled:=False;
  StartNext;
  end;

procedure TGateClientForm.udpServerDataReceived(Sender: TRtcConnection);
{$IFDEF USE_UDP}
  var
    msg:String;
  begin
  msg:=Sender.Read;
  if (Sender.PeerPort=udpClient.LocalPort) then
    Exit; // ignore our messages

  if msg='start' then
    begin
    if not GateCli.AutoLogin then
      begin
      Log('Received START',MyLOGName);
      btnLogINClick(nil);
      end;
    end
  else if msg='stop' then
    begin
    if GateCli.AutoLogin then
      begin
      Log('Received STOP',MyLOGName);
      btnLogINClick(nil);
      end;
    end
  else if msg='send' then
    begin
    if not FSendingFile then
      begin
      Log('Received SEND',MyLOGName);
      btnSendFileClick(nil);
      end;
    end
  else if msg='nosend' then
    begin
    if FSendingFile then
      begin
      Log('Received NOSEND',MyLOGName);
      btnSendFileClick(Sender);
      end;
    end
  else if msg='close' then
    begin
    Log('Received CLOSE',MyLOGName);
    Close;
    end;
  end;
{$ELSE}
  begin
  end;
{$ENDIF}

procedure TGateClientForm.GCMAfterLoggedInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  var
    s:String;
  begin
  case xUseAPI of
    0:s:='IE ';
    1:s:='HT ';
    2:s:='BS ';
    3:s:='AS ';
    end;
  if xAsync then s:=s+'BT ';
  case xAVFriendly of
    0: s:=s+'-- ';
    1: s:=s+'<I ';
    2: s:=s+'O> ';
    3: s:=s+'<> ';
    end;
  s:=s+'- Logged IN ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).';
  PrintMsg(s,msg_Status);

  eYourID.Caption:=LWord2Str(LastUID);

  FUserList:=FUserList+' '+eYourID.Caption;
  FUserListNew:=True;

  btnLogIN.Caption:='OUT';

  btnClose.Visible:=False;

  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.GCMAfterLoginFailGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  if btnLogIN.Caption<>'IN' then
    btnLogIN.Caption:='IN';

  StatusUpdate.Enabled:=False;
  StatusUpdateTimer(nil);

  PrintMsg('Login attempt FAILED.',msg_Status);
  if State.LastError<>'' then
    PrintMsg(State.LastError, msg_Error);

  FCS.Acquire;
  try
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  InGroupCnt:=0;
  OutGroupCnt:=0;
  InGroupMe:=False;
  OutGroupMe:=False;

  btnClose.Visible:=True;

  btnCLR.Color:=clRed;
  btnCLR.Font.Color:=clYellow;
  end;

procedure TGateClientForm.GCMAfterLogOutGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  PrintMsg('Logged OUT.',msg_Status);
  if State.LastError<>'' then
    PrintMsg(State.LastError,msg_Error);

  if btnLogIN.Caption<>'IN' then
    btnLogIN.Caption:='IN';

  StatusUpdate.Enabled:=False;

  FCS.Acquire;
  try
    if FSendingFile then
      begin
      FSendingFile:=False;
      sSend:='RUN';
      end;
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  InGroupCnt:=0;
  OutGroupCnt:=0;
  InGroupMe:=False;
  OutGroupMe:=False;

  if btnCLR.Caption<>'CLR' then
    begin
    btnCLR.Color:=clRed;
    btnCLR.Font.Color:=clYellow;
    end;

  btnClose.Visible:=True;

  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.GCMBeforeLogInGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  FLoginStart:=GetAppRunTime;

  { You can change all component parameters here,
    including GateAddr and GatePort properties. }
  // GateCli.GateAddr:=MyGateAddr;
  // GateCli.GatePort:=MyGatePort;

  { To test all 4 supported APIs,
    set UseBlocking, UseProxy and UseWinHTTP
    based on the "xUseAPI" variable: }
  Client.UseProxy:=xUseAPI=0;
  Client.UseWinHTTP:=xUseAPI=1;
  Client.UseBlocking:=xUseAPI=2;
  case xAVFriendly of
    0:begin
      Client.StreamBlockSizeIn:=1;
      Client.StreamBlockSizeOut:=1;
      end;
    1:begin
      Client.StreamBlockSizeIn:=0;
      Client.StreamBlockSizeOut:=1;
      end;
    2:begin
      Client.StreamBlockSizeIn:=1;
      Client.StreamBlockSizeOut:=0;
      end;
    3:begin
      Client.StreamBlockSizeIn:=0;
      Client.StreamBlockSizeOut:=0;
      end;
    end;

  CntReset:=0;
  btnCLR.Color:=clWhite;
  btnCLR.Font.Color:=clNavy;
  btnCLR.Caption:='CLR';
  FSendingFile:=False;

  btnLogIN.Caption:='...';
  shInput.Brush.Color:=clYellow;
  shInput.Pen.Color:=clWhite;
  shOutput.Brush.Color:=clYellow;
  shOutput.Pen.Color:=clWhite;
  PrintMsg('Logging in ...',msg_Status);

  FCS.Acquire;
  try
    sSend:='RUN';
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  InGroupCnt:=0;
  OutGroupCnt:=0;
  InGroupMe:=False;
  OutGroupMe:=False;

  StatusUpdate.Enabled:=True;
  end;

procedure TGateClientForm.GCMDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  // Only Handle messages with our known CallID's
  case Data.CallID of
    cid_TestFileSend,
    cid_AddMeFriend:
      begin
      if Data.Footer then
        Wanted:=True
      else if Data.Header then // Start Buffering content after we receive the header
        Data.ToBuffer:=True;
      end;
    end;
  end;

procedure TGateClientForm.GCMInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  case Data.Command of
    gc_UserOnline:  PrintMsg(IntToStr(Data.UserID)+' ON',msg_Group);
    gc_UserOffline: PrintMsg(IntToStr(Data.UserID)+' OFF',msg_Group);
    gc_Error:       PrintMsg('ERR #'+IntToStr(Data.ErrCode)+' from User '+IntToStr(Data.UserID),msg_Group);

    gc_BeFriend,
    gc_UserJoined, gc_UserLeft,
    gc_JoinedUser, gc_LeftUser:    Wanted:=True;
    end;
  end;

procedure TGateClientForm.GCMStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  if FUserList<>'' then
    FUserListNew:=True;
  FLoginStart:=GetAppRunTime;

  Inc(CntReset);
  btnCLR.Color:=clYellow;
  btnCLR.Font.Color:=clRed;
  btnCLR.Caption:=IntToStr(CntReset);

  if GateCli.Active then
    PrintMsg('#LOST ('+FloatToStr(GateCli.State.InputResetTime/RUN_TIMER_PRECISION)+'s / '+FloatToStr(GateCli.State.OutputResetTime/RUN_TIMER_PRECISION)+'s)',msg_Status)
  else
    PrintMsg('#FAIL ('+FloatToStr(GateCli.State.InputResetTime/RUN_TIMER_PRECISION)+'s / '+FloatToStr(GateCli.State.OutputResetTime/RUN_TIMER_PRECISION)+'s)',msg_Status);
  if GateCli.State.LastError<>'' then
    PrintMsg(GateCli.State.LastError, msg_Error);
  FCS.Acquire;
  try
    if FSendingFile then
      begin
      FSendingFile:=False; // Stop sending file
      sSend:='#'+IntToStr((FSendFileLoc*100) div FSendFileSize);
      end;
    InGroupMe:=False; OutGroupMe:=False;
    InGroupCnt:=0; OutGroupCnt:=0;
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  end;

procedure TGateClientForm.btnHardResetClick(Sender: TObject);
  begin
  GateCli.ResetStreams(True);
  end;

procedure TGateClientForm.GCThreadDataReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI: Boolean);
  begin
  // Only Handle messages with our known CallID's
  if Data.CallID=cid_TestFileSend then
    PrintMsg('<'+IntToStr(Length(Data.Content) div 1024)+'K id'+IntToStr(Data.UserID), msg_Input)
  else if Data.CallID=cid_AddMeFriend then
    begin
    // To simplify this test, we will be removing the Friend before Adding,
    // making sure that a new "BeFriend" notification will be sent out.
    if GCM.RemoveFriend(Data.UserID) then
      Log('Removing '+IntToStr(Data.UserID)+' OK',MyLOGName)
    else
      Log('Removing '+IntToStr(Data.UserID)+' FAIL',MyLOGName);
    if GCM.AddFriend(Data.UserID) then
      Log('Adding '+IntToStr(Data.UserID)+' OK',MyLOGName)
    else
      Log('Adding '+IntToStr(Data.UserID)+' FAIL',MyLOGName);
    PrintMsg('+- '+IntToStr(Data.UserID), msg_Output);
    end;
  end;

procedure TGateClientForm.GCMDataReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);
  begin
  if xAsync then
    WantBackThread:=True
  else
    GCThreadDataReceived(Client,Data,WantGUI);
  end;

procedure TGateClientForm.GCThreadInfoReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI: Boolean);
  var
    s:String;
  begin
  // Handle info about other Users (Clients) joining and leaving our Groups
  // and about us leaving and joining Groups managed by other Users (Clients)

  // We are in a background thread here,
  // so do NOT access any GUI elements!
  case Data.Command of
    gc_BeFriend:
      begin
      // Received "FriendAdd" notification from the user.
      // Now, we can add the user to our receiver Group ...
      if GCM.AddUserToGroup(Data.UserID) then // this will also trigger the "ReadyToSend" event
        PrintMsg('++ '+IntToStr(Data.UserID)+'/'+INtToStr(Data.GroupID)+' ('+IntToStr(OutGroupCnt)+')',msg_Group)
      else
        PrintMsg('!#+ '+IntToStr(Data.UserID)+'/'+INtToStr(Data.GroupID)+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
      end;
    gc_UserJoined:
      begin
      if Data.UserID=Data.MyUID then
        OutGroupMe:=True
      else
        Inc(OutGroupCnt);
      PrintMsg('OUT +'+IntToStr(Data.UserID)+'/'+INtToStr(Data.GroupID)+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
      end;
    gc_UserLeft:
      begin
      if Data.UserID=Data.MyUID then
        OutGroupMe:=False
      else if OutGroupCnt>0 then
        Dec(OutGroupCnt);
      PrintMsg('OUT -'+IntToStr(Data.UserID)+'/'+INtToStr(Data.GroupID)+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
      end;
    gc_JoinedUser:
      begin
      if Data.UserID=Data.MyUID then
        InGroupMe:=True
      else
        Inc(InGroupCnt);
      PrintMsg('IN +'+INtToStr(Data.GroupID)+'/'+IntToStr(Data.UserID)+' ('+IntToStr(InGroupCnt)+')',msg_Group);
      end;
    gc_LeftUser:
      begin
      if Data.UserID=Data.MyUID then
        InGroupMe:=False
      else if InGroupCnt>0 then
        Dec(InGroupCnt);
      PrintMsg('IN -'+IntToStr(Data.GroupID)+'/'+INtToStr(Data.UserID)+' ('+IntToStr(InGroupCnt)+')',msg_Group);
      end;
    end;
  s:='';
  if InGroupMe then s:=s+'+';
  s:=s+IntToStr(InGroupCnt)+'/'+IntToStr(OutGroupCnt);
  if OutGroupMe then s:=s+'+';
  FCS.Acquire;
  try
    sGroups:=s;
  finally
    FCS.Release;
    end;
  end;

procedure TGateClientForm.GCMInfoReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI, WantBackThread:Boolean);
  begin
  if xAsync then
    WantBackThread:=True
  else
    GCThreadInfoReceived(Client,Data,WantGUI);
  end;

procedure TGateClientForm.GCThreadReadyToSend(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI: Boolean);
  begin
  // Client is ready to send data to the Gateway
  // We are in a backgrdound thread, so NO GUI ACCESS here,
  // unless we use the Sync() method to synchronize the event with the Main thread.
  // Check other events for an example on using the Sync() method.
  if FUserListNew then
    begin
    PrintMsg('Prepare ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Output);
    WantGUI:=True;
    end
  else if GCM.UsersInGroup>0 then
    begin
    if FSendingFile then
      DoSendFile
    else if xRepeatSend then
      WantGUI:=True;
    end
  else
    begin
    PrintMsg('Ready ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Output);
    FLoginStart:=GetAppRunTime;
    end;
  end;

procedure TGateClientForm.GCMReadyToSendGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  if FUserListNew then
    btnAddUserClick(nil)
  else if xRepeatSend then
    btnSendFileClick(nil);
  end;

procedure TGateClientForm.GCMReadyToSend(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread:Boolean);
  begin
  if xAsync then
    WantBackThread:=True
  else
    GCThreadReadyToSend(Client,State,WantGUI);
  end;

procedure TGateClientForm.GCMAfterClientRemoved;
  begin
  Log('Client removed',MyLOGName);
  Close;
  end;

procedure TGateClientForm.GCMAfterLoggedIn(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo; var WantGUI, WantBackThread: Boolean);
  begin
  if LastUID<>0 then
    Log('LOGGED IN. Old ID = '+IntToStr(LastUID)+'; new ID = '+IntToStr(State.MyUID),MyLOGName)
  else
    Log('LOGGED IN. Start ID = '+IntToStr(State.MyUID),MyLOGName);

  Copy_Log(MyLOGName,IntToStr(State.MyUID));
  Delete_Log(MyLOGName);

  LastUID:=State.MyUID;
  MyLOGName:=IntToStr(LastUID);

  WantGUI:=True;
  end;

end.
