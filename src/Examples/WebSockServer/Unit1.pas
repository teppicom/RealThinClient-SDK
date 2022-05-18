unit Unit1;

{ This is a Multi-Threaded Web Socket Server Test Project.

  Compile and Run this Project, open any Web Browser with Web Socket support
  (most modern Web Browsers have it - like Google Chrome and Internet Explorer),
  enter "http://localhost" in the Address bar of your Web Browser and ...

  you should see a Web Socket connection established between the Web Browser
  and this Server. Click on any of the buttons in the Web Socket Server Test
  Project (Main Form) to send data to all connected Web Socket Clients.

  Java Script used in the "ws.html" file (executed in the Web Browser) is
  implemented to send the same content back to the Server, so we can use
  this Test as an example of sending and receiving Web Socket Frames between
  a RTC Web Socket Server and any Web Browser.

  This Web Socket Server also works with the "WSockCliTest" Project,
  which can be found in the "SDKExamples_QuickStart_VCL" Project Group.

  There are 2 Data Providers implemented in this test.

  "SockProv" is the one responsible for handling '/' (root) requests and
  is used by Web Browsers and Web Socket Clients requesting '/' (root).

  "EchoProv" is a simple "ECHO" Web Socket provider, which responds to
  requests sent to '/echo' and implements the "OnWSDataReceived" event
  to directly return all Web Socket Frames received by the Client.
  It can be used with the "WSockCliTest" Project if you enter
  '/echo' into its "SrvURI" Edit field (set to '/' by default).

  Read source code comments below for more info. }

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,

  rtcSystem,
  rtcConn,
  rtcInfo,
  rtcThrPool,
  rtcThrJobs,
  rtcZLib,
  rtcLog,

  rtcDataSrv,
  rtcHttpSrv;

type
  TForm1 = class(TForm)
    Server: TRtcHttpServer;
    SockProv: TRtcDataProvider;
    Memo1: TMemo;
    Panel1: TPanel;
    btnSendText: TButton;
    btnSendBinary: TButton;
    btnSendPing: TButton;
    btnSendPong: TButton;
    btnSendTxtOne: TButton;
    btnSendExeOne: TButton;
    btnClose: TButton;
    btnEmptyQ: TButton;
    btnDisconnect: TButton;
    EchoProv: TRtcDataProvider;
    btnSendTxtMulti: TButton;
    btnSendExeMulti: TButton;
    btnSendTxtRam: TButton;
    btnSendExeRam: TButton;
    btnMemoClear: TButton;
    btnSendTxtCompr: TButton;
    btnSendExeCompr: TButton;
    Label1: TLabel;
    ePort: TEdit;
    btnListen: TButton;
    cbLOG: TComboBox;
    Label4: TLabel;
    procedure SockProvCheckRequest(Sender: TRtcConnection);
    procedure FormCreate(Sender: TObject);
    procedure AnyProvResponseDone(Sender: TRtcConnection);
    procedure AnyProvDisconnect(Sender: TRtcConnection);
    procedure btnSendTextClick(Sender: TObject);
    procedure btnSendBinaryClick(Sender: TObject);
    procedure btnSendPingClick(Sender: TObject);
    procedure btnSendPongClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSendTxtOneClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SockProvWSDataReceived(Sender: TRtcConnection);
    procedure AnyProvWSDataSent(Sender: TRtcConnection);
    procedure AnyProvWSDisconnect(Sender: TRtcConnection);
    procedure AnyProvWSConnect(Sender: TRtcConnection);
    procedure btnSendExeOneClick(Sender: TObject);
    procedure btnEmptyQClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure AnyProvDataReceived(Sender: TRtcConnection);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure EchoProvCheckRequest(Sender: TRtcConnection);
    procedure EchoProvWSDataReceived(Sender: TRtcConnection);
    procedure btnSendTxtMultiClick(Sender: TObject);
    procedure btnSendExeMultiClick(Sender: TObject);
    procedure btnSendTxtRamClick(Sender: TObject);
    procedure btnSendExeRamClick(Sender: TObject);
    procedure btnMemoClearClick(Sender: TObject);
    procedure AnyProvWSDataIn(Sender: TRtcConnection);
    procedure AnyProvWSDataOut(Sender: TRtcConnection);
    procedure btnListenClick(Sender: TObject);
    procedure btnSendTxtComprClick(Sender: TObject);
    procedure btnSendExeComprClick(Sender: TObject);
    procedure cbLOGChange(Sender: TObject);
  private
    { Private declarations }
    procedure MemoEvent(Data:TRtcValue);
  public
    { Public declarations }
    MyThread:TRtcThread;
    LogLevel:integer;
    procedure MemoAdd(Level:integer; const s:RtcWideString; Sender:TRtcConnection=nil);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ Utility functions ... }

procedure TForm1.MemoEvent(Data: TRtcValue);
  begin
  { This event should ONLY be used from "MemoAdd" (below) }
  Memo1.Lines.Add(Data.asText);
  end;

procedure TForm1.MemoAdd(Level:integer; const s:RtcWideString; Sender:TRtcConnection=nil);
  var
    s2:String;
  begin
  { 0 = None / Disable logging
    1 = Basic / + Basic messages, errors, connect and disconnect
    2 = General / + Headers
    3 = Detailed / + Data
    4 = Full Debug / + Bytes IN/OUT }
  if Level<=LogLevel then
    if InsideMainThread then
      begin // Called from the Main Thread ...
      { We can access "Memo1" directly }
      if Sender=nil then
        s2:='---------- '+s+' ----------'
      else if Sender.IsWebSocket then
        s2:='ws'+IntToStr(Sender.WSGetID)+' > '+s
      else
        s2:='http'+Sender.PeerAddr+':'+Sender.PeerPort+' > '+s;
      xLog(s2,'WS');
      Memo1.Lines.Add(s2);
      end
    else if assigned(MyThread) then
      begin // Called from a background thread ...
      { Post a job to the background thread "MyThread",
        but execute it from the Main Thread (GUI). }
      if Sender=nil then
        s2:='---------- '+s+' ----------'
      else if Sender.IsWebSocket then
        s2:='ws'+IntToStr(Sender.WSGetID)+' > '+s
      else
        s2:='http'+Sender.PeerAddr+':'+Sender.PeerPort+' > '+s;
      xLog(s2,'WS');
      PostQuickJob(MemoEvent,TRtcTextValue.Create(s2),True,MyThread);
      end;
  end;

{ Main Form events ... }

procedure TForm1.FormCreate(Sender: TObject);
  begin
  StartLog;
  LogLevel:=cbLog.ItemIndex;
  MyThread:=TRtcThread.Create; // Thread used by "MemoAdd" in "PostQuickJob" to serialize calls
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  TRtcThread.Stop(MyThread);
  MyThread:=nil;
  Server.StopListenNow();
  end;

procedure TForm1.btnListenClick(Sender: TObject);
  begin
  if not Server.isListening then
    begin
    Server.ServerPort:=ePort.Text;
    Server.Listen();
    end
  else
    Server.StopListen;
  end;

procedure TForm1.btnSendTextClick(Sender: TObject);
  begin
  { In this Server example, we will be using the "wSendToAll" method
    to send the same content to ALL connected Web Socket clients,
    but ... you can also send content to one specific Client from
    the "OnWSDataReceived" event as a direct reply to content received,
    or to any connected Client using the "wSend" method and a connection ID
    (ID returned from the wsGetID method on any "TRtcConnection" component). }

  if SockProv.wSendToAll(wf_Text,'Date/Time:'+DateTimeToStr(Now))>0 then
    MemoAdd(1,'Sending Text to /');

  if EchoProv.wSendToAll(wf_Text,'Date&Time:'+DateTimeToStr(Now))>0 then
    MemoAdd(1,'Sending Text to /echo');
  end;

procedure TForm1.btnSendBinaryClick(Sender: TObject);
  var
    ar:RtcByteArray;
    a:integer;
  begin
  SetLength(ar,255);
  for a:=0 to length(ar)-1 do ar[a]:=a;

  if SockProv.wSendToAll(wf_Binary,ar)>0 then
    MemoAdd(1,'Sending Binary to /');

  if EchoProv.wSendToAll(wf_Binary,ar)>0 then
    MemoAdd(1,'Sending Binary to /echo');
  end;

procedure TForm1.btnSendPingClick(Sender: TObject);
  begin
  if SockProv.wSendToAll(wf_Ping,'Ping!')>0 then
    MemoAdd(1,'Sending Ping to /');

  if EchoProv.wSendToAll(wf_Ping,'Ping?')>0 then
    MemoAdd(1,'Sending Ping to /echo');
  end;

procedure TForm1.btnSendPongClick(Sender: TObject);
  begin
  if SockProv.wSendToAll(wf_Pong,'Pong!')>0 then
    MemoAdd(1,'Sending Pong to /');

  if EchoProv.wSendToAll(wf_Pong,'Pong?')>0 then
    MemoAdd(1,'Sending Pong to /echo');
  end;

procedure TForm1.btnCloseClick(Sender: TObject);
  begin
  if SockProv.wSendToAll(wf_Close)>0 then
    MemoAdd(1,'Sending Close to /');
    
  if EchoProv.wSendToAll(wf_Close)>0 then
    MemoAdd(1,'Sending Close to /echo');
  end;

procedure TForm1.btnSendTxtOneClick(Sender: TObject);
  var
    wf, wf2:TRtcWSFrame;
  begin
  { Sending this Unit's source code (TEXT file)
    as an example of sending TEXT files in a single
    Frame, but read and sent out in smaller pieces
    by implementing the "OnWSDataSent" event ... }

  // We need a new Frame for sending out a binary file ...
  wf:=TRtcWSFrame.Create(wf_Text);

  // Using Frame's local variable "fname" to se the file name,
  // used in the "OnWSDataSent" event to check the name of the file
  wf.asText['fname']:='Unit1.pas';

  // "PayloadLength" has to be set before using "wSend" when sending
  // Frames out in multiple smaller chunks (using the OnWSDataSent event)
  wf.waPayloadLength:=File_Size(wf.asText['fname']);

  // We need a separate Frame instance for "EchoProv"
  wf2:=TRtcWSFrame(wf.copyOf);

  { "ws" object is managed by the connection component
    and auto-freed by the connection component ... }
  if SockProv.wSendToAll(wf,'ws-file')>0 then // Frame.Name will be set to "ws-file"
    MemoAdd(1,'Sending TEXT File in one Frame to /');

  { "EchoProv" uses the same method as "SockProv" for the "OnWSDataSent" event }
  if EchoProv.wSendToAll(wf2,'ws-file')>0 then // Frame.Name will be set to "ws-file"
    MemoAdd(1,'Sending TEXT File in one Frame to /echo');
  end;

procedure TForm1.btnSendExeOneClick(Sender: TObject);
  var
    wf, wf2:TRtcWSFrame;
  begin
  { Sending this Applications EXE file as an example
    of sending BINARY files in a single Frame,
    but read and sent out in smaller pieces
    by implementing the "OnWSDataSent" event ... }

  // We need a new Frame for sending out a binary file ...
  wf:=TRtcWSFrame.Create(wf_Binary);

  // Using Frame's local variable "fname" to se the file name,
  // used in the "OnWSDataSent" event to check the name of the file
  wf.asText['fname']:=AppFileName;

  // "PayloadLength" has to be set before using "wSend" when sending
  // Frames out in multiple smaller chunks (using the OnWSDataSent event)
  wf.waPayloadLength:=File_Size(wf.asText['fname']);

  // We need a separate Frame instance for "EchoProv"
  wf2:=TRtcWSFrame(wf.copyOf);

  { "ws" object is managed by the connection component
    and auto-freed by the connection component ... }
  if SockProv.wSendToAll(wf,'ws-file')>0 then // Frame.Name will be set to "ws-file"
    MemoAdd(1,'Sending EXE File in one Frame to /');

  { "EchoProv" uses the same method as "SockProv" for the "OnWSDataSent" event }
  if EchoProv.wSendToAll(wf2,'ws-file')>0 then // Frame.Name will be set to "ws-file"
    MemoAdd(1,'Sending EXE File in one Frame to /echo');

  { Instead of implementing the "OnWSDataSent" event, if we can afford keeping a copy of
    the entire file in memory for every Client until the entire file has been sent out,
    we can use any "wSend*" method with the entire file content  ...
  if EchoProv.wSendToAll(wf_Binary,Read_FileEx(AppFileName))>0 then
    MemoAdd('Sending EXE File to /echo'); }
  end;

procedure TForm1.btnSendTxtMultiClick(Sender: TObject);
  var
    wf,wf2:TRtcWSFrame;
  begin
  { Sending this Unit's source code as an example for sending files in
    multiple Frames, with each Frame containing only a part of the File
    and the file being read and sent from the "OnWSDataSent" event ... }

  // We need a new Frame for sending out a Text file with waFinal=FALSE
  wf:=TRtcWSFrame.Create(wf_Text,FALSE);

  // Use Frame's local variable 'fname' to set the file name,
  // so we can access it directly from "OnWSDataSent" events
  wf.asText['fname']:='Unit1.pas';

  { Since we KNOW how many bytes we want to send in total, we can
    use 'wfTotalLength' to set the total number of bytes to be sent.

    The alternative would be to manually set "wfFinished:=TRUE"
    just before or shortly after using the last "wfWrite" or
    "WriteEx" method call to prepare the Payload for sending. }
  wf.wfTotalLength:=File_Size(wf.asText['fname']);

  // We need a separate Frame instance for "EchoProv"
  wf2:=TRtcWSFrame(wf.copyOf);

  { "wf" object will be managed and auto-freed by the connection component }
  if SockProv.wSendToAll(wf,'ws-multi')>0 then // Frame.wfName will be set to "ws-multi"
    MemoAdd(1,'Sending TEXT File in multiple Frames to /');

  { "EchoProv" uses the same method as "SockProv" for the "OnWSDataSent" event }
  if EchoProv.wSendToAll(wf2,'ws-multi')>0 then // Frame.wfName will be set to "ws-multi"
    MemoAdd(1,'Sending TEXT File in multiple Frames to /echo');
  end;

procedure TForm1.btnSendExeMultiClick(Sender: TObject);
  var
    wf, wf2:TRtcWSFrame;
  begin
  { Sending this Application's EXE file as an example for sending files in
    multiple Frames, with each Frame containing only a part of the File
    and the file being read and sent from the "OnWSDataSent" event ... }

  // We need a new Frame for sending out a Text file with waFinal=FALSE
  wf:=TRtcWSFrame.Create(wf_Binary,FALSE);

  // Use Frame's local variable 'fname' to set the file name,
  // so we can access it directly from "OnWSDataSent" events
  wf.asText['fname']:=AppFileName;

  { Since we KNOW how many bytes we want to send in total, we can
    use 'wfTotalLength' to set the total number of bytes to be sent.

    The alternative would be to manually set "wfFinished:=TRUE"
    just before or shortly after using the last "wfWrite" or
    "WriteEx" method call to prepare the Payload for sending. }
  wf.wfTotalLength:=File_Size(wf.asText['fname']);

  // We need a separate Frame instance for "EchoProv"
  wf2:=TRtcWSFrame(wf.copyOf);

  { "wf" object will be managed and auto-freed by the connection component }
  if SockProv.wSendToAll(wf,'ws-multi')>0 then // Frame.wfName will be set to "ws-multi"
    MemoAdd(1,'Sending EXE File in multiple Frames to /');

  if EchoProv.wSendToAll(wf2,'ws-multi')>0 then // Frame.wfName will be set to "ws-multi"
    MemoAdd(1,'Sending EXE File in multiple Frames to /echo');
  end;

procedure TForm1.btnSendTxtComprClick(Sender: TObject);
  var
    wf,wf2:TRtcWSFrame;
  begin
  { Sending this Unit's source code in a compressed Base-64-Encoded format as
    an example for sending textual data in multiple Frames, without knowing
    up-front how much data has to be sent, using "wSend" to send the 1st header,
    then using the "OnWSDataSent" event to send all the "Payload" data. }

  // We need a new Frame for sending out a Text file with waFinal=FALSE
  wf:=TRtcWSFrame.Create(wf_Text,FALSE);

  // Use Frame's local variable 'fname' to set the file name,
  // so we can access it directly from the "OnWSDataSent" event
  wf.asText['fname']:='Unit1.pas';

  { We only know how large our file is, but we do NOT know how many bytes
    it will occupy after all its parts have been compressed, so we can
    NOT set "waPayloadLength" or "wfTotalLength" parameters here, but ...
    we can store the size of the file we want to send in our Frame's variable,
    which can be of any type and any name ("total" - in this example).
    We can access this variable from inside the "OnWSDataSent" event. }
  wf.asLargeInt['total']:=File_Size(wf.asText['fname']);

  { This is our "Compressed Text" marker, which the Client
    can use to check if the next Frames being received contain a
    Base-64-Encoded Compressed Text file instead of (as usual) plain Text data.
    As an example, we are also sending the original File name here. }
  wf.wfWrite(#13#10'bAsE64@TxT'#12'=='+Utf8Encode(wf.asText['fname'])+#13#10);

  // We need a separate Frame instance for "EchoProv"
  wf2:=TRtcWSFrame(wf.copyOf);

  { "wf" object will be managed and auto-freed by the connection component }
  if SockProv.wSendToAll(wf,'ws-mime')>0 then // Frame.wfName will be set to "ws-mime"
    MemoAdd(1,'Sending compressed TEXT File in multiple Frames to /');

  { "EchoProv" uses the same method as "SockProv" for the "OnWSDataSent" event }
  if EchoProv.wSendToAll(wf2,'ws-mime')>0 then // Frame.wfName will be set to "ws-mime"
    MemoAdd(1,'Sending compressed TEXT File in multiple Frames to /echo');
  end;

procedure TForm1.btnSendExeComprClick(Sender: TObject);
  var
    wf,wf2:TRtcWSFrame;
  begin
  { Sending this Application's EXE file in a compressed base64-encoded format
    as an example for sending Binary data in multiple Frames, without knowing
    up-front how much data has to be sent, using "wSend" to send the 1st header,
    then using the "OnWSDataSent" event to send all the "Payload" data ... }

  { We need a new Frame for sending out the file with waFinal=FALSE.
    Since we will be using Base64-encoding, even though we are sending
    a compressed binary file, we can use the Opcode "wf_Text" here. }
  wf:=TRtcWSFrame.Create(wf_Text,FALSE);

  // Use Frame's local variable 'fname' to set the file name,
  // so we can access it directly from the "OnWSDataSent" event
  wf.asText['fname']:=AppFileName;

  { We only know how large our file is, but we do NOT know how many bytes
    it will occupy after all its parts have been compressed, so we can
    NOT set "waPayloadLength" or "wfTotalLength" parameters here, but ...
    we can store the size of the file we want to send in our Frame's variable,
    which can be of any type and any name ("total" - in this example).
    We can access this variable from inside the "OnWSDataSent" event. }
  wf.asLargeInt['total']:=File_Size(wf.asText['fname']);

  { This is our "Compressed Executable" marker, which a Web Socket Client
    can use to check if the next Frames being received as "ws_Text" contain
    a Compressed Base-64-Encoded Executable file, instead of (as usual) plain Text data.
    As an example, we are also sending the original File name here. }
  wf.wfWrite(#13#10'eXe@BaSe64'#12'=='+Utf8Encode(wf.asText['fname'])+#13#10);

  // We need a separate Frame instance for "EchoProv"
  wf2:=TRtcWSFrame(wf.copyOf);

  { "wf" object will be managed and auto-freed by the connection component }
  if SockProv.wSendToAll(wf,'ws-mime')>0 then // Frame.wfName will be set to "ws-mime"
    MemoAdd(1,'Sending compressed EXE File in multiple Frames to /');

  { "EchoProv" uses the same method as "SockProv" for the "OnWSDataSent" event }
  if EchoProv.wSendToAll(wf2,'ws-mime')>0 then // Frame.wfName will be set to "ws-mime"
    MemoAdd(1,'Sending compressed EXE File in multiple Frames to /echo');
  end;

procedure TForm1.btnSendTxtRamClick(Sender: TObject);
  begin
  { Instead of implementing the "OnWSDataSent" event,
    if we can afford keeping a copy of the entire file in RAM
    for every Client until the entire File has been sent out,
    we can also use the "wSend*" method like this  ... }

  if SockProv.wSendToAll(wf_Text,Read_File('Unit1.pas'))>0 then
    MemoAdd(1,'Sending TEXT File from RAM in 1 Frame to /');

  if EchoProv.wSendToAll(wf_Text,Read_File('Unit1.pas'))>0 then
    MemoAdd(1,'Sending TEXT File from RAM in 1 Frame to /echo');
  end;

procedure TForm1.btnSendExeRamClick(Sender: TObject);
  begin
  { Instead of implementing the "OnWSDataSent" event,
    if we can afford keeping a copy of the entire file in RAM
    for every Client until the entire File has been sent out,
    we can also use the "wSend*" method like this  ... }

  if SockProv.wSendToAll(wf_Binary,Read_FileEx(AppFileName))>0 then
    MemoAdd(1,'Sending EXE File from RAM in 1 Frame to /');

  if EchoProv.wSendToAll(wf_Binary,Read_FileEx(AppFileName))>0 then
    MemoAdd(1,'Sending EXE File from RAM in 1 Frame to /echo');
  end;

procedure TForm1.btnEmptyQClick(Sender: TObject);
  begin
  if SockProv.wsClearAllSendingQueues>0 then
    MemoAdd(1,'Clear ALL Sending Queues for /');

  if EchoProv.wsClearAllSendingQueues>0 then
    MemoAdd(1,'Clear ALL Sending Queues for /echo');
  end;

procedure TForm1.btnDisconnectClick(Sender: TObject);
  begin
  if SockProv.wsDisconnectAll>0 then
    MemoAdd(1,'Disconnect ALL Web Sockets from /');

  if EchoProv.wsDisconnectAll>0 then
    MemoAdd(1,'Disconnect ALL Web Sockets from /echo');
  end;

procedure TForm1.btnMemoClearClick(Sender: TObject);
  begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('--- Cleared ---');
  end;

{ General "Server" events ... }

procedure TForm1.ServerListenStart(Sender: TRtcConnection);
  begin
  if Sender.Sync(ServerListenStart) then Exit;

  btnListen.Caption:='STOP';
  MemoAdd(1,'Server Listening on Port '+Sender.ServerPort, Sender);
  end;

procedure TForm1.ServerListenStop(Sender: TRtcConnection);
  begin
  if Sender.Sync(ServerListenStop) then Exit;

  btnListen.Caption:='GO';
  MemoAdd(1,'Server stopped Listening.', Sender);
  end;

procedure TForm1.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  if Sender.Sync(ServerListenError,E) then Exit;

  btnListen.Caption:='GO';
  MemoAdd(1,'Error starting Server listener on Port '+Sender.ServerPort+#13#10+
          E.ClassName+' : '+E.Message, Sender);
  end;

{ "SockProv:TRtcDataProvider" events ... }

procedure TForm1.SockProvCheckRequest(Sender: TRtcConnection);
  begin
  if Sender.Request.FileName='/' then
    begin
    if Sender.Request.WSUpgrade then
      begin
      { We have recieved a Web Socket upgrade Request at '/' }
      Sender.Accept; // Accept Request
      Sender.Response.WSUpgrade:=True; // Prepare Upgrade Response
      Sender.Write; // Send Upgrade Response
      end
    else
      begin
      { Send a HTML file for the Web Browser.
        The file being sent uses JavaScript to open a Web Socket connection
        to 'ws://localhost' and ECHO all Web Socket "Frames" received. }
      Sender.Accept;
      Sender.WriteEx(Read_FileEx('ws.html'));
      end;
    end
  else if SameText(Sender.Request.FileName,'/time') then
    begin
    { Just for the sake of an example, we will also be
      accepting and responding to normal '/time' requests here ... }
    Sender.Accept;
    Sender.Write('Date & Time: '+DateTimeToStr(Now));
    end;
  end;

procedure TForm1.SockProvWSDataReceived(Sender: TRtcConnection);
  var
    wf:TRtcWSFrame;
    s:RtcString;
  begin
  wf:=Sender.wsFrameIn; // <- Web Socket Frame currenty being received

  if wf.wfStarted and (wf.wfOpcode=wf.waOpcode) then // Started receiving a new Frame set
    MemoAdd(2,'---> IN: '+wf.wfHeadInfo, Sender);

  // We want to analyze all text message with less than 500 characters total ...
  if wf.waFinal and (wf.waOpcode=wf_Text) and (wf.waPayloadLength<500) then
    begin
    // wait until the entire Payload has been received.
    // This should NOT be more than 500 bytes (see above) ...
    if wf.wfComplete then
      begin
      s:=wf.wfRead; // <- reading Frame's "Payload" data ...
      MemoAdd(3,'IN ('+
        IntToStr(wf.wfTotalInOut)+'/'+
        IntToStr(wf.wfTotalLength)+') >>> '+
        s, Sender);

      // We have received a text message we want to respond to ...
      if s='WebSocket rocks' then
        { We can send a new Frame as a response to the Client that sent us the
          Web Socket Frame, by using the "Sender.wSend" method like this ... }
        Sender.wSend(wf_Text,'Absolutely!');
      end;
    end
  else
    begin
    s:=wf.wfRead;  // <- reading Frame's "Payload" data ...
    if wf.wfOpcode=wf_Text then
        MemoAdd(3,'IN ('+
          IntToStr(wf.wfTotalInOut)+'/'+
          IntToStr(wf.wfTotalLength)+') TXT >>> '+
          s, Sender)
    else
        MemoAdd(3,'IN ('+
          IntToStr(wf.wfTotalInOut)+'/'+
          IntToStr(wf.wfTotalLength)+') BIN('+IntToStr(length(s))+') >>>', Sender);
    end;

  if wf.wfDone then // <- Frame Done (all read)
    begin
    if wf.waFinal then // <- final Frame?
      MemoAdd(2,'IN DONE, '+
        IntToStr(wf.wfTotalInOut)+'/'+IntToStr(wf.wfTotalLength)+' bytes <-----', Sender)
    else // More data shoud arrive in the next Frame ...
      MemoAdd(3,'<--- IN ... MORE --->', Sender);
    end;
  end;

{ "EchoProv:TRtcDataProvider" events ... }

procedure TForm1.EchoProvCheckRequest(Sender: TRtcConnection);
  begin
  { To test this "ECHO" Web Socket Data Provider, you need
    a Web Socket Client requesting the '/echo' resource.
    Using the WSockCliTest Project, enter '/echo' into the
    "SrvURI" edit field (where '/' is entered by default)
    and open a new connection to this Server.

    This Data Provider returns anything it receives. }
  if Sender.Request.FileName='/echo' then
    if Sender.Request.WSUpgrade then
      begin
      { We have recieved a Web Socket upgrade Request for '/echo' }
      Sender.Accept; // Accept Request
      Sender.Response.WSUpgrade:=True; // Prepare Upgrade Response
      Sender.Write; // Send Upgrade Response
      end;
  end;

procedure TForm1.EchoProvWSDataReceived(Sender: TRtcConnection);
  var
    wfi, wfo:TRtcWSFrame;
  begin
  { This is a simple "ECHO" Web Socket provider,
    sending back all Frames we get from the Client ... }
  wfi:=Sender.wsFrameIN;

  if wfi.wfStarted and (wfi.wfOpcode=wfi.waOpcode) then // Started receiving a new Frame set ...
    MemoAdd(2,'---> echo START: '+wfi.wfHeadInfo, Sender);

  if wfi.wfFinished then
    MemoAdd(2,'echo fINished, '+IntToStr(wfi.wfTotalInOut)+'/'+IntToStr(wfi.wfTotalLength)+' bytes <-----', Sender)
  else
    MemoAdd(3,'echo IN: '+wfi.wfHeadInfo, Sender);

  { In this example, we do NOT want to ECHO a "PONG".
    To do this, we can simply read any data received and EXIT here. }
  if wfi.waOpcode=wf_Pong then
    begin
    { If we skip this step (do not use wfRead or wfRedEx), any Payload
      we have received would remain in the Frame buffer until the entire
      Frame has been received. To minimize memory usage, we should always
      read any Payload we receive, even if we are going to discard it. }
    wfi.wfRead;
    Exit;
    end;

  { Get a pointer to the Frame we are currently sending
    (see code below for more info) ... }
  wfo:=Sender.wsTemp['retour'];

  { We don't have a "retour" object yet? }
  if wfo=nil then
    begin
    { Create a new Frame for sending and copying all Header
      attributes from the Frame just being received ... }
    wfo:=TRtcWSFrame.Create(wfi);

    { Store a temporary pointer to the Frame we use for sending,
      so we can find it in the next "OnWSDataReceived" event (see code above) }
    Sender.wsTemp['retour']:=wfo;

    { If we receive a "PING", we want to send it back as a "PONG" ... }
    if wfo.waOpcode=wf_Ping then
      wfo.waOpcode:=wf_Pong;

    { Add all Payload data already received with this Frame
      to be sent back in the first chunk with our sending Frame }
    wfo.wfWriteEx(wfi.wfReadEx);

    MemoAdd(2,'---> Echo OUT: '+wfo.wfHeadInfo, Sender);

    { Add the new Frame (created above) to the sending queue.
      If there is nothing else being sent and the sending queue
      for this Client is empty, This will start sending immediately. }
    Sender.wSend(wfo,'echo');

    { All Frame objects used with any "wSend*" method will be auto-freed
      by the connection component after the Frame has been sent out,
      or after a disconnect - if the Frame could not be sent out. }
    end
  else
    begin
    { Add more Payload data to the Frame we use for sending.
      If our "retour" Frame is already being sent,
      this will send the new Payload data immediately.

      Since we are using the "wSendMore" method to forward all the
      "Payload" we are receiving from the Client and the Client could
      be sending us multiple connected Frames and we might not be
      able to send Frames as fast as we are receiving them (because
      of a queue and sending buffers), it is important to call the
      "wSendMore" method with the "wfFinished" flag from the source Frame,
      to notify the output Frame when the last bytes have been written. }
    Sender.wSendMore(wfo, wfi.wfReadEx, wfi.wfFinished);

    { NOTE: Unless you are currently in the "OnWSDataSent" event and are
      preparing more "Payload" data to be sent using the "FrameOut" object,
      you should NOT use the "wfWrite" or "wfWriteEx" methods directly to
      add more "Payload" data to the Frame. Once the "Frame" object has
      already been added to the Sending queue, it may ONLY be accessed
      directly from within the "OnWSDatSent" event. To add more Payload
      data from any other event to a Frame that is already being sent
      or placed into the sending queue, you have to use the "wSendMore"
      method, which can be called from anywhere, takes care of the sending
      queue, writes the new payload at the end of the "Frame" and continues
      sending the "Frame" if all the other Frames placed into the queue
      before this Frame have already been sent out, or buffers all
      the new Payload until the Frame can be sent out. }
    end;

  if wfi.wfFinished and assigned(wfo) then // We are finished with this Frame?
    begin
    MemoAdd(2,'Echo DONE: '+wfi.wfHeadInfo+' ('+IntToStr(wfi.wfTotalInOut)+'/'+IntToStr(wfi.wfTotalLength)+' bytes) <-----', Sender);
    { To avoid unused objects lingering in memory when using ARC, we should
      remove the pointer we have stored in "wsTemp" when it is no longer needed
      (which is the case after the Frame we are receving is finished). }
    Sender.wsTemp['retour']:=nil;
    end;
  end;

{ Events used by both "Web Socket" Data Providers ... }

procedure TForm1.AnyProvWSDataSent(Sender: TRtcConnection);
  var
    wf:TRtcWSFrame;
    bytes:int64;
  begin
  wf:=Sender.wsFrameOut;
  { If there is no Frame object (Sender.FrameOut=NIL), then this is just a
    notification to let us know that all data has been sent to this Client. }
  if wf=nil then
    MemoAdd(3,'---> ALL SENT <---',Sender)
  // you can check the Name of the Frame object like this ...
  else if wf.wfName='ws-file' then // <- we used "ws-file" in Send() when sending out files
    begin
    if wf.wfStarted then // <- we have NOT read any "Payload" data from this Frame yet ...
      MemoAdd(2,'---> OUT: '+wf.wfHeadInfo, Sender);
    if not wf.wfComplete then // <- Frame is NOT complete yet, more "Payload" data should be sent ...
      begin
      MemoAdd(3,'SENT '+
              IntToStr(wf.wfPayloadInOut)+'/'+IntToStr(wf.waPayloadLength)+' bytes.',Sender);

      { How many bytes do we have to send out in this Frame?
        PayloadLength = Payload length of the current Frame
        PayloadInOut = Payload already read and sent }
      bytes:=wf.waPayloadLength - wf.wfPayloadInOut;

      { Limit the number of bytes copied to sending buffers and sent out at once.
        Using smaller buffers will slow down file transfer and use a bit more CPU,
        but it also reduces the amount of RAM required per Client for sending files. }
      if bytes>32*1024 then bytes:=32*1024; // 32 KB is relatively small

      // Read the next file chunk and add it to this Frames "PayLoad" for sending ...
      wf.wfWriteEx( Read_FileEx(wf.asText['fname'], wf.wfPayloadInOut, bytes) );

      { Because we have set the "waPayloadLength" property, "wf.wfComplete"
        will be returning TRUE when all "Payload" data has been written ... }
      if wf.wfComplete then // Payload complete, no more bytes can be added to this Frame ...
        MemoAdd(3,'OUT Complete, '+IntToStr(wf.waPayloadLength)+' bytes.', Sender);
      end
    else if wf.wfDone then
      MemoAdd(2,'OUT! '+
          IntToStr(wf.wfPayloadInOut)+'/'+IntToStr(wf.waPayloadLength)+' bytes <-----', Sender);
    end
  { We used "ws-multi" in Send() for sending files in multiple frames by
    explicitly setting the total length using the "wfTotalLength" property ... }
  else if wf.wfName='ws-multi' then
    begin
    if wf.wfDone then
      MemoAdd(3,'OUT! '+wf.wfHeadInfo,Sender);

    if wf.wfTotalInOut<wf.wfTotalLength then
      begin
      if wf.wfTotalInOut=0 then
        MemoAdd(2,'---> OUT Start: '+IntToStr(wf.wfTotalLength)+' bytes --->',Sender);

      { How many bytes are left to be sent from the File?
        wf.wfTotalLength = our file size (total bytes to send),
        wf.wfTotalInOut = number of bytes already sent }
      bytes:=wf.wfTotalLength - wf.wfTotalInOut;

      { Limit the number of bytes sent out at once.
        Using 8KB limit here for demonstration purposes ONLY! }
      if bytes>8*1024 then bytes:=8*1024;

      // Read the next file chunk and add it to this Frames "PayLoad" for sending ...
      wf.wfWriteEx( Read_FileEx(wf.asText['fname'], wf.wfTotalInOut, bytes) );

      MemoAdd(3,'SENT '+
              IntToStr(wf.wfTotalInOut)+'/'+
              IntToStr(wf.wfTotalLength)+' bytes', Sender);
      end
    else // File complete
      MemoAdd(2,'OUT Complete: '+IntToStr(wf.wfTotalInOut)+'/'+IntToStr(wf.wfTotalLength)+' bytes <-----', Sender);
    end
  { We used "ws-mime" in Send() for sending compressed mime-encoded files in multiple frames,
    without explicity setting the "waPayloadLength" or the "wfTotalLength" ... }
  else if wf.wfName='ws-mime' then
    begin
    if wf.wfDone then
      MemoAdd(3,'OUT! '+wf.wfHeadInfo,Sender);

    if wf.asLargeInt['sent']<wf.asLargeInt['total'] then
      begin
      if wf.asLargeInt['sent']=0 then
        MemoAdd(2,'---> OUT Start: '+IntToStr(wf.asLargeInt['total'])+' bytes --->',Sender);

      { How many bytes are left to be sent from the File?
        wf.asLargeInt['total'] = our file size (total bytes to send),
        wf.asLargeInt['sent'] = number of bytes already sent }
      bytes:=wf.asLargeInt['total'] - wf.asLargeInt['sent'];

      { Limit the number of bytes sent out at once.
        Using 16KB limit here for demonstration purposes ONLY! }
      if bytes>16*1024 then
        bytes:=16*1024
      else
        begin
        { Would know now that no more data will be sent after this, so we should
          set the "wf.wfFinished:=TRUE" just before sending the last bytes out,
          to have the "wf.waFinal" flag set to TRUE for the last Payload data. }
        wf.wfFinished:=True;
        end;

      { Read the next file chunk, compress it, encode it using Base-64 encoding,
        then add it to this Frames "Payload" for sending ... }
      wf.wfWriteEx(
        Mime_EncodeEx(
          ZCompress_Ex(
            Read_FileEx(wf.asText['fname'], wf.asLargeInt['sent'], bytes),
            zcFastest),
          TRUE
          )
        );
      { To make decoding and decompression possible even if our Frames will be
        re-packaged (split and re-sent using different sizes), we will be using
        #12 as our end-marker for every compressed and encoded chunk. }
      wf.wfWrite(#12);

      { Update the number of bytes we have already sent from the file }
      wf.asLargeint['sent']:=wf.asLargeInt['sent']+bytes;

      MemoAdd(3,'SENT '+
              IntToStr(wf.wfTotalInOut)+'/'+
              IntToStr(wf.wfTotalLength)+' MIME, '+
              IntToStr(wf.asLargeInt['sent'])+'/'+
              IntToStr(wf.asLargeInt['total'])+' ORIG bytes', Sender);
      end
    else // File complete
      begin
      { Setting "wf.wfFinished:=TRUE" here is a "safety measure",
        to make sure that "wf.waFinal" flag will be set to TRUE
        if we do NOT plan on sending any more Payload data out.
        This is important in this example, because the file we
        are sending out could have 0 bytes in total. }
      wf.wfFinished:=True;
      MemoAdd(2,'OUT Complete: '+
          IntToStr(wf.wfTotalInOut)+'/'+
          IntToStr(wf.wfTotalLength)+' MIME, '+
          IntToStr(wf.asLargeInt['sent'])+'/'+
          IntToStr(wf.asLargeInt['total'])+' ORIG bytes <-----', Sender);
      end;
    end
  else if wf.wfName='' then // all Frames without a "name" ...
    begin
    if wf.wfStarted then
      MemoAdd(2,'-=+> OUT: '+wf.wfHeadInfo, Sender);
    MemoAdd(3,'SENT '+IntToStr(wf.wfPayloadInOut)+'/'+IntToStr(wf.waPayloadLength)+' bytes', Sender);
    if wf.wfDone then
      MemoAdd(2,'OUT! '+IntToStr(wf.wfPayloadInOut)+' bytes <+=---', Sender);
    end;
  end;

procedure TForm1.AnyProvWSConnect(Sender: TRtcConnection);
  begin
  { This event is not required, but we are using it to send notifications
    to all other (related) Clients when a new Web Socket Client connects. }
  MemoAdd(1,'---> CONNECTED #'+IntToStr(Sender.WSCount), Sender);

  { We can start sending Web Socket Frames to this Client now: }
  Sender.wSend(wf_Text,'Hello, Web Socket Client!');

  { Just as an example, we will be sending a Text message to
    all OTHER Clients connected using the same Data Provider ... }
  Sender.wSendToOthers( Sender.WSGetID, // our connection ID
      wf_Text,'New Client from '+Sender.PeerAddr+':'+Sender.PeerPort+' Connected!');
  end;

procedure TForm1.AnyProvWSDisconnect(Sender: TRtcConnection);
  begin
  { This event is also not required, but we are using it to send notifications
    to all other (related) Clients when a Web Socket Client disconnects. }
  MemoAdd(1,'---> DISCONNECTED #'+IntToStr(Sender.WSCount), Sender);

  { Using "Send" from "OnWSDisconnect" to try and send
    data to the Client which has already disconnected
    will have no effect - for obvious reasons ... }
  Sender.wSend(wf_Text,'This will NOT be sent.');

  { Just to demonstrate that nothing bad will happen if you try
    to Send more data using the "Sender" object even from the
    "OnWSDisconnect" event (the Client has already disconnected),
    we are using the "wSendToAll" method from here to send a
    message to ALL Clients connected to the same Data Provider. }
  Sender.wSendToAll(
      wf_Text,'Client from '+Sender.PeerAddr+':'+Sender.PeerPort+' Disconnected!');

  { A cleaner approach is to use the Sender.wSendToOthers method
    and supply Sender.WSGetID as the 1st parameeter to SKIP this
    Client, since it has been disconnected, but the result is the same ... }
  Sender.wSendToOthers( Sender.WSGetID,
      wf_Text,'Now, there is one less Client to talk to :P');
  end;

procedure TForm1.AnyProvDataReceived(Sender: TRtcConnection);
  begin
  // This event is implementer for information-only (it is not required)
  if Sender.Request.Complete then
    MemoAdd(3,'---> IN ' +
      Sender.Request.Method+' '+Sender.Request.URI+#13#10+
      Sender.Request.HeaderText+#13#10+
      Sender.Read+#13#10+
      IntToStr(Sender.Request.ContentIn)+'/'+
      IntToStr(Sender.Request.ContentLength)+' bytes <-----', Sender);
  end;

procedure TForm1.AnyProvResponseDone(Sender: TRtcConnection);
  begin
  // Another event used only for informational purposes (not required)
  MemoAdd(3,'---> OUT '+
          IntToStr(Sender.Response.StatusCode)+' '+Sender.Response.StatusText+#13#10+
          Sender.Response.HeaderText+#13#10+
          IntToStr(Sender.Response.ContentOut)+'/'+
          IntToStr(Sender.Response.ContentLength)+' bytes <-----', Sender);
  end;

procedure TForm1.AnyProvDisconnect(Sender: TRtcConnection);
  begin
  // And ... another event used only for information (not required)
  MemoAdd(1,'---> DISCONNECTED!', Sender);
  end;

procedure TForm1.AnyProvWSDataIn(Sender: TRtcConnection);
  begin
  // More basic information (not required)
  MemoAdd(4,'<<<< '+IntToStr(Sender.DataIn)+' <<--',Sender);
  end;

procedure TForm1.AnyProvWSDataOut(Sender: TRtcConnection);
  begin
  // And ... even more basic information (also not required)
  MemoAdd(4,'>>>> '+IntToStr(Sender.DataOut)+' -->>',Sender);
  end;

procedure TForm1.cbLOGChange(Sender: TObject);
  begin
  LogLevel:=cbLOG.ItemIndex;
  end;

end.
