unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, rtcDataCli,
  rtcInfo, rtcConn, rtcHttpCli, rtcThrPool, rtcLog, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Edit;

type
  TForm1 = class(TForm)
    Client: TRtcHttpClient;
    SockReq: TRtcDataRequest;
    Memo1: TMemo;
    btnConnect: TButton;
    btnSendText: TButton;
    btnSendBinary: TButton;
    btnSendPing: TButton;
    btnSendPong: TButton;
    btnClear: TButton;
    eSrvAddr: TEdit;

    procedure btnConnectClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure SockReqResponseDone(Sender: TRtcConnection);
    procedure SockReqDataSent(Sender: TRtcConnection);
    procedure SockReqConnectLost(Sender: TRtcConnection);
    procedure btnSendTextClick(Sender: TObject);
    procedure btnSendBinaryClick(Sender: TObject);
    procedure btnSendPingClick(Sender: TObject);
    procedure btnSendPongClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSendTxtOneClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SockReqWSDataReceived(Sender: TRtcConnection);
    procedure SockReqWSDataSent(Sender: TRtcConnection);
    procedure SockReqWSDisconnect(Sender: TRtcConnection);
    procedure SockReqWSConnect(Sender: TRtcConnection);
    procedure btnSendExeMultiClick(Sender: TObject);
    procedure btnEmptyQClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure ClientConnect(Sender: TRtcConnection);
    procedure ClientDisconnect(Sender: TRtcConnection);
    procedure ClientConnectError(Sender: TRtcConnection; E: Exception);
    procedure btnSendTxtMultiClick(Sender: TObject);
    procedure btnSendExeOneClick(Sender: TObject);
    procedure btnSendTxtRamClick(Sender: TObject);
    procedure btnSendExeRamClick(Sender: TObject);
    procedure btnMemoClearClick(Sender: TObject);
    procedure SockReqWSDataIn(Sender: TRtcConnection);
    procedure SockReqWSDataOut(Sender: TRtcConnection);
    procedure btnSendTxtChunksClick(Sender: TObject);
    procedure btnSendExeChunksClick(Sender: TObject);
    procedure ClientConnectFail(Sender: TRtcConnection);
    procedure cbLOGChange(Sender: TObject);
    procedure ClientConnectLost(Sender: TRtcConnection);
    procedure btnClearClick(Sender: TObject);

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

{$R *.fmx}

{ Utility methods ... }

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
  LogLevel:=4; // Full Debug cbLog.ItemIndex;
  MyThread:=TRtcThread.Create; // Used by "MemoAdd" for "PostQuickJob" to serialize all calls
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  TRtcThread.Stop(MyThread);
  MyThread:=nil;

  Client.DisconnectNow();
  end;

procedure TForm1.btnConnectClick(Sender: TObject);
  begin
  if not Client.isConnecting then // not already connected or connecting ...
    begin
    Client.AutoConnect:=True;
    Client.ServerAddr:=eSrvAddr.Text;

    { The following lines of code send a request to the
      Server (Address and Port configured above) to
      upgrade this connection to a Web Socket.

      If successful, the OnWSConnect event will
      be triggered on the "SockReq" component ... }
    SockReq.Request.URI:='/echo'; // SrvURI.Text;
    SockReq.Request.WSUpgrade:=True;
    SockReq.PostMethod();
    end
  else
    Client.Disconnect;
  end;

procedure TForm1.cbLOGChange(Sender: TObject);
  begin
  // LogLevel:=cbLOG.ItemIndex;
  end;

procedure TForm1.btnSendTextClick(Sender: TObject);
  begin
  { Since this is a Client which only has one connection and
    the connection component is never destroyed, we can simply
    use the Client component directly to send Web Socket Frames
    after establishing a Web Socket connection. If a Web Socket
    connection is NOT established, this method does nothing. }

  if Client.wSend(wf_Text,'Date/Time:'+DateTimeToStr(Now)) then
    MemoAdd(1,'Sending Text');
  end;

procedure TForm1.btnSendBinaryClick(Sender: TObject);
  var
    ar:RtcByteArray;
    a:integer;
  begin
  SetLength(ar,255);
  for a:=0 to length(ar)-1 do ar[a]:=a;
  if Client.wSend(wf_Binary,ar) then
    MemoAdd(1,'Sending Binary');
  end;

procedure TForm1.btnSendPingClick(Sender: TObject);
  begin
  if Client.wSend(wf_Ping,'Ping?') then
    MemoAdd(1,'Sending Ping');
  end;

procedure TForm1.btnSendPongClick(Sender: TObject);
  begin
  if Client.wSend(wf_Pong,'Pong!') then
    MemoAdd(1,'Sending Pong');
  end;

procedure TForm1.btnClearClick(Sender: TObject);
  begin
  Memo1.Lines.Clear;
  end;

procedure TForm1.btnCloseClick(Sender: TObject);
  begin
  if Client.wSend(wf_Close) then
    MemoAdd(1,'Sending Close');
  end;

procedure TForm1.btnSendTxtOneClick(Sender: TObject);
  var
    wf:TRtcWSFrame;
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

  { "wf" object is managed and auto-freed by the connection component }
  if Client.wSend(wf,'ws-file') then // Frame.wfName will be set to "ws-file"
    MemoAdd(1,'Sending TEXT File in one Frame');
  end;

procedure TForm1.btnSendExeOneClick(Sender: TObject);
  var
    wf:TRtcWSFrame;
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

  { "wf" object is managed and auto-freed by the connection component }
  if Client.wSend(wf,'ws-file') then // Frame.wfName will be set to "ws-file"
    MemoAdd(1,'Sending EXE File in one Frame');
  end;

procedure TForm1.btnSendTxtMultiClick(Sender: TObject);
  var
    wf:TRtcWSFrame;
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
    use 'wfTotalLength' here to set the total number of bytes to be sent.
    The alternative would be to manually set "wfFinished:=TRUE" when we
    know that no more Payload data will be sent for this Frame. }
  wf.wfTotalLength:=File_Size(wf.asText['fname']);

  { "wf" object will be managed and auto-freed by the connection component }
  if Client.wSend(wf,'ws-multi') then // Frame.wfName will be set to "ws-multi"
    MemoAdd(1,'Sending TEXT File in multiple Frames');
  end;

procedure TForm1.btnSendExeMultiClick(Sender: TObject);
  var
    wf:TRtcWSFrame;
  begin
  { Sending this Application's EXE file as an example for sending files in
    multiple Frames, with each Frame containing only a part of the File
    and the file being read and sent from the "OnWSDataSent" event ... }

  // We need a new Frame for sending out a Text file with waFinal=FALSE
  wf:=TRtcWSFrame.Create(wf_Binary,FALSE);

  { Use Frame's local variable 'fname' to set the file name,
    so we can access it directly from "OnWSDataSent" events. }
  wf.asText['fname']:=AppFileName;

  { Since we KNOW how many bytes we want to send in total, we can
    use 'wfTotalLength' here to set the total number of bytes to be sent.
    The alternative would be to manually set "wfFinished:=TRUE" when we
    know that no more Payload data will be sent for this Frame. }
  wf.wfTotalLength:=File_Size(wf.asText['fname']);

  { "wf" object will be managed and auto-freed by the connection component }
  if Client.wSend(wf,'ws-multi') then // Frame.wfName will be set to "ws-multi"
    MemoAdd(1,'Sending EXE File in multiple Frames');
  end;

procedure TForm1.btnSendTxtChunksClick(Sender: TObject);
  var
    wf:TRtcWSFrame;
  begin
  { Sending this Unit's source code as an example for sending textual
    data in multiple Frames, without knowing up-front how much data
    has to be sent, using "wSend" to send the header, then using
    the "OnWSDataSent" event to send the file contents, without
    ever checking if the file exists or how large it is. }

  // We need a new Frame for sending out a Text file with waFinal=FALSE
  wf:=TRtcWSFrame.Create(wf_Text,FALSE);

  { We will use Frame's local variable 'fname' to set the file name,
    so we can access it directly from "OnWSDataSent" events ... }
  wf.asText['fname']:='Unit1.pas';

  { "wf" object will be managed and auto-freed by the connection component }
  if Client.wSend(wf,'ws-chunks') then // Frame.wfName will be set to "ws-chunks"
    MemoAdd(1,'Sending TEXT File in multiple Chunks');
  end;

procedure TForm1.btnSendExeChunksClick(Sender: TObject);
  var
    wf:TRtcWSFrame;
  begin
  { Sending this Application EXE file as an example for sending Binary
    data in multiple Frames, without knowing up-front how much data
    has to be sent, using "wSend" to send the header, then using
    the "OnWSDataSent" event to send the file contents, without
    ever checking if the file exists or how large it is. }

  // We need a new Frame for sending out a Binary file with waFinal=FALSE
  wf:=TRtcWSFrame.Create(wf_Binary,FALSE);

  { We will use Frame's local variable 'fname' to set the file name,
    so we can access it directly from "OnWSDataSent" events ... }
  wf.asText['fname']:=AppFileName;

  { "wf" object will be managed and auto-freed by the connection component }
  if Client.wSend(wf,'ws-chunks') then // Frame.wfName will be set to "ws-chunks"
    MemoAdd(1,'Sending EXE File in multiple Chunks');
  end;

procedure TForm1.btnSendTxtRamClick(Sender: TObject);
  begin
  { Instead of implementing the "OnWSDataSent" event,
    if we can afford keeping a copy of the entire file in RAM
    for every Client until the entire File has been sent out,
    we can also use the "wSend*" method like this  ... }

  if Client.wSend(wf_Text,Read_File('Unit1.pas')) then
    MemoAdd(1,'Sending TEXT File from RAM in 1 Frame');
  end;

procedure TForm1.btnSendExeRamClick(Sender: TObject);
  begin
  { Instead of implementing the "OnWSDataSent" event,
    if we can afford keeping a copy of the entire file in RAM
    for every Client until the entire File has been sent out,
    we can also use the "wSend*" method like this  ... }

  if Client.wSend(wf_Binary,Read_FileEx(AppFileName)) then
    MemoAdd(1,'Sending EXE File from RAM in 1 Frame');
  end;

procedure TForm1.btnEmptyQClick(Sender: TObject);
  begin
  if Client.wsClearSendingQueue then
    MemoAdd(1,'Clear Web Socket Sending Queue');
  end;

procedure TForm1.btnDisconnectClick(Sender: TObject);
  begin
  if Client.wsDisconnect then
    MemoAdd(1,'Disconnect Web Socket');
  end;

procedure TForm1.btnMemoClearClick(Sender: TObject);
  begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('--- Cleared ---');
  end;

{ "Client:TRtcHttpClient" connection events ... }

procedure TForm1.ClientConnect(Sender: TRtcConnection);
  begin
  if Sender.Sync(ClientConnect) then Exit;

  btnConnect.Text:='D';
  MemoAdd(1,'Connected to '+Sender.PeerAddr+':'+Sender.PeerPort, Sender);
  end;

procedure TForm1.ClientDisconnect(Sender: TRtcConnection);
  begin
  if Sender.Sync(ClientDisconnect) then Exit;

  btnConnect.Text:='C';
  MemoAdd(1,'Disconnected from '+Sender.PeerAddr+':'+Sender.PeerPort, Sender);
  end;

procedure TForm1.ClientConnectError(Sender: TRtcConnection; E: Exception);
  begin
  if Sender.Sync(ClientConnectError,E) then Exit;

  btnConnect.Text:='C';
  MemoAdd(1,'ERROR connecting to '+
          Sender.ServerAddr+':'+Sender.ServerPort+#13#10+
          E.ClassName+' : '+E.Message, Sender);
  end;

procedure TForm1.ClientConnectFail(Sender: TRtcConnection);
  begin
  if Sender.Sync(ClientConnectFail) then Exit;

  btnConnect.Text:='C';
  MemoAdd(1,'FAILED connecting to '+
          Sender.ServerAddr+':'+Sender.ServerPort,
          Sender);
  end;

procedure TForm1.ClientConnectLost(Sender: TRtcConnection);
begin

end;

{ "SockReq:TRtcDataRequest" HTTP Request/Response events ... }

procedure TForm1.SockReqDataSent(Sender: TRtcConnection);
  begin
  { This event is implemented for information only.
    It is not required for Client-side Web Sockets to work. }
  if Sender.Request.Complete then
    MemoAdd(3,'---> OUT ' + Sender.Request.Method+' '+Sender.Request.URI+#13#10+
            Sender.Request.HeaderText+#13#10+
            IntToStr(Sender.Request.ContentOut)+'/'+
            IntToStr(Sender.Request.ContentLength)+' bytes <-----', Sender);
  end;

procedure TForm1.SockReqResponseDone(Sender: TRtcConnection);
  begin
  { This event is also here only for informational purposes.
    It is not required for Client-side Web Sockets to work. }
  MemoAdd(3,'---> IN '+
          IntToStr(Sender.Response.StatusCode)+' '+Sender.Response.StatusText+#13#10+
          Sender.Response.HeaderText+#13#10+
          IntToStr(Sender.Response.ContentIn)+'/'+
          IntToStr(Sender.Response.ContentLength)+' bytes <-----', Sender);
  end;

procedure TForm1.SockReqConnectLost(Sender: TRtcConnection);
  begin
  // Information only ...
  MemoAdd(1,'---> DISCONNECTED', Sender);
  end;

{ "SockReq:TRtcDataRequest" Web Socket events ... }

procedure TForm1.SockReqWSConnect(Sender: TRtcConnection);
  begin
  MemoAdd(1,'---> CONNECTED', Sender);

  { The Web Socket connection has already been established before
    this event was called and the "Sender:TRtcConnection" component
    is ready to start accepting new Frames in its Sending Queue,
    so we can start sending Web Socket Frames from here: }
  Sender.wSend(wf_Text,'WebSocket rocks');
  end;

procedure TForm1.SockReqWSDataReceived(Sender: TRtcConnection);
  var
    wf:TRtcWSFrame;
    s:RtcString;
  begin
  wf:=Sender.wsFrameIn; // <- using the "Sender.FrameIn" property

  if wf.wfStarted and (wf.wfOpcode=wf.waOpcode) then // Started receiving a new Frame set ...
    MemoAdd(2,'---> IN: '+wf.wfHeadInfo, Sender);

  if wf.wfFinished and (wf.wfOpcode=wf_Text) and (wf.waPayloadLength<1000) then // short text message
    begin
    if wf.wfComplete then
      begin
      s:=wf.wfRead; // <- reading Frame "Payload" data ...
      MemoAdd(3,'IN ('+
        IntToStr(wf.wfTotalInOut)+'/'+
        IntToStr(wf.wfTotalLength)+') >>> '+
        s, Sender);
      if s='Hello, Web Socket Client!' then
        { From here, after receiving a Frame from a Server,
          we can send a response back to the Server simply
          by using the "Sender.Send" method directly ... }
        Sender.wSend(wf_Text,'And a HELLO to you, Web Socket Server!');
      end;
    end
  else // if ws.Complete then // -> this would buffer everything received
    begin
    s:=wf.wfRead;  // <- reading Frame "Payload" data ...
    if wf.wfOpcode=wf_Text then
      MemoAdd(3,'IN ('+
          IntToStr(wf.wfTotalInOut)+'/'+
          IntToStr(wf.wfTotalLength)+') TXT >>> '+
          s, Sender)
    else
      MemoAdd(3,'IN ('+
        IntToStr(wf.wfTotalInOut)+'/'+
        IntToStr(wf.wfTotalLength)+') BIN ('+IntToStr(length(s))+') >>>',
        Sender);
    end;

  if wf.wfDone then // <- Frame Done (all read)
    if wf.waFinal then // <- final Frame?
      MemoAdd(2,'IN DONE, '+
        IntToStr(wf.wfTotalInOut)+'/'+IntToStr(wf.wfTotalLength)+' bytes <-----', Sender)
    else // More data shoud arrive in this next Frame ...
      MemoAdd(3,'<--- IN ... MORE --->'+wf.wfHeadInfo, Sender);
  end;

procedure TForm1.SockReqWSDataSent(Sender: TRtcConnection);
  var
    wf:TRtcWSFrame;
    bytes:int64;
    data:RtcString;
  begin
  wf:=Sender.wsFrameOut;
  { If there is no Frame object, then this is just a notification
    that all been was sent (Web Socket Frames sending queue is empty). }
  if wf=nil then
    MemoAdd(3,'---> ALL SENT <---',Sender)
  { We've used "ws-file" in Send() when sending files in a single Frame with "waPayloadLength" set }
  else if wf.wfName='ws-file' then
    begin
    if wf.wfStarted then // <- we have NOT read any "Payload" data from this Frame yet ...
      MemoAdd(2,'---> OUT: '+wf.wfHeadInfo,Sender);

    if wf.wfDone then // <- Frame is done, no "Payload" data left to be read ...
      MemoAdd(2,'OUT! '+
          IntToStr(wf.wfPayloadInOut)+'/'+
          IntToStr(wf.waPayloadLength)+' bytes <-----',Sender)
    else
      begin
      MemoAdd(3,'SENT '+
              IntToStr(wf.wfPayloadInOut)+'/'+
              IntToStr(wf.waPayloadLength)+' bytes', Sender);

      { How many bytes do we have to send out in this Frame?
        PayloadLength = Payload length of the current Frame,
        PayloadInOut = Payload already read and sent }
      bytes:=wf.waPayloadLength - wf.wfPayloadInOut;

      { Limit the number of bytes copied to sending buffers and sent out at once.
        Using smaller buffers will slow down file transfer and use a bit more CPU,
        but it also reduces the amount of RAM required per Client for sending files. }
      if bytes>32*1024 then
        bytes:=32*1024; // 32 KB is relatively small

      // Read the next file chunk and add it to this Frames "PayLoad" for sending ...
      wf.wfWriteEx( Read_FileEx(wf.asText['fname'], wf.wfPayloadInOut, bytes) );

      if wf.wfComplete then // Payload complete, no more bytes can be added to this Frame ...
        MemoAdd(3,'OUT Complete, '+IntToStr(wf.waPayloadLength)+' bytes', Sender);
      end;
    end
  { We've used "ws-multi" in Send() when sending files in multiple frames with "wfTotalLength" set }
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

      { Limit the number of bytes sent out at once. }
      if bytes>8*1024 then
        bytes:=8*1024; // using 8 KB here as an example

      // Read the next file chunk and add it to this Frames "PayLoad" for sending ...
      wf.wfWriteEx( Read_FileEx(wf.asText['fname'], wf.wfTotalInOut, bytes) );

      MemoAdd(3,'SENT '+
              IntToStr(wf.wfTotalInOut)+'/'+
              IntToStr(wf.wfTotalLength)+' bytes', Sender);
      end
    else // File complete
      MemoAdd(2,'OUT Complete: '+
          IntToStr(wf.wfTotalInOut)+'/'+
          IntToStr(wf.wfTotalLength)+' bytes <-----', Sender);
    end
  { We've used "ws-chunks" in Send() when sending files in multiple chunks }
  else if wf.wfName='ws-chunks' then
    begin
    if wf.wfDone then
      MemoAdd(3,'OUT! '+wf.wfHeadInfo,Sender);

    if not wf.wfFinished then
      begin
      if wf.wfTotalInOut=0 then
        MemoAdd(2,'---> OUT Start: '+IntToStr(wf.asLargeInt['total'])+' bytes --->',Sender);

      { For demonstration purposes, we will NOT be checking the size of the file
        being sent. Instead, we will try to read the next 16KB bytes from the file
        in every "OnWSDataSent" event, until we get an empty string back as a result. }
      data:=Read_File(wf.asText['fname'], wf.wfTotalInOut, 16*1024);

      if length(data)>0 then
        begin
        // We have some content, send it out ...
        wf.wfWrite(data);
        MemoAdd(3,'SENT '+
                IntToStr(wf.wfTotalInOut)+'/'+
                IntToStr(wf.wfTotalLength)+' bytes', Sender);
        end
      else
        begin
        { No content returned from Read_File, we need to set the "wfFinished"
          flag to TRUE, so the last Frame will be sent out with "waFinal=TRUE". }
        wf.wfFinished:=True;
        MemoAdd(2,'OUT Complete: '+
            IntToStr(wf.wfTotalInOut)+'/'+
            IntToStr(wf.wfTotalLength)+' bytes <-----', Sender);
        end;
      end;
    end
  else if wf.wfName='' then // all Frames without a name ...
    begin
    if wf.wfStarted then
      MemoAdd(2,'-=+> OUT: '+wf.wfHeadInfo, Sender);
    MemoAdd(3,'SENT '+IntToStr(wf.wfPayloadInOut)+'/'+IntToStr(wf.waPayloadLength), Sender);
    if wf.wfDone then
      MemoAdd(2,'OUT! '+IntToStr(wf.waPayloadLength)+' bytes <+=---', Sender);
    end;
  end;

procedure TForm1.SockReqWSDisconnect(Sender: TRtcConnection);
  begin
  MemoAdd(1,'DISCONNECTED!', Sender);
  end;

procedure TForm1.SockReqWSDataIn(Sender: TRtcConnection);
  begin
  MemoAdd(4,'<<<< in '+IntToStr(Sender.DataIn)+' <<--',Sender);
  end;

procedure TForm1.SockReqWSDataOut(Sender: TRtcConnection);
  begin
  MemoAdd(4,'-->> out '+IntToStr(Sender.DataOut)+' >>>>',Sender);
  end;

end.
