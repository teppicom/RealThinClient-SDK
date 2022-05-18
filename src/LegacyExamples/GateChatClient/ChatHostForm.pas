unit ChatHostForm;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcSystem, rtcInfo, rtcConn,
  rtcGateConst, rtcGateCli, rtcZLib,
  rtcTypes, rtcDataCli,

  ChatCIDs, ComCtrls, Buttons;

type
  TChatHostFrm = class(TForm)
    Link: TRtcGateClientLink;
    Panel2: TPanel;
    eUsers: TListBox;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    btnAddUser: TButton;
    Panel1: TPanel;
    Splitter2: TSplitter;
    Panel4: TPanel;
    eMessage: TMemo;
    Panel5: TPanel;
    panChat: TPanel;
    eChat: TMemo;
    panDraw: TPanel;
    spChat: TSplitter;
    pbDrawing: TPaintBox;
    Panel9: TPanel;
    pbPenColor: TColorBox;
    pbPenWidth: TTrackBar;
    Panel10: TPanel;
    btnClearDrawing: TBitBtn;
    GCThread: TRtcGCThread;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure eUsersDblClick(Sender: TObject);
    procedure eMessageKeyPress(Sender: TObject; var Key: Char);
    procedure pbDrawingMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbDrawingMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbDrawingMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure pbDrawingPaint(Sender: TObject);
    procedure btnClearDrawingClick(Sender: TObject);
    procedure LinkAfterClientRemoved;
    procedure LinkAfterLogOutGUI(Client: TRtcHttpGateClient;State: TRtcGateClientStateInfo);
    procedure LinkDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure LinkDataReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);
    procedure LinkDataReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
    procedure LinkInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
    procedure LinkInfoReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
    procedure LinkReadyAfterResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure LinkStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
    procedure GCThreadDataReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI: Boolean);
    procedure GCThreadDataReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    MyGroupID:TGateUID;
    InviteKey:RtcByteArray;
    LMDown:boolean;
    LMX1,LMY1:integer;
    FCS:TRtcCritSec;

    FDrawing:TRtcDataSet;
    FLastPainted:integer;

    procedure PaintLine;
    procedure PaintNewLines;
    procedure PaintDrawing;

    procedure UserIsActive(UserID,GroupID:TGateUID);
    procedure UserIsPassive(UserID,GroupID:TGateUID);
    procedure UserIsRemoved(UserID,GroupID:TGateUID);

    function ConnectedUsers:integer;
    function ConnectedUserList:String;

    procedure AddAllDisabledUsers;

    procedure InviteUserToChat(UserID, GroupID:TGateUID; Forced:boolean=True);
  end;

function NewChatHostForm(Cli:TRtcHttpGateClient):TChatHostFrm;

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

function Now2Str:RtcString;
  begin
  Result:=' @'+Time2Str(Now);
  end;

function MakeRandomKey(GID:TGateUID; len:integer):RtcByteArray;
  var
    a:integer;
    b:byte;
  begin
  SetLength(Result,len+1);
  Result[0]:=GID;
  for a:=1 to length(Result)-1 do
    begin
    b:=random(255);
    Result[a]:=b;
    end;
  end;

function CompareKeys(const OrigKey,RecvKey:RtcByteArray):boolean;
  var
    a:integer;
  begin
  if length(OrigKey)>length(RecvKey) then
    Result:=False
  else
    begin
    Result:=True;
    for a:=0 to length(OrigKey)-1 do
      if OrigKey[a]<>RecvKey[a] then
        begin
        Result:=False;
        Break;
        end;
    end;
  end;

function NewChatHostForm(Cli:TRtcHttpGateClient):TChatHostFrm;
  begin
  if not Cli.Ready then
    raise ERtcGateClient.Create('No connection');
  Result:=TChatHostFrm.Create(Application);
  Result.Link.Client:=Cli;
  Result.MyGroupID:=Result.Link.MyGroupID;
  Result.InviteKey:=MakeRandomKey(Result.MyGroupID,16);
  Result.Caption:=IntToStr(Cli.MyUID)+': Chat Room #'+IntToStr(Result.MyGroupID);
  Result.Show;
  end;

procedure TChatHostFrm.btnAddUserClick(Sender: TObject);
  var
    UID:String;
  begin
  if not Link.Ready then Exit;
  UID:='';
  if InputQuery('Invite User to Chat','Enter remote User ID',UID) then
    InviteUserToChat(Str2IntDef(Trim(UID),0),0);
  eMessage.SetFocus;
  end;

procedure TChatHostFrm.InviteUserToChat(UserID, GroupID: TGateUID; Forced:boolean=True);
  var
    Frm:TChatHostFrm;
  begin
  if (UserID<MinUserID) or
     (UserID>MaxUserID) or
     (UserID=Link.MyUID) then
    begin
    if Forced then
      ShowMessage('Invalid User ID.');
    end
  else
    begin
    if GroupID=0 then
      GroupID:=Link.Groups.GetMinID(UserID);
    if GroupID>0 then
      begin
      if Link.Groups.GetStatus(UserID,GroupID)=10 then
        begin
        if Forced then
          begin
          Frm:=NewChatHostForm(Link.Client);
          Frm.InviteUserToChat(UserID,0);
          end;
        end
      else
        begin
        Link.AddFriend(UserID);
        Link.SendBytes(UserID,MyGroupID,cid_ChatReturn,InviteKey);
        end;
      end
    else
      begin
      if Forced then
        UserIsPassive(UserID,0);
      Link.Groups.SetStatus(UserID,0,1);
      Link.SendBytes(UserID,MyGroupID,cid_ChatInvite,InviteKey);
      Link.PingUser(UserID);
      end;
    end;
  end;

procedure TChatHostFrm.eMessageKeyPress(Sender: TObject; var Key: Char);
  var
    msg:RtcByteArray;
    con:integer;
  begin
  msg:=nil;
  if Key=#13 then
    begin
    Key:=#0;
    if Link.Ready then
      begin
      con:=ConnectedUsers;
      if con=0 then
        begin
        eChat.Lines.Add('* Nobody is listening in this Chat Room.'+#13#10+
                        '* Click "INVITE" to invite a new User.');
        end
      else if eMessage.Text<>'' then
        begin
        msg:=ZCompress_Ex(Utf8EncodeEx(eMessage.Text),zcFastest);
        Link.SendToGroup(cid_ChatMessage,msg);
        eChat.Lines.Add('[ME] to {'+ConnectedUserList+'}'+#13#10+
                        eMessage.Text+#13#10+'----->'+Now2Str);
        eMessage.Text:='';
        end;
      end;
    end;
  end;

procedure TChatHostFrm.eUsersDblClick(Sender: TObject);
  var
    UID:String;
    i:integer;
    UserID,GroupID:TGateUID;
  begin
  if (eUsers.Items.Count>0) and (eUsers.ItemIndex>=0) and Link.Ready then
    begin
    UID:=Trim(eUsers.Items.Strings[eUsers.ItemIndex]);
    if Copy(UID,1,2)='* ' then Delete(UID,1,2);
    i:=Pos('/',UID);
    if i<=0 then 
      begin
      UserID:=StrToInt(UID);
      GroupID:=0;
      end
    else
      begin
      UserID:=StrToInt(Copy(UID,1,i-1));
      GroupID:=StrToInt(Copy(UID,i+1,length(UID)-i));
      end;

    if Link.Groups.GetStatus(UserID,GroupID)<10 then
      InviteUserToChat(UserID,GroupID)
    else
      Link.RemoveUserFromGroup(UserID);
    end;
    
  eMessage.SetFocus;
  end;

procedure TChatHostFrm.UserIsActive(UserID,GroupID: TGateUID);
  var
    UID:RtcString;
    i:integer;
  begin
  if GroupID>0 then
    begin
    UID:=IntToStr(UserID);
    i:=eUsers.Items.IndexOf('* '+UID);
    if i>=0 then eUsers.Items.Delete(i);
    UID:=IntToStr(UserID)+'/'+IntToStr(GroupID);
    end
  else
    UID:=IntToStr(UserID);
  i:=eUsers.Items.IndexOf('* '+UID);
  if i>=0 then eUsers.Items.Delete(i);
  i:=eUsers.Items.IndexOf(UID);
  if i<0 then eUsers.Items.Add(UID);
  end;

procedure TChatHostFrm.UserIsPassive(UserID,GroupID: TGateUID);
  var
    UID:RtcString;
    i:integer;
  begin
  if GroupID>0 then
    begin
    UID:=IntToStr(UserID);
    i:=eUsers.Items.IndexOf('* '+UID);
    if i>=0 then eUsers.Items.Delete(i);
    UID:=IntToStr(UserID)+'/'+IntToStr(GroupID);
    end
  else
    UID:=IntToStr(UserID);
  i:=eUsers.Items.IndexOf(UID);
  if i>=0 then eUsers.Items.Delete(i);
  i:=eUsers.Items.IndexOf('* '+UID);
  if i<0 then eUsers.Items.Add('* '+UID);
  end;

procedure TChatHostFrm.UserIsRemoved(UserID,GroupID: TGateUID);
  var
    UID:RtcString;
    i:integer;
  begin
  if GroupID>0 then
    begin
    UID:=IntToStr(UserID);
    i:=eUsers.Items.IndexOf('* '+UID);
    if i>=0 then eUsers.Items.Delete(i);
    UID:=IntToStr(UserID)+'/'+IntToStr(GroupID);
    end
  else
    UID:=IntToStr(UserID);
  i:=eUsers.Items.IndexOf('* '+UID);
  if i>=0 then eUsers.Items.Delete(i);
  i:=eUsers.Items.IndexOf(UID);
  if i>=0 then eUsers.Items.Delete(i);
  end;

procedure TChatHostFrm.AddAllDisabledUsers;
  var
    UID,GID,GST:TGateUID;
  begin
  if MyGroupID>0 then
    begin
    UID:=0; GID:=0;
    repeat
      GST:=Link.Groups.GetNextStatus(UID,GID);
      if (GST>0) and (GST<10) then
        begin
        Link.AddFriend(UID);
        Link.AddUserToGroup(UID);
        end;
      until GST=0;
    end;
  end;

function TChatHostFrm.ConnectedUsers: integer;
  var
    UID,GID,GST:TGateUID;
  begin
  Result:=0;
  UID:=0; GID:=0;
  repeat
    GST:=Link.Groups.GetNextStatus(UID,GID);
    if GST=10 then
      Inc(Result);
    until GST=0;
  end;

function TChatHostFrm.ConnectedUserList: String;
  var
    UID,GID,GST:TGateUID;
  begin
  Result:='';
  UID:=0; GID:=0;
  repeat
    GST:=Link.Groups.GetNextStatus(UID,GID);
    if GST=10 then
      Result:=Result+','+IntToStr(UID)+'/'+IntToStr(GID);
    until GST=0;
  if length(Result)>0 then
    Delete(Result,1,1);
  end;

procedure TChatHostFrm.pbDrawingMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    CR,CG,CB,CW:byte;
    OutBytes:TRtcHugeByteArray;
  begin
  if (Button=mbLeft) and Link.Ready and (ConnectedUsers>0) then
    begin
    if X<0 then X:=0;
    if Y<0 then Y:=0;

    LMDown:=True;
    LMX1:=X;
    LMY1:=Y;
    CR:=ColorToRGB(pbPenColor.Selected) and $FF;
    CG:=ColorToRGB(pbPenColor.Selected) shr 8 and $FF;
    CB:=ColorToRGB(pbPenColor.Selected) shr 16 and $FF;
    CW:=pbPenWidth.Position;

    OutBytes:=TRtcHugeByteArray.Create;
    try
      OutBytes.AddEx(OneByte2Bytes(CR));
      OutBytes.AddEx(OneByte2Bytes(CG));
      OutBytes.AddEx(OneByte2Bytes(CB));
      OutBytes.AddEx(OneByte2Bytes(CW));
      OutBytes.AddEx(Word2Bytes(LMX1));
      OutBytes.AddEx(Word2Bytes(LMY1));
      OutBytes.AddEx(Word2Bytes(LMX1));
      OutBytes.AddEx(Word2Bytes(LMY1));
      Link.SendToGroup(cid_ChatPaintLine,OutBytes.GetEx);
    finally
      OutBytes.Free;
      end;

    FCS.Acquire;
    try
      FDrawing.Append;
      FDrawing.asInteger['R']:=CR;
      FDrawing.asInteger['G']:=CG;
      FDrawing.asInteger['B']:=CB;
      FDrawing.asInteger['W']:=CW;
      FDrawing.asInteger['X1']:=LMX1;
      FDrawing.asInteger['Y1']:=LMY1;
      FDrawing.asInteger['X2']:=LMX1;
      FDrawing.asInteger['Y2']:=LMY1;
    finally
      FCS.Release;
      end;

    PaintNewLines;
    end;
  end;

procedure TChatHostFrm.pbDrawingMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    CR,CG,CB,CW:byte;
    OutBytes:TRtcHugeByteArray;
  begin
  if LMDown and Link.Ready and (ConnectedUsers>0) then
    begin
    if X<0 then X:=0;
    if Y<0 then Y:=0;

    CR:=ColorToRGB(pbPenColor.Selected) and $FF;
    CG:=ColorToRGB(pbPenColor.Selected) shr 8 and $FF;
    CB:=ColorToRGB(pbPenColor.Selected) shr 16 and $FF;
    CW:=pbPenWidth.Position;

    OutBytes:=TRtcHugeByteArray.Create;
    try
      OutBytes.AddEx(OneByte2Bytes(CR));
      OutBytes.AddEx(OneByte2Bytes(CG));
      OutBytes.AddEx(OneByte2Bytes(CB));
      OutBytes.AddEx(OneByte2Bytes(CW));
      OutBytes.AddEx(Word2Bytes(LMX1));
      OutBytes.AddEx(Word2Bytes(LMY1));
      OutBytes.AddEx(Word2Bytes(X));
      OutBytes.AddEx(Word2Bytes(Y));
      Link.SendToGroup(cid_ChatPaintLine,OutBytes.GetEx);
    finally
      OutBytes.Free;
      end;

    FCS.Acquire;
    try
      FDrawing.Append;
      FDrawing.asInteger['R']:=CR;
      FDrawing.asInteger['G']:=CG;
      FDrawing.asInteger['B']:=CB;
      FDrawing.asInteger['W']:=CW;
      FDrawing.asInteger['X1']:=LMX1;
      FDrawing.asInteger['Y1']:=LMY1;
      FDrawing.asInteger['X2']:=X;
      FDrawing.asInteger['Y2']:=Y;
    finally
      FCS.Release;
      end;

    LMX1:=X; LMY1:=Y;

    PaintNewLines;
    end;
  end;

procedure TChatHostFrm.pbDrawingMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if Button=mbLeft then
    LMDown:=False;
  end;

procedure TChatHostFrm.PaintLine;
  begin
  with pbDrawing.Canvas do
    begin
    Pen.Color:=RGB(FDrawing.asInteger['R'],FDrawing.asInteger['G'],FDrawing.asInteger['B']);
    Pen.Width:=FDrawing.asInteger['W'];
    MoveTo(FDrawing.asInteger['X1'],FDrawing.asInteger['Y1']);
    LineTo(FDrawing.asInteger['X2'],FDrawing.asInteger['Y2']);
    end;
  end;

procedure TChatHostFrm.PaintDrawing;
  begin
  with pbDrawing.Canvas do
    begin
    Brush.Color:=clWhite;
    Brush.Style:=bsSolid;
    FillRect(Rect(0,0,pbDrawing.Width,pbDrawing.Height));
    end;
  FCS.Acquire;
  try
    FDrawing.First;
    while not FDrawing.Eof do
      begin
      PaintLine;
      FDrawing.Next;
      end;
    FLastPainted:=FDrawing.Row;
  finally
    FCS.Release;
    end;
  end;

procedure TChatHostFrm.PaintNewLines;
  begin
  if FLastPainted=0 then
    PaintDrawing
  else
    begin
    FCS.Acquire;
    try
      if FLastPainted<FDrawing.RowCount then
        begin
        FDrawing.Row:=FLastPainted;
        while not FDrawing.Eof do
          begin
          PaintLine;
          FDrawing.Next;
          end;
        FLastPainted:=FDrawing.Row;
        end;
    finally
      FCS.Release;
      end;
    end;
  end;

procedure TChatHostFrm.FormCreate(Sender: TObject);
  begin
  FCS:=TRtcCritSec.Create;
  FDrawing:=TRtcDataSet.Create;
  end;

procedure TChatHostFrm.pbDrawingPaint(Sender: TObject);
  begin
  PaintDrawing;
  end;

procedure TChatHostFrm.btnClearDrawingClick(Sender: TObject);
  begin
  if MessageDlg('Clear the Whiteboard in this Room?'#13#10+
                'This operation can NOT be undone!',mtWarning,[mbYes,mbNo],0)=mrYes then
    begin
    FCS.Acquire;
    try
      FDrawing.Clear;
      FLastPainted:=0;
    finally
      FCS.Release;
      end;
    PaintNewLines;
    Link.SendToGroup(cid_ChatPaintClear);
    end;
  end;

procedure TChatHostFrm.LinkAfterLogOutGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  eChat.Lines.Add('* Logged OUT'+Now2Str);
  Link.Groups.ClearAllStates;
  eUsers.Clear;
  end;

procedure TChatHostFrm.LinkInfoFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  case Data.Command of
    gc_UserOffline: Wanted:=Link.Groups.GetMinStatus(Data.UserID)>0;
    gc_JoinedUser,
    gc_LeftUser:    Wanted:=Link.Groups.GetStatus(Data.UserID, Data.GroupID)=10;
    end;
  end;

procedure TChatHostFrm.LinkInfoReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  var
    gid:TGateUID;
  begin
  case Data.Command of
    gc_UserOffline:
      begin
      eChat.Lines.Add('* User '+IntToStr(Data.UserID)+' "'+Client.UserInfo[Data.UserID]+'" is OFF-LINE'+Now2Str);
      if GetActiveWindow<>Handle then MessageBeep(0);
      repeat
        GID:=Link.Groups.GetMinID(Data.UserID);
        UserIsRemoved(Data.UserID, GID);
        Link.Groups.ClearStatus(Data.UserID,GID);
        until GID=0;
      end;
    gc_JoinedUser:
      begin
      UserIsActive(Data.UserID, Data.GroupID);
      eChat.Lines.Add('* User '+IntToStr(Data.UserID)+'/'+IntToStr(Data.GroupID)+' "'+Client.UserInfo[Data.UserID]+'" JOINED'+Now2Str);
      if GetActiveWindow<>Handle then MessageBeep(0);
      end;
    gc_LeftUser:
      begin
      UserIsPassive(Data.UserID, Data.GroupID);
      eChat.Lines.Add('* User '+IntToStr(Data.UserID)+'/'+IntToStr(Data.GroupID)+' "'+Client.UserInfo[Data.UserID]+'" AWAY'+Now2Str);
      if GetActiveWindow<>Handle then MessageBeep(0);
      Link.Groups.SetStatus(Data.UserID, Data.GroupID, 5);
      Link.RemoveUserFromGroup(Data.UserID);
      end;
    end;
  end;

procedure TChatHostFrm.LinkReadyAfterResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  var
    UID,GID:TGateUID;
  begin
  eChat.Lines.Add('* Connected'+Now2Str);
  UID:=0; GID:=0;
  while Link.Groups.GetNextStatus(UID,GID)>0 do
    InviteUserToChat(UID,GID,False);
  end;

procedure TChatHostFrm.LinkStreamResetGUI(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  var
    UID,GID,GST:TGateUID;
  begin
  eChat.Lines.Add('* Connection Lost'+Now2Str);
  UID:=0; GID:=0;
  repeat
    GST:=Link.Groups.GetNextStatus(UID,GID);
    if GST=10 then
      begin
      Link.Groups.SetStatus(UID,GID,5);
      UserIsPassive(UID,GID);
      end;
    until GST=0;
  end;

procedure TChatHostFrm.LinkDataFilter(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var Wanted: Boolean);
  begin
  case Data.CallID of
    cid_ChatAccept,
    cid_ChatConfirm:
      if Data.Footer then
        Wanted:= Data.ToGroupID=MyGroupID
      else if Data.Header then
        Data.ToBuffer:= Data.ToGroupID=MyGroupID;

    cid_ChatJoined,
    cid_ChatLeft:
      if Data.Footer then
        Wanted:= Link.Groups.GetStatus(Data.UserID, Data.GroupID)>0
      else if Data.Header then
        Data.ToBuffer:= Link.Groups.GetStatus(Data.UserID, Data.GroupID)>0;

    cid_ChatReturn:
      if Data.Footer then
        Wanted:= Link.Groups.GetStatus(Data.UserID, Data.ToGroupID)=5
      else if Data.Header then
        Data.ToBuffer:= Link.Groups.GetStatus(Data.UserID, Data.ToGroupID)=5;

    cid_ChatMessage,
    cid_ChatPaintLine,
    cid_ChatPaintClear:
      if Data.Footer then
        Wanted:= Link.Groups.GetStatus(Data.UserID, Data.GroupID)=10
      else if Data.Header then
        Data.ToBuffer:= Link.Groups.GetStatus(Data.UserID, Data.GroupID)=10;
    end;
  end;

procedure TChatHostFrm.LinkDataReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI, WantBackThread: Boolean);
  procedure ProcessJoin;
    var
      UserKey:RtcByteArray;
      NewUserID,UserGroupID:TGateUID;
      OutBytes:TRtcHugeByteArray;
    begin
    UserKey:=nil;

    NewUserID:=Bytes2Int(Data.Content);
    UserKey:=Copy(Data.Content,4,length(Data.Content)-4);

    if length(UserKey)=0 then Exit;
    UserGroupID:=UserKey[0];

    // Do NOT add Self!
    if (NewUserID=Data.MyUID) then Exit; // and (UserGroupID=MyGroupID) then Exit;

    // Do NOT add the same link twice!
    if Link.Groups.GetStatus(NewUserID,UserGroupID)>0 then Exit;

    Link.AddFriend(NewUserID);

    OutBytes:=TRtcHugeByteArray.Create;
    try
      OutBytes.AddEx(UserKey);
      OutBytes.AddEx(InviteKey);
      Link.SendBytes(NewUserID,UserGroupID,cid_ChatAccept,OutBytes.GetEx);
    finally
      OutBytes.Free;
      end;
    end;
  begin
  case Data.CallID of
    cid_ChatJoined:     ProcessJoin;

    cid_ChatPaintClear: WantBackThread:=True;

    cid_ChatPaintLine:  WantBackThread:=length(Data.Content)=12;

    cid_ChatAccept,
    cid_ChatConfirm,
    cid_ChatLeft,
    cid_ChatReturn,
    cid_ChatMessage:    WantGUI:=True;
    end;
  end;

procedure TChatHostFrm.LinkDataReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  procedure ProcessAccept;
    var
      UserKey:RtcByteArray;
      UserGroupID:TGateUID;
      OutBytes:TRtcHugeByteArray;
    begin
    UserKey:=nil;

    // Invitation Key is correct?
    if not CompareKeys(InviteKey, Data.Content) then Exit;

    UserKey:=Copy(Data.Content,length(InviteKey),length(Data.Content)-length(InviteKey));

    if length(UserKey)=0 then Exit;
    UserGroupID:=UserKey[0];

    // Do NOT add Self!
    if (Data.UserID=Data.MyUID) then Exit; // and (UserGroupID=MyGroupID) then Exit;

    // Do NOT add the same link twice!
    if Link.Groups.GetStatus(Data.UserID, UserGroupID)=10 then Exit;

    if Link.Groups.GetStatus(Data.UserID, UserGroupID)=0 then
      begin
      UserIsPassive(Data.UserID, UserGroupID);
      Link.Groups.ClearStatus(Data.UserID, 0);
      end;
    Link.Groups.SetStatus(Data.UserID, UserGroupID, 10);

    Link.AddFriend(Data.UserID);
    if Data.CallID=cid_ChatAccept then
      begin
      OutBytes:=TRtcHugeByteArray.Create;
      try
        OutBytes.AddEx(UserKey);
        OutBytes.AddEx(InviteKey);
        Link.SendBytes(Data.UserID, UserGroupID, cid_ChatConfirm, OutBytes.GetEx);

        OutBytes.Clear;
        OutBytes.AddEx(Int2Bytes(Data.UserID));
        OutBytes.AddEx(UserKey);
        Link.SendToGroup(cid_ChatJoined, OutBytes.GetEx);
      finally
        OutBytes.Free;
        end;
      end;
    Link.AddUserToGroup(Data.UserID);

    // User has already invited us to join his Group?
    if Link.Client.Groups.GetStatus(Data.UserID, UserGroupID)>0 then
      begin
      UserIsActive(Data.UserID, UserGroupID);
      eChat.Lines.Add('* USER '+IntToStr(Data.UserID)+'/'+IntToStr(UserGroupID)+' "'+Client.UserInfo[Data.UserID]+'" JOINED'+Now2Str);
      if GetActiveWindow<>Handle then MessageBeep(0);
      end;
    end;
  procedure ProcessLeft;
    begin
    UserIsRemoved(Data.UserID, Data.GroupID);

    eChat.Lines.Add('* USER '+IntToStr(Data.UserID)+'/'+IntToStr(Data.GroupID)+' "'+Client.UserInfo[Data.UserID]+'" LEFT'+Now2Str);

    // Beep if Chat window isn't currently active
    if GetActiveWindow<>Handle then MessageBeep(0);

    Link.Groups.ClearStatus(Data.UserID, Data.GroupID);

    // We have no more active Groups with this User?
    if Link.Groups.GetMaxStatus(Data.UserID)<10 then
      // Remove the User from our receiver Group
      Link.RemoveUserFromGroup(Data.UserID);
    end;
  procedure ProcessReturn;
    var
      UserKey:RtcByteArray;
      OutBytes:TRtcHugeByteArray;
    begin
    UserKey:=nil;
    UserKey:=Data.Content;

    Link.AddFriend(Data.UserID);
    OutBytes:=TRtcHugeByteArray.Create;
    try
      OutBytes.AddEx(UserKey);
      OutBytes.AddEx(InviteKey);
      Link.SendBytes(Data.UserID, Data.ToGroupID, cid_ChatConfirm, OutBytes.GetEx);
      OutBytes.Clear;

      Link.Groups.SetStatus(Data.UserID, Data.ToGroupID, 10);

      OutBytes.AddEx(Int2Bytes(Data.UserID));
      OutBytes.AddEx(UserKey);
      Link.SendToGroup(cid_ChatJoined, OutBytes.GetEx);
    finally
      OutBytes.Free;
      end;
    Link.AddUserToGroup(Data.UserID);

    // User has already invited us to join his Group?
    if Link.Client.Groups.GetStatus(Data.UserID, Data.ToGroupID)>0 then
      begin
      UserIsActive(Data.UserID, Data.ToGroupID);
      eChat.Lines.Add('* USER '+IntToStr(Data.UserID)+'/'+IntToStr(Data.ToGroupID)+' "'+Client.UserInfo[Data.UserID]+'" JOINED'+Now2Str);
      if GetActiveWindow<>Handle then MessageBeep(0);
      end;
    end;
  procedure ProcessMessage;
    var
      msg:String;
    begin
    msg:=Utf8DecodeEx(ZDecompress_Ex(Data.Content));
    eChat.Lines.Add('['+IntToStr(Data.UserID)+'/'+IntToStr(Data.GroupID)+'] "'+Client.UserInfo[Data.UserID]+'":'+#13#10+
                        msg+#13#10+'<-----'+Now2Str);
    if GetActiveWindow<>Handle then MessageBeep(0);
    end;
  begin
  case Data.CallID of
    cid_ChatAccept,
    cid_ChatConfirm:    ProcessAccept;
    cid_ChatLeft:       ProcessLeft;
    cid_ChatReturn:     ProcessReturn;
    cid_ChatMessage:    ProcessMessage;
    end;
  end;

procedure TChatHostFrm.GCThreadDataReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData; var WantGUI: Boolean);
  procedure ProcessPaintLine;
    var
      CR,CG,CB,CW:byte;
      MX1,MX2,MY1,MY2:longint;
    begin
    CR:=Bytes2OneByte(Data.Content,0);
    CG:=Bytes2OneByte(Data.Content,1);
    CB:=Bytes2OneByte(Data.Content,2);
    CW:=Bytes2OneByte(Data.Content,3);
    MX1:=Bytes2Word(Data.Content,4);
    MY1:=Bytes2Word(Data.Content,6);
    MX2:=Bytes2Word(Data.Content,8);
    MY2:=Bytes2Word(Data.Content,10);

    FCS.Acquire;
    try
      FDrawing.Append;
      FDrawing.asInteger['R']:=CR;
      FDrawing.asInteger['G']:=CG;
      FDrawing.asInteger['B']:=CB;
      FDrawing.asInteger['W']:=CW;
      FDrawing.asInteger['X1']:=MX1;
      FDrawing.asInteger['Y1']:=MY1;
      FDrawing.asInteger['X2']:=MX2;
      FDrawing.asInteger['Y2']:=MY2;
    finally
      FCS.Release;
      end;
    WantGUI:=True;
    end;
  procedure ProcessPaintClear;
    begin
    FCS.Acquire;
    try
      FDrawing.Clear;
      FLastPainted:=0;
    finally
      FCS.Release;
      end;
    WantGUI:=True;
    end;
  begin
  case Data.CallID of
    cid_ChatPaintLine: ProcessPaintLine;
    cid_ChatPaintClear: ProcessPaintClear;
    end;
  end;

procedure TChatHostFrm.GCThreadDataReceivedGUI(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  procedure ProcessPaintLine;
    begin
    PaintNewLines;
    end;
  procedure ProcessPaintClear;
    begin
    eChat.Lines.Add('['+IntToStr(Data.UserID)+'/'+IntToStr(Data.GroupID)+'] "'+Client.UserInfo[Data.UserID]+'":'+#13#10+
                    '> Whiteboard (Drawing) CLEARED <'+#13#10+'<-----'+Now2Str);
    PaintNewLines;
    end;
  begin
  case Data.CallID of
    cid_ChatPaintLine: ProcessPaintLine;
    cid_ChatPaintClear: ProcessPaintClear;
    end;
  end;

procedure TChatHostFrm.LinkAfterClientRemoved;
  begin
  // This Form requires a Client component to work.
  // To avoid problems, we will close the Form if the Client property is removed.
  Close;
  end;

procedure TChatHostFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  // Before closing this Form, we should notify
  // all Users in our Group that we are closing
  if assigned(Link.Client) then
    begin
    if Link.Ready then
      begin
      AddAllDisabledUsers;
      Link.SendToGroup(cid_ChatLeft);
      end;
    Link.Client:=nil;
    end;
  // Close the Form ONLY if the Client property has been removed.
  CanClose:=Link.Client=NIL;
  end;

procedure TChatHostFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  Action:=caFree;
  end;

procedure TChatHostFrm.FormDestroy(Sender: TObject);
  begin
  RtcFreeAndNil(FDrawing);
  RtcFreeAndNil(FCS);
  end;

end.
