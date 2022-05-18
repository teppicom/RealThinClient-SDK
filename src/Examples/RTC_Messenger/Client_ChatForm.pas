unit Client_ChatForm;

interface

{$include rtcDefs.inc}

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,

  tools,

  rtcInfo, rtcConn, rtcFunction, ComCtrls;

type
  TChatForm = class(TForm)
    mainPanel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    btnSend: TBitBtn;
    eEnter: TMemo;
    btnBuzz: TSpeedButton;
    rtfChat: TRichEdit;
    //............................
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSendClick(Sender: TObject);
    procedure eEnterChange(Sender: TObject);
    procedure eEnterKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure eEnterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnBuzzClick(Sender: TObject);

  private
    { Private declarations }
    users:string;
    FLastUser:String;

    class procedure closeForm(obj:TChatForm);

  public
    { Public declarations }

    class function getForm(fname:string; beep:boolean=False):TChatForm;
    class function isFormOpen(fname:string):boolean;

    class procedure enableAllForms;
    class procedure disableAllForms;
    class procedure closeAllForms;

    procedure AddMessage(const uname,text:string; message_typ :integer);
  end;

type
  TSendMsg=procedure(to_user,msg:string) of object;

var
  SendMsg:TSendMsg=nil;

const
  //rtc_message_typ ......................
  RTC_ADDMSG_SELF = 0;
  RTC_ADDMSG_FRIEND = 1;
  RTC_ADDMSG_ERROR = 2;
  RTC_ADDMSG_LOGIN = 3;
  RTC_ADDMSG_LOGOUT = 4;

implementation

{$R *.dfm}

const
  //chat-colors, you can alter wiht dialogbox, if you want ...
  CChatSays     = clBlack;
  CChatIam      = clNavy;
  CChatSystem   = clRed;
  CChatLogin    = clGreen;
  CChatLogout   = clMaroon;

var
  List:TStringList;

class function TChatForm.isFormOpen(fname:string):boolean;
  begin
  Result:=List.IndexOf(UpperCase(fname))>=0;
  end;

class function TChatForm.getForm(fname:string; beep:boolean=False):TChatForm;
  var
    i:integer;
  begin
  i:=List.IndexOf(UpperCase(fname));
  if i>=0 then
    begin
    Result:=TChatForm(List.Objects[i]);
    if beep then
      if not Result.Active then
        eingang;
    end
  else
    begin
    if beep then eingang;

    Result:=TChatForm.Create(nil);
    Result.Caption:=fname+' - Message';
    Result.users:=fname;
    Result.Show;
    List.AddObject(UpperCase(fname),Result);
    end;
  end;

class procedure TChatForm.closeForm(obj:TChatForm);
  var
    i:integer;
  begin
  for i:=0 to List.Count-1 do
    if List.Objects[i]=obj then
      begin
      List.Delete(i);
      Break;
      end;
  end;

class procedure TChatForm.enableAllForms;
  var
    i:integer;
  begin
  for i:=0 to List.Count-1 do
    with TChatForm(List.Objects[i]) do
      begin
      eEnter.ReadOnly:=False;
      eEnter.Color:=clWindow;
      btnSend.Enabled:=eEnter.Lines.Text<>'';
      end;
  end;

class procedure TChatForm.disableAllForms;
  var
    i:integer;
  begin
  for i:=0 to List.Count-1 do
    with TChatForm(List.Objects[i]) do
      begin
      eEnter.ReadOnly:=True;
      eEnter.Color:=clBtnFace;
      btnSend.Enabled:=False;
      end;
  end;

class procedure TChatForm.closeAllForms;
  var
    i:integer;
  begin
  for i:=0 to List.Count-1 do
    List.Objects[i].Free;
  List.Clear;
  end;

procedure TChatForm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  Action:=caFree;
  closeForm(self);
  end;

procedure TChatForm.btnSendClick(Sender: TObject);
  var
    s:string;
  begin
  s:=eEnter.Lines.Text;
  if s<>'' then
    begin
    if not assigned(SendMsg) then
      raise Exception.Create('SendMsg procedure not assigned!');
    SendMsg(users, s);
    eEnter.Lines.Clear;
    btnSend.Enabled:=False;
    eEnter.SetFocus;
    end;
  end;

procedure TChatForm.eEnterChange(Sender: TObject);
  begin
  btnSend.Enabled:=eEnter.Lines.Text<>'';
  end;

procedure StartUp;
  begin
  List:=TStringList.Create;
  end;

procedure ShutDown;
  begin
  TChatForm.CloseAllForms;
  List.Free;
  end;

procedure TChatForm.eEnterKeyPress(Sender: TObject; var Key: Char);
begin
  // 'Eat' the return key
  if Key = #13 then
    Key := #0;
  end;

procedure TChatForm.AddMessage(const uname, text: string; message_typ :integer);
  begin
  if text = '' then Exit;

  rtfChat.Lines.BeginUpdate;
  rtfChat.Paragraph.FirstIndent := 0;
  case message_typ of
    RTC_ADDMSG_SELF:
      begin
      if FLastUser<>uname then
        begin
        FLastUser:=uname;
        rtfChat.SelAttributes.Color:=CChatIAm;
        rtfChat.SelAttributes.Style:=[];
        rtfChat.Lines.Add(uname + ':');
        end;
      rtfChat.SelAttributes.Color:=CChatIAm;
      rtfChat.SelAttributes.Style:=[];
      rtfChat.Lines.Add('   '+text);
      end;

    RTC_ADDMSG_FRIEND:
      begin
      if FLastUser<>uname then
        begin
        FLastUser:=uname;
        rtfChat.SelAttributes.Color:=CChatSays;
        rtfChat.SelAttributes.Style:=[];
        rtfChat.Lines.Add(uname + ':');
        end;
      rtfChat.SelAttributes.Color:=CChatSays;
      rtfChat.SelAttributes.Style:=[fsBold];
      rtfChat.Lines.Add('   '+text);
      end;

    RTC_ADDMSG_ERROR:
      begin
      FLastUser:='';
      rtfChat.SelAttributes.Color:=CChatSystem;
      rtfChat.SelAttributes.Style:=[fsBold];
      rtfChat.Lines.Add(uname + ' '+ text);
      click;
      end;

    RTC_ADDMSG_LOGIN:
      begin
      FLastUser:='';
      rtfChat.SelAttributes.Color:=CChatLogin;
      rtfChat.SelAttributes.Style:=[];
      rtfChat.Lines.Add(uname +' '+ text);
      eingang;
      end;

    RTC_ADDMSG_LOGOUT:
      begin
      FLastUser:='';
      rtfChat.SelAttributes.Color:=CChatLogout;
      rtfChat.SelAttributes.Style:=[];
      rtfChat.Lines.Add(uname +' '+ text);
      door;
      end;
    end;
  rtfChat.Lines.EndUpdate;

  if pos(':BUZZ)', text) > 0 then
    doorbell;
  end;


//new for support smilies ......................................................
procedure TChatForm.FormCreate(Sender: TObject);
begin
  FLastuser:='';

  // This is absolutely needed! Without this, the RichEdit won't scroll
  // to the end automatically! Don't ask me why though...
  rtfChat.HideSelection   := False;
  rtfChat.HideScrollBars  := True;
end;
//..............................................................................


procedure TChatForm.eEnterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if Shift  = [ssShift] then begin
      // Insert line break when Shift-return is pressed
      eEnter.SelText := #13#10;
    end else begin
      // Click 'Send' button when return is pressed
      if btnSend.Enabled then
        btnSend.Click();
    end;
end;

procedure TChatForm.btnBuzzClick(Sender: TObject);
  var
    s:string;
  begin
  s:= ':BUZZ)';
  if s<>'' then
    begin
    if not assigned(SendMsg) then
      raise Exception.Create('SendMsg procedure not assigned!');
    SendMsg(users, s);
    eEnter.SetFocus;
    end;
  end;

initialization
StartUp;
finalization
ShutDown;
end.
