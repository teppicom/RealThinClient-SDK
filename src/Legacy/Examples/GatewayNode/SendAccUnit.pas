unit SendAccUnit;

interface

uses
  Windows, Messages, SysUtils,
  Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls,

  rtcTypes;

type
  TRTCPSendAccount = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnSend: TBitBtn;
    btnCancel: TBitBtn;
    lAccount: TLabel;
    eAccount: TLabel;
    eGateway: TLabel;
    eCode: TEdit;
    eUserID: TEdit;
    procedure btnSendClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SendAccountQuery(const FormCap,BtnCap,AccName:RtcWideString; const GateAddr:RtcString; var AuthCode,UserID:RtcString):boolean;

implementation

{$R *.dfm}

function OnlyDigits(const a:RtcString):RtcString;
  var
    i:integer;
  begin
  Result:='';
  for i:=1 to length(a) do
    case a[i] of
      '0'..'9':Result:=Result+a[i];
      end;
  end;

function SendAccountQuery(const FormCap,BtnCap,AccName:RtcWideString; const GateAddr:RtcString; var AuthCode,UserID:RtcString):boolean;
  var
    frm:TRTCPSendAccount;
  begin
  frm:=TRTCPSendAccount.Create(nil);
  with frm do
    begin
    Caption:=FormCap;
    btnSend.Caption:=BtnCap;
    eAccount.Caption:=AccName;
    eGateway.Caption:=GateAddr;
    eCode.Text:=AuthCode;
    eUserID.Text:=UserID;
    btnSend.Left:=ClientWidth-btnSend.Width;
    Result:=ShowModal=mrOk;
    if Result then
      begin
      AuthCode:=OnlyDigits(eCode.Text);
      UserID:=OnlyDigits(eUserID.Text);
      if (length(UserID)<>6) or (length(AuthCode)=0) then
        Result:=False;
      end;
    end;
  RtcFreeAndNil(frm);
  end;

procedure TRTCPSendAccount.btnSendClick(Sender: TObject);
  begin
  ModalResult:=mrOk;
  end;

procedure TRTCPSendAccount.btnCancelClick(Sender: TObject);
  begin
  ModalResult:=mrCancel;
  end;

end.
