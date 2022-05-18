unit MakeAccUnit;

interface

uses
  Windows, Messages, SysUtils,
  Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls,

  rtcTypes, Mask;

type
  TRTCPMakeAccount = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    btnMake: TBitBtn;
    btnCancel: TBitBtn;
    eRemoteName: TEdit;
    eLocalName: TEdit;
    btnKey: TSpeedButton;
    btnLink: TSpeedButton;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function MakeAccountQuery(const FormCap,BtnCap:RtcWideString; var LocalName,RemoteName:RtcWideString):boolean;

implementation

{$R *.dfm}

function MakeAccountQuery(const FormCap,BtnCap:RtcWideString; var LocalName,RemoteName:RtcWideString):boolean;
  var
    frm:TRTCPMakeAccount;
  begin
  frm:=TRTCPMakeAccount.Create(nil);
  with frm do
    begin
    Caption:=FormCap;
    btnMake.Caption:=BtnCap;
    eLocalName.Text:=LocalName;
    eRemoteName.Text:=RemoteName;
    Result:=ShowModal=mrOk;
    if Result then
      begin
      LocalName:=Trim(eLocalName.Text);
      RemoteName:=Trim(eRemoteName.Text);
      if (length(LocalName)=0) or (length(RemoteName)=0) then
        Result:=False;
      end;
    end;
  RtcFreeAndNil(frm);
  end;

procedure TRTCPMakeAccount.FormResize(Sender: TObject);
  begin
  btnMake.Left:=ClientWidth-btnMake.Width;
  end;

end.
