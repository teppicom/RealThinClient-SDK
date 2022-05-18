unit loLocalTest;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,

  rtcInfo, rtcLink, loFileCli, loFileSrv, StdCtrls;

type
  TForm1 = class(TForm)
    btnNew: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Manager:TRtcObjectManager;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  // Create our Local Object Manager
  Manager:=TRtcLocalObjectManager.Create(True);
  // Activate the Object Manager
  SetRtcObjectManager(Manager);
  end;

procedure TForm1.FormDestroy(Sender: TObject);
  begin
  // Deactivate the Object Manager
  SetRtcObjectManager(nil);
  // Destroy the Object Manager
  Manager.Free;
  end;

procedure TForm1.btnNewClick(Sender: TObject);
  begin
  TloFileClient.Create(nil).Show;
  end;

procedure TForm1.Button1Click(Sender: TObject);
  begin
  TloFileServer.Create(nil);
  end;

end.
