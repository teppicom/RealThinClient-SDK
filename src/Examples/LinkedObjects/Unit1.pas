unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,

  rtcInfo, rtcLink;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

end.
