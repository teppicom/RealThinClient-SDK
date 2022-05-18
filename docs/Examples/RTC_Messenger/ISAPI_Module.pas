unit ISAPI_Module;

interface

{$include rtcDefs.inc}

uses
  SysUtils, Classes,

  {$IFDEF VER120}
  Forms, // D4
  {$ENDIF}
  {$IFDEF VER130}
  Forms, // D5
  {$ENDIF}

  rtcInfo, rtcSystem,
  rtcConn, rtcDataSrv, rtcISAPISrv,

  rtcMessengerProvider;

type
  TISAPIModule = class(TDataModule)
    Server: TRtcISAPIServer;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ISAPIModule: TISAPIModule;

implementation

{$R *.dfm}

procedure TISAPIModule.DataModuleCreate(Sender: TObject);
  begin
  GetMessengerProvider.ServerLink.Server:=Server;
  end;

end.
