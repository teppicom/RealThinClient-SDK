unit Isapi_Module;

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

  rtcSystem, rtcInfo,
  rtcConn, rtcDataSrv,
  rtcISAPISrv,

  rtcLog,

  AppServer_Module;

type
  TMyISAPI_Module = class(TDataModule)
    Server: TRtcISAPIServer;
    procedure ServerListenStart(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MyISAPI_Module: TMyISAPI_Module;

implementation

{$R *.dfm}

procedure TMyISAPI_Module.ServerListenStart(Sender: TRtcConnection);
  begin
  GetAppSrvModule.ServerLink.Server:=Server;
  end;

end.
