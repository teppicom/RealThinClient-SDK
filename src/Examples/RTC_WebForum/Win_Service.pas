unit Win_Service;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, SvcMgr, Dialogs,
{$IFDEF SSL}
  Server_ModuleTLS;
{$ELSE}
  Server_Module;
{$ENDIF}

const
  RTC_DATASERVICE_NAME='Rtc_WebServer';

type
  TRtc_WebServer = class(TService)
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    Running:boolean;

    function GetServiceController:
      {$IFDEF VER120} PServiceController;
      {$ELSE} TServiceController; {$ENDIF} override;

    procedure StartMyService;
    procedure StopMyService;
  end;

var
  Rtc_WebServer: TRtc_WebServer;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
  begin
  Rtc_WebServer.Controller(CtrlCode);
  end;

function TRtc_WebServer.GetServiceController:
  {$IFDEF VER120} PServiceController;
  {$ELSE} TServiceController; {$ENDIF}
  begin
  Result := {$IFDEF VER120}@{$ENDIF}ServiceController;
  end;

procedure TRtc_WebServer.ServiceStart(Sender: TService; var Started: Boolean);
  begin
  StartMyService;
  Started:=Running;
  end;

procedure TRtc_WebServer.ServiceStop(Sender: TService; var Stopped: Boolean);
  begin
  StopMyService;
  Stopped:=not Running;
  end;

procedure TRtc_WebServer.ServiceShutdown(Sender: TService);
  begin
  StopMyService;
  end;

procedure TRtc_WebServer.StartMyService;
  begin
  if not running then
    begin
    Data_Server.Start;
    running := True;
    end;
  end;

procedure TRtc_WebServer.StopMyService;
  begin
  if running then
    begin
    Data_Server.Stop;
    running := False;
    end;
  end;

end.
