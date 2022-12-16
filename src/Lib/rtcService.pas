{
  @html(<b>)
  MS Windows Service functions
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit is ONLY for MS Windows.
}
unit rtcService;

{$include rtcDefs.inc}

interface

{$IFDEF WINDOWS}

uses
  Windows,
  WinSvc,
  SysUtils,

  rtcTypes;

function IsServiceStarting(const ServiceName:String):boolean;

function IsDesktopMode(const ServiceName:String):boolean;

function IsMiniMode:boolean;

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

function IsServiceStarting(const ServiceName:String): Boolean;
  var
    Svc: THandle;
    SvcMgr: THandle;
    ServSt: TServiceStatus;
  begin
  Result := False;
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SvcMgr = 0 then Exit;
  try
    Svc := OpenService (SvcMgr, PChar(ServiceName), SERVICE_QUERY_STATUS);
    if Svc = 0 then Exit;
    try
      if not QueryServiceStatus (Svc, ServSt) then Exit;
      Result := (ServSt.dwCurrentState = SERVICE_START_PENDING);
    finally
      CloseServiceHandle(Svc);
      end;
  finally
    CloseServiceHandle(SvcMgr);
    end;
  end;

function IsDesktopMode(const ServiceName:String):boolean;
  begin
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    Result := True
  else
    begin
    Result := not FindCmdLineSwitch('INSTALL', ['-', '/'], True) and
              not FindCmdLineSwitch('UNINSTALL', ['-', '/'], True) and
              not IsServiceStarting(ServiceName);
    end;
  end;

function IsMiniMode:boolean;
  begin
  Result:=FindCmdLineSwitch('M',['-','/'],True);
  end;


{$ENDIF} // {$IFDEF WINDOWS}
end.