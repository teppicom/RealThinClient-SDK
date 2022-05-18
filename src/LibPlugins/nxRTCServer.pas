{##############################################################################}
{# NexusDB - RTC: nxRTCServer.pas %NXDBVERSION%                               #}
{# Copyright (c) NexusDB Pty. Ltd. %COPYRIGHT%                                #}
{# All rights reserved.                                                       #}
{##############################################################################}
{# NexusDB - RTC: Server                                                      #}
{##############################################################################}

unit nxRTCServer;

{$I nxDefine.inc}

interface

uses
  Classes,
  nxllMemoryManager,
  nxllTypes,
  nxllThread,
  nxllStreams,
  nxllSync,
  nxllTransport,
  rtcTransports,
  nxllComponent,
  nxRTCCommon;

type
  EnxRTCPluginCommandHandlerException = class(EnxPluginCommandHandlerException);

  TGMode = (tgmDirect, tgmDLL);

  TGetIRTCMessageReceiver = function       : IRTCMessageReceiver; stdcall;
  TStartGW                = function(settings: OleVariant): integer; stdcall;
  TStopGW                 = function                       : integer; stdcall;

  TnxRTCPluginCommandHandler = class(TnxBasePluginCommandHandler)
  protected {private}
    rpchDLLInst            : THandle;
    rpchServer             : IRTCMessageReceiver;
    fMode                  : TGMode;
    fStartGW               : TStartGW;
    fStopGW                : TStopGW;
    fGetIRTCMessageReceiver: TGetIRTCMessageReceiver;

    procedure rpchSetServer(const aValue: IRTCMessageReceiver);
  private
    fSettings: String;
    procedure SetMode(const Value: TGMode);
  protected
    DLLServer: IRTCMessageReceiver;

    procedure InitDLL;
    procedure ShutDOwnDLL;
    procedure scInitializing; override;
    // procedure scDeactivating; override;
    procedure scDeactivating; override;
    function ActualServer: IRTCMessageReceiver;
    procedure nxRTCTunnel(var Msg: TnxDataMessage);
    function UISettingsVisible: Boolean; override;
    procedure scSetActive(const aActive: Boolean); override;
  public
    constructor Create(aOwner: TComponent); override;
    procedure bpchProcess(aMsg: PnxDataMessage; var aHandled: Boolean);
      override;
    procedure GetConfigSettings(aSettings: TnxBaseSettings); override;
    procedure LoadConfig(aConfig: TnxBaseComponentConfiguration); override;
    procedure SaveConfig(aConfig: TnxBaseComponentConfiguration); override;
  published
    property RTCServer: IRTCMessageReceiver
      read rpchServer
      write rpchSetServer;
    property Mode: TGMode
      read fMode
      write SetMode;
    property settings: String
      read fSettings
      write fSettings;
  end;

implementation

uses
  Windows,
  nxllBde,
  SysUtils,
  nxllConvertException;

resourcestring
  rsNoServerAssigend = 'No Server assigend';
  rsTheServerMustSupportIRTCMessageReceiver =
    'The Server must support IRTCMessageReceiver';

  {===TnxRTCPluginCommandHandler=======================================}
function TnxRTCPluginCommandHandler.ActualServer: IRTCMessageReceiver;
begin
  if Mode = tgmDLL then
    result := DLLServer
  else
    result := rpchServer;
end;

procedure TnxRTCPluginCommandHandler.bpchProcess(aMsg: PnxDataMessage;
  var aHandled: Boolean);
begin
  aHandled := True;
  case aMsg^.dmMsg of
    nxnmRTCTunnel:
      nxRTCTunnel(aMsg^);
  else
    aHandled := False;
  end;
end;

constructor TnxRTCPluginCommandHandler.Create(aOwner: TComponent);
begin
  inherited;
  fMode := tgmDirect;
  DisplayName := 'Portal Gateway';
  DisplayCategory := 'Plugins';
  fSettings := 'NoHTTP=0;Port=8080';
end;

procedure TnxRTCPluginCommandHandler.GetConfigSettings
  (aSettings: TnxBaseSettings);
begin
  inherited;
  with aSettings.AddSetting(TnxBaseSetting.Create) do begin
    Name := 'Settings';
    DefaultValue := '';
    SettingType := nxstString;
    PropertyName := 'Settings';
    EnforceValues := False;
    ValueList := '';
  end; // with
end;

{------------------------------------------------------------------------------}
procedure TnxRTCPluginCommandHandler.InitDLL;
begin
  rpchDLLInst := LoadLibrary('nxPortalGateway.dll');
  if rpchDLLInst <> 0 then begin
    @fStartGW := GetProcAddress(rpchDLLInst, 'StartGW');
    @fStopGW := GetProcAddress(rpchDLLInst, 'StopGW');
    @fGetIRTCMessageReceiver := GetProcAddress(rpchDLLInst,
      'GetIRTCMessageReceiver');
    fStartGW(settings);
    if DLLServer = nil then
      DLLServer := fGetIRTCMessageReceiver;
  end;
end;

procedure TnxRTCPluginCommandHandler.LoadConfig
  (aConfig: TnxBaseComponentConfiguration);
begin
  Active := False;
  inherited;
  settings := aConfig.GetValue('Settings', fSettings);
end;

{------------------------------------------------------------------------------}
procedure TnxRTCPluginCommandHandler.nxRTCTunnel(var Msg: TnxDataMessage);
var
  Request: TnxDataMessageStream;
  Reply  : TnxMemoryStream;
  Error  : TnxResult;
begin
  if State <> nxsStopped then begin
    Reply := TnxMemoryStream.Create;
    try
      if ActualServer <> nil then begin
        Request := TnxDataMessageStream.Create(Msg);
        try
          try
            ActualServer.ProcessMessage(Request, Reply);
            Error := DBIERR_NONE;
          except
            on E: Exception do
              Error := ConvertException(E, lcEventLog, nil, 0);
          end;
        finally
          nxFreeAndNil(Request);
        end;
      end
      else
        Error := DBIERR_NOSERVERSW;

      if Error = DBIERR_NONE then
        TnxBaseTransport.Reply(nxnmRTCTunnel, Error, Reply.Memory, Reply.Size)
      else
        TnxBaseTransport.Reply(nxnmRTCTunnel, Error, nil, 0);
    finally
      nxFreeAndNil(Reply);
    end;
  end;
end;

{------------------------------------------------------------------------------}
procedure TnxRTCPluginCommandHandler.rpchSetServer(const aValue
  : IRTCMessageReceiver);
begin
  scCheckInactive;

  if aValue <> rpchServer then begin
    // close the DLL if one is open
    rpchServer := aValue;
  end;
end;

procedure TnxRTCPluginCommandHandler.SaveConfig
  (aConfig: TnxBaseComponentConfiguration);
begin
  inherited;
  aConfig.SetValue('Settings', fSettings);
end;

{------------------------------------------------------------------------------}
procedure TnxRTCPluginCommandHandler.scSetActive(const aActive: Boolean);
begin
  inherited;
end;

procedure TnxRTCPluginCommandHandler.scDeactivating;
begin
  inherited;
  if rpchDLLInst <> 0 then
    ShutDOwnDLL;
end;

procedure TnxRTCPluginCommandHandler.scInitializing;
{$IFNDEF DCC6OrLater}
var
  Dummy: IUnknown;
  {$ENDIF}
begin
  inherited;

  if Mode = tgmDLL then
    InitDLL;

  if (Mode = tgmDirect) and not Assigned(rpchServer) then
    raise EnxRTCPluginCommandHandlerException.nxcCreate(Self,
      rsNoServerAssigend);
  if (Mode = tgmDirect) and not Supports(rpchServer,
    IRTCMessageReceiver{$IFNDEF DCC6OrLater}, Dummy{$ENDIF}) then
    raise EnxRTCPluginCommandHandlerException.nxcCreate(Self,
      rsTheServerMustSupportIRTCMessageReceiver);
end;

procedure TnxRTCPluginCommandHandler.SetMode(const Value: TGMode);
begin
  assert(Active = False);
  fMode := Value;
end;

procedure TnxRTCPluginCommandHandler.ShutDOwnDLL;
begin
  if rpchDLLInst <> 0 then begin
    fStopGW;
    FreeLibrary(rpchDLLInst);
  end;
end;

function TnxRTCPluginCommandHandler.UISettingsVisible: Boolean;
begin
  result := True;
end;

{==============================================================================}

end.
