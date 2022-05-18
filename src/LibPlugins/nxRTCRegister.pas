{##############################################################################}
{# NexusDB - RTC components registration %NXDBVERSION%                        #}
{# Copyright (c) NexusDB Pty. Ltd. %COPYRIGHT%                                #}
{# All rights reserved.                                                       #}
{##############################################################################}
{# NexusDB - RTC SDK: Command Handler and Plugin Engine                       #}
{##############################################################################}

unit nxRTCRegister;

{$INCLUDE rtcDefs.inc}

interface

procedure Register;

implementation

uses
  {$IFNDEF FPC}
  {$IFDEF IDE_6up}
  DesignIntf,
  DesignEditors,
  TypInfo,
  Consts,
  {$ELSE}
  DsgnIntf,
  TypInfo,
  RTLConsts,
  {$ENDIF}
  {$ENDIF}
  Classes,

  rtcTransports,
  rtcEditors,

  nxRTCClient,
  nxRTCServer;

type
  TRtcMessageReceiverInterfacedComponentProperty = class
    (TRtcInterfacedComponentProperty)
  public
    function GetIID: TGUID; override;
  end;

function TRtcMessageReceiverInterfacedComponentProperty.GetIID: TGUID;
begin
  Result := IRTCMessageReceiverGUID;
end;

procedure Register;
begin
  RegisterComponents('RTC Server', [TnxRTCPluginCommandHandler]);
  RegisterComponents('RTC Client', [TnxRTCPluginEngine]);

  RegisterPropertyEditor(TComponent.ClassInfo, TnxRTCPluginCommandHandler,
    'RTCServer', TRtcMessageReceiverInterfacedComponentProperty);
end;

end.
