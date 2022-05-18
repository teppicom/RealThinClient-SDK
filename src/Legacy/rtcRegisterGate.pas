{
  @html(<b>)
  RTC Gateway components Registration (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  RealThinClient Gateway components are being
  registered to Delphi component palette.

  @exclude
}
unit rtcRegisterGate;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,
  rtcTypes,

  rtcGateConst,
  rtcGateCli,
  rtcGateSrv;

procedure Register;
  begin
  RegisterComponents('RTC Gate',[TRtcGateway,
                                 TRtcHttpGateway,
                                 TRtcHttpGateClient,
                                 TRtcGateClientLink,
                                 TRtcGateAccountManager,
                                 TRtcHttpMultiGateClient,
                                 TRtcMultiGateClientLink,
                                 TRtcGCThread]);
  end;

end.
