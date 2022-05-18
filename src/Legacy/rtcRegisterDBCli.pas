{
  @html(<b>)
  Client DataSet component Registration (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  RealThinClient Client DataSet component is being
  registered to Delphi component palette.
  
  @exclude
}
unit rtcRegisterDBCli;

{$INCLUDE rtcDefs.inc}

interface

// This procedure is being called by Delphi to register the components.
procedure Register;

implementation

uses
  Classes,
  rtcDBCli;

procedure Register;
  begin
  RegisterComponents('RTC DBA',[TRtcClientDataSet]);
  end;

end.
