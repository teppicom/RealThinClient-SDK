{
  @exclude

  @html(<b>)
  StreamSec SSL Plug-in Registration
  @html(</b>)
  - Copyright (c) Teppi Technology (https://rtc.teppi.net)
  - Copyright (c) Henrick Hellstrom
  @html(<br><br>)
}
unit rtcSSecRegister;

interface

uses
  Classes, SysUtils,

  // RTC SDK
  rtcPlugins,
  rtcSSecPlugin;

procedure Register;

implementation

procedure Register;
  begin
  RegisterComponents('RTC Server',[TRtcSSecServerPlugin]);
  RegisterComponents('RTC Client',[TRtcSSecClientPlugin]);
  end;

end.
