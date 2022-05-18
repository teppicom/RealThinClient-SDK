{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  @exclude }
library rtcISAPIProject;

uses
  ActiveX,
  ComObj,
  rtcISAPIApp;

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.Run;
end.
