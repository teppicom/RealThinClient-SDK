unit myDebugLog;

interface

uses
  rtcLog;

implementation

initialization
RTC_LOG_THREADID:=false;
LOG_AV_ERRORS:=true;
LOG_FOLDER:='MYLOG';
end.
