{
  Load the appropriate low-level Socket API implementation.
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)

  @exclude
}
unit rtcSockets;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTypes,
  rtcSockBase;

var
  DefaultRtcSocketClass:TRtcSocketBaseClass=nil;

implementation

{$IFDEF WINDOWS}
  {$IFNDEF RTC_noAsynSock}
    uses
      rtcWinSocket;
    initialization
    DefaultRtcSocketClass:=TRtcWinSocket;
  {$ELSE}{$IFNDEF RTC_noSynSock}
    uses
      rtcSynSocket;
    initialization
    DefaultRtcSocketClass:=TRtcSynSocket;
  {$ELSE}
    {$MESSAGE WARN 'AsynSock and SynSock API support excluded (RTC_noAsynSock, RTC_noSynSock).'}
  {$ENDIF}{$ENDIF}
{$ELSE}
  {$IFNDEF RTC_noSynSock}
  uses
    rtcSynSocket;
  initialization
  DefaultRtcSocketClass:=TRtcSynSocket;
  {$ELSE}
    {$MESSAGE WARN 'SynSock API support excluded (RTC_noSynSock).'}
  {$ENDIF}
{$ENDIF}
end.
