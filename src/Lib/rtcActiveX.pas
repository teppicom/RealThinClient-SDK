{
  @html(<b>)
  ActiveX support
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)  
  
  Include this unit somewhere in your Project if you are using 
  ActiveX objects in multi-threaded RTC Applications.
  
  This unit creates and registers a Thread Callback to automatically call 
  "CoInitialize" when starting RTC Threads and "CoUnInitialize" when closing them.
}
unit rtcActiveX;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes, ActiveX,
  rtcTypes, rtcLog, rtcThrPool;

type
  { An instance of the "TRtcActiveX" class will be created and registered
    as RTC Thread Callback if this (rtcActiveX) unit is used anywhere in the 
    Project, to automatically call "CoInitialize" when starting RTC Threads 
    and "CoUnInitialize" when closing them. }
  TRtcActiveX=class(TRtcThreadCallback)
    procedure AfterThreadStart; override;
    { Called from inside each Thread, before it will be stopped/destroyed }
    procedure BeforeThreadStop; override;
    { Callled after all threads have been stopped.
      This is the method from which you should destroy the object by calling "Free" }
    procedure DestroyCallback; override;
    end;

implementation

{ TRtcActiveX }

procedure TRtcActiveX.AfterThreadStart;
  begin
  CoInitialize(nil);
  end;

procedure TRtcActiveX.BeforeThreadStop;
  begin
  CoUninitialize;
  end;

procedure TRtcActiveX.DestroyCallback;
  begin
  Free;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcActiveX Initializing ...','DEBUG');{$ENDIF}

AddThreadCallback( TRtcActiveX.Create );

{$IFDEF RTC_DEBUG} Log('rtcActiveX Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcActiveX Finalizing ...','DEBUG');{$ENDIF}
CloseThreadPool;
{$IFDEF RTC_DEBUG} Log('rtcActiveX Finalized.','DEBUG');{$ENDIF}
end.
