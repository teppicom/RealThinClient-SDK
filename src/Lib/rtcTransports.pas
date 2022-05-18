{
  @html(<b>)
  Transports Plug-in
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit defines a Plug-in interface, which should be implemented when writing
  third-party transports to be used with the RealThinClient SDK components.
}
unit rtcTransports;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,

  rtcTypes;

const
  // Interface GUID
  IRTCMessageReceiverGUID: TGUID = '{C9E3606F-ED08-43F4-AF97-F55A3EC8F14B}';

type
  { @abstract(Transport Plugin interface)
    This interface should be *IMPLEMENTED* when writing a "receiver" transport plugin,
    which can be linked to TRtcMessageClient in place of the TRtcMessageServer component. }
  IRTCMessageReceiver = interface
    ['{C9E3606F-ED08-43F4-AF97-F55A3EC8F14B}']
    { ProcessMessage need to be called *only* once per request, with a complete request data 
      waiting in the "aRequest" stream and an empty "aReply" stream to receive the response.
      Request processing is done in a "blocking mode", which means that the caller will
      have the complete response filled in the "aReply" stream, before the call returns. }
    procedure ProcessMessage(aRequest, aReply: TStream);
  end;

implementation

end.
