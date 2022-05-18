{##############################################################################}
{# NexusDB - RTC: nxRTCClient.pas %NXDBVERSION%                               #}
{# Copyright (c) NexusDB Pty. Ltd. %COPYRIGHT%                                #}
{# All rights reserved.                                                       #}
{##############################################################################}
{# NexusDB - RTC: Client                                                      #}
{##############################################################################}

{$I nxDefine.inc}
unit nxRTCClient;

interface

uses
  Classes,
  Windows,
  nxllTypes,
  nxllComponent,
  nxllPluginBase,

  rtcTransports,
  nxRTCCommon;

type
  TnxRTCPluginEngine = class(TnxBasePluginEngine, IRTCMessageReceiver)
  protected
    class function bpeIsRemote: Boolean; override;

    { IRTCMessageReceiver }
    procedure ProcessMessage(aRequest, aReply: TStream);

  published
    property Session;
    property Timeout;
  end;

implementation

uses
  nxllBde,
  nxllMemoryManager,
  SysUtils,
  nxllStreams;

{===TnxRTCPluginEngine=========================================================}
class function TnxRTCPluginEngine.bpeIsRemote: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------}
procedure TnxRTCPluginEngine.ProcessMessage(aRequest, aReply: TStream);
var
  Result : TnxResult;
  Request: Pointer;
  FreeMe : Pointer;
begin
  scCheckStarted;

  FreeMe := nil;

  if aRequest is TCustomMemoryStream then
    Request := TCustomMemoryStream(aRequest).Memory
  else
    if aRequest is TStringStream then
      Request := Pointer(TStringStream(aRequest).DataString)
    else
      if aRequest is TnxCustomMemoryStream then
        Request := TnxCustomMemoryStream(aRequest).Memory
      else begin
        GetMem(FreeMe, aRequest.Size);
        Request := FreeMe;
        aRequest.Position := 0;
        aRequest.WriteBuffer(Request^, aRequest.Size);
      end;

  try
    Result := bpeProcessRequest(nxnmRTCTunnel, Request, aRequest.Size, @aReply,
      nil, nmdStream);

    if Result <> DBIERR_NONE then
      raise EnxPluginException.nxcCreate(Self, Result);
  finally
    if Assigned(FreeMe) then
      FreeMem(FreeMe);
  end;
end;
{==============================================================================}

end.
