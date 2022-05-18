object RTCPModule: TRTCPModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 390
  Top = 185
  Height = 197
  Width = 314
  object MultiClient: TRtcHttpMultiGateClient
    OnStreamReset = MultiClientStreamReset
    GateFileName = '/'
    Left = 28
    Top = 12
  end
  object Gateway: TRtcHttpGateway
    GateFileName = '/'
    MaxUserInfoLen = 1024
    BeforeUserLogin = GatewayBeforeUserLogin
    GatePort = '80'
    Left = 112
    Top = 12
  end
end
