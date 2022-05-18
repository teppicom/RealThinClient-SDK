object ISAPI_Provider: TISAPI_Provider
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 375
  Top = 164
  Height = 143
  Width = 282
  object Provider: TRtcDataProvider
    Link = ServerLink
    CheckOrder = 100
    OnListenStart = ProviderListenStart
    OnListenStop = ProviderListenStop
    OnCheckRequest = ProviderCheckRequest
    OnDataReceived = ProviderDataReceived
    OnDisconnect = ProviderDisconnect
    Left = 152
    Top = 15
  end
  object ServerLink: TRtcDataServerLink
    CheckOrder = 700
    Left = 20
    Top = 15
  end
  object Unloader: TRtcDataProvider
    Link = ServerLink
    OnCheckRequest = UnloaderCheckRequest
    OnDataReceived = UnloaderDataReceived
    Left = 88
    Top = 15
  end
end
