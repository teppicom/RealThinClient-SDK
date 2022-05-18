object PHP_Provider: TPHP_Provider
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 223
  Top = 194
  Height = 143
  Width = 206
  object PhpProvider: TRtcDataProvider
    Link = ServerLink
    OnListenStart = PhpProviderListenStart
    OnListenStop = PhpProviderListenStop
    OnCheckRequest = PhpProviderCheckRequest
    OnDataReceived = PhpProviderDataReceived
    OnDisconnect = PhpProviderDisconnect
    Left = 88
    Top = 15
  end
  object ServerLink: TRtcDataServerLink
    CheckOrder = 900
    Left = 20
    Top = 15
  end
end
