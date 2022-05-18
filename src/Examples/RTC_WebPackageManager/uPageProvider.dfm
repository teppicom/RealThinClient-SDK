object PageDM: TPageDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 313
  Top = 239
  Height = 121
  Width = 215
  object ServerLink: TRtcDataServerLink
    CheckOrder = 1
    Left = 25
    Top = 20
  end
  object Provider: TRtcDataProvider
    Link = ServerLink
    CheckOrder = 100
    OnCheckRequest = ProviderCheckRequest
    OnDataReceived = ProviderSendBody
    OnDisconnect = ProviderDisconnect
    Left = 100
    Top = 20
  end
end
