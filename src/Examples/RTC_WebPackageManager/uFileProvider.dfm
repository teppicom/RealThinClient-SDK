object FileDM: TFileDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 359
  Top = 226
  Height = 122
  Width = 170
  object Provider: TRtcDataProvider
    Link = ServerLink
    CheckOrder = 900
    OnCheckRequest = ProviderCheckRequest
    OnDataReceived = ProviderSendBody
    OnDataSent = ProviderSendBody
    OnDisconnect = ProviderDisconnect
    Left = 96
    Top = 15
  end
  object ServerLink: TRtcDataServerLink
    CheckOrder = 1000
    Left = 24
    Top = 15
  end
end
