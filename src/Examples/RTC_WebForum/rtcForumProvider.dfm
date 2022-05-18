object Forum_Provider: TForum_Provider
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 268
  Top = 204
  Height = 121
  Width = 297
  object ServerLink: TRtcDataServerLink
    CheckOrder = 10
    Left = 25
    Top = 20
  end
  object PageProvider: TRtcDataProvider
    Link = ServerLink
    CheckOrder = 100
    OnCheckRequest = PageProviderCheckRequest
    OnDataReceived = PageProviderDataReceived
    OnDisconnect = PageProviderDisconnect
    Left = 100
    Top = 20
  end
  object FileProvider: TRtcDataProvider
    Link = ServerLink
    CheckOrder = 900
    OnCheckRequest = FileProviderCheckRequest
    OnDataReceived = FileProviderSendBody
    OnDataSent = FileProviderSendBody
    OnDisconnect = FileProviderDisconnect
    Left = 176
    Top = 20
  end
end
