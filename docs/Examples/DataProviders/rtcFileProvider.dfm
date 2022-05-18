object File_Provider: TFile_Provider
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 366
  Top = 223
  Height = 122
  Width = 266
  object FileProvider: TRtcDataProvider
    Link = ServerLink
    CheckOrder = 900
    OnCheckRequest = FileProviderCheckRequest
    OnDataReceived = FileProviderSendBody
    OnDataSent = FileProviderSendBody
    OnDisconnect = FileProviderDisconnect
    Left = 96
    Top = 15
  end
  object TimeProvider: TRtcDataProvider
    Link = ServerLink
    CheckOrder = 700
    OnCheckRequest = TimeProviderCheckRequest
    OnDataReceived = TimeProviderDataReceived
    Left = 176
    Top = 15
  end
  object ServerLink: TRtcDataServerLink
    CheckOrder = 1000
    Left = 24
    Top = 15
  end
end
