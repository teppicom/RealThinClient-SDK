object Gateway_Provider: TGateway_Provider
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 499
  Top = 157
  Height = 150
  Width = 278
  object ServerLink: TRtcDataServerLink
    Left = 32
    Top = 15
  end
  object RtcGateway: TRtcGateway
    GateFileName = '/'
    Link = ServerLink
    Left = 120
    Top = 16
  end
end
