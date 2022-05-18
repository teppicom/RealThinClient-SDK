object ISAPIModule: TISAPIModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 252
  Top = 225
  Height = 150
  Width = 215
  object Server: TRtcISAPIServer
    FixupRequest.RemovePrefix = True
    Left = 32
    Top = 15
  end
end
