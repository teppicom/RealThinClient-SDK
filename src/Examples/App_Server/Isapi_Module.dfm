object MyISAPI_Module: TMyISAPI_Module
  OldCreateOrder = False
  Left = 227
  Top = 175
  Height = 152
  Width = 212
  object Server: TRtcISAPIServer
    OnListenStart = ServerListenStart
    FixupRequest.RemovePrefix = True
    Left = 44
    Top = 15
  end
end
