object ISAPI_Server: TISAPI_Server
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 228
  Top = 107
  Height = 150
  Width = 215
  object Server: TRtcISAPIServer
    OnListenStart = ServerListenStart
    OnListenStop = ServerListenStop
    FixupRequest.RemovePrefix = True
    OnRequestNotAccepted = ServerRequestNotAccepted
    Left = 32
    Top = 15
  end
end
