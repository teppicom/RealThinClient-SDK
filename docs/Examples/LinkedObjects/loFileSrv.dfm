object loFileServer: TloFileServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 204
  Top = 152
  Height = 166
  Width = 267
  object Link: TRtcLinkedModule
    RemoteClass = 'FileClient'
    OnCallEvent = LinkCallEvent
    OnCallMethod = LinkCallMethod
    OnBroadcast = LinkBroadcast
    Left = 24
    Top = 16
  end
end
