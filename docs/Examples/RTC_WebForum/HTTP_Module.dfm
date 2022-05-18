object HTTP_Server: THTTP_Server
  OldCreateOrder = False
  Left = 254
  Top = 200
  Height = 163
  Width = 196
  object Server: TRtcHttpServer
    MultiThreaded = True
    Timeout.AfterConnecting = 60
    ServerPort = '80'
    OnConnecting = ServerConnecting
    OnDisconnecting = ServerDisconnecting
    OnDisconnect = ServerDisconnect
    OnException = ServerException
    RestartOn.ListenLost = True
    OnListenStart = ServerListenStart
    OnListenStop = ServerListenStop
    OnListenError = ServerListenError
    FixupRequest.RemovePrefix = True
    FixupRequest.DecodeFileName = True
    OnRequestNotAccepted = ServerRequestNotAccepted
    MaxRequestSize = 128000
    MaxHeaderSize = 16000
    OnInvalidRequest = ServerInvalidRequest
    Left = 28
    Top = 20
  end
end
