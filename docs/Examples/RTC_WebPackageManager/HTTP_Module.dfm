object HTTP_Server: THTTP_Server
  OldCreateOrder = False
  Left = 371
  Top = 277
  Height = 163
  Width = 196
  object Server: TRtcHttpServer
    MultiThreaded = True
    Timeout.AfterConnecting = 60
    ServerPort = '8080'
    OnConnecting = ServerConnecting
    OnDisconnecting = ServerDisconnecting
    OnDisconnect = ServerDisconnect
    OnException = ServerException
    RestartOn.ListenLost = True
    OnListenStart = ServerListenStart
    OnListenStop = ServerListenStop
    OnListenError = ServerListenError
    FixupRequest.RemovePrefix = True
    OnRequestNotAccepted = ServerRequestNotAccepted
    MaxRequestSize = 128000
    MaxHeaderSize = 16000
    OnInvalidRequest = ServerInvalidRequest
    Left = 28
    Top = 20
  end
end
