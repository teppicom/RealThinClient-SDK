object Form1: TForm1
  Left = 499
  Top = 152
  Width = 482
  Height = 565
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    464
    520)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 6
    Top = 225
    Width = 37
    Height = 16
    Caption = 'Status'
  end
  object Label2: TLabel
    Left = 7
    Top = 274
    Width = 412
    Height = 16
    Caption = 
      'Type '#39'hi'#39' to start a new session, '#39'bye'#39' to close a session on th' +
      'e Server.'
  end
  object Label3: TLabel
    Left = 8
    Top = 32
    Width = 74
    Height = 16
    Caption = 'Data Format'
  end
  object Label4: TLabel
    Left = 196
    Top = 32
    Width = 121
    Height = 16
    Caption = 'Data Request Mode'
  end
  object Edit1: TEdit
    Left = 7
    Top = 246
    Width = 390
    Height = 24
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
  end
  object Memo1: TMemo
    Left = 7
    Top = 296
    Width = 446
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object xMultiThr: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Multi-Threaded'
    TabOrder = 2
    OnClick = xMultiThrClick
  end
  object xBlocking: TCheckBox
    Left = 116
    Top = 8
    Width = 81
    Height = 17
    Caption = 'Blocking'
    TabOrder = 3
    OnClick = xBlockingClick
  end
  object xProxy: TCheckBox
    Left = 208
    Top = 8
    Width = 65
    Height = 17
    Caption = 'Proxy'
    TabOrder = 4
    OnClick = xProxyClick
  end
  object xWinHttp: TCheckBox
    Left = 284
    Top = 8
    Width = 89
    Height = 17
    Caption = 'WinHTTP'
    TabOrder = 5
    OnClick = xWinHttpClick
  end
  object xFMT: TListBox
    Left = 8
    Top = 56
    Width = 173
    Height = 101
    ItemHeight = 16
    Items.Strings = (
      'fmt_RTC'
      'fmt_XMLRPC'
      'fmt_JSONrpc1'
      'fmt_JSONrpc2'
      'fmt_JSON')
    TabOrder = 6
    OnClick = xFMTClick
  end
  object xMode: TListBox
    Left = 196
    Top = 56
    Width = 197
    Height = 165
    ItemHeight = 16
    Items.Strings = (
      'req_contentBodyALL'
      'req_contentBodyOptional'
      'req_contentBodyParams'
      'req_directJSON'
      'req_queryJSON'
      'req_queryNameJSON'
      'req_queryNameText'
      'req_uriParamsJSON'
      'req_uriParamsText')
    TabOrder = 7
    OnClick = xModeClick
  end
  object RtcHttpClient1: TRtcHttpClient
    ServerAddr = 'localhost'
    ServerPort = '81'
    OnConnect = RtcHttpClient1Connect
    OnDisconnect = RtcHttpClient1Disconnect
    ReconnectOn.ConnectError = True
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    AutoConnect = True
    Left = 52
    Top = 277
  end
  object RtcClientModule1: TRtcClientModule
    AutoSyncEvents = True
    Client = RtcHttpClient1
    ModuleFileName = '/mytest'
    OnEncryptWrongKey = RtcClientModule1EncryptWrongKey
    OnEncryptNotSupported = RtcClientModule1EncryptNotSupported
    OnEncryptRequired = RtcClientModule1EncryptRequired
    OnResponseError = RtcClientModule1ResponseError
    OnBeginRequest = RtcClientModule1BeginRequest
    OnResponseDone = RtcClientModule1ResponseDone
    OnRepostCheck = RtcClientModule1RepostCheck
    OnResponseAbort = RtcClientModule1ResponseAbort
    OnResponseReject = RtcClientModule1ResponseReject
    OnConnectLost = RtcClientModule1ConnectLost
    OnResultError = RtcClientModule1ResultError
    Left = 164
    Top = 277
  end
end
