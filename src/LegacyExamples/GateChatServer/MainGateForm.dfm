object GateForm: TGateForm
  Left = 336
  Top = 158
  Width = 428
  Height = 151
  AutoSize = True
  Caption = 'RTC Gate Chat Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 410
    Height = 106
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 24
      Height = 16
      Caption = 'Port'
    end
    object shGateway: TShape
      Left = 340
      Top = 60
      Width = 61
      Height = 37
      Brush.Color = clRed
      Shape = stRoundRect
    end
    object Label2: TLabel
      Left = 240
      Top = 4
      Width = 99
      Height = 16
      Caption = 'Max OUT speed'
    end
    object Label3: TLabel
      Left = 304
      Top = 28
      Width = 22
      Height = 16
      Caption = 'Kbit'
    end
    object lblStatus: TLabel
      Left = 4
      Top = 56
      Width = 78
      Height = 16
      Caption = 'Click START'
    end
    object lblConnect: TLabel
      Left = 92
      Top = 76
      Width = 16
      Height = 16
      Caption = '---'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 4
      Top = 76
      Width = 77
      Height = 16
      Caption = 'Connections:'
    end
    object Label5: TLabel
      Left = 144
      Top = 4
      Width = 83
      Height = 16
      Caption = 'Max IN speed'
    end
    object Label6: TLabel
      Left = 208
      Top = 28
      Width = 22
      Height = 16
      Caption = 'Kbit'
    end
    object ePort: TEdit
      Left = 4
      Top = 24
      Width = 61
      Height = 24
      TabOrder = 0
      Text = '80'
    end
    object btnStart: TButton
      Left = 340
      Top = 8
      Width = 61
      Height = 45
      Caption = 'START'
      TabOrder = 4
      OnClick = btnStartClick
    end
    object eSendSpeed: TSpinEdit
      Left = 240
      Top = 24
      Width = 61
      Height = 26
      Increment = 50
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 3
      Value = 500
    end
    object eRecvSpeed: TSpinEdit
      Left = 144
      Top = 24
      Width = 61
      Height = 26
      Increment = 50
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 2
      Value = 500
    end
    object xIPv6: TCheckBox
      Left = 76
      Top = 28
      Width = 61
      Height = 17
      Caption = 'IPv6'
      TabOrder = 1
    end
  end
  object Server: TRtcHttpServer
    Timeout.AfterConnecting = 300
    OnException = ServerException
    RestartOn.ListenLost = True
    RestartOn.ListenError = True
    OnListenStart = ServerListenStart
    OnListenStop = ServerListenStop
    OnListenError = ServerListenError
    FixupRequest.RemovePrefix = True
    OnRequestNotAccepted = ServerRequestNotAccepted
    MaxHeaderSize = 2048
    OnInvalidRequest = ServerInvalidRequest
    TimeoutsOfAPI.SendTimeout = 50
    TimeoutsOfAPI.ReceiveTimeout = 50
    Left = 116
    Top = 56
  end
  object StatusTimer: TTimer
    Interval = 500
    OnTimer = StatusTimerTimer
    Left = 224
    Top = 56
  end
  object Gate: TRtcGateway
    GateFileName = '/'
    GatePrimaryKey = 'MyPrimaryChatKey'
    BeforeUserLogin = GateBeforeUserLogin
    OnUserReady = GateUserReady
    OnUserNotReady = GateUserNotReady
    BeforeUserLogout = GateBeforeUserLogout
    Server = Server
    Left = 164
    Top = 56
  end
end
