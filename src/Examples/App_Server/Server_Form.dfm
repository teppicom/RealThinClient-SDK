object Form1: TForm1
  Left = 354
  Top = 161
  Width = 305
  Height = 434
  AutoSize = True
  Caption = 'RTC App Server demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 1
    Top = 250
    Width = 87
    Height = 16
    Caption = 'Not listening'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 100
    Top = 194
    Width = 67
    Height = 16
    Caption = 'Server Port'
  end
  object Label3: TLabel
    Left = 0
    Top = 299
    Width = 43
    Height = 16
    Caption = 'Clients:'
  end
  object lblCliCnt: TLabel
    Left = 79
    Top = 299
    Width = 7
    Height = 16
    Caption = '0'
  end
  object Label6: TLabel
    Left = 0
    Top = 348
    Width = 52
    Height = 16
    Caption = 'Memory:'
  end
  object lblTotalMem: TLabel
    Left = 79
    Top = 348
    Width = 7
    Height = 16
    Caption = '0'
  end
  object Label5: TLabel
    Left = 1
    Top = 194
    Width = 79
    Height = 16
    Caption = 'Max Threads'
  end
  object Label7: TLabel
    Left = 0
    Top = 373
    Width = 40
    Height = 16
    Caption = 'In+Out:'
  end
  object lblDataInOut: TLabel
    Left = 59
    Top = 373
    Width = 21
    Height = 16
    Caption = '???'
  end
  object lblPluginState: TLabel
    Left = 0
    Top = 167
    Width = 107
    Height = 16
    Caption = 'CryptPlugin state?'
  end
  object Label4: TLabel
    Left = 0
    Top = 324
    Width = 73
    Height = 16
    Caption = 'Responses:'
  end
  object lblResult: TLabel
    Left = 79
    Top = 324
    Width = 7
    Height = 16
    Caption = '0'
  end
  object btnListen: TButton
    Left = 183
    Top = 207
    Width = 93
    Height = 32
    Caption = 'Listen'
    Default = True
    TabOrder = 2
    OnClick = btnListenClick
  end
  object ePort: TEdit
    Left = 100
    Top = 213
    Width = 65
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    Text = '81'
  end
  object xEncrypt: TCheckBox
    Left = 0
    Top = 0
    Width = 95
    Height = 21
    Caption = 'Encryption'
    TabOrder = 3
    OnClick = xEncryptClick
  end
  object xMultiThreaded: TCheckBox
    Left = 0
    Top = 85
    Width = 272
    Height = 21
    Caption = 'Multi-Threaded mode (set before "Listen")'
    TabOrder = 7
    OnClick = xMultiThreadedClick
  end
  object xCompress: TCheckBox
    Left = 93
    Top = 0
    Width = 101
    Height = 21
    Caption = 'Compression'
    TabOrder = 4
    OnClick = xCompressClick
  end
  object eThreads: TEdit
    Left = 1
    Top = 213
    Width = 60
    Height = 24
    TabOrder = 0
    Text = '64'
  end
  object xMonitorDataInOut: TCheckBox
    Left = 0
    Top = 113
    Width = 287
    Height = 20
    Caption = 'Count Bytes In + Out (a lot slower)'
    TabOrder = 8
  end
  object xBlocking: TCheckBox
    Left = 0
    Top = 56
    Width = 262
    Height = 21
    Caption = 'Use Blocking API (set before "Listen")'
    TabOrder = 6
    OnClick = xBlockingClick
  end
  object xCryptPlugin: TCheckBox
    Left = 0
    Top = 141
    Width = 247
    Height = 21
    Caption = 'Use CryptPlugin (no SSL)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = xCryptPluginClick
  end
  object xRTCTimeouts: TCheckBox
    Left = 0
    Top = 29
    Width = 116
    Height = 21
    Caption = 'RTC Timeouts'
    TabOrder = 5
    OnClick = xRTCTimeoutsClick
  end
  object xAPITimeouts: TCheckBox
    Left = 123
    Top = 29
    Width = 150
    Height = 21
    Caption = 'Socket API Timeouts'
    TabOrder = 10
    OnClick = xAPITimeoutsClick
  end
  object xIPv6: TCheckBox
    Left = 196
    Top = 188
    Width = 61
    Height = 17
    Caption = 'IPv6'
    TabOrder = 11
  end
  object RtcDataServer1: TRtcHttpServer
    ServerPort = '81'
    RestartOn.ListenLost = True
    OnClientConnect = RtcDataServer1ClientConnect
    OnClientDisconnect = RtcDataServer1ClientDisconnect
    OnListenStart = RtcDataServer1ListenStart
    OnListenStop = RtcDataServer1ListenStop
    OnListenError = RtcDataServer1ListenError
    FixupRequest.RemovePrefix = True
    OnResponseDone = RtcDataServer1ResponseDone
    OnDataOut = RtcDataServer1DataOut
    OnDataIn = RtcDataServer1DataIn
    MaxHeaderSize = 16000
    Left = 209
    Top = 258
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 121
    Top = 258
  end
end
