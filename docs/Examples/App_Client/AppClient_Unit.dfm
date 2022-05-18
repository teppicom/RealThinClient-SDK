object Form1: TForm1
  Left = 290
  Top = 132
  Width = 421
  Height = 590
  Caption = 'RTC App Client demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object myPanel: TPanel
    Left = 0
    Top = 383
    Width = 403
    Height = 162
    Align = alClient
    BorderWidth = 5
    TabOrder = 0
    object myBox: TPaintBox
      Left = 6
      Top = 6
      Width = 391
      Height = 150
      Align = alClient
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 403
    Height = 383
    Align = alTop
    BorderWidth = 4
    Caption = '^'
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 5
      Top = 5
      Width = 393
      Height = 373
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Setup'
        object Label11: TLabel
          Left = 10
          Top = 246
          Width = 85
          Height = 16
          Caption = 'Module Name'
        end
        object Label12: TLabel
          Left = 5
          Top = 21
          Width = 94
          Height = 16
          Caption = 'Server Address'
        end
        object Label13: TLabel
          Left = 30
          Top = 55
          Width = 67
          Height = 16
          Caption = 'Server Port'
        end
        object Label30: TLabel
          Left = 10
          Top = 277
          Width = 76
          Height = 16
          Caption = 'Module Host'
        end
        object lblPluginState: TLabel
          Left = 22
          Top = 207
          Width = 107
          Height = 16
          Caption = 'CryptPlugin state?'
        end
        object eModule: TEdit
          Left = 108
          Top = 240
          Width = 243
          Height = 24
          TabOrder = 10
          Text = '/TEST'
          OnExit = eModuleChange
        end
        object eServer: TEdit
          Left = 103
          Top = 17
          Width = 248
          Height = 24
          TabOrder = 0
          Text = 'localhost'
          OnChange = eServerChange
        end
        object xProxy: TCheckBox
          Left = 25
          Top = 148
          Width = 89
          Height = 19
          TabStop = False
          Caption = 'use Proxy'
          TabOrder = 6
          OnClick = xProxyClick
        end
        object ePort: TEdit
          Left = 102
          Top = 50
          Width = 46
          Height = 24
          TabOrder = 1
          Text = '81'
          OnChange = eServerChange
        end
        object xSSL: TCheckBox
          Left = 25
          Top = 177
          Width = 114
          Height = 20
          TabStop = False
          Caption = 'SSL (HTTPS)'
          TabOrder = 8
          OnClick = xSSLClick
        end
        object eModuleHost: TEdit
          Left = 108
          Top = 271
          Width = 243
          Height = 24
          TabOrder = 11
          Text = 'localhost'
          OnExit = eModuleChange
        end
        object xUseXML: TCheckBox
          Left = 108
          Top = 302
          Width = 164
          Height = 20
          Caption = 'Use XML-RPC Format'
          TabOrder = 12
          OnClick = xUseXMLClick
        end
        object xWinHTTP: TCheckBox
          Left = 158
          Top = 148
          Width = 114
          Height = 19
          TabStop = False
          Caption = 'WinHTTP'
          TabOrder = 7
          OnClick = xWinHTTPClick
        end
        object xBlocking: TCheckBox
          Left = 25
          Top = 118
          Width = 85
          Height = 21
          Caption = 'Blocking'
          TabOrder = 4
          OnClick = xBlockingClick
        end
        object xCryptPlugin: TCheckBox
          Left = 158
          Top = 177
          Width = 193
          Height = 20
          TabStop = False
          Caption = 'CryptPlugin (no SSL)'
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
          Left = 25
          Top = 89
          Width = 119
          Height = 21
          Caption = 'RTC Timeouts'
          TabOrder = 2
          OnClick = xRTCTimeoutsClick
        end
        object xHTTP10: TCheckBox
          Left = 158
          Top = 118
          Width = 129
          Height = 21
          Caption = 'Force HTTP/1.0'
          TabOrder = 5
          OnClick = xHTTP10Click
        end
        object xAPITimeouts: TCheckBox
          Left = 158
          Top = 89
          Width = 158
          Height = 21
          Caption = 'Socket API Timeouts'
          TabOrder = 3
          OnClick = xAPITimeoutsClick
        end
        object xIPv6: TCheckBox
          Left = 232
          Top = 52
          Width = 65
          Height = 17
          Caption = 'IPv6'
          TabOrder = 13
          OnClick = xIPv6Click
        end
        object xDefIP: TCheckBox
          Left = 156
          Top = 52
          Width = 69
          Height = 17
          Caption = 'Def.IP'
          TabOrder = 14
          OnClick = xIPv6Click
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'User Test'
        ImageIndex = 1
        object Label1: TLabel
          Left = 94
          Top = 111
          Width = 7
          Height = 16
          Caption = '+'
        end
        object Label2: TLabel
          Left = 202
          Top = 111
          Width = 7
          Height = 16
          Caption = '='
        end
        object Label3: TLabel
          Left = 5
          Top = 135
          Width = 41
          Height = 16
          Caption = 'waiting'
        end
        object Label4: TLabel
          Left = 5
          Top = 191
          Width = 41
          Height = 16
          Caption = 'waiting'
        end
        object Label5: TLabel
          Left = 94
          Top = 166
          Width = 6
          Height = 16
          Caption = 'x'
        end
        object Label6: TLabel
          Left = 202
          Top = 166
          Width = 7
          Height = 16
          Caption = '='
        end
        object Label7: TLabel
          Left = 261
          Top = 135
          Width = 7
          Height = 16
          Caption = '+'
        end
        object Label8: TLabel
          Left = 261
          Top = 191
          Width = 7
          Height = 16
          Caption = '='
        end
        object Label9: TLabel
          Left = 261
          Top = 246
          Width = 41
          Height = 16
          Alignment = taRightJustify
          Caption = 'waiting'
        end
        object Label10: TLabel
          Left = 5
          Top = 55
          Width = 90
          Height = 16
          Caption = 'Not connected.'
        end
        object lblCount: TLabel
          Left = 49
          Top = 218
          Width = 8
          Height = 16
          Caption = '--'
        end
        object lblTotal: TLabel
          Left = 153
          Top = 215
          Width = 8
          Height = 16
          Caption = '--'
        end
        object Label14: TLabel
          Left = 5
          Top = 218
          Width = 39
          Height = 16
          Caption = 'To do:'
        end
        object Label15: TLabel
          Left = 113
          Top = 215
          Width = 36
          Height = 16
          Caption = 'Done:'
        end
        object Label22: TLabel
          Left = 5
          Top = 80
          Width = 289
          Height = 16
          Caption = 'Enter values to get results from the Server'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit1: TEdit
          Left = 5
          Top = 105
          Width = 75
          Height = 24
          TabOrder = 3
          OnChange = Edit1Change
        end
        object Edit2: TEdit
          Left = 113
          Top = 105
          Width = 75
          Height = 24
          TabOrder = 4
          OnChange = Edit1Change
        end
        object Edit3: TEdit
          Left = 222
          Top = 105
          Width = 80
          Height = 24
          TabStop = False
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 10
        end
        object Edit4: TEdit
          Left = 5
          Top = 160
          Width = 75
          Height = 24
          TabOrder = 5
          OnChange = Edit4Change
        end
        object Edit5: TEdit
          Left = 113
          Top = 160
          Width = 75
          Height = 24
          TabOrder = 6
          OnChange = Edit4Change
        end
        object Edit6: TEdit
          Left = 222
          Top = 160
          Width = 80
          Height = 24
          TabStop = False
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 11
        end
        object Edit7: TEdit
          Left = 222
          Top = 215
          Width = 80
          Height = 24
          TabStop = False
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 12
        end
        object btnFlood: TButton
          Left = 0
          Top = 271
          Width = 238
          Height = 31
          Caption = 'User Flood Test: Send 1.000 requests'
          TabOrder = 9
          OnClick = btnFloodClick
        end
        object xFlood: TCheckBox
          Left = 113
          Top = 240
          Width = 95
          Height = 21
          TabStop = False
          Caption = 'Auto-Flood'
          TabOrder = 8
        end
        object xRepost: TCheckBox
          Left = 5
          Top = 240
          Width = 100
          Height = 21
          TabStop = False
          Caption = 'Auto-Repost'
          TabOrder = 7
          OnClick = xRepostClick
        end
        object btnConnect: TButton
          Left = 271
          Top = 12
          Width = 77
          Height = 31
          Caption = 'Connect'
          TabOrder = 2
          OnClick = btnConnectClick
        end
        object xAutoConnect: TCheckBox
          Left = 140
          Top = 6
          Width = 105
          Height = 21
          TabStop = False
          Caption = 'AutoConnect'
          TabOrder = 1
          OnClick = xAutoConnectClick
        end
        object xMultiThreaded: TCheckBox
          Left = 5
          Top = 6
          Width = 119
          Height = 21
          TabStop = False
          Caption = 'Multi-Threaded'
          TabOrder = 0
          OnClick = xMultiThreadedClick
        end
        object btnConnDisconn: TButton
          Left = 236
          Top = 271
          Width = 125
          Height = 31
          Caption = '10 x conn/disconn'
          TabOrder = 13
          OnClick = btnConnDisconnClick
        end
        object xCompress: TCheckBox
          Left = 140
          Top = 31
          Width = 105
          Height = 21
          Caption = 'Compression'
          TabOrder = 14
          OnClick = xCompressClick
        end
        object eCryptMode: TComboBox
          Left = 4
          Top = 28
          Width = 121
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          ItemIndex = 0
          TabOrder = 15
          TabStop = False
          Text = 'No Encryption'
          OnChange = xEncryptClick
          Items.Strings = (
            'No Encryption'
            'Basic'
            'ISAAC'
            'RSA + Basic'
            'RSA + ISAAC')
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Multi-connection Flooding'
        ImageIndex = 2
        object Label16: TLabel
          Left = 143
          Top = 272
          Width = 77
          Height = 16
          Caption = 'Connections:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 10
          Top = 37
          Width = 116
          Height = 16
          Caption = 'Connections to use:'
        end
        object Label18: TLabel
          Left = 15
          Top = 65
          Width = 114
          Height = 16
          Caption = 'Requests per loop:'
        end
        object lblFlood: TLabel
          Left = 241
          Top = 293
          Width = 7
          Height = 16
          Caption = '0'
        end
        object lblClients: TLabel
          Left = 226
          Top = 272
          Width = 16
          Height = 16
          Caption = '----'
        end
        object Label25: TLabel
          Left = 143
          Top = 293
          Width = 95
          Height = 16
          Caption = 'Requests done:'
        end
        object Label26: TLabel
          Left = 9
          Top = 98
          Width = 115
          Height = 16
          Alignment = taRightJustify
          Caption = 'Update graph after:'
        end
        object Label27: TLabel
          Left = 5
          Top = 6
          Width = 338
          Height = 16
          Caption = 'Open connections and flood Server with requests'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label28: TLabel
          Left = 266
          Top = 60
          Width = 77
          Height = 16
          Caption = 'Manual Test:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label31: TLabel
          Left = 143
          Top = 314
          Width = 90
          Height = 16
          Caption = 'Memory in use:'
        end
        object lblMemTotal: TLabel
          Left = 241
          Top = 314
          Width = 7
          Height = 16
          Caption = '0'
        end
        object Label32: TLabel
          Left = 212
          Top = 37
          Width = 82
          Height = 16
          Caption = 'Max Threads:'
        end
        object Label19: TLabel
          Left = 12
          Top = 272
          Width = 99
          Height = 16
          Caption = 'Auto-Flood Test:'
        end
        object eConCnt: TEdit
          Left = 138
          Top = 31
          Width = 60
          Height = 24
          TabOrder = 0
          Text = '300'
        end
        object eReqCnt: TEdit
          Left = 138
          Top = 62
          Width = 60
          Height = 24
          TabOrder = 1
          Text = '200'
        end
        object btnMultiFlood: TButton
          Left = 5
          Top = 289
          Width = 124
          Height = 36
          Caption = 'Start Flooding'
          Default = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 13
          OnClick = btnMultiFloodClick
        end
        object xReqAutoRepeat: TCheckBox
          Left = 10
          Top = 250
          Width = 164
          Height = 17
          Caption = 'Auto-Repeat Loops'
          TabOrder = 7
        end
        object eUpdCnt: TEdit
          Left = 138
          Top = 92
          Width = 40
          Height = 24
          TabOrder = 2
          Text = '5'
        end
        object xAutoDisconnect: TCheckBox
          Left = 10
          Top = 123
          Width = 203
          Height = 20
          Caption = 'Disconnect after each loop'
          TabOrder = 6
        end
        object xReqAutoConnect: TCheckBox
          Left = 10
          Top = 148
          Width = 149
          Height = 21
          Caption = 'Use Auto-Connect'
          TabOrder = 4
        end
        object btnMultiCreate: TButton
          Left = 241
          Top = 80
          Width = 115
          Height = 31
          Caption = 'Create'
          TabOrder = 8
          OnClick = btnMultiCreateClick
        end
        object btnMultiConnect: TButton
          Left = 241
          Top = 110
          Width = 115
          Height = 32
          Caption = 'Connect'
          Enabled = False
          TabOrder = 9
          OnClick = btnMultiConnectClick
        end
        object btnMultiSend: TButton
          Left = 241
          Top = 140
          Width = 115
          Height = 32
          Caption = 'Send requests'
          Enabled = False
          TabOrder = 10
          OnClick = btnMultiSendClick
        end
        object xReqMultiThread: TCheckBox
          Left = 10
          Top = 172
          Width = 154
          Height = 21
          Caption = 'Multi-Threaded mode'
          TabOrder = 3
        end
        object xCompress2: TCheckBox
          Left = 137
          Top = 222
          Width = 96
          Height = 20
          Caption = 'Compress'
          TabOrder = 5
        end
        object eThreads: TEdit
          Left = 305
          Top = 31
          Width = 51
          Height = 24
          TabOrder = 14
          Text = '64'
        end
        object xExtensiveTest: TCheckBox
          Left = 10
          Top = 197
          Width = 228
          Height = 20
          Caption = 'Extensive Test (complex structure)'
          TabOrder = 15
        end
        object btnMultiSkip: TButton
          Left = 241
          Top = 171
          Width = 115
          Height = 32
          Caption = 'Skip requests'
          Enabled = False
          TabOrder = 16
          OnClick = btnMultiSkipClick
        end
        object btnMultiDisconnect: TButton
          Left = 241
          Top = 202
          Width = 115
          Height = 32
          Caption = 'Disconnect'
          Enabled = False
          TabOrder = 11
          OnClick = btnMultiDisconnectClick
        end
        object btnMultiFree: TButton
          Left = 241
          Top = 233
          Width = 115
          Height = 32
          Caption = 'Free'
          Enabled = False
          TabOrder = 12
          OnClick = btnMultiFreeClick
        end
        object eCryptMode2: TComboBox
          Left = 8
          Top = 220
          Width = 121
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          ItemIndex = 0
          TabOrder = 17
          TabStop = False
          Text = 'No Encryption'
          Items.Strings = (
            'No Encryption'
            'Basic'
            'ISAAC'
            'RSA + Basic'
            'RSA + ISAAC')
        end
      end
    end
  end
  object RtcClient: TRtcHttpClient
    ServerAddr = 'localhost'
    ServerPort = '81'
    OnConnect = RtcClientConnect
    OnDisconnect = RtcClientDisconnect
    ReconnectOn.ConnectError = True
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    Left = 24
    Top = 329
  end
  object RtcClientModule1: TRtcClientModule
    AutoSyncEvents = True
    Client = RtcClient
    SecureKey = 'This is a test.'
    AutoSessions = True
    AutoRepost = 2
    ModuleHost = 'LOCALHOST'
    ModuleFileName = '/TEST'
    OnEncryptWrongKey = RtcClientModule1EncryptWrongKey
    OnResponseDone = RtcClientModule1ResponseDone
    OnResponseAbort = RtcClientModule1ResponseAbort
    Left = 96
    Top = 329
  end
  object RtcResult1: TRtcResult
    OnReturn = RtcResult1Return
    Left = 164
    Top = 305
  end
  object RtcResult2: TRtcResult
    OnReturn = RtcResult2Return
    Left = 216
    Top = 305
  end
  object RtcResult3: TRtcResult
    OnReturn = RtcResult3Return
    Left = 268
    Top = 305
  end
  object MultiResult: TRtcResult
    OnReturn = MultiResultReturn
    Left = 84
    Top = 355
  end
  object SendResult: TRtcResult
    OnReturn = SendResultReturn
    Left = 156
    Top = 356
  end
  object UpdateTimer: TTimer
    Interval = 500
    OnTimer = UpdateTimerTimer
    Left = 240
    Top = 358
  end
  object StopFloodTimer: TTimer
    Enabled = False
    OnTimer = StopFloodTimerTimer
    Left = 240
    Top = 415
  end
end
