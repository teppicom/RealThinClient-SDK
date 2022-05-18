object RtcFileClient: TRtcFileClient
  Left = 227
  Top = 150
  Width = 956
  Height = 545
  Caption = 'RTC File Client Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 938
    Height = 500
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 514
      Top = 0
      Height = 500
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 514
      Height = 500
      Align = alLeft
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 0
      object Bevel1: TBevel
        Left = 211
        Top = 5
        Width = 198
        Height = 80
        Shape = bsFrame
      end
      object Label7: TLabel
        Left = 4
        Top = 334
        Width = 45
        Height = 16
        Caption = 'Method'
      end
      object Label8: TLabel
        Left = 92
        Top = 334
        Width = 231
        Height = 16
        Caption = 'File Name, without "http://serveraddr/"'
      end
      object Label10: TLabel
        Left = 83
        Top = 358
        Width = 8
        Height = 16
        Caption = '/'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label11: TLabel
        Left = 4
        Top = 383
        Width = 108
        Height = 16
        Caption = 'Query parameters'
      end
      object Label9: TLabel
        Left = 4
        Top = 303
        Width = 71
        Height = 16
        Caption = 'Host Name:'
      end
      object Label3: TLabel
        Left = 4
        Top = 201
        Width = 70
        Height = 16
        Caption = 'Server Port:'
      end
      object Label2: TLabel
        Left = 92
        Top = 201
        Width = 97
        Height = 16
        Caption = 'Server Address:'
      end
      object Label1: TLabel
        Left = 4
        Top = 278
        Width = 74
        Height = 16
        Caption = 'REQUEST'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label12: TLabel
        Left = 4
        Top = 475
        Width = 108
        Height = 16
        Caption = 'Total bytes in+out:'
      end
      object lblDataInOut: TLabel
        Left = 133
        Top = 476
        Width = 7
        Height = 16
        Caption = '0'
      end
      object Label13: TLabel
        Left = 4
        Top = 92
        Width = 376
        Height = 16
        Caption = 
          'Proxy Address, with "http://" and port number (empty for default' +
          ')'
      end
      object Label14: TLabel
        Left = 4
        Top = 146
        Width = 100
        Height = 16
        Caption = 'Proxy Username'
      end
      object Label15: TLabel
        Left = 201
        Top = 146
        Width = 97
        Height = 16
        Caption = 'Proxy Password'
      end
      object lblStatus: TLabel
        Left = 4
        Top = 256
        Width = 90
        Height = 16
        Caption = 'Not connected.'
      end
      object Label5: TLabel
        Left = 420
        Top = 74
        Width = 68
        Height = 16
        Caption = 'Reconnect:'
      end
      object eReqMethod: TEdit
        Left = 4
        Top = 352
        Width = 70
        Height = 24
        TabOrder = 10
        Text = 'GET'
      end
      object eReqFileName: TEdit
        Left = 92
        Top = 352
        Width = 307
        Height = 24
        TabOrder = 11
        OnChange = eReqFileNameChange
      end
      object eReqQuery: TEdit
        Left = 4
        Top = 401
        Width = 395
        Height = 24
        TabOrder = 12
      end
      object xSaveToFile: TCheckBox
        Left = 4
        Top = 438
        Width = 100
        Height = 26
        Caption = 'Save to file:'
        TabOrder = 13
      end
      object eFileName: TEdit
        Left = 102
        Top = 438
        Width = 253
        Height = 24
        TabOrder = 14
        Text = 'download\index.htm'
      end
      object btnSaveAs: TButton
        Left = 369
        Top = 438
        Width = 31
        Height = 26
        Caption = '...'
        TabOrder = 15
        OnClick = btnSaveAsClick
      end
      object eReqHost: TEdit
        Left = 92
        Top = 302
        Width = 307
        Height = 24
        TabStop = False
        TabOrder = 9
        Text = 'www.realthinclient.com'
      end
      object ePort: TEdit
        Left = 4
        Top = 222
        Width = 75
        Height = 24
        TabStop = False
        TabOrder = 6
        Text = '80'
        OnChange = ePortChange
      end
      object eAddr: TEdit
        Left = 92
        Top = 222
        Width = 307
        Height = 24
        TabOrder = 7
        Text = 'www.realthinclient.com'
        OnChange = eAddrChange
      end
      object xUseProxy: TCheckBox
        Left = 137
        Top = 36
        Width = 60
        Height = 21
        TabStop = False
        Alignment = taLeftJustify
        Caption = 'Proxy'
        TabOrder = 0
        OnClick = xUseProxyClick
      end
      object xThreads: TCheckBox
        Left = 4
        Top = 36
        Width = 124
        Height = 21
        TabStop = False
        Caption = 'Multi-Threaded'
        TabOrder = 16
        OnClick = xThreadsClick
      end
      object xUseSSL: TCheckBox
        Left = 142
        Top = 11
        Width = 55
        Height = 21
        TabStop = False
        Alignment = taLeftJustify
        Caption = 'SSL'
        TabOrder = 2
        OnClick = xUseSSLClick
      end
      object xAutoConnect: TCheckBox
        Left = 4
        Top = 11
        Width = 109
        Height = 21
        TabStop = False
        Caption = 'Auto-Connect'
        Checked = True
        State = cbChecked
        TabOrder = 17
        OnClick = xAutoConnectClick
      end
      object xWinHTTP: TCheckBox
        Left = 107
        Top = 60
        Width = 90
        Height = 21
        TabStop = False
        Alignment = taLeftJustify
        Caption = 'WinHTTP'
        TabOrder = 1
        OnClick = xWinHTTPClick
      end
      object eProxyAddr: TEdit
        Left = 4
        Top = 113
        Width = 395
        Height = 24
        TabStop = False
        TabOrder = 3
        OnChange = eProxyAddrChange
      end
      object eProxyUsername: TEdit
        Left = 4
        Top = 167
        Width = 183
        Height = 24
        TabStop = False
        TabOrder = 4
        OnChange = eProxyUsernameChange
      end
      object eProxyPassword: TEdit
        Left = 201
        Top = 167
        Width = 198
        Height = 24
        TabStop = False
        TabOrder = 5
        OnChange = eProxyPasswordChange
      end
      object xBlocking: TCheckBox
        Left = 4
        Top = 60
        Width = 85
        Height = 21
        TabStop = False
        Caption = 'Blocking'
        TabOrder = 18
        OnClick = xBlockingClick
      end
      object xCryptPlugin: TCheckBox
        Left = 220
        Top = 11
        Width = 184
        Height = 21
        Caption = 'CryptPlugin (dummy)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 19
        OnClick = xCryptPluginClick
      end
      object xTrustServer: TCheckBox
        Left = 220
        Top = 34
        Width = 174
        Height = 21
        Caption = 'Trust anything received'
        TabOrder = 20
        OnClick = xTrustServerClick
      end
      object xAllowExpired: TCheckBox
        Left = 220
        Top = 59
        Width = 179
        Height = 21
        Caption = 'Allow Expired Certificates'
        TabOrder = 21
        OnClick = xAllowExpiredClick
      end
      object xUseHttp10: TCheckBox
        Left = 92
        Top = 276
        Width = 223
        Height = 21
        Caption = 'Use the old HTTP/1.0 protocol?'
        TabOrder = 22
      end
      object btnConnect: TButton
        Left = 420
        Top = 6
        Width = 90
        Height = 32
        Caption = 'Connect'
        TabOrder = 23
        TabStop = False
        OnClick = btnConnectClick
      end
      object btnDisconnect: TButton
        Left = 420
        Top = 37
        Width = 90
        Height = 32
        Caption = 'Disconnect'
        Enabled = False
        TabOrder = 24
        TabStop = False
        OnClick = btnDisconnectClick
      end
      object btnPost: TButton
        Left = 420
        Top = 178
        Width = 90
        Height = 31
        Caption = 'POST'
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 25
        OnClick = btnPostClick
      end
      object btn100Post: TButton
        Left = 420
        Top = 210
        Width = 90
        Height = 31
        Caption = '100 x POST'
        TabOrder = 26
        OnClick = btn100PostClick
      end
      object btnCancelAll: TButton
        Left = 420
        Top = 244
        Width = 90
        Height = 30
        Caption = 'SKIP ALL'
        TabOrder = 27
        OnClick = btnCancelAllClick
      end
      object xReconError: TCheckBox
        Left = 425
        Top = 92
        Width = 80
        Height = 21
        Caption = 'on Error'
        TabOrder = 28
        OnClick = xReconErrorClick
      end
      object xReconFail: TCheckBox
        Left = 425
        Top = 113
        Width = 80
        Height = 20
        Caption = 'on Fail'
        Checked = True
        State = cbChecked
        TabOrder = 29
        OnClick = xReconFailClick
      end
      object xReconLost: TCheckBox
        Left = 425
        Top = 133
        Width = 80
        Height = 21
        Caption = 'on Lost'
        Checked = True
        State = cbChecked
        TabOrder = 30
        OnClick = xReconLostClick
      end
      object xShowWarning: TCheckBox
        Left = 425
        Top = 154
        Width = 85
        Height = 21
        Caption = '>5? Stop!'
        Checked = True
        State = cbChecked
        TabOrder = 31
      end
      object xIPv6: TCheckBox
        Left = 296
        Top = 200
        Width = 61
        Height = 17
        Caption = 'IPv6'
        TabOrder = 8
        OnClick = xIPv6Click
      end
      object xDefIP: TCheckBox
        Left = 204
        Top = 200
        Width = 77
        Height = 17
        Caption = 'Def.IPv'
        TabOrder = 32
        OnClick = xIPv6Click
      end
    end
    object Panel6: TPanel
      Left = 517
      Top = 0
      Width = 421
      Height = 500
      Align = alClient
      TabOrder = 1
      object Splitter2: TSplitter
        Left = 1
        Top = 94
        Width = 419
        Height = 3
        Cursor = crVSplit
        Align = alTop
      end
      object Panel7: TPanel
        Left = 1
        Top = 1
        Width = 419
        Height = 93
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Panel9: TPanel
          Left = 0
          Top = 0
          Width = 419
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label4: TLabel
            Left = 10
            Top = 6
            Width = 84
            Height = 16
            Caption = 'RESPONSE'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblCount: TLabel
            Left = 236
            Top = 6
            Width = 69
            Height = 16
            Caption = '0 Received'
          end
          object lblRetry: TLabel
            Left = 118
            Top = 6
            Width = 61
            Height = 16
            Caption = '0 Reposts'
          end
        end
        object Panel10: TPanel
          Left = 0
          Top = 26
          Width = 419
          Height = 67
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 2
          TabOrder = 1
          object eResponseHeader: TMemo
            Left = 2
            Top = 2
            Width = 415
            Height = 63
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
      end
      object Panel8: TPanel
        Left = 1
        Top = 97
        Width = 419
        Height = 402
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Panel11: TPanel
          Left = 0
          Top = 0
          Width = 419
          Height = 53
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblTime: TLabel
            Left = 169
            Top = 6
            Width = 105
            Height = 16
            Caption = 'Time: 0 < 0 < 0 ms'
          end
          object Label6: TLabel
            Left = 10
            Top = 6
            Width = 61
            Height = 16
            Caption = 'Content in:'
          end
          object lblSpeed: TLabel
            Left = 374
            Top = 6
            Width = 40
            Height = 16
            Alignment = taRightJustify
            Caption = '0 KB/s'
          end
          object lblBytes: TLabel
            Left = 79
            Top = 6
            Width = 43
            Height = 16
            Caption = '0 bytes'
          end
          object pBar: TProgressBar
            Left = 10
            Top = 31
            Width = 508
            Height = 19
            TabOrder = 0
          end
        end
        object Panel12: TPanel
          Left = 0
          Top = 53
          Width = 419
          Height = 349
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 2
          TabOrder = 1
          object eResponseBody: TMemo
            Left = 2
            Top = 2
            Width = 415
            Height = 345
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
      end
    end
  end
  object Client: TRtcHttpClient
    ServerAddr = 'www.realthinclient.com'
    ServerPort = '80'
    OnConnect = ClientConnect
    OnDisconnect = ClientDisconnect
    OnException = ClientException
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    OnConnectFail = ClientConnectFail
    OnConnectError = ClientConnectError
    AutoConnect = True
    OnDataOut = ClientDataOut
    OnDataIn = ClientDataIn
    Left = 204
    Top = 248
  end
  object DataRequest: TRtcDataRequest
    AutoSyncEvents = True
    Client = Client
    OnBeginRequest = DataRequestBeginRequest
    OnRepostCheck = DataRequestRepostCheck
    OnResponseAbort = DataRequestResponseAbort
    OnDataReceived = DataRequestDataReceived
    Left = 260
    Top = 248
  end
  object SaveDialog1: TSaveDialog
    Left = 361
    Top = 234
  end
end
