object Form1: TForm1
  Left = 31
  Top = 138
  Width = 815
  Height = 511
  Caption = 'RTC WebServer Stress Tool v2.0'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object myPanel: TPanel
    Left = 0
    Top = 233
    Width = 797
    Height = 201
    Align = alClient
    BorderWidth = 5
    TabOrder = 0
    object myBox: TPaintBox
      Left = 6
      Top = 6
      Width = 785
      Height = 189
      Align = alClient
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 797
    Height = 233
    Align = alTop
    BorderWidth = 4
    TabOrder = 1
    object Label16: TLabel
      Left = 414
      Top = 138
      Width = 117
      Height = 16
      Alignment = taRightJustify
      Caption = 'Connections Active:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label17: TLabel
      Left = 399
      Top = 45
      Width = 105
      Height = 16
      Caption = 'Connection count:'
    end
    object Label18: TLabel
      Left = 399
      Top = 73
      Width = 114
      Height = 16
      Caption = 'Requests per loop:'
    end
    object lblFlood: TLabel
      Left = 537
      Top = 160
      Width = 7
      Height = 16
      Caption = '0'
    end
    object lblClients: TLabel
      Left = 542
      Top = 138
      Width = 16
      Height = 16
      Caption = '----'
    end
    object Label25: TLabel
      Left = 402
      Top = 160
      Width = 130
      Height = 16
      Alignment = taRightJustify
      Caption = 'Requests Completed:'
    end
    object Label26: TLabel
      Left = 400
      Top = 107
      Width = 108
      Height = 16
      Alignment = taRightJustify
      Caption = 'Update stats after:'
    end
    object Label32: TLabel
      Left = 399
      Top = 15
      Width = 110
      Height = 16
      Caption = 'Max Thread count:'
    end
    object Label11: TLabel
      Left = 10
      Top = 68
      Width = 348
      Height = 16
      Caption = 'Enter a list of requests you want to send (1 request per line):'
    end
    object Label12: TLabel
      Left = 103
      Top = 12
      Width = 94
      Height = 16
      Caption = 'Server Address'
    end
    object Label13: TLabel
      Left = 315
      Top = 12
      Width = 24
      Height = 16
      Caption = 'Port'
    end
    object Bevel1: TBevel
      Left = 384
      Top = 6
      Width = 6
      Height = 211
    end
    object Label1: TLabel
      Left = 429
      Top = 182
      Width = 102
      Height = 16
      Alignment = taRightJustify
      Caption = 'Requests Failed:'
    end
    object lblRepost: TLabel
      Left = 537
      Top = 182
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label2: TLabel
      Left = 598
      Top = 138
      Width = 66
      Height = 16
      Alignment = taRightJustify
      Caption = 'Max Loops'
    end
    object Label3: TLabel
      Left = 410
      Top = 204
      Width = 121
      Height = 16
      Alignment = taRightJustify
      Caption = 'Avg. Requests/Sec:'
    end
    object lblReqSec: TLabel
      Left = 537
      Top = 204
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label4: TLabel
      Left = 670
      Top = 160
      Width = 63
      Height = 16
      Caption = '(0=no limit)'
    end
    object Label5: TLabel
      Left = 593
      Top = 108
      Width = 71
      Height = 16
      Caption = 'Auto Conn +'
    end
    object eConCnt: TEdit
      Left = 517
      Top = 39
      Width = 60
      Height = 24
      TabOrder = 6
      Text = '1'
    end
    object eReqCnt: TEdit
      Left = 517
      Top = 70
      Width = 60
      Height = 24
      TabOrder = 7
      Text = '20000'
    end
    object xReqAutoRepeat: TCheckBox
      Left = 593
      Top = 84
      Width = 163
      Height = 18
      Caption = 'Auto-Repeat Loops'
      Checked = True
      State = cbChecked
      TabOrder = 11
    end
    object eUpdCnt: TEdit
      Left = 517
      Top = 100
      Width = 60
      Height = 24
      TabOrder = 8
      Text = '10000'
    end
    object xAutoDisconnect: TCheckBox
      Left = 593
      Top = 60
      Width = 158
      Height = 19
      Caption = 'Disconnect after loop'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object xReqAutoConnect: TCheckBox
      Left = 593
      Top = 33
      Width = 149
      Height = 21
      Caption = 'Use Auto-Connect'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
    object eThreads: TEdit
      Left = 517
      Top = 9
      Width = 60
      Height = 24
      TabOrder = 5
      Text = '50'
    end
    object eServer: TEdit
      Left = 103
      Top = 31
      Width = 203
      Height = 24
      TabOrder = 2
      Text = 'localhost'
      OnChange = eServerChange
    end
    object xProxy: TCheckBox
      Left = 10
      Top = 12
      Width = 85
      Height = 20
      TabStop = False
      Alignment = taLeftJustify
      Caption = 'use Proxy'
      TabOrder = 0
      OnClick = xProxyClick
    end
    object ePort: TEdit
      Left = 315
      Top = 31
      Width = 60
      Height = 24
      TabOrder = 3
      Text = '80'
      OnChange = eServerChange
    end
    object xSSL: TCheckBox
      Left = 10
      Top = 37
      Width = 85
      Height = 20
      TabStop = False
      Alignment = taLeftJustify
      Caption = 'use SSL'
      TabOrder = 1
      OnClick = xSSLClick
    end
    object Memo1: TMemo
      Left = 10
      Top = 92
      Width = 365
      Height = 125
      Lines.Strings = (
        '/service/HelloWorld')
      TabOrder = 4
    end
    object btnMultiFlood: TButton
      Left = 670
      Top = 180
      Width = 86
      Height = 47
      Caption = 'START'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 12
      OnClick = btnMultiFloodClick
    end
    object xBlockingWinSock: TCheckBox
      Left = 593
      Top = 6
      Width = 181
      Height = 21
      Caption = 'Blocking WinSock'
      TabOrder = 13
    end
    object eMaxLoops: TEdit
      Left = 670
      Top = 135
      Width = 59
      Height = 24
      TabOrder = 14
      Text = '25'
    end
    object eConnPlus: TEdit
      Left = 670
      Top = 105
      Width = 59
      Height = 24
      TabOrder = 15
      Text = '1'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 434
    Width = 797
    Height = 32
    Cursor = crHandPoint
    Align = alBottom
    Caption = 
      'Copyright (c) Danijel Tkalcec -> Built using RealThinClient SDK ' +
      '-> http://www.realthinclient.com'
    TabOrder = 2
    OnClick = Panel2Click
  end
  object RtcClient: TRtcHttpClient
    MultiThreaded = True
    Timeout.AfterConnecting = 120
    Timeout.AfterConnect = 60
    ServerAddr = 'localhost'
    ServerPort = '80'
    ReconnectOn.ConnectError = True
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    AutoConnect = True
    MaxHeaderSize = 16000
    Left = 20
    Top = 195
  end
  object DataRequest: TRtcDataRequest
    OnBeginRequest = DataRequestBeginRequest
    OnResponseDone = DataRequestResponseDone
    OnRepostCheck = DataRequestRepostCheck
    OnDataReceived = DataRequestDataReceived
    Left = 92
    Top = 196
  end
end
