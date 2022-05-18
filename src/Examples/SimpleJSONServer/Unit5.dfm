object Form5: TForm5
  Left = 0
  Top = 0
  ActiveControl = bListen
  AutoSize = True
  BorderStyle = bsSingle
  Caption = 'Simple RTC JSON Server'
  ClientHeight = 249
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object bListen: TButton
    Left = 351
    Top = 40
    Width = 106
    Height = 73
    Caption = 'START'
    TabOrder = 0
    OnClick = bListenClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 167
    Width = 457
    Height = 82
    Lines.Strings = (
      'Server is NOT running.'
      'Configure all parameters and press START.')
    ReadOnly = True
    TabOrder = 1
  end
  object ConfigPanel: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 161
    TabOrder = 2
    object Label1: TLabel
      Left = 143
      Top = 5
      Width = 74
      Height = 16
      Caption = 'Max Threads'
    end
    object Label2: TLabel
      Left = 256
      Top = 5
      Width = 77
      Height = 16
      Caption = 'Listening Port'
    end
    object xFileName: TLabel
      Left = 7
      Top = 104
      Width = 103
      Height = 16
      Caption = 'Request FileName'
    end
    object Label3: TLabel
      Left = 7
      Top = 131
      Width = 93
      Height = 16
      Caption = 'Response String'
    end
    object xMultiThreaded: TCheckBox
      Left = 8
      Top = 6
      Width = 129
      Height = 17
      Caption = 'Multi-Threaded'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object ePoolSize: TEdit
      Left = 143
      Top = 27
      Width = 97
      Height = 24
      TabOrder = 1
      Text = '50'
    end
    object ePort: TEdit
      Left = 256
      Top = 27
      Width = 81
      Height = 24
      TabOrder = 2
      Text = '80'
    end
    object xJSON: TCheckBox
      Left = 8
      Top = 54
      Width = 329
      Height = 17
      Caption = 'Generate JSON Results from RTC Objects on-the-fly'
      TabOrder = 3
    end
    object xBlocking: TCheckBox
      Left = 8
      Top = 31
      Width = 129
      Height = 17
      Caption = 'Blocking WinSock'
      TabOrder = 4
    end
    object eFileName: TEdit
      Left = 116
      Top = 101
      Width = 221
      Height = 24
      TabOrder = 5
      Text = '/service/HelloWorld'
    end
    object eResponseText: TEdit
      Left = 116
      Top = 128
      Width = 221
      Height = 24
      TabOrder = 6
      Text = '[Hello World!]'
    end
    object xHeaders: TCheckBox
      Left = 7
      Top = 78
      Width = 329
      Height = 17
      Caption = 'Generate custom HTTP Headers for each Response'
      TabOrder = 7
    end
  end
  object RtcHttpServer1: TRtcHttpServer
    ServerPort = '81'
    OnException = RtcHttpServer1Exception
    OnListenStart = RtcHttpServer1ListenStart
    OnListenStop = RtcHttpServer1ListenStop
    OnListenLost = RtcHttpServer1ListenLost
    OnListenError = RtcHttpServer1ListenError
    OnRequestNotAccepted = RtcHttpServer1RequestNotAccepted
    OnInvalidRequest = RtcHttpServer1InvalidRequest
    Left = 280
    Top = 176
  end
  object RtcDataProvider1: TRtcDataProvider
    Server = RtcHttpServer1
    OnCheckRequest = RtcDataProvider1CheckRequest
    OnDataReceived = RtcDataProvider1DataReceived
    Left = 384
    Top = 176
  end
end
