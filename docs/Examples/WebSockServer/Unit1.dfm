object Form1: TForm1
  Left = 45
  Top = 127
  Width = 796
  Height = 440
  Caption = 'RTC Web Socket Server Test'
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
  PixelsPerInch = 120
  TextHeight = 16
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 581
    Height = 395
    Align = alClient
    Lines.Strings = (
      
        '1. Enter "Server Port" (Listening Port - defaults to 80 for HTTP' +
        ')'
      '2. Click "GO" to start the Server Listener'
      '3. When you see the message "Server Listening on port ...", '
      
        '    the Server is ready to accept connections and handle request' +
        's from Clients'
      ''
      
        '4. Open a Web Browser and/or Web Socket Client and connect to th' +
        'e Server'
      
        '5. Click any button on the right to send data to ALL connected C' +
        'lients'
      
        '6. Use the "Disconnect" button at anytime to disconnect all Clie' +
        'nts'
      ''
      '7. Click STOP to Stop the Server listener'
      ''
      '---------------')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 581
    Top = 0
    Width = 197
    Height = 395
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 8
      Width = 67
      Height = 16
      Caption = 'Server Port'
    end
    object Label4: TLabel
      Left = 16
      Top = 352
      Width = 27
      Height = 16
      Caption = 'LOG'
    end
    object btnSendText: TButton
      Left = 8
      Top = 61
      Width = 90
      Height = 33
      Caption = 'Text'
      TabOrder = 0
      OnClick = btnSendTextClick
    end
    object btnSendBinary: TButton
      Left = 8
      Top = 93
      Width = 90
      Height = 33
      Caption = 'Binary'
      TabOrder = 1
      OnClick = btnSendBinaryClick
    end
    object btnSendPing: TButton
      Left = 100
      Top = 61
      Width = 90
      Height = 33
      Caption = 'Ping'
      TabOrder = 2
      OnClick = btnSendPingClick
    end
    object btnSendPong: TButton
      Left = 100
      Top = 93
      Width = 90
      Height = 33
      Caption = 'Pong'
      TabOrder = 3
      OnClick = btnSendPongClick
    end
    object btnSendTxtOne: TButton
      Left = 8
      Top = 133
      Width = 90
      Height = 33
      Caption = 'TXT one'
      TabOrder = 4
      OnClick = btnSendTxtOneClick
    end
    object btnSendExeOne: TButton
      Left = 8
      Top = 201
      Width = 90
      Height = 33
      Caption = 'EXE one'
      TabOrder = 5
      OnClick = btnSendExeOneClick
    end
    object btnClose: TButton
      Left = 8
      Top = 273
      Width = 90
      Height = 33
      Caption = 'Close'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnClick = btnCloseClick
    end
    object btnEmptyQ: TButton
      Left = 8
      Top = 309
      Width = 90
      Height = 33
      Caption = 'Empty Q'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      OnClick = btnEmptyQClick
    end
    object btnDisconnect: TButton
      Left = 100
      Top = 273
      Width = 90
      Height = 33
      Caption = 'Disconnect'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = btnDisconnectClick
    end
    object btnSendTxtMulti: TButton
      Left = 100
      Top = 133
      Width = 90
      Height = 33
      Caption = 'TXT multi'
      TabOrder = 6
      OnClick = btnSendTxtMultiClick
    end
    object btnSendExeMulti: TButton
      Left = 100
      Top = 201
      Width = 90
      Height = 33
      Caption = 'EXE multi'
      TabOrder = 7
      OnClick = btnSendExeMultiClick
    end
    object btnSendTxtRam: TButton
      Left = 8
      Top = 165
      Width = 90
      Height = 33
      Caption = 'TXT RAM'
      TabOrder = 8
      OnClick = btnSendTxtRamClick
    end
    object btnSendExeRam: TButton
      Left = 8
      Top = 233
      Width = 90
      Height = 33
      Caption = 'EXE RAM'
      TabOrder = 9
      OnClick = btnSendExeRamClick
    end
    object btnMemoClear: TButton
      Left = 100
      Top = 309
      Width = 90
      Height = 33
      Caption = 'Clear LOG'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      OnClick = btnMemoClearClick
    end
    object btnSendTxtCompr: TButton
      Left = 100
      Top = 165
      Width = 90
      Height = 33
      Caption = 'TXT mime'
      TabOrder = 14
      OnClick = btnSendTxtComprClick
    end
    object btnSendExeCompr: TButton
      Left = 100
      Top = 233
      Width = 90
      Height = 33
      Caption = 'EXE mime'
      TabOrder = 15
      OnClick = btnSendExeComprClick
    end
    object ePort: TEdit
      Left = 12
      Top = 28
      Width = 77
      Height = 24
      TabOrder = 16
      Text = '80'
    end
    object btnListen: TButton
      Left = 100
      Top = 4
      Width = 91
      Height = 49
      Caption = 'GO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 17
      OnClick = btnListenClick
    end
    object cbLOG: TComboBox
      Left = 52
      Top = 348
      Width = 133
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      ItemIndex = 2
      TabOrder = 18
      Text = 'General'
      OnChange = cbLOGChange
      Items.Strings = (
        'None'
        'Basic'
        'General'
        'Detailed'
        'Full Debug')
    end
  end
  object Server: TRtcHttpServer
    MultiThreaded = True
    ServerPort = '80'
    OnListenStart = ServerListenStart
    OnListenStop = ServerListenStop
    OnListenError = ServerListenError
    Left = 492
    Top = 16
  end
  object SockProv: TRtcDataProvider
    Server = Server
    OnCheckRequest = SockProvCheckRequest
    OnDataReceived = AnyProvDataReceived
    OnResponseDone = AnyProvResponseDone
    OnDisconnect = AnyProvDisconnect
    OnWSConnect = AnyProvWSConnect
    OnWSDataReceived = SockProvWSDataReceived
    OnWSDataOut = AnyProvWSDataOut
    OnWSDataIn = AnyProvWSDataIn
    OnWSDataSent = AnyProvWSDataSent
    OnWSDisconnect = AnyProvWSDisconnect
    Left = 492
    Top = 76
  end
  object EchoProv: TRtcDataProvider
    Server = Server
    OnCheckRequest = EchoProvCheckRequest
    OnDataReceived = AnyProvDataReceived
    OnResponseDone = AnyProvResponseDone
    OnDisconnect = AnyProvDisconnect
    OnWSConnect = AnyProvWSConnect
    OnWSDataReceived = EchoProvWSDataReceived
    OnWSDataOut = AnyProvWSDataOut
    OnWSDataIn = AnyProvWSDataIn
    OnWSDataSent = AnyProvWSDataSent
    OnWSDisconnect = AnyProvWSDisconnect
    Left = 492
    Top = 144
  end
end
