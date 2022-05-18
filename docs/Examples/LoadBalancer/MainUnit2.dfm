object RtcLoadBalancerMainForm: TRtcLoadBalancerMainForm
  Left = 329
  Top = 112
  Width = 484
  Height = 706
  AutoSize = True
  Caption = 'RTC Load Balancer v2 - Single WebApp Demo'
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
  PixelsPerInch = 120
  TextHeight = 16
  object Label4: TLabel
    Left = 5
    Top = 39
    Width = 80
    Height = 16
    Caption = 'Listen on Port'
  end
  object Label7: TLabel
    Left = 5
    Top = 502
    Width = 69
    Height = 16
    Caption = 'LOG Folder'
  end
  object Label8: TLabel
    Left = 5
    Top = 394
    Width = 161
    Height = 16
    Caption = 'Application URI on Servers'
  end
  object Bevel2: TBevel
    Left = 0
    Top = 448
    Width = 454
    Height = 4
  end
  object Label9: TLabel
    Left = 384
    Top = 39
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Label12: TLabel
    Left = 384
    Top = 69
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Label14: TLabel
    Left = 379
    Top = 182
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Label16: TLabel
    Left = 379
    Top = 152
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Bevel3: TBevel
    Left = 148
    Top = 29
    Width = 302
    Height = 66
    Style = bsRaised
  end
  object Bevel4: TBevel
    Left = 153
    Top = 143
    Width = 292
    Height = 65
    Style = bsRaised
  end
  object Label3: TLabel
    Left = 226
    Top = 615
    Width = 51
    Height = 16
    Caption = 'Threads'
  end
  object Label5: TLabel
    Left = 5
    Top = 2
    Width = 150
    Height = 16
    Caption = 'Incoming connections'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Label10: TLabel
    Left = 5
    Top = 116
    Width = 150
    Height = 16
    Caption = 'Outgoing connections'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Bevel5: TBevel
    Left = 0
    Top = 103
    Width = 449
    Height = 4
  end
  object Label1: TLabel
    Left = 5
    Top = 256
    Width = 461
    Height = 16
    Caption = 
      'ServerAddr:Port MaxConnections MaxSessions SessionTimeout (0=unl' +
      'imited)'
  end
  object Label2: TLabel
    Left = 5
    Top = 236
    Width = 75
    Height = 16
    Caption = 'Server List'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object bConnect: TButton
    Left = 359
    Top = 605
    Width = 80
    Height = 41
    Caption = 'START'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 29
    OnClick = bConnectClick
  end
  object eFromPort: TEdit
    Left = 93
    Top = 34
    Width = 46
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    Text = '80'
  end
  object xServerMulti: TCheckBox
    Left = 238
    Top = 0
    Width = 119
    Height = 26
    Caption = 'Multi-Threaded'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object xServerBlocking: TCheckBox
    Left = 370
    Top = 0
    Width = 79
    Height = 26
    Caption = 'Blocking'
    TabOrder = 2
  end
  object xClientMulti: TCheckBox
    Left = 238
    Top = 113
    Width = 119
    Height = 26
    Caption = 'Multi-Threaded'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object xClientBlocking: TCheckBox
    Left = 370
    Top = 113
    Width = 79
    Height = 26
    Caption = 'Blocking'
    TabOrder = 11
  end
  object xChangeHost: TCheckBox
    Left = 5
    Top = 418
    Width = 104
    Height = 21
    Caption = 'Update Host'
    Checked = True
    State = cbChecked
    TabOrder = 20
  end
  object xChangeURLs: TCheckBox
    Left = 113
    Top = 418
    Width = 346
    Height = 21
    Caption = 'Change direct URLs on Web Pages with relative URIs'
    TabOrder = 21
  end
  object xForceHttp10: TCheckBox
    Left = 10
    Top = 152
    Width = 124
    Height = 21
    Caption = 'Force HTTP/1.0'
    TabOrder = 12
  end
  object xResponseBuffer: TCheckBox
    Left = 10
    Top = 182
    Width = 134
    Height = 21
    Caption = 'Buffer Responses'
    TabOrder = 13
  end
  object xDebugLog: TCheckBox
    Left = 5
    Top = 468
    Width = 100
    Height = 20
    Caption = 'Debug LOG'
    TabOrder = 22
  end
  object xRequestBuffer: TCheckBox
    Left = 10
    Top = 69
    Width = 129
    Height = 21
    Caption = 'Buffer Requests'
    TabOrder = 4
  end
  object eLogFolder: TEdit
    Left = 84
    Top = 497
    Width = 365
    Height = 24
    TabOrder = 26
    OnChange = eLogFolderChange
  end
  object cReqOrder: TRadioGroup
    Left = 5
    Top = 586
    Width = 208
    Height = 75
    Caption = 'Request Forwarding Order'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Standard'
      'Reverse'
      'Random'
      'Chance')
    TabOrder = 27
  end
  object xEventLog: TCheckBox
    Left = 108
    Top = 468
    Width = 95
    Height = 20
    Caption = 'Event LOG'
    TabOrder = 23
  end
  object eToURI: TEdit
    Left = 172
    Top = 389
    Width = 277
    Height = 24
    TabOrder = 19
    OnExit = eToURIExit
  end
  object xBuffLog: TCheckBox
    Left = 212
    Top = 468
    Width = 153
    Height = 20
    Caption = 'LOG Buffering (faster)'
    TabOrder = 24
    OnClick = xBuffLogClick
  end
  object bDumpLog: TButton
    Left = 369
    Top = 458
    Width = 80
    Height = 35
    Caption = 'Dump Log'
    TabOrder = 25
    OnClick = bDumpLogClick
  end
  object xRequestInTimeouts: TCheckBox
    Left = 157
    Top = 39
    Width = 154
    Height = 21
    Caption = 'Request IN Timeout'
    TabOrder = 5
  end
  object xResponseOutTimeout: TCheckBox
    Left = 157
    Top = 69
    Width = 174
    Height = 21
    Caption = 'Response OUT Timeout'
    TabOrder = 7
  end
  object xRequestOutTimeout: TCheckBox
    Left = 162
    Top = 152
    Width = 159
    Height = 21
    Caption = 'Request OUT Timeout'
    TabOrder = 14
  end
  object xResponseInTimeout: TCheckBox
    Left = 162
    Top = 182
    Width = 154
    Height = 21
    Caption = 'Response IN Timeout'
    TabOrder = 16
  end
  object eRequestInTime: TSpinEdit
    Left = 330
    Top = 34
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 6
    Value = 5
  end
  object eResponseOutTime: TSpinEdit
    Left = 330
    Top = 64
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 8
    Value = 5
  end
  object eResponseInTime: TSpinEdit
    Left = 325
    Top = 177
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 17
    Value = 5
  end
  object eRequestOutTime: TSpinEdit
    Left = 325
    Top = 148
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 15
    Value = 5
  end
  object eThrCount: TSpinEdit
    Left = 285
    Top = 615
    Width = 66
    Height = 26
    MaxValue = 2000
    MinValue = 1
    TabOrder = 28
    Value = 64
  end
  object xPostReturnBeforeResponseSent: TCheckBox
    Left = 10
    Top = 212
    Width = 429
    Height = 20
    Caption = 
      'PostReturn before ResponseSent (faster, but can result in higher' +
      ' load)'
    TabOrder = 18
    OnClick = xPostReturnBeforeResponseSentClick
  end
  object eServerList: TMemo
    Left = 5
    Top = 276
    Width = 444
    Height = 109
    Lines.Strings = (
      'localhost:81  100  0  600'
      'localhost:82  100  0  600')
    ScrollBars = ssVertical
    TabOrder = 30
  end
  object cBalanceType: TRadioGroup
    Left = 5
    Top = 532
    Width = 444
    Height = 50
    Caption = 'Server Load Balancing'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Session Balance'
      'Request Balance'
      'Round Robin')
    TabOrder = 31
  end
  object xServerIP6: TCheckBox
    Left = 166
    Top = 0
    Width = 59
    Height = 26
    Caption = 'IPv6'
    TabOrder = 0
  end
  object xClientIP6: TCheckBox
    Left = 166
    Top = 112
    Width = 59
    Height = 26
    Caption = 'IPv6'
    TabOrder = 9
  end
  object Server: TRtcHttpServer
    Left = 136
    Top = 240
  end
  object DataBalancer: TRtcLoadBalancer
    Server = Server
    CheckOrder = 10
    OnCheckRequestI = DataBalancerCheckRequestI
    OnPostNewRequestI = DataBalancerPostNewRequestI
    OnPostOldRequestI = DataBalancerPostOldRequestI
    OnPostReturn = DataBalancerPostReturn
    OnRequestBeginO = DataBalancerRequestBeginO
    OnRequestReceiveAbortI = DataBalancerRequestReceiveAbortI
    OnRequestReceivedI = DataBalancerRequestReceivedI
    OnRequestSendAbortO = DataBalancerRequestSendAbortO
    OnRequestSentO = DataBalancerRequestSentO
    OnResponseBeginO = DataBalancerResponseBeginO
    OnResponseReceiveAbortO = DataBalancerResponseReceiveAbortO
    OnResponseReceivedO = DataBalancerResponseReceivedO
    OnResponseSendAbortI = DataBalancerResponseSendAbortI
    OnResponseSentI = DataBalancerResponseSentI
    OnDebugLog = DataBalancerDebugLog
    PrepareConnection = DataBalancerPrepareConnection
    RequestBodyHasSessionID = DataBalancerRequestBodyHasSessionID
    RequestPrepareSessionID = DataBalancerRequestPrepareSessionID
    ResponseBodyHasSessionID = DataBalancerResponseBodyHasSessionID
    ResponsePrepareSessionID = DataBalancerResponsePrepareSessionID
    Left = 192
    Top = 240
  end
  object StatProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = StatProviderCheckRequest
    OnDataReceived = StatProviderDataReceived
    Left = 260
    Top = 240
  end
  object DumpProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = DumpProviderCheckRequest
    OnDataReceived = DumpProviderDataReceived
    Left = 328
    Top = 240
  end
end
