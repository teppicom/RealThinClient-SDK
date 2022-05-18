object RtcRouterMainForm: TRtcRouterMainForm
  Left = 365
  Top = 179
  Width = 472
  Height = 593
  AutoSize = True
  Caption = 'RTC Router'
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
    Left = 5
    Top = 118
    Width = 101
    Height = 16
    Caption = 'Outgoig Address'
  end
  object Label2: TLabel
    Left = 5
    Top = 148
    Width = 81
    Height = 16
    Caption = 'Outgoing Port'
  end
  object Label4: TLabel
    Left = 5
    Top = 5
    Width = 92
    Height = 16
    Caption = 'Incomming Port'
  end
  object Label7: TLabel
    Left = 5
    Top = 413
    Width = 69
    Height = 16
    Caption = 'LOG Folder'
  end
  object Label8: TLabel
    Left = 5
    Top = 285
    Width = 106
    Height = 16
    Caption = 'Outgoing root URI'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 103
    Width = 454
    Height = 4
  end
  object Bevel2: TBevel
    Left = 0
    Top = 364
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
    Top = 212
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Label16: TLabel
    Left = 379
    Top = 182
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
    Top = 172
    Width = 292
    Height = 65
    Style = bsRaised
  end
  object Label6: TLabel
    Left = 10
    Top = 506
    Width = 105
    Height = 16
    Caption = 'Max. Connections'
  end
  object Label3: TLabel
    Left = 213
    Top = 518
    Width = 82
    Height = 16
    Caption = 'Max. Threads'
  end
  object Bevel5: TBevel
    Left = 0
    Top = 271
    Width = 449
    Height = 3
  end
  object Label5: TLabel
    Left = 46
    Top = 526
    Width = 69
    Height = 16
    Caption = '(0 = no limit)'
  end
  object eToAddr: TEdit
    Left = 118
    Top = 113
    Width = 331
    Height = 24
    TabOrder = 9
    Text = 'www.realthinclient.com'
    OnChange = eToAddrChange
  end
  object eToPort: TEdit
    Left = 118
    Top = 143
    Width = 55
    Height = 24
    TabOrder = 10
    Text = '80'
  end
  object bConnect: TButton
    Left = 369
    Top = 507
    Width = 80
    Height = 41
    Caption = 'START'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 33
    OnClick = bConnectClick
  end
  object eFromPort: TEdit
    Left = 118
    Top = 0
    Width = 55
    Height = 24
    TabOrder = 0
    Text = '80'
  end
  object xServerMulti: TCheckBox
    Left = 246
    Top = 0
    Width = 119
    Height = 26
    Caption = 'Multi-Threaded'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object xServerBlocking: TCheckBox
    Left = 370
    Top = 0
    Width = 79
    Height = 26
    Caption = 'Blocking'
    TabOrder = 3
  end
  object xClientMulti: TCheckBox
    Left = 246
    Top = 143
    Width = 119
    Height = 25
    Caption = 'Multi-Threaded'
    Checked = True
    State = cbChecked
    TabOrder = 12
  end
  object xClientBlocking: TCheckBox
    Left = 370
    Top = 143
    Width = 79
    Height = 25
    Caption = 'Blocking'
    TabOrder = 13
  end
  object xChangeHost: TCheckBox
    Left = 5
    Top = 310
    Width = 109
    Height = 26
    Caption = 'Change Host'
    Checked = True
    State = cbChecked
    TabOrder = 27
  end
  object eToHost: TEdit
    Left = 118
    Top = 310
    Width = 331
    Height = 24
    TabOrder = 28
    Text = 'www.realthinclient.com'
  end
  object xChangeURLs: TCheckBox
    Left = 5
    Top = 340
    Width = 395
    Height = 20
    Caption = 'Try to replace direct URLs on HTML Pages with relative URIs'
    Checked = True
    State = cbChecked
    TabOrder = 29
  end
  object xForceHttp10: TCheckBox
    Left = 5
    Top = 182
    Width = 124
    Height = 21
    Caption = 'Force HTTP/1.0'
    TabOrder = 14
  end
  object xResponseBuffer: TCheckBox
    Left = 5
    Top = 212
    Width = 134
    Height = 20
    Caption = 'Buffer Responses'
    TabOrder = 15
  end
  object xDebugLog: TCheckBox
    Left = 5
    Top = 379
    Width = 100
    Height = 21
    Caption = 'Debug LOG'
    TabOrder = 30
  end
  object xRequestBuffer: TCheckBox
    Left = 15
    Top = 54
    Width = 129
    Height = 21
    Caption = 'Buffer Requests'
    TabOrder = 4
  end
  object eLogFolder: TEdit
    Left = 84
    Top = 408
    Width = 365
    Height = 24
    TabOrder = 19
    OnChange = eLogFolderChange
  end
  object cReqOrder: TRadioGroup
    Left = 10
    Top = 448
    Width = 439
    Height = 55
    Caption = 'Request Forwarding Order'
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'Standard'
      'Reverse'
      'Random'
      'Chance')
    TabOrder = 20
  end
  object xEventLog: TCheckBox
    Left = 108
    Top = 379
    Width = 95
    Height = 21
    Caption = 'Event LOG'
    TabOrder = 16
  end
  object eToURI: TEdit
    Left = 118
    Top = 280
    Width = 331
    Height = 24
    TabOrder = 26
    OnExit = eToURIExit
  end
  object xBuffLog: TCheckBox
    Left = 212
    Top = 379
    Width = 153
    Height = 21
    Caption = 'LOG Buffering (faster)'
    TabOrder = 17
    OnClick = xBuffLogClick
  end
  object bDumpLog: TButton
    Left = 369
    Top = 369
    Width = 80
    Height = 36
    Caption = 'Dump Log'
    TabOrder = 18
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
    Top = 182
    Width = 159
    Height = 21
    Caption = 'Request OUT Timeout'
    TabOrder = 22
  end
  object xResponseInTimeout: TCheckBox
    Left = 162
    Top = 212
    Width = 154
    Height = 20
    Caption = 'Response IN Timeout'
    TabOrder = 24
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
    Top = 207
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 25
    Value = 5
  end
  object eRequestOutTime: TSpinEdit
    Left = 325
    Top = 177
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 23
    Value = 5
  end
  object eConCount: TSpinEdit
    Left = 124
    Top = 513
    Width = 80
    Height = 26
    MaxValue = 30000
    MinValue = 0
    TabOrder = 31
    Value = 0
  end
  object eThrCount: TSpinEdit
    Left = 300
    Top = 513
    Width = 61
    Height = 26
    MaxValue = 2000
    MinValue = 1
    TabOrder = 32
    Value = 64
  end
  object xPostReturnBeforeResponseSent: TCheckBox
    Left = 5
    Top = 241
    Width = 429
    Height = 21
    Caption = 
      'PostReturn before ResponseSent (faster, but can result in higher' +
      ' load)'
    TabOrder = 21
    OnClick = xPostReturnBeforeResponseSentClick
  end
  object xServerIP6: TCheckBox
    Left = 183
    Top = 2
    Width = 58
    Height = 21
    Caption = 'IPv6'
    TabOrder = 1
  end
  object xClientIP6: TCheckBox
    Left = 179
    Top = 146
    Width = 58
    Height = 21
    Caption = 'IPv6'
    TabOrder = 11
  end
  object Server: TRtcHttpServer
    ServerPort = '80'
    Left = 136
    Top = 404
  end
  object DataRouter: TRtcDataRouter
    Server = Server
    CheckOrder = 10
    OnCheckRequestI = DataRouterCheckRequest
    OnPostNewRequestI = DataRouterPostNewRequest
    OnPostOldRequestI = DataRouterPostOldRequest
    OnQueuedRequestI = DataRouterQueuedRequest
    OnPostReturn = DataRouterPostReturn
    OnRequestBeginO = DataRouterRequestBegin
    OnRequestReceiveAbortI = DataRouterRequestReceiveAbort
    OnRequestReceivedI = DataRouterRequestReceived
    OnRequestSendAbortO = DataRouterRequestSendAbort
    OnRequestSentO = DataRouterRequestSent
    OnResponseBeginO = DataRouterResponseBegin
    OnResponseReceiveAbortO = DataRouterResponseReceiveAbort
    OnResponseReceivedO = DataRouterResponseReceived
    OnResponseSendAbortI = DataRouterResponseSendAbort
    OnResponseSentI = DataRouterResponseSent
    OnDebugLog = DataRouterDebugLog
    Left = 216
    Top = 412
  end
  object StatProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = StatProviderCheckRequest
    OnDataReceived = StatProviderDataReceived
    Left = 292
    Top = 412
  end
  object DumpProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = DumpProviderCheckRequest
    OnDataReceived = DumpProviderDataReceived
    Left = 380
    Top = 412
  end
end
