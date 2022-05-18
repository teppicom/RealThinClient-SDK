object RtcRouter2MainForm: TRtcRouter2MainForm
  Left = 352
  Top = 139
  Width = 472
  Height = 588
  AutoSize = True
  Caption = 'RTC Router 2'
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
  object Label4: TLabel
    Left = 5
    Top = 39
    Width = 80
    Height = 16
    Caption = 'Listen on Port'
  end
  object Label7: TLabel
    Left = 5
    Top = 443
    Width = 69
    Height = 16
    Caption = 'LOG Folder'
  end
  object Label8: TLabel
    Left = 5
    Top = 305
    Width = 100
    Height = 16
    Caption = 'Default Root URI'
  end
  object Bevel2: TBevel
    Left = 0
    Top = 389
    Width = 454
    Height = 3
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
    Left = 236
    Top = 502
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
  object Label6: TLabel
    Left = 177
    Top = 276
    Width = 102
    Height = 16
    Caption = 'Max Connections'
  end
  object Label1: TLabel
    Left = 14
    Top = 246
    Width = 94
    Height = 16
    Alignment = taRightJustify
    Caption = 'Server Address'
  end
  object Label2: TLabel
    Left = 41
    Top = 276
    Width = 67
    Height = 16
    Alignment = taRightJustify
    Caption = 'Server Port'
  end
  object Label11: TLabel
    Left = 369
    Top = 276
    Width = 75
    Height = 16
    Caption = '(0=unlimited)'
  end
  object bConnect: TButton
    Left = 364
    Top = 487
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
    Top = 330
    Width = 109
    Height = 26
    Caption = 'Change Host'
    Checked = True
    State = cbChecked
    TabOrder = 23
  end
  object eToHost: TEdit
    Left = 118
    Top = 330
    Width = 331
    Height = 24
    TabOrder = 24
    Text = 'www.realthinclient.com'
  end
  object xChangeURLs: TCheckBox
    Left = 5
    Top = 359
    Width = 395
    Height = 21
    Caption = 'Try to replace direct URLs on HTML Pages with relative URIs'
    TabOrder = 25
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
    Top = 408
    Width = 100
    Height = 21
    Caption = 'Debug LOG'
    TabOrder = 26
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
    Top = 438
    Width = 365
    Height = 24
    TabOrder = 30
    OnChange = eLogFolderChange
  end
  object cReqOrder: TRadioGroup
    Left = 5
    Top = 472
    Width = 218
    Height = 71
    Caption = 'Request Forwarding Order'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Standard'
      'Reverse'
      'Random'
      'Chance')
    TabOrder = 31
  end
  object xEventLog: TCheckBox
    Left = 108
    Top = 408
    Width = 95
    Height = 21
    Caption = 'Event LOG'
    TabOrder = 27
  end
  object eToURI: TEdit
    Left = 118
    Top = 300
    Width = 331
    Height = 24
    TabOrder = 22
    OnExit = eToURIExit
  end
  object xBuffLog: TCheckBox
    Left = 212
    Top = 408
    Width = 153
    Height = 21
    Caption = 'LOG Buffering (faster)'
    TabOrder = 28
    OnClick = xBuffLogClick
  end
  object bDumpLog: TButton
    Left = 369
    Top = 399
    Width = 80
    Height = 35
    Caption = 'Dump Log'
    TabOrder = 29
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
    Left = 290
    Top = 497
    Width = 66
    Height = 26
    MaxValue = 2000
    MinValue = 1
    TabOrder = 32
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
  object eConCount: TSpinEdit
    Left = 290
    Top = 271
    Width = 71
    Height = 26
    MaxValue = 30000
    MinValue = 0
    TabOrder = 21
    Value = 1
  end
  object eToAddr: TEdit
    Left = 118
    Top = 241
    Width = 326
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 19
    Text = 'www.realthinclient.com'
    OnChange = eToAddrChange
  end
  object eToPort: TEdit
    Left = 118
    Top = 271
    Width = 51
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 20
    Text = '80'
  end
  object xClientIP6: TCheckBox
    Left = 166
    Top = 113
    Width = 59
    Height = 26
    Caption = 'IPv6'
    TabOrder = 9
  end
  object xServerIP6: TCheckBox
    Left = 166
    Top = 1
    Width = 59
    Height = 26
    Caption = 'IPv6'
    TabOrder = 0
  end
  object Server: TRtcHttpServer
    ServerPort = '80'
    Left = 216
    Top = 252
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
    Left = 252
    Top = 252
  end
  object StatProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = StatProviderCheckRequest
    OnDataReceived = StatProviderDataReceived
    Left = 288
    Top = 252
  end
  object DumpProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = DumpProviderCheckRequest
    OnDataReceived = DumpProviderDataReceived
    Left = 324
    Top = 252
  end
end
