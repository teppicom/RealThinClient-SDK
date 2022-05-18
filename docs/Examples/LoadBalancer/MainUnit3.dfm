object RtcLoadBalancerMainForm: TRtcLoadBalancerMainForm
  Left = 356
  Top = 98
  Width = 566
  Height = 712
  Caption = 'RTC Load Balancer v3 *** Multi-Web-App Demo'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 548
    Height = 667
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'General'
      object Bevel6: TBevel
        Left = 5
        Top = 373
        Width = 523
        Height = 170
        Shape = bsFrame
      end
      object Bevel2: TBevel
        Left = 5
        Top = 172
        Width = 523
        Height = 203
        Shape = bsFrame
      end
      object Label4: TLabel
        Left = 9
        Top = 551
        Width = 95
        Height = 16
        Caption = 'Listening Port'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 5
        Top = 10
        Width = 255
        Height = 16
        Caption = 'Web Applications and Servers List ...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label1: TLabel
        Left = 84
        Top = 153
        Width = 445
        Height = 16
        Caption = 
          '[ Select a line above to edit it below, add a line above to add ' +
          'a new Server ]'
      end
      object Label13: TLabel
        Left = 15
        Top = 207
        Width = 158
        Height = 16
        Caption = 'Virtual Host ( empty = ALL )'
      end
      object Label15: TLabel
        Left = 197
        Top = 207
        Width = 275
        Height = 16
        Caption = 'Virtual Path ( '#39'/'#39' for root = accept ALL requests )'
      end
      object Label17: TLabel
        Left = 15
        Top = 409
        Width = 94
        Height = 16
        Caption = 'Server Address'
      end
      object Label18: TLabel
        Left = 197
        Top = 409
        Width = 67
        Height = 16
        Caption = 'Server Port'
      end
      object Label19: TLabel
        Left = 15
        Top = 463
        Width = 105
        Height = 32
        Caption = 'Max Connections (0 = unlimited)'
        WordWrap = True
      end
      object Label20: TLabel
        Left = 281
        Top = 409
        Width = 233
        Height = 16
        Caption = 'Web Application root URI on the Server'
      end
      object Label21: TLabel
        Left = 133
        Top = 463
        Width = 87
        Height = 32
        Caption = 'Max Sessions (0 = unlimited)'
        WordWrap = True
      end
      object Label22: TLabel
        Left = 241
        Top = 463
        Width = 104
        Height = 32
        Caption = 'Session Timeout (0 = 24 hours)'
        WordWrap = True
      end
      object Label23: TLabel
        Left = 345
        Top = 507
        Width = 22
        Height = 16
        Caption = 'sec'
      end
      object Label24: TLabel
        Left = 10
        Top = 30
        Width = 522
        Height = 16
        Caption = 
          'Host/Path AppType LBType ROrder SrvAddr:Port/Uri MaxConn MaxSess' +
          ' SessTimeout'
      end
      object Label26: TLabel
        Left = 10
        Top = 153
        Width = 63
        Height = 16
        Caption = 'Selected'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label27: TLabel
        Left = 15
        Top = 182
        Width = 502
        Height = 16
        Caption = 
          'Each Web Application is accessible through one unique VirtualHos' +
          't + VirtualPath pair'
      end
      object Label28: TLabel
        Left = 15
        Top = 384
        Width = 412
        Height = 16
        Caption = 'Every Web Application can be hosted on any number of Web Servers'
      end
      object Label29: TLabel
        Left = 10
        Top = 308
        Width = 511
        Height = 61
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Web Application type, Load balancing type and Request order (see' +
          ' above) are set per Web Application and NOT per Server. If more ' +
          'than one Server is used for the same Web App, only Web App param' +
          'eters set for the 1st (topmost) Server will be used.'
        WordWrap = True
      end
      object Label6: TLabel
        Left = 108
        Top = 572
        Width = 308
        Height = 32
        Alignment = taCenter
        Caption = 
          'Listening Port is the Port number on which the Load Balancer wil' +
          'l be waiting for all incoming connections.'
        WordWrap = True
      end
      object Label11: TLabel
        Left = 15
        Top = 253
        Width = 128
        Height = 16
        Caption = 'Web Application type'
      end
      object Label25: TLabel
        Left = 251
        Top = 253
        Width = 123
        Height = 16
        Caption = 'Load Balancing type'
      end
      object Label30: TLabel
        Left = 409
        Top = 253
        Width = 85
        Height = 16
        Caption = 'RequestOrder'
      end
      object eFromPort: TEdit
        Left = 15
        Top = 592
        Width = 75
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 14
        Text = '80'
      end
      object bConnect: TButton
        Left = 440
        Top = 568
        Width = 91
        Height = 51
        Caption = 'START'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 15
        OnClick = bConnectClick
      end
      object eServerList: TMemo
        Left = 10
        Top = 49
        Width = 513
        Height = 90
        Lines.Strings = (
          '/x 3 0 0 localhost:81/ 100 0 600'
          '/x 3 0 0 localhost:82/ 100 0 600')
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
        OnKeyUp = eServerListKeyUp
        OnMouseDown = eServerListMouseDown
      end
      object eVirtualHost: TEdit
        Left = 15
        Top = 226
        Width = 168
        Height = 24
        TabOrder = 1
        OnChange = eServerDetailsChange
      end
      object eVirtualPath: TEdit
        Left = 197
        Top = 226
        Width = 321
        Height = 24
        TabOrder = 2
        OnChange = eServerDetailsChange
      end
      object eServerAddr: TEdit
        Left = 15
        Top = 428
        Width = 168
        Height = 24
        TabOrder = 6
        OnChange = eServerDetailsChange
      end
      object eServerPort: TEdit
        Left = 197
        Top = 428
        Width = 70
        Height = 24
        TabOrder = 7
        OnChange = eServerDetailsChange
      end
      object eServerURI: TEdit
        Left = 281
        Top = 428
        Width = 237
        Height = 24
        TabOrder = 8
        OnChange = eServerDetailsChange
      end
      object eMaxConn: TSpinEdit
        Left = 15
        Top = 502
        Width = 104
        Height = 26
        MaxValue = 10000
        MinValue = 0
        TabOrder = 9
        Value = 0
        OnChange = eServerDetailsChange
      end
      object eMaxSess: TSpinEdit
        Left = 133
        Top = 502
        Width = 95
        Height = 26
        MaxValue = 1000000
        MinValue = 0
        TabOrder = 10
        Value = 0
        OnChange = eServerDetailsChange
      end
      object eSessTimeout: TSpinEdit
        Left = 241
        Top = 502
        Width = 100
        Height = 26
        MaxValue = 1000000
        MinValue = 0
        TabOrder = 11
        Value = 0
        OnChange = eServerDetailsChange
      end
      object bAddServer: TButton
        Left = 394
        Top = 458
        Width = 119
        Height = 36
        Caption = 'New Server'
        TabOrder = 12
        OnClick = bAddServerClick
      end
      object bAddWebApp: TButton
        Left = 394
        Top = 497
        Width = 119
        Height = 36
        Caption = 'New Web App'
        TabOrder = 13
        OnClick = bAddWebAppClick
      end
      object cAppType: TComboBox
        Left = 15
        Top = 273
        Width = 227
        Height = 24
        Style = csDropDownList
        ItemHeight = 16
        ItemIndex = 0
        TabOrder = 3
        Text = '0=Stateless (no Sessions)'
        OnChange = eServerDetailsChange
        Items.Strings = (
          '0=Stateless (no Sessions)'
          '1=RTC with Sessions'
          '2=IW Hidden Sessions'
          '3=IW Absolute Hidden Sessions')
      end
      object cBalanceType: TComboBox
        Left = 251
        Top = 273
        Width = 149
        Height = 24
        Style = csDropDownList
        ItemHeight = 16
        ItemIndex = 0
        TabOrder = 4
        Text = '0=Session Balance'
        OnChange = eServerDetailsChange
        Items.Strings = (
          '0=Session Balance'
          '1=Request Balance'
          '2=Round Robin')
      end
      object cReqOrder: TComboBox
        Left = 409
        Top = 273
        Width = 109
        Height = 24
        Style = csDropDownList
        ItemHeight = 16
        ItemIndex = 0
        TabOrder = 5
        Text = '0=Standard'
        OnChange = eServerDetailsChange
        Items.Strings = (
          '0=Standard'
          '1=Reverse'
          '2=Random'
          '3=Chance')
      end
      object xSSL: TCheckBox
        Left = 16
        Top = 572
        Width = 61
        Height = 17
        Caption = 'SSL'
        TabOrder = 16
        OnClick = xSSLClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Connection Settings'
      ImageIndex = 1
      object Bevel4: TBevel
        Left = 187
        Top = 241
        Width = 293
        Height = 65
        Style = bsRaised
      end
      object Bevel3: TBevel
        Left = 182
        Top = 128
        Width = 303
        Height = 65
        Style = bsRaised
      end
      object Label9: TLabel
        Left = 418
        Top = 138
        Width = 60
        Height = 16
        Caption = 'Sec/pack'
      end
      object Label12: TLabel
        Left = 418
        Top = 167
        Width = 60
        Height = 16
        Caption = 'Sec/pack'
      end
      object Label14: TLabel
        Left = 412
        Top = 250
        Width = 60
        Height = 16
        Caption = 'Sec/pack'
      end
      object Label16: TLabel
        Left = 412
        Top = 281
        Width = 60
        Height = 16
        Caption = 'Sec/pack'
      end
      object Label5: TLabel
        Left = 39
        Top = 101
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
        Left = 39
        Top = 214
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
        Left = 34
        Top = 202
        Width = 450
        Height = 4
      end
      object Label3: TLabel
        Left = 39
        Top = 54
        Width = 79
        Height = 16
        Caption = 'Max Threads'
      end
      object Bevel1: TBevel
        Left = 39
        Top = 89
        Width = 445
        Height = 3
      end
      object Label8: TLabel
        Left = 202
        Top = 54
        Width = 246
        Height = 16
        Caption = '(more threads = more parallel operations)'
      end
      object xServerMulti: TCheckBox
        Left = 273
        Top = 98
        Width = 119
        Height = 26
        Caption = 'Multi-Threaded'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object xServerBlocking: TCheckBox
        Left = 404
        Top = 98
        Width = 81
        Height = 26
        Caption = 'Blocking'
        TabOrder = 3
      end
      object xClientMulti: TCheckBox
        Left = 277
        Top = 212
        Width = 119
        Height = 26
        Caption = 'Multi-Threaded'
        Checked = True
        State = cbChecked
        TabOrder = 10
      end
      object xClientBlocking: TCheckBox
        Left = 405
        Top = 212
        Width = 80
        Height = 26
        Caption = 'Blocking'
        TabOrder = 11
      end
      object xForceHttp10: TCheckBox
        Left = 44
        Top = 251
        Width = 125
        Height = 21
        Caption = 'Force HTTP/1.0'
        TabOrder = 12
      end
      object xResponseBuffer: TCheckBox
        Left = 44
        Top = 281
        Width = 134
        Height = 21
        Caption = 'Buffer Responses'
        TabOrder = 13
      end
      object xRequestBuffer: TCheckBox
        Left = 44
        Top = 148
        Width = 130
        Height = 21
        Caption = 'Buffer Requests'
        TabOrder = 4
      end
      object xRequestInTimeouts: TCheckBox
        Left = 192
        Top = 138
        Width = 154
        Height = 21
        Caption = 'Request IN Timeout'
        TabOrder = 5
      end
      object xResponseOutTimeout: TCheckBox
        Left = 192
        Top = 167
        Width = 174
        Height = 21
        Caption = 'Response OUT Timeout'
        TabOrder = 7
      end
      object xRequestOutTimeout: TCheckBox
        Left = 197
        Top = 251
        Width = 159
        Height = 21
        Caption = 'Request OUT Timeout'
        TabOrder = 15
      end
      object xResponseInTimeout: TCheckBox
        Left = 197
        Top = 281
        Width = 154
        Height = 21
        Caption = 'Response IN Timeout'
        TabOrder = 17
      end
      object eRequestInTime: TSpinEdit
        Left = 364
        Top = 133
        Width = 51
        Height = 26
        MaxValue = 60
        MinValue = 0
        TabOrder = 6
        Value = 5
      end
      object eResponseOutTime: TSpinEdit
        Left = 364
        Top = 162
        Width = 51
        Height = 26
        MaxValue = 60
        MinValue = 0
        TabOrder = 8
        Value = 5
      end
      object eResponseInTime: TSpinEdit
        Left = 359
        Top = 276
        Width = 51
        Height = 26
        MaxValue = 60
        MinValue = 0
        TabOrder = 18
        Value = 5
      end
      object eRequestOutTime: TSpinEdit
        Left = 359
        Top = 246
        Width = 51
        Height = 26
        MaxValue = 60
        MinValue = 0
        TabOrder = 16
        Value = 5
      end
      object xPostReturnBeforeResponseSent: TCheckBox
        Left = 44
        Top = 310
        Width = 430
        Height = 21
        Caption = 
          'PostReturn before ResponseSent (faster, but can result in higher' +
          ' load)'
        TabOrder = 14
      end
      object eThrCount: TSpinEdit
        Left = 128
        Top = 49
        Width = 65
        Height = 26
        MaxValue = 2000
        MinValue = 1
        TabOrder = 0
        Value = 64
      end
      object xServerIP6: TCheckBox
        Left = 200
        Top = 98
        Width = 57
        Height = 26
        Caption = 'IPv6'
        TabOrder = 1
      end
      object xClientIP6: TCheckBox
        Left = 201
        Top = 212
        Width = 64
        Height = 26
        Caption = 'IPv6'
        TabOrder = 9
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Debug Logging'
      ImageIndex = 2
      object Label7: TLabel
        Left = 34
        Top = 59
        Width = 69
        Height = 16
        Caption = 'LOG Folder'
      end
      object xDebugLog: TCheckBox
        Left = 113
        Top = 89
        Width = 174
        Height = 21
        Caption = 'Enable Debug Logging?'
        TabOrder = 1
      end
      object eLogFolder: TEdit
        Left = 113
        Top = 54
        Width = 366
        Height = 24
        TabOrder = 0
        OnChange = eLogFolderChange
      end
      object xBuffLog: TCheckBox
        Left = 295
        Top = 89
        Width = 179
        Height = 21
        Caption = 'Use LOG Buffers? (faster)'
        TabOrder = 2
        OnClick = xBuffLogClick
      end
      object bDumpLog: TButton
        Left = 345
        Top = 118
        Width = 129
        Height = 36
        Caption = 'Dump Log to Files'
        TabOrder = 3
        OnClick = bDumpLogClick
      end
    end
  end
  object Server: TRtcHttpServer
    OnRequestNotAccepted = ServerRequestNotAccepted
    Left = 276
    Top = 40
  end
  object StatProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = StatProviderCheckRequest
    OnDataReceived = StatProviderDataReceived
    Left = 340
    Top = 36
  end
  object DumpProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = DumpProviderCheckRequest
    OnDataReceived = DumpProviderDataReceived
    Left = 428
    Top = 36
  end
end
