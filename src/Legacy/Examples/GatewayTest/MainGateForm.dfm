object GateForm: TGateForm
  Left = 370
  Top = 170
  Width = 482
  Height = 321
  AutoSize = True
  Caption = 'Simple RTC Gateway'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 276
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 172
      Width = 24
      Height = 16
      Caption = 'Port'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblStatus: TLabel
      Left = 376
      Top = 252
      Width = 78
      Height = 16
      Alignment = taRightJustify
      Caption = 'Click START'
    end
    object lblConnect: TLabel
      Left = 346
      Top = 226
      Width = 16
      Height = 16
      Alignment = taRightJustify
      Caption = '---'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 368
      Top = 226
      Width = 86
      Height = 16
      Alignment = taRightJustify
      Caption = 'Connections'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object shGateway4: TShape
      Left = 288
      Top = 168
      Width = 77
      Height = 53
      Brush.Color = clRed
      Shape = stRoundRect
    end
    object lblThreads: TLabel
      Left = 138
      Top = 250
      Width = 16
      Height = 16
      Caption = '---'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object shGateway6: TShape
      Left = 372
      Top = 168
      Width = 77
      Height = 53
      Brush.Color = clRed
      Shape = stRoundRect
    end
    object Label5: TLabel
      Left = 72
      Top = 248
      Width = 51
      Height = 16
      Caption = 'Threads'
    end
    object btnStart: TButton
      Left = 184
      Top = 168
      Width = 97
      Height = 53
      Caption = 'START'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = btnStartClick
    end
    object ePort: TEdit
      Left = 44
      Top = 168
      Width = 65
      Height = 24
      TabOrder = 2
      Text = '80'
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 217
      Height = 153
      Caption = 'Receiving parameters'
      TabOrder = 1
      object Label3: TLabel
        Left = 148
        Top = 56
        Width = 34
        Height = 16
        Caption = 'Bytes'
      end
      object Label10: TLabel
        Left = 147
        Top = 24
        Width = 30
        Height = 16
        Caption = 'KBits'
      end
      object Label6: TLabel
        Left = 6
        Top = 56
        Width = 71
        Height = 16
        Alignment = taRightJustify
        Caption = 'Packet Size'
      end
      object Label9: TLabel
        Left = 14
        Top = 88
        Width = 63
        Height = 16
        Alignment = taRightJustify
        Caption = 'Buffer Size'
      end
      object Label11: TLabel
        Left = 148
        Top = 88
        Width = 49
        Height = 16
        Caption = 'Packets'
      end
      object Label2: TLabel
        Left = 8
        Top = 24
        Width = 69
        Height = 16
        Alignment = taRightJustify
        Caption = 'Max Speed'
      end
      object Label13: TLabel
        Left = 23
        Top = 120
        Width = 54
        Height = 16
        Alignment = taRightJustify
        Caption = '1 Read ='
      end
      object Label14: TLabel
        Left = 148
        Top = 120
        Width = 49
        Height = 16
        Caption = 'Packets'
      end
      object eInPack: TSpinEdit
        Left = 84
        Top = 52
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Increment = 100
        MaxValue = 65500
        MinValue = 0
        ParentFont = False
        TabOrder = 1
        Value = 1500
      end
      object eInBuff: TSpinEdit
        Left = 84
        Top = 84
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 255
        MinValue = 0
        ParentFont = False
        TabOrder = 2
        Value = 3
      end
      object eInSpeed: TSpinEdit
        Left = 84
        Top = 20
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Increment = 50
        MaxValue = 1000000
        MinValue = 0
        ParentFont = False
        TabOrder = 0
        Value = 1000
      end
      object eInRead: TSpinEdit
        Left = 84
        Top = 116
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 255
        MinValue = 0
        ParentFont = False
        TabOrder = 3
        Value = 3
      end
    end
    object GroupBox2: TGroupBox
      Left = 236
      Top = 8
      Width = 217
      Height = 153
      Caption = 'Sending parameters'
      TabOrder = 0
      object Label7: TLabel
        Left = 148
        Top = 56
        Width = 34
        Height = 16
        Caption = 'Bytes'
      end
      object Label12: TLabel
        Left = 147
        Top = 24
        Width = 30
        Height = 16
        Caption = 'KBits'
      end
      object Label15: TLabel
        Left = 6
        Top = 56
        Width = 71
        Height = 16
        Alignment = taRightJustify
        Caption = 'Packet Size'
      end
      object Label16: TLabel
        Left = 14
        Top = 88
        Width = 63
        Height = 16
        Alignment = taRightJustify
        Caption = 'Buffer Size'
      end
      object Label17: TLabel
        Left = 148
        Top = 88
        Width = 49
        Height = 16
        Caption = 'Packets'
      end
      object Label18: TLabel
        Left = 8
        Top = 24
        Width = 69
        Height = 16
        Alignment = taRightJustify
        Caption = 'Max Speed'
      end
      object Label19: TLabel
        Left = 26
        Top = 120
        Width = 51
        Height = 16
        Alignment = taRightJustify
        Caption = '1 Write ='
      end
      object Label20: TLabel
        Left = 148
        Top = 120
        Width = 49
        Height = 16
        Caption = 'Packets'
      end
      object eOutPack: TSpinEdit
        Left = 84
        Top = 52
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Increment = 100
        MaxValue = 65500
        MinValue = 0
        ParentFont = False
        TabOrder = 1
        Value = 1500
      end
      object eOutBuff: TSpinEdit
        Left = 84
        Top = 84
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 255
        MinValue = 0
        ParentFont = False
        TabOrder = 2
        Value = 9
      end
      object eOutSpeed: TSpinEdit
        Left = 84
        Top = 20
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Increment = 50
        MaxValue = 1000000
        MinValue = 0
        ParentFont = False
        TabOrder = 0
        Value = 1000
      end
      object eOutWrite: TSpinEdit
        Left = 84
        Top = 116
        Width = 61
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 18
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 255
        MinValue = 0
        ParentFont = False
        TabOrder = 3
        Value = 9
      end
    end
    object xMultiThreaded: TCheckBox
      Left = 8
      Top = 222
      Width = 117
      Height = 17
      Caption = 'Multi-Threaded'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object xIPv6: TCheckBox
      Left = 120
      Top = 196
      Width = 57
      Height = 17
      Caption = 'IPv6'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object xBlockingAPI: TCheckBox
      Left = 8
      Top = 200
      Width = 85
      Height = 17
      Caption = 'Blocking'
      TabOrder = 6
    end
    object xIPv4: TCheckBox
      Left = 120
      Top = 172
      Width = 57
      Height = 17
      Caption = 'IPv4'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object eThreads: TSpinEdit
      Left = 8
      Top = 244
      Width = 61
      Height = 26
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 18
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Increment = 10
      MaxValue = 2000
      MinValue = 0
      ParentFont = False
      TabOrder = 8
      Value = 500
    end
  end
  object StatusTimer: TTimer
    OnTimer = StatusTimerTimer
    Left = 408
    Top = 171
  end
  object MyGate: TRtcGateway
    GateFileName = '/'
    Link = MyLink
    Left = 304
    Top = 172
  end
  object MyLink: TRtcDualDataServerLink
    Server = Server4
    Server2 = Server6
    Left = 300
    Top = 228
  end
  object Server6: TRtcHttpServer
    MultiThreaded = True
    ServerIPV = rtc_IPv6
    OnListenStart = MyGateListenStart
    OnListenStop = MyGateListenStop
    OnListenError = MyGateListenError
    Left = 244
    Top = 228
  end
  object Server4: TRtcHttpServer
    MultiThreaded = True
    OnListenStart = MyGateListenStart
    OnListenStop = MyGateListenStop
    OnListenError = MyGateListenError
    Left = 184
    Top = 228
  end
end
