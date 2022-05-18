object GateClientForm: TGateClientForm
  Left = 391
  Top = 216
  AutoSize = True
  BorderStyle = bsSingle
  Caption = 'RTC Gate Chat Client v3'
  ClientHeight = 442
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 514
    Height = 442
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 82
      Width = 66
      Height = 16
      Caption = 'My User ID'
    end
    object Label2: TLabel
      Left = 288
      Top = 32
      Width = 217
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = 'Own Groups:'
      Color = clWhite
      ParentColor = False
      Layout = tlCenter
    end
    object Label3: TLabel
      Left = 289
      Top = 184
      Width = 216
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = 'Joined Groups:'
      Color = clWhite
      ParentColor = False
      Layout = tlCenter
    end
    object btnCLR: TLabel
      Left = 468
      Top = 7
      Width = 42
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = 'CLR'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
      OnClick = btnCLRClick
    end
    object l_Groups: TLabel
      Left = 291
      Top = 9
      Width = 15
      Height = 12
      Caption = '0/0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object l_Status3: TLabel
      Left = 424
      Top = 7
      Width = 33
      Height = 15
      Alignment = taRightJustify
      Caption = '0 KBit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Bevel1: TBevel
      Left = 284
      Top = 25
      Width = 237
      Height = 2
    end
    object Bevel2: TBevel
      Left = 282
      Top = -4
      Width = 3
      Height = 381
    end
    object lblInvites: TLabel
      Left = 8
      Top = 136
      Width = 269
      Height = 21
      AutoSize = False
      Caption = 'Chat invitations. Dbl-Click to Accept:'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      Visible = False
      WordWrap = True
    end
    object lblGroups: TLabel
      Left = 288
      Top = 344
      Width = 221
      Height = 37
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'Active Groups. Double-Click in the List to remove a User from Gr' +
        'oup.'
      Layout = tlCenter
      Visible = False
      WordWrap = True
    end
    object lblOnline: TLabel
      Left = 8
      Top = 232
      Width = 269
      Height = 21
      AutoSize = False
      Caption = 'Users Online. Dbl-Click to Invite:'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      Visible = False
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 4
      Width = 233
      Height = 21
      AutoSize = False
      Caption = 'Gateways:'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
    object InfoPanel: TPanel
      Left = 8
      Top = 388
      Width = 501
      Height = 49
      TabOrder = 4
      object l_Status1: TLabel
        Left = 4
        Top = 5
        Width = 70
        Height = 15
        Caption = 'Logged OUT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object l_Status2: TLabel
        Left = 4
        Top = 25
        Width = 17
        Height = 15
        Caption = 'OK'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
    end
    object Panel1: TPanel
      Left = 96
      Top = 84
      Width = 181
      Height = 47
      TabOrder = 5
      object shInput: TShape
        Left = 3
        Top = 4
        Width = 17
        Height = 17
        Brush.Color = clRed
        Pen.Width = 3
      end
      object lblRecvBufferSize: TLabel
        Left = 22
        Top = 6
        Width = 13
        Height = 13
        Caption = '0K'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblSendBuffSize: TLabel
        Left = 22
        Top = 27
        Width = 13
        Height = 13
        Caption = '0K'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object shOutput: TShape
        Left = 3
        Top = 25
        Width = 17
        Height = 17
        Brush.Color = clRed
        Pen.Width = 3
      end
      object btnReset: TSpeedButton
        Left = 136
        Top = 4
        Width = 41
        Height = 37
        Hint = 'Change connection provider'
        Caption = 'IE'
        OnClick = btnResetClick
      end
    end
    object eYourID: TEdit
      Left = 8
      Top = 100
      Width = 81
      Height = 29
      TabStop = False
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -18
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 6
      Text = '??????'
    end
    object eMyGroup: TListBox
      Left = 288
      Top = 56
      Width = 217
      Height = 125
      Hint = 'Doubl-Click to KICK User'
      ExtendedSelect = False
      ItemHeight = 16
      Items.Strings = (
        '999999@localhost:8080/255')
      TabOrder = 2
      OnDblClick = eMyGroupDblClick
    end
    object eInGroup: TListBox
      Left = 288
      Top = 208
      Width = 217
      Height = 137
      Hint = 'Double-Click to LEAVE Group'
      ExtendedSelect = False
      ItemHeight = 16
      Items.Strings = (
        '999999@localhost:8080/255')
      TabOrder = 3
      OnDblClick = eInGroupDblClick
    end
    object btnHostChat: TButton
      Left = 400
      Top = 392
      Width = 105
      Height = 41
      Caption = 'NEW CHAT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      WordWrap = True
      OnClick = btnHostChatClick
    end
    object eChatUsers: TListBox
      Left = 8
      Top = 156
      Width = 269
      Height = 73
      Hint = 'Doubl-Click to JOIN Chat'
      BiDiMode = bdLeftToRight
      ExtendedSelect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 18
      Items.Strings = (
        '999999@localhost:8080/255')
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 1
      OnDblClick = eChatUsersDblClick
    end
    object eOnlineUsers: TListBox
      Left = 8
      Top = 252
      Width = 269
      Height = 133
      Hint = 'Doubl-Click to Invite User'
      ExtendedSelect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 18
      Items.Strings = (
        '999999@localhost:8080')
      ParentFont = False
      TabOrder = 7
      OnClick = eOnlineUsersClick
      OnDblClick = eOnlineUsersDblClick
    end
    object eGateways: TListBox
      Left = 8
      Top = 24
      Width = 233
      Height = 57
      BiDiMode = bdLeftToRight
      ExtendedSelect = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ItemHeight = 16
      Items.Strings = (
        'localhost'
        'localhost:8080')
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 8
      OnClick = eGatewaysClick
      OnDblClick = eGatewaysDblClick
    end
    object btnNewGateway: TButton
      Left = 244
      Top = 24
      Width = 33
      Height = 29
      Hint = 'Add New Gateway'
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -20
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 9
      WordWrap = True
      OnClick = btnNewGatewayClick
    end
    object btnRemoveGateway: TButton
      Left = 244
      Top = 52
      Width = 33
      Height = 29
      Hint = 'Remove selected Gateway'
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 10
      WordWrap = True
      OnClick = btnRemoveGatewayClick
    end
  end
  object StatusUpdate: TTimer
    Enabled = False
    Interval = 250
    OnTimer = StatusUpdateTimer
    Left = 180
    Top = 88
  end
  object MultiCli: TRtcHttpMultiGateClient
    MinGroupID = 100
    MaxGroupID = 999
    OnDataFilter = MultiCliDataFilter
    OnInfoFilter = MultiCliInfoFilter
    OnInfoReceived = MultiCliInfoReceived
    OnReadyToSend = MultiCliReadyToSend
    BeforeLogInGUI = MultiCliBeforeLogInGUI
    AfterLoggedInGUI = MultiCliAfterLoggedInGUI
    AfterLoginFailGUI = MultiCliAfterLoginFailGUI
    AfterLogOutGUI = MultiCliAfterLogOutGUI
    OnInfoReceivedGUI = MultiCliInfoReceivedGUI
    OnStreamResetGUI = MultiCliStreamResetGUI
    GateFileName = '/'
    Left = 188
    Top = 32
  end
  object ChatLink: TRtcMultiGateClientLink
    MultiClient = MultiCli
    OnDataFilter = ChatLinkDataFilter
    OnDataReceivedGUI = ChatLinkDataReceivedGUI
    Left = 132
    Top = 32
  end
end
