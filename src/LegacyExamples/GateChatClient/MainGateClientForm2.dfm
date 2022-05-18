object GateClientForm: TGateClientForm
  Left = 364
  Top = 174
  Width = 511
  Height = 346
  Caption = 'RTC Gate Chat Client v2'
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
  DesignSize = (
    493
    301)
  PixelsPerInch = 120
  TextHeight = 16
  object Bevel1: TBevel
    Left = 276
    Top = 25
    Width = 209
    Height = 4
    Anchors = [akLeft, akTop, akRight]
  end
  object Bevel2: TBevel
    Left = 276
    Top = 0
    Width = 2
    Height = 300
    Anchors = [akLeft, akTop, akBottom]
  end
  object Bevel3: TBevel
    Left = 163
    Top = 112
    Width = 3
    Height = 188
    Anchors = [akLeft, akTop, akBottom]
  end
  object btnCLR: TLabel
    Left = 443
    Top = 4
    Width = 42
    Height = 15
    Alignment = taCenter
    Anchors = [akTop, akRight]
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
    Left = 283
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
    Left = 404
    Top = 4
    Width = 33
    Height = 15
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = '0 KBit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = False
  end
  object Label1: TLabel
    Left = 8
    Top = 3
    Width = 66
    Height = 16
    Caption = 'My User ID'
  end
  object Label2: TLabel
    Left = 284
    Top = 32
    Width = 93
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Caption = 'Own Groups'
    Color = clWhite
    ParentColor = False
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 383
    Top = 32
    Width = 102
    Height = 25
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Joined Groups'
    Color = clWhite
    ParentColor = False
    Layout = tlCenter
  end
  object Label4: TLabel
    Left = 172
    Top = 110
    Width = 97
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'ONLINE'
    Color = clWhite
    ParentColor = False
    Layout = tlCenter
    WordWrap = True
  end
  object Label7: TLabel
    Left = 8
    Top = 110
    Width = 145
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Invitations Waiting ...'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
  end
  object btnHostChat: TButton
    Left = 8
    Top = 255
    Width = 149
    Height = 38
    Anchors = [akLeft, akBottom]
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
    Top = 129
    Width = 149
    Height = 120
    Hint = 'Doubl-Click to JOIN Chat'
    Anchors = [akLeft, akTop, akBottom]
    BiDiMode = bdLeftToRight
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -18
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 21
    Items.Strings = (
      '999999/255')
    ParentBiDiMode = False
    ParentFont = False
    TabOrder = 1
    OnDblClick = eChatUsersDblClick
  end
  object eInGroup: TListBox
    Left = 383
    Top = 56
    Width = 102
    Height = 237
    Hint = 'Double-Click to LEAVE Group'
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExtendedSelect = False
    ItemHeight = 16
    Items.Strings = (
      '999999/255')
    TabOrder = 2
    OnDblClick = eInGroupDblClick
  end
  object eMyGroup: TListBox
    Left = 284
    Top = 56
    Width = 93
    Height = 237
    Hint = 'Doubl-Click to KICK User'
    Anchors = [akLeft, akTop, akBottom]
    ExtendedSelect = False
    ItemHeight = 16
    Items.Strings = (
      '999999/255')
    TabOrder = 3
    OnDblClick = eMyGroupDblClick
  end
  object eOnlineUsers: TListBox
    Left = 173
    Top = 129
    Width = 97
    Height = 164
    Hint = 'Double-Click for Chat Invite'
    Anchors = [akLeft, akTop, akBottom]
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -18
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 21
    Items.Strings = (
      '999999')
    ParentFont = False
    TabOrder = 4
    OnClick = eOnlineUsersClick
    OnDblClick = eOnlineUsersDblClick
  end
  object eYourID: TEdit
    Left = 5
    Top = 21
    Width = 77
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
    TabOrder = 5
    Text = '??????'
  end
  object InfoPanel: TPanel
    Left = 8
    Top = 55
    Width = 262
    Height = 49
    TabOrder = 6
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
    Left = 88
    Top = 3
    Width = 181
    Height = 49
    TabOrder = 7
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
      Left = 132
      Top = 6
      Width = 41
      Height = 37
      Hint = 'Change connection provider'
      Caption = 'AS'
      OnClick = btnResetClick
    end
  end
  object StatusUpdate: TTimer
    Enabled = False
    Interval = 250
    OnTimer = StatusUpdateTimer
    Left = 136
    Top = 4
  end
  object GateCli: TRtcHttpGateClient
    GateAddr = 'localhost'
    GatePort = '80'
    GateFileName = '/'
    AccountManager = AccMan
    OnDataFilter = GateCliDataFilter
    OnInfoFilter = GateCliInfoFilter
    OnInfoReceived = GateCliInfoReceived
    OnReadyToSend = GateCliReadyToSend
    BeforeLogInGUI = GateCliBeforeLogInGUI
    AfterLoggedInGUI = GateCliAfterLoggedInGUI
    AfterLoginFailGUI = GateCliAfterLoginFailGUI
    AfterLogOutGUI = GateCliAfterLogOutGUI
    OnInfoReceivedGUI = GateCliInfoReceivedGUI
    OnStreamResetGUI = GateCliStreamResetGUI
    Left = 32
    Top = 172
  end
  object ChatLink: TRtcGateClientLink
    Client = GateCli
    OnDataFilter = ChatLinkDataFilter
    OnDataReceivedGUI = ChatLinkDataReceivedGUI
    Left = 208
    Top = 172
  end
  object AccMan: TRtcGateAccountManager
    MinGroupID = 100
    MaxGroupID = 2000
    Left = 92
    Top = 172
  end
end
