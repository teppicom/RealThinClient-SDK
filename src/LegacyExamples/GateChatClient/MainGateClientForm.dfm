object GateClientForm: TGateClientForm
  Left = 358
  Top = 206
  AutoSize = True
  BorderStyle = bsSingle
  Caption = 'RTC Gate Chat Client v1'
  ClientHeight = 366
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
    Height = 366
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 6
      Width = 66
      Height = 16
      Caption = 'My User ID'
    end
    object Label2: TLabel
      Left = 292
      Top = 32
      Width = 105
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = 'Own Groups'
      Color = clWhite
      ParentColor = False
      Layout = tlCenter
    end
    object Label3: TLabel
      Left = 401
      Top = 32
      Width = 104
      Height = 25
      Alignment = taCenter
      AutoSize = False
      Caption = 'Joined Groups'
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
      Left = 284
      Top = 0
      Width = 2
      Height = 365
    end
    object Label7: TLabel
      Left = 8
      Top = 156
      Width = 153
      Height = 29
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
    object lblInvites: TLabel
      Left = 8
      Top = 324
      Width = 157
      Height = 37
      Alignment = taCenter
      AutoSize = False
      Caption = 'Invitation(s) waiting. Dbl-Click to Accept'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      Visible = False
      WordWrap = True
    end
    object lblGroups: TLabel
      Left = 288
      Top = 324
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
    object Label4: TLabel
      Left = 180
      Top = 116
      Width = 97
      Height = 69
      Alignment = taCenter
      AutoSize = False
      Caption = 'USERS  ONLINE'
      Color = clWhite
      ParentColor = False
      Layout = tlCenter
      WordWrap = True
    end
    object lblOnline: TLabel
      Left = 180
      Top = 324
      Width = 97
      Height = 37
      Alignment = taCenter
      AutoSize = False
      Caption = 'Dbl-Click for Chat Invite'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      Visible = False
      WordWrap = True
    end
    object Bevel3: TBevel
      Left = 172
      Top = 107
      Width = 3
      Height = 262
    end
    object InfoPanel: TPanel
      Left = 8
      Top = 60
      Width = 269
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
      Top = 8
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
      Top = 24
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
      Left = 292
      Top = 56
      Width = 105
      Height = 265
      Hint = 'Doubl-Click to KICK User'
      ExtendedSelect = False
      ItemHeight = 16
      Items.Strings = (
        '999999/255')
      TabOrder = 2
      OnDblClick = eMyGroupDblClick
    end
    object eInGroup: TListBox
      Left = 400
      Top = 56
      Width = 105
      Height = 265
      Hint = 'Double-Click to LEAVE Group'
      ExtendedSelect = False
      ItemHeight = 16
      Items.Strings = (
        '999999/255')
      TabOrder = 3
      OnDblClick = eInGroupDblClick
    end
    object btnHostChat: TButton
      Left = 8
      Top = 112
      Width = 157
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
      Top = 188
      Width = 157
      Height = 133
      Hint = 'Doubl-Click to JOIN Chat'
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
    object eOnlineUsers: TListBox
      Left = 180
      Top = 188
      Width = 97
      Height = 133
      Hint = 'Doubl-Click to KICK User'
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
      TabOrder = 7
      OnDblClick = eOnlineUsersDblClick
    end
  end
  object StatusUpdate: TTimer
    Enabled = False
    Interval = 250
    OnTimer = StatusUpdateTimer
    Left = 188
    Top = 16
  end
  object GateCli: TRtcHttpGateClient
    GateAddr = 'localhost'
    GatePort = '80'
    GateFileName = '/'
    GatePrimaryKey = 'MyPrimaryChatKey'
    GateUserAuth = 'MyChatClient'
    GateUserInfo = 'This is me'
    UseProxy = True
    StreamBlockSizeOut = 124998
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
    Left = 172
    Top = 68
  end
  object ChatLink: TRtcGateClientLink
    Client = GateCli
    OnDataFilter = ChatLinkDataFilter
    OnDataReceivedGUI = ChatLinkDataReceivedGUI
    Left = 96
    Top = 68
  end
end
