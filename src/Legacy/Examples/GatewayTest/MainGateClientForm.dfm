object GateClientForm: TGateClientForm
  Left = 355
  Top = 157
  BorderStyle = bsNone
  Caption = 'Simple Gate Test Client'
  ClientHeight = 131
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
    Width = 100
    Height = 100
    TabOrder = 0
    OnMouseDown = InfoPanelMouseDown
    OnMouseMove = InfoPanelMouseMove
    OnMouseUp = InfoPanelMouseUp
    object shInput: TShape
      Left = -1
      Top = 0
      Width = 17
      Height = 17
      Brush.Color = clRed
      Pen.Width = 3
    end
    object shOutput: TShape
      Left = -1
      Top = 17
      Width = 17
      Height = 17
      Brush.Color = clRed
      Pen.Width = 3
    end
    object lblSendBuffSize: TLabel
      Left = 18
      Top = 19
      Width = 13
      Height = 13
      Caption = '0K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnMouseDown = InfoPanelMouseDown
      OnMouseMove = InfoPanelMouseMove
      OnMouseUp = InfoPanelMouseUp
    end
    object lblRecvBufferSize: TLabel
      Left = 18
      Top = 2
      Width = 13
      Height = 13
      Caption = '0K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnMouseDown = InfoPanelMouseDown
      OnMouseMove = InfoPanelMouseMove
      OnMouseUp = InfoPanelMouseUp
    end
    object btnCLR: TLabel
      Left = 72
      Top = 35
      Width = 27
      Height = 14
      Hint = 'Re-Invite all IDs'
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
    object btnClose: TLabel
      Left = 16
      Top = 0
      Width = 57
      Height = 34
      Hint = 'Close'
      Alignment = taCenter
      AutoSize = False
      Caption = 'X'
      Color = clRed
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
      OnClick = btnCloseClick
    end
    object l_Status3: TLabel
      Left = 52
      Top = 35
      Width = 17
      Height = 13
      Alignment = taRightJustify
      Caption = '- Ks'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = False
      OnMouseDown = InfoPanelMouseDown
      OnMouseMove = InfoPanelMouseMove
      OnMouseUp = InfoPanelMouseUp
    end
    object btnLogIN: TSpeedButton
      Left = 72
      Top = 0
      Width = 28
      Height = 17
      Hint = 'Log in/out'
      Caption = 'GO'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      Layout = blGlyphBottom
      ParentFont = False
      OnClick = btnLogInClick
    end
    object btnSendFile: TSpeedButton
      Left = 72
      Top = 16
      Width = 28
      Height = 19
      Hint = 'Send/Pause'
      Caption = 'RUN'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = btnSendFileClick
    end
    object shState: TShape
      Left = 4
      Top = 38
      Width = 8
      Height = 9
      Brush.Color = clRed
      Pen.Style = psClear
    end
    object InfoPanel: TPanel
      Left = 2
      Top = 50
      Width = 98
      Height = 50
      TabOrder = 0
      OnMouseDown = InfoPanelMouseDown
      OnMouseMove = InfoPanelMouseMove
      OnMouseUp = InfoPanelMouseUp
      object l_Status1: TLabel
        Left = 1
        Top = 18
        Width = 49
        Height = 14
        Caption = 'Logged OUT'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
        OnMouseDown = InfoPanelMouseDown
        OnMouseMove = InfoPanelMouseMove
        OnMouseUp = InfoPanelMouseUp
      end
      object l_Status2: TLabel
        Left = 1
        Top = 34
        Width = 11
        Height = 14
        Caption = 'OK'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial Narrow'
        Font.Style = []
        ParentFont = False
        OnMouseDown = InfoPanelMouseDown
        OnMouseMove = InfoPanelMouseMove
        OnMouseUp = InfoPanelMouseUp
      end
      object eYourID: TSpeedButton
        Left = -1
        Top = 0
        Width = 46
        Height = 18
        Hint = 'Copy ID'
        Caption = '999999'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        OnClick = eYourIDClick
      end
      object eToID: TSpeedButton
        Left = 44
        Top = 0
        Width = 22
        Height = 18
        Hint = 'Add all IDs'
        Caption = '>>'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        OnClick = eToIDClick
      end
      object l_Groups: TLabel
        Left = 68
        Top = 2
        Width = 29
        Height = 12
        Alignment = taCenter
        AutoSize = False
        Caption = '0/0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
        OnMouseDown = InfoPanelMouseDown
        OnMouseMove = InfoPanelMouseMove
        OnMouseUp = InfoPanelMouseUp
      end
      object btnReset: TSpeedButton
        Left = 82
        Top = 17
        Width = 17
        Height = 16
        Hint = 'Soft Reset'
        Caption = 'O'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        OnClick = btnSoftResetClick
      end
      object btnHardReset: TSpeedButton
        Left = 82
        Top = 33
        Width = 17
        Height = 16
        Hint = 'Hard Reset'
        Caption = 'X'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        OnClick = btnHardResetClick
      end
    end
  end
  object StatusUpdate: TTimer
    Enabled = False
    OnTimer = StatusUpdateTimer
    Left = 276
    Top = 52
  end
  object StartAnother: TTimer
    Enabled = False
    Interval = 100
    OnTimer = StartAnotherTimer
    Left = 242
    Top = 51
  end
  object udpServer: TRtcUdpServer
    ServerPort = '8888'
    UdpReuseAddr = True
    OnDataReceived = udpServerDataReceived
    Left = 158
    Top = 51
  end
  object udpClient: TRtcUdpClient
    ServerAddr = '255.255.255.255'
    ServerPort = '8888'
    UdpReuseAddr = True
    Left = 190
    Top = 51
  end
  object GateCli: TRtcHttpGateClient
    GateAddr = 'localhost'
    GatePort = '80'
    GateFileName = '/'
    Left = 168
    Top = 4
  end
  object GCM: TRtcGateClientLink
    Client = GateCli
    BackThread = GCThread
    AfterClientRemoved = GCMAfterClientRemoved
    AfterLoggedIn = GCMAfterLoggedIn
    OnDataFilter = GCMDataFilter
    OnInfoFilter = GCMInfoFilter
    OnDataReceived = GCMDataReceived
    OnInfoReceived = GCMInfoReceived
    OnReadyToSend = GCMReadyToSend
    BeforeLogInGUI = GCMBeforeLogInGUI
    AfterLoggedInGUI = GCMAfterLoggedInGUI
    AfterLoginFailGUI = GCMAfterLoginFailGUI
    AfterLogOutGUI = GCMAfterLogOutGUI
    OnReadyToSendGUI = GCMReadyToSendGUI
    OnStreamResetGUI = GCMStreamResetGUI
    Left = 232
    Top = 4
  end
  object GCThread: TRtcGCThread
    OnDataReceived = GCThreadDataReceived
    OnInfoReceived = GCThreadInfoReceived
    OnReadyToSend = GCThreadReadyToSend
    OnReadyToSendGUI = GCMReadyToSendGUI
    Left = 284
    Top = 4
  end
end
