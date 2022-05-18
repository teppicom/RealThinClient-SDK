object ChatHostFrm: TChatHostFrm
  Left = 398
  Top = 143
  Width = 568
  Height = 443
  Caption = 'Chat Host'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 430
    Top = 0
    Width = 5
    Height = 398
    Align = alRight
    ResizeStyle = rsLine
  end
  object Panel2: TPanel
    Left = 435
    Top = 0
    Width = 115
    Height = 398
    Align = alRight
    TabOrder = 1
    object eUsers: TListBox
      Left = 1
      Top = 65
      Width = 113
      Height = 332
      TabStop = False
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 18
      ParentFont = False
      TabOrder = 0
      OnDblClick = eUsersDblClick
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 113
      Height = 64
      Align = alTop
      TabOrder = 1
      object Label1: TLabel
        Left = 4
        Top = 44
        Width = 53
        Height = 16
        Caption = 'In Room:'
      end
      object btnAddUser: TButton
        Left = 4
        Top = 4
        Width = 105
        Height = 33
        Hint = 'Invite a User to this CHAT Room'
        Caption = 'INVITE'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = btnAddUserClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 430
    Height = 398
    Align = alClient
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 1
      Top = 282
      Width = 428
      Height = 5
      Cursor = crVSplit
      Align = alBottom
      MinSize = 50
    end
    object Panel4: TPanel
      Left = 1
      Top = 287
      Width = 428
      Height = 110
      Align = alBottom
      TabOrder = 0
      object eMessage: TMemo
        Left = 1
        Top = 38
        Width = 426
        Height = 71
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
        OnKeyPress = eMessageKeyPress
      end
      object Panel9: TPanel
        Left = 1
        Top = 1
        Width = 426
        Height = 37
        Align = alTop
        TabOrder = 1
        object pbPenColor: TColorBox
          Left = 8
          Top = 8
          Width = 145
          Height = 22
          ItemHeight = 16
          TabOrder = 0
        end
        object pbPenWidth: TTrackBar
          Left = 156
          Top = 4
          Width = 113
          Height = 29
          Min = 1
          Position = 2
          TabOrder = 1
        end
        object Panel10: TPanel
          Left = 376
          Top = 1
          Width = 49
          Height = 35
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          object btnClearDrawing: TBitBtn
            Left = 0
            Top = 4
            Width = 45
            Height = 29
            Caption = 'CLR'
            TabOrder = 0
            OnClick = btnClearDrawingClick
          end
        end
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 428
      Height = 281
      Align = alClient
      TabOrder = 1
      object spChat: TSplitter
        Left = 205
        Top = 1
        Height = 279
        Align = alRight
        MinSize = 50
      end
      object panChat: TPanel
        Left = 208
        Top = 1
        Width = 219
        Height = 279
        Align = alRight
        TabOrder = 0
        object eChat: TMemo
          Left = 1
          Top = 1
          Width = 217
          Height = 277
          TabStop = False
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object panDraw: TPanel
        Left = 1
        Top = 1
        Width = 204
        Height = 279
        Align = alClient
        TabOrder = 1
        object pbDrawing: TPaintBox
          Left = 1
          Top = 1
          Width = 202
          Height = 277
          Cursor = crCross
          Align = alClient
          OnMouseDown = pbDrawingMouseDown
          OnMouseMove = pbDrawingMouseMove
          OnMouseUp = pbDrawingMouseUp
          OnPaint = pbDrawingPaint
        end
      end
    end
  end
  object Link: TRtcGateClientLink
    BackThread = GCThread
    AfterClientRemoved = LinkAfterClientRemoved
    OnDataFilter = LinkDataFilter
    OnInfoFilter = LinkInfoFilter
    OnDataReceived = LinkDataReceived
    AfterLogOutGUI = LinkAfterLogOutGUI
    OnDataReceivedGUI = LinkDataReceivedGUI
    OnInfoReceivedGUI = LinkInfoReceivedGUI
    OnReadyAfterResetGUI = LinkReadyAfterResetGUI
    OnStreamResetGUI = LinkStreamResetGUI
    Left = 32
    Top = 84
  end
  object GCThread: TRtcGCThread
    OnDataReceived = GCThreadDataReceived
    OnDataReceivedGUI = GCThreadDataReceivedGUI
    Left = 102
    Top = 86
  end
end
