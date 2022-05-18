object RtcFileServer: TRtcFileServer
  Left = 205
  Top = 171
  Width = 466
  Height = 324
  AutoSize = True
  Caption = 'RTC FileServer Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter2: TSplitter
    Left = 0
    Top = 221
    Width = 448
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 448
    Height = 221
    Align = alClient
    AutoSize = True
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 105
      Top = 1
      Height = 219
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 104
      Height = 219
      Align = alLeft
      AutoSize = True
      BorderWidth = 4
      Caption = 'Panel3'
      TabOrder = 0
      object Panel6: TPanel
        Left = 5
        Top = 5
        Width = 94
        Height = 18
        Align = alTop
        AutoSize = True
        BevelOuter = bvLowered
        TabOrder = 1
        object Label8: TLabel
          Left = 4
          Top = 1
          Width = 90
          Height = 16
          Caption = 'Index pages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object eIndexPages: TMemo
        Left = 5
        Top = 23
        Width = 94
        Height = 191
        Align = alClient
        Lines.Strings = (
          'default.html'
          'default.htm'
          'default.php'
          'default.pht'
          'm'
          ''
          'index.html'
          'index.htm'
          'index.php'
          'index.phtm')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object Panel5: TPanel
      Left = 108
      Top = 1
      Width = 339
      Height = 219
      Align = alClient
      AutoSize = True
      BorderWidth = 4
      Caption = 'Panel5'
      TabOrder = 1
      object Panel2: TPanel
        Left = 5
        Top = 5
        Width = 329
        Height = 18
        Align = alTop
        AutoSize = True
        BevelOuter = bvLowered
        TabOrder = 1
        object Label6: TLabel
          Left = 8
          Top = 1
          Width = 93
          Height = 16
          Caption = 'Virtual Hosts:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label1: TLabel
          Left = 96
          Top = 1
          Width = 132
          Height = 16
          Caption = 'HostName = DocRoot'
        end
        object Label7: TLabel
          Left = 228
          Top = 1
          Width = 60
          Height = 16
          Caption = '* = Default'
        end
      end
      object eVirtualHosts: TMemo
        Left = 5
        Top = 23
        Width = 329
        Height = 191
        Align = alClient
        Lines.Strings = (
          '* = .\')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object Panel7: TPanel
    Left = 0
    Top = 224
    Width = 448
    Height = 55
    Align = alBottom
    AutoSize = True
    TabOrder = 0
    object lblCliCon: TLabel
      Left = 12
      Top = 34
      Width = 116
      Height = 16
      Caption = 'Server not listening.'
    end
    object Label9: TLabel
      Left = 12
      Top = 4
      Width = 129
      Height = 16
      Caption = 'Streaming Extensions'
    end
    object btnListen: TButton
      Left = 328
      Top = 29
      Width = 57
      Height = 25
      Caption = 'Listen'
      TabOrder = 0
      OnClick = btnListenClick
    end
    object btnStop: TButton
      Left = 388
      Top = 29
      Width = 55
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 1
      OnClick = btnStopClick
    end
    object eStreamExtensions: TEdit
      Left = 148
      Top = 1
      Width = 297
      Height = 24
      Hint = 'Files with this extensions will be sent without HTTP Header.'
      TabOrder = 2
      Text = 'mov, mpg, mpeg, rm'
    end
  end
end
