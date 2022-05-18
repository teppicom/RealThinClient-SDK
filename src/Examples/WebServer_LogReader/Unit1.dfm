object Form1: TForm1
  Left = 2
  Top = 119
  Width = 957
  Height = 573
  Caption = 'RTC WebServer Log Analyzer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 498
    Top = 0
    Width = 7
    Height = 528
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 498
    Height = 528
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 0
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 496
      Height = 360
      Align = alTop
      TabOrder = 0
      DesignSize = (
        496
        360)
      object Label1: TLabel
        Left = 10
        Top = 261
        Width = 395
        Height = 16
        Caption = 
          'LOG File Name (full path): You can use * and ? to load multiple ' +
          'files.'
      end
      object Label2: TLabel
        Left = 10
        Top = 5
        Width = 207
        Height = 16
        Caption = 'Starting Page (leave Blank for Any)'
      end
      object Label3: TLabel
        Left = 10
        Top = 315
        Width = 93
        Height = 16
        Caption = 'Unique Visitors:'
      end
      object eVisitorCnt: TLabel
        Left = 108
        Top = 315
        Width = 12
        Height = 16
        Caption = '---'
      end
      object Label5: TLabel
        Left = 10
        Top = 54
        Width = 261
        Height = 16
        Caption = 'Starting Page Refferer (Leave blank for Any)'
      end
      object Label6: TLabel
        Left = 10
        Top = 335
        Width = 252
        Height = 16
        Caption = 'Click a Line in the List to see Details (Right)'
      end
      object Label9: TLabel
        Left = 10
        Top = 108
        Width = 429
        Height = 16
        Caption = 
          'Page which has to be visited for the User to be listed (Leave Bl' +
          'ank if Any)'
      end
      object Label10: TLabel
        Left = 10
        Top = 158
        Width = 344
        Height = 16
        Caption = 'Page which should NOT be visited for the User to be listed'
      end
      object Label11: TLabel
        Left = 10
        Top = 207
        Width = 91
        Height = 16
        Caption = 'Min Pageviews'
      end
      object Label12: TLabel
        Left = 212
        Top = 207
        Width = 97
        Height = 16
        Caption = 'Min Time on site'
      end
      object Label13: TLabel
        Left = 108
        Top = 207
        Width = 95
        Height = 16
        Caption = 'Max Pageviews'
      end
      object Label14: TLabel
        Left = 315
        Top = 207
        Width = 101
        Height = 16
        Caption = 'Max Time on site'
      end
      object Label15: TLabel
        Left = 423
        Top = 231
        Width = 66
        Height = 16
        Caption = 'HH:MM:SS'
      end
      object eFileName: TEdit
        Left = 10
        Top = 281
        Width = 434
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
      end
      object btnOpen: TButton
        Left = 448
        Top = 281
        Width = 31
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 11
        OnClick = btnOpenClick
      end
      object eStart: TEdit
        Left = 10
        Top = 25
        Width = 469
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object btnLoad: TButton
        Left = 350
        Top = 315
        Width = 131
        Height = 31
        Anchors = [akTop, akRight]
        Caption = 'Append from File(s)'
        TabOrder = 10
        OnClick = btnLoadClick
      end
      object eRef: TEdit
        Left = 10
        Top = 74
        Width = 469
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object btnNew: TButton
        Left = 286
        Top = 315
        Width = 60
        Height = 31
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        TabOrder = 9
        OnClick = btnNewClick
      end
      object eMandatoryPage: TEdit
        Left = 10
        Top = 128
        Width = 469
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object eNegativePage: TEdit
        Left = 10
        Top = 177
        Width = 469
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object eMinPages: TEdit
        Left = 10
        Top = 226
        Width = 95
        Height = 24
        TabOrder = 4
      end
      object eMinTime: TEdit
        Left = 212
        Top = 226
        Width = 99
        Height = 24
        TabOrder = 6
      end
      object eMaxPages: TEdit
        Left = 108
        Top = 226
        Width = 95
        Height = 24
        TabOrder = 5
      end
      object eMaxTime: TEdit
        Left = 315
        Top = 226
        Width = 105
        Height = 24
        TabOrder = 7
      end
    end
    object eVisitors: TListBox
      Left = 1
      Top = 361
      Width = 496
      Height = 166
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ItemHeight = 18
      ParentFont = False
      ScrollWidth = 1024
      TabOrder = 1
      OnClick = eVisitorsClick
    end
  end
  object Panel4: TPanel
    Left = 505
    Top = 0
    Width = 434
    Height = 528
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 432
      Height = 64
      Align = alTop
      TabOrder = 0
      DesignSize = (
        432
        64)
      object Label4: TLabel
        Left = 10
        Top = 10
        Width = 55
        Height = 16
        Caption = 'Visitor IP:'
      end
      object Label7: TLabel
        Left = 229
        Top = 10
        Width = 114
        Height = 16
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Time spent on Site:'
      end
      object Label8: TLabel
        Left = 492
        Top = 10
        Width = 70
        Height = 16
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Pageviews:'
      end
      object lVisitRef: TLabel
        Left = 10
        Top = 39
        Width = 51
        Height = 16
        Caption = 'Refferer:'
      end
      object eVisIP: TEdit
        Left = 74
        Top = 5
        Width = 119
        Height = 24
        ReadOnly = True
        TabOrder = 0
        OnEnter = eVisIPEnter
      end
      object eTimeSite: TEdit
        Left = 352
        Top = 5
        Width = 70
        Height = 24
        Anchors = [akTop, akRight]
        ReadOnly = True
        TabOrder = 1
      end
      object ePageVisit: TEdit
        Left = 568
        Top = 5
        Width = 56
        Height = 24
        Anchors = [akTop, akRight]
        ReadOnly = True
        TabOrder = 2
      end
      object eVisitRef: TEdit
        Left = 72
        Top = 34
        Width = 349
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 3
      end
    end
    object eInfo: TMemo
      Left = 1
      Top = 65
      Width = 432
      Height = 462
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
  end
  object OpenDlg: TOpenDialog
    DefaultExt = '*.LOG'
    Left = 352
    Top = 4
  end
end
