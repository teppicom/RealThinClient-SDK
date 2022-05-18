object Form2: TForm2
  Left = 418
  Top = 132
  Width = 799
  Height = 598
  Caption = 
    'RTC Fish Facts Client 2 (ClientDataSet + RtcDataSetMonitor versi' +
    'on)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 0
    Top = 341
    Width = 781
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 781
    Height = 341
    Align = alTop
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 453
      Top = 1
      Height = 339
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 452
      Height = 339
      Align = alLeft
      TabOrder = 0
      object DBImage1: TDBImage
        Left = 1
        Top = 118
        Width = 450
        Height = 220
        Align = alClient
        BorderStyle = bsNone
        DataField = 'Graphic'
        DataSource = DataSource1
        TabOrder = 0
        OnClick = DBImage1Click
      end
      object Panel3: TPanel
        Left = 1
        Top = 77
        Width = 450
        Height = 41
        Align = alTop
        TabOrder = 1
        object DBNavigator1: TDBNavigator
          Left = 1
          Top = 1
          Width = 448
          Height = 39
          DataSource = DataSource1
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          Align = alClient
          TabOrder = 0
        end
      end
      object Panel5: TPanel
        Left = 1
        Top = 1
        Width = 450
        Height = 76
        Align = alTop
        TabOrder = 2
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 94
          Height = 16
          Caption = 'Server Address'
        end
        object Label2: TLabel
          Left = 208
          Top = 8
          Width = 67
          Height = 16
          Caption = 'Server Port'
        end
        object btnRefresh: TSpeedButton
          Left = 300
          Top = 8
          Width = 141
          Height = 41
          Caption = 'Reload DataSet'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000130B0000130B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
            3333333777333777FF33339993707399933333773337F3777FF3399933000339
            9933377333777F3377F3399333707333993337733337333337FF993333333333
            399377F33333F333377F993333303333399377F33337FF333373993333707333
            333377F333777F333333993333101333333377F333777F3FFFFF993333000399
            999377FF33777F77777F3993330003399993373FF3777F37777F399933000333
            99933773FF777F3F777F339993707399999333773F373F77777F333999999999
            3393333777333777337333333999993333333333377777333333}
          NumGlyphs = 2
          OnClick = btnRefreshClick
        end
        object Label3: TLabel
          Left = 8
          Top = 56
          Width = 355
          Height = 16
          Caption = 'Click the image field (below) to Load a new image from Disk.'
        end
        object eAddr: TEdit
          Left = 8
          Top = 28
          Width = 189
          Height = 24
          TabOrder = 0
          Text = 'localhost'
          OnChange = eAddrChange
        end
        object ePort: TEdit
          Left = 208
          Top = 28
          Width = 81
          Height = 24
          TabOrder = 1
          Text = '81'
          OnChange = eAddrChange
        end
      end
    end
    object DBMemo1: TDBMemo
      Left = 456
      Top = 1
      Width = 324
      Height = 339
      Align = alClient
      DataField = 'Notes'
      DataSource = DataSource1
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 344
    Width = 781
    Height = 209
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -13
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 16
    Top = 160
  end
  object RtcHttpClient1: TRtcHttpClient
    ServerAddr = 'localhost'
    ServerPort = '81'
    ReconnectOn.ConnectError = True
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    AutoConnect = True
    Left = 16
    Top = 112
  end
  object RtcClientModule1: TRtcClientModule
    AutoSyncEvents = True
    Client = RtcHttpClient1
    Compression = cDefault
    AutoRepost = 2
    ModuleFileName = '/biolife'
    Left = 48
    Top = 112
  end
  object RtcResult1: TRtcResult
    OnReturn = RtcResult1Return
    RequestAborted = RtcResult1RequestAborted
    Left = 80
    Top = 112
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 49
    Top = 161
  end
  object RtcDataSetMonitor1: TRtcDataSetMonitor
    DataSet = ClientDataSet1
    OnDataChange = RtcDataSetMonitor1DataChange
    Left = 81
    Top = 161
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 397
    Top = 129
  end
end
