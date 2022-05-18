object Form2: TForm2
  Left = 540
  Top = 150
  Width = 799
  Height = 598
  Caption = 'RTC Fish Facts Client (RtcMemDataSet version)'
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
    Top = 357
    Width = 781
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 781
    Height = 357
    Align = alTop
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 453
      Top = 1
      Height = 355
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 452
      Height = 355
      Align = alLeft
      TabOrder = 0
      object DBImage1: TDBImage
        Left = 1
        Top = 166
        Width = 450
        Height = 188
        Align = alClient
        BorderStyle = bsNone
        DataField = 'Graphic'
        DataSource = DataSource1
        TabOrder = 0
        OnClick = DBImage1Click
      end
      object Panel3: TPanel
        Left = 1
        Top = 125
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
        Height = 124
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
          Left = 204
          Top = 8
          Width = 67
          Height = 16
          Caption = 'Server Port'
        end
        object btnRefresh: TSpeedButton
          Left = 300
          Top = 40
          Width = 141
          Height = 45
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
          Top = 104
          Width = 372
          Height = 16
          Caption = 'Click the image field (below) to Load a new image file from Disk'
        end
        object Label4: TLabel
          Left = 8
          Top = 56
          Width = 110
          Height = 16
          Caption = 'Module File Name'
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
          Left = 204
          Top = 28
          Width = 81
          Height = 24
          TabOrder = 1
          Text = '81'
          OnChange = eAddrChange
        end
        object eModuleFileName: TEdit
          Left = 8
          Top = 76
          Width = 277
          Height = 24
          TabOrder = 2
          Text = '/biolife'
          OnChange = eAddrChange
        end
      end
    end
    object DBMemo1: TDBMemo
      Left = 456
      Top = 1
      Width = 324
      Height = 355
      Align = alClient
      DataField = 'Notes'
      DataSource = DataSource1
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 360
    Width = 781
    Height = 193
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
    DataSet = RtcMemDataSet1
    Left = 44
    Top = 228
  end
  object RtcMemDataSet1: TRtcMemDataSet
    TrackChanges = True
    OnDataChange = RtcMemDataSet1DataChange
    Left = 12
    Top = 228
  end
  object RtcHttpClient1: TRtcHttpClient
    ServerAddr = 'localhost'
    ServerPort = '81'
    ReconnectOn.ConnectError = True
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    AutoConnect = True
    Left = 12
    Top = 180
  end
  object RtcClientModule1: TRtcClientModule
    AutoSyncEvents = True
    Client = RtcHttpClient1
    Compression = cDefault
    AutoRepost = 2
    ModuleFileName = '/biolife'
    Left = 44
    Top = 180
  end
  object RtcResult1: TRtcResult
    OnReturn = RtcResult1Return
    RequestAborted = RtcResult1RequestAborted
    Left = 76
    Top = 180
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 125
    Top = 177
  end
end
