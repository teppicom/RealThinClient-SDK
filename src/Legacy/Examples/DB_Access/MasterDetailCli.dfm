object Form1: TForm1
  Left = 263
  Top = 372
  Width = 770
  Height = 585
  Caption = 'RTC Master Detail Client (RtcMemDataSet version)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 373
    Top = 97
    Height = 443
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 94
    Width = 752
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel5: TPanel
    Left = 0
    Top = 0
    Width = 752
    Height = 61
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 94
      Height = 16
      Caption = 'Server Address'
    end
    object Label2: TLabel
      Left = 236
      Top = 8
      Width = 67
      Height = 16
      Caption = 'Server Port'
    end
    object btnRefreshDataSet1: TSpeedButton
      Left = 328
      Top = 8
      Width = 169
      Height = 45
      Caption = 'Load Customer data'
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
      OnClick = btnRefreshDataSet1Click
    end
    object eAddr: TEdit
      Left = 8
      Top = 28
      Width = 217
      Height = 24
      TabOrder = 0
      Text = 'localhost'
      OnChange = eAddrChange
    end
    object ePort: TEdit
      Left = 236
      Top = 28
      Width = 81
      Height = 24
      TabOrder = 1
      Text = '81'
      OnChange = eAddrChange
    end
    object xValidCheck: TCheckBox
      Left = 512
      Top = 20
      Width = 205
      Height = 25
      Caption = 'Client-side Validity Checks'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 97
    Width = 373
    Height = 443
    Align = alLeft
    TabOrder = 1
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 371
      Height = 52
      Align = alTop
      TabOrder = 0
      object Label3: TLabel
        Left = 1
        Top = 1
        Width = 369
        Height = 16
        Align = alTop
        Caption = 'Customers (Master DataSet)'
      end
      object DBNavigator1: TDBNavigator
        Left = 1
        Top = 17
        Width = 369
        Height = 34
        DataSource = DataSource1
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
        Align = alClient
        TabOrder = 0
      end
    end
    object DBGrid1: TDBGrid
      Left = 1
      Top = 53
      Width = 371
      Height = 389
      Align = alClient
      DataSource = DataSource1
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -13
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 61
    Width = 752
    Height = 33
    Align = alTop
    TabOrder = 2
    object lStatus: TLabel
      Left = 8
      Top = 8
      Width = 632
      Height = 16
      Caption = 
        'Start the BDEDemoServer, enter Server Address and Port here, the' +
        'n click the "Load Customer data" button.'
    end
  end
  object Panel3: TPanel
    Left = 376
    Top = 97
    Width = 376
    Height = 443
    Align = alClient
    TabOrder = 3
    object Splitter2: TSplitter
      Left = 1
      Top = 221
      Width = 374
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object Panel7: TPanel
      Left = 1
      Top = 224
      Width = 374
      Height = 218
      Align = alClient
      TabOrder = 0
      object Panel9: TPanel
        Left = 1
        Top = 1
        Width = 372
        Height = 52
        Align = alTop
        TabOrder = 0
        object Label5: TLabel
          Left = 1
          Top = 1
          Width = 370
          Height = 16
          Align = alTop
          Caption = 'Items (Order Details)'
        end
        object DBNavigator3: TDBNavigator
          Left = 41
          Top = 17
          Width = 330
          Height = 34
          DataSource = DataSource3
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          Align = alClient
          TabOrder = 0
        end
        object Panel11: TPanel
          Left = 1
          Top = 17
          Width = 40
          Height = 34
          Align = alLeft
          TabOrder = 1
          object btnRefreshDataSet3: TSpeedButton
            Left = 4
            Top = 5
            Width = 29
            Height = 24
            Hint = 'Reload Items'
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
            OnClick = btnRefreshDataSet3Click
          end
        end
      end
      object DBGrid3: TDBGrid
        Left = 1
        Top = 53
        Width = 372
        Height = 164
        Align = alClient
        DataSource = DataSource3
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 374
      Height = 220
      Align = alTop
      TabOrder = 1
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 372
        Height = 52
        Align = alTop
        TabOrder = 0
        object Label4: TLabel
          Left = 1
          Top = 1
          Width = 370
          Height = 16
          Align = alTop
          Caption = 'Orders (Customer Details)'
        end
        object DBNavigator2: TDBNavigator
          Left = 41
          Top = 17
          Width = 330
          Height = 34
          DataSource = DataSource2
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          Align = alClient
          TabOrder = 0
        end
        object Panel10: TPanel
          Left = 1
          Top = 17
          Width = 40
          Height = 34
          Align = alLeft
          TabOrder = 1
          object btnRefreshDataSet2: TSpeedButton
            Left = 3
            Top = 4
            Width = 30
            Height = 26
            Hint = 'Reload Orders'
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
            OnClick = btnRefreshDataSet2Click
          end
        end
      end
      object DBGrid2: TDBGrid
        Left = 1
        Top = 53
        Width = 372
        Height = 166
        Align = alClient
        DataSource = DataSource2
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
  end
  object RtcHttpClient1: TRtcHttpClient
    ServerAddr = 'localhost'
    ServerPort = '81'
    ReconnectOn.ConnectError = True
    ReconnectOn.ConnectLost = True
    ReconnectOn.ConnectFail = True
    AutoConnect = True
    Left = 20
    Top = 200
  end
  object RtcClientModule1: TRtcClientModule
    AutoSyncEvents = True
    Client = RtcHttpClient1
    Compression = cDefault
    AutoRepost = 2
    ModuleFileName = '/biolife'
    Left = 52
    Top = 200
  end
  object RtcResult1: TRtcResult
    OnReturn = RtcResult1Return
    PreparingCall = RtcResult1PreparingCall
    RequestAborted = RtcResult1RequestAborted
    Left = 84
    Top = 200
  end
  object RtcMemDataSet1: TRtcMemDataSet
    TrackChanges = True
    OnDataChange = RtcMemDataSetDataChange
    AfterOpen = RtcMemDataSet1Scrolled
    BeforePost = RtcMemDataSet1BeforePost
    BeforeDelete = RtcMemDataSet1BeforeDelete
    AfterScroll = RtcMemDataSet1Scrolled
    Left = 20
    Top = 236
  end
  object DataSource1: TDataSource
    DataSet = RtcMemDataSet1
    Left = 52
    Top = 236
  end
  object RtcMemDataSet2: TRtcMemDataSet
    TrackChanges = True
    OnDataChange = RtcMemDataSetDataChange
    AfterOpen = RtcMemDataSet2Scrolled
    BeforePost = RtcMemDataSet2BeforePost
    BeforeDelete = RtcMemDataSet2BeforeDelete
    AfterScroll = RtcMemDataSet2Scrolled
    OnNewRecord = RtcMemDataSet2NewRecord
    Left = 20
    Top = 272
  end
  object DataSource2: TDataSource
    DataSet = RtcMemDataSet2
    Left = 52
    Top = 272
  end
  object RtcMemDataSet3: TRtcMemDataSet
    TrackChanges = True
    OnDataChange = RtcMemDataSetDataChange
    BeforePost = RtcMemDataSet3BeforePost
    OnNewRecord = RtcMemDataSet3NewRecord
    Left = 20
    Top = 308
  end
  object DataSource3: TDataSource
    DataSet = RtcMemDataSet3
    Left = 52
    Top = 308
  end
end
