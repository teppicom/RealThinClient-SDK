object AppSrv_Module: TAppSrv_Module
  OldCreateOrder = False
  Left = 210
  Top = 166
  Height = 239
  Width = 308
  object ServerModule: TRtcServerModule
    Link = ServerLink
    DataFormats = [fmt_RTC, fmt_XMLRPC]
    SecureKey = 'This is a test.'
    AutoSessionsLive = 600
    ModuleFileName = '/TEST'
    FunctionGroup = FuncGroup
    Left = 96
    Top = 25
  end
  object FuncGroup: TRtcFunctionGroup
    Left = 188
    Top = 25
  end
  object MulFunc: TRtcFunction
    Group = FuncGroup
    FunctionName = 'Mul'
    OnExecute = MulFuncExecute
    Left = 100
    Top = 90
  end
  object AddFunc: TRtcFunction
    Group = FuncGroup
    FunctionName = 'Add'
    OnExecute = AddFuncExecute
    Left = 28
    Top = 90
  end
  object ServerLink: TRtcDataServerLink
    Left = 24
    Top = 25
  end
  object LoopoFunc: TRtcFunction
    Group = FuncGroup
    FunctionName = 'loopo'
    OnExecute = LoopoFuncExecute
    Left = 172
    Top = 90
  end
end
