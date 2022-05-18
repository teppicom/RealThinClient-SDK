object Messenger_Provider: TMessenger_Provider
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 390
  Top = 169
  Height = 355
  Width = 294
  object Module: TRtcServerModule
    Link = ServerLink
    DataFormats = [fmt_RTC, fmt_XMLRPC, fmt_JSONrpc1, fmt_JSONrpc2]
    EncryptionKey = 16
    AutoSessions = True
    AutoSessionsLive = 40
    ModuleFileName = '/$MSG'
    FunctionGroup = MsgFunctions
    OnSessionClose = ModuleSessionClose
    Left = 96
    Top = 15
  end
  object MsgFunctions: TRtcFunctionGroup
    Left = 168
    Top = 15
  end
  object MsgLogin: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'Login'
    OnExecute = MsgLoginExecute
    Left = 28
    Top = 85
  end
  object MsgRegister: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'Register'
    OnExecute = MsgRegisterExecute
    Left = 28
    Top = 140
  end
  object MsgSendText: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'SendText'
    OnExecute = MsgSendTextExecute
    Left = 212
    Top = 85
  end
  object MsgGetData: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'GetData'
    OnExecute = MsgGetDataExecute
    Left = 212
    Top = 140
  end
  object ServerLink: TRtcDataServerLink
    Left = 24
    Top = 15
  end
  object MsgAddFriend: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'AddFriend'
    OnExecute = MsgAddFriendExecute
    Left = 120
    Top = 85
  end
  object MsgDelFriend: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'DelFriend'
    OnExecute = MsgDelFriendExecute
    Left = 120
    Top = 140
  end
  object MsgAddIgnore: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'AddIgnore'
    OnExecute = MsgAddIgnoreExecute
    Left = 120
    Top = 195
  end
  object MsgDelIgnore: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'DelIgnore'
    OnExecute = MsgDelIgnoreExecute
    Left = 120
    Top = 250
  end
  object MsgLogOut: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'Logout'
    OnExecute = MsgLogOutExecute
    Left = 28
    Top = 195
  end
  object MsgLogin2: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'Login2'
    OnExecute = MsgLogin2Execute
    Left = 28
    Top = 250
  end
  object MsgPing: TRtcFunction
    Group = MsgFunctions
    FunctionName = 'Ping'
    OnExecute = MsgPingExecute
    Left = 212
    Top = 195
  end
end
