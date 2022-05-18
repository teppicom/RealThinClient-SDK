{
  @html(<b>)
  Encryption/Decryption Plug-in
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit defines a Plug-in class, which should be extended when implementing
  third-party plug-ins for SSL/SSH encryption/decryption using RTC SDK components.
}
unit rtcPlugins;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes;

type
  TRtcCryptPluginState = (
      // connection closed
      cpsClosed, 
      // waiting for data
      cpsWaiting, 
      // data ready (HTTP/1.1)
      cpsReady, 
      // data ready (HTTP/2.0)
      cpsReady2);

  TRtcCryptPluginProtocol = (
      // TCP/IP
      cppTcp, 
      // HTTP/1.1
      cppHttp, 
      // HTTP/2.0
      cppHttp2);

  { @abstract(Cryptography Plugin)
    This is a basic class for any kind of third-party encryption/decryption
    plug-ins used by rtcHttpClient and rtcHttpServer components. }
  TRtcCryptPlugin = class(TComponent)
  public
    { Called after a new connection was established.
      OutData = data which has to be sent out immediately. }
    function AfterConnectEx(var ConnCryptObj:TObject;
                            var OutData:RtcByteArray;
                            Protocol: TRtcCryptPluginProtocol;
                            const RemoteAddr, RemoteIP:RtcString):TRtcCryptPluginState; virtual; abstract;

    { Called before we do a graceful disconnect, in case some data has to be sent out.
      OutData = data which has to be sent out immediately. }
    function BeforeDisconnectEx(var ConnCryptObj:TObject;
                                var OutData:RtcByteArray):TRtcCryptPluginState;  virtual; abstract;

    { Called after a connection was closed. }
    procedure AfterDisconnectEx(var ConnCryptObj:TObject); overload; virtual; abstract;

    { Called when data arrived.
      InData = data received from recipient (decode that!)
      OutData = data prepared by "decoder" for sending back to recipient (encoded data)
      OutPlainText = decrypted input data (for use by application) }
    function DataReceivedEx(var ConnCryptObj:TObject;
                            const InData:RtcByteArray;
                            var OutData:RtcByteArray;
                            var OutPlainText:RtcByteArray):TRtcCryptPluginState;  virtual; abstract;

    { Called when data needs to be sent.
      InData = application data which we want to be encoded for sending
      OutData = encoded data which should be sent out }
    function DataToSendEx(var ConnCryptObj:TObject;
                          const InData:RtcByteArray;
                          var OutData:RtcByteArray):TRtcCryptPluginState;  virtual; abstract;
    end;

  { @abstract(Dummy Cryptography Plugin)
    This is a a dummy implementation which will not be doing anything else but pass all data through.
    This implementation is used purely for testing the RTC SDK's plugin functionality. }
  TRtcDummyCryptPlugin=class(TRtcCryptPlugin)
  public
    function AfterConnectEx(var ConnCryptObj:TObject;
                            var OutData:RtcByteArray;
                            Protocol: TRtcCryptPluginProtocol;
                            const RemoteAddr, RemoteIP:RtcString):TRtcCryptPluginState; override;

    function BeforeDisconnectEx(var ConnCryptObj:TObject;
                                var OutData:RtcByteArray):TRtcCryptPluginState;  override;

    procedure AfterDisconnectEx(var ConnCryptObj:TObject); override;

    function DataReceivedEx(var ConnCryptObj:TObject;
                            const InData:RtcByteArray;
                            var OutData:RtcByteArray;
                            var OutPlainText:RtcByteArray):TRtcCryptPluginState;  override;

    function DataToSendEx(var ConnCryptObj:TObject;
                          const InData:RtcByteArray;
                          var OutData:RtcByteArray):TRtcCryptPluginState;  override;
    end;

implementation

{ TRtcDummyCryptPlugin }

type
  TRtcDummyConnCryptObj=class(TObject)
  public
    Protocol:TRtcCryptPluginProtocol;
    RemoteAddr,RemoteIP:RtcString;
    end;

function TRtcDummyCryptPlugin.AfterConnectEx(var ConnCryptObj:TObject; var OutData:RtcByteArray;
                                             Protocol: TRtcCryptPluginProtocol;
                                             const RemoteAddr, RemoteIP:RtcString):TRtcCryptPluginState;
  begin
  ConnCryptObj:=TRtcDummyConnCryptObj.Create;
  TRtcDummyConnCryptObj(ConnCryptObj).Protocol:=Protocol;
  TRtcDummyConnCryptObj(ConnCryptObj).RemoteAddr:=RemoteAddr;
  TRtcDummyConnCryptObj(ConnCryptObj).RemoteIP:=RemoteIP;
  Result:=cpsReady;
  end;

function TRtcDummyCryptPlugin.DataReceivedEx(var ConnCryptObj: TObject; const InData: RtcByteArray; var OutData, OutPlainText: RtcByteArray): TRtcCryptPluginState;
  begin
  OutPlainText := InData;
  Result:=cpsReady;
  end;

function TRtcDummyCryptPlugin.DataToSendEx(var ConnCryptObj: TObject; const InData: RtcByteArray; var OutData: RtcByteArray): TRtcCryptPluginState;
  begin
  OutData:=InData;
  Result:=cpsReady;
  end;

function TRtcDummyCryptPlugin.BeforeDisconnectEx(var ConnCryptObj: TObject; var OutData: RtcByteArray): TRtcCryptPluginState;
  begin
  Result:=cpsReady;
  end;

procedure TRtcDummyCryptPlugin.AfterDisconnectEx(var ConnCryptObj: TObject);
  begin
  if assigned(ConnCryptObj) then
    RtcFreeAndNil(ConnCryptObj);
  end;

end.
