{
  @exclude

  @html(<b>)
  StreamSec SSL Plug-in
  @html(</b>)
  - Copyright (c) Henrick Hellstrom & Danijel Tkalcec
  @html(<br><br>)

  This unit defines Plug-ins for using StreamSec Mobile TLS 1.0 components
  with rtcHttpClient and rtcHttpServer for SSL encryption/decryption.
}
unit rtcSTMTPlugin;

{$DEFINE ST40} // Define for StreamSec Tools 4.0

interface

uses
  Classes, SysUtils,

  // RTC SDK
  rtcSystem,
  rtcPlugins,

  // StreamSec Tools 4.0
  {$IFDEF ST40}
  stSecUtils,
  StreamSec.DSI.PkixCert,
  stTlsUtils,
  {$ELSE}
  StreamSec.Mobile.SecUtils,
  StreamSec.Mobile.PkixCert,
  StreamSec.Mobile.TlsUtils,
  {$ENDIF}
  StreamSec.Mobile.Tls,
  StreamSec.Mobile.TlsClass,
  StreamSec.Mobile.TlsConst,
  StreamSec.Mobile.StreamSecII,
  StreamSec.Mobile.TlsInternalServer;

type
  // Basic StreamSec Plug-in implementation
  TRtcSTMTPlugin = class(TRtcCryptPlugin)
  private
    FTLSServer: TsmCustomTLSInternalServer;
    procedure SetTLSServer(const Value: TsmCustomTLSInternalServer);
  protected
    procedure CheckTLSServer; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Called before we do a graceful disconnect, in case some data has to be sent out. }
    function BeforeDisconnectEx(var ConnCryptObj:TObject; var OutData:RtcByteArray): TRtcCryptPluginState; override;

    { Called after a connection was closed. }
    procedure AfterDisconnectEx(var ConnCryptObj:TObject); override;

    { Called when data arrived.
      InData = data received from recipient (decode that!)
      OutData = data prepared by "decoder" for sending back to recipient (encoded data)
      OutPlainText = decrypted input data (for use by application) }
    function DataReceivedEx(var ConnCryptObj:TObject; const InData:RtcByteArray; var OutData:RtcByteArray; var OutPlainText:RtcByteArray): TRtcCryptPluginState; override;

    { Called when data needs to be sent.
      InData = application data which we want to be encoded for sending
      OutData = encoded data which should be sent out }
    function DataToSendEx(var ConnCryptObj:TObject; const InData:RtcByteArray; var OutData:RtcByteArray): TRtcCryptPluginState; override;
  published
    property TLSServer: TsmCustomTLSInternalServer read FTLSServer write SetTLSServer;
    end;

  { StreamSec Plugin for Server-side SSL encryption/decryption }
  TRtcSTMTServerPlugin = class(TRtcSTMTPlugin)
  protected
    procedure CheckTLSServer; override;
  public
    { Called after a new connection was established.
      OutData = data which has to be sent out immediately. }
    function AfterConnectEx(var ConnCryptObj:TObject; var OutData:RtcByteArray;
                            Protocol: TRtcCryptPluginProtocol; const RemoteAddr, RemoteIP:RtcString): TRtcCryptPluginState; override;

  end;

  { StreamSec Plugin for client-side SSL encryption/decryption }
  TRtcSTMTClientPlugin = class(TRtcSTMTPlugin)
  private
    FCertificateNameMap: TStrings;
    procedure SetCertificateNameMap(const Value: TStrings);
  protected
    procedure CheckTLSServer; override;
  published
  public
    destructor Destroy; override;
    { Called after a new connection was established.
      OutData = data which has to be sent out immediately. }
    function AfterConnectEx(var ConnCryptObj:TObject; var OutData:RtcByteArray;
                            Protocol: TRtcCryptPluginProtocol; const RemoteAddr, RemoteIP:RtcString): TRtcCryptPluginState; override;

    procedure AddCertificateNameMap(const aRemoteAddr, aCertName: string);
    property CertificateNameMap: TStrings read FCertificateNameMap write SetCertificateNameMap;
  end;

  TRtcSTMTConnCryptObject = class
  private
    fTLSLayer: TCustomTLS_ContentLayer;
    fBuffer: RtcByteArray;
    fIP: RtcString;
    fAddress: RtcString;
    function GetSecurityParams: PTLSSecurityParams;
    function GetBulkCipherAlgorithm: TBulkCipherAlgorithm;
    function GetCipherType: TCipherType;
    function GetClientCert: iCertificate;
    function GetCompressionAlgorithm: TCompressionMethod;
    function GetEncrypted: Boolean;
    function GetEntity: TConnectionEnd;
    function GetIsExportable: Boolean;
    function GetKeyMaterialLength: Word;
    function GetKeySize: Word;
    function GetMacAlgorithm: THashAlgorithm;
    function GetServerCert: iCertificate;
    procedure SetAddress(const Value: RtcString);
    procedure SetIP(const Value: RtcString);
  public
    property Encrypted: Boolean read GetEncrypted;
    property ServerCert: iCertificate read GetServerCert;
    property ClientCert: iCertificate read GetClientCert;
    property Entity: TConnectionEnd read GetEntity;
    property BulkCipherAlgorithm: TBulkCipherAlgorithm read GetBulkCipherAlgorithm;
    property CipherType: TCipherType read GetCipherType;
    property KeySize: Word read GetKeySize;
    property KeyMaterialLength: Word read GetKeyMaterialLength;
    property IsExportable: Boolean read GetIsExportable;
    property MacAlgorithm: THashAlgorithm read GetMacAlgorithm;
    property CompressionAlgorithm: TCompressionMethod read GetCompressionAlgorithm;
    property IP: RtcString read fIP write SetIP;
    property Address: RtcString read fAddress write SetAddress;
  end;

implementation

{ TRtcSSecPlugin }

procedure TRtcSTMTPlugin.AfterDisconnectEx(var ConnCryptObj: TObject);
var
  lDummy: RtcByteArray;
begin
  BeforeDisconnectEx(ConnCryptObj,lDummy);
end;

function TRtcSTMTPlugin.BeforeDisconnectEx(var ConnCryptObj: TObject;
   var OutData: RtcByteArray): TRtcCryptPluginState;
var
  lObj: TRtcSTMTConnCryptObject;
  lRes: TRtcByteArrayStream;
  lTLS: TCustomTLS_ContentLayer;
begin
  Result := cpsReady;
  if Assigned(ConnCryptObj) then begin
    lObj := ConnCryptObj as TRtcSTMTConnCryptObject;
    lTLS := lObj.fTLSLayer;
    lObj.fTLSLayer := nil;
    if Assigned(lTLS) then begin
      lRes := TRtcByteArrayStream.Create(nil);
      try
        lTLS.Close(lRes);
        OutData := lRes.GetBytes;
      finally
        lRes.Free;
      end;
      lTLS.Release;
    end;
    FreeAndNil(ConnCryptObj);
    if Assigned(FTLSServer) then
      FTLSServer.ClearExpiredSessionKeys(False);
  end;
end;

function TRtcSTMTPlugin.DataReceivedEx(var ConnCryptObj: TObject;
  const InData: RtcByteArray; var OutData, OutPlainText: RtcByteArray): TRtcCryptPluginState;
var
  lObj: TRtcSTMTConnCryptObject;
  lIn, lOut, lPT: TRtcByteArrayStream;
  Client: TCustomTLS_ContentLayer;
begin
  if Assigned(ConnCryptObj) then begin
    lObj := ConnCryptObj as TRtcSTMTConnCryptObject;
    Client := lObj.fTLSLayer;
    lIn := TRtcByteArrayStream.Create(InData);
    lOut := TRtcByteArrayStream.Create(nil);
    lPT := TRtcByteArrayStream.Create(nil);
    try
      if not Assigned(Client) then begin
        if lIn.Size > 0 then begin
          Assert(Assigned(FTLSServer),'TLSServer must be assigned');
          Client := FTLSServer.TLSAddServerSession;
          Client.Unmanaged := True;
          Client.UserData := self;
          Client.IPToCheck := string(lObj.IP);
          Client.DNSNameToCheck := string(lObj.Address);
          Client.URIToCheck := string(lObj.Address);
          lObj.fTLSLayer := Client;
          Client.Accept(lIn,lOut);
        end else begin
          Result := cpsClosed;
          Exit;
        end;
      end;

      while lIn.Size>lIn.Position do begin
        if not Client.Active then begin
          Result := cpsClosed;
          Exit;
        end;
        Client.DecodeData(lIn,lPT,lOut);
      end;

      if not Client.Encrypted then
        Result := cpsWaiting
      else begin
        Result := cpsReady;
        if length(lObj.fBuffer)>0 then begin
          lIn.Size := 0;
          lIn.WriteBytes(lObj.fBuffer);
          lIn.Position := 0;
          SetLength(lObj.fBuffer,0);
          while lIn.Size>lIn.Position do
            Client.EncodeData(lIn,lOut);
        end;
      end;
      OutData := lOut.GetBytes;
      OutPlainText := lPT.GetBytes;
    finally
      lIn.Free;
      lOut.Free;
      lPT.Free;
    end;
  end else
    Result := cpsClosed;
end;

function TRtcSTMTPlugin.DataToSendEx(var ConnCryptObj: TObject;
  const InData: RtcByteArray; var OutData: RtcByteArray): TRtcCryptPluginState;
var
  lObj: TRtcSTMTConnCryptObject;
  lIn, lOut: TRtcByteArrayStream;
  Client: TCustomTLS_ContentLayer;
begin
  if Assigned(ConnCryptObj) then begin
    lObj := ConnCryptObj as TRtcSTMTConnCryptObject;
    Client := lObj.fTLSLayer;
    if (not Assigned(Client)) or (not Client.Active) then
      Result := cpsClosed
    else if Client.Encrypted then begin
      lIn := TRtcByteArrayStream.Create(lObj.fBuffer);
      lIn.Seek(0,soFromEnd);
      lIn.WriteBytes(InData);
      lIn.Seek(0,soFromBeginning);
      lOut := TRtcByteArrayStream.Create(nil);
      try
        SetLength(lObj.fBuffer,0);
        while lIn.Size > lIn.Position do
          Client.EncodeData(lIn,lOut);

        OutData := lOut.GetBytes;
      finally
        lIn.Free;
        lOut.Free;
      end;
      Result := cpsReady;
    end else begin
      AddBytes(lObj.fBuffer,InData);
      SetLength(OutData,0);
      Result := cpsWaiting;
    end;
  end else
    Result := cpsClosed;
end;

procedure TRtcSTMTPlugin.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTLSServer then
      FTLSServer := nil;
  end;
end;

procedure TRtcSTMTPlugin.SetTLSServer(const Value: TsmCustomTLSInternalServer);
begin
  if Assigned(FTLSServer) then
    FTLSServer.RemoveFreeNotification(Self);
  FTLSServer := Value;
  CheckTLSServer;
  if Assigned(FTLSServer) then begin
    FTLSServer.FreeNotification(Self);
  end;
end;

{ TRtcSSecServerPlugin }

type
  TTLSServerHack = class(TsmCustomTLSInternalServer);

function TRtcSTMTServerPlugin.AfterConnectEx(var ConnCryptObj: TObject;
  var OutData: RtcByteArray; Protocol: TRtcCryptPluginProtocol;
  const RemoteAddr, RemoteIP: RtcString): TRtcCryptPluginState;
var
  lObj: TRtcSTMTConnCryptObject;
begin
  Assert(Assigned(FTLSServer),'TLSServer must be assigned');
  SetLength(OutData,0);
  lObj := TRtcSTMTConnCryptObject.Create;
  lObj.IP := RemoteIP;
  lObj.Address := '';

  ConnCryptObj := lObj;

  Result := cpsWaiting;
end;

procedure TRtcSTMTServerPlugin.CheckTLSServer;
begin
  if Assigned(FTLSServer) then
    if FTLSServer.ClientOrServer = cosClientSide then begin
      FTLSServer := nil;
      raise Exception.Create('ClientOrServer must be set to cosServerSide');
    end;
end;

{ TRtcSSecClientPlugin }

procedure TRtcSTMTClientPlugin.AddCertificateNameMap(const aRemoteAddr,
  aCertName: string);
begin
  if not Assigned(FCertificateNameMap) then
    FCertificateNameMap := TStringList.Create;
  FCertificateNameMap.Values[aRemoteAddr] := aCertName;
end;

function TRtcSTMTClientPlugin.AfterConnectEx(var ConnCryptObj: TObject;
  var OutData: RtcByteArray; Protocol: TRtcCryptPluginProtocol;
  const RemoteAddr, RemoteIP:RtcString): TRtcCryptPluginState;
var
  lRes: TRtcByteArrayStream;
  Client: TCustomTLS_ContentLayer;
  lObj: TRtcSTMTConnCryptObject;
  Idx: Integer;
  IdName, Addr: string;
begin
  Assert(Assigned(FTLSServer),'TLSServer must be assigned');
  lObj := TRtcSTMTConnCryptObject.Create;
  lObj.IP := RemoteIP;
  { RemoteAddr = RemoteIP will happen if an IP address is entered as ServerAddr
    on the RTC client component. NOTE that this will SKIP certificate name
    verification, except if the server certificate has specified an IP altname.
    This diverges from how some other clients work. }
  if RemoteAddr = RemoteIP then
    lObj.Address := ''
  else
    lObj.Address := RemoteAddr;
  if Assigned(FCertificateNameMap) then begin
    Idx := FCertificateNameMap.IndexOfName(string(RemoteAddr));
    if Idx >= 0 then
      IdName := string(RemoteAddr)
    else begin
      Idx := FCertificateNameMap.IndexOfName(string(RemoteIP));
      if Idx >= 0 then
        IdName := string(RemoteIP)
      else begin
        Idx := FCertificateNameMap.IndexOfName('*');
        if Idx >= 0 then
          IdName := '*';
      end;
    end;
    if Idx >= 0 then begin
      Addr := FCertificateNameMap.Values[IdName];
      if Addr = '*' then
        lObj.Address := ''
      else
        lObj.Address := RtcString(Addr);
      lObj.IP := '';
    end;
  end;
  ConnCryptObj := lObj;
  lRes := TRtcByteArrayStream.Create(nil);
  try
    Client := FTLSServer.TLSAddClientSession;
    Client.Unmanaged := True;
    Client.UserData := self;
    TRtcSTMTConnCryptObject(ConnCryptObj).fTLSLayer := Client;
    Client.IPToCheck := string(lObj.IP);
    Client.DNSNameToCheck := string(lObj.Address);
    Client.URIToCheck := string(lObj.Address);
    Client.Connect(lRes);
    OutData := lRes.GetBytes;
    Result := cpsWaiting;
  finally
    lRes.Free;
  end;
end;

procedure TRtcSTMTClientPlugin.CheckTLSServer;
begin
  if Assigned(FTLSServer) then
    if FTLSServer.ClientOrServer = cosServerSide then begin
      FTLSServer := nil;
      raise Exception.Create('ClientOrServer must be set to cosClientSide');
    end;
end;

destructor TRtcSTMTClientPlugin.Destroy;
begin
  if assigned(FCertificateNameMap) then
    FreeAndNil(FCertificateNameMap);
  inherited;
end;

procedure TRtcSTMTClientPlugin.SetCertificateNameMap(const Value: TStrings);
begin
  if not Assigned(FCertificateNameMap) then
    FCertificateNameMap := TStringList.Create;
  FCertificateNameMap.Assign(Value);
end;

{ TRtcSSecConnCryptObject }

function TRtcSTMTConnCryptObject.GetBulkCipherAlgorithm: TBulkCipherAlgorithm;
begin
  if GetEncrypted then
    Result := GetSecurityParams.bulk_cipher_algorithm
  else
    Result := bcaNull;
end;

function TRtcSTMTConnCryptObject.GetCipherType: TCipherType;
begin
  if GetEncrypted then
    Result := GetSecurityParams.cipher_type
  else
    Result := ctStream;
end;

function TRtcSTMTConnCryptObject.GetClientCert: iCertificate;
var
  lCert: iCertificate;
begin
  Result := nil;
  if Assigned(fTLSLayer) then begin
    lCert := fTLSLayer.ClientCertificate;
    if Assigned(lCert) then
      Result := lCert;
  end;
end;

function TRtcSTMTConnCryptObject.GetCompressionAlgorithm: TCompressionMethod;
begin
  if GetEncrypted then
    Result := GetSecurityParams.compression_algorithm
  else
    Result := cmNull;
end;

function TRtcSTMTConnCryptObject.GetEncrypted: Boolean;
begin
  Result := False;
  if Assigned(fTLSLayer) then
    if fTLSLayer.Active then
      if fTLSLayer.Encrypted then
        Result := True;
end;

function TRtcSTMTConnCryptObject.GetEntity: TConnectionEnd;
begin
  if GetEncrypted then
    Result := GetSecurityParams.entity
  else
    Result := ceServer;
end;

function TRtcSTMTConnCryptObject.GetIsExportable: Boolean;
begin
  if GetEncrypted then
    Result := GetSecurityParams.is_exportable
  else
    Result := True;
end;

function TRtcSTMTConnCryptObject.GetKeyMaterialLength: Word;
begin
  if GetEncrypted then
    Result := GetSecurityParams.key_material_length
  else
    Result := 0;
end;

function TRtcSTMTConnCryptObject.GetKeySize: Word;
begin
  if GetEncrypted then
    Result := GetSecurityParams.key_size
  else
    Result := 0;
end;

function TRtcSTMTConnCryptObject.GetMacAlgorithm: THashAlgorithm;
begin
  if GetEncrypted then
    Result := GetSecurityParams.mac_algorithm
  else
    Result := haNull;
end;

function TRtcSTMTConnCryptObject.GetSecurityParams: PTLSSecurityParams;
begin
  if Assigned(fTLSLayer) then
    Result := fTLSLayer.Context
  else
    Result := nil;
end;

function TRtcSTMTConnCryptObject.GetServerCert: iCertificate;
var
  lCert: iCertificate;
begin
  Result := nil;
  if Assigned(fTLSLayer) then begin
    lCert := fTLSLayer.ServerCertificate;
    if Assigned(lCert) then
      Result := lCert;
  end;
end;

procedure TRtcSTMTConnCryptObject.SetAddress(const Value: RtcString);
begin
  fAddress := Value;
end;

procedure TRtcSTMTConnCryptObject.SetIP(const Value: RtcString);
begin
  fIP := Value;
end;

end.
