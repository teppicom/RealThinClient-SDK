unit rtcSSecTest;

{$include rtcDefs.inc}

{.$DEFINE DEMO} // Undefine this for default production settings
{.$DEFINE STREAMSECMOBILE} // Define for StreamSec Mobile TLS 1.0
{.$DEFINE ST40} // Define for StreamSec Tools 4.0

{$IFDEF ST40}{$DEFINE STREAMSECMOBILE}{$ENDIF} // Do not modify

interface

uses
  SysUtils, SyncObjs, Classes,
  rtcSystem, rtcPlugins, rtcLog,

  {$IFNDEF STREAMSECMOBILE}
  // Required for "Application.ProcessMessages" ...
  Forms,
  // StreamSec Tools II ...
  MpX509, TlsInternalServer, rtcSSecPlugin,
  SecUtils, StreamSecII, MPYarrow, SecComp,
  Asn1, TlsClass, TlsConst, X509Base
  {$ELSE}
  // StreamSec Mobile Tools 1.0 ...
  StreamSec.Mobile.X509Comp,
  StreamSec.Mobile.TlsInternalServer,
  rtcSTMTPlugin,
  {$IFDEF ST40}
  stSecUtils,
  stGC,
  StreamSec.DSI.PkixCert,
  {$ELSE}
  StreamSec.Mobile.SecUtils,
  StreamSec.Mobile.GC,
  StreamSec.Mobile.PkixCert,
  {$ENDIF}
  StreamSec.Mobile.StreamSecII,
  StreamSec.Mobile.SecComp,
  StreamSec.Mobile.TlsClass,
  StreamSec.Mobile.TlsConst
  {$ENDIF}
  ;

const
  {$IFDEF StreamSecMobile}
  Plugin_Caption='CryptPlugin enables SSL (StreamSec Mobile TLS)';
  {$ELSE}
  Plugin_Caption='CryptPlugin enables SSL (StreamSec Tools II)';
  {$ENDIF}

type
  StrArray=array of String;
  WStrArray=array of RtcWideString;

var
  ClientSessionTimeout:longint=60; // SSL session timeout in seconds (default = 60 seconds)
  ServerSessionTimeout:longint=60; // SSL session timeout in seconds (default = 60 seconds)
  {$IFDEF DEMO}
  { Set to false to disable RC4 and enable AES-CBC. It is recommended that
    StrSecII is updated to version 2.1.9.248 or later if this option is
    disabled.}
  ExpectOldBrowsers:boolean=true;
  { This value SHOULD be set to at least 2048, but some servers and demos still
    use 1024 bit RSA keys. }
  LeastPublicKeySize:integer = 1024;
  {$ELSE}
  { Set to true to enable RC4 and disable AES-CBC. Please note that RC4 is
    officially deprecated, but that some old browser did not implement the
    AES-CBC cipher suites securely. }
  ExpectOldBrowsers:boolean=false;
  { This value SHOULD be set to at least 2048, but some servers and demos still
    use 1024 bit RSA keys. }
  LeastPublicKeySize:integer = 2048;
  {$ENDIF}
{ As of March 2020, the major browsers no longer support TLS 1.0 and TLS 1.1.
  StreamSec Tools version 2.3 and version 4.0 both support TLS 1.2, so your
  server will continue to function, even without any changes to the settings.
  However, if you need security certification, you might have to show that your
  server only supports TLS 1.2 and up, and in such case you have to use
  StreamSec Tools 4.0 or later and set EnableLegacyTLS to False. }
  EnableLegacyTLS:boolean=true;

{ A server root cert file serves two purposes: It allows you to load
  intermediary CA certificate issued by the root when present in the server PFX,
  and it allows you to accept client certificates issued by the root. }
procedure AddServerRootCertFile(const CertFileName:String);
{$IFDEF STREAMSECMOBILE}
procedure AddServerRootCert(const Cert:iCertificate);
{$ENDIF}
procedure AddServerPFXFile(const PFXFileName:String; PFXKey:RtcWideString);
procedure ClientCertificateAuthentication(aRequest, aRequire: Boolean);
function GetServerCryptPlugin:TRtcCryptPlugin;

{ A client root cert file serves two purposes: It allows you to load
  intermediary CA certificate issued by the root when present in the client PFX,
  and it allows you to accept server certificates issued by the root. NOTE that
  unless certificate name checking is turned on CA chaining is the ONLY way the
  server certificates are authenticated. DO NOT use the same inhouse root CA
  for issuing server certificates deployed to any server you do not want the
  client to regard as authentic. }
procedure AddClientRootCertFile(const CertFileName:String);
{$IFDEF STREAMSECMOBILE}
procedure AddClientRootCert(const Cert:iCertificate); overload;
{$ENDIF}
procedure AddClientPFXFile(const PFXFileName:String; PFXKey:RtcWideString);
function GetClientCryptPlugin:TRtcCryptPlugin;

{ If both the client at the server are both protected by a StrSecII compliant
  TLS layer, it is recommended that this value is set to False, unless the
  protocol is required to comply with published SSL/TLS standards. If the value
  is set to True, RC4 will be the only allowed bulk cipher algorithm, to
  prevent padding oracle attacks against CBC mode. If the value is set to
  False, the preferred bulk cipher algorithm will be AESCTR.
  The default is True, meaning that RC4 is the only allowed bulk cipher. }
procedure UseOnlyStandardCipherSuites(aValue: Boolean);
{ Call AllowExpiredCertificates(True) to accept expired server certificates.
  Please note that expired root certificates will always be rejected.
  The default is False. }
procedure AllowExpiredCertificates(aValue: Boolean);
{ Call BeGullablAndTrustAnythingSentToYou(True) if you have to suspend all CA
  chaining for testing purposes, e.g. when testing a client against a server
  with an unknown CA chain.
  The default is False. }
procedure BeGullableAndTrustAnythingSentToYou(aValue: Boolean);
{ Use AddCertificateNameMap client side if the remote server is using a
  certificate with the wrong name, but should be accepted anyway.
  aRemoteAddr might be either ServerAddr or the IP of the server, depending on
  which certificate name information you need to map. Both aRemoteAddr and
  aCertName might be wildcard '*' strings. If aRemoteAddr is a wildcard,
  the commonname, DNSname or URI of the server certificate must match aCertName.
  If aCertName is a wildcard, any certificate sent from a server at a
  location matching aRemoteAddr will be accepted. }
procedure AddCertificateNameMap(const aRemoteAddr, aCertName: string);
{ Perfect Forward Secrecy means that access to the server private key (which is
  in the PFX file) is not sufficient for an attacker to decrypt past TLS
  traffic from TLS sessions that have long expired. Setting
  PerfectForwardSecrecy(True) will induce a performance penalty during the
  handshake, and must be coupled with other measures to be effective.
  * Consider setting PerfectForwardSecrecy(True) e.g. if clients post sensitive
  information to the server, such as passwords (for Basic Authentication),
  credit card numbers etc.
  * Consider setting PerfectForwardSecrecy(False) if any sensitive data, which
  might be obtained by decrypting past traffic, is persisted server side
  anyway, e.g. in log files, server side data files, etc.
  The default is False. }
procedure PerfectForwardSecrecy(aValue: Boolean);

procedure ReleaseCryptPlugins;

implementation

type
  TMyPlugs=class
  protected
    {$IFNDEF STREAMSECMOBILE}
    procedure SimpleTLSInternalServer1BeforeImportTLSCert(Sender: TObject;
      Cert: TASN1Struct; var ExplicitTrust, AllowExpired: Boolean);
    procedure SimpleTLSInternalServer1CertNotAccepted(Sender: TObject;
      Cert: TASN1Struct; Status: TCertStatusCode);
    procedure SimpleTLSInternalServer1CertNotTrusted(Sender: TObject;
      Cert: TASN1Struct; var ExplicitTrust: Boolean);
    procedure SimpleTLSInternalServer1TLSIncomingAlert(Sender: TObject;
      Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
    procedure SimpleTLSInternalServer1TLSOutgoingAlert(Sender: TObject;
      Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
    {$ELSE}
    procedure SimpleTLSInternalServer1BeforeImportTLSCert(Sender: TObject;
      const Cert: iCertificate; var ExplicitTrust, AllowExpired: Boolean);
    procedure SimpleTLSInternalServer1CertNotAccepted(Sender: TObject;
      const Cert: iCertificate; Status: TCertStatusCode{$IFDEF ST40}; var aRetry: Boolean{$ENDIF});
    procedure SimpleTLSInternalServer1CertNotTrusted(Sender: TObject;
      const Cert: iCertificate; var ExplicitTrust: Boolean);
    procedure SimpleTLSInternalServer1TLSIncomingAlert(Sender: TObject;
      Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
    procedure SimpleTLSInternalServer1TLSOutgoingAlert(Sender: TObject;
      Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    end;

var
  myPlugs:TMyPlugs=nil;

var
  Server_RootCertFiles:array of String;
  Server_PFXFiles:array of String;
  Server_PFXKeys:array of RtcWideString;
  Server_RequestClientCertificateAuthentication:boolean=False;
  Server_RequireClientCertificateAuthentication:boolean=False;

  Client_RootCertFiles:array of String;
  Client_PFXFiles:array of String;
  Client_PFXKeys:array of RtcWideString;

  {$IFNDEF STREAMSECMOBILE}
  Server_Ready:boolean=False;
  Server_SimpleTLSInternalServer:TSimpleTLSInternalServer=nil;
  Server_SsPrivateKeyRingComponent:TSsPrivateKeyRingComponent=nil;
  Server_CryptPlugin:TRtcSSecServerPlugin=nil;

  Client_Ready:boolean=False;
  Client_SsPrivateKeyRingComponent:TSsPrivateKeyRingComponent=nil;
  Client_SimpleTLSInternalServer:TSimpleTLSInternalServer=nil;
  Client_CryptPlugin:TRtcSSecClientPlugin=nil;
  Client_CertificateExceptions:TStringList;
  {$ELSE}
  Server_RootCerts:array of iCertificate;
  Client_RootCerts:array of iCertificate;

  Server_Ready:boolean=False;
  Server_SimpleTLSInternalServer:TsmSimpleTLSInternalServer=nil;
  Server_SsPrivateKeyRingComponent:TsmPrivateKeyRingComponent=nil;
  Server_CryptPlugin:TRtcSTMTServerPlugin=nil;

  Client_Ready:boolean=False;
  Client_SsPrivateKeyRingComponent:TsmPrivateKeyRingComponent=nil;
  Client_SimpleTLSInternalServer:TsmSimpleTLSInternalServer=nil;
  Client_CryptPlugin:TRtcSTMTClientPlugin=nil;
  Client_CertificateExceptions:TStringList;
  {$ENDIF}

  Global_ExplicitTrust:boolean=False;
  Global_AllowExpired:boolean=False;
  Global_UseOnlyStandard:boolean=True;
  Global_PerfectForwardSecrecy:boolean=False;

  CS:TCriticalSection;


procedure UseOnlyStandardCipherSuites(aValue: Boolean);
begin
  Global_UseOnlyStandard := aValue;
end;

procedure AllowExpiredCertificates(aValue: Boolean);
begin
  Global_AllowExpired := aValue;
end;

procedure BeGullableAndTrustAnythingSentToYou(aValue: Boolean);
begin
  Global_ExplicitTrust := aValue;
end;

procedure AddCertificateNameMap(const aRemoteAddr, aCertName: string);
begin
  if not Assigned(Client_CertificateExceptions) then
    Client_CertificateExceptions := TStringList.Create;
  Client_CertificateExceptions.Values[aRemoteAddr] := aCertName;
end;

procedure AddServerRootCertFile(const CertFileName:String);
  var
    have:boolean;
    a:integer;
  begin
  if not File_Exists(CertFileName) then Exit;
  CS.Acquire;
  try
    have:=False;
    for a:=0 to length(Server_RootCertFiles)-1 do
      if Server_RootCertFiles[a]=CertFileName then
        have:=True;
    if not have then
      begin
      SetLength(Server_RootCertFiles,length(Server_RootCertFiles)+1);
      Server_RootCertFiles[length(Server_RootCertFiles)-1]:=CertFileName;
      end;
  finally
    CS.Release;
    end;
  end;

{$IFDEF STREAMSECMOBILE}
procedure AddServerRootCert(const Cert:iCertificate);
  var
    have:boolean;
    a:integer;
  begin
  if not Assigned(Cert) then Exit;
  CS.Acquire;
  try
    have:=False;
    for a:=0 to length(Server_RootCerts)-1 do
      if Server_RootCerts[a].GetStruct.Compare(Cert.GetStruct) then
        have:=True;
    if not have then
      begin
      SetLength(Server_RootCerts,length(Server_RootCerts)+1);
      Server_RootCerts[length(Server_RootCerts)-1]:=Cert;
      end;
  finally
    CS.Release;
    end;
  end;
{$ENDIF}

procedure AddServerPFXFile(const PFXFileName:String; PFXKey:RtcWideString);
  var
    have:boolean;
    a:integer;
  begin
  if not File_Exists(PFXFileName) then Exit;
  CS.Acquire;
  try
    have:=False;
    for a:=0 to length(Server_PFXFiles)-1 do
      if Server_PFXFiles[a]=PFXFileName then
        have:=True;
    if not have then
      begin
      SetLength(Server_PFXFiles,length(Server_PFXFiles)+1);
      Server_PFXFiles[length(Server_PFXFiles)-1]:=PFXFileName;
      SetLength(Server_PFXKeys,length(Server_PFXKeys)+1);
      Server_PFXKeys[length(Server_PFXKeys)-1]:=PFXKey;
      end;
  finally
    CS.Release;
    end;
  end;

procedure ClientCertificateAuthentication(aRequest, aRequire: Boolean);
  begin
    Server_RequestClientCertificateAuthentication:=aRequest;
    Server_RequireClientCertificateAuthentication:=aRequire;
  end;

procedure PerfectForwardSecrecy(aValue: Boolean);
  begin
    Global_PerfectForwardSecrecy:=aValue;
  end;

procedure AddClientRootCertFile(const CertFileName:String);
  var
    have:boolean;
    a:integer;
  begin
  if not File_Exists(CertFileName) then Exit;
  CS.Acquire;
  try
    have:=False;
    for a:=0 to length(Client_RootCertFiles)-1 do
      if Client_RootCertFiles[a]=CertFileName then
        have:=True;
    if not have then
      begin
      SetLength(Client_RootCertFiles,length(Client_RootCertFiles)+1);
      Client_RootCertFiles[length(Client_RootCertFiles)-1]:=CertFileName;
      end;
  finally
    CS.Release;
    end;
  end;

{$IFDEF STREAMSECMOBILE}
procedure AddClientRootCert(const Cert:iCertificate);
  var
    have:boolean;
    a:integer;
  begin
  if not Assigned(Cert) then Exit;
  CS.Acquire;
  try
    have:=False;
    for a:=0 to length(Client_RootCerts)-1 do
      if Client_RootCerts[a].GetStruct.Compare(Cert.GetStruct) then
        have:=True;
    if not have then
      begin
      SetLength(Client_RootCerts,length(Client_RootCerts)+1);
      Client_RootCerts[length(Client_RootCerts)-1]:=Cert;
      end;
  finally
    CS.Release;
    end;
  end;
{$ENDIF}

procedure AddClientPFXFile(const PFXFileName:String; PFXKey:RtcWideString);
  var
    have:boolean;
    a:integer;
  begin
  if not File_Exists(PFXFileName) then Exit;
  CS.Acquire;
  try
    have:=False;
    for a:=0 to length(Client_PFXFiles)-1 do
      if Client_PFXFiles[a]=PFXFileName then
        have:=True;
    if not have then
      begin
      SetLength(Client_PFXFiles,length(Client_PFXFiles)+1);
      Client_PFXFiles[length(Client_PFXFiles)-1]:=PFXFileName;
      SetLength(Client_PFXKeys,length(Client_PFXKeys)+1);
      Client_PFXKeys[length(Client_PFXKeys)-1]:=PFXKey;
      end;
  finally
    CS.Release;
    end;
  end;

function GetServerCryptPlugin:TRtcCryptPlugin;
  var
    havePFX:boolean;
    a:integer;
    {$IFDEF STREAMSECMOBILE}
    status: TCertStatusCode;
    {$ENDIF}
  begin
  Result:=nil;
  CS.Acquire;
  try
    if Server_Ready then
      Result:=Server_CryptPlugin
    else
      begin
      if length(Server_PFXFiles)<>length(Server_PFXKeys) then
        raise Exception.Create('PFXKeys array requires exactly one element for each PFXFile element.');

      havePFX:=False;
      for a:=0 to length(Server_PFXFiles)-1 do
        if File_Exists(Server_PFXFiles[a]) then
          havePFX:=True;

      if havePFX then
        begin
        {$IFNDEF STREAMSECMOBILE}
        { Wait for the PRNG to finish. It is threaded by default to allow background
          reseeding while the application is running. Background reseeding is not
          implemented in this demo. }
        while not YarrowHasReseeded do begin
          Sleep(10);                   // give the thread a time slice
          Application.ProcessMessages; // allow the thread to terminate
        end;
        {$ENDIF}

        {$IFNDEF STREAMSECMOBILE}
        Server_SsPrivateKeyRingComponent := TSsPrivateKeyRingComponent.Create(nil);
        Server_SimpleTLSInternalServer := TSimpleTLSInternalServer.Create(nil);
        {$ELSE}
        Server_SsPrivateKeyRingComponent := TsmPrivateKeyRingComponent.Create(nil);
        Server_SimpleTLSInternalServer := TsmSimpleTLSInternalServer.Create(nil);
        {$ENDIF}

        with Server_SsPrivateKeyRingComponent do
          begin
          {$IFNDEF STREAMSECMOBILE}
          AllowPlainTextKeys := True;
          CacheKeyInterfaces := True;
          {$ENDIF}
          DefaultHashAlgorithm := haSHA1;
          SessionKeyLifeSpan := ServerSessionTimeout /24/60/60;
          end;
        with Server_SimpleTLSInternalServer do
          begin
          PrivateKeyRing := Server_SsPrivateKeyRingComponent;
          SessionKeyLifeSpan := ServerSessionTimeout /24/60/60;
          Options.OnlyStandardCipherSuites := Global_UseOnlyStandard;
          if Global_UseOnlyStandard then begin
            if ExpectOldBrowsers then begin
              Options.BulkCipherARC4 := prPrefer;
              LeastKeyBitSize := 1024;
            end else begin
              {$IFDEF ST40}
              Options.BulkCipherAES128GCM := prPrefer;
              Options.BulkCipherAES128 := prAllowed;
              IncludeRootInTLSChain := False;
              {$ELSE}
              Options.BulkCipherAES128 := prPrefer;
              {$ENDIF}
              LeastKeyBitSize := 2048;
            end;
            if EnableLegacyTLS then begin
              Options.HashAlgorithmMD5 := prAllowed;
              Options.HashAlgorithmSHA1 := prAllowed;
            end else begin
              Options.HashAlgorithmMD5 := prNotAllowed;
              Options.HashAlgorithmSHA1 := prNotAllowed;
              Options.BulkCipherARC4 := prNotAllowed;
              Options.BulkCipherAES128 := prNotAllowed;
              Options.BulkCipherAES256 := prNotAllowed;
            end;
            Options.HashAlgorithmSHA256 := prPrefer;
          end else begin
            Options.BulkCipherAES128CTR := prPrefer;
            Options.BulkCipherAES256CTR := prAllowed;
            Options.BulkCipherTwoFish128 := prNotAllowed;
            Options.BulkCipherTwoFish192 := prNotAllowed;
            Options.BulkCipherTwoFish256 := prNotAllowed;
            Options.BulkCipherARC4 := prNotAllowed;
            Options.HashAlgorithmSHA256 := prPrefer;
            Options.HashAlgorithmMD5 := prNotAllowed;
            Options.HashAlgorithmSHA1 := prNotAllowed;
            LeastKeyBitSize := 2048;
          end;
          if EnableLegacyTLS then begin
            if ExpectOldBrowsers then begin
              Options.BulkCipherAES128 := prNotAllowed;
              Options.BulkCipherAES192 := prNotAllowed;
              Options.BulkCipherAES256 := prNotAllowed;
            end else begin
              Options.BulkCipherARC4 := prNotAllowed;
              Options.BulkCipherAES192 := prAllowed;
              Options.BulkCipherAES256 := prAllowed;
            end;
          end else begin
            Options.BulkCipherARC4 := prNotAllowed;
            Options.BulkCipherAES128 := prNotAllowed;
            Options.BulkCipherAES256 := prNotAllowed;
          end;
          Options.BulkCipherTripleDES := prNotAllowed;
          {$IFNDEF STREAMSECMOBILE}
          Options.BulkCipherARC2 := prNotAllowed;
          {$ENDIF}

          Options.EphemeralECDHKeySize := ecsAuto;
          if Global_PerfectForwardSecrecy then begin
            Options.KeyAgreementDHE := prPrefer;
            Options.KeyAgreementRSA := prNotAllowed;
            Options.KeyAgreementDH := prNotAllowed;
            Options.KeyAgreementECDHE := prAllowed;
            Options.SignatureRSA := prAllowed;
          end else begin
            Options.KeyAgreementRSA := prPrefer;
            Options.KeyAgreementDHE := prAllowed;
            Options.KeyAgreementECDHE := prAllowed;
            Options.SignatureRSA := prPrefer;
          end;
          Options.SignatureDSS := prNotAllowed;
          Options.VerifyServerName := [vsnIP, vsnDNS, vsnURI];
          BeforeImportTLSCert := myPlugs.SimpleTLSInternalServer1BeforeImportTLSCert;
          OnCertNotTrusted := myPlugs.SimpleTLSInternalServer1CertNotTrusted;
          OnCertNotAccepted := myPlugs.SimpleTLSInternalServer1CertNotAccepted;
          OnTLSIncomingAlert := myPlugs.SimpleTLSInternalServer1TLSIncomingAlert;
          OnTLSOutgoingAlert := myPlugs.SimpleTLSInternalServer1TLSOutgoingAlert;
          end;

        with Server_SimpleTLSInternalServer do
          begin
          for a:=0 to length(Server_RootCertFiles)-1 do
            if File_Exists(Server_RootCertFiles[a]) then // root.cer
              LoadRootCertsFromFile(Server_RootCertFiles[a]);
          {$IFDEF STREAMSECMOBILE}
          for a:=0 to length(Server_RootCerts)-1 do
            AddCertificate(Server_RootCerts[a],True,status);
          {$ENDIF}

          for a:=0 to length(Server_PFXFiles)-1 do
            if File_Exists(Server_PFXFiles[a]) then // server.pfx
              ImportFromPFX(Server_PFXFiles[a],TSecretKey.CreateBMPStr(PWideChar(Server_PFXKeys[a]),length(Server_PFXKeys[a]))); // abc, 3

          Options.RequestClientCertificate := Server_RequestClientCertificateAuthentication;
          Options.RequireClientCertificate := Server_RequireClientCertificateAuthentication;

          TLSSetupServer;
          end;

        {$IFNDEF STREAMSECMOBILE}
        Server_CryptPlugin := TRtcSSecServerPlugin.Create(nil);
        {$ELSE}
        Server_CryptPlugin := TRtcSTMTServerPlugin.Create(nil);
        {$ENDIF}
        Server_CryptPlugin.TLSServer := Server_SimpleTLSInternalServer;

        Server_Ready:=True;
        Result:=Server_CryptPlugin;
        end
      else
        Server_Ready:=True;
      end;
  finally
    CS.Release;
    end;
  end;

function GetClientCryptPlugin:TRtcCryptPlugin;
  var
    haveRoot, havePFX:boolean;
    a:integer;
    {$IFDEF STREAMSECMOBILE}
    status: TCertStatusCode;
    {$ENDIF}
  begin
  Result:=nil;
  CS.Acquire;
  try
    if Client_Ready then
      Result:=Client_CryptPlugin
    else
      begin
      if length(Client_PFXFiles)<>length(Client_PFXKeys) then
        raise Exception.Create('PFXKeys array requires exactly one element for each PFXFile element.');

      haveRoot:=length(Client_RootCertFiles)=0;
      for a:=0 to length(Client_RootCertFiles)-1 do
        if File_Exists(Client_RootCertFiles[a]) then
          haveRoot:=True;

      if haveRoot then
        begin
        {$IFNDEF STREAMSECMOBILE}
        { Wait for the PRNG to finish. It is threaded by default to allow background
          reseeding while the application is running. Background reseeding is not
          implemented in this demo. }
        while not YarrowHasReseeded do
          begin
          Sleep(10);                   // give the thread a time slice
          Application.ProcessMessages; // allow the thread to terminate
          end;
        {$ENDIF}

        {$IFNDEF STREAMSECMOBILE}
        Client_SsPrivateKeyRingComponent:=TSsPrivateKeyRingComponent.Create(nil);
        Client_SimpleTLSInternalServer:=TSimpleTLSInternalServer.Create(nil);
        {$ELSE}
        Client_SsPrivateKeyRingComponent:=TsmPrivateKeyRingComponent.Create(nil);
        Client_SimpleTLSInternalServer:=TsmSimpleTLSInternalServer.Create(nil);
        {$ENDIF}

        with Client_SsPrivateKeyRingComponent do
          begin
          {$IFNDEF STREAMSECMOBILE}
          AllowPlainTextKeys := True;
          CacheKeyInterfaces := True;
          {$ENDIF}
          DefaultHashAlgorithm := haSHA1;
          SessionKeyLifeSpan := ClientSessionTimeout /24/60/60;
          end;

        with Client_SimpleTLSInternalServer do
          begin
          PrivateKeyRing := Client_SsPrivateKeyRingComponent;
          PublicKeyAlgorithms := [pkaRSA, pkaDSA, pkaECDSA, pkaDH, pkaECDH];
          SessionKeyLifeSpan := ClientSessionTimeout /24/60/60;
          {$IFDEF DEMO}
          LeastKeyBitSize := 1024;
          {$ELSE}
          LeastKeyBitSize := 2048;
          {$ENDIF}
          Options.OnlyStandardCipherSuites := Global_UseOnlyStandard;
          if Global_UseOnlyStandard then begin
            if ExpectOldBrowsers then begin
              Options.BulkCipherARC4 := prPrefer;
            end else begin
              {$IFDEF ST40}
              Options.BulkCipherAES128GCM := prPrefer;
              Options.BulkCipherAES128 := prAllowed;
              {$ELSE}
              Options.BulkCipherAES128 := prPrefer;
              {$ENDIF}
            end;
            if EnableLegacyTLS then begin
              Options.HashAlgorithmMD5 := prAllowed;
              Options.HashAlgorithmSHA1 := prAllowed;
            end else begin
              Options.HashAlgorithmMD5 := prNotAllowed;
              Options.HashAlgorithmSHA1 := prNotAllowed;
            end;
          end else begin
            Options.BulkCipherAES128CTR := prPrefer;
            Options.BulkCipherAES256CTR := prAllowed;
            Options.BulkCipherTwoFish128 := prNotAllowed;
            Options.BulkCipherTwoFish192 := prNotAllowed;
            Options.BulkCipherTwoFish256 := prNotAllowed;
            Options.BulkCipherARC4 := prNotAllowed;
            Options.HashAlgorithmSHA256 := prPrefer;
            Options.HashAlgorithmMD5 := prNotAllowed;
            Options.HashAlgorithmSHA1 := prNotAllowed;
          end;
          if EnableLegacyTLS then begin
            if ExpectOldBrowsers then begin
              Options.BulkCipherAES128 := prNotAllowed;
              Options.BulkCipherAES192 := prNotAllowed;
              Options.BulkCipherAES256 := prNotAllowed;
            end else begin
              Options.BulkCipherARC4 := prNotAllowed;
              Options.BulkCipherAES192 := prAllowed;
              Options.BulkCipherAES256 := prAllowed;
            end;
          end else begin
            Options.BulkCipherARC4 := prNotAllowed;
            Options.BulkCipherAES128 := prNotAllowed;
            Options.BulkCipherAES256 := prNotAllowed;
          end;
          Options.BulkCipherTripleDES := prNotAllowed;
          {$IFNDEF STREAMSECMOBILE}
          Options.BulkCipherARC2 := prNotAllowed;
          {$ENDIF}

          Options.EphemeralECDHKeySize := ecs256;
          if Global_PerfectForwardSecrecy then begin
            Options.KeyAgreementDHE := prPrefer;
            Options.KeyAgreementRSA := prNotAllowed;
            Options.KeyAgreementDH := prNotAllowed;
            Options.KeyAgreementECDHE := prAllowed;
            Options.SignatureRSA := prAllowed;
          end else begin
            Options.KeyAgreementRSA := prPrefer;
            Options.KeyAgreementDHE := prAllowed;
            Options.KeyAgreementECDHE := prAllowed;
            Options.SignatureRSA := prPrefer;
          end;
          Options.SignatureECDSA := prAllowed;
          Options.VerifyServerName := [vsnIP, vsnDNS, vsnURI];
          BeforeImportTLSCert := myPlugs.SimpleTLSInternalServer1BeforeImportTLSCert;
          OnCertNotTrusted := myPlugs.SimpleTLSInternalServer1CertNotTrusted;
          OnCertNotAccepted := myPlugs.SimpleTLSInternalServer1CertNotAccepted;
          OnTLSIncomingAlert := myPlugs.SimpleTLSInternalServer1TLSIncomingAlert;
          OnTLSOutgoingAlert := myPlugs.SimpleTLSInternalServer1TLSOutgoingAlert;
          end;

        with Client_SimpleTLSInternalServer do
          begin
          ClientOrServer := cosClientSide;
          for a:=0 to length(Client_RootCertFiles)-1 do
            if File_Exists(Client_RootCertFiles[a]) then
              LoadRootCertsFromFile(Client_RootCertFiles[a]); // root.cer
          {$IFDEF STREAMSECMOBILE}
          for a:=0 to length(Client_RootCerts)-1 do
            AddCertificate(Client_RootCerts[a],True,status);
          {$ENDIF}

          havePFX:=False;
          for a:=0 to length(Client_PFXFiles)-1 do
            if File_Exists(Client_PFXFiles[a]) then // client.pfx
              begin
              havePFX:=True;
              ImportFromPFX(Client_PFXFiles[a],TSecretKey.CreateBMPStr(PWideChar(Client_PFXKeys[a]),length(Client_PFXKeys[a]))) // abc
              end;
          if not havePFX then
            begin
            Options.RequestClientCertificate := False;
            Options.RequireClientCertificate := False;
            end;
          TLSSetupClient;
          end;

        {$IFNDEF STREAMSECMOBILE}
        Client_CryptPlugin:=TRtcSSecClientPlugin.Create(nil);
        {$ELSE}
        Client_CryptPlugin:=TRtcSTMTClientPlugin.Create(nil);
        {$ENDIF}
        Client_CryptPlugin.TLSServer := Client_SimpleTLSInternalServer;
        if assigned(Client_CertificateExceptions) then
          Client_CryptPlugin.CertificateNameMap := Client_CertificateExceptions;

        Client_Ready:=True;
        Result:=Client_CryptPlugin;
        end
      else
        Client_Ready:=True;
      end;
  finally
    CS.Release;
    end;
  end;

procedure ReleaseCryptPlugins;
  var
    a:integer;
  begin
  for a:=0 to length(Server_RootCertFiles)-1 do
    Server_RootCertFiles[a]:='';
  SetLength(Server_RootCertFiles,0);

  for a:=0 to length(Server_PFXFiles)-1 do
    Server_PFXFiles[a]:='';
  SetLength(Server_PFXFiles,0);

  for a:=0 to length(Server_PFXKeys)-1 do
    Server_PFXKeys[a]:='';
  SetLength(Server_PFXKeys,0);

  for a:=0 to length(Client_RootCertFiles)-1 do
    Client_RootCertFiles[a]:='';
  SetLength(Client_RootCertFiles,0);

  for a:=0 to length(Client_PFXFiles)-1 do
    Client_PFXFiles[a]:='';
  SetLength(Client_PFXFiles,0);

  for a:=0 to length(Client_PFXKeys)-1 do
    Client_PFXKeys[a]:='';
  SetLength(Client_PFXKeys,0);

  if assigned(Client_CertificateExceptions) then
    FreeAndNil(Client_CertificateExceptions);

  if Server_Ready then
    begin
    if assigned(Server_CryptPlugin) then
      FreeAndNil(Server_CryptPlugin);
    if assigned(Server_SimpleTLSInternalServer) then
      FreeAndNil(Server_SimpleTLSInternalServer);
    if assigned(Server_SsPrivateKeyRingComponent) then
      FreeAndNil(Server_SsPrivateKeyRingComponent);
    Server_Ready:=False;
    end;

  if Client_Ready then
    begin
    if assigned(Client_CryptPlugin) then
      FreeAndNil(Client_CryptPlugin);
    if assigned(Client_SimpleTLSInternalServer) then
      FreeAndNil(Client_SimpleTLSInternalServer);
    if assigned(Client_SsPrivateKeyRingComponent) then
      FreeAndNil(Client_SsPrivateKeyRingComponent);
    Client_Ready:=False;
    end;

  end;

{ TMyPlugs }

constructor TMyPlugs.Create;
  begin
  inherited;
  CS:=TCriticalSection.Create;
  end;

{$IFNDEF STREAMSECMOBILE}
destructor TMyPlugs.Destroy;
  begin
  ReleaseCryptPlugins;
  FreeAndNil(CS);
  inherited;
  end;

procedure TMyPlugs.SimpleTLSInternalServer1BeforeImportTLSCert(Sender: TObject;
  Cert: TASN1Struct; var ExplicitTrust, AllowExpired: Boolean);
begin
  AllowExpired := Global_AllowExpired;
  // ExplicitTrust := Global_ExplicitTrust; // Set in OnCertNotTrusted
end;

procedure TMyPlugs.SimpleTLSInternalServer1CertNotAccepted(Sender: TObject;
  Cert: TASN1Struct; Status: TCertStatusCode);
var
  lName: TX501Name;
  lMsg: string;
begin
  ExtractSubject(Cert,lName);
  lMsg := Format('Certificate with commonname "%s" not accepted.'#13#10,[lName.commonName.Str]);
  case Status of
    // crcOK: ;
    crcExpired:
      lMsg := lMsg + 'Status = Expired.';
    crcInvalidSignature:
      lMsg := lMsg + 'Status = The signature is invalid.'#13#10 +
        'Probable cause: The certificate data is corrupt, the certificate is a forgery, or'#13#10 +
        'the issuer public key has been replaced.';
    crcCANotTrusted:
      lMsg := lMsg + 'Status = The issuer of this certificate is not trusted.';
    crcRevoked:
      lMsg := lMsg + 'Status = Revoked.';
    crcCAExpiredOrRevoked:
      lMsg := lMsg + 'Status = The issuer certificate has expired or has been revoked.';
    crcConstraintFault:
      lMsg := lMsg + 'Status = Constraint fault. The certificate does not comply with the constraints of the issuer.';
    crcInvalidKeyUsage:
      lMsg := lMsg + 'Status = Invalid key usage. The issuer certificate cannot be used for issuing this kind of certificates.';
    crcUnsupportedExtension:
      lMsg := lMsg + 'Status = Unsupported extension. The certificate has a critical extension this implementation does not understand.';
    // crcSuperseeded: ; // CRLs only
    crcSyntax:
      lMsg := lMsg + 'Status = Syntax error. There is a problem with the format of this certificate.';
    // crcBaseNotFound: ; // CRLs only
    // crcTrusted: ;
    crcTooSmallKey:
      lMsg := lMsg + 'Status = Too small key. The public key of this certificate is smaller than LeastKeyBitSize.';
    crcPolicyNotAccepted:
      lMsg := lMsg + 'Status = You do not accept the policies of this certificate.';
    crcDuplicateKeyIdentifier:
      lMsg := lMsg + 'Status = Duplicate key identifier. There already exists another certificate with the same key identifier.';
  end;
  lMsg := StringReplace(lMsg,#13#10,#32,[rfReplaceAll]);
  rtcLog.Log(lMsg,'SSL-TLS-FATAL');
end;

procedure TMyPlugs.SimpleTLSInternalServer1CertNotTrusted(Sender: TObject;
  Cert: TASN1Struct; var ExplicitTrust: Boolean);
var
  lName: TX501Name;
  lMsg: string;
begin
  ExtractSubject(Cert,lName);
  ExplicitTrust := Global_ExplicitTrust;
  if ExplicitTrust then begin
    lMsg := Format('Certificate with commonname "%s" explicitly trusted.'#13#10,[lName.commonName.Str]);
    rtcLog.Log(lMsg,'SSL-TLS-WARN');
  end else begin
    lMsg := Format('Certificate with commonname "%s" not trusted.'#13#10,[lName.commonName.Str]);
    rtcLog.Log(lMsg,'SSL-TLS-FATAL');
  end;
end;

procedure TMyPlugs.SimpleTLSInternalServer1TLSOutgoingAlert(Sender: TObject;
  Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
var
  lMsg: string;
begin
  if AlertCode <> 0 then begin
    if Fatal then
      lMsg := 'OUTgoing fatal alert:'
    else
      lMsg := 'OUTgoing warning:';
    lMsg := lMsg + AlertMsg(AlertCode);
    if AlertCode and certificate_unknown <> 0 then begin
      { Certificate name errors are just warnings by default. Typically,
        StrSecII is used client side only for connecting to "your" servers
        and you are expected to use inhouse PKI, so that only "your" servers
        will have server certificates issued by the root CA the client is
        aware of. If the certificate is issued by an unknown CA an unknown_ca
        alert will occur sooner. If you do not want to connect if the name is
        wrong, simply flip Fatal to True, although please note that
        certificates WITHOUT any name information will ALWAYS be accepted as is,
        so it is imperative that you only load root CA certificates of CAs you
        trust to issue certificates that will work with the implementation. }
      // Fatal := True;
      if AlertCode and $FFFF0000 = server_cert_name then begin
        lMsg := Format('%s cert_name=%s expected=%s',
          [lMsg,ExtractDNSName(Client.ServerCertificate^),Client.DNSNameToCheck])
      end else if AlertCode and $FFFF0000 = server_cert_uri then begin
        lMsg := Format('%s cert_uri=%s expected=%s',
          [lMsg,ExtractURI(Client.ServerCertificate^),Client.URIToCheck])
      end else if AlertCode and $FFFF0000 = server_cert_ip then begin
        lMsg := Format('%s cert_ip=%s expected=%s',
          [lMsg,ExtractIP(Client.ServerCertificate^),Client.IPToCheck])
      end;
    end;
    lMsg := StringReplace(lMsg,#13#10,#32,[rfReplaceAll]);
    if Fatal then
      rtcLog.Log(lMsg,'SSL-TLS-FATAL')
    else
      rtcLog.Log(lMsg,'SSL-TLS-WARN');
  end;
end;

{$ELSE}
destructor TMyPlugs.Destroy;
  begin

  ReleaseCryptPlugins;
  FreeAndNil(CS);
  inherited;
  tGCManager.GetDefault.Sweep;
  tGCManager.GetDefault.Sweep;
  end;

procedure TMyPlugs.SimpleTLSInternalServer1BeforeImportTLSCert(Sender: TObject;
  const Cert: iCertificate; var ExplicitTrust, AllowExpired: Boolean);
begin
  AllowExpired := Global_AllowExpired;
  // ExplicitTrust := Global_ExplicitTrust; // Set in OnCertNotTrusted
end;

procedure TMyPlugs.SimpleTLSInternalServer1CertNotAccepted(Sender: TObject;
  const Cert: iCertificate; Status: TCertStatusCode{$IFDEF ST40}; var aRetry: Boolean{$ENDIF});
var
  lMsg: string;
begin
  lMsg := Format('Certificate with commonname "%s" not accepted.'#13#10,[Cert.tbsCertificate.subject.AsRdnSequence.commonName]);
  case Status of
    // crcOK: ;
    crcExpired:
      lMsg := lMsg + 'Status = Expired.';
    crcInvalidSignature:
      lMsg := lMsg + 'Status = The signature is invalid.'#13#10 +
        'Probable cause: The certificate data is corrupt, the certificate is a forgery, or'#13#10 +
        'the issuer public key has been replaced.';
    crcCANotTrusted:
      lMsg := lMsg + 'Status = The issuer of this certificate is not trusted.';
    crcRevoked:
      lMsg := lMsg + 'Status = Revoked.';
    crcCAExpiredOrRevoked:
      lMsg := lMsg + 'Status = The issuer certificate has expired or has been revoked.';
    crcConstraintFault:
      lMsg := lMsg + 'Status = Constraint fault. The certificate does not comply with the constraints of the issuer.';
    crcInvalidKeyUsage:
      lMsg := lMsg + 'Status = Invalid key usage. The issuer certificate cannot be used for issuing this kind of certificates.';
    crcUnsupportedExtension:
      lMsg := lMsg + 'Status = Unsupported extension. The certificate has a critical extension this implementation does not understand.';
    // crcSuperseeded: ; // CRLs only
    crcSyntax:
      lMsg := lMsg + 'Status = Syntax error. There is a problem with the format of this certificate.';
    // crcBaseNotFound: ; // CRLs only
    // crcTrusted: ;
    crcTooSmallKey:
      lMsg := lMsg + 'Status = Too small key. The public key of this certificate is smaller than LeastKeyBitSize.';
    crcPolicyNotAccepted:
      lMsg := lMsg + 'Status = You do not accept the policies of this certificate.';
    crcDuplicateKeyIdentifier:
      lMsg := lMsg + 'Status = Duplicate key identifier. There already exists another certificate with the same key identifier.';
  end;
  lMsg := StringReplace(lMsg,#13#10,#32,[rfReplaceAll]);
  rtcLog.Log(lMsg,'SSL-TLS-FATAL');
end;

procedure TMyPlugs.SimpleTLSInternalServer1CertNotTrusted(Sender: TObject;
  const Cert: iCertificate; var ExplicitTrust: Boolean);
var
  lMsg: string;
begin
  ExplicitTrust := Global_ExplicitTrust;
  if ExplicitTrust then begin
    lMsg := Format('Certificate with commonname "%s" explicitly trusted.'#13#10,[Cert.tbsCertificate.subject.AsRdnSequence.commonName]);
    rtcLog.Log(lMsg,'SSL-TLS-WARN');
  end else begin
    lMsg := Format('Certificate with commonname "%s" not trusted.'#13#10,[Cert.tbsCertificate.subject.AsRdnSequence.commonName]);
    rtcLog.Log(lMsg,'SSL-TLS-FATAL');
  end;
end;

procedure TMyPlugs.SimpleTLSInternalServer1TLSOutgoingAlert(Sender: TObject;
  Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
var
  lMsg: string;
begin
  if AlertCode <> 0 then begin
    if Fatal then
      lMsg := 'OUTgoing fatal alert:'
    else
      lMsg := 'OUTgoing warning:';
    lMsg := lMsg + AlertMsg(AlertCode);
    if AlertCode and certificate_unknown <> 0 then begin
      { Certificate name errors are just warnings by default. Typically,
        StrSecII is used client side only for connecting to "your" servers
        and you are expected to use inhouse PKI, so that only "your" servers
        will have server certificates issued by the root CA the client is
        aware of. If the certificate is issued by an unknown CA an unknown_ca
        alert will occur sooner. If you do not want to connect if the name is
        wrong, simply flip Fatal to True, although please note that
        certificates WITHOUT any name information will ALWAYS be accepted as is,
        so it is imperative that you only load root CA certificates of CAs you
        trust to issue certificates that will work with the implementation. }
      // Fatal := True;
      if AlertCode and $FFFF0000 = server_cert_name then begin
        lMsg := Format('%s cert_name=%s expected=%s',
          [lMsg,Client.ServerCertificate.tbsCertificate.subject.AsRdnSequence.commonname,Client.DNSNameToCheck])
      end else if AlertCode and $FFFF0000 = server_cert_uri then begin
        lMsg := Format('%s cert_uri=%s expected=%s',
          [lMsg,'',Client.URIToCheck])
      end else if AlertCode and $FFFF0000 = server_cert_ip then begin
        lMsg := Format('%s cert_ip=%s expected=%s',
          [lMsg,'',Client.IPToCheck])
      end;
    end;
    lMsg := StringReplace(lMsg,#13#10,#32,[rfReplaceAll]);
    if Fatal then
      rtcLog.Log(lMsg,'SSL-TLS-FATAL')
    else
      rtcLog.Log(lMsg,'SSL-TLS-WARN');
  end;
end;
{$ENDIF}

procedure TMyPlugs.SimpleTLSInternalServer1TLSIncomingAlert(Sender: TObject;
  Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
var
  lMsg: string;
begin
  // AlertCode = 0 signals connection close and is expected
  if AlertCode <> 0 then begin
    if Fatal then
      lMsg := 'INcoming fatal alert:'
    else
      lMsg := 'INcoming warning:';
    lMsg := lMsg + AlertMsg(AlertCode);
    lMsg := StringReplace(lMsg,#13#10,#32,[rfReplaceAll]);
    if Fatal then
      rtcLog.Log(lMsg,'SSL-TLS-FATAL')
    else
      rtcLog.Log(lMsg,'SSL-TLS-WARN');
  end;
end;

initialization
{ "StartLog" will enable logging errors and warnings to a file.
  If you do not want errors and warnings to be logged to a file,
  comment this line out and do not call "StartLog" in your code. }
rtcLog.StartLog;

myPlugs:=TMyPlugs.Create;
finalization
myPlugs.Free;
end.
