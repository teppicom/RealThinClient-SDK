{ If you want to debug the RTC SDK (including loading and unloading of units),
  declare the RTC_DEBUG compiler directive. }
{.$DEFINE RTC_DEBUG}

{ This compiler define will be declared, once HTTP/2.0 support is implemented. }
{.$DEFINE RTC_HTTP20}

{ By default, all RTC Worker Threads will be using Sleep(0) after every job
  to release the rest of their CPU cycle time to other threads and processes.
  If you do NOT want this, add "RTC_NOSLEEP" to conditional defines for your Project.
  This can make your Application run faster, but it will also use more CPU power
  and is not recommended in mixed Application environments or mobile platforms. }
{.$DEFINE RTC_NOSLEEP}

{ You want to remove Asynchronous WinSock API (Windows) support to reduce your applications size?
  Then you should declare the RTC_noAsynSock compiler directive in your app.
  NOTE: This only applies to applications compiled for Windows.
  On all other platforms, this compiler directive is ignored.

  If RTC_noAsynSock is declared, but RTC_noSynSock is NOT declared,
  blocking WinSock API will be used instead of the asynchronous WinSock API.

  WARNING: If you want to use Third-Party Encryption components for SSL support,
     you should NOT remove Asynchronous WinSock API support from your application,
     because Third-Party encryption components are working only in combination with
     the Asynchronous WinSock API on Windows. If you remove the Asynchronous WinSock API
     support from your Application, you will be making the "CryptPlugin" property on the
     TRtcHttpClient component useless. On Windows, you will still be able to get SSL support
     by using WinInet or WinHTTP APIs directly, but NOT by using third-party components with
     the CryptPlugin property. SSL support would then be handled by the WinInet or WinHTTP API
     directly and the CryptPlugin component would simply be ignored and not used. }
{.$DEFINE RTC_noAsynSock}

{ You want to remove Synchronous Sockets API (cross-platform) support to reduce your applications size?
  Then you should declare the RTC_noSynSock compiler directive in your app.
  NOTE: This applies to ALL applications compiled for Mac and Linux, as well as
  Windows applications compiled with the RTC_noAsynSock compiler directive declared. }
{.$DEFINE RTC_noSynSock}

{ You want to remove blocking WinInet API (Windows) support to reduce your applications size?
  Then you should declare the RTC_noWinInet compiler directive in your app.
  NOTE: This only applies to applications compiled for Windows.
  On all other platforms, this compiler directive is ignored.
  WARNING: If you disable WinInet and WinHTTP support, but are NOT using the CryptPlugin
     property in combination with third-party components for SSL support, you will NOT
     have support for SSL encryption in your Application. }
{.$DEFINE RTC_noWinInet}

{ You want to remove WinHTTP API (Windows) support to reduce your applications size?
  Then you should declare the RTC_noWinHTTP compiler directive in your app.
  NOTE: This only applies to applications compiled for Windows.
  On all other platforms, this compiler directive is ignored.
    WARNING: If you disable WinInet and WinHTTP support, but are NOT using the CryptPlugin
     property in combination with third-party components for SSL support, you will NOT
     have support for SSL encryption in your Application. }
{.$DEFINE RTC_noWinHTTP}

{ To use RSA and ISAAC Encryption modes with TRtcClientModule and TRtcServerModule,
  the compiler directive "RTC_RSA" has to be declared. Should you have problems compiling
  the RSA or ISAAC code with the compiler you are using, or if the code does NOT appear
  to be working correctly on the platform(s) you are targetting, simply undeclare the compiler
  directive below and all the refferences to RSA and ISAAC will be removed from the RTC SDK.

  NOTE: If you undeclare the "RTC_RSA" compiler directive to remove "RSA" and "ISAAC" functionality
  from the TRtcClientModule and TRtcServerModule components, the "EncryptionMode" and "RSAKey" properties
  on the TRtcClientModule component will be ignored and the components will be working in default mode
  (EncryptionMode=rem_Basic; RSAKey=0). Also, should a Client with RSA and ISAAC features enabled try to
  communicate using RSA or ISAAC encryption with your Server where you have disabled these features, the
  Server will be responding with "Encryption disabled", even if Encryption is enabled for the standard mode.
  If the Client does NOT try to force encryption, communication security will automatically be lowered to
  work without any encryption. But if the Client which is using RSA and ISAAC features has its ForceEncryption
  property set to TRUE, communication will NOT be possible with a Server which has these features disabled. }
{$DEFINE RTC_RSA}

{ When "UseGUIDs" is defined, CoCreateGUID() API function is used
  to create Session IDs. Otherwise, a short string counter is used.
  For security reasons, use the GUID option (keep the directive defined): }
{$DEFINE UseGUIDs}

{ If you want to use the "TimeoutsOfAPI" property to set low-level API
  timeout parameters, make sure the RTC_USESETTIMEOUTS is declared. }
{$DEFINE RTC_UseSetTimeouts}

{ By default, PHP5.0 is used.
  If you are using PHP 4, undeclare this compiler directive: }
{$DEFINE PHP5} // PHP 5.0.x - seems to work with 5.0.3

{ If you are using a newer PHP 5 version, declare the one you are using }
{.$DEFINE PHP510} // PHP 5.1.x - not tested
{.$DEFINE PHP520} // PHP 5.2.x - seems to work with 5.2.17
{.$DEFINE PHP530} // PHP 5.3.x - not tested

{ You want to reduce traffic when working with XML-RPC, and the clients/servers
   you are working with understand that data enclosed in <value></value> is
   implied to be of type String? Then define this compiler directive: }
{.$DEFINE RTC_XMLRPC_VALUESTRING}

{ If you need all your new clients and servers to stay compatible with all
  older RTC clients and servers, even if this means disabling some new features
  from the latest RTC SDK, you simply need to uncomment this compiler directive: }
{.$DEFINE OldRtcCompatibilityMode}

{ RTC SDK versions up to 2.04 are expecting all Field names received with
  remote function calls to be in UpperCase. This is not the case with newer
  RTC SDK versions, which allow working with field names using mixed case,
  without internaly converting all field names to uppercase.

  To send all Field Names in UpperCase and keep your new application
  compatible with apps compiled with older RTC SDK versions,
  declare the compiler directive below (remove the dot): }
{.$DEFINE RtcUpperCaseFieldNames}

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE RtcUpperCaseFieldNames}
{$ENDIF}

{ RTC Clients compiled with RTC SDK versions *before* 2.27 do NOT send a
  control sum when using RTC functions with encryption, which makes it
  hard for the Server to check if the control key received from the client
  is a valid control key. Starting with RTC SDK 2.27, RTC Clients will be
  sending a control sum value along with each control key, which RTC Servers
  can use to check if the control key is valid and make sure at least the
  end of the request data was decrypted properly.

  But, since this control sum value is not being sent by RTC Clients compiled
  with RTC SDK versions older than 2.27, those clients would NOT work with
  a RTC Server compiled with the latest RTC SDK version if the Server would
  expect this control sum value to be sent by the Client.

  If you have RTC Clients compiled with RTC SDK 2.26 or older,
  you need to *uncomment* the below compiler directive, so that your RTC
  Server (when compiled with the laterst RTC SDK version) does NOT check
  for the control sum value, so it can work with older clients.
  To keep your Server backward-compatible, remove the dot: }
{.$DEFINE RtcDoNotCheckCryptControlSums}

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE RtcDoNotCheckCryptControlSums}
{$ENDIF}

{ All "AnsiString" references have been replaced with "RtcString" in the entire RTC SDK, and
  the "RtcString" type is now mapped to the "String" type by default instead of "AnsiString".

  If your existing code depends on the "AnsiString" type when compiled with Delphi 2009 and later,
  or if you want to use the "WideString" type with FPC or Delphi 6-2007 instead of AnsiString,
  you can *force* the RTC SDK to change the mapping of the "RtcString" type ...

  Declare the "FORCE_ANSI_STRING" compiler directive if you want the
  "RtcString" type to be mapped to the "AnsiString" type instead of "String".

  Or ...

  Declare the "FORCE_WIDE_STRING" compiler directive if you want the
  "RtcString" type to be mapped to the "WideString" type instead of "String".

  If both compiler directives ("FORCE_ANSI_STRING" and "FORCE_WIDE_STRING") are declared,
  "FORCE_ANSI_STRING" compiler directive will be used and "FORCE_WIDE_STRING" will be ignored. }

{.$DEFINE FORCE_ANSI_STRING} // set type RtcString = AnsiString ?
{.$DEFINE FORCE_WIDE_STRING} // set type RtcString = WideString ?

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE FORCE_ANSI_STRING}
{$ENDIF}

{ If you have been using the "TRtcObject" class extensively in your code to
  implement custom classes for use with the "Obj" property on "TRtcInfo" or
  "TRtcSession" objects and need a quick solution to compile your code without
  making any manual changes, declare the "RTC_OBJECT_CLASS" compiler define,
  which will re-introduce the "TRtcObject" class as declared in RTC SDK v7.51
  and older versions and re-introduce the "Obj[]" property on "TRtcInfo" and
  "TRtcSession" classes with the old behavior, which was calling "Kill" on
  "TRtcObject" instances to free objects assigned to INFO and SESSION properties.
  Or ... if you are upgrading from any RTC SDK version between v7.52 and
  v8.00 rc4, you should declare the compiler define "RTC_OBJECT_CLASS2". }

{.$DEFINE RTC_OBJECT_CLASS}
{.$DEFINE RTC_OBJECT_CLASS2}

{$IFDEF OldRtcCompatibilityMode}
  {$DEFINE RTC_OBJECT_CLASS}
{$ENDIF}

{ COMPRESS has to be declared if you want to allow the use of ZLib
  for automated compression when using RTC remote functions. }

{$DEFINE COMPRESS}

{ Declare the compiler directive "RTC_USEZLIB" (below) if you want to use the "ZLib.pas"
  unit from Delphi (or FPC) for compression and decompression instead of using the default
  RTC implementation which is based on a modified FastZLib version and uses zlib OBJ files
  on Windows and the "libz.dylib" library on Mac OSX and iOS platforms.

  NOTE: On mobile platforms (iOS and Android), "ZLib.pas" unit is always used.  }

{.$DEFINE RTC_USEZLIB}


// Quick Fix for the C++ Builder XE3 problem with the rtcDB.pas unit
{$DEFINE RTC_DSNOPS}


{***********************************************************
 !!! Here come the options which you should NEVER change !!!
************************************************************}


{ VER80  => Delphi 1 }
{ VER90  => Delphi 2 }
{ VER93  => Bcb 1    }
{ VER100 => Delphi 3 }
{ VER110 => Bcb 3    }

{ VER120 => Delphi 4 }
{ VER125 => Bcb 4    }
{ VER130 => Delphi 5 }
{ VER140 => Delphi 6 }
{ VER150 => Delphi 7 }
{ VER160 => Delphi 8 }
{ VER170 => Delphi / BDS 2005 }
{ VER180 => Delphi / BDS 2006 }
{ VER190 => Delphi / RAD 2007 }
{ VER200 => Delphi / RAD 2009 }
{ VER210 => Delphi / RAD 2010 }
{ VER220 => Delphi / RAD XE }
{ VER230 => Delphi / RAD XE2 }
{ VER240 => Delphi / RAD XE3 }
{ VER250 => Delphi / RAD XE4 }
{ VER260 => Delphi / RAD XE5 }
{ VER270 => Delphi / RAD XE6 }
{ VER280 => Delphi / RAD XE7 }
{ VER290 => Delphi / RAD XE8 }
{ VER300 => Delphi / RAD 10 Seattle }
{ VER310 => Delphi / RAD 10.1 Berlin }
{ VER320 => Delphi / RAD 10.2 Tokyo }
{ VER330 => Delphi / RAD 10.3 Rio }

{$V+} // The $V directive controls type checking on short strings passed as variable parameters. In the {$V+} state, strict type checking is performed, requiring the formal and actual parameters to be of identical string types.
{$B-} // In the {$B-} state, the compiler generates code for short-circuit Boolean expression evaluation, which means that evaluation stops as soon as the result of the entire expression becomes evident in left to right order of evaluation.
{$X+} // The $X directive enables or disables Delphi's extended syntax. You should not use the {$X-} mode when writing Delphi applications.
{$T-} // In the {$T-} state, the result of the @ operator is always an untyped pointer (Pointer) that is compatible with all other pointer types.
{$H+} // By default {$H+}, Delphi defines the generic string type to be the long UnicodeString.
{$P+} // In the {$P+} state, variable parameters declared using the string keyword are open string parameters, but in the {$P-} state, they are normal variable parameters.
{$J-} // In the {$J-} state, typed constants are truly constant, and any attempt to modify a typed constant causes the compiler to report an error.

{$IFNDEF FPC}
  {$DEFINE IDE_6up}
  {$DEFINE IDE_7up}
  {$DEFINE IDE_2005up}
  {$DEFINE IDE_2006up}
  {$DEFINE IDE_2007up}
  {$DEFINE IDE_2009up}
  {$DEFINE IDE_2010up}
  {$DEFINE IDE_XEup}
  {$DEFINE IDE_XE2up}
  {$DEFINE IDE_XE3up}
  {$DEFINE IDE_XE4up}
  {$DEFINE IDE_XE5up}
  {$DEFINE IDE_XE6up}
  {$DEFINE IDE_XE7up}
  {$DEFINE IDE_XE8up}
  {$DEFINE IDE_10up}
  {$DEFINE IDE_101up}
  {$DEFINE IDE_102up}
  {$DEFINE IDE_103up}
{$ENDIF}

{$IFDEF VER120}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER125}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER130}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER135}
  {$UNDEF IDE_6up}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER140}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER145}
  {$UNDEF IDE_7up}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER150}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER160}
  {$UNDEF IDE_2005up}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER170}
  {$UNDEF IDE_2006up}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER180}
  {$UNDEF IDE_2007up}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER190}
  {$UNDEF IDE_2009up}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER200}
  {$UNDEF IDE_2010up}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER210}
  {$UNDEF IDE_XEup}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER220}
  {$UNDEF IDE_XE2up}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER230}
  {$UNDEF IDE_XE3up}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER240}
  {$UNDEF IDE_XE4up}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER250}
  {$UNDEF IDE_XE5up}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER260}
  {$UNDEF IDE_XE6up}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER270}
  {$UNDEF IDE_XE7up}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER280}
  {$UNDEF IDE_XE8up}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER290}
  {$UNDEF IDE_10up}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER300}
  {$UNDEF IDE_101up}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER310}
  {$UNDEF IDE_102up}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF VER320}
  {$UNDEF IDE_103up}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE WINDOWS}
  {$DEFINE CPUX86}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE WINDOWS}
  {$DEFINE CPUX64}
{$ENDIF}

{$IFDEF CPUARM64}
  {$DEFINE CPUX64}
  {$UNDEF CPUX32}
{$ENDIF}
{$IFDEF CPU64BITS}
  {$DEFINE CPUX64}
  {$UNDEF CPUX32}
{$ENDIF}
{$IFDEF MACOS64}
  {$DEFINE CPUX64}
  {$UNDEF CPUX32}
{$ENDIF}
{$IFDEF IOS64}
  {$DEFINE CPUX64}
  {$UNDEF CPUX32}
{$ENDIF}
{$IFDEF LINUX64}
  {$DEFINE CPUX64}
  {$UNDEF CPUX32}
{$ENDIF}

{$IFDEF IDE_XE4up}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF UNICODE}
  {$IFDEF IDE_2009up}
    {$DEFINE UNICODE_2009up}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$mode delphi}
  {$IFDEF VER3}
    {$DEFINE FPC_UNICODE}
  {$ENDIF}
  {$PACKRECORDS 1}
  {$HINTS OFF}
  {$DEFINE RTC_NOISAPI}
  {$DEFINE RTC_NOINTF}
  {$DEFINE FPC_POSIX}
{$ELSE}
  {$IFDEF POSIX}
    {$DEFINE FPC_POSIX}
  {$ENDIF}
  {$IFDEF MACOS}
    {$IFDEF IOS}
      {$DEFINE MACIOS}
    {$ELSE}
      {$DEFINE MACOSX}
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF WIN32} // The IDE (RAD Studio) currently only works on 32-bit Windows
    {$DEFINE RTC_NOINTF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF WINDOWS} // Asynchronous Sockets, WinInet, WinHTTP and ISAPI available only on Windows
  {$DEFINE RTC_noAsynSock}
  {$DEFINE RTC_noWinInet}
  {$DEFINE RTC_noWinHttp}
  {$DEFINE RTC_NOISAPI}
{$ENDIF}

{$DEFINE DESKTOP}
{$IFDEF ANDROID}
  {$UNDEF DESKTOP}
{$ENDIF}
{$IFDEF MACIOS}
  {$UNDEF DESKTOP}
{$ENDIF}

{$IFDEF BCB5}
  {$DEFINE RTC_NOINTF}
{$ENDIF}

{$IFDEF CPUX64}
  {$DEFINE RTC_NOASM}
  // 64-bit CPU
{$ELSE}
  {$IFDEF CPUX86}
    // Intel 32-bit
    {$DEFINE CPUX32}
  {$ELSE}
    {$IFDEF CPU386}
      // Intel 32-bit
      {$DEFINE CPUX32} // For backwards compatibility
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE RTC_NOASM}
    {.$DEFINE RTC_USEZLIB}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$DEFINE RTC_NOASM}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
  {$DEFINE IDE_0}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
  {$DEFINE IDE_0}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE IDE_2}
  {$DEFINE IDE_1}
{$ENDIF}
{$IFDEF VER135}
  {$DEFINE IDE_2}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE IDE_2}
{$ENDIF}

{$IFNDEF IDE_1}
  {$IFNDEF FPC}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF IDE_2}
  {$IFNDEF FPC}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}

{$include rtcDeploy.inc}

{$UNDEF RTC_BYTESTRING}
{$UNDEF RTC_WIDESTRING}
{$IFDEF FORCE_ANSI_STRING}
  {$IFNDEF NEXTGEN}
    {$DEFINE RTC_BYTESTRING}
  {$ENDIF}
{$ELSE}
  {$IFNDEF UNICODE}
    {$IFDEF FORCE_WIDE_STRING}
      {$DEFINE RTC_WIDESTRING}
    {$ELSE}
      {$DEFINE RTC_BYTESTRING}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$UNDEF FPC_WIDESTRING}
{$IFDEF FPC}
  {$IFNDEF RTC_BYTESTRING}
    {$DEFINE FPC_WIDESTRING}
  {$ENDIF}
{$ENDIF}

{$IFNDEF RTC_RSA}
  {$DEFINE RTC_NORSA}
{$ENDIF}

{$IFNDEF WINDOWS} //Win32 and Win64
  {$DEFINE NOT_WINDOWS}
  {$IFNDEF POSIX} // Delphi - Mac OSX
    {$IFDEF DARWIN} // FPC - iOS (iPhone + iPad)
      {$DEFINE RTC_NIX_SOCK}
    {$ELSE}         // FPC - the rest
      {$DEFINE FPSOCK}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF IDE_2010up}
  {$DEFINE RTC_ANON_METHODS}
  {$DEFINE RTC_WANT_GENERICS}
{$ENDIF}
