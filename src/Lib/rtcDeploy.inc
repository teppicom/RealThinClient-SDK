// Enable all RTC SDK debug logs
{.$DEFINE RTC_DEBUG}

// Compile for deployment
{.$DEFINE RtcDeploy}

// Force the use of pure pascal code (no ASM routimes)
{.$DEFINE RTC_NOASM}

// set type RtcString = AnsiString ?
{.$DEFINE FORCE_ANSI_STRING}

// set type RtcString = WideString ?
{.$DEFINE FORCE_WIDE_STRING}

// Compile for testing
{.$DEFINE RtcTest}

// Enable extended Session destructor debug logging
{.$DEFINE RTC_EXTDEBUG}

// Compile with StreamSec Tools
{.$DEFINE StreamSecII}

// Compile with StreamSec Tools 4.0
{.$DEFINE ST40}

// Compile package for FireMonkey iOS
{.$DEFINE RTC_FMI}

// Compile package for FireMonkey Win32, Win64 and OSX
{.$DEFINE RTC_FMX}

{$IFDEF ST40}
  // "StreamSecII" has to be declared for RTC Examples to use StreamSec Tools
  {$DEFINE StreamSecII}
{$ENDIF}

