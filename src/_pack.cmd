call _clear

call ..\Bin\BuildSDKHelp.cmd
copy MyHelp\Docs.chm Help\RTCSDK_Help.chm

del ..\Bin\Help\*.* /Q
del MyHelp\Docs.* /Q
move MyHelp\*.* ..\Bin\Help
rd MyHelp

del ..\RealThinClientSDK_Current.zip /Q

call "%ProgramFiles%\7-zip\7z.exe" a -r -x!.* -tZIP ..\RealThinClientSDK_Current.zip *.*

pause