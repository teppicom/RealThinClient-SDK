del ModelSupport\*.* /Q
rd ModelSupport

rd __recovery /Q /S

del __history\*.* /Q
rd __history

del Deploy\Log\*.* /Q
rd Deploy\Log

del Deploy\ModelSupport\*.* /Q
rd Deploy\ModelSupport

del Deploy\__history\*.* /Q
rd Deploy\__history

del Deploy\RtcForumData\Files\*.* /Q
rd Deploy\RtcForumData\Files
del Deploy\RtcForumData\Data\*.* /Q
rd Deploy\RtcForumData\Data

del Deploy\RtcMessengerData\*.* /Q
rd Deploy\RtcMessengerData

call _pack

copy Deploy\RTCWebForum.exe ..\..\..\Bin\RTCSDK\RtcWebForum /y
copy Deploy\RTCWebForumTLS.exe ..\..\..\Bin\RTCSDK\RtcWebForumTLS /y
copy Deploy\RTCWebServer2.exe ..\..\..\Bin\RTCSDK\RtcWebServer /y
copy Deploy\RTCWebServer2TLS.exe ..\..\..\Bin\RTCSDK\RtcWebServerTLS /y

del Deploy\*_Icon.ico
del Deploy\*.tvsconfig

del Deploy\*.exe /Q
del Deploy\*.~*
del Deploy\*.local
del Deploy\*.identcache
del Deploy\*.tgs
del Deploy\*.tgw
del Deploy\*.dproj*
del Deploy\*.drc
del Deploy\*.map
del Deploy\*.stat

copy Deploy\*.dll ..\..\..\Bin\RTCSDK /y
del Deploy\*.dll /Q

del *_Icon.ico
del *.tvsconfig

del *.groupproj
del *.ddp
del *.dsk
del *.rsm
del *.m*
del *.local
del *.identcache
del *.tgs
del *.tgw
del *.dcu
del *.~*
del *.log
del *.stat
del *.drc
del *.dproj*

del *.obj
del *.hpp
del *.tds
