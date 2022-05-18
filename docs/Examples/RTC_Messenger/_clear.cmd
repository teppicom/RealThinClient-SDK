del ModelSupport\*.* /Q
rd ModelSupport

rd __recovery /Q /S

del __history\*.* /Q
rd __history

del RtcMessengerData\*.* /Q
rd rtcMessengerData

del Log\*.* /Q
rd Log

del *_Icon.ico
del *.tvsconfig

del *.ddp
del *.dof
del *.bdsproj
del *.cfg
del *.dpp
del *.rsm
del *.m*
del *.local
del *.identcache
del *.t*
del *.dcu
del *.~*
del *.log
del *.drc
del *.stat
del *.drc
del *.dproj*
del *.dsk

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
copy *.dll ..\..\..\Bin\RTCSDK
del *.exe
del *.dll
del *.lib
