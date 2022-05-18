del ModelSupport\*.* /Q
rd ModelSupport

rd __recovery /Q /S

del __history\*.* /Q
rd __history

del Log\*.* /Q
rd Log

del LOG\*.* /Q
rd LOG

rd RtcMessengerData

del *.o
del *.deployproj
del *.
del *_Icon.ico
del *.tvsconfig

del *.bdsproj
del *.ddp
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
del *.dsk

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
