del ModelSupport\*.* /Q
rd ModelSupport

del __history\*.* /Q
rd __history

del __recovery\*.* /Q
rd __recovery

del Log\*.* /Q
rd Log

del *_Icon.ico
del *.tvsconfig

del *.cfg
del *.dof
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

del *.asc
del *.tx*
del *.nev

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
copy *.dll ..\..\..\Bin\RTCSDK
del *.dll
del *.lib
