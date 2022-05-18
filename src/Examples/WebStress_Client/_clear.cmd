del ModelSupport\*.* /Q
rd ModelSupport

del __history\*.* /Q
rd __history

del *_Icon.ico
del *.tvsconfig

del *.bdsproj
del *.cfg
del *.dof
del *.ddp
del *.txt
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
del *.dsk

del *.asc
del *.tx*
del *.nev
del *.dproj*

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
