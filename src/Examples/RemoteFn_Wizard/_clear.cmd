del ModelSupport\*.* /Q
rd ModelSupport

del __history\*.* /Q
rd __history

del LOG\*.* /Q
rd LOG

del *_Icon.ico
del *.tvsconfig

del *.ddp
del *.dof
del *.cfg
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
del *.lst
del *.opt

del *.asc
del *.nev

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
