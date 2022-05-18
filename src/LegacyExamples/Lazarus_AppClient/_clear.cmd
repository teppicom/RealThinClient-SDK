del backup\*.* /Q
rd backup

del ModelSupport\*.* /Q
rd ModelSupport

rd __recovery /Q /S

del __history\*.* /Q
rd __history

del LOG\*.* /Q
rd LOG

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
del *.o
del *.bak
del *.ppu
del *.a
del *.
del *.ddp
del *.or
del *.dof
del *.compiled
del *.cfg
del *.cpp
del *.bdsproj
del *.bpr
del *.dsk
del *.dpr

del *.asc
del *.tx*
del *.nev

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
