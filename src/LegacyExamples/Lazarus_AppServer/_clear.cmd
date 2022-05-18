del backup\*.* /Q
rd backup

del ModelSupport\*.* /Q
rd ModelSupport

del __history\*.* /Q
rd __history

rd __recovery /Q /S

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
del *.compiled
del *.a
del *.or
del *.

del *.asc
del *.tx*
del *.nev

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
