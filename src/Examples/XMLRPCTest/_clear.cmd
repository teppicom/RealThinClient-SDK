del ModelSupport\*.* /Q
rd ModelSupport

del __recovery\*.* /Q
rd __recovery

del __history\*.* /Q
rd __history

del LOG\*.* /Q
rd LOG

del *_Icon.ico
del *.tvsconfig

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

del *.asc
del *.tx*
del *.nev

del *.obj
del *.hpp
del *.tds
del *.dsk
del *.dproj*

call _Pack
copy *.exe ..\..\Bin\Demos
del *.exe
