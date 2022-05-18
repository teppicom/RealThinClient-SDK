del ModelSupport\*.* /Q
rd ModelSupport

del __history\*.* /Q
rd __history

del LOG\*.* /Q
rd LOG

del mem\ModelSupport\*.* /Q
rd mem\ModelSupport

del mem\__history\*.* /Q
rd mem\__history

del mem\*.~* /Q

del *_Icon.ico
del *.tvsconfig
del *.otares

del *.plist
del *.cfg
del *.dof
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
del *.dproj*
del *.dsk
del *.lst

del *.asc
del *.tx*
del *.nev

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
