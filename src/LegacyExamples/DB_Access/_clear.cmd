rd ModelSupport /S /Q

rd __history /S /Q

rd __recovery /Q /S

rd LOG /S /Q

rd xcode /S /Q

del fmx*.res
del ios*.res

del *_Icon.ico
del *.tvsconfig
del *.plist

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
del *.
