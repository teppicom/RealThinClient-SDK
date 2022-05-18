rd ModelSupport /Q /S

rd __history /Q /S

rd __recovery /Q /S

rd LOG /Q /S

rd INBOX /Q /S

rd xcode /Q /S

del *.inf
del *.ini

del fmx*.res
del ios*.res

del *_Icon.ico
del *.tvsconfig

del *.deployproj
del *.plist

del *.bdsproj
del *.cfg
del *.dof
del *.ddp
del *.txt
del *.rsm
del *.map
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