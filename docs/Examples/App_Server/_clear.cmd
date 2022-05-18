rd ModelSupport /S /Q

rd __history /S /Q

rd __recovery /Q /S

rd LOG /S /Q

rd xcode /S /Q

del fmx*.res
del ios*.res

del *.deployproj
del *.plist

del *_Icon.ico
del *.tvsconfig

del *.ddp
del *.cfg
del *.dof
del *.bpf
del *.bdsproj
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
copy *.dll ..\..\..\Bin\RTCSDK
del *.dll
del *.
