rd ModelSupport /Q /S

rd __history /Q /S

rd __recovery /Q /S

rd iOSDevice32 /Q /S
rd iOSDevice64 /Q /S
rd Win32 /Q /S
rd Android /Q /S

rd LOG /Q /S

rd xcode /Q /S

del *_Icon.ico
del *.tvsconfig

del *.dproj
del *.entitlements
del *.otares

del *.deployproj
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
del *.o

del *.obj
del *.hpp
del *.tds

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
del *.