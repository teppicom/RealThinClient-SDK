rd ModelSupport /Q /S

rd __history /Q /S

rd __recovery /Q /S

rd Android /Q /S
rd OSX32 /Q /S
rd iOSDevice /Q /S
rd iOSDevice32 /Q /S
rd iOSDevice64 /Q /S
rd Win32 /Q /S
rd Win64 /Q /S

rd LOG /Q /S

rd xcode /Q /S

rd fmxGateCli /Q /S

del *.so
del *.dex
del *.entitlements
del *.plist
del *.xml
del *.deployproj
del *.dproj
del *.otares
del *.stat
del *.ico

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
del *.drc
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