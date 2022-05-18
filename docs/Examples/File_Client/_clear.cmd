rd ModelSupport /S /Q

rd __history /S /Q

rd download /S /Q

rd __recovery /Q /S

rd iOSDevice32 /Q /S
rd iOSDevice64 /Q /S
rd Win32 /Q /S
rd Android /Q /S

rd LOG /S /Q

rd xcode /S /Q

del *.plist
del *.o
del *.so

del *_Icon.ico
del *.tvsconfig
del *.xml
del *.dex

del *.dproj
del *.entitlements
del *.otares

del *.deployproj
del *.plist

del *.txt
del *.cfg
del *.dof
del *.ddp
del *.bdsproj
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
del *.obj
del *.hpp
del *.tds
del *.dsk

call _Pack
copy *.exe ..\..\..\Bin\RTCSDK
del *.exe
del *.
