rd ModelSupport /Q /S
rd __history /Q /S
rd __recovery /Q /S
rd lib /Q /S

del sort\*.~*
rd sort\__history /Q /S
rd sort\__recovery /Q /S

del synsock\*.~*
rd synsock\__history /Q /S
rd synsock\__recovery /Q /S

rd Backup /Q /S
rd Release /Q /S
rd Debug /Q /S

del rtcsdk_fpc.pas
del rtcsdk_typhon.pas

del *_Icon.ico
del *.tvsconfig
del *.dpu
del *.otares

del *.il?
del *.err
del *.out
del *.a
del *.lst
del *.dcuosx
del *.local
del *.identcache
del *.dcpil
del *.pdb
del *.bpl
del *.dcp
del *.rsp
del *.dcu
del *.hpp
del *.dcc
del *.bpl
del *.~*
del *.cfg
del *.dof
del *.bak
del *.dll
del *.stat
del *.mps
del *.mpt
del *.map
del *.dsk
del *.ddp
del *.drc

del *.obj

del *.dproj*
del *.bdsproj
rem del *.cbproj

del *.bdsgroup
del *.groupproj*
del *.groupproj
del *.xml

del *.tds
del *.hpp
del *.lib
del *.compiled

del *.ppu
del *.o
del lib\*.* /Q
rd lib

del rtcSDK_X.res

del Debug_Build\*.* /Q
rd Debug_Build

del Release_Build\*.* /Q
rd Release_Build

del Debug\*.* /Q
rd Debug
