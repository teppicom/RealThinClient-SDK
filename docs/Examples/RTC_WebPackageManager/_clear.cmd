del ModelSupport\*.* /Q
rd ModelSupport

rd __recovery /Q /S

del __history\*.* /Q
rd __history

del Deploy\Log\*.* /Q
rd Deploy\Log

del Deploy\ModelSupport\*.* /Q
rd Deploy\ModelSupport

del Deploy\__history\*.* /Q
rd Deploy\__history

del Delploy\Files\*.* /Q
rd Delploy\Files

call _pack
copy Deploy\*.exe ..\..\..\Bin\RTCSDK\RtcWebPackMan /y

del Deploy\*_Icon.ico
del Deploy\*.tvsconfig

del Deploy\*.exe /Q
del Deploy\*.~*
del Deploy\*.local
del Deploy\*.identcache
del Deploy\*.tgs
del Deploy\*.tgw
del Deploy\*.dproj*
del Deploy\*.drc
del Deploy\*.map
del Deploy\*.stat

del *_Icon.ico
del *.tvsconfig

del *.ddp
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

del *.obj
del *.hpp
del *.tds
