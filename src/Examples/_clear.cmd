@echo off

rd __history /S /Q

del *.bdsgroup
del *.groupproj*
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
del *.mps
del *.mpt
del *.dsk
del *.obj
del *.hpp
del *.tds
del *.dsk
del *.groupproj
del *.exe
del *.tvsconfig

cd DataProviders
call _clear
cd ..

cd RTC_WebPackageManager
call _clear
cd ..

cd RTC_WebForum
call _clear
cd ..

cd RTC_WebServer
call _clear
cd ..

cd RTC_Messenger
call _clear
cd ..

cd File_Client
call _clear
cd ..

cd File_Server
call _clear
cd ..

cd App_Client
call _clear
cd ..

cd App_Server
call _clear
cd ..

cd LoadBalancer
call _clear
cd ..

cd Router
call _clear
cd ..

cd Raw_HttpServer
call _clear
cd ..

cd BrowserUpload
call _clear
cd ..

cd ClientFormPost
call _clear
cd ..

cd ClientUpload
call _clear
cd ..

cd RemoteFunctions
call _clear
cd ..

cd DualServer
call _clear
cd ..

cd rtcParse1
call _clear
cd ..

cd rtcParse2
call _clear
cd ..

cd rtcParse3
call _clear
cd ..

cd ServerLesson1
call _clear
cd ..

cd ServerLesson2
call _clear
cd ..

cd ServerLesson2b
call _clear
cd ..

cd ServerLesson3
call _clear
cd ..

cd ServerLesson4
call _clear
cd ..

cd XMLRPCTest
call _clear
cd ..

cd rtcScript
call _clear
cd ..

cd LinkedObjects
call _clear
cd ..

cd SimpleJSONServer
call _clear
cd ..

cd ConsoleServer
call _clear
cd ..

cd WebSockServer
call _clear
cd ..

cd WebSockClient
call _clear
cd ..

cd RemoteFn_Wizard
call _clear
cd ..

cd WebServer_LogReader
call _clear
cd ..

cd WebStress_Client
call _clear
cd ..

cd UnitTesting
call _clear
cd ..

cd RouterCheck
call _clear
cd ..
