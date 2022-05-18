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

cd BCB_Client1
call _clear
cd ..

cd BCB_RemoteFunctions
call _clear
cd ..

cd BCB_Server1
call _clear
cd ..

cd DB_Access
call _clear
cd ..

cd FMXGateStressClient
call _clear
cd ..

cd GateChatClient
call _clear
cd ..

cd GateChatServer
call _clear
cd ..

cd GatewayNode
call _clear
cd ..

cd GatewayTest
call _clear
cd ..

cd Lazarus_AppClient
call _clear
cd ..

cd Lazarus_AppServer
call _clear
cd ..

cd Lazarus_Server
call _clear
cd ..

cd Raw_TCP
call _clear
cd ..

cd Raw_UDP
call _clear
cd ..

cd RSATest
call _clear
cd ..

