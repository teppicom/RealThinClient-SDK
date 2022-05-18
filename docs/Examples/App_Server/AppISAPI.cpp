//---------------------------------------------------------------------------
#include <ActiveX.hpp>
#include <ComObj.hpp>
#include <rtcISAPIApp.hpp>
#pragma hdrstop

USEFORMNS("Isapi_Module.pas", Isapi_module, MyISAPI_Module); /* TDataModule: File Type */
USEFORMNS("AppServer_Module.pas", Appserver_module, AppSrv_Module); /* TDataModule: File Type */
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try
  {
    if (reason == DLL_PROCESS_ATTACH) {
      Application->Initialize();
      Application->CreateForm(__classid(TMyISAPI_Module), &MyISAPI_Module);
                 Application->CreateForm(__classid(TAppSrv_Module), &AppSrv_Module);
                 Application->Run();
    }
  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
