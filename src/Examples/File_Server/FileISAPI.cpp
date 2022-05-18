//---------------------------------------------------------------------------
#include <ActiveX.hpp>
#include <ComObj.hpp>
#include <rtcISAPIApp.hpp>
#pragma link "rtcISAPIApp"
#pragma hdrstop


USEFORMNS("ISAPI_Module.pas", Isapi_module, ISAPI_Server); /* TDataModule: File Type */
USEFORMNS("..\DataProviders\rtcFileProvider.pas", Rtcfileprovider, File_Provider); /* TDataModule: File Type */
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try
  {
    if (reason == DLL_PROCESS_ATTACH) {
      Application->Initialize();
      Application->CreateForm(__classid(TISAPI_Server), &ISAPI_Server);
                 Application->Run();
    }
  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
