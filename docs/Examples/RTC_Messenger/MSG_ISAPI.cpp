//---------------------------------------------------------------------------
#include <ActiveX.hpp>
#include <ComObj.hpp>
#include <rtcISAPIApp.hpp>
#pragma link "rtcISAPIApp"
#pragma hdrstop


USEFORMNS("ISAPI_Module.pas", Isapi_module, ISAPIModule); /* TDataModule: File Type */
USEFORMNS("..\DataProviders\rtcMessengerProvider.pas", Rtcmessengerprovider, Messenger_Provider); /* TDataModule: File Type */
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try
  {
    if (reason == DLL_PROCESS_ATTACH) {
      Application->Initialize();
      Application->CreateForm(__classid(TISAPIModule), &ISAPIModule);
                 Application->Run();
    }
  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
