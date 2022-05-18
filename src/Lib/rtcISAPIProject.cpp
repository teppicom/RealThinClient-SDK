//---------------------------------------------------------------------------
#include <ActiveX.hpp>
#include <ComObj.hpp>
#include <rtcISAPIApp.hpp>
#pragma link "rtcISAPIApp"
#pragma hdrstop


int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try
  {
    if (reason == DLL_PROCESS_ATTACH) {
      Application->Initialize();
      Application->Run();
    }
  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
