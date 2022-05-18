#include <vcl.h>
#include <rtcservice.hpp>
#pragma hdrstop

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
USEFORMNS("..\Win_Service.pas", Win_service, Rtc_WebServer); /* TService: File Type */
USEFORMNS("..\Server_Form.pas", Server_form, WebServerForm);
USEFORMNS("..\Server_Module.pas", Server_module, Data_Server); /* TDataModule: File Type */
USEFORMNS("..\..\DataProviders\rtcPhpProvider.pas", Rtcphpprovider, PHP_Provider); /* TDataModule: File Type */
USEFORMNS("..\..\DataProviders\rtcFileProvider.pas", Rtcfileprovider, File_Provider); /* TDataModule: File Type */
USEFORMNS("..\..\DataProviders\rtcISAPIProvider.pas", Rtcisapiprovider, ISAPI_Provider); /* TDataModule: File Type */
USEFORMNS("..\..\DataProviders\rtcMessengerProvider.pas", Rtcmessengerprovider, Messenger_Provider); /* TDataModule: File Type */
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        boolean svc_mode = !IsDesktopMode("Rtc_WebServer");

        try
        {
                if (svc_mode)
                {
                        Svcmgr::Application->Initialize();
                        Svcmgr::Application->CreateForm(__classid(TData_Server), &Data_Server);
                        Svcmgr::Application->CreateForm(__classid(TRtc_WebServer), &Rtc_WebServer);
                        Svcmgr::Application->Run();
                }
                else
                {
                        Forms::Application->Initialize();
                        Forms::Application->CreateForm(__classid(TData_Server), &Data_Server);
                        Forms::Application->CreateForm(__classid(TWebServerForm), &WebServerForm);
                        Forms::Application->Run();
                }
        }
        catch (Exception &exception)
        {
                 if (!svc_mode)
                 {
                        Forms::Application->ShowException(&exception);
                 }
        }
        catch (...)
        {
                 try
                 {
                         throw Exception("");
                 }
                 catch (Exception &exception)
                 {
                        if (!svc_mode)
                        {
                                Forms::Application->ShowException(&exception);
                        }
                 }
        }
        return 0;
}
//---------------------------------------------------------------------------

/**************************************
        boolean svc_mode = !IsDesktopMode("Rtc_WebServer");

        try
        {
                if (svc_mode)
                {
                        Svcmgr::Application->Initialize();
                        Svcmgr::Application->CreateForm(__classid(TData_Server), &Data_Server);
                        Svcmgr::Application->CreateForm(__classid(TRtc_WebServer), &Rtc_WebServer);
                        Svcmgr::Application->Run();
                }
                else
                {
                        Forms::Application->Initialize();
                        Forms::Application->CreateForm(__classid(TData_Server), &Data_Server);
                        Forms::Application->CreateForm(__classid(TWebServerForm), &WebServerForm);
                        Forms::Application->Run();
                }
        }
        catch (Exception &exception)
        {
                 if (!svc_mode)
                 {
                        Forms::Application->ShowException(&exception);
                 }
        }
        catch (...)
        {
                 try
                 {
                         throw Exception("");
                 }
                 catch (Exception &exception)
                 {
                        if (!svc_mode)
                        {
                                Forms::Application->ShowException(&exception);
                        }
                 }
        }
        return 0;
***************************/
