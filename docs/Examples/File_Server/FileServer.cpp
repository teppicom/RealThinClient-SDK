//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORMNS("HTTP_Module.pas", Http_module, HTTP_Server); /* TDataModule: File Type */
USEFORMNS("Server_Form.pas", Server_form, RtcFileServer);
USEFORMNS("..\DataProviders\rtcFileProvider.pas", Rtcfileprovider, File_Provider); /* TDataModule: File Type */
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TRtcFileServer), &RtcFileServer);
                 Application->CreateForm(__classid(THTTP_Server), &HTTP_Server);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        catch (...)
        {
                 try
                 {
                         throw Exception("");
                 }
                 catch (Exception &exception)
                 {
                         Application->ShowException(&exception);
                 }
        }
        return 0;
}
//---------------------------------------------------------------------------
