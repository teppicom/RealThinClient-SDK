//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "rtcConn.hpp"
#include "rtcDataSrv.hpp"
#include "rtcHttpSrv.hpp"
#include "rtcInfo.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TRtcHttpServer *RtcHttpServer1;
        TRtcDataProvider *RtcDataProvider1;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall RtcDataProvider1CheckRequest(
          TRtcConnection *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
