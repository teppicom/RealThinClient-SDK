//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "rtcConn.hpp"
#include "rtcDataCli.hpp"
#include "rtcHttpCli.hpp"
#include "rtcInfo.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TEdit *Edit1;
        TMemo *Memo1;
        TRtcHttpClient *RtcHttpClient1;
        TRtcDataRequest *DataReq;
        TLabel *Label1;
        void __fastcall DataReqBeginRequest(
          TRtcConnection *Sender);
        void __fastcall DataReqDataReceived(
          TRtcConnection *Sender);
        void __fastcall Edit1KeyPress(TObject *Sender, char &Key);
        void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
