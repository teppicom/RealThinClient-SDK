�
 TFORM1 0�  TPF0TForm1Form1LeftTop� Width�Height�CaptionUDP Server + Client exampleColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenter
PrintScalepoNoneScaledShowHint	PixelsPerInchx
TextHeight 	TSplitter	Splitter1Left Top� Width|HeightCursorcrVSplitAlignalBottom  TPanelPanel1Left Top Width|Height� AlignalClientTabOrder  TPanelPanel3LeftTopWidthzHeightKAlignalTop
BevelOuterbvNoneTabOrder  TPanelPanel7Left.Top WidthLHeightKAlignalRight
BevelOuterbvNoneTabOrder TButton	btnListenLeftTopWidth=Height)Hint.Start/Stop Listening and waiting for messages.CaptionListenTabOrder OnClickbtnListenClick   TPanelPanel11Left Top Width.HeightKAlignalClient
BevelOuterbvNoneTabOrder  TLabelLabel3LeftTop
WidthPHeightCaptionListen on PortFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel4LeftXTop
Width_HeightCaptionBind to Address  TLabelLabel7Left� Top
Width`HeightCaptionSend Nickname  TLabelLabel5LeftTop<Width`HeightCaptionConversation:Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TEditeMyPortLeftTopWidthEHeightHint6Port on which you want to wait for incomming messages.TabOrder Text81  TEdit
eMyAddressLeftXTopWidthYHeightHint]Leave empty to bind to all network addapters in your PC, or enter addapter IP to bind to one.TabOrder  TEditeMyNicknameLeft� TopWidthyHeightHint4Nickname to be sent with all your outgoing messages.TabOrder    TPanelPanel4LeftTopLWidthzHeight� AlignalClient
BevelOuterbvNoneBorderWidthTabOrder TMemoeReceiveLeftTopWidthrHeight� AlignalClient
ScrollBars
ssVerticalTabOrder     TPanelPanel2Left Top� Width|Height� AlignalBottomTabOrder TPanelPanel5LeftTopWidthzHeightPAlignalTop
BevelOuterbvNoneTabOrder  TLabelLabel1LeftTop
WidthZHeightCaptionConnect to PortFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel2Left`Top
WidthoHeightCaptionRecipient Address  TLabelLabel6LeftTop<Width~HeightCaptionMessage to Send:Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TEdit	eSendPortLeftTopWidthMHeightHint5Port on which your recipient is waiting for messages.TabOrder Text81  TEditeSendAddressLeft`TopWidthHeightHint0Recipient's address (can be IP address or name).TabOrder  	TCheckBox	xFillAddrLeft� Top
Width� HeightHint=When Recipient Addr is empty, copy the IP of your sender hereTabStopCaptionget address from received msg.TabOrder   TPanelPanel6LeftTopQWidthzHeightYAlignalClient
BevelOuterbvNoneTabOrder TPanelPanel8Left1Top WidthIHeightYAlignalRight
BevelOuterbvNoneTabOrder TPanelPanel9Left TopWidthIHeightQAlignalBottom
BevelOuterbvNoneTabOrder  TButtonbtnSendLeftTop&Width5Height$HintDSend the message entered (Enter key in message window does the same)CaptionSENDTabOrder OnClickbtnSendClick  TButtonbtnResetLeftTopWidth5HeightHintDSend the message entered (Enter key in message window does the same)CaptionResetTabOrderOnClickbtnResetClick    TPanelPanel10Left Top Width1HeightYAlignalClient
BevelOuterbvNoneBorderWidthTabOrder  TMemoeSendLeftTopWidth)HeightQAlignalClient
ScrollBars
ssVerticalTabOrder 
OnKeyPresseSendKeyPress     TRtcUdpServer	UdpServerOnListenStartUdpServerListenStartOnListenStopUdpServerListenStopUdpReuseAddr	OnDataReceivedUdpServerDataReceivedLeftTopV  TRtcUdpClient	UdpClientTimeout.AfterDataSentOnDisconnectUdpClientDisconnectUdpMultiCast	UdpMultiCastMaxHopsOnDataReceivedUdpClientDataReceived
OnDataLostUdpClientDataLostLeftTopU   