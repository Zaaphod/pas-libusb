Unit SNAPI;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, TypInfo,
  LibUSB,LibUsboop;

{$PACKENUM 1}
Type
  TSNAPIBeepCode = (bcOneShortHi,bcTwoShortHi,bcThreeShortHi,bcFourShortHi,bcFiveShortHi,
                    bcOneShortLo,bcTwoShortLo,bcThreeShortLo,bcFourShortLo,bcFiveShortLo,
                    bcOneLongHi,bcTwoLongHi,bcThreeLongHi,bcFourLongHi,bcFiveLongHi,
                    bcOneLongLo,bcTwoLongLo,bcThreeLongLo,bcFourLongLo,bcFiveLongLo,
                    bcFastHiLoHiLo,bcSlowHiLoHiLo,bcHiLo,bcLoHi,bcHiLoHi,bcLoHiLo);

  TSNAPICodeType = (ctCode39=1,ctCodabar,ctCode128,ctDiscrete2of5,ctIATA,ctInterleaved2of5,
                    ctCode93,ctUPCA,ctUPCE0,ctEAN8,ctEAN13,ctCode11,ctCode49,ctMSI,ctEAN128,
                    ctUPCE1,ctPDF417,ctCode16K,ctCode39FullASCII,ctUPCD,ctCode39Trioptic,
                    ctBookland,ctCouponCode,ctNW7,ctISBT128,ctMicroPDF,ctDataMatrix,ctQRCode,
                    ctMicroPDFCCA,ctPostNetUS,ctPlanenCode,ctCod32,ctISBT128Con,ctJapanPostal,
                    ctAustralianPostal,ctDutchPostal,ctMaxiCode,ctCanadianPostal,ctUKPostal,
                    ctMacroPDF,
                    ctRSS14=48,ctRSSLimited,ctRSSExpanded,
                    ctScanlet=55,
                    ctUPCA2=72,ctUPCE02,ctEAN82,ctEAN132,
                    ctUPCE12=80,ctCCAEAN128,ctCCAEAN13,ctCCAEAN8,ctCCARSSExpanded,
                    ctCCARSSLimited,ctCCARSS14,ctCCAUPCA,ctCCAUPCE,ctCCCEAN128,ctTLC39,
                    ctCCBEAN128=97,ctCCBEAN13,ctCCBEAN8,ctCCBRSSExpanded,ctCCBRSSLimited,
                    ctCCBRSS14,ctCCBUPCA,ctCCBUPCE,ctSignatureCapture,
                    ctMatris2of5=113,ctChinese2of5,
                    ctUPCA5=136,ctUPCE05,ctEAN85,ctEAN135,ctUPCE15=144,ctMacroMicroPDF=154);

  TSNAPIError = (seOk=0,seNoACK=-1,seCommandNotSupported=-2);

Type
  PReportStatus = ^TReportStatus;
  TReportStatus = record  // total size: 4 Bytes
    ReportID   : Byte;
    PrevReport : Byte;
    Status     : Byte;
    Unknown    : Byte;
  End;
  PReportBarcode = ^TReportBarcode;
  TReportBarcode = packed record  // total size: 32 Bytes
    ReportID   : Byte;
    Packets    : Byte;  // total number of packets
    PacketNum  : Byte;  // number of this packet
    Length     : Byte;  // length of this packet
    CodeType   : TSNAPICodeType;
    Unknown3   : Byte;
    Barcode    : Array[0..25] of Byte;
  End;

  ESNAPIError               = class(Exception);
  ESNAPINoACK               = class(ESNAPIError);
  ESNAPICommandNotSupported = class(ESNAPIError);
  ESNAPIUSBError            = class(ESNAPIError);

  { TSNAPIDevice }

  TOnBarcodeFunc = Procedure(Barcode:String) of object;

  TSNAPIDevice = class(TLIBUSBDevice)
  private
    FSNAPICommandInterface : TLIBUSBPseudoHIDInterface;
    FFragmentedBarcode : Array of TReportBarcode;
    FOnBarcode : TOnBarcodeFunc;
    Procedure SendACK(ReportID: Byte);
    Function WaitForACK(ReportID:Byte): TSNAPIError;
    Procedure SetOutputReport(ReportID:Byte;Data:Byte);
    Function IntrReport(Report:PHIDReport) : Boolean;
  public
    Constructor Create(idVendor,idProduct:LongInt;AConfig,AInterface,AAltInterface:LongInt); overload;
    Destructor Destroy; override;
    Procedure AbortMacroPdf;
    Procedure AimOff;
    Procedure AimOn;
    //Procedure Connect;
    //Procedure Disconnect;
    Procedure EnterLowPwrMode;
    Procedure FlushMacroPdf;
    Function  GetSerialNumber : String;
    //Procedure Init;
    Procedure LedOff(LEDs:Byte);
    Procedure LedOn(LEDs:Byte);
    Procedure PullTrigger;
    Procedure ReleaseTrigger;
    //Procedure RequestAllParameters;
    Procedure RequestParameters(Parameters : Array of Word);
    Procedure RequestScannerCapabilities;
    //Procedure ReturnDLLVersion;
    Procedure ScanDisable;
    Procedure ScanEnable;
    //Procedure SetCapabilitiesBuffer;  // replaced by OnXyz
    //Procedure SetDecodeBuffer;  // replaced by OnXyz
    //Procedure SetImageBuffer;
    Procedure SetParamPersistance(Persistent:Boolean);
    //Procedure SetParameterBuffer;
    Procedure SetParameterDefaults;
    Procedure SetParameters(Parameters : Array of Word);  // number/value alternating, maximum 7x
    //Procedure SetVersionBuffer; // beongs to TransmitVersion
    //Procedure SetVideoBuffer;
    Procedure SnapShot;        // will call OnSnapshot when an image is there
    Procedure TransmitVersion;
    Procedure TransmitVideo;

    Procedure Beep(Code:TSNAPIBeepCode);
    Procedure Listen;
    property OnBarcode : TOnBarcodeFunc read FOnBarcode write FOnBarcode;
  End;

Const
  SNAPI_PARAM_IMAGE_FORMAT      = $0130;
  SNAPI_PARAM_VIDEO_VIEW_FINDER = $0144;           // $0000: disable, $0001: enable

  SNAPI_IMAGE_FORMAT_JPEG = $0001;
  SNAPI_IMAGE_FORMAT_BMP  = $0003;
  SNAPI_IMAGE_FORMAT_TIFF = $0004;

Implementation

Const
  SNAPI_FEATURE_REPORT_SOFTWARE_REVISION = $31; //  -> NBRPVAAM
  SNAPI_FEATURE_REPORT_BOARD_TYPE = $32; // ???  -> Hardware Revision
  SNAPI_FEATURE_REPORT_ENGINE_CODE = $33; // ??? -> Engine Code
  SNAPI_FEATURE_REPORT_CLASS_INFO = $34; // ??? -> Class Code
  SNAPI_FEATURE_REPORT_UNKNOWN_1  = $3; //5 -> :D:C:V:b

  SNAPI_OUTPUT_REPORT_UNKNOWN_2 = $0c; // ->  01 01 00 30 01 , dann das selbe als HID_SET_REPORT, beides nur noch mit $20 Bytes
  SNAPI_OUTPUT_REPORT_STATUS = $21; // -> (oder so, kommt nach jedem SET_REPORT OUTPUT)  Report ID, Status, $00
  SNAPI_OUTPUT_REPORT_BARCODE = $22; //
  SNAPI_OUTPUT_REPORT_PARAM_STORED = $24; //    (data = 03 = Event Data Received???)
  SNAPI_OUTPUT_REPORT_UNKNOWN_3 = $25; //

  SNAPI_OUTPUT_REPORT_ACK = $01; // -> (nach jedem Strichcode?)

   SNAPI_OUTPUT_REPORT_AIM = $02; //
   SNAPI_OUTPUT_REPORT_LED = $05; //
   SNAPI_OUTPUT_REPORT_SCANNING = $06; //
   SNAPI_OUTPUT_REPORT_TRIGGER = $0A; //
   SNAPI_OUTPUT_REPORT_BEEP = $04; //
   SNAPI_OUTPUT_REPORT_MACRO_PDF = $08; //
   SNAPI_OUTPUT_REPORT_LOW_POWER = $07; //
   SNAPI_OUTPUT_REPORT_SET_DEFAULTS = $09; //    (data = 00)
  SNAPI_OUTPUT_REPORT_SET_IMAGE_FORMAT = $0b; // ($20 bytes data)
  SNAPI_OUTPUT_REPORT_GET_PARAMETER = $0c; //

  SNAPI_STATUS_OK = $01;
  SNAPI_STATUS_COMMAND_NOT_SUPPORTED = $03;


{ TSNAPIDevice }

Constructor TSNAPIDevice.Create(idVendor, idProduct: LongInt; AConfig, AInterface, AAltInterface: LongInt);
Var Intf : Plibusb_interface_descriptor;
Begin
  inherited Create(idVendor,idProduct,AConfig);
  { create USB interface }
  Intf := TLibUsbDevice.FindInterface(AInterface,AAltInterface,FDevice); { better: search for iInterface = "SNAPI Command Interface" }
  FSNAPICommandInterface := TLIBUSBPseudoHIDInterface.Create(Self,Intf);
  FSNAPICommandInterface.OnIntrReport := @IntrReport;
End;

Destructor TSNAPIDevice.Destroy;
Begin
  if (Self <> Nil) and (FSNAPICommandInterface <> Nil) then
    Begin
      ReleaseTrigger;
      ScanDisable;
    End;
  FSNAPICommandInterface.Free;
  inherited Destroy;
End;

Function TSNAPIDevice.WaitForACK(ReportID : Byte): TSNAPIError;
Const BufSize = 64;
Var Buf : Array[0..BufSize-1] of Byte;
    Len : Integer;
    Status : PHIDReport;
Begin
  Len := FSNAPICommandInterface.InterruptRead;
  if Len < 0 then
    Begin
      WriteLn('No Interrupt In!');
      Exit(seNoACK);  // ERROR
    End;
  Status := FSNAPICommandInterface.HasReport(SNAPI_OUTPUT_REPORT_STATUS);
  if Status = Nil then
    Begin
      WriteLn('No Status Report!');
      Exit(seNoACK);  // ERROR
    End;
  if Status^.Data[0] = ReportID then
    Begin
      FSNAPICommandInterface.EatReport(Status);
      Case Status^.Data[1] of
        SNAPI_STATUS_OK : Begin
          //WriteLn('Ok');
          FreeMem(Status);
          Exit(seOk);
        End;
        SNAPI_STATUS_COMMAND_NOT_SUPPORTED : Begin
          WriteLn('Command not supported');
          FreeMem(Status);
          Exit(seCommandNotSupported)
        End;
      else
        WriteLn('Unknown SNAPI Status: $',IntToHex(Status^.Data[1],2));
        FreeMem(Status);
      End;
    End;
End;

Procedure TSNAPIDevice.SetOutputReport(ReportID:Byte;Data:Byte);
Var Ret : TSNAPIError;
Begin
  FSNAPICommandInterface.SetOutputReport(ReportID, Data, 1);
  Ret := WaitForAck(ReportID);
  Case Ret of
    seOk                  : ;
    seNoACK               : raise ESNAPINoAck.Create('No Ack');
    seCommandNotSupported : raise ESNAPICommandNotSupported.Create('Command not Supported');
    else
      raise ESNAPIError.CreateFmt('Unknown error %d',[Ord(Ret)]);
  End;
End;

Procedure TSNAPIDevice.SendACK(ReportID : Byte);
Var ACK : Array[0..2] of Byte;
Begin
  ACK[0] := ReportID;
  ACK[1] := SNAPI_STATUS_OK;
  ACK[2] := 0;
  FSNAPICommandInterface.SetOutputReport(SNAPI_OUTPUT_REPORT_ACK, ACK, 3);
End;

Function TSNAPIDevice.IntrReport(Report: PHIDReport): Boolean;
Var St : String;
    I  : Integer;
    Len : Integer;
    Pos : Integer;
Begin
  Result := False;
  Case Report^.ReportID of
    SNAPI_OUTPUT_REPORT_BARCODE : Begin
      // every single report has to be acknowledged, even if one barcode is split into multiple reports
      SendACK(SNAPI_OUTPUT_REPORT_BARCODE);
      if PReportBarcode(Report)^.Length > 26 then
        Begin
          WriteLn('Error: Invalid length of barcode: ',PReportBarcode(Report)^.Length);
          Exit(True);
        End;
      if PReportBarcode(Report)^.Packets > 1 then
        Begin
          //WriteLn('Received packet ',PReportBarcode(Report)^.PacketNum,' of ',PReportBarcode(Report)^.Packets);
          // first fragment? create array
          if Length(FFragmentedBarcode) = 0 then
            SetLength(FFragmentedBarcode,PReportBarcode(Report)^.Packets);
          // check if this report belongs to us
          if PReportBarcode(Report)^.Packets <> Length(FFragmentedBarcode) then    // correct number of reports?
            Begin
              WriteLn('Error: Fragment with different total number of packets');
              SetLength(FFragmentedBarcode,0);
              Exit(True);
            End;
          if FFragmentedBarcode[PReportBarcode(Report)^.PacketNum].Packets <> 0 then  // not already received?
            Begin
              WriteLn('Error: Fragment with same number already received');
              SetLength(FFragmentedBarcode,0);
              Exit(True);
            End;
          // store report
          FFragmentedBarcode[PReportBarcode(Report)^.PacketNum] := PReportBarcode(Report)^;
          FreeMem(Report);
          // check if all reports have arrived
          For I := 0 to Length(FFragmentedBarcode)-1 do
            if FFragmentedBarcode[I].Packets = 0 then
              Exit(True);   // this entry was not yet set, i.e. we are not yet finished
          // all fragments have arrived
          Len := 0;
          For I := 0 to Length(FFragmentedBarcode)-1 do
            Len := Len + FFragmentedBarcode[I].Length;
          SetLength(St,Len);
          Pos := 1;
          For I := 0 to Length(FFragmentedBarcode)-1 do
            Begin
              Move(FFragmentedBarcode[I].Barcode,St[Pos],FFragmentedBarcode[I].Length);
              Pos := Pos + FFragmentedBarcode[I].Length;
            End;
          // Debug
          WriteLn('Barcode: ',St,' Packets: ',Length(FFragmentedBarcode));
          // remove array
          SetLength(FFragmentedBarcode,0);
        End
      else
        Begin
          SetLength(St,PReportBarcode(Report)^.Length);
          Move(PReportBarcode(Report)^.Barcode,St[1],PReportBarcode(Report)^.Length);
          With PReportBarcode(Report)^ do
            WriteLn('Barcode: ',St,' Packets: ',Packets,' # ',PacketNum,' Code Type: ',Ord(CodeType){doesn't work with holes:,'=',GetEnumName(TypeInfo(TSNAPICodeType),Ord(CodeType))},' U3: ',Unknown3);
          FreeMem(Report);
        End;

      if FOnBarcode <> Nil then
        FOnBarcode(St);
      Result := True;
    End;
  else
  End;
End;

Procedure TSNAPIDevice.AbortMacroPdf;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_MACRO_PDF,$01);
End;

Procedure TSNAPIDevice.FlushMacroPdf;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_MACRO_PDF,$00);
End;

Procedure TSNAPIDevice.AimOff;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_AIM,$00);
End;

Procedure TSNAPIDevice.AimOn;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_AIM,$01);
End;

Procedure TSNAPIDevice.Beep(Code: TSNAPIBeepCode);
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_BEEP,Byte(Code));
End;

Procedure TSNAPIDevice.EnterLowPwrMode;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_LOW_POWER,$01);
End;

function TSNAPIDevice.GetSerialNumber: String;
Begin

End;

Procedure TSNAPIDevice.LedOff(LEDs: Byte);
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_AIM,$00);  // TODO: but how to set the LEDs?
End;

Procedure TSNAPIDevice.LedOn(LEDs: Byte);
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_AIM,$01);
End;

Procedure TSNAPIDevice.PullTrigger;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_TRIGGER,$01);
End;

Procedure TSNAPIDevice.ReleaseTrigger;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_TRIGGER,$00);
End;

Procedure TSNAPIDevice.RequestParameters(Parameters: array of Word);
Begin

End;

Procedure TSNAPIDevice.RequestScannerCapabilities;
Begin

End;

Procedure TSNAPIDevice.ScanDisable;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_SCANNING,$00);
End;

Procedure TSNAPIDevice.ScanEnable;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_SCANNING,$01);
End;

Procedure TSNAPIDevice.SetParamPersistance(Persistent: Boolean);
Begin

End;

Procedure TSNAPIDevice.SetParameterDefaults;
Begin
  SetOutputReport(SNAPI_OUTPUT_REPORT_SET_DEFAULTS,$00);
  // this is followed by a Param Stored Event
End;

Procedure TSNAPIDevice.SetParameters(Parameters: array of Word);
Begin

End;

Procedure TSNAPIDevice.SnapShot;
Begin

End;

Procedure TSNAPIDevice.TransmitVersion;
Begin

End;

Procedure TSNAPIDevice.TransmitVideo;
Begin

End;

Procedure TSNAPIDevice.Listen;
Var Len : Integer;
Begin
  Len := FSNAPICommandInterface.InterruptRead;
  // negative values are pre-filtered, i.e. ESysETIMEDOUT is returned as 0
  if Len < 0 then
    raise Exception.Create('TSNAPIDevice.Listen',Len);
End;

End.

