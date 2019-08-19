Unit BarcodeScannerSymbolMS4407;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

Interface

Uses
  Classes, SysUtils,List,
  Callback,DevOperator,CharBuf,BarcodeScanner,USB,SNAPI;


Type
  { CSNAPIBarcodeScanner }

  CSNAPIBarcodeScanner = class(TSNAPIDevice,IBarcodeScanner,IPollable)
  private
    { callbacks }
    BarcodeCallback   : CCallback;
    { local variables }
    Active            : Boolean;
    { callback variable for my installed cbRecv }
    RecvCallback      : CCallback;
    { callbacks }
    Procedure   NotifyCallback(aBarcode:String);
  public
    Constructor Create(idVendor,idProduct:LongInt;AConfig,AInterface,AAltInterface:LongInt); overload;
    Destructor  Destroy; override;
    (** Interface IBarcodeScanner ********************************************)
    { callback admin methods }
    Function    AddBarcodeCallback(aCallBack:cbCallback;aData:Pointer) : CCallback;
    Function    DelBarcodeCallback(aCallback:CCallback):LongInt;
    { top level methods }
    Procedure Activate;      virtual;
    Procedure Deactivate;    virtual;
    Procedure NotifySuccess; virtual;
    Procedure NotifyProblem; virtual;
    Procedure Operate;
  End;

  { CBarcodeScannerSymbolMS4407 }

  CBarcodeScannerSymbolMS4407 = class(CSNAPIBarcodeScanner)
  public
    Constructor Create;
    Destructor  Destroy; override;
  End;

Implementation

Constructor CSNAPIBarcodeScanner.Create(idVendor,idProduct:LongInt;AConfig,AInterface,AAltInterface:LongInt);
Begin
  inherited Create(idVendor,idProduct,AConfig,AInterface,AAltInterface);
  { set local variables }
  Active := false;
  OnBarcode := @NotifyCallback;
  TheDevOperator.Add(Self);
End;

Destructor CSNAPIBarcodeScanner.Destroy;
Begin
  try
    TheDevOperator.Del(Self);
  Except
    On E : Exception do
      ;  // ignore, this most proably happend because we are called due to an exception in the constructor
  End;
  Deactivate;
  Inherited Destroy;
End;

Procedure CSNAPIBarcodeScanner.Activate;
Begin
  Active := True;
  { switch on barcode scanner's read function }
  ScanEnable;
  PullTrigger;
End;

Procedure CSNAPIBarcodeScanner.Deactivate;
Begin
  if Active = False then Exit;
  Active := False;
  { switch off barcode scanner's read function }
  ReleaseTrigger;
  ScanDisable;
End;

Procedure CSNAPIBarcodeScanner.NotifySuccess;
Begin
  Beep(bcOneShortHi);
End;

Procedure CSNAPIBarcodeScanner.NotifyProblem;
Begin
  Beep(bcFastHiLoHiLo);
End;

Procedure CSNAPIBarcodeScanner.Operate;
Begin
  Listen;
End;

Procedure CSNAPIBarcodeScanner.NotifyCallback(aBarcode:String);
Var BarcodeEvent : TBarcodeEvent;
Begin
  BarcodeEvent.Barcode := aBarcode;
  if Assigned(BarcodeCallback) then
    BarcodeCallback.Execute(@BarcodeEvent);
End;

Function  CSNAPIBarcodeScanner.AddBarcodeCallback(aCallBack:cbCallback;aData:Pointer) : CCallback;
Begin
  Result := CCallback.Create(@BarcodeCallback,aCallback,aData);
End;
Function CSNAPIBarcodeScanner.DelBarcodeCallback(aCallback:CCallback):LongInt;
Begin
  { pricipially we only need to destroy the aCallback class instance }
  { But for security we search it in our UARTRecvCallbacks that we   }
  { don't delete foreigh callbacks. }
  Result := 0;
  if aCallback.InList(BarcodeCallback) then
    Begin
      aCallback.Free;
      Exit;
    End;
  raise Exception.Create('INTERNAL ERROR: CSNAPIBarcodeScanner.DelBarcodeCallback was called with a wrong callback');
  Result := -1;  { not necessary, because we will not reach this point }
End;

Const USBVend : LongInt = $05E0;
      USBProd : LongInt = $1900;

{ CBarcodeScannerSymbolMS4407 }

Constructor CBarcodeScannerSymbolMS4407.Create;
Begin
  try
    inherited Create(USBVend,USBProd,1,0,0);
  Except
    on E : Exception do
      Begin
        WriteLn('Couldn''t instantiate a Symbol MS4407 Barcode Scanner.');
        raise;
      End;
  End;
End;

Destructor CBarcodeScannerSymbolMS4407.Destroy;
Begin
  Inherited Destroy;
End;

End.

