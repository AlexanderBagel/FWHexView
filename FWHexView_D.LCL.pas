{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FWHexView_D.LCL;

{$warn 5023 off : no warning about unused units}
interface

uses
  FWHexView.AsmTokenizer, FWHexView.Common, FWHexView.MappedView, FWHexView, 
  FWHexView.Reg, FWHexView.Actions, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FWHexView.Reg', @FWHexView.Reg.Register);
end;

initialization
  RegisterPackage('FWHexView_D.LCL', @Register);
end.
