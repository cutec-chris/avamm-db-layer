{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit avammdblayer;

{$warn 5023 off : no warning about unused units}
interface

uses
  uBaseDatasetInterfaces, uAbstractDBLayer, uEncrypt, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('avammdblayer', @Register);
end.
