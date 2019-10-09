{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit avamm_dblayer_sqlitedb;

{$warn 5023 off : no warning about unused units}
interface

uses
  usqlitedm, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('avamm_dblayer_sqlitedb', @Register);
end.
