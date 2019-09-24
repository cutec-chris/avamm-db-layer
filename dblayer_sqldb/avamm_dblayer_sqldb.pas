{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit avamm_dblayer_sqldb;

{$warn 5023 off : no warning about unused units}
interface

uses
  uSQLDBDM, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('avamm_dblayer_sqldb', @Register);
end.
