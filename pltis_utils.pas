{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_utils;

interface

uses
  tisutils, TisDateTime, tishttp, tisstrings, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pltis_utils', @Register);
end.
