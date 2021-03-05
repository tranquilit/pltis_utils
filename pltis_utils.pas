{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_utils;

{$warn 5023 off : no warning about unused units}
interface

uses
  tisutils, tisdatetime, tisstrings, tisinifiles, tiscommon, tislogging, 
  tiswinhttp, tiswinhttpini, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pltis_utils', @Register);
end.
