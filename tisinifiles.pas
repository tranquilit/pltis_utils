{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclIniFiles.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is John C Molyneux.                                   }
{ Portions created by John C Molyneux are Copyright (C) John C Molyneux.                           }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Eric S. Fisher                                                                                 }
{   John C Molyneux                                                                                }
{   Petr Vones (pvones)                                                                            }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2011-09-03 00:07:50 +0200 (sam., 03 sept. 2011)                         $ }
{ Revision:      $Rev:: 3599                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit tisinifiles;

interface

uses
  SysUtils, Classes, IniFiles, tisstrings ;

type

  { TTisInifiles }

  TTisInifiles = class(TIniFile)
  public
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;

  end;

// Initialization (ini) Files
function IniReadBool(const FileName, Section, Line: string;Default:Boolean=False): Boolean;              // John C Molyneux
function IniReadInteger(const FileName, Section, Line: string;Default:Integer=0): Integer;           // John C Molyneux
function IniReadString(const FileName, Section, Line: string;Default:String=''): string;             // John C Molyneux
procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);     // John C Molyneux
procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);  // John C Molyneux
procedure IniWriteString(const FileName, Section, Line, Value: string);            // John C Molyneux

procedure IniDeleteKey(const FileName, Section, Line: string);

function IniHasKey(const FileName, Section, Line: string):Boolean;

implementation

procedure IniDeleteKey(const FileName, Section, Line: string);
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Ini.DeleteKey(Section, Line);
  finally
    Ini.Free;
  end;
end;

// Initialization Files
function IniReadBool(const FileName, Section, Line: string; Default: Boolean
  ): Boolean;
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Result := Ini.ReadBool(Section, Line, Default);
  finally
    Ini.Free;
  end;
end;

function IniReadInteger(const FileName, Section, Line: string; Default: Integer
  ): Integer;
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Result := Ini.ReadInteger(Section, Line, Default);
  finally
    Ini.Free;
  end;
end;

function IniReadString(const FileName, Section, Line: string; Default: String
  ): string;
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Result := Ini.ReadString(Section, Line, Default);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Ini.WriteBool(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Ini.WriteInteger(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteString(const FileName, Section, Line, Value: string);
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Ini.WriteString(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

// Initialization (ini) Files helper routines
const
  ItemCountName = 'Count';

function IniHasKey(const FileName, Section, Line: string): Boolean;
var
  Ini: TTisInifiles;
begin
  Ini := TTisInifiles.Create(FileName);
  try
    Result := Ini.ValueExists(Section,Line);
  finally
    Ini.Free;
  end;
end;

{ TTisInifiles }

function TTisInifiles.ReadBool(const Section, Ident: string; Default: Boolean
  ): Boolean;
const
  BoolTrueValues: array [0..3] of String = ('true', '1', 'yes', 'on');
  BoolFalseValues: array [0..3] of String = ('false', '0', 'no', 'off');
var
  s: String;
begin
  Result := Default;
  s:= ReadString(Section, Ident, '').ToLower;
  if s = '' then
    Exit;
  if TStringArray(BoolTrueValues).Exist(S) then
    Result:=True
  else if TStringArray(BoolFalseValues).Exist(S) then
    Result:=False;
end;

end.
