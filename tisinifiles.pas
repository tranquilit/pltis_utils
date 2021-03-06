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
  SysUtils, Classes, IniFiles;

// Initialization (ini) Files
function IniReadBool(const FileName, Section, Line: string;Default:Boolean=False): Boolean;              // John C Molyneux
function IniReadInteger(const FileName, Section, Line: string;Default:Integer=0): Integer;           // John C Molyneux
function IniReadString(const FileName, Section, Line: string;Default:String=''): string;             // John C Molyneux
procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);     // John C Molyneux
procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);  // John C Molyneux
procedure IniWriteString(const FileName, Section, Line, Value: string);            // John C Molyneux

procedure IniDeleteKey(const FileName, Section, Line: string);

function IniHasKey(const FileName, Section, Line: string):Boolean;

// Initialization (ini) Files helper routines
procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);


implementation

procedure IniDeleteKey(const FileName, Section, Line: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
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
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadBool(Section, Line, Default);
  finally
    Ini.Free;
  end;
end;

function IniReadInteger(const FileName, Section, Line: string; Default: Integer
  ): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadInteger(Section, Line, Default);
  finally
    Ini.Free;
  end;
end;

function IniReadString(const FileName, Section, Line: string; Default: String
  ): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadString(Section, Line, Default);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteBool(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteString(const FileName, Section, Line, Value: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
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
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ValueExists(Section,Line);
  finally
    Ini.Free;
  end;
end;

procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  Count, I: Integer;
begin
  with IniFile do
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      Count := ReadInteger(Section, ItemCountName, 0);
      for I := 0 to Count - 1 do
        Strings.Add(ReadString(Section, IntToStr(I), ''));
    finally
      Strings.EndUpdate;
    end;
  end;
end;

procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  I: Integer;
begin
  with IniFile do
  begin
    EraseSection(Section);
    WriteInteger(Section, ItemCountName, Strings.Count);
    for I := 0 to Strings.Count - 1 do
      WriteString(Section, IntToStr(I), Strings[I]);
  end;
end;

end.
