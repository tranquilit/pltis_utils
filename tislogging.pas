unit tislogging;
{ -----------------------------------------------------------------------
#    This file is part of WAPT
#    Copyright (C) 2013  Tranquil IT Systems http://www.tranquil.it
#    WAPT aims to help Windows systems administrators to deploy
#    setup and update applications on users PC.
#
#    Part of this file is based on JEDI JCL library
#
#    WAPT is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    WAPT is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with WAPT.  If not, see <http://www.gnu.org/licenses/>.
#
# -----------------------------------------------------------------------
}
interface

uses
  Classes, SysUtils {$ifdef windows},Windows{$endif};

type LogLevel=(DEBUG, INFO, WARNING, ERROR, CRITICAL);
const StrLogLevel: array[DEBUG..CRITICAL] of String = ('DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL');
procedure Logger(Msg:String;level:LogLevel=WARNING);

procedure ODS(Msg:String);

var
  loghook : procedure(logmsg:String) of object;

const
    currentLogLevel:LogLevel=WARNING;

implementation

procedure Logger(Msg: String;level:LogLevel=WARNING);
begin
  if level>=currentLogLevel then
  begin
    if IsConsole then
      WriteLn(Msg)
    else
      if Assigned(loghook) then
        loghook(Msg);
  end;
end;


{$ifdef windows}
procedure ODS(Msg:String);
var
  umsg: UnicodeString;
begin
  umsg := UTF8Decode(msg);
  OutputDebugStringW(PWideChar(umsg));
end;
{$else}
procedure ODS(Msg:String);
begin

end;
{$endif}

end.

