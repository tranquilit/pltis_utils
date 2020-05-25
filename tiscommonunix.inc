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

{$ifdef unix}
interface

uses
  Classes, SysUtils,tisstrings,Process;

Procedure UnzipFile(ZipFilePath,OutputPath:Utf8String);
procedure AddToSystemPath(APath:Utf8String);

procedure UpdateCurrentApplication(fromURL:Utf8String;Restart:Boolean;restartparam:Utf8String);
procedure UpdateApplication(fromURL:Utf8String;SetupExename,SetupParams,ExeName,RestartParam:Utf8String);

function SortableVersion(VersionString:String):String;
function CompareVersion(const v1,v2:String):integer;

function GetComputerName : String;
function GetUserName : String;
function GetWorkgroupName: String;
function GetDomainName: String;
function UserInGroup(Group :DWORD) : Boolean;

function CheckOpenPort(dwPort : Word; ipAddressStr:AnsiString;timeout:integer=5000):boolean;
function GetFreeLocalPort( portStart : Word = 5000; portEnd : Word = 10000):Word;
function GetIPFromHost(const HostName: ansistring): ansistring;

function MakePath(const parts:array of Utf8String):Utf8String;
function RunTask(cmd: utf8string;var ExitStatus:integer;WorkingDir:utf8String='';ShowWindow:TShowWindowOptions=swoHIDE): utf8string;

function GetCmdParams(ID:Utf8String;Default:Utf8String=''):Utf8String;

Function GetAppUserFolder:Utf8String;

function Appuserinipath:Utf8String;

function GetApplicationVersion(FileName:Utf8String=''): Utf8String;

implementation

uses registry,LazFileUtils,LazUTF8, zipper,tiswinhttp,tislogging
    {$ifdef unix}
    , baseunix,
    {$ifndef darwin}
    cnetdb,
    {$else}
    netdb,
    {$endif}
    errors, sockets, unix
    {$endif}
    ;

function MakePath(const parts:array of Utf8String):Utf8String;
var
  i:integer;
begin
  result := '';
  for i:=low(parts) to high(parts) do
  begin
    result := Result+parts[i];
    if (i<High(parts)) and (parts[i][length(parts[i])] <> PathDelim) then
      result := result+PathDelim;
  end;
end;


function GetBIOSDate: AnsiString;
begin
  Result := 'Unknown BIOS version';
end;

function UserInGroupUnix(Group: Integer) : Boolean;
begin
 { TODO }
  Result := False;
end;

function UserInGroup(Group: DWORD): Boolean;
begin
  Result := UserInGroupUnix(Integer(Group));
end;


//Unzip file to path, and return list of files as a string
procedure UnzipFile(ZipFilePath, OutputPath: Utf8String);
var
  UnZipper: TUnZipper;
begin
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := ZipFilePath;
    UnZipper.OutputPath := OutputPath;
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;
  finally
    UnZipper.Free;
  end;
end;

procedure AddToSystemPath(APath: Utf8String);
begin
  { XXX TODO }
end;


procedure UpdateCurrentApplication(fromURL: Utf8String; Restart: Boolean;
  restartparam: Utf8String);
var
  bat: TextFile;
  tempdir,tempfn,updateBatch,fn,zipfn,version,destdir : Utf8String;
  files:TStringList;
  UnZipper: TUnZipper;
  i:integer;
  hinstance:Integer;
begin
  { TODO }
end;

function GetUniqueTempdir(Prefix: Utf8String): Utf8String;
var
  I: Integer;
  Start: Utf8String;
begin
  Start:=GetTempDir;
  if (Prefix='') then
      Start:=Start+'TMP'
  else
    Start:=Start+Prefix;
  I:=0;
  repeat
    Result:=Format('%s%.5d.tmp',[Start,I]);
    Inc(I);
  until not DirectoryExistsUTF8(Result);
end;


procedure UpdateApplication(fromURL:Utf8String;SetupExename,SetupParams,ExeName,RestartParam:Utf8String);
var
  bat: TextFile;
  tempdir,tempfn,updateBatch,zipfn,version : Utf8String;
  files:TStringList;
  UnZipper: TUnZipper;
  i,hinstance:integer;
begin
     { TODO }
end;

function GetUserNameUnix: AnsiString;
var
  User: String;
begin
  Result := 'john.doe';

  // LOGNAME is POSIX, USER is BSD
  User := GetEnvironmentVariable('LOGNAME');
  if User = '' then
    User := GetEnvironmentVariable('USER');

  { XXX fallback on fpgetuid + getpwnam? }

  Result := User;
end;

function GetUserName: String;
begin
  Result := GetUserNameUnix();
end;

procedure StrResetLength(var S: AnsiString);
var
  I: SizeInt;
begin
  for I := 1 to Length(S) do
    if S[I] = #0 then
    begin
      SetLength(S, I);
      Exit;
    end;
end;

function GetUserDomainName(const curUser: Ansistring): AnsiString;
begin
  Result := 'unknowndomain';
end;

function GetWorkGroupNameUnix(): AnsiString;
begin
  Result := 'unknowndomain';
end;

function GetWorkgroupName: String;
begin
  Result := GetWorkGroupNameUnix();
end;

function GetDomainNameUnix(): WideString;
var
  Res, Host: WideString;
begin
  // Per RFC 6761
  Result := 'invalid';

  Res := unix.GetDomainName();
  if Res = '(none)' then
    Res := '';

  if Res = '' then
  begin
    Host := gethostname();
    if (Host <> '') and (Pos('.', Host) > 0) then
      Res := Copy(Host, Pos('.', Host) + 1, 255);
  end;

  if Res = '' then
  { XXX parse resolv.conf? }
  ;

  if Res <> '' then
    Result := Res;

end;

function GetDomainName(): String;
begin
  Result := GetDomainNameUnix();
end;

function SortableVersion(VersionString: String): String;
var
  version,tok : String;
begin
  version := VersionString;
  tok := StrToken(version,'.');
  Result :='';
  repeat
    if tok[1] in ['0'..'9'] then
      Result := Result+FormatFloat('0000',StrToInt(tok))
    else
      Result := Result+tok;
    tok := StrToken(version,'.');
  until tok='';
end;

function CompareVersion(const v1,v2:String):integer;
var
  version1,version2,pack1,pack2,tok1,tok2:String;
begin
  // '1.2.3-4';
  pack1 := v1;
  pack2 := v2;

  version1 := StrToken(pack1,'-');
  version2 := StrToken(pack2,'-');

  //version base
  repeat
    tok1 := StrToken(version1,'.');
    tok2 := StrToken(version2,'.');
    if (tok1<>'') or (tok2<>'') then
    try
      result := StrToInt(tok1)-StrToInt(tok2);
    except
      result := CompareStr(tok1,tok2)
    end;
    if (result<>0) or (tok1='') or (tok2='') then
      break;
  until (result<>0) or (tok1='') or (tok2='');

  // packaging
  if (Result=0) and ((pack1<>'') or (pack2<>'')) then
  begin
    if (pack1<>'') or (pack2<>'') then
    try
      result := StrToInt(pack1)-StrToInt(pack2);
    except
      result := CompareStr(pack1,pack2)
    end;
  end;
end;

procedure Logger(Msg: Utf8String;level:LogLevel=WARNING);
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

function GetComputerNameUnix : AnsiString;
var
  Host: AnsiString;
begin
  Result := 'unknown';

  Host := gethostname();
  if (Host <> '') then
  begin
    if Pos('.', Host) > 1 then
      Result := Copy(Host, 1, Pos('.', Host) - 1)
    else
      Result := Host;
  end;
end;

function GetComputerName : String;
begin
  Result := GetComputerNameUnix();
end;

function WaitForConnection(sock: cint; timeout:LongInt):Boolean;
var
  wrsel: TFDSet;
  res, sockopt_res, optval: cint;
  optlen: TSockLen;
  tv: timeVal;
  ptv: ptimeval;
begin

  if timeout = -1 then
    ptv := nil
  else
  begin
    tv.tv_Sec := Timeout div 1000;
    tv.tv_Usec := (Timeout mod 1000)*1000;
    ptv := @tv;
  end;

  fpFD_ZERO(wrsel);
  fpFD_SET(sock, wrsel);

  while True do
  begin
    res := fpselect(sock + 1, nil, @wrsel, nil, ptv);
    if (res < 0) then
    begin
      // select() error, did we get a signal?
      if (fpGetErrno() <> EsysEINTR) then
      begin
        logger('error within fpselect: ' + strerror(fpGetErrno()));
        Exit;
      end
    end
    else if (res > 0) then
    begin
      // select() indicates writability
      Assert(fpFD_IsSet(sock, wrsel) <> 0, 'fpselect returned > 0 but there is no socket marked as writable');
      // we have the result of the connect() call; is it a success?
      optval := 0;
      optlen := sizeof(optval);
      sockopt_res := fpgetsockopt(sock, SOL_SOCKET, SO_ERROR, @optval, @optlen);
      if sockopt_res <> 0 then
      begin
        // unexpected error, better bail out
        writeln('WaitForConnection: fpgetsockopt() failed: ', StrError(fpGetErrno()));
        Exit;
      end
      else if optval = 0 then
      begin
        // connect() succeeded
        Result := True;
        Exit;
      end
      else if (optval = EsysEINPROGRESS) or (optval = EsysEALREADY) then
        // nothing
      else
      begin
        // connect() error; silently return False
        //writeln('WaitForConnection: connect() failed: ', Strerror(optval));
        Exit;
      end
      ;
    end
    else
    begin
       writeln('WaitForConnection: fpselect() timeout');
       Exit;
    end;
  end;
end;

function CheckOpenPort(dwPort : Word; ipAddressStr:AnsiString; timeout: integer = 5000):boolean;
var
  sock, sockflags, res: cint;
  sin: sockaddr_in;
  ip: AnsiString;
begin
 Result := True; // TODO CHANGE, obviously
 {
  Result := False;
  sock := -1;

  ip := GetIPFromHost(ipAddressStr);
  if ip = '' then
    Exit;

  FillChar(sin, sizeof(sin), 0);
  sin.sin_family := AF_INET;
  sin.sin_port := htons(dwPort);
  sin.sin_addr := StrToNetAddr(ip);

  try
    sock := fpsocket(AF_INET, SOCK_STREAM, 0);
    if sock = -1 then
      Exit;

    sockflags := fpfcntl(sock, F_GetFl);
    if sockflags = -1 then
      Exit;
    sockflags := sockflags or O_NONBLOCK;
    if fpfcntl(sock, F_SetFl, sockflags) = -1 then
      Exit;

    res := fpconnect(sock, psockaddr(@sin), sizeof(sin));

    if res = 0 then
      Result := True
    else if (res = -1) and ((fpGetErrno() = EsysEINPROGRESS) or (fpGetErrno() = EsysEALREADY)) then
      Result := WaitForConnection(sock, timeout)
    ;

  finally
    if sock <> -1 then
      fpclose(sock);
  end;
   }
end;

function GetFreeLocalPort( portStart : Word = 5000; portEnd : Word = 10000):Word;
begin
  Result := portStart + random(portEnd-portStart);
  while CheckOpenPort(Result,'127.0.0.1',500) do
    Result := portStart + random(portEnd-portStart);

end;

{$ifdef linux}
function GetIPFromHostLinux(const Hostname: AnsiString): AnsiString;
type
  TaPInAddr = array[0..10] of pin_addr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostent;
  pptr: PaPInAddr;
begin
  Result := '';
  phe := gethostbyname(PAnsiChar(Hostname));
  if phe = nil then
    Exit;
  pPtr := PaPInaddr(phe^.h_addr_list);
  if pPtr^[0] <> nil then
    Result := NetAddrToStr(pPtr^[0]^);
end;
{$endif}

{$ifdef darwin}
function GetIPFromHostMacOS(const Hostname: AnsiString): AnsiString;
type
  TaPInAddr = array[0..10] of pin_addr;
  PaPInAddr = ^TaPInAddr;
var
  phe: THostEntry;
  res: Boolean;
begin
  Result := '';
  res := GetHostByName(PAnsiChar(Hostname), phe);
  if res = false then
    Exit;
  Result := phe.name
end;
{$endif}

function GetIPFromHost(const HostName: ansistring): ansistring;
begin
  {$if defined(linux)}
  Result := GetIPFromHostLinux(Hostname);
  {$elseif defined(darwin)}
  Result := GetIPFromHostMacOS(Hostname);
  {$else}
  raise ENotImplemented.Create('OS not supported');
  {$endif}
end;

function RunTask(cmd: utf8string;var ExitStatus:integer;WorkingDir:utf8String='';ShowWindow:TShowWindowOptions=swoHIDE): utf8string;
var
  AProcess: TProcess;
  AStringList: TStringList;
  Wow64FsEnableRedirection: LongBool;
begin
  try
    AProcess := TProcess.Create(nil);
    AStringList := TStringList.Create;
    try
      AProcess.CommandLine := cmd;
      if WorkingDir<>'' then
        AProcess.CurrentDirectory := ExtractFilePath(cmd);
      AProcess.Options := AProcess.Options + [poStderrToOutPut, poWaitOnExit, poUsePipes];
      AProcess.ShowWindow:=ShowWindow;
      AProcess.Execute;
      while AProcess.Running do;
      AStringList.LoadFromStream(AProcess.Output);
      Result := AStringList.Text;
      ExitStatus:= AProcess.ExitStatus;
    finally
      AStringList.Free;
      AProcess.Free;
    end;

  finally
  end;
end;


procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;


function GetCmdParams(ID:Utf8String;Default:Utf8String=''):Utf8String;
var
	i:integer;
	S:Utf8String;
  found:Boolean;
begin
	Result:='';
  found:=False;
	for i:=1 to ParamCount do
	begin
		S:=ParamStrUTF8(i);
		if
			(UTF8CompareText(Copy(S, 1, Length(ID)+2), '/'+ID+'=') = 0) or
			(UTF8CompareText(Copy(S, 1, Length(ID)+2), '-'+ID+'=') = 0) or
			(UTF8CompareText(Copy(S, 1, Length(ID)+3), '--'+ID+'=') = 0) then
		begin
      found:=True;
			Result:=Copy(S,pos('=',S)+1,MaxInt);
			Break;
		end;
	end;
  if not Found then
    Result:=Default;
end;

// to store use specific settings for this application
function Appuserinipath:Utf8String;
var
  dir : Utf8String;
begin
  Result:='/home/octavel/.local/wapt/wapt.ini'; // TODO change, obviously
end;

Function GetAppUserFolder : Utf8String;
begin
  Result := '/home/olarose/.local/wapt'; // TODO CHANGE, obviously
end;

function GetApplicationVersion(FileName: Utf8String): Utf8String;
begin
  Result:=''; // TODO change
end;

end.
{$endif}
