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


function UserInGroupUnix(Group: Integer) : Boolean;
begin
 { TODO }
  Result := False;
end;

function UserInGroup(Group: DWORD): Boolean;
begin
  Result := UserInGroupUnix(Integer(Group));
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

function GetIPFromHost(const Hostname: AnsiString): AnsiString;
type
  TaPInAddr = array[0..10] of pin_addr;
  PaPInAddr = ^TaPInAddr;
var
  phe: {$IF defined(LINUX)}PHostent {$ELSEIF defined(DARWIN)} THostEntry {$ENDIF};
  {$IF defined(LINUX)}
  pPtr: PaPInAddr;
  {$ELSEIF defined(DARWIN)}
  res: Boolean;
  {$ENDIF}
begin
  Result := '';
  phe := GetHostByName(PAnsiChar(Hostname){$IFDEF DARWIN}, phe {$ENDIF});
  if {$IF defined(LINUX)} phe = nil {$ELSEIF defined(DARWIN)} not(Res) {$ENDIF} then
    Exit;
  {$IF defined(LINUX)}
  pPtr := PaPInaddr(phe^.h_addr_list);
  if pPtr^[0] <> nil then
    Result := NetAddrToStr(pPtr^[0]^);
  {$ELSEIF defined(DARWIN)}
  Result := phe.name
  {$ENDIF}
end;

function AppUserIniPath:Utf8String;
begin
  if not DirectoryExistsUTF8(GetAppUserFolder) then
    MkDir(GetAppUserFolder);
  Result:=IncludeTrailingPathDelimiter(GetAppUserFolder)+ApplicationName+'.ini';
end;

function GetAppUserFolder : Utf8String;
begin
  Result := IncludeTrailingPathDelimiter(GetUserDir)+ApplicationName;
end;

function GetApplicationVersion(FileName: Utf8String): Utf8String;
begin
  Result:=''; // TODO change
end;
