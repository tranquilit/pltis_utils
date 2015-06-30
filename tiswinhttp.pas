unit tiswinhttp;
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



{functions to get files and string using Winet windows http facilities}

{$mode delphiunicode}
{$codepage UTF8}

{$ifndef windows}

interface
implementation
end.

{$else}

interface

uses
  Classes, SysUtils, wininet;

type
    TProgressCallback=function(Receiver:TObject;current,total:Integer):Boolean of object;
    TLoginCallback = function(realm:String;var user,password:String):Boolean of object;
    EHTTPException=Class(Exception)
      HTTPStatus: Integer;
      constructor Create(const msg: string;AHTTPStatus:Integer);
    end;

function wget(const fileURL, DestFileName: Utf8String; CBReceiver:TObject=Nil;progressCallback:TProgressCallback=Nil;enableProxy:Boolean=False;forceReload:Boolean=True): boolean;

function httpGetHeaders(url: ansistring; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000;user:AnsiString='';password:AnsiString=''):RawByteString;
function httpGetString(url: ansistring; enableProxy:Boolean= False;
    ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000;user:AnsiString='';password:AnsiString=''):RawByteString;
function httpPostData(const UserAgent: ansistring; const url: Ansistring; const Data: RawByteString; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000;user:AnsiString='';password:AnsiString=''):RawByteString;

function ignoreCerticateErrors(oRequestHandle:HINTERNET; var aErrorMsg: ansistring): Boolean;

function GetWinInetError(ErrorCode:Cardinal): ansistring;

implementation

uses URIParser,FileUtil,JwaWinType,JwaWinBase;

Function wget(const fileURL, DestFileName: Utf8String; CBReceiver:TObject=Nil;progressCallback:TProgressCallback=Nil;enableProxy:Boolean=False;forceReload:Boolean=True): boolean;
 const
   BufferSize = 1024*512;
 var
   hSession, hURL: HInternet;
   Buffer: array[1..BufferSize] of Byte;
   BufferLen: DWORD;
   f: File;
   sAppName: Utf8string;
   Size: Integer;
   total:DWORD;
   totalLen:DWORD;
   dwindex: cardinal;
   dwcode : array[1..20] of Ansichar;
   dwCodeLen : DWORD;
   res : PAnsiChar;

begin
  result := false;
  sAppName := ExtractFileName(ParamStr(0)) ;
  if enableProxy then
      hSession := InternetOpenW(PWideChar(UTF8Decode(sAppName)), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0)
  else
      hSession := InternetOpenW(PWideChar(UTF8Decode(sAppName)), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0) ;
  try
    if not forceReload then
      hURL := InternetOpenUrlW(hSession, PWideChar(UTF8Decode(fileURL)), nil, 0,  INTERNET_FLAG_DONT_CACHE+INTERNET_FLAG_KEEP_CONNECTION, 0)
    else
      hURL := InternetOpenUrlW(hSession, PWideChar(UTF8Decode(fileURL)), nil, 0, INTERNET_FLAG_DONT_CACHE+INTERNET_FLAG_RELOAD+INTERNET_FLAG_PRAGMA_NOCACHE+INTERNET_FLAG_KEEP_CONNECTION, 0) ;
    if assigned(hURL) then
    try
      dwIndex  := 0;
      dwCodeLen := SizeOf(dwcode);
      totalLen := SizeOf(totalLen);
      HttpQueryInfo(hURL, HTTP_QUERY_STATUS_CODE, @dwcode, dwcodeLen, dwIndex);
      HttpQueryInfo(hURL, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @total,totalLen, dwIndex);
      res := pansichar(@dwcode);
      if (res ='200') or (res ='302') then
      begin
        Size:=0;
        try
          AssignFile(f, UTF8Decode(DestFileName)) ;
          try
            Rewrite(f,1) ;
            repeat
              BufferLen:= 0;
              if InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen) then
              begin
                inc(Size,BufferLen);
                BlockWrite(f, Buffer, BufferLen);
                if Assigned(progressCallback) then
                  if not progressCallback(CBReceiver,size,total) then
                  begin
                    BufferLen:=0;
                    raise EHTTPException.Create('Download stopped by user',0);
                  end;
              end;
            until BufferLen = 0;
          finally
            CloseFile(f);
          end;

        except
          If FileExists(DestFileName) then
            FileUtil.DeleteFileUTF8(DestFileName);
          raise;
        end;
        result := (Size>0);
      end
      else
        raise EHTTPException.Create('Unable to download: "'+fileURL+'", HTTP Status:'+res, int(res^));
    finally
      InternetCloseHandle(hURL)
    end
  finally
    InternetCloseHandle(hSession)
  end
end;


function httpGetHeaders(url: ansistring; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000;user:AnsiString='';password:AnsiString=''):RawByteString;
var
  hInet,hFile,hConnect: HINTERNET;
  buffer: array[1..1024] of byte;
  OutBufferLen:Cardinal;
  OutBuffer:PAnsiChar;
  flags,bytesRead,dwError,port : DWORD;
  pos:integer;
  dwindex,dwcodelen,dwread,dwNumber: cardinal;
  dwcode : array[1..20] of ansichar;
  res    : PAnsiChar;
  doc,error: AnsiString;
  uri :TURI;
  puser,ppassword:PAnsiChar;

begin
  result := '';

  hInet:=Nil;
  hConnect := Nil;
  hFile:=Nil;
  if enableProxy then
     hInet := InternetOpen('wapt',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0)
  else
     hInet := InternetOpen('wapt',INTERNET_OPEN_TYPE_DIRECT,nil,nil,0);
  try
    InternetSetOption(hInet,INTERNET_OPTION_CONNECT_TIMEOUT,@ConnectTimeout,sizeof(integer));
    InternetSetOption(hInet,INTERNET_OPTION_SEND_TIMEOUT,@SendTimeOut,sizeof(integer));
    InternetSetOption(hInet,INTERNET_OPTION_RECEIVE_TIMEOUT,@ReceiveTimeOut,sizeof(integer));
    uri := ParseURI(url,'http',80);
    if (uri.Protocol = 'https') and (uri.port=80) then
      uri.port := 443;

    if uri.Username<>'' then
      puser:= PAnsiChar(uri.Username)
    else
       puser := Nil;
    if uri.Password<>'' then
      ppassword:=PAnsiChar(uri.Password)
    else
      ppassword:=Nil;

    if user<>'' then
      puser:=PAnsiChar(user);
    if password<>'' then
      ppassword:=PAnsiChar(password);

    hConnect := InternetConnect(hInet, PAnsiChar(uri.Host), uri.port, puser, ppassword, INTERNET_SERVICE_HTTP, 0, 0);
    if not Assigned(hConnect) then
      Raise EHTTPException.Create('Unable to connect to '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);
    flags := INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION;
    if uri.Protocol='https' then
      flags := flags or INTERNET_FLAG_SECURE;
    doc := uri.Path+uri.document;
    if uri.params<>'' then
      doc:= doc+'?'+uri.Params;
    hFile := HttpOpenRequest(hConnect, 'HEAD', PAnsiChar(doc), HTTP_VERSION, nil, nil,flags , 0);
    if not Assigned(hFile) then
      Raise EHTTPException.Create('Unable to get doc '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);

    if not HttpSendRequest(hFile, nil, 0, nil, 0) then
    begin
      ErrorCode:=GetLastError;
      if (ErrorCode = ERROR_INTERNET_INVALID_CA) then
      begin
        ignoreCerticateErrors(hFile, error);
        if not HttpSendRequest(hFile, nil, 0, nil, 0) then
          Raise EHTTPException.Create('Unable to send request to '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);
      end;
    end;

    OutBuffer:=Nil;
    OutBufferLen:=0;
    if Assigned(hFile) then
    try
      HttpQueryInfo(hFile, HTTP_QUERY_RAW_HEADERS_CRLF, OutBuffer,@OutBufferLen, Nil);
      if OutBufferLen>0 then
      begin
        OutBuffer := Getmem(OutBufferLen);
        if HttpQueryInfo(hFile, HTTP_QUERY_RAW_HEADERS_CRLF, OutBuffer,@OutBufferLen, Nil) then
        begin
          Result := PAnsiChar(OutBuffer);
          //SetLength(Result,OutBufferLen);
          //Move(OutBuffer,Result[1],OutBufferLen);
        end;
      end;
    finally
      if Assigned(OutBuffer) then
        Freemem(OutBuffer);
      InternetCloseHandle(hFile);
      if Assigned(hFile) then
        InternetCloseHandle(hFile);
    end

  finally
    if Assigned(hConnect) then
      InternetCloseHandle(hConnect);
    if Assigned(hInet) then
      InternetCloseHandle(hInet);
  end;
end;



// récupère une chaine de caractères en http en utilisant l'API windows
function httpGetString(url: ansistring; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000;user:AnsiString='';password:AnsiString=''):RawByteString;
var
  hInet,hFile,hConnect: HINTERNET;
  buffer: array[1..1024] of byte;
  flags,bytesRead,dwError,port : DWORD;
  pos:integer;
  dwindex,dwcodelen,dwread,dwNumber: cardinal;
  dwcode : array[1..20] of ansichar;
  res    : PAnsiChar;
  doc,error: AnsiString;
  uri :TURI;
  puser,ppassword:PAnsiChar;

begin
  result := '';
  hInet:=Nil;
  hConnect := Nil;
  hFile:=Nil;
  if enableProxy then
     hInet := InternetOpen('wapt',INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0)
  else
     hInet := InternetOpen('wapt',INTERNET_OPEN_TYPE_DIRECT,nil,nil,0);
  try
    InternetSetOption(hInet,INTERNET_OPTION_CONNECT_TIMEOUT,@ConnectTimeout,sizeof(integer));
    InternetSetOption(hInet,INTERNET_OPTION_SEND_TIMEOUT,@SendTimeOut,sizeof(integer));
    InternetSetOption(hInet,INTERNET_OPTION_RECEIVE_TIMEOUT,@ReceiveTimeOut,sizeof(integer));
    uri := ParseURI(url,'http',80);
    if (uri.Protocol = 'https') and (uri.port=80) then
      uri.port := 443;

    if uri.Username<>'' then
      puser:= PAnsiChar(uri.Username)
    else
       puser := Nil;
    if uri.Password<>'' then
      ppassword:=PAnsiChar(uri.Password)
    else
      ppassword:=Nil;

    if user<>'' then
      puser:=PAnsiChar(user);
    if password<>'' then
      ppassword:=PAnsiChar(password);

    hConnect := InternetConnect(hInet, PAnsiChar(uri.Host), uri.port, puser, ppassword, INTERNET_SERVICE_HTTP, 0, 0);
    if not Assigned(hConnect) then
      Raise EHTTPException.Create('Unable to connect to '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);
    flags := INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION;
    if uri.Protocol='https' then
      flags := flags or INTERNET_FLAG_SECURE;
    doc := uri.Path+uri.document;
    if uri.params<>'' then
      doc:= doc+'?'+uri.Params;
    hFile := HttpOpenRequest(hConnect, 'GET', PAnsiChar(doc), HTTP_VERSION, nil, nil,flags , 0);
    if not Assigned(hFile) then
      Raise EHTTPException.Create('Unable to get doc '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);

    if not HttpSendRequest(hFile, nil, 0, nil, 0) then
    begin
      ErrorCode:=GetLastError;
      if (ErrorCode = ERROR_INTERNET_INVALID_CA) then
      begin
        ignoreCerticateErrors(hFile, error);
        if not HttpSendRequest(hFile, nil, 0, nil, 0) then
          Raise EHTTPException.Create('Unable to send request to '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);
      end;
    end;

    if Assigned(hFile) then
    try
      dwIndex  := 0;
      dwCodeLen := 10;
      if HttpQueryInfo(hFile, HTTP_QUERY_STATUS_CODE, @dwcode, dwcodeLen, dwIndex) then
      begin
        res := pansichar(@dwcode);
        dwNumber := sizeof(Buffer)-1;
        if (res ='200') or (res ='302') then
        begin
          Result:='';
          pos:=1;
          repeat
            FillChar(buffer,SizeOf(buffer),0);
            InternetReadFile(hFile,@buffer,SizeOf(buffer),bytesRead);
            SetLength(Result,Length(result)+bytesRead);
            Move(Buffer,Result[pos],bytesRead);
            inc(pos,bytesRead);
          until bytesRead = 0;
        end
        else
          if res='401' then
            raise EHTTPException.Create('Not authorized: '+URL+' HTTP Status: '+res,int(res^))
          else
            raise EHTTPException.Create('Unable to download: '+URL+' HTTP Status: '+res,int(res^));
      end
      else
         raise EHTTPException.Create('Unable to download: '+URL+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);
    finally
      if Assigned(hFile) then
        InternetCloseHandle(hFile);
    end
    else
       raise EHTTPException.Create('Unable to download: "'+URL+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);

  finally
    if Assigned(hConnect) then
      InternetCloseHandle(hConnect);
    if Assigned(hInet) then
      InternetCloseHandle(hInet);
  end;
end;

function GetWinInetError(ErrorCode:Cardinal): ansistring;
const
   winetdll = 'wininet.dll';
var
  Len: Integer;
  Buffer: PAnsiChar;
begin
  Len := FormatMessage(
  FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or
  FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or  FORMAT_MESSAGE_ARGUMENT_ARRAY,
  Pointer(GetModuleHandle(winetdll)), ErrorCode, 0, @Buffer, SizeOf(Buffer), nil);
  try
    while (Len > 0) and {$IFDEF UNICODE}(CharInSet(Buffer[Len - 1], [#0..#32, '.'])) {$ELSE}(Buffer[Len - 1] in [#0..#32, '.']) {$ENDIF} do Dec(Len);
    SetString(Result, Buffer, Len);
  finally
    LocalFree(HLOCAL(Buffer));
  end;
end;

function ignoreCerticateErrors(oRequestHandle:HINTERNET; var aErrorMsg: ansistring): Boolean;
var
  vDWFlags: DWord;
  vDWFlagsLen: DWord;
begin
  Result := False;
  try
    vDWFlagsLen := SizeOf(vDWFlags);
    if not InternetQueryOption(oRequestHandle, INTERNET_OPTION_SECURITY_FLAGS, @vDWFlags, vDWFlagsLen) then begin
      aErrorMsg := 'Internal error in SetToIgnoreCerticateErrors when trying to get wininet flags.' + GetWininetError(GetLastError);
      Exit;
    end;
    vDWFlags := vDWFlags or SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or SECURITY_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_REVOCATION;
    if not InternetSetOption(oRequestHandle, INTERNET_OPTION_SECURITY_FLAGS, @vDWFlags, vDWFlagsLen) then begin
      aErrorMsg := 'Internal error in SetToIgnoreCerticateErrors when trying to set wininet INTERNET_OPTION_SECURITY_FLAGS flag .' + GetWininetError(GetLastError);
      Exit;
    end;
    Result := True;
  except
    on E: Exception do begin
      aErrorMsg := 'Unknown error in SetToIgnoreCerticateErrors.' + E.Message;
    end;
  end;
end;

function httpPostData(const UserAgent: Ansistring; const url: Ansistring; const Data: RawByteString; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000;user:AnsiString='';password:AnsiString=''):RawByteString;
var
  hInet: HINTERNET;
  hHTTP: HINTERNET;
  hReq: HINTERNET;
  uri:TURI;
  pdata,
  ErrorMsg:AnsiString;

  buffer: array[1..1024] of byte;
  flags,bytesRead,dwError : DWORD;
  pos:integer;
  dwindex,dwcodelen,dwread,dwNumber: cardinal;
  dwcode : array[1..20] of Ansichar;
  res    : PAnsiChar;

  timeout:integer;
  puser,ppassword:PAnsiChar;

const
  wall : AnsiString = '*/*';
  accept: packed array[0..1] of LPSTR = (@wall, nil);
  header: AnsiString = 'Content-Type: application/json';

begin
  uri := ParseURI(url,'http',80);
  if (uri.Protocol = 'https') and (uri.port=80) then
    uri.port := 443;
  if enableProxy then
     hInet := InternetOpen(PAnsiChar(UserAgent),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0)
  else
     hInet := InternetOpen(PAnsiChar(UserAgent),INTERNET_OPEN_TYPE_DIRECT,nil,nil,0);
  try
    InternetSetOption(hInet,INTERNET_OPTION_CONNECT_TIMEOUT,@ConnectTimeout,sizeof(integer));
    InternetSetOption(hInet,INTERNET_OPTION_SEND_TIMEOUT,@SendTimeOut,sizeof(integer));
    InternetSetOption(hInet,INTERNET_OPTION_RECEIVE_TIMEOUT,@ReceiveTimeOut,sizeof(integer));

    if uri.Username<>'' then
      puser:= PAnsiChar(uri.Username)
    else
       puser := Nil;
    if uri.Password<>'' then
      ppassword:=PAnsiChar(uri.Password)
    else
      ppassword:=Nil;

    if user<>'' then
      puser:=PAnsiChar(user);
    if password<>'' then
      ppassword:=PAnsiChar(password);

    hHTTP := InternetConnect(hInet, PAnsiChar(uri.Host), uri.Port, puser,ppassword, INTERNET_SERVICE_HTTP, 0, 0);
    if hHTTP =Nil then
        Raise EHTTPException.Create('Unable to connect to '+url+' code: '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')',0);
    try
      flags := INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD or INTERNET_FLAG_KEEP_CONNECTION;
      if uri.Protocol='https' then
        flags := flags or INTERNET_FLAG_SECURE;

      hReq := HttpOpenRequestA(hHTTP, PAnsiChar('POST'), PAnsiChar(uri.Document), nil, nil, @accept, flags, 1);
      if hReq=Nil then
          Raise EHTTPException.Create('Unable to POST to: '+url+' code: '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')',0);
      try
        pdata := Data;
        if not HttpSendRequestA(hReq, PAnsiChar(header), length(header), PAnsiChar(pdata), length(pdata)) then
        begin
          ErrorCode:=GetLastError;
          if (ErrorCode = ERROR_INTERNET_INVALID_CA) then
          begin
            ignoreCerticateErrors(hReq, ErrorMsg);
            if not HttpSendRequestA(hReq, PAnsiChar(header), length(header), PAnsiChar(pdata), length(pdata))  then
              Raise EHTTPException.Create('Unable to send request to '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')',0);
          end;
        end;
        dwIndex  := 0;
        dwCodeLen := 10;
        if HttpQueryInfo(hReq, HTTP_QUERY_STATUS_CODE, @dwcode, dwcodeLen, dwIndex) then
        begin
          res := pAnsichar(@dwcode);
          dwNumber := sizeof(Buffer)-1;
          if (res ='200') or (res ='302') then
          begin
            Result:='';
            pos:=1;
            repeat
              FillChar(buffer,SizeOf(buffer),0);
              InternetReadFile(hReq,@buffer,SizeOf(buffer),bytesRead);
              SetLength(Result,Length(result)+bytesRead);
              Move(Buffer,Result[pos],bytesRead);
              inc(pos,bytesRead);
            until bytesRead = 0;
          end
          else
             raise EHTTPException.Create('Unable to get return data for: '+URL+' HTTP Status: '+res,int(res^));
        end
        else
            Raise EHTTPException.Create('Unable to get http status for: '+url+' code: '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')',0);

      finally
        InternetCloseHandle(hReq);
      end;
    finally
      InternetCloseHandle(hHTTP);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
end;

{ HTTPException }

constructor EHTTPException.Create(const msg: string; AHTTPStatus: Integer);
begin
  inherited Create(msg);
  HTTPStatus:=AHTTPStatus;
end;

{$endif} // windows

end.

