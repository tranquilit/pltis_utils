unit tishttp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,jwawintype, wininet, Dialogs;

function httpGetString(url: string; enableProxy:Boolean= False;
    ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000):Utf8String;
function httpPostData(const UserAgent: string; const url: string; const Data: AnsiString; httpMethod:String='POST'; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000):Utf8String;
function SetToIgnoreCerticateErrors(oRequestHandle:HINTERNET; var aErrorMsg: string): Boolean;
function GetWinInetError(ErrorCode:Cardinal): string;


implementation
uses FileUtil,JwaWinBase,jwalmaccess,jwalmcons,jwalmerr,JwaWinNT,jwawinuser,URIParser;


// récupère une chaine de caractères en http en utilisant l'API windows
function httpGetString(url: string; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000):Utf8String;
var
  hInet,hFile,hConnect: HINTERNET;
  buffer: array[1..1024] of byte;
  flags,bytesRead,dwError,port : DWORD;
  pos:integer;
  dwindex,dwcodelen,dwread,dwNumber: cardinal;
  dwcode : array[1..20] of char;
  res    : pchar;
  doc,error: String;
  uri :TURI;

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
    BEGIN

      hConnect := InternetConnect(hInet, PChar(uri.Host), port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
      if not Assigned(hConnect) then
        Raise Exception.Create('Unable to connect to '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')');
      flags := INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD;
      if uri.Protocol='https' then
        flags := flags or INTERNET_FLAG_SECURE;
      doc := uri.Path+uri.document;
      if uri.params<>'' then
        doc:= doc+'?'+uri.Params;
      hFile := HttpOpenRequest(hConnect, 'GET', PChar(doc), HTTP_VERSION, nil, nil,flags , 0);
      if not Assigned(hFile) then
        Raise Exception.Create('Unable to get doc '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')');

      if not HttpSendRequest(hFile, nil, 0, nil, 0) then
      begin
        ErrorCode:=GetLastError;
        if (ErrorCode = ERROR_INTERNET_INVALID_CA) then
        begin
          SetToIgnoreCerticateErrors(hFile, url);
          if not HttpSendRequest(hFile, nil, 0, nil, 0) then
            Raise Exception.Create('Unable to send request to '+url+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')');
        end;
      end;
    end;

    if Assigned(hFile) then
    try
      dwIndex  := 0;
      dwCodeLen := 10;
      if HttpQueryInfo(hFile, HTTP_QUERY_STATUS_CODE, @dwcode, dwcodeLen, dwIndex) then
      begin
        res := pchar(@dwcode);
        dwNumber := sizeof(Buffer)-1;
        if (res ='200') or (res ='302') then
        begin
          Result:='';
          pos:=1;
          repeat
            FillChar(buffer,SizeOf(buffer),0);
            InternetReadFile(hFile,@buffer,SizeOf(buffer),bytesRead);
            SetLength(Result,Length(result)+bytesRead+1);
            Move(Buffer,Result[pos],bytesRead);
            inc(pos,bytesRead);
          until bytesRead = 0;
        end
        else
           raise Exception.Create('Unable to download: '+URL+' HTTP Status:'+res+#13#10+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')');
      end
      else
         raise Exception.Create('Unable to download: '+URL+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')');
    finally
      if Assigned(hFile) then
        InternetCloseHandle(hFile);
    end
    else
       raise Exception.Create('Unable to download: "'+URL+' code : '+IntToStr(GetLastError)+' ('+GetWinInetError(GetlastError)+')');

  finally
    if Assigned(hConnect) then
      InternetCloseHandle(hConnect);
    if Assigned(hInet) then
      InternetCloseHandle(hInet);
  end;
end;

function GetWinInetError(ErrorCode:Cardinal): string;
const
   winetdll = 'wininet.dll';
var
  Len: Integer;
  Buffer: PChar;
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

function SetToIgnoreCerticateErrors(oRequestHandle:HINTERNET; var aErrorMsg: string): Boolean;
var
  vDWFlags: DWord;
  vDWFlagsLen: DWord;
begin
  Result := False;
  try
    vDWFlagsLen := SizeOf(vDWFlags);
    if not InternetQueryOption(oRequestHandle, INTERNET_OPTION_SECURITY_FLAGS, @vDWFlags, vDWFlagsLen) then begin
      ShowMessage(IntToStr(GetLastError()));
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

function httpPostData(const UserAgent: string; const url: string; const Data: AnsiString; httpMethod:String='POST'; enableProxy:Boolean= False;
   ConnectTimeout:integer=4000;SendTimeOut:integer=60000;ReceiveTimeOut:integer=60000):Utf8String;
var
  hInet: HINTERNET;
  hHTTP: HINTERNET;
  hReq: HINTERNET;
  uri:TURI;
  pdata:String;

  buffer: array[1..1024] of byte;
  flags,bytesRead,dwError,port : DWORD;
  pos:integer;
  dwindex,dwcodelen,dwread,dwNumber: cardinal;
  dwcode : array[1..20] of char;
  res    : pchar;

  timeout:integer;
//  doc,error: String;
//  uri :TIdURI;


const
  wall : WideString = '*/*';
  accept: packed array[0..1] of LPWSTR = (@wall, nil);
  header: string = 'Content-Type: application/json';
begin
  uri := ParseURI(url);
  try
    if enableProxy then
       hInet := InternetOpen(PChar(UserAgent),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0)
    else
       hInet := InternetOpen(PChar(UserAgent),INTERNET_OPEN_TYPE_DIRECT,nil,nil,0);
    try
      InternetSetOption(hInet,INTERNET_OPTION_CONNECT_TIMEOUT,@ConnectTimeout,sizeof(integer));
      InternetSetOption(hInet,INTERNET_OPTION_SEND_TIMEOUT,@SendTimeOut,sizeof(integer));
      InternetSetOption(hInet,INTERNET_OPTION_RECEIVE_TIMEOUT,@ReceiveTimeOut,sizeof(integer));

      ShowMessage(uri.host);
      hHTTP := InternetConnect(hInet, PChar(uri.Host), uri.Port, PCHAR(uri.Username),PCHAR(uri.Password), INTERNET_SERVICE_HTTP, 0, 0);
      if hHTTP =Nil then
          Raise Exception.Create('Unable to connect to '+url+' code : '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')');
      try
        hReq := HttpOpenRequest(hHTTP, PChar(httpMethod), pchar(url), nil, nil, @accept, 0, 0);
        if hReq=Nil then
            Raise Exception.Create('Unable to '+httpMethod+' to '+url+' code : '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')');
        try
          pdata := Data+#0;
          if not HttpSendRequest(hReq, PChar(header), length(header), PChar(pdata), length(pdata)) then
             Raise Exception.Create('Unable to send data to '+url+' code : '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')');

          dwIndex  := 0;
          dwCodeLen := 10;
          if HttpQueryInfo(hReq, HTTP_QUERY_STATUS_CODE, @dwcode, dwcodeLen, dwIndex) then
          begin
            res := pchar(@dwcode);
            dwNumber := sizeof(Buffer)-1;
            if (res ='200') or (res ='302') then
            begin
              Result:='';
              pos:=1;
              repeat
                FillChar(buffer,SizeOf(buffer),0);
                InternetReadFile(hReq,@buffer,SizeOf(buffer),bytesRead);
                SetLength(Result,Length(result)+bytesRead+1);
                Move(Buffer,Result[pos],bytesRead);
                inc(pos,bytesRead);
              until bytesRead = 0;
            end
            else
               raise Exception.Create('Unable to get return data for '+URL+#13#10+'HTTP Status:'+res+#13#10+' code : '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')');
          end
          else
              Raise Exception.Create('Unable to get http status for '+url+' code : '+IntToStr(GetLastError)+' ('+UTF8Encode(GetWinInetError(GetlastError))+')');

        finally
          InternetCloseHandle(hReq);
        end;
      finally
        InternetCloseHandle(hHTTP);
      end;
    finally
      InternetCloseHandle(hInet);
    end;
  finally
  end;
end;


end.

