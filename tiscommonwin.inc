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

function IsAdminLoggedOn: Boolean;
{ Returns True if the logged-on user is a member of the Administrators local
  group. Always returns True on Windows 9x/Me. }
const
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
begin
  Result := UserInGroup(DOMAIN_ALIAS_RID_ADMINS);
end;

function UserInGroupWindows(Group :DWORD) : Boolean;
var
  pIdentifierAuthority :TSidIdentifierAuthority;
  apSid : PSID;
  IsMember    : BOOL;
begin
  pIdentifierAuthority := SECURITY_NT_AUTHORITY;
  Result := AllocateAndInitializeSid(@pIdentifierAuthority,2, SECURITY_BUILTIN_DOMAIN_RID, Group, 0, 0, 0, 0, 0, 0, apSid);
  try
    if Result then
      if not CheckTokenMembership(0, apSid, IsMember) then //passing 0 means which the function will be use the token of the calling thread.
         Result:= False
      else
         Result:=IsMember;
  finally
     FreeSid(apSid);
  end;
end;

function UserInGroup(Group: DWORD): Boolean;
begin
  Result := UserInGroupWindows(group);
end;

type
  WinIsWow64 = function( Handle: THandle; var Iret: BOOL ): BOOL; stdcall;
//from http://stackoverflow.com/questions/1436185/how-can-i-tell-if-im-running-on-x64

function IsWin64: Boolean;
var
  HandleTo64BitsProcess: WinIsWow64;
  Iret                 : BOOL;
begin
  Result := False;
  HandleTo64BitsProcess := WinIsWow64(pointer( GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process')));
  if Assigned(HandleTo64BitsProcess) then
  begin
    if not HandleTo64BitsProcess(GetCurrentProcess, Iret) then
    Raise Exception.Create('Invalid handle');
    Result := Iret;
  end;
end;


function GetAccountSid(const Server, User: WideString; var Sid: PSID): DWORD;
var
  dwDomainSize, dwSidSize: DWord;
  R                 : LongBool;
  wDomain           : WideString;
  Use               : DWord;
begin
  Result := 0;
  SetLastError(0);
  dwSidSize := 0;
  dwDomainSize := 0;
  R := LookupAccountNameW(PWideChar(Server), PWideChar(User), nil, dwSidSize,
    nil, dwDomainSize, Use);
  if (not R) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    SetLength(wDomain, dwDomainSize);
    Sid := GetMemory(dwSidSize);
    R := LookupAccountNameW(PWideChar(Server), PWideChar(User), Sid,
      dwSidSize, PWideChar(wDomain), dwDomainSize, Use);
    if not R then
    begin
      FreeMemory(Sid);
      Sid := nil;
    end;
  end
  else
    Result := GetLastError;
end;


function GetAccountSidString(const Server, User: WideString):String;
var
  ASID:PSid;
begin
  ASID:=Nil;
  try
    if GetAccountSid(Server,User,ASID)=0 then
      Result := SIDToStringSID(ASID)
    else
      Result := '';
  finally
     if ASID<>Nil then
      Freememory(ASID);
  end;
end;

(*
 * Procedure  : StrSIDToName
 * Author     : MPu
 *)
function StrSIDToName(const StrSID: AnsiString; var Name: Ansistring; var SIDType: DWORD): Boolean;
var
  SID               : PSID;
  Buffer            : PAnsiChar;
  NameLen, TempLen  : Cardinal;
  err               : Boolean;
begin
  SID := nil;
  err := ConvertStringSIDToSID(PChar(StrSID), SID);
  if err then
  begin
    NameLen := 0;
    TempLen := 0;
    LookupAccountSidA(nil, SID, nil, NameLen, nil, TempLen, SIDType);
    GetMem(Buffer, NameLen);
    try
      err := LookupAccountSidA(nil, SID, Buffer, NameLen, nil, TempLen, SIDType);
      if err then
        SetString(Name, Buffer, Namelen);
    finally
      FreeMem(Buffer);
    end;
  end;
  if Assigned(SID) then
    LocalFree(Cardinal(SID));
  result := err;
end;

function SIDToStringSID(const aSID:PSID): String;
var
  Buffer            : PAnsiChar;
  err               : Boolean;
begin
  err := ConvertSidToStringSid(aSID,Buffer);
  if err then
  begin
    Result := String(Buffer);
    if Assigned(Buffer) then
      LocalFree(HLocal(Buffer));
  end
  else
    Result :='';
end;



(*
 * Procedure  : AddToGroup
 * Author     : MPu
 * Adds a domain account to a local group
 *)
function AddToGroup(const member, Group: WideString): NET_API_STATUS;
var
  memberInfo             : LOCALGROUP_MEMBERS_INFO_3;
  NetError          : DWORD;

begin
  NetError := 0;
  if (member <> '') and (Group <> '')  then
  begin
    memberInfo.lgrmi3_domainandname:=PWideChar(member);
    NetError := NetLocalGroupAddMembers(Nil, PWideChar(Group), 3, @memberInfo, 1);
    if NetError = 1378 then //Le nom de compte spécifié est déjà membre du groupe"
      NetError := 0;
  end;
  result := NetError;
end;

(*
 * Procedure  : UserModalsGet
 * Author     : MPu
 * Password stuff
 *)
function UserModalsGet(const Server: String): USER_MODALS_INFO_0;
var
  UserModalsInfo    : PUSER_MODALS_INFO_0;
  dwRet             : DWORD;
begin
  UserModalsInfo := nil;
  dwRet := NetUserModalsGet(nil, 0, Pointer(UserModalsInfo));
  if ((dwRet = NERR_Success) and Assigned(UserModalsInfo)) then
  begin
    result.usrmod0_min_passwd_len := UserModalsInfo^.usrmod0_min_passwd_len;
    result.usrmod0_max_passwd_age := UserModalsInfo^.usrmod0_max_passwd_age;
    result.usrmod0_min_passwd_age := UserModalsInfo^.usrmod0_min_passwd_age;
    result.usrmod0_force_logoff := UserModalsInfo^.usrmod0_force_logoff;
    result.usrmod0_password_hist_len := UserModalsInfo^.usrmod0_password_hist_len;
    NetApiBufferFree(UserModalsInfo);
  end;
end;

function DomainGet: String;
var
  UserModalsInfo    : PUSER_MODALS_INFO_2;
  dwRet             : DWORD;
begin
  UserModalsInfo := nil;
  dwRet := NetUserModalsGet(nil, 2, Pointer(UserModalsInfo));
  if ((dwRet = NERR_Success) and Assigned(UserModalsInfo)) then
  begin
    Result := UserModalsInfo^.usrmod2_domain_name;
    //result.usrmod2_domain_id := UserModalsInfo^.usrmod2_domain_id;
    NetApiBufferFree(UserModalsInfo);
  end;
end;


function DomainSID: String;
var
  UserModalsInfo    : PUSER_MODALS_INFO_2;
  dwRet             : DWORD;
  SID               : PSID;
begin
  UserModalsInfo := nil;
  dwRet := NetUserModalsGet(nil, 2, Pointer(UserModalsInfo));
  if ((dwRet = NERR_Success) and Assigned(UserModalsInfo)) then
  begin
    Result := SIDToStringSID(UserModalsInfo^.usrmod2_domain_id);
    //result.usrmod2_domain_id := UserModalsInfo^.usrmod2_domain_id;
    NetApiBufferFree(UserModalsInfo);
  end;
end;


function GetJoinInformation:String;
var
  name: LPWSTR;
  status: NETSETUP_JOIN_STATUS;
begin
  Result := '';
  Name := Nil;
  if NetGetJoinInformation(Nil,name,@status)=NERR_Success then
  begin
    if status=NetSetupDomainName then
      Result := name;
    NetApiBufferFree(name);
  end;
end;

function EnablePrivilege(const Privilege: string; fEnable: Boolean; out PreviousState: Boolean): DWORD;
var
  Token             : THandle;
  NewState          : TTokenPrivileges;
  Luid              : _LUID;
  PrevState         : TTokenPrivileges;
  Return            : DWORD;
begin
  PreviousState := True;
  if (GetVersion() > $80000000) then
    // Win9x
    Result := ERROR_SUCCESS
  else
  begin
    // WinNT
    if not OpenProcessToken(GetCurrentProcess(), MAXIMUM_ALLOWED, Token) then
      Result := GetLastError()
    else
    try
      if not LookupPrivilegeValue(nil, PChar(Privilege), Luid) then
        Result := GetLastError()
      else
      begin
        NewState.PrivilegeCount := 1;
        NewState.Privileges[0].Luid := Luid;
        if fEnable then
          NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
        else
          NewState.Privileges[0].Attributes := 0;
        if not AdjustTokenPrivileges(Token, False, @NewState,
          SizeOf(TTokenPrivileges), @PrevState, @Return) then
          Result := GetLastError()
        else
        begin
          Result := ERROR_SUCCESS;
          PreviousState :=
            (PrevState.Privileges[0].Attributes and SE_PRIVILEGE_ENABLED <> 0);
        end;
      end;
    finally
      CloseHandle(Token);
    end;
  end;
end;


function GetAdminSid: PSID;
// Author       : Nico Bendlin
const
  // bekannte SIDs ... (WinNT.h)
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  // bekannte RIDs ... (WinNT.h)
  SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
  DOMAIN_ALIAS_RID_ADMINS: DWORD = $00000220;
begin
  Result := nil;
  AllocateAndInitializeSid(@SECURITY_NT_AUTHORITY, 2,
    SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
    0, 0, 0, 0, 0, 0, Result);
end;

function IsAdmin: LongBool;
// Author       : Nico Bendlin
var
  TokenHandle       : THandle;
  ReturnLength      : DWORD;
  TokenInformation  : PTokenGroups;
  AdminSid          : PSID;
  Loop              : Integer;
begin
  Result := False;
  TokenHandle := 0;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  try
    ReturnLength := 0;
    GetTokenInformation(TokenHandle, TokenGroups, nil, 0, ReturnLength);
    TokenInformation := GetMemory(ReturnLength);
    if Assigned(TokenInformation) then
    try
      if GetTokenInformation(TokenHandle, TokenGroups, TokenInformation,
        ReturnLength, ReturnLength) then
      begin
        AdminSid := GetAdminSid;
        for Loop := 0 to TokenInformation^.GroupCount - 1 do
        begin
          if EqualSid(TokenInformation^.Groups[Loop].Sid, AdminSid) then
          begin
            Result := True;
            Break;
          end;
        end;
        FreeSid(AdminSid);
      end;
    finally
      FreeMemory(TokenInformation);
    end;
  finally
    CloseHandle(TokenHandle);
  end;
end;


procedure AddToUserPath(APath:Utf8String);
var
  SystemPath : Utf8String;
begin
  with TRegistry.Create do
  try
    //RootKey:=HKEY_LOCAL_MACHINE;
    OpenKey('Environment',False);
    SystemPath:=ReadString('PATH');
    if pos(LowerCase(APath),LowerCase(SystemPath))=0 then
    begin
      if RightStr(SystemPath,1)<>';' then SystemPath:=SystemPath+';';
      SystemPath:=SystemPath+APath;
      if RightStr(SystemPath,1)<>';' then SystemPath:=SystemPath+';';
      WriteString('PATH',SystemPath);
    end;
  finally
    Free;
  end;
end;


procedure UpdateApplication(fromURL:Utf8String;SetupExename,SetupParams,ExeName,RestartParam:Utf8String);
var
  bat: TextFile;
  tempdir,tempfn,updateBatch,zipfn,version : Utf8String;
  files:TStringList;
  UnZipper: TUnZipper;
  i,hinstance:integer;
begin
  Files := TStringList.Create;
  try
    Logger('Updating application...');
    tempdir := GetUniqueTempdir('tis');
    if ExeName='' then
      ExeName :=ExtractFileName(ParamStr(0));

    tempfn := AppendPathDelim(tempdir)+SetupExename;
    mkdir(tempdir);
    Logger('Getting new file from: '+fromURL+' into '+tempfn);
    try
      wget(fromURL,tempfn,Nil,Nil,True);
      version := GetApplicationVersion(tempfn);
      if version='' then
        raise Exception.create('no version information in downloaded file.');
      Logger(' got '+SetupExename+' version: '+version);
      Files.Add(SetupExename);
    except
      //trying to get a zip file instead (exe files blocked by proxy ...)
      zipfn:= AppendPathDelim(tempdir)+ChangeFileExt(SetupExename,ansistring('.zip'));
      wget(ChangeFileExt(fromURL,ansistring('.zip')),zipfn,Nil,Nil,True);
      Logger('  unzipping file '+zipfn);
      UnZipper := TUnZipper.Create;
      try
        UnZipper.FileName := zipfn;
        UnZipper.OutputPath := tempdir;
        UnZipper.Examine;
        UnZipper.UnZipAllFiles;
        for i := 0 to UnZipper.Entries.count-1 do
          if not UnZipper.Entries[i].IsDirectory then
            Files.Add(StringReplace(UnZipper.Entries[i].DiskFileName,'/','\',[rfReplaceAll]));
      finally
        UnZipper.Free;
      end;

      version := GetApplicationVersion(tempfn);
      if version='' then
        raise Exception.create('no version information in downloaded exe file.');
      Logger(' got '+SetupExename+' version: '+version);
    end;

    if FileExistsUtf8(tempfn) and (FileSizeUtf8(tempfn)>0) then
    begin
      // small batch to replace current running application
      updatebatch := AppendPathDelim(tempdir) + 'update.bat';
      AssignFile(bat,updateBatch);
      Rewrite(bat);
      try
        Logger(' Creating update batch file '+updateBatch);
        // wait for program to terminate..
        Writeln(bat,'timeout /T 2');
        Writeln(bat,'taskkill /im '+Exename+' /f');
        Writeln(bat,'"'+IncludeTrailingPathDelimiter(tempdir)+SetupExename+'" '+SetupParams);
        Writeln(bat,'cd ..');
        if RestartParam<>'' then
          Writeln(bat,'start "" "'+ParamStr(0)+'" '+restartparam);
        Writeln(bat,'rmdir /s /q "'+tempdir+'"');
      finally
        CloseFile(bat)
      end;
      Logger(' Launching update batch file '+updateBatch);
      hinstance := ShellExecute(
        0,
        PAnsiChar('open'),
        PAnsiChar( SysUtils.GetEnvironmentVariable(AnsiString('ComSpec'))),
        PAnsiChar(AnsiString('/C '+ updatebatch)),
        PAnsiChar(TempDir),
        SW_HIDE);
      if hinstance<=32 then
      begin
        writeln('Error launching update batch file :'+IntToStr(hinstance));
        ExitProcess(1);
      end
      else
        ExitProcess(0)
    end;

  finally
    Files.Free;
  end;

end;

function GetSpecialFolderLocation(csidl: Integer; ForceFolder: Boolean = False ): Utf8String;
Var
  APath : Array[0..MAX_PATH] of WideChar;
  WS: UnicodeString;
  Len: SizeInt;
  hr: HRESULT;
begin
  Result := '';
  If (@SHGetFolderPathW <> Nil) then
  begin
    FillChar(APath{%H-}, SizeOf(APath), #0);
    hr := SHGetFolderPathW(0,csidl or CSIDL_FLAG_CREATE,0,0, @APATH[0]);
    if hr = S_OK then
    begin
      Len := StrLen(APath);
      SetLength(WS, Len);
      System.Move(APath[0], WS[1], Len * SizeOf(WideChar));
      Result := AppendPathDelim(Utf8Decode(APath));
    end;
  end;
end;

function GetSendToFolder: Utf8String;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;
  if Registry.OpenKeyReadOnly( strnShellFolders ) then
    Result := AppendPathDelim(Registry.ReadString( 'SendTo' ))
  else
    Result := '';
  Registry.Free;
end;

function GetUserNameEx(NameFormat: DWORD; lpNameBuffer: LPSTR; var nSize: DWORD): Boolean; stdcall; external
  'secur32.dll' Name 'GetUserNameExA';

function GetCurrentUserName(fFormat: DWORD=NameSamCompatible) : Ansistring;
const
  cnMaxUserNameLen = 254;
var
  sUserName     : Ansistring;
  dwUserNameLen : DWord;
begin
  dwUserNameLen := cnMaxUserNameLen-1;
  SetLength( sUserName, cnMaxUserNameLen );
  GetUserNameEx(fFormat,
    PChar( sUserName ),
    dwUserNameLen );
  SetLength( sUserName, dwUserNameLen );
  Result := sUserName;
end;

function GetPersonalFolder:Utf8String;
begin
  result := GetSpecialFolderLocation(CSIDL_PERSONAL)
end;

function GetLocalAppdataFolder:Utf8String;
begin
  result :=  GetSpecialFolderLocation(CSIDL_LOCAL_APPDATA);
end;


function GetAppdataFolder:Utf8String;
begin
  result :=  GetSpecialFolderLocation(CSIDL_APPDATA);
end;

function GetAppUserFolder : Utf8String;
begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolderLocation(CSIDL_APPDATA)) + ApplicationName;
end;

function GetStartMenuFolder: Utf8String;
begin
  result := GetSpecialFolderLocation(CSIDL_STARTMENU);
end;

function GetCommonStartMenuFolder: Utf8String;
begin
  result := GetSpecialFolderLocation(CSIDL_COMMON_STARTMENU);
end;

function GetStartupFolder: Utf8String;
begin
  result := GetSpecialFolderLocation(CSIDL_STARTUP);
end;

function GetCommonStartupFolder: Utf8String;
begin
  result := GetSpecialFolderLocation(CSIDL_COMMON_STARTUP);
end;

function GetCurrentUser: Utf8String;
var
  charBuffer: array[0..128] of WideChar;
  intgBufferSize: DWORD;
begin
  intgBufferSize := 128;
  if GetUserNameW( charBuffer, intgBufferSize ) then
  begin
    Result := StrPas( charBuffer );
  end
  else
  begin
    Result := '';
  end;
end;

// to store use specific settings for this application
function Appuserinipath:Utf8String;
var
  dir : Utf8String;
begin
  dir := IncludeTrailingPathDelimiter(GetAppdataFolder)+ApplicationName;
  if not DirectoryExistsUTF8(dir) then
    MkDir(dir);
  Result:=IncludeTrailingPathDelimiter(dir)+ApplicationName+'.ini';
end;


function GetOSVersionInfo: TOSVersionInfoEx;
begin
  FillChar(Result, SizeOf(TOSVersionInfoEx), 0);
  Result.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  if not GetVersionEx(POSVersionInfo(@Result)) then
    Result.dwOSVersionInfoSize := 0;
end;

function IsWinXP:Boolean;
var
  Info : TOSVersionInfoEx;
begin
  Result := False;
  info := GetOSVersionInfo;
  if info.dwOSVersionInfoSize>0 then
    result := (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
              (Info.dwMajorVersion = 5) and (Info.dwMinorVersion = 1)
end;


 type
    PFixedFileInfo = ^TFixedFileInfo;
    TFixedFileInfo = record
         dwSignature       : DWORD;
         dwStrucVersion    : DWORD;
         wFileVersionMS    : WORD;  // Minor Version
         wFileVersionLS    : WORD;  // Major Version
         wProductVersionMS : WORD;  // Build Number
         wProductVersionLS : WORD;  // Release Version
         dwFileFlagsMask   : DWORD;
         dwFileFlags       : DWORD;
         dwFileOS          : DWORD;
         dwFileType        : DWORD;
         dwFileSubtype     : DWORD;
         dwFileDateMS      : DWORD;
         dwFileDateLS      : DWORD;
    end; // TFixedFileInfo


function GetApplicationVersion(FileName: Utf8String): Utf8String;
var
    dwHandle, dwVersionSize : DWORD;
    strSubBlock             : String;
    pTemp                   : Pointer;
    pData                   : Pointer;
begin
  Result:='';
    if Filename='' then
    FileName:=ParamStr(0);
     strSubBlock := '\';

     // get version information values
     dwVersionSize := GetFileVersionInfoSizeW( PWideChar( UTF8Decode(FileName) ), // pointer to filename string
                                                                                        dwHandle );        // pointer to variable to receive zero

     // if GetFileVersionInfoSize is successful
     if dwVersionSize <> 0 then
     begin
            GetMem( pTemp, dwVersionSize );
            try
                 if GetFileVersionInfo( PAnsiChar( FileName ),             // pointer to filename string
                                                                dwHandle,                      // ignored
                                                                dwVersionSize,                 // size of buffer
                                                                pTemp ) then                   // pointer to buffer to receive file-version info.

                        if VerQueryValue( pTemp,                           // pBlock     - address of buffer for version resource
                                                            PAnsiChar( strSubBlock ),            // lpSubBlock - address of value to retrieve
                                                            pData,                           // lplpBuffer - address of buffer for version pointer
                                                            dwVersionSize ) then             // puLen      - address of version-value length buffer
                             with PFixedFileInfo( pData )^ do
                                Result:=IntToSTr(wFileVersionLS)+'.'+IntToSTr(wFileVersionMS)+
                                            '.'+IntToStr(wProductVersionLS)+'.'+IntToStr(wProductVersionMS);
            finally
                 FreeMem( pTemp );
            end; // try
     end; // if dwVersionSize
end;

function ProcessExists(ExeFileName: string): boolean;
{description checks if the process is running. Adapted for freepascal from:
URL: http://www.swissdelphicenter.ch/torry/showcode.php?id=2554}
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  CurrentPId:LongWord;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  CurrentPId:=longWord(GetCurrentProcessId);
  while integer(ContinueLoop) <> 0 do
  begin
    if (FProcessEntry32.th32ProcessID <> CurrentPId) and  ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;


function KillTask(ExeFileName: string): integer;
const
  PROCESS_TERMINATE=$0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  result := 0;

  FSnapshotHandle := CreateToolhelp32Snapshot
           (TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle,
                 FProcessEntry32);

  while integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
          = UpperCase(ExeFileName)) or
        (UpperCase(FProcessEntry32.szExeFile) =
         UpperCase(ExeFileName))) then

    Result := Integer(TerminateProcess(OpenProcess(
            PROCESS_TERMINATE, BOOL(0),
            FProcessEntry32.th32ProcessID), 0));

    ContinueLoop := Process32Next(FSnapshotHandle,
                 FProcessEntry32);
  end;

  CloseHandle(FSnapshotHandle);
end;


//From Pascal SCADA / sockets_w32_w64  (C) Fabio Luis Girardi
function WaitForConnection(FListenerSocket:TSocket; timeout:LongInt):Boolean;
var
  readsel,wrsel:TFDSet;
  mode:u_long;
  tv : TTimeVal;
  p:ptimeval;
begin

  if timeout=-1 then
    p:=nil
  else begin
    tv.tv_Sec:=Timeout div 1000;
    tv.tv_Usec:=(Timeout mod 1000)*1000;
    p:=@tv;
  end;


  FD_ZERO(readsel);
  FD_SET(FListenerSocket, readsel);
  FD_ZERO(wrsel);
  FD_SET(FListenerSocket, wrsel);

  // XXX should probably be "FListenerSocket + 1" (ie. max_fd + 1)
  mode := select(FListenerSocket, @readsel, @wrsel, nil, p);

  if (mode <= 0) then begin
    Result := false;
  end else
    if (mode > 0) then begin
      Result := true;
    end;
end;

function GetIPFromHostWindows(const HostName: String): String;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  i: Integer;
  GInitData: TWSAData;
begin
  WSAStartup($101, GInitData);
  Result := '';
  phe := gethostbyname(PAnsiChar(HostName));
  if phe = nil then Exit;
  pPtr := PaPInAddr(phe^.h_addr_list);
  i := 0;
  while pPtr^[i] <> nil do
  begin
    Result := inet_ntoa(pptr^[i]^);
    Inc(i);
  end;
  WSACleanup;
end;


function GetIPFromHost(const HostName: ansistring): ansistring;
begin
  Result := GetIPFromHostWindows(Hostname);
end;

// From JCL library
function GetServiceStatusByName(const AServer,AServiceName:ansistring):TServiceState;
var
  ServiceHandle,
  SCMHandle: DWORD;
  SCMAccess,Access:DWORD;
  ServiceStatus: TServiceStatus;
begin
  Result:=ssUnknown;

  SCMAccess:=SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE or SC_MANAGER_QUERY_LOCK_STATUS;
  Access:=SERVICE_INTERROGATE or GENERIC_READ;

  SCMHandle:= OpenSCManager(PAnsiChar(AServer), Nil, SCMAccess);
  if SCMHandle <> 0 then
  try
    ServiceHandle:=OpenService(SCMHandle,PAnsiChar(AServiceName),Access);
    if ServiceHandle <> 0 then
    try
      ResetMemory(ServiceStatus, SizeOf(ServiceStatus));
      if QueryServiceStatus(ServiceHandle,ServiceStatus) then
        Result:=TServiceState(ServiceStatus.dwCurrentState);
    finally
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function StartServiceByName(const AServer,AServiceName: AnsiString):Boolean;
var
  ServiceHandle,
  SCMHandle: DWORD;
  p: PAnsiChar;
  ts:TDateTime;
  SS:SERVICE_STATUS;
begin
  p:=nil;
  Result:=False;

  SCMHandle:= OpenSCManager(PAnsiChar(AServer), nil, SC_MANAGER_ALL_ACCESS);
  if SCMHandle <> 0 then
  try
    ServiceHandle:=OpenService(SCMHandle,PAnsiChar(AServiceName),SERVICE_ALL_ACCESS);
    if ServiceHandle <> 0 then
    begin
      Result:=StartService(ServiceHandle,0,p);
      if Result then
      begin
        ts := Now;
        repeat
          ResetMemory(SS, SizeOf(SS));
          QueryServiceStatus(ServiceHandle,SS);
        until (SS.dwCurrentState = SERVICE_RUNNING) or ((Now-ts)> 5/24/3600);
        Result := SS.dwCurrentState = SERVICE_RUNNING;
      end;
    end;
    CloseServiceHandle(ServiceHandle);
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function StopServiceByName(const AServer, AServiceName: AnsiString):Boolean;
var
  ServiceHandle,
  SCMHandle: DWORD;
  SS: SERVICE_STATUS;
  ts:TDateTime;
begin
  Result := False;

  SCMHandle := OpenSCManager(PAnsiChar(AServer), nil, SC_MANAGER_ALL_ACCESS);
  if SCMHandle <> 0 then
  try
    ServiceHandle := OpenService(SCMHandle, PAnsiChar(AServiceName), SERVICE_ALL_ACCESS);
    if ServiceHandle <> 0 then
    begin
      ResetMemory(SS, SizeOf(SS));
      Result := ControlService(ServiceHandle, SERVICE_CONTROL_STOP, SS);
      if Result then
      begin
        ts := Now;
        repeat
          ResetMemory(SS, SizeOf(SS));
          QueryServiceStatus(ServiceHandle,SS);
        until (SS.dwCurrentState = SERVICE_STOPPED) or ((Now-ts)> 5/24/3600);
        Result := SS.dwCurrentState = SERVICE_STOPPED;
      end;
    end;
    CloseServiceHandle(ServiceHandle);
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function GetGroups(srvName, usrName: WideString):TStringArray;
var
  dwEntriesRead, dwEntriesTotal: DWORD;
  grpi0: Pointer;
  pInfo: PGroupInfo0;
  nErr: Integer;
begin
  SetLength(Result,0);
  nErr := NetUserGetGroups(PWideChar(srvName), PWideChar(usrName), 0, grpi0,MAX_PREFERRED_LENGTH, @dwEntriesRead, @dwEntriesTotal);
  if nErr = NERR_SUCCESS then
  begin
    pInfo := grpi0;
    while dwEntriesRead > 0 do
    begin
      SetLength(result,length(result)+1);
      result[length(result)-1] := pInfo^.grpi0_name;
      Inc(pInfo);
      Dec(dwEntriesRead);
    end;
    NetAPIBufferFree(grpi0);
  end;
end;

function GetLocalGroups:TStringArray;
var
  dwEntriesRead, dwEntriesTotal,resumehandle: DWORD;
  grpi0: Pointer;
  pInfo: PGroupInfo0;
  nErr: Integer;
begin
  SetLength(Result,0);
  resumehandle := 0;
  nErr := NetLocalGroupEnum (Nil, 0, grpi0,MAX_PREFERRED_LENGTH, @dwEntriesRead, @dwEntriesTotal,Nil);
  if nErr = NERR_SUCCESS then
  begin
    pInfo := grpi0;
    while dwEntriesRead > 0 do
    begin
      SetLength(result,length(result)+1);
      result[length(result)-1] := pInfo^.grpi0_name;
      Inc(pInfo);
      Dec(dwEntriesRead);
    end;
    NetAPIBufferFree(grpi0);
  end;
end;

function GetLocalGroupMembers(GroupName:WideString):TStringArray;
var
  dwEntriesRead, dwEntriesTotal,resumehandle: DWORD;
  grpi0: Pointer;
  pInfo: PLOCALGROUP_MEMBERS_INFO_3;
  nErr: Integer;
begin
  SetLength(Result,0);
  resumehandle := 0;
  nErr := NetLocalGroupGetMembers(Nil,PWideChar(GroupName),3,grpi0,MAX_PREFERRED_LENGTH, @dwEntriesRead, @dwEntriesTotal,Nil);
  if nErr = NERR_SUCCESS then
  begin
    pInfo := grpi0;
    while dwEntriesRead > 0 do
    begin
      SetLength(result,length(result)+1);
      result[length(result)-1] := pInfo^.lgrmi3_domainandname;
      Inc(pInfo);
      Dec(dwEntriesRead);
    end;
    NetAPIBufferFree(grpi0);
  end;
end;

function UserLogin(user,password,domain:String):THandle;
var
  htok:THandle;
begin
 {$H-}
  if not LogonUser(PAnsiChar(user),pAnsichar(domain),pAnsichar(password),LOGON32_LOGON_NETWORK,LOGON32_PROVIDER_DEFAULT,htok) then
    raise EXCEPTION.Create('Unable to login as '+user+' on domain '+domain);
  result := htok;
  {$H+}
end;

function OnSystemAccount(): Boolean;
begin
  Result := GetCurrentUserSid='S-1-5-18';
end;

function UserDomain(htoken:THandle):AnsiString;
var
  cbBuf: Cardinal;
  ptiUser: PTOKEN_USER;
  snu: SID_NAME_USE;
  ProcessHandle: THandle;
  UserSize, DomainSize: DWORD;
  bSuccess: Boolean;
  user,domain:AnsiString;
begin
  Result := '';
  bSuccess := GetTokenInformation(hToken, TokenUser, nil, 0, cbBuf);
  ptiUser  := nil;
  while (not bSuccess) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) do
  begin
    ReallocMem(ptiUser, cbBuf);
    bSuccess := GetTokenInformation(hToken, TokenUser, ptiUser, cbBuf, cbBuf);
  end;

  if not bSuccess then
  begin
    Exit;
  end;

  UserSize := 0;
  DomainSize := 0;
  LookupAccountSid(nil, ptiUser^.User.Sid, nil, UserSize, nil, DomainSize, snu);
  if (UserSize <> 0) and (DomainSize <> 0) then
  begin
    SetLength(User, UserSize);
    SetLength(Domain, DomainSize);
    if LookupAccountSid(nil, ptiUser^.User.Sid, PAnsiChar(User), UserSize,
      PAnsiChar(Domain), DomainSize, snu) then
    begin
      User := StrPas(PAnsiChar(User));
      Domain := StrPas(PAnsiChar(Domain));
      Result := Domain;
    end;
  end;

  if bSuccess then
  begin
    FreeMem(ptiUser);
  end;
end;

// From http://www.swissdelphicenter.ch/torry/showcode.php?id=2095
function ConvertSid(Sid: PSID; pszSidText: PAnsiChar; var dwBufferLen: DWORD): BOOL;
var
  psia: PSIDIdentifierAuthority;
  dwSubAuthorities: DWORD;
  dwSidRev: DWORD;
  dwCounter: DWORD;
  dwSidSize: DWORD;
begin
  Result := False;

  dwSidRev := SID_REVISION;

  if not IsValidSid(Sid) then Exit;

  psia := GetSidIdentifierAuthority(Sid);

  dwSubAuthorities := GetSidSubAuthorityCount(Sid)^;

  dwSidSize := (15 + 12 + (12 * dwSubAuthorities) + 1) * SizeOf(Char);

  if (dwBufferLen < dwSidSize) then
  begin
    dwBufferLen := dwSidSize;
    SetLastError(ERROR_INSUFFICIENT_BUFFER);
    Exit;
  end;

  StrFmt(pszSidText, 'S-%u-', [dwSidRev]);

  if (psia^.Value[0] <> 0) or (psia^.Value[1] <> 0) then
    StrFmt(pszSidText + StrLen(pszSidText),
      '0x%.2x%.2x%.2x%.2x%.2x%.2x',
      [psia^.Value[0], psia^.Value[1], psia^.Value[2],
      psia^.Value[3], psia^.Value[4], psia^.Value[5]])
  else
    StrFmt(pszSidText + StrLen(pszSidText),
      '%u',
      [DWORD(psia^.Value[5]) +
      DWORD(psia^.Value[4] shl 8) +
      DWORD(psia^.Value[3] shl 16) +
      DWORD(psia^.Value[2] shl 24)]);

  dwSidSize := StrLen(pszSidText);

  for dwCounter := 0 to dwSubAuthorities - 1 do
  begin
    StrFmt(pszSidText + dwSidSize, '-%u',
      [GetSidSubAuthority(Sid, dwCounter)^]);
    dwSidSize := StrLen(pszSidText);
  end;

  Result := True;
end;

function ObtainTextSid(hToken: THandle; pszSid: PAnsiChar;
  var dwBufferLen: DWORD): BOOL;
var
  dwReturnLength: DWORD;
  dwTokenUserLength: DWORD;
  tic: TTokenInformationClass;
  ptu: Pointer;
begin
  Result := False;
  dwReturnLength := 0;
  dwTokenUserLength := 0;
  tic := TokenUser;
  ptu := nil;

  if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength,
    dwReturnLength) then
  begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      ptu := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, dwReturnLength);
      if ptu <> nil then
      try
        dwTokenUserLength := dwReturnLength;
        dwReturnLength    := 0;
        if not GetTokenInformation(hToken, tic, ptu, dwTokenUserLength,
          dwReturnLength) then Exit;
        if not ConvertSid((PTokenUser(ptu)^.User).Sid, pszSid, dwBufferLen) then Exit;
        Result := True;
      finally
        if ptu <> Nil then
          HeapFree(GetProcessHeap, 0, ptu);
      end;
    end
    else
      Exit;
  end;

end;

function GetCurrentUserSid: Ansistring;
var
  hAccessToken: THandle;
  bSuccess: BOOL;
  dwBufferLen: DWORD;
  szSid: array[0..260] of AnsiChar;
begin
  Result := '';

  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
    hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
        hAccessToken);
  end;
  if bSuccess then
  begin
    ZeroMemory(@szSid, SizeOf(szSid));
    dwBufferLen := SizeOf(szSid);

    if ObtainTextSid(hAccessToken, szSid, dwBufferLen) then
      Result := szSid;
    CloseHandle(hAccessToken);
  end;
end;

procedure SetUserProfilePath(SID:AnsiString;ImagePath:AnsiString);
var
  r: TRegistry;
begin
  r := TRegistry.Create(KEY_ALL_ACCESS);
  try
    r.RootKey:=HKEY_LOCAL_MACHINE;
    r.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\'+SID,True);
    r.WriteExpandString('ProfileImagePath',ImagePath);
  finally
    r.CloseKey;
    r.Free;
  end;
end;

function GetUserProfilePath(SID:String):String;
var
  r: TRegistry;
begin
  r := TRegistry.Create(KEY_READ);
  try
    r.RootKey:=HKEY_LOCAL_MACHINE;
    r.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\'+SID,False);
    Result := r.ReadString('ProfileImagePath');
  finally
    r.CloseKey;
    r.Free;
  end;
end;

