unit tiscommon;
{ -----------------------------------------------------------------------
#    This file is part of WAPT
#    Copyright (C) 2013  Tranquil IT Systems http://www.tranquil.it
#    WAPT aims to help Windows systems administrators to deploy
#    setup and update applications on users PC.
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
    Classes, SysUtils, tisstrings, Process
{$IFDEF WINDOWS}
    , Windows, JwaWindows
{$ENDIF}
;

{$IFDEF WINDOWS}
procedure UpdateApplication(fromURL:String;SetupExename,SetupParams,ExeName,RestartParam:String);
function UserInGroup(Group :DWORD) : Boolean;

function IsWin64: Boolean;

procedure AddToUserPath(APath:String);

function EnablePrivilege(const Privilege: string; fEnable: Boolean; out PreviousState: Boolean): DWORD;


function GetAccountSid(const Server, User: WideString; var Sid: PSID): DWORD;
function GetAccountSidString(const Server, User: WideString):String;
function StrSIDToName(const StrSID: AnsiString; var Name: Ansistring; var SIDType: DWORD): Boolean;
function SIDToStringSID(const aSID:PSID): String;
function AddToGroup(const member, Group: WideString): NET_API_STATUS;
function UserModalsGet(const Server: String): USER_MODALS_INFO_0;

// Return domain name
function DomainGet: String;

// Return current domain sid
function DomainSID: String;

// Return the NetBIOS name of the domain or workgroup to which the computer is joined
function GetJoinInformation:String;

function IsAdmin: LongBool;
function GetAdminSid: PSID;

function GetApplicationVersion(FileName:String=''): String;

function GetOSVersionInfo: TOSVersionInfoEx;
function IsWinXP:Boolean;

function GetPersonalFolder:String;
function GetLocalAppdataFolder:String;
function GetAppdataFolder:String;

function GetStartMenuFolder: String;
function GetCommonStartMenuFolder: String;
function GetStartupFolder: String;
function GetCommonStartupFolder: String;

const
  NameUnknown       = 0; // Unknown name type.
  NameFullyQualifiedDN = 1; // Fully qualified distinguished name
  NameSamCompatible = 2; // Windows NT® 4.0 account name
  NameDisplay       = 3; // A "friendly" display name
  NameUniqueId      = 6; // GUID string that the IIDFromString function returns
  NameCanonical     = 7; // Complete canonical name
  NameUserPrincipal = 8; // User principal name
  NameCanonicalEx   = 9;
  NameServicePrincipal = 10; // Generalized service principal name
  DNSDomainName     = 11; // DNS domain name, plus the user name

function GetCurrentUserName(fFormat: DWORD=NameSamCompatible) : Ansistring;
function GetCurrentUserSid: Ansistring;

procedure SetUserProfilePath(SID:String;ImagePath:String);
function GetUserProfilePath(SID:String):String;

function UserLogin(user,password,domain:String):THandle;
function UserDomain(htoken:THandle):AnsiString;
function OnSystemAccount: Boolean;

function GetGroups(srvName, usrName: WideString):TStringArray;
function GetLocalGroups:TStringArray;
function GetLocalGroupMembers(GroupName:WideString):TStringArray;

const
  CSIDL_LOCAL_APPDATA = $001c;
  strnShellFolders = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';

type
  TServiceState =
   (ssUnknown,         // Just fill the value 0
    ssStopped,         // SERVICE_STOPPED
    ssStartPending,    // SERVICE_START_PENDING
    ssStopPending,     // SERVICE_STOP_PENDING
    ssRunning,         // SERVICE_RUNNING
    ssContinuePending, // SERVICE_CONTINUE_PENDING
    ssPausePending,    // SERVICE_PAUSE_PENDING
    ssPaused);         // SERVICE_PAUSED

  TServiceStates = set of TServiceState;

const
  ssPendingStates = [ssStartPending, ssStopPending, ssContinuePending, ssPausePending];

function IsAdminLoggedOn: Boolean;
function ProcessExists(ExeFileName: string): boolean;
function KillTask(ExeFileName: string): integer;

function GetServiceStatusByName(const AServer,AServiceName:ansistring):TServiceState;
function StartServiceByName(const AServer,AServiceName: AnsiString):Boolean;
function StopServiceByName(const AServer, AServiceName: AnsiString):Boolean;

function ProgramFilesX86:String;

{$ELSE}
{$IFDEF UNIX}
function UserInGroup(Group :DWORD) : Boolean;

function GetApplicationVersion(FileName:String=''): String;
{$ENDIF}
{$ENDIF}

procedure UnzipFile(ZipFilePath,OutputPath:String);

function GetSystemProductName: String;
function GetSystemManufacturer: String;
function GetBIOSVendor: String;
function GetBIOSVersion: String;
function GetBIOSDate: String;

function GetComputerName : String;
function GetUserName : String;
function GetWorkgroupName: String;
function GetDomainName: String;

function AppUserIniPath:String;
function GetAppUserFolder:String;
function MakePath(const parts:array of String):String;
function GetUniqueTempdir(Prefix: String): String;

function SortableVersion(VersionString:String):String;

// Compare package versions like 1.2.3-4
// Compare first part before '-' then 2nd part after '-'
// returns -1 if v1<v2, 0 if v1=v2 and +1 if v1>v2
// try to convert each version memebr into integer to compare
// if member is not an int, compare as string.
function CompareVersion(const v1,v2:String;MembersCount:Integer=-1):integer;

function GetIPFromHost(const Hostname: String): String;

// Starts a process as shell with cmd command line. Returns stdout of process.
function RunTask(cmd: String;out ExitStatus:integer;WorkingDir:String='';ShowWindow:TShowWindowOptions=swoHIDE): String;

// Get a command line argument with either /id=value  -id=value or --id=value
// if ID is not found in command line, returns Default
function GetCmdParams(ID:String;Default:String=''):String;

procedure ResetMemory(out P; Size: Longint);

function ExtractResourceString(Ident: String): RawByteString;

const
  Language:String = '';
  LanguageFull:String = '';

implementation

uses
  mormot.core.base,
  registry,
  LazFileUtils,
  LazUTF8,
  {$ifdef windows}
  tiswinhttp,
  {$endif}
  tislogging,
  gettext,
  uSMBIOS,
  mormot.core.zip
  {$IFDEF UNIX}
  , baseunix, errors, sockets, unix,
    {$IFDEF DARWIN}
  netdb
    {$ELSE}
  cnetdb
    {$ENDIF}
  {$ELSE IFDEF WINDOWS}
  , shlobj, winsock2
  {$ENDIF}
  ;

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

{$IF defined(WINDOWS)}
{$i tiscommonwin.inc}
{$ELSEIF defined(UNIX)}
{$i tiscommonunix.inc}
{$ENDIF}

function MakePath(const parts:array of String):String;
var
  i:integer;
begin
  result := '';
  for i:=low(parts) to high(parts) do
  begin
    result := Result+parts[i];
    if (i<High(parts)) and (parts[i]<>'') and (parts[i][length(parts[i])] <> PathDelim) then
      result := result+PathDelim;
  end;
end;

procedure UnzipFile(ZipFilePath, OutputPath: String);
var
  UnZipper: TZipRead;
  ErrorIdx: Integer;
  FI: TFileInfoFull;
begin
  UnZipper := TZipRead.Create(ZipFilePath);
  try
    ErrorIdx := UnZipper.UnZipAll(OutputPath);
    if ErrorIdx >= 0 then
    begin
      UnZipper.RetrieveFileInfo(ErrorIdx,FI);
      Raise Exception.Create('Error extracting file from zip '+UnZipper.Entry[ErrorIdx].zipName);
    end;
  finally
    UnZipper.Free;
  end;
end;

function GetUniqueTempdir(Prefix: String): String;
var
  I: Integer;
  Start: String;
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

function GetBIOSDate: String;
var
  SMBios : TSMBios;
begin
  SMBios:=TSMBios.Create;
  try
     Result:=SMBios.BiosInfo.ReleaseDateStr;
  finally
    SMBios.Free;
  end;
end;

function GetSystemProductName: String;
var
  SMBios : TSMBios;
  LBaseBoard : TBaseBoardInformation;
begin
  SMBios:=TSMBios.Create;
  try
     if SMBios.HasBaseBoardInfo then
        for LBaseBoard in SMBios.BaseBoardInfo do
          Result:=LBaseBoard.ProductStr
      else
          Result:='No Base Board Info was found';
  finally
    SMBios.Free;
  end;
end;

function GetSystemManufacturer: String;
var
  SMBios : TSMBios;
  LBaseBoard : TBaseBoardInformation;
begin
  SMBios:=TSMBios.Create;
  try
     if SMBios.HasBaseBoardInfo
      then
        for LBaseBoard in SMBios.BaseBoardInfo do
        begin
          Result:=LBaseBoard.ManufacturerStr;
        end
      else
          Result:= 'No Base Board Info was found'
  finally
    SMBios.Free;
  end;
end;

function GetBIOSVendor: String;
var
  SMBios : TSMBios;
begin
  SMBios:=TSMBios.Create;
  try
     Result:=SMBios.BiosInfo.VendorStr;
  finally
    SMBios.Free;
  end;
end;

function GetBIOSVersion: String;
var
  SMBios : TSMBios;
begin
  SMBios:=TSMBios.Create;
  try
     Result:=SMBios.BiosInfo.VersionStr;
  finally
    SMBios.Free;
  end;
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

function CompareVersion(const v1,v2:String;MembersCount:Integer=-1):integer;
var
  version1,version2,pack1,pack2,tok1,tok2:String;
  I1,I2: Int64;
  Error: Integer;
  MemberIdx:integer;

  function CompareInt(const i1,i2:Int64):integer; inline;
  begin
    if i1<i2 then exit(-1)
    else if i1=i2 then exit(0)
    else exit(1)
  end;

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
    MemberIdx:=1;

    if (tok1<>'') or (tok2<>'') then
    begin
      if (MembersCount>0) and (MemberIdx<=MembersCount) then
      begin
        If tok1='' then tok1 := '0';
        If tok2='' then tok2 := '0';
      end;

      Val(Tok1, I1, Error);
      if Error=0 then
      begin
        Val(Tok2, I2, Error);
        if Error=0 then
          result := CompareInt(I1,I2)
        else
          result := CompareStr(tok1,tok2)
      end
      else
        result := CompareStr(tok1,tok2);
    end;
    if (result<>0) or (tok1='') or (tok2='') or ((MembersCount>0) and (MemberIdx>=MembersCount)) then
      break;
    inc(MemberIdx);
  until (result<>0) or (tok1='') or (tok2='');

  // packaging
  if (Result=0) and ((pack1<>'') or (pack2<>'')) then
  begin
    if (pack1<>'') or (pack2<>'') then
    try
      result := CompareInt(StrToInt(pack1),StrToInt(pack2));
    except
      result := CompareStr(pack1,pack2)
    end;
  end;
end;

function GetComputerName : String;
var
  {$IF defined(UNIX)}
  Host: AnsiString;
  {$ELSEIF defined(WINDOWS)}
  buffer: array[0..255] of WideChar;
  size: dword;
  {$ENDIF}
begin
  Result := '';
  {$IF defined(UNIX)}
  Host := gethostname();
  if (Host <> '') then
  begin
    if Pos('.', Host) > 1 then
      Result := Copy(Host, 1, Pos('.', Host) - 1)
    else
      Result := Host;
  end;
  {$ELSEIF defined(WINDOWS)}
  size := 256;
  if windows.GetComputerNameW(@buffer, size) then
    Result := UTF8Encode(WideString(buffer))
  {$ENDIF}
end;

function GetUserName: String;
{$IF defined(WINDOWS)}
var
  pcUser   : PWideChar;
  dwUSize : DWORD;
{$ENDIF}
begin
  Result := '';
  {$IF defined(UNIX)}
  Result := GetEnvironmentVariable('LOGNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
  {$ELSEIF defined(WINDOWS)}
  dwUSize := 21 * SizeOf(WideChar); // user name can be up to 20 characters
  GetMem( pcUser, dwUSize); // allocate memory for the string
  try
         if GetUserNameW( pcUser, dwUSize ) then
              Result := pcUser;
  finally
         FreeMem( pcUser ); // now free the memory allocated for the string
  end;
  {$ENDIF}
end;

function GetWorkGroupName(): String;
var
  {$IF defined(UNIX)}
  FileLines: TStringList;
  Line: String;
  {$ELSEIF defined(WINDOWS)}
  WkstaInfo: PByte;
  WkstaInfo100: PWKSTA_INFO_100;
  {$ENDIF}
begin
  Result:='';
  {$IF defined(UNIX)}
  try
    FileLines:=TStringList.Create();
    FileLines.LoadFromFile('/etc/samba/smb.conf');
    for Line in FileLines do
        if Pos('workgroup',Line)<>0 then
        begin
          Result:=TrimLeft(Copy(Line, Pos('=', Line) + 1, Length(Line)));
          Break;
        end;
  finally
    FileLines.Free;
  end;
  {$ELSEIF defined(WINDOWS)}
  if NetWkstaGetInfo(nil, 100, WkstaInfo) = 0 then
  begin
    WkstaInfo100 := PWKSTA_INFO_100(WkstaInfo);
    Result := WkstaInfo100^.wki100_langroup;
    NetApiBufferFree(Pointer(WkstaInfo))
  end;
  {$ENDIF}
end;

function GetDomainName(): String;
var
  {$IF defined(UNIX)}
  Host: String;
  {$ELSEIF defined(WINDOWS)}
  hProcess, hAccessToken: THandle;
  InfoBuffer: PWideChar;
  AccountName: array [0..UNLEN] of WideChar;
  DomainName: array [0..UNLEN] of WideChar;

  InfoBufferSize: Cardinal;
  AccountSize: Cardinal;
  DomainSize: Cardinal;
  snu: SID_NAME_USE;
  {$ENDIF}
begin
  Result := '';
  {$IF defined(UNIX)}
  Result := unix.GetDomainName();
  if Result = '(none)' then
  begin
    Result := '';
    Host := gethostname();
    if (Host <> '') and (Pos('.', Host) > 0) then
      Result := Copy(Host, Pos('.', Host) + 1, 255);
  end;
  {$ELSEIF defined(WINDOWS)}
  InfoBufferSize := 1000;
  AccountSize := SizeOf(AccountName);
  DomainSize := SizeOf(DomainName);
  hProcess := GetCurrentProcess;
  if OpenProcessToken(hProcess, TOKEN_READ, hAccessToken) then
  try
    GetMem(InfoBuffer, InfoBufferSize);
    try
      if GetTokenInformation(hAccessToken, TokenUser, InfoBuffer, InfoBufferSize, InfoBufferSize) then
        LookupAccountSidW(nil, PSIDAndAttributes(InfoBuffer)^.sid, AccountName, AccountSize,
                         DomainName, DomainSize, snu)
      else
        RaiseLastOSError;
    finally
      FreeMem(InfoBuffer)
    end;
    Result := DomainName;
  finally
    CloseHandle(hAccessToken);
  end
  {$ENDIF}
end;

procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

function GetCmdParams(ID:String;Default:String=''):String;
var
    i:integer;
    S:String;
  found:Boolean;
begin
    Result:='';
  found:=False;
    for i:=1 to ParamCount do
    begin
        S:=ParamStrUTF8(i);
        if
            (UTF8CompareStr(Copy(S, 1, Length(ID)+2), '/'+ID+'=') = 0) or
            (UTF8CompareStr(Copy(S, 1, Length(ID)+2), '-'+ID+'=') = 0) or
            (UTF8CompareStr(Copy(S, 1, Length(ID)+3), '--'+ID+'=') = 0) then
        begin
      found:=True;
            Result:=Copy(S,pos('=',S)+1,MaxInt);
            Break;
        end;
    end;
  if not Found then
    Result:=Default;
end;

function RunTask(cmd: String;out ExitStatus:integer;WorkingDir:String='';ShowWindow:TShowWindowOptions=swoHIDE): String;
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

function ExtractResourceString(Ident: String): RawByteString;
var
  S: TResourceStream;
begin
  S := TResourceStream.Create(HInstance, Ident, MAKEINTRESOURCE(10)); // RT_RCDATA
  try
    SetLength(Result,S.Size);
    S.Seek(0,soFromBeginning);
    S.Read(PChar(Result)^,S.Size);
  finally
    S.Free; // destroy the resource stream
  end;
end;


initialization
  GetLanguageIDs(LanguageFull,Language);

end.
