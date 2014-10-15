unit tisutils;
{$mode delphi}
{$h+}

interface

uses
	Classes,SysUtils,IniFiles;
type
  TUrlIniFile=Class(TMemIniFile)
    public
      constructor Create(const URL: string);
  end;

function Appuserinipath:String;
function GetUserName : String;
function GetApplicationName:String;
function GetPersonalFolder:String;
function GetAppdataFolder:String;
function GetApplicationVersion(FileName:String=''):String;

function GetCmdParams(ID:String;Default:String=''):String;

//Create a Tmemorystream or a Tfilestream with the content of the result of http URL or file location
// if the file exist (url is a local file location of type p:\bin\truc.jpg), then a filestream is created and returned in stream
// else the http protocol is used and a memorystream is returned
// IMPORTANT : stream should be freed by the caller after use !
function httpNewStream(url:String):TStream;

function phonenormalize(phone:String):String;
function phonesimplify(number:String):String;

function ISODebutSem(ADate:TDateTime):TDateTime;
function ISOFinSem(ADate:TDateTime):TDateTime;

implementation

uses FileUtil,tisdatetime,tisstrings,tishttp;

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
		S:=ParamStr(i);
		if
			(AnsiCompareText(Copy(S, 1, Length(ID)+2), '/'+ID+'=') = 0) or
			(AnsiCompareText(Copy(S, 1, Length(ID)+2), '-'+ID+'=') = 0) then
		begin
      found:=True;
			Result:=Copy(S,Length(ID)+2+1,MaxInt);
			Break;
		end;
	end;
  if not Found then
    Result:=Default;
end;

function GetUserName : String;
begin
  result := GetUserName;
end;

function GetApplicationName:String;
begin
  //Result := Application.Name;
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)),'');
end;

function GetPersonalFolder:String;
begin
  result := GetUserDir;
end;

function GetAppdataFolder:String;
begin
  result :=  GetAppConfigDir(False);
end;

function Appuserinipath:String;
var
  dir : String;
begin
  dir := GetAppConfigFile(False,True);
  if not DirectoryExistsUTF8(dir) { *Converted from DirectoryExists*  } then
    MkDir(dir);
  Result:=IncludeTrailingPathDelimiter(dir)+GetApplicationName+'.ini';
end;

function GetApplicationVersion(Filename:String=''): String;
begin
  Result:='';
end;

{ TUrlIniFile }

constructor TURLIniFile.Create(const URL: string);
var
  St:TStringList;
begin
  inherited Create('');
  St:=TStringList.Create;
  try
    St.Text:=httpGetString(URL);
    SetStrings(St);
  finally
    St.Free;
  end;
end;

Function httpNewStream(url:String):TStream;
var
  content:String;
begin
  if not FileExistsUTF8(URL) { *Converted from FileExists*  } then
  begin
    result:=TMemoryStream.Create;
    Content:=httpGetString(url);
    Result.Write(Content[1],Length(Content));
    result.Seek(0,soFromBeginning);
  end
  else
    result:=TFileStream.Create(URL,fmOpenRead,fmShareDenyNone);
end;

function ISODebutSem(ADate:TDateTime):TDateTime;
var
	D, W, Y: Integer;
begin
	W := ISOWeekNumber(ADate, Y, D);
	Result := ISOWeekToDateTime(Y, W, 1);
end;

function ISOFinSem(ADate:TDateTime):TDateTime;
var
	D, W, Y: Integer;
begin
	W := ISOWeekNumber(ADate, Y, D);
	Result := ISOWeekToDateTime(Y, W, 7);
end;

function phonenormalize(phone:String):String;
var
  i:integer;
begin
  Result := '';
  //supression de tout sauf digits
  for i:=1 to length(phone) do
    if CharIsDigit(Phone[i]) then
      Result := Result+phone[i];
  if copy(Result,1,4)='0033' then
    result := copy(result,5,255);
  //ajout un zéro en tête
  if (length(result)=9) and (result[1]<>'0')
    then result := '0'+Result;
  if length(result)>=10 then
    result := copy(result,1,2)+'.'+copy(result,3,2)+'.'+copy(result,5,2)+'.'+copy(result,7,2)+'.'+copy(result,9,2);
end;

function phonesimplify(number:String):String;
var
  i:integer;
begin
  Result:='';
  for i:=1 to length(number) do
     if CharIsDigit(number[i]) or (number[i]='+') then
      Result := Result + number[i];
end;

end.
