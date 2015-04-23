unit tiswinhttpini;
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

//Create a Tmemorystream or a Tfilestream with the content of the result of http URL or file location
// if the file exist (url is a local file location of type p:\bin\truc.jpg), then a filestream is created and returned in stream
// else the http protocol is used and a memorystream is returned
// IMPORTANT : stream should be freed by the caller after use !
function httpNewStream(url:String):TStream;

implementation

uses FileUtil,tiswinhttp;

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


end.
