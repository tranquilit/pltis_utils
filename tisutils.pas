unit tisutils;
{$mode delphiunicode}
{.$h+}

interface

uses
	Classes,SysUtils;

function phonenormalize(phone:String):String;
function phonesimplify(number:String):String;

function ISODebutSem(ADate:TDateTime):TDateTime;
function ISOFinSem(ADate:TDateTime):TDateTime;

implementation

uses FileUtil,tisdatetime,tisstrings;

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
