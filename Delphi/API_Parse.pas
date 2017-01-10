unit API_Parse;

interface

type
  TParseTools = class
    class function ParseByRegEx(aPage:string; aRegEx:string): TArray<string>;
    class function ParseStrByRegEx(aPage:string; aRegEx:string): string;
    class function Explode(aIncome: string; aDelimiter: string): TArray<string>;
    class function GetNormalizeString(aLine: string): string;
  end;

implementation

uses
   System.SysUtils
  ,RegularExpressions;

class function TParseTools.GetNormalizeString(aLine: string): string;
begin
  while Pos(#$D, aLine)>0 do
    aLine:=StringReplace(aLine, #$D, ' ', [rfReplaceAll, rfIgnoreCase]);
  while Pos(#$A, aLine)>0 do
    aLine:=StringReplace(aLine, #$A, ' ', [rfReplaceAll, rfIgnoreCase]);
  while Pos('  ', aLine)>0 do
    aLine:=StringReplace(aLine, '  ', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result:=Trim(aLine);
end;

class function TParseTools.ParseByRegEx(aPage:string; aRegEx:string): TArray<string>;
var
  m: TMatch;
begin
  m := TRegEx.Match(aPage, aRegEx);
  while m.Success do
    begin
      Result := Result + [m.value];
      m := m.NextMatch;
    end;
end;

class function TParseTools.ParseStrByRegEx(aPage:string; aRegEx:string): string;
var
  m: TArray<string>;
begin
  Result:='';
  m:=ParseByRegEx(aPage, aRegEx);
  if Length(m)>0 then Result:=m[0];
end;

class function TParseTools.Explode(aIncome: string; aDelimiter: string): TArray<string>;
var
  i: integer;
  tx: string;
begin
  tx:='';
  for i := 1 to Length(aIncome) do
    begin
      if (aIncome[i]<>aDelimiter) then tx:=tx+aIncome[i];
      if (aIncome[i]=aDelimiter) or (i=Length(aIncome)) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[Length(Result)-1]:=Trim(tx);
          tx:='';
        end;
    end;
end;

end.
