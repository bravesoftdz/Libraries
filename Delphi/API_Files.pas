unit API_Files;

interface

uses
   System.Classes
  ,System.SysUtils;

type
  TFilesEngine = class
    public
      class function GetTextFromFile(FileName: String): String;
  end;

implementation

class function TFilesEngine.GetTextFromFile(FileName: string): string;
var
  SL: TStringList;
begin
  if FileExists(FileName) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FileName);
      Result := SL.Text;
    finally
      SL.Free;
    end;
  end;
end;

end.
