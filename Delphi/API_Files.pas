unit API_Files;

interface

type
  TFilesEngine = class
  public
    class function GetTextFromFile(aFileName: String): String;
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

class function TFilesEngine.GetTextFromFile(aFileName: string): string;
var
  SL: TStringList;
begin
  if FileExists(aFileName) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(aFileName);
      Result := SL.Text;
    finally
      SL.Free;
    end;
  end;
end;

end.
