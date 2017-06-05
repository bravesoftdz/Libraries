unit API_Files;

interface

type
  TFilesEngine = class
  public
    class function GetTextFromFile(aFileName: String): String;
    class procedure SaveTextToFile(aFileName, aText: String);
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

class procedure TFilesEngine.SaveTextToFile(aFileName, aText: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := aText;
    SL.SaveToFile(aFileName);
  finally
    SL.Free;
  end;
end;

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
