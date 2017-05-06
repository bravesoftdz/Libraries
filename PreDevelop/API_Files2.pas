unit API_Files;

interface

uses
   System.Classes
  ,System.SysUtils;

type
  TFilesEngine = class
    public
      class function GetTextFromFile(FileName: String): String;
      class procedure SaveTextToFile(FileName, Text: String);
      class procedure AppendToFile(FileName, Text: String);
  end;

implementation

class procedure TFilesEngine.AppendToFile(FileName, Text: String);
var
  EditFile: TextFile;
begin
  try
    AssignFile(EditFile, FileName);
    Append(EditFile);
    WriteLn(EditFile, Text);
    CloseFile(EditFile);
  except
  end;
end;

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

class procedure TFilesEngine.SaveTextToFile(FileName, Text: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Text;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

end.
