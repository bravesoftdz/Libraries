unit API_Files;

interface

type
  TFilesEngine = class
  public
    class procedure CreateFile(aFileName: String);
    class function GetTextFromFile(aFileName: String): String;
    class procedure SaveTextToFile(aFileName, aText: String);
    class procedure AppendToFile(aFileName, aText: String);
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

class procedure TFilesEngine.CreateFile(aFileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.SaveToFile(aFileName);
  finally
    SL.Free;
  end;
end;

class procedure TFilesEngine.AppendToFile(aFileName, aText: String);
var
  EditFile: TextFile;
begin
  //try
    //AssignFile(EditFile, aFileName, CP_UTF8);
    AssignFile(EditFile, aFileName);
    Append(EditFile);
    WriteLn(EditFile, aText);
    CloseFile(EditFile);
  //except
    //raise;
  //end;
end;

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
