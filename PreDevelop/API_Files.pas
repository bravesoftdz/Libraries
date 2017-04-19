unit API_Files;

interface

uses
  System.JSON, System.SysUtils, System.Classes, Vcl.Dialogs;

type
  TFilesEngine = class
  private
    FPathList: array of string;
    FCurrentPath: String;
    function CheckLastSlash(Path: String): String;
    procedure ScanForNewPath;
    procedure CheckThisPath(FileMask: String; JSON: TJSONObject);
  public
    procedure GetFileNamesByMask(SearchPath, FileMask: String;
      JSON: TJSONObject);
    class procedure CreateFile(FileName: String);
    class procedure SaveTextToFile(FileName, Text: String);
    class procedure AppendToFile(FileName, Text: String);
    class function GetTextFromFile(FileName: String): String;
  end;

implementation

procedure TFilesEngine.CheckThisPath(FileMask: string; JSON: TJSONObject);
var
  SearchResult: TSearchRec;
  SearchMask: String;
  jpair: TJSONPair;
begin
  SearchMask := FCurrentPath + FileMask;
  if FindFirst(SearchMask, faAnyFile, SearchResult) = 0 then
  begin
    repeat
      if (SearchResult.Name <> '.') and (SearchResult.Name <> '..') then
      begin
        jpair := TJSONPair.Create(FCurrentPath, SearchResult.Name);
        JSON.AddPair(jpair);
      end;
    until FindNext(SearchResult) <> 0;
    FindClose(SearchResult);
  end;
end;

procedure TFilesEngine.ScanForNewPath;
var
  SearchResult: TSearchRec;
  Path: String;
begin
  if FindFirst(FCurrentPath + '*.*', faAnyFile, SearchResult) = 0 then
  begin
    repeat
      if (SearchResult.Attr = faDirectory) and (SearchResult.Name <> '.') and
        (SearchResult.Name <> '..') then
      begin
        Path := FCurrentPath + SearchResult.Name;
        FPathList := FPathList + [Path];
      end;
    until FindNext(SearchResult) <> 0;
    FindClose(SearchResult);
  end;
end;

function TFilesEngine.CheckLastSlash(Path: string): String;
begin
  if Path[Length(Path)] <> '\' then
    Path := Path + '\';
  result := Path;
end;

procedure TFilesEngine.GetFileNamesByMask(SearchPath: string; FileMask: string;
  JSON: TJSONObject);
begin
  SearchPath := CheckLastSlash(SearchPath);
  FPathList := [SearchPath];

  while Length(FPathList) > 0 do
  begin
    FCurrentPath := CheckLastSlash(FPathList[0]);
    ScanForNewPath;
    CheckThisPath(FileMask, JSON);
    Delete(FPathList, 0, 1);
  end;
end;

class procedure TFilesEngine.CreateFile(FileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

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
    ShowMessage('Ошибка открытия или записи файла');
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
      result := SL.Text;
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
