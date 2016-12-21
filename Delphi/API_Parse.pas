unit API_Parse;

interface

uses
   Vcl.Dialogs
  ,API_DBases;

type
  TJob=class
  private
    FZeroLink: string;
    FId: Integer;
  public
    constructor Create(aJobID: integer; aMySQLEngine: TMySQLEngine);
    property Id: integer read FId;
    property ZeroLink: string read FZeroLink;
  end;

  TParser=class
  private
    FMySQLEngine: TMySQLEngine;
    FJob: TJob;
    function CheckFirstRun: Boolean;
    function AddLink(aLink: string; aLevel: Integer): Integer;
  public
    constructor Create(aJobID: Integer; aMySQLEngine: TMySQLEngine);
    function GetLinkToProcess(out aLinkId, aLevel: integer): Boolean;
  end;

  TParserModel=class
  private
    FJobID: Integer;
    FParser: TParser;
    procedure ProcessLink(aLink: Integer);
  public
    procedure Execute(aJobID: integer);
  end;

implementation

uses
  FireDAC.Comp.Client;

function TParser.AddLink(aLink: string; aLevel: Integer): Integer;
var
  dsQuery: TFDQuery;
  sql: string;
begin
  sql:='insert into links set';
  sql:=sql+' job_id=:JobId';
  sql:=sql+',level=:Level';
  sql:=sql+',link=:Link';
  sql:=sql+',link_hash=md5(:Link)';
  dsQuery:=TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('JobId').AsInteger:=FJob.Id;
    dsQuery.ParamByName('Level').AsInteger:=aLevel;
    dsQuery.ParamByName('Link').AsString:=aLink;

    FMySQLEngine.ExecQuery(dsQuery);
    Result:=FMySQLEngine.GetLastInsertedID;
  finally
    dsQuery.Free;
  end;
end;

constructor TJob.Create(aJobID: Integer; aMySQLEngine: TMySQLEngine);
var
  dsQuery: TFDQuery;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text:='select * from jobs where id=:JobID';
    dsQuery.ParamByName('JobID').AsInteger:=aJobID;
    aMySQLEngine.OpenQuery(dsQuery);

    FId:=dsQuery.FieldByName('Id').AsInteger;
    FZeroLink:=dsQuery.FieldByName('zero_link').AsString;
  finally
    dsQuery.Free
  end;
end;

procedure TParserModel.ProcessLink(aLink: Integer);
begin

end;

function TParser.CheckFirstRun: Boolean;
var
  dsQuery: TFDQuery;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text:='select count(*) as LinksCount from links where job_id=:JobID';
    dsQuery.ParamByName('JobID').AsInteger:=FJob.Id;
    FMySQLEngine.OpenQuery(dsQuery);
    if dsQuery.FieldByName('LinksCount').AsInteger=0 then Result:=True
    else Result:=False;
  finally
    dsQuery.Free
  end;
end;

function TParser.GetLinkToProcess(out aLinkId, aLevel: integer): Boolean;
begin
  Result:=False;

  {if FisFirstRun then
    begin
      FisFirstRun:=False;
      Result:=True;
      aLinkId:=0;
      aLevel:=1;
    end;}
end;

constructor TParser.Create(aJobID: Integer; aMySQLEngine: TMySQLEngine);
begin
  FMySQLEngine:=aMySQLEngine;
  FJob:=TJob.Create(aJobID, FMySQLEngine);
  if CheckFirstRun then AddLink(FJob.ZeroLink, 0);
end;

procedure TParserModel.Execute(aJobID: integer);
var
  MySQLEngine: TMySQLEngine;
  isProcessing: Boolean;
  LinkId, Level: Integer;
begin
  MySQLEngine:=TMySQLEngine.Create;
  MySQLEngine.OpenConnection('MySQL.ini');
  FParser:=TParser.Create(aJobID, MySQLEngine);
  try

    isProcessing := True;
    while isProcessing do
      begin
        isProcessing := FParser.GetLinkToProcess(LinkId, Level);

        ProcessLink(LinkId);
      end;

  finally
    FParser.Free;
    MySQLEngine.Free;
  end;
end;
end.
