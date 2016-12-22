unit API_Parse;

interface

uses
   Vcl.Dialogs
  ,SHDocVw
  ,MSHTML
  ,API_DBases
  ,API_Files;

type
  TProcessDOM = procedure(aDocument: IHTMLDocument2) of object;

  TJobRule=record
    Mode: string;
    Level: Integer;
    XPath: string;
  end;

  TJobParam=record
    level: Integer;
    rules: TArray<TJobRule>;
  end;

  TJob=class
  private
    FZeroLink: string;
    FId: Integer;
    FParams: TArray<TJobParam>;
  public
    constructor Create(aJobID: integer; aMySQLEngine: TMySQLEngine);
    property Id: integer read FId;
    property ZeroLink: string read FZeroLink;
    property Params: TArray<TJobParam> read FParams;
  end;

  TCurrLink=record
    Id: Integer;
    Link: string;
    Level: Integer;
  end;

  TParser=class
  private
    FMySQLEngine: TMySQLEngine;
    FJob: TJob;
    FWebBrowser: TWebBrowser;
    FProcessDOM: TProcessDOM;
    function CheckFirstRun: Boolean;
    function AddLink(aLink: string; aLevel: Integer): Integer;
    procedure WebBrowserInit;
    procedure WebBrowserDocumentComplete(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
  public
    constructor Create(aJob: TJob; aMySQLEngine: TMySQLEngine);
    function GetCurrLink: TCurrLink;
    procedure GetDocumentByLink(aCurrLink: TCurrLink; aCallBack: TProcessDOM);
    property ProcessDOM: TProcessDOM read FProcessDOM write FProcessDOM;
  end;

  TParserModel=class
  private
    FParser: TParser;
    procedure ProcessDOM(aDocument: IHTMLDocument2);
  public
    procedure StartJob(aJobID: integer);
  end;

  TParseTools = class
    class function ParseByKey(aPage:string; aFirstKey:string; aLastKey:string; aFirstKeyCount:integer = 0): string;
    class function Explode(aIncome: string; aDelimiter: string): TArray<string>;
  end;

implementation

uses
   FireDAC.Comp.Client
  ,Variants, unit1, Vcl.Controls, System.SysUtils;

class function TParseTools.ParseByKey(aPage:string; aFirstKey:string; aLastKey:string; aFirstKeyCount:integer = 0): string;
var
  tx:string;
  i:integer;
begin
  if aFirstKey.Length=0 then
    begin
      aFirstKey:='!#!';
      aPage:='!#!'+aPage;
    end;

  for i:=1 to aFirstKeyCount-1 do
    begin
      Delete(aPage, 1, Pos(aFirstKey, aPage) + Length(aFirstKey));
    end;
  if Pos(aFirstKey, aPage)>0 then
    begin
      tx:=Copy(aPage, Pos(aFirstKey, aPage) + Length(aFirstKey), Length(aPage));
      Delete(tx, Pos(aLastKey, tx), Length(tx));
      tx:=Trim(tx);
    end;
  Result:=tx;
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

procedure TParser.WebBrowserDocumentComplete(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
begin
  ProcessDOM(FWebBrowser.Document as IHtmlDocument2);
end;

procedure TParserModel.ProcessDOM(aDocument: IHTMLDocument2);
var
  sXPath: string;
  i: integer;
  Collection: IHTMLElementCollection;
  iElement: IHTMLElement;
  XPath: TArray<string>;
  Tag: string;
  TagIndx: Integer;
begin






  sXPath:='/html/body/div[3]/div[3]/div[4]/div[2]/div[2]/div/div/div/ul/li/a';

  XPath:=TParseTools.Explode(sXPath, '/');
  Collection:=aDocument.all as IHTMLElementCollection;
  for i := 1 to Length(XPath)-1 do
    begin
      Tag:=XPath[i];
      TagIndx:=StrToIntDef(TParseTools.ParseByKey(Tag, '[', ']'), 1)-1;
      Delete(Tag, Pos('[', Tag), Length(Tag));

      Collection:=Collection.tags(Tag) as IHTMLElementCollection;
      iElement:=Collection.Item(TagIndx, TagIndx) as IHTMLElement;
      Collection:=iElement.children as IHTMLElementCollection;
    end;
  ShowMessage(iElement.outerText);
end;

procedure TParser.GetDocumentByLink(aCurrLink: TCurrLink; aCallBack: TProcessDOM);
begin
  ProcessDOM:=aCallBack;
  FWebBrowser.Navigate(aCurrLink.Link);
end;

procedure TParser.WebBrowserInit;
begin
  FWebBrowser:=TWebBrowser.Create(Form1);
  TWinControl(FWebBrowser).Parent:=Form1;
  FWebBrowser.Align := alClient;

  FWebBrowser.OnDocumentComplete:=WebBrowserDocumentComplete;
end;

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

function TParser.GetCurrLink: TCurrLink;
var
  dsQuery: TFDQuery;
  sql: string;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    sql:='select links.*';
    sql:=sql+',(select count(*) from links t where t.job_id=links.job_id) links_count';
    sql:=sql+' from links';
    sql:=sql+' where job_id=:JobID';
    sql:=sql+' and handled is null';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('JobID').AsInteger:=FJob.Id;
    FMySQLEngine.OpenQuery(dsQuery);

    Result.Id:=dsQuery.FieldByName('Id').AsInteger;
    Result.Link:=dsQuery.FieldByName('link').AsString;
    Result.Level:=dsQuery.FieldByName('level').AsInteger;
  finally
    dsQuery.Free;
  end;
end;

constructor TParser.Create(aJob: TJob; aMySQLEngine: TMySQLEngine);
begin
  FJob:=aJob;
  FMySQLEngine:=aMySQLEngine;
  if CheckFirstRun then AddLink(FJob.ZeroLink, 0);
  WebBrowserInit;
end;

procedure TParserModel.StartJob(aJobID: integer);
var
  MySQLEngine: TMySQLEngine;
  Job: TJob;
  CurrLink: TCurrLink;
begin
  MySQLEngine:=TMySQLEngine.Create;
  MySQLEngine.OpenConnection('MySQL.ini');
  Job:=TJob.Create(aJobID, MySQLEngine);
  FParser:=TParser.Create(Job, MySQLEngine);

  CurrLink:=FParser.GetCurrLink;
  FParser.GetDocumentByLink(CurrLink, ProcessDOM);
  {try

    isProcessing := True;
    while isProcessing do
      begin
        isProcessing := FParser.GetLinkToProcess(CurrLink);

        Document:=FParser.GetDocumentByLink(CurrLink);
      end;

  finally
    FParser.Free;
    MySQLEngine.Free;
    Job.Free;
  end; }
end;
end.
