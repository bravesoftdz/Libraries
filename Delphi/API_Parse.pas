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
    ObjType: set of (Link, Text);
    Level: Integer;
    XPath: string;
    ContainerOffset: Integer;
    RegExp: string;
  end;

  TJobParam=record
    Level: Integer;
    Rules: TArray<TJobRule>;
  end;

  TJob=class
  private
    FZeroLink: string;
    FId: Integer;
    FJobParams: TArray<TJobParam>;
  public
    function GetJobParamByLevel(aLevel: integer): TJobParam;
    constructor Create(aJobID: integer; aMySQLEngine: TMySQLEngine);
    property Id: integer read FId;
    property ZeroLink: string read FZeroLink;
    property JobParams: TArray<TJobParam> read FJobParams;
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
    FJob: TJob;
    FCurrLink: TCurrLink;
    procedure ProcessDOM(aDocument: IHTMLDocument2);
  public
    procedure StartJob(aJobID: integer);
  end;

  TParseTools = class
    class function ParseDOMByRule(aDocument: IHTMLDocument2; aRule: TJobRule): TArray<string>;
    class function ParseByRegEx(aPage:string; aRegEx:string): TArray<string>;
    class function ParseStrByRegEx(aPage:string; aRegEx:string): string;
    class function Explode(aIncome: string; aDelimiter: string): TArray<string>;
  end;

implementation

uses
   FireDAC.Comp.Client
  ,RegularExpressions
  ,Variants, unit1, Vcl.Controls, System.SysUtils;

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

class function TParseTools.ParseDOMByRule(aDocument: IHTMLDocument2; aRule: TJobRule): TArray<string>;

  procedure GetTagAndIndx(var aTag: string; out aIndex: integer);
    begin
      aIndex:=StrToIntDef(ParseStrByRegEx(aTag, '[\d+]'), 1);
      aTag:=ParseStrByRegEx(aTag, '[a-zA-Z]+');
    end;

  function GetChildCollection(aCollection: IHTMLElementCollection; aTag: string; aIndex: Integer): IHTMLElementCollection;
  var
    iElement: IHTMLElement;
    begin
      Result:=aCollection.tags(aTag) as IHTMLElementCollection;
      iElement:=Result.Item(aIndex-1, aIndex-1) as IHTMLElement;
      Result:=iElement.Children as IHTMLElementCollection;
    end;

    function GetElIndex(aHandledTree: TArray<TArray<integer>>; i: Integer): Integer;
      begin
        if Length(aHandledTree[i])=0 then Result:=0
        else Result:=aHandledTree[i][High(aHandledTree[i])];
      end;

var
  XPath: TArray<string>;
  ContCollection, Collection: IHTMLElementCollection;
  iElement: IHTMLElement;
  i, j, ElIndex: Integer;
  Tag: string;
  TagIndx, ContainerIndx: Integer;
  HandledTree: TArray<TArray<integer>>;
  TagList: TArray<string>;
  isProcessing: Boolean;
begin
  XPath:=TParseTools.Explode(aRule.XPath, '/');
  ContCollection:=aDocument.all as IHTMLElementCollection;
  ContainerIndx:=Length(XPath) - aRule.ContainerOffset;

  // �������� HTML ��������� � ���� ���������
  for i := 1 to ContainerIndx - 1 do
    begin
      Tag:=XPath[i];
      GetTagAndIndx(Tag, TagIndx);
      ContCollection:=GetChildCollection(ContCollection, Tag, TagIndx);
    end;

  // ������ ������ �����
  for i := ContainerIndx to Length(XPath)-1 do
    begin
      Tag:=XPath[i];
      GetTagAndIndx(Tag, TagIndx);
      TagList:=TagList+[Tag];
    end;

  // ����� ������ ������ ����������
  isProcessing:=True;
  SetLength(HandledTree, aRule.ContainerOffset);
  while isProcessing do
    begin
      Collection:=ContCollection;
      for i := 0 to Length(HandledTree)-1 do
        begin
          // �������� ������ � ������ � ����������� �� �������
          ElIndex:=GetElIndex(HandledTree, i);

          // �������� DOM �������
          Collection:=Collection.tags(TagList[i]) as IHTMLElementCollection;
          iElement:=Collection.Item(ElIndex, ElIndex) as IHTMLElement;

          if iElement<>nil then // DOM ������� ����������
            begin
              // ���� ����� �� ���, �� �������� ���������
              if i=Length(HandledTree)-1 then
                begin
                  Inc(ElIndex);
                  HandledTree[i]:=HandledTree[i] + [ElIndex];

                  if aRule.ObjType=[Link] then
                    Result:=Result+[iElement.getAttribute('href', 0)];
                end;

              Collection:=iElement.children as IHTMLElementCollection;
            end
          else // DOM ������� �� ����������
            begin
              if i=0 then isProcessing:=False
              else
                begin
                  //����������� ������ �� ������ ����
                  ElIndex:=GetElIndex(HandledTree, i-1);
                  Inc(ElIndex);
                  HandledTree[i-1]:=HandledTree[i-1] + [ElIndex];

                  //������ ������� �� ������ �������
                  for j := i to Length(HandledTree)-1 do SetLength(HandledTree[j], 0);
                end;

              Break;
            end;
        end;
    end;
end;

function TJob.GetJobParamByLevel(aLevel: integer): TJobParam;
var
  i: Integer;
begin
  for i:=0 to Length(FJobParams)-1 do
    if FJobParams[i].Level=aLevel  then
      begin
        Result:=FJobParams[i];
        Break;
      end;
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
  CurrentParam: TJobParam;
  i, j: integer;
  Rule: TJobRule;
  Records: TArray<string>;
begin
  CurrentParam:=FJob.GetJobParamByLevel(FCurrLink.Level);

  for i:=0 to Length(CurrentParam.Rules)-1 do
    begin
      Rule:=CurrentParam.Rules[i];

      Records:=TParseTools.ParseDOMByRule(aDocument, Rule);
      for j := 0 to Length(Records)-1 do
        begin
          if Rule.ObjType=[Link] then
            FParser.AddLink(Records[j], Rule.Level);
        end;
    end;
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
  dsJob, dsJobParams, dsJobRule: TFDQuery;
  JobParam: TJobParam;
  JobRule: TJobRule;
begin
  dsJob:=TFDQuery.Create(nil);
  dsJobParams:=TFDQuery.Create(nil);
  dsJobRule:=TFDQuery.Create(nil);
  try
    dsJob.SQL.Text:='select * from jobs where id=:JobID';
    dsJob.ParamByName('JobID').AsInteger:=aJobID;
    aMySQLEngine.OpenQuery(dsJob);

    FId:=dsJob.FieldByName('Id').AsInteger;
    FZeroLink:=dsJob.FieldByName('zero_link').AsString;

    //JobParams
    dsJobParams.SQL.Text:='select * from job_params where job_id=:JobID order by level';
    dsJobParams.ParamByName('JobID').AsInteger:=aJobID;
    aMySQLEngine.OpenQuery(dsJobParams);

    while not dsJobParams.Eof do
      begin
        JobParam.level:=dsJobParams.FieldByName('level').AsInteger;

        //JobRule
        dsJobRule.SQL.Text:='select * from job_param_rules where job_param_id=:ParamID order by level';
        dsJobRule.ParamByName('ParamID').AsInteger:=dsJobParams.FieldByName('Id').AsInteger;
        aMySQLEngine.OpenQuery(dsJobRule);

        while not dsJobRule.EOF do
          begin
            case dsJobRule.FieldByName('obj_type').AsInteger of
              1: JobRule.ObjType:=[Link];
              2: JobRule.ObjType:=[Text];
            end;
            JobRule.Level:=dsJobRule.FieldByName('level').AsInteger;
            JobRule.XPath:=dsJobRule.FieldByName('xpath').AsString;
            JobRule.ContainerOffset:=dsJobRule.FieldByName('container_offset').AsInteger;
            JobRule.RegExp:=dsJobRule.FieldByName('regexp').AsString;
            JobParam.Rules:=JobParam.Rules+[JobRule];
            dsJobRule.Next;
          end;

        FJobParams:=FJobParams + [JobParam];
        dsJobParams.Next;
      end;
  finally
    dsJob.Free;
    dsJobParams.Free;
    dsJobRule.Free;
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
begin
  MySQLEngine:=TMySQLEngine.Create;
  MySQLEngine.OpenConnection('MySQL.ini');
  FJob:=TJob.Create(aJobID, MySQLEngine);
  FParser:=TParser.Create(FJob, MySQLEngine);

  FCurrLink:=FParser.GetCurrLink;
  FParser.GetDocumentByLink(FCurrLink, ProcessDOM);
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
