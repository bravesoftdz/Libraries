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
    Key: string;
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
    procedure WebBrowserInit;
    procedure WebBrowserDocumentComplete(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
  public
    constructor Create(aJob: TJob; aMySQLEngine: TMySQLEngine);
    function GetCurrLink: TCurrLink;
    function AddLink(aLink: string; aLevel: Integer): Integer;
    procedure AddData(aLinkId, aRecordNum: Integer; aKey, aValue: string);
    procedure SetLinkHandle(aLinkID: Integer; aValue: integer);
    procedure GetDocumentByLink(aCurrLink: TCurrLink; aCallBack: TProcessDOM);
    property ProcessDOM: TProcessDOM read FProcessDOM write FProcessDOM;
  end;

  TParserModel=class
  private
    FParser: TParser;
    FCurrLink: TCurrLink;
    procedure GetNextLink;
    procedure ProcessDOM(aDocument: IHTMLDocument2);
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

procedure TParser.AddData(aLinkId: Integer; aRecordNum: Integer; aKey: string; aValue: string);
var
  sql: string;
  dsQuery: TFDQuery;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    sql:='insert into records set';
    sql:=sql+' `link_id`=:link_id';
    sql:=sql+',`num`=:num';
    sql:=sql+',`key`=:key';
    sql:=sql+',`value`=:value';
    sql:=sql+',`value_hash`=md5(:value)';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('link_id').AsInteger:=aLinkId;
    dsQuery.ParamByName('num').AsInteger:=aRecordNum;
    dsQuery.ParamByName('key').AsString:=aKey;
    dsQuery.ParamByName('value').AsWideString:=aValue;

    FMySQLEngine.ExecQuery(dsQuery);
  finally
    dsQuery.Free;
  end;
end;

procedure TParserModel.GetNextLink;
begin
  FParser.SetLinkHandle(FCurrLink.Id, 2);
  FCurrLink:=FParser.GetCurrLink;
  FParser.GetDocumentByLink(FCurrLink, ProcessDOM);
end;

procedure TParser.SetLinkHandle(aLinkID: Integer; aValue: Integer);
var
  dsQuery: TFDQuery;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text:='update links set handled=:Value where id=:LinkID';
    dsQuery.ParamByName('Value').AsInteger:=aValue;
    dsQuery.ParamByName('LinkID').AsInteger:=aLinkID;
    FMySQLEngine.ExecQuery(dsQuery);
  finally
    dsQuery.Free;
  end;
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

  procedure FillResult(var aResult:TArray<string>; aRule: TJobRule; aElement: IHTMLElement);
    begin
      if aRule.ObjType=[Link] then
        aResult:=aResult+[aElement.getAttribute('href', 0)];
      if aRule.ObjType=[Text] then
        aResult:=aResult+[aElement.outerText];
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
  SetLength(Result, 0);
  XPath:=TParseTools.Explode(aRule.XPath, '/');
  ContCollection:=aDocument.all as IHTMLElementCollection;
  ContainerIndx:=Length(XPath) - aRule.ContainerOffset;
  if aRule.ContainerOffset=0 then Dec(ContainerIndx);

  // получить HTML контейнер в виде коллекции
  for i := 1 to ContainerIndx - 1 do
    begin
      Tag:=XPath[i];
      GetTagAndIndx(Tag, TagIndx);
      ContCollection:=GetChildCollection(ContCollection, Tag, TagIndx);
    end;

  // строим список тегов
  for i := ContainerIndx to Length(XPath)-1 do
    begin
      Tag:=XPath[i];
      GetTagAndIndx(Tag, TagIndx);
      TagList:=TagList+[Tag];
    end;

  // обход дерева внутри контейнера
  isProcessing:=True;
  SetLength(HandledTree, aRule.ContainerOffset);
  if aRule.ContainerOffset=0 then SetLength(HandledTree, 1);
  while isProcessing do
    begin
      Collection:=ContCollection;
      for i := 0 to Length(HandledTree)-1 do
        begin
          // получаем индекс в дереве в зависимости от глубины
          ElIndex:=GetElIndex(HandledTree, i);

          // получаем DOM элемент
          Collection:=Collection.tags(TagList[i]) as IHTMLElementCollection;
          iElement:=Collection.Item(ElIndex, ElIndex) as IHTMLElement;

          if iElement<>nil then // DOM элемент существует
            begin
              // если дошли до дна, то получаем результат
              if i=Length(HandledTree)-1 then
                begin
                  Inc(ElIndex);
                  HandledTree[i]:=HandledTree[i] + [ElIndex];

                  FillResult(Result, aRule, iElement);
                  if aRule.ContainerOffset=0 then isProcessing:=False
                end;

              Collection:=iElement.children as IHTMLElementCollection;
            end
          else // DOM элемент не существует
            begin
              if i=0 then isProcessing:=False
              else
                begin
                  //увеличиваем индекс на уровне выше
                  ElIndex:=GetElIndex(HandledTree, i-1);
                  Inc(ElIndex);
                  HandledTree[i-1]:=HandledTree[i-1] + [ElIndex];

                  //чистим индексы на нижних уровнях
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
          if Rule.ObjType=[Text] then
            FParser.AddData(FCurrLink.Id, j, Rule.Key, Records[j]);
        end;
    end;

  GetNextLink;
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
    dsJobParams.Close;
    dsJobParams.SQL.Text:='select * from job_params where job_id=:JobID order by level';
    dsJobParams.ParamByName('JobID').AsInteger:=aJobID;
    aMySQLEngine.OpenQuery(dsJobParams);

    while not dsJobParams.EOF do
      begin
        JobParam.level:=dsJobParams.FieldByName('level').AsInteger;

        //JobRule
        setLength(JobParam.Rules, 0);
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
            JobRule.Key:=dsJobRule.FieldByName('key').AsString;
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
    sql:=sql+' order by level desc, id';
    sql:=sql+' limit 1';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('JobID').AsInteger:=FJob.Id;
    FMySQLEngine.OpenQuery(dsQuery);

    Result.Id:=dsQuery.FieldByName('Id').AsInteger;
    Result.Link:=dsQuery.FieldByName('link').AsString;
    Result.Level:=dsQuery.FieldByName('level').AsInteger;

    SetLinkHandle(Result.Id, 1);
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

end.
