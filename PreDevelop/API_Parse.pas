unit API_Parse;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  ,IdHTTP
  ,IdSSLOpenSSL
  ,API_MVC
  ,API_DBases
  ,API_Threads, Vcl.Grids, Vcl.StdCtrls;

type
  TParser = class
  private
    FMySQLEngine: TMySQLEngine;
    FHTTP: TIdHTTP;
    FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
    FParserID: integer;
    FParserName: string;
    function GetParserId: Integer;
    procedure HTTPInit;
    procedure SetLinkHandledValue(aLinkId, aValue: integer);
  public
    constructor Create(aDBEngine: TMySQLEngine; aParserName: string); overload;
    constructor Create; overload;
    destructor Destroy; override;
    function GetHTMLByLinkId(aLinkId: integer): string;
    function PostHTMLByLinkId(aLinkId: integer): string;
    function GetHTMLByURL(aURL: string; aPostData: TStringList = nil): string;
    function GetLinkLevel(aLinkId: integer): integer;
    function AddLink(aLevel: integer; aLink: String; aParentLinkID: Integer; aParentNum: integer; aIsPost: Boolean = False): integer;
    function GetLinkToProcess(out aLinkId: Integer; out alevel: Integer; out aIsPost: Boolean; out aLinkCount: Integer; out aHandledCount: integer; aThreadNum: Integer=1): Boolean;
    function GetLinkById(aLinkId: Integer): string;
    function GetValueByKey(aLinkId: Integer; aKey: string; out alevel: integer; aNum: integer = 1): String;
    function GetArrayByKey(aLinkId: Integer; aKey: string; aNum: integer = 1): TArray<string>;
    function GetRedirectedUrl(aURL: string): String;
    function GetLinkParentNum(aLinkId: integer): integer;
    procedure AddData(aLinkId, aRecordNum: Integer; aKey, aValue: string);
    procedure ParserInit;
    procedure WriteErrorLog(aLinkID: integer; aFunction, aIParams, aEMessage: string);
  end;

  TParseTool = class
  public
    class function MultiParseByKey(aPage:string; aFirstKey:string; aLastKey:string): TArray<string>;
    {class function ParseByKey(aPage:string; aFirstKey:string; aLastKey:string; aFirstKeyCount:integer = 0): string;}
    class function ParseByKeyReverse(aPage: string; aFirstKey: string; aLastKey: string; aFirstKeyCount: Integer): string;
    class function Explode(aIncome: string; aDelimiter: string): TArray<string>;
    class function Inplode(aIncome: TArray<string>; aDelimiter: string; aFirstIndex: integer=0; aLastIndex: integer=0): string;
    {class function RemoveTags(aIncome: string): string;}
    class function CutBetween(aIncome, aFirstKey, aLastKey: string): string;
    class function TranslitRus2Lat(const Str: string): string;
  end;

  TParseMethod = procedure(aLinkId: integer; aPage: string) of object;

  TModelParse = class abstract(TModelThread)
  private
    procedure SetZeroAndStartLinks;
  protected
    FParseMethod: TParseMethod;
    FParser: TParser;
    FLevelForCustomerAddRecord: Integer;
    procedure InitParserData; virtual; abstract;
    procedure DeinitParserData; virtual;
    procedure SetParseMethods(aLevel: Integer); virtual; abstract;
    procedure SetStartLinks(aLinkID: integer); virtual; abstract;
    procedure CustomerTableAddRecord(aLinkId: integer); virtual;
  public
    procedure ModelInitForThreads; override;
    procedure Execute; override;
  end;

  TParserInfo = record
    Name: string;
    Num: Integer;
    StartMessage: string;
  end;

  TViewParse = class(TViewAbstract)
    ParsersGrid: TStringGrid;
    btnStop: TButton;
    btnStart: TButton;
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    procedure InitParserGrid;
  protected
    FParsersList: TArray<TParserInfo>;
    procedure SetParsers; virtual; abstract;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure UpdateCountersInGrid(aParserNum, aLinkCount, aHandledCount: integer);
    property ParsersList: TArray<TParserInfo> read FParsersList;
  end;

  TParserStateModel = class(TModelAbstract)
  public
    procedure Execute; override;
  end;


implementation

{$R *.dfm}

uses
   System.StrUtils
  ,System.Threading
  ,API_Files
  ,FireDAC.Comp.Client
  ,FireDAC.Stan.Param
  ,Data.DB;

function TParser.PostHTMLByLinkId(aLinkId: Integer): string;
var
  Link, tx: string;
  Rows: TArray<string>;
  Row: string;
  PostData: TStringList;
begin
  tx:=GetLinkById(aLinkId);

  Link:=TParseTool.ParseByKey(tx, '', '?');
  tx:=TParseTool.ParseByKey(tx, '?', '');

  Rows:=TParseTool.Explode(tx, '&');
  PostData:=TStringList.Create;
  try
    for Row in Rows do
      begin
        PostData.Add(Row);
      end;

    Result := GetHTMLByURL(Link, PostData);
  finally
    PostData.Free;
  end;

  if Result='HTTP_READ_ERROR' then SetLinkHandledValue(aLinkId, -1)
  else SetLinkHandledValue(aLinkId, 2);
end;

function TParser.GetLinkParentNum(aLinkId: integer): integer;
var
  sql: string;
  dsQuery: TFDQuery;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    sql:='select parent_num from links where id=:id';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('id').AsInteger:=aLinkId;
    FMySQLEngine.OpenQuery(dsQuery);
    Result:=dsQuery.FieldByName('parent_num').AsInteger;
  finally
    dsQuery.Free;
  end;
end;

class function TParseTool.TranslitRus2Lat(const Str: string): string;
const
  RArrayL = 'абвгдеЄжзийклмнопрстуфхцчшщьыъэю€';
  RArrayU = 'јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’÷„Ўў№џЏЁёя';
  colChar = 33;
  arr: array[1..2, 1..ColChar] of string =
  (('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y',
    'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f',
    'kh', 'ts', 'ch', 'sh', 'shch', '''', 'y', '''', 'e', 'yu', 'ya'),
    ('A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'Y',
    'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F',
    'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '''', 'Y', '''', 'E', 'Yu', 'Ya'));
var
  i: Integer;
  LenS: Integer;
  p: integer;
  d: byte;
begin
  result := '';
  LenS := length(str);
  for i := 1 to lenS do
  begin
    d := 1;
    p := pos(str[i], RArrayL);
    if p = 0 then
    begin
      p := pos(str[i], RArrayU);
      d := 2
    end;
    if p <> 0 then
      result := result + arr[d, p]
    else
      result := result + str[i]; //если не русска€ буква, то берем исходную
  end;
end;

procedure TModelParse.DeinitParserData;
begin
end;

procedure TViewParse.UpdateCountersInGrid(aParserNum, aLinkCount, aHandledCount: integer);
begin
  ParsersGrid.Cells[2, aParserNum] :=  IntToStr(aLinkCount);
  ParsersGrid.Cells[3, aParserNum] :=  IntToStr(aHandledCount);
end;

procedure TParserStateModel.Execute;
var
  Parcer: TParser;
  LinkId, Level, LinkCount, HandledCount: Integer;
  IsPost: Boolean;
begin
  Parcer := TParser.Create(TMySQLEngine(FDBEngine), FData.Items['ParserName']);
  try
    Parcer.GetLinkToProcess(LinkId, Level, IsPost, LinkCount, HandledCount);
    Self.FEventData.AddOrSetValue('LinkCount', LinkCount);
    Self.FEventData.AddOrSetValue('HandledCount', HandledCount);
    Self.FEventData.AddOrSetValue('ParserNum', FData.Items['ParserNum']);
    Self.GenerateEvent('UpdateGrid');
  finally
    Parcer.Free;
  end;
end;

procedure TViewParse.InitParserGrid;
var
  Parser: TParserInfo;
  i: Integer;
begin
  ParsersGrid.Cells[0,0]:='parser';
  ParsersGrid.Cells[1,0]:='status';
  ParsersGrid.Cells[2,0]:='links count';
  ParsersGrid.Cells[3,0]:='handled links';
  ParsersGrid.Cells[4,0]:='';

  i:=0;
  for Parser in FParsersList do
    begin
      Inc(i);
      if i>1 then ParsersGrid.RowCount:=ParsersGrid.RowCount+1;
      ParsersGrid.Cells[0, Parser.Num] := Parser.Name;
      ParsersGrid.Cells[1, Parser.Num] := 'not working';
    end;
end;

procedure TModelParse.CustomerTableAddRecord(aLinkId: integer);
begin
end;

class function TParseTool.CutBetween(aIncome, aFirstKey, aLastKey: string): string;
var
  FirstIndx, LastIndx, CutLength: Integer;
begin
  FirstIndx:=Pos(aFirstKey, aIncome);

  while FirstIndx>0 do
    begin

      LastIndx:=PosEx(aLastKey, aIncome, FirstIndx);
      if LastIndx>0 then
        CutLength:=LastIndx-FirstIndx+Length(aLastKey)
      else Break;

      Delete(aIncome, FirstIndx, CutLength);
      FirstIndx:=Pos(aFirstKey, aIncome);
    end;

  Result:=aIncome;
end;

procedure TModelParse.SetZeroAndStartLinks;
var
  LinkId: Integer;
begin
  LinkId:=FParser.AddLink(0, FData.Items['ZeroLink'], 0, 0);
  FDBEngine.SetData('update links set handled=2 where id='+LinkId.ToString);
  SetStartLinks(LinkId);
end;

procedure TModelParse.ModelInitForThreads;
begin
  InitParserData;
  DeinitParserData;
  FParser := TParser.Create(TMySQLEngine(FDBEngine), FData.Items['ParserName']);
  try
    FParser.ParserInit;
  finally
    FParser.Free;
  end;
end;

procedure TModelParse.Execute;
var
  Level: Integer;
  LinkId: Integer;
  LinkCount: Integer;
  HandledCount: Integer;
  Page: string;
  i: Integer;
  isPost: Boolean;
  isProcessing: Boolean;
begin
  InitParserData;
  i:=0;
  FParser := TParser.Create(TMySQLEngine(FDBEngine), FData.Items['ParserName']);

  try
    isProcessing := True;
    while isProcessing do
      begin
        // запрос на LinkId выполн€ем в главном потоке, чтобы дочерние потоки не хватали одну ссылку одновременно
        TThread.Synchronize(nil,
          procedure()
          begin
            isProcessing := FParser.GetLinkToProcess(LinkId, Level, isPost, LinkCount, HandledCount, FThreadNum)
          end
        );

        inc(i);

        if LinkCount>0 then
          begin
            FEventData.AddOrSetValue('LinkCount',IntToStr(LinkCount));
            FEventData.AddOrSetValue('HandledCount',IntToStr(HandledCount));
            FEventData.AddOrSetValue('ParserNum', FData.Items['ParserNum']);
            Self.GenerateEvent('UpdateGrid');
          end;

        if Level=0 then SetZeroAndStartLinks
        else
          begin
            FParseMethod:=nil;
            SetParseMethods(Level);
          end;

        if (LinkId>0) and (Level>0) then
          begin
            if isPost then Page:=FParser.PostHTMLByLinkId(LinkId)
            else Page:=FParser.GetHTMLByLinkId(LinkId);
            FParseMethod(LinkId, Page);

            if FLevelForCustomerAddRecord=level then
              try
                CustomerTableAddRecord(LinkId);
              except
                On E : Exception do
                  begin
                    // log
                    FParser.WriteErrorLog(LinkId, 'CustomerTableAddRecord', '', E.Message);
                  end;
              end;
          end;
      end;
  finally
    FParser.Free;
    DeinitParserData;
  end;
end;

procedure TParser.WriteErrorLog(aLinkID: integer; aFunction, aIParams, aEMessage: string);
var
  sql: string;
  dsQuery: TFDQuery;
begin
  sql:='insert into errors set';
  sql:=sql+' e_datetime=now()';
  sql:=sql+',project_id=:project_id';
  sql:=sql+',link_id=:link_id';
  sql:=sql+',function=:function';
  sql:=sql+',input_params=:input_params';
  sql:=sql+',e_message=:e_message';

  dsQuery:=TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('project_id').AsInteger:=FParserID;

    if aLinkID=0 then dsQuery.ParamByName('link_id').Clear
    else dsQuery.ParamByName('link_id').AsInteger:=aLinkID;
    dsQuery.ParamByName('link_id').DataType:=ftInteger;

    dsQuery.ParamByName('function').AsString:=aFunction;
    dsQuery.ParamByName('input_params').AsString:=aIParams;
    dsQuery.ParamByName('e_message').AsString:=aEMessage;

    FMySQLEngine.ExecQuery(dsQuery);
  finally
    dsQuery.Free;
  end;

end;

function TParser.GetParserId: Integer;
var
  sql: string;
  dsQuery: TFDQuery;
begin
  Result:=-1;
  sql:='select id from projects where name='+FMySQLEngine.StrToSQL(FParserName);
  dsQuery:=TFDQuery.Create(nil);
  try
    FMySQLEngine.GetData(dsQuery,sql);
    if not dsQuery.IsEmpty then
      Result:=dsQuery.FieldByName('id').AsInteger
  finally
    dsQuery.Free;
  end;
end;

procedure TParser.ParserInit;
var
  sql: string;
  dsQuery: TFDQuery;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    if FParserID=-1 then
      begin
        FMySQLEngine.SetData('insert into projects set name='+FMySQLEngine.StrToSQL(FParserName));
        FParserID:=FMySQLEngine.GetLastInsertedId;
      end;

    sql:=Format('update links set handled=NULL where project_id=%d and handled=1', [FParserID]);
    FMySQLEngine.SetData(sql);
  finally
    dsQuery.Free
  end;
end;

procedure TParser.SetLinkHandledValue(aLinkId, aValue: integer);
var
  dsQuery: TFDQuery;
  sql: string;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    sql:='update links set handled=:value where id=:id';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('id').AsInteger:=aLinkId;
    dsQuery.ParamByName('value').AsInteger:=aValue;
    FMySQLEngine.ExecQuery(dsQuery);
  finally
    dsQuery.Free;
  end;
end;

procedure TParser.HTTPInit;
begin
  if Assigned(FHTTP) then FreeAndNil(FHTTP);
  if Assigned(FIdSSLIOHandlerSocketOpenSSL) then FreeAndNil(FIdSSLIOHandlerSocketOpenSSL);

  FHTTP := TIdHTTP.Create;
  FHTTP.HandleRedirects:=True;
  FHTTP.Request.UserAgent:='Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.62 Safari/537.36';

  FIdSSLIOHandlerSocketOpenSSL:=TIdSSLIOHandlerSocketOpenSSL.Create;
  FHTTP.IOHandler:=FIdSSLIOHandlerSocketOpenSSL;
end;

{function TParseTool.RemoveTags(aIncome: string): string;
var
tx, tag:string;
i,t: integer;
begin
  t:=0;
  tag:='';
  tx:=aIncome;
  for i:=1 to length(aIncome) do
    begin
      if aIncome[i]='<' then t:=1;
      if t=1 then tag:=tag+aIncome[i];
      if aIncome[i]='>' then
        begin
          tx:=StringReplace(tx,tag,'',[rfReplaceAll, rfIgnoreCase]);
          t:=0;
          tag:='';
        end;
    end;
  while Pos(#$A+#$A, tx)>0 do tx:=StringReplace(tx,#$A+#$A,#$A,[rfReplaceAll, rfIgnoreCase]);
  Result:=tx;
end;}

function TParser.GetHTMLByURL(aURL: string; aPostData: TStringList = nil): string;
var
  i: Integer;
begin
  i:=0;
  while i<11 do
    begin
      try
        Inc(i);
        if aPostData=nil then
          Result := FHTTP.get(aURL)
        else
          Result := FHTTP.Post(aURL, aPostData);
        Exit(Result);
      except
        On E : Exception do
          begin
            // log
            WriteErrorLog(0, 'GetHTMLByURL', aURL, E.Message);

            HTTPInit;
          end;
      end;
    end;
  Result:='HTTP_READ_ERROR';
end;

function TParser.GetRedirectedUrl(aURL: string): String;
begin
  Result:='';
  FHTTP.HandleRedirects:=False;

  try
    FHTTP.Get(aURL);
  except
    on E: EIdHTTPProtocolException do
      Result := FHTTP.Response.Location;
  end;

  FHTTP.HandleRedirects:=True;
end;

class function TParseTool.Inplode(aIncome: TArray<string>; aDelimiter: string; aFirstIndex: integer=0; aLastIndex: integer=0): string;
var
  i:integer;
  tx:string;
begin
  Result:='';
  if aLastIndex=0 then aLastIndex:=Length(aIncome);

  for i:=aFirstIndex to aLastIndex-1 do
    begin
      if i>aFirstIndex then Result:=Result+aDelimiter;
      Result:=Result+aIncome[i];
    end;
end;

function TParser.GetArrayByKey(aLinkId: Integer; aKey: string; aNum: integer = 1): TArray<string>;
var
  sql: string;
  dsQuery: TFDQuery;
  LinkId: integer;
  Num: Integer;
  IsProcess: Boolean;
  IsSkip: Boolean;
begin
  Result:=[];
  LinkId:=aLinkId;
  Num:=aNum;
  IsProcess:=True;
  IsSkip:=False;

  dsQuery:=TFDQuery.Create(nil);
  try
    while IsProcess do
      begin
        if not IsSkip then
          begin
            sql:='select `value`, level from `records`';
            sql:=sql+' join links on links.id=`records`.link_id';
            sql:=sql+' where link_id=:link_id and `key`=:key and num=:num';
            dsQuery.SQL.Text:=sql;
            dsQuery.ParamByName('key').AsString:=aKey;
            dsQuery.ParamByName('link_id').AsInteger:=LinkId;
            dsQuery.ParamByName('num').AsInteger:=Num;
            FMySQLEngine.OpenQuery(dsQuery);
            dsQuery.FetchAll;
            while not dsQuery.Eof do
              begin
                Result:=Result+[dsQuery.FieldByName('value').AsString];
                dsQuery.Next;
              end;
          end;

        if (dsQuery.RecordCount>0) and not IsSkip then IsProcess:=False
        else
          begin
            sql:='select l2.id, l1.parent_num, l1.`level` as lv1, l2.`level` as lv2 from links l1';
            sql:=sql+' join links l2 on l2.id=l1.parent_link_id';
            sql:=sql+' where l1.id=:id';
            dsQuery.SQL.Text:=sql;
            dsQuery.ParamByName('id').AsInteger:=LinkId;
            FMySQLEngine.OpenQuery(dsQuery);

            if dsQuery.FieldByName('id').AsInteger>0 then
              begin
                LinkId:=dsQuery.FieldByName('id').AsInteger;
                Num:=dsQuery.FieldByName('parent_num').AsInteger;
                if dsQuery.FieldByName('lv1').AsInteger=dsQuery.FieldByName('lv2').AsInteger
                then
                     IsSkip:=True
                else IsSkip:=False;
              end
            else IsProcess:=False;
          end;
      end;
  finally
    dsQuery.Free;
  end;
end;

class function TParseTool.ParseByKeyReverse(aPage: string; aFirstKey: string; aLastKey: string; aFirstKeyCount: Integer): string;
var
  i: integer;
begin
  if Pos(aFirstKey, aPage)>0 then
    begin
      for i:=0 to aFirstKeyCount do
        begin
          Delete(aPage, Pos(aFirstKey, aPage), Length(aPage));
        end;
      for i:=Length(aPage)-1 downto 0 do
        begin
          if Copy(aPage, i, Length(aLastKey))=aLastKey then break;
        end;
      if i>0 then
        Delete(aPage, 1, i+Length(aLastKey)-1)
      else aPage:='';
      Result:=Trim(aPage);
    end
  else  Result:='';
end;

function TParser.GetValueByKey(aLinkId: Integer; aKey: string; out alevel: integer; aNum: integer = 1): String;
var
  sql: string;
  dsQuery: TFDQuery;
  LinkId: integer;
  Num: Integer;
  IsProcess: Boolean;
  IsSkip: Boolean;
begin
  Result:='';
  LinkId:=aLinkId;
  Num:=aNum;
  IsProcess:=True;
  IsSkip:=False;

  dsQuery:=TFDQuery.Create(nil);
  try
    while IsProcess do
      begin
        if not IsSkip then
          begin
            sql:='select `value`, level from `records`';
            sql:=sql+' join links on links.id=`records`.link_id';
            sql:=sql+' where link_id=:link_id and `key`=:key and num=:num';
            dsQuery.SQL.Text:=sql;
            dsQuery.ParamByName('key').AsString:=aKey;
            dsQuery.ParamByName('link_id').AsInteger:=LinkId;
            dsQuery.ParamByName('num').AsInteger:=Num;
            FMySQLEngine.OpenQuery(dsQuery);

            if not dsQuery.FieldByName('value').IsNull then
              begin
                Result:=dsQuery.FieldByName('value').AsString;
                alevel:=dsQuery.FieldByName('level').AsInteger;
              end;
          end;

        if (dsQuery.RecordCount>0) and not IsSkip then IsProcess:=False
        else
          begin
            sql:='select l2.id, l1.parent_num, l1.`level` as lv1, l2.`level` as lv2 from links l1';
            sql:=sql+' join links l2 on l2.id=l1.parent_link_id';
            sql:=sql+' where l1.id=:id';
            dsQuery.SQL.Text:=sql;
            dsQuery.ParamByName('id').AsInteger:=LinkId;
            FMySQLEngine.OpenQuery(dsQuery);

            if dsQuery.FieldByName('id').AsInteger>0 then
              begin
                LinkId:=dsQuery.FieldByName('id').AsInteger;
                Num:=dsQuery.FieldByName('parent_num').AsInteger;

                if dsQuery.FieldByName('lv1').AsInteger=dsQuery.FieldByName('lv2').AsInteger
                then
                     IsSkip:=True
                else IsSkip:=False;
              end
            else IsProcess:=False;
          end;
      end;
  finally
    dsQuery.Free;
  end;
end;

function TParser.GetLinkById(aLinkId: Integer): string;
var
  sql: string;
  dsQuery: TFDQuery;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    sql:='select link from links where id=:id';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('id').AsInteger:=aLinkId;
    FMySQLEngine.OpenQuery(dsQuery);
    Result:=dsQuery.FieldByName('link').AsString;
  finally
    dsQuery.Free;
  end;
end;

function TParser.GetLinkToProcess(out aLinkId: Integer; out alevel: Integer; out aIsPost: Boolean; out aLinkCount: Integer; out aHandledCount: integer; aThreadNum: Integer=1): Boolean;
var
  sql: string;
  dsQuery: TFDQuery;
  UnHandledCount: integer;
begin
  dsQuery:=TFDQuery.Create(nil);
  try
    if aThreadNum = 1 then
      begin
        sql:='select count(*) as link_count from links where project_id=:project_id';
        dsQuery.SQL.Text:=sql;
        dsQuery.ParamByName('project_id').AsInteger:=FParserID;
        FMySQLEngine.OpenQuery(dsQuery);
        aLinkCount:=dsQuery.FieldByName('link_count').AsInteger;

        sql:='select count(*) as unhandled_count from links where handled is null';
        sql:=sql+' and project_id=:project_id';
        dsQuery.SQL.Text:=sql;
        dsQuery.ParamByName('project_id').AsInteger:=FParserID;
        FMySQLEngine.OpenQuery(dsQuery);
        UnHandledCount:=dsQuery.FieldByName('unhandled_count').AsInteger;

        aHandledCount:=aLinkCount-UnHandledCount;
      end;

    if    (aLinkCount>1)
      and (UnHandledCount=0)
    then Result:=False
    else
      begin
        Result:=True;

        sql:='select id, level, is_post from links';
        sql:=sql+' where project_id=:project_id';
        sql:=sql+' and handled is null';
        sql:=sql+' order by level desc, id';
        sql:=sql+' limit :limit, 1';

        dsQuery.SQL.Text:=sql;
        dsQuery.ParamByName('project_id').AsInteger:=FParserID;
        dsQuery.ParamByName('limit').AsInteger:=aThreadNum-1;
        FMySQLEngine.OpenQuery(dsQuery);

        aLinkId:=dsQuery.FieldByName('id').AsInteger;
        alevel:=dsQuery.FieldByName('level').AsInteger;
        aIsPost:=Boolean(dsQuery.FieldByName('is_post').AsInteger);

        if (dsQuery.IsEmpty) and (aThreadNum>1) then  alevel := -1; // дл€ потока нет доступной ссылки, пропускаем итерацию

        SetLinkHandledValue(aLinkId, 1);
      end;
  finally
    dsQuery.Free;
  end;
end;

function TParser.AddLink(aLevel: integer; aLink: String; aParentLinkID: Integer; aParentNum: integer; aIsPost: Boolean = False): integer;
var
  sql: string;
  dsQuery: TFDQuery;
begin
  Result:=0;

  dsQuery:=TFDQuery.Create(nil);
  try
    sql:='select * from links where link_hash=md5(:link) and project_id=:project_id';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('project_id').AsInteger:=FParserID;
    dsQuery.ParamByName('link').AsString:=aLink;
    FMySQLEngine.OpenQuery(dsQuery);
    if not dsQuery.IsEmpty then exit;

    sql:='insert into links set';
    sql:=sql+' project_id=:project_id';
    sql:=sql+',level=:level';
    sql:=sql+',link=:link';
    sql:=sql+',parent_link_id=:parent_link_id';
    sql:=sql+',parent_num=:parent_num';
    sql:=sql+',link_hash=md5(:link)';
    sql:=sql+',is_post=:is_post';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('project_id').AsInteger:=FParserID;
    dsQuery.ParamByName('level').AsInteger:=aLevel;
    dsQuery.ParamByName('link').AsString:=aLink;

    dsQuery.ParamByName('parent_link_id').DataType:=ftInteger;
    if aParentLinkID>0 then dsQuery.ParamByName('parent_link_id').AsInteger:=aParentLinkID
    else dsQuery.ParamByName('parent_link_id').Clear;

    dsQuery.ParamByName('parent_num').DataType:=ftInteger;
    if aParentNum>0 then dsQuery.ParamByName('parent_num').AsInteger:=aParentNum
    else dsQuery.ParamByName('parent_num').Clear;

    dsQuery.ParamByName('is_post').DataType:=ftInteger;
    if aIsPost then dsQuery.ParamByName('is_post').AsInteger:=1
    else dsQuery.ParamByName('is_post').Clear;

    try
      FMySQLEngine.ExecQuery(dsQuery);
    except
      On E : Exception do
        begin
          // log
          WriteErrorLog(aParentLinkID, 'AddLink', aLink, E.Message);
          Exit;
        end;
    end;

    Result:=FMySQLEngine.GetLastInsertedId;
  finally
    dsQuery.Free;
  end;
end;

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

    try
      FMySQLEngine.ExecQuery(dsQuery);
    except
      On E : Exception do
        begin
          // log
          WriteErrorLog(aLinkId, 'AddData', aKey+': '+aValue, E.Message);
          Exit;
        end;
    end;
  finally
    dsQuery.Free;
  end;
end;

function TParser.GetLinkLevel(aLinkID: Integer): integer;
var
  dsQuery: TFDQuery;
  sql: string;
begin
  Result:=-1;
  dsQuery:=TFDQuery.Create(nil);
  try
    sql:='select level from links where project_id=:project_id and id=:id';
    dsQuery.SQL.Text:=sql;
    dsQuery.ParamByName('project_id').AsInteger:=FParserID;
    dsQuery.ParamByName('id').AsInteger:=aLinkID;
    FMySQLEngine.OpenQuery(dsQuery);
    Result:=dsQuery.FieldByName('level').AsInteger;
  finally
    dsQuery.Free;
  end;
end;

{class function TParseTool.ParseByKey(aPage:string; aFirstKey:string; aLastKey:string; aFirstKeyCount:integer = 0): string;
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
end;}

class function TParseTool.MultiParseByKey(aPage: string; aFirstKey: string; aLastKey: string): TArray<string>;
var
  RowData: TArray<string>;
  RowNum: integer;
  tx: string;
begin
  RowNum:=0;
  while Pos(aFirstKey, aPage)>0 do
    begin
      Inc(RowNum);
      SetLength(RowData, RowNum);
      tx:=Copy(aPage, Pos(aFirstKey, aPage) + Length(aFirstKey), Length(aPage));
      Delete(tx, Pos(aLastKey, tx), Length(tx));
      RowData[RowNum-1]:=Trim(tx);

      Delete(aPage, 1, Pos(aFirstKey, aPage) + Length(aFirstKey));
      Delete(aPage, 1, Pos(aLastKey, aPage));
    end;
  Result:=RowData;
end;

constructor TParser.Create;
begin
  inherited Create;
  HTTPInit;
end;

constructor TParser.Create(aDBEngine: TMySQLEngine; aParserName: string);
var
  sql: string;
  dsQuery: TFDQuery;
begin
  inherited Create;

  HTTPInit;
  FMySQLEngine:=aDBEngine;
  FParserName:=aParserName;
  FParserID:=Self.GetParserId;
end;

destructor TParser.Destroy;
begin
  FHTTP.Free;
  FIdSSLIOHandlerSocketOpenSSL.Free;
  inherited;
end;

function TParser.GetHTMLByLinkId(aLinkId: integer): string;
var
  Link: string;
begin
  Link:=GetLinkById(aLinkId);

  Result := GetHTMLByURL(Link);

  if Result='HTTP_READ_ERROR' then SetLinkHandledValue(aLinkId, -1)
  else SetLinkHandledValue(aLinkId, 2);
end;

class function TParseTool.Explode(aIncome: string; aDelimiter: string): TArray<string>;
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
procedure TViewParse.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled:=False;
  btnStop.Enabled:=True;
  ParsersGrid.Cells[1, ParsersGrid.Row]:='working';
  SendViewMessage(FParsersList[ParsersGrid.Row-1].StartMessage);
end;

constructor TViewParse.Create(AOwner: TComponent);
begin
  inherited;
  SetParsers;
  InitParserGrid;
end;

end.
