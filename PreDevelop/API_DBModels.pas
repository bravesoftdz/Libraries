unit API_DBModels;

interface

uses
   System.JSON
  ,System.Generics.Collections
  ,System.SysUtils
  ,Data.DB
  ,FireDAC.Stan.Param
  ,FireDAC.Comp.Client
  ,API_MVC
  ,API_Files
  ,API_DBases;

type
  // миграция данных
  TDataMigrate = class(TModelAbstract)
  private
    FSource: TDBEngine;
    FDestination: TDBEngine;
    FRulesFile: String;
    FPreScriptFile: String;
    FPostScriptFile: String;
    function SetQueries(SourceTable: string; jsnTableRule: TJSONObject; FieldCode: TObjectDictionary<String,TDictionary<string,string>>): TList<String>;
    function CheckAndGetDublicateID(TableName: string; params: TFDParams; FieldCode: TObjectDictionary<String,TDictionary<string,string>>): Integer;
    procedure GetValueForParam(Value: Variant; Param: TFDParam);
    procedure MoveRecord(dsSource, dsDestination: TFDQuery; Queries: TList<string>; FieldCode: TObjectDictionary<String,TDictionary<string,string>>);
    procedure InputDataParce; override;
    procedure ScriptsExcute(jsnScripts: TJSONObject; TableName: string);
  public
    procedure Excute; override;
  end;

implementation

procedure TDataMigrate.ScriptsExcute(jsnScripts: TJSONObject; TableName: string);
var
  sql: string;
  jsnQueries: TJSONArray;
  jsnQuery: TJSONValue;
begin
  if jsnScripts=nil then exit;

  jsnQueries:=jsnScripts.Values[TableName] as TJSONArray;
  if jsnQueries=nil then exit;
  for jsnQuery in jsnQueries do
    begin
      FDestination.SetData(jsnQuery.Value);
    end;
end;

function TDataMigrate.CheckAndGetDublicateID(TableName: string; Params: TFDParams; FieldCode: TObjectDictionary<String,TDictionary<string,string>>): Integer;
var
  dsDublicate: TFDQuery;
  sql, FieldName: string;
  i, j, pi: Integer;
  Pair: TPair<string,string>;
  CheckParams: TFDParams;
begin
  dsDublicate:=TFDQuery.Create(nil);
  dsDublicate.Connection:=Self.FDestination.Connection;
  CheckParams:=TFDParams.Create;
  try
    sql:='SELECT id FROM '+TableName+' WHERE ';
    pi:=0;
    for i := 0 to Params.Count-1 do
      begin
        if not Params.Items[i].IsNull then
          begin
            if pi>0 then sql:=sql+' AND ';
            FieldName:='';
            for Pair in FieldCode.Items[TableName] do
              if UpperCase(Pair.Value)=Params.Items[i].Name then FieldName:=Pair.Key;
            sql:=sql+FieldName+'=:'+Params.Items[i].Name;

            CheckParams.CreateParam(Params.Items[i].DataType, Params.Items[i].Name, Params.Items[i].ParamType);
            CheckParams.Items[CheckParams.Count-1].Value:=Params.Items[i].Value;

            inc(pi);
          end;
      end;
    dsDublicate.Close;
    dsDublicate.SQL.Text:=sql;
    dsDublicate.Params:=CheckParams;
    dsDublicate.Open;
    dsDublicate.FetchAll;
    Result:=dsDublicate.Fields[0].AsInteger;
  finally
    dsDublicate.Free;
    CheckParams.Free;
  end;
end;

procedure TDataMigrate.GetValueForParam(Value: Variant; Param: TFDParam);
var
  ParamType: TFieldType;
begin
  ParamType:=Param.DataType;
  if ParamType in [ftFloat, ftInteger, ftCurrency] then
    begin
      Param.AsFloat:=Value;
      Exit;
    end;
  if   (ParamType in [ftDate, ftDateTime])
    or (StrToDateDef(string(Value),0)>0)
  then
    begin
      Param.AsDate:=StrToDateDef(string(Value),0);
      Exit;
    end;
  if ParamType in [ftBoolean] then
    begin
      Param.AsInteger:=Integer(Value);
      Exit;
    end;
  Param.AsString:=string(Value);
end;

procedure TDataMigrate.MoveRecord(dsSource, dsDestination: TFDQuery; Queries: TList<string>; FieldCode: TObjectDictionary<String,TDictionary<string,string>>);
var
  i, j: Integer;
  TableName: string;
  SourceFieldName: string;
  ExtRecordIDs: TDictionary<string,integer>;
  DublicateID: integer;
begin
  ExtRecordIDs:=TDictionary<string,integer>.Create;
  try
    for i:=0 to Queries.Count-1 do
      begin
        // имя таблицы
        TableName:=Queries.Items[i].Split([#32])[2];

        // подключаем текст sql
        dsDestination.Close;
        dsDestination.SQL.Text:=Queries.Items[i];

        // перебираем параметры и заполняем их
        for j := 0 to dsDestination.ParamCount-1 do
          begin
            SourceFieldName:=dsDestination.Params.Items[j].Name;

            // вставляем значение
            if not SourceFieldName.Contains('.') then
              begin
                // тип параметра = тип поля источника
                dsDestination.Params.Items[j].DataType:=dsSource.FieldByName(SourceFieldName).DataType;

                if not dsSource.FieldByName(SourceFieldName).IsNull then
                  Self.GetValueForParam(dsSource.FieldByName(SourceFieldName).Value, dsDestination.Params.Items[j]);
              end
            else // вставляем ссылку на запись доп таблицы
              begin
                dsDestination.Params.Items[j].DataType:=ftInteger;
                dsDestination.Params.Items[j].AsInteger:=ExtRecordIDs.Items[LowerCase(SourceFieldName.Split(['.'])[0])];
              end;
          end;

        // проверяем на дубликат и вставляем запись
        DublicateID:=Self.CheckAndGetDublicateID(TableName, dsDestination.Params, FieldCode);
        if DublicateID=0 then
          begin
            dsDestination.ExecSQL;
            DublicateID:=Self.CheckAndGetDublicateID(TableName, dsDestination.Params, FieldCode);
          end;

        // если доп. таблицы записываем id вставленной записи
        if i<Queries.Count-1 then
          begin
            ExtRecordIDs.Add(TableName,Self.CheckAndGetDublicateID(TableName, dsDestination.Params, FieldCode));
          end;
      end;
  finally
    ExtRecordIDs.Free;
  end;
end;

function TDataMigrate.SetQueries(SourceTable: string; jsnTableRule: TJSONObject; FieldCode: TObjectDictionary<String,TDictionary<string,string>>): TList<String>;
var
  jsnFieldRule: TJSONPair;
  jsnDivide: TJSONArray;
  Key, Value, Table, Field: String;
  sql, SourceSQL:string;
  i: Integer;
begin
  FieldCode.Clear;

  for jsnFieldRule in jsnTableRule do
    begin
      // прямой перенос
      if not (jsnFieldRule.JsonValue is TJSONArray) then
        begin
          if not FieldCode.ContainsKey(SourceTable) then
            begin
              FieldCode.Add(SourceTable, TDictionary<string,string>.Create);
            end;

          Key:=jsnFieldRule.JsonValue.Value;
          Value:=jsnFieldRule.JsonString.Value;

          FieldCode.Items[SourceTable].Add(Key,Value);
        end;

      // перенос с разносом id и значения в другую таблицу
      if (jsnFieldRule.JsonValue is TJSONArray) then
        begin
          jsnDivide:=jsnFieldRule.JsonValue as TJSONArray;
          Table:=jsnDivide.Items[1].Value;
          if not FieldCode.ContainsKey(Table) then FieldCode.Add(Table, TDictionary<string,string>.Create);

          Key:=jsnDivide.Items[2].Value;
          Value:=jsnFieldRule.JsonString.Value;

          FieldCode.Items[Table].Add(Key,Value);
          if not FieldCode.Items[SourceTable].ContainsKey(jsnDivide.Items[0].Value) then
            begin
              Key:=jsnDivide.Items[0].Value;
              Value:=jsnDivide.Items[1].Value+'.'+jsnDivide.Items[3].Value;
              FieldCode.Items[SourceTable].Add(Key,Value);
            end;
        end;
    end;

  // формируем sql
  Result:=TList<string>.Create;
  for Table in FieldCode.Keys do
    begin
      sql:='INSERT INTO '+Table+' SET ';
      i:=0;
      for Field in FieldCode.Items[Table].Keys do
        begin
          if i>0 then sql:=sql+',';
          sql:=sql+Field+'=:'+FieldCode.Items[Table].Items[Field];
          inc(i);
        end;
      if Table=SourceTable then SourceSQL:=sql
      else Result.Add(sql);
    end;
  if not SourceSQL.IsEmpty then Result.Add(SourceSQL); // sql основной таблицы добавляем в конец
end;

procedure TDataMigrate.InputDataParce;
begin
  inherited;
  FSource:=FObjData.Items['Source'] as TDBEngine;
  FDestination:=FObjData.Items['Destination'] as TDBEngine;
  FRulesFile:=FData.Items['RulesFileName'];
  FPreScriptFile:=FData.Items['PreScriptFileName'];
  FPostScriptFile:=FData.Items['PostScriptFileName'];
end;

procedure TDataMigrate.Excute;
var
  jsnPreScripts: TJSONObject;
  jsnPostScripts: TJSONObject;
  jsnRules: TJSONObject;
  jsnTableRule: TJSONPair;
  dsSource: TFDQuery;
  dsDestination: TFDQuery;
  sql: String;
  FieldCode: TObjectDictionary<String,TDictionary<string,string>>;
  Queries: TList<String>;
  RecNum: integer;
begin
  FormatSettings.DecimalSeparator:='.';

  jsnPreScripts:=TJSONObject.ParseJSONValue(TFilesEngine.GetTextFromFile(FPreScriptFile)) as TJSONObject;
  jsnPostScripts:=TJSONObject.ParseJSONValue(TFilesEngine.GetTextFromFile(FPostScriptFile)) as TJSONObject;
  jsnRules:=TJSONObject.ParseJSONValue(TFilesEngine.GetTextFromFile(FRulesFile)) as TJSONObject;
  FieldCode:=TObjectDictionary<String,TDictionary<string,string>>.Create;
  try
    // выполнение скриптов препроцесса
    Self.ScriptsExcute(jsnPreScripts, 'common');

    for jsnTableRule in jsnRules do
      begin
        // событие
        FEventData.Clear;
        FEventData.Add('TableName',jsnTableRule.JsonString.Value);
        Self.GenerateEvent('SetTableName');

        dsSource:=TFDQuery.Create(nil);
        dsDestination:=TFDQuery.Create(nil);
        dsDestination.Connection:=Self.FDestination.Connection;
        try
          // получаем данные исходной таблицы
          sql:='SELECT * FROM '+jsnTableRule.JsonString.Value;
          FSource.GetData(dsSource, sql);

          // составляем sql запросы по правилам
          Queries:=Self.SetQueries(jsnTableRule.JsonString.Value, jsnTableRule.JsonValue as TJSONObject, FieldCode);

          // для каждой записи исходной таблицы применяем запросы
          dsSource.FetchAll;
          // событие
          FEventData.Clear;
          FEventData.Add('RecCount',dsSource.RecordCount);
          Self.GenerateEvent('SetRecCount');

          RecNum:=0;
          while not dsSource.Eof do
            begin
              // событие
              inc(RecNum);
              FEventData.Clear;
              FEventData.Add('RecNum',RecNum);
              Self.GenerateEvent('SetRecNum');
              Self.MoveRecord(dsSource, dsDestination, Queries, FieldCode);
              dsSource.Next;
            end;
        finally
          dsSource.Free;
          dsDestination.Free;
        end;
      end;

    // выполнение скриптов постпроцесса
    Self.ScriptsExcute(jsnPostScripts, 'common');
  finally
    jsnRules.Free;
    FieldCode.Free;
  end;
end;

end.
