unit API_ORM;

interface

uses
  System.Generics.Collections,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Data.DB,
  API_DB;

type
  TDBField = record
    FieldName: string;
    FieldType: TFieldType;
  end;

  TEntityAbstract = class abstract
  private
    function GetEntityRecordFromDB(aID: integer): TFDQuery;
    function CheckChanges(aFieldName: string; aCurrentRecord: TFDQuery): Boolean;
    function GetKeyValueString(aFields: TArray<string>): string;
    function GetFieldTypeByName(aFieldName: string): TFieldType;
    procedure FillEntity(aID: integer);
    procedure SetParams(aParams: TFDParams);
    procedure StoreToDB(aSQL: string);
    procedure InsertToDB(aChangedFields: TArray<string>);
    procedure UpdateToDB(aChangedFields: TArray<string>);
    function GetID: integer;
  protected
    FDBEngine: TDBEngine;
    FFields: TArray<TDBField>;
    FData: TDictionary<string, variant>;
    procedure InitFields; virtual; abstract;
    procedure AddField(aFieldName: string; aFieldType: TFieldType);
  public
    procedure SaveEntity;
    class function GetTableName: string; virtual; abstract;
    constructor Create(aDBEngine: TDBEngine; aID: integer = 0);
    destructor Destroy; override;
    property ID: integer read GetID;
    property Data: TDictionary<string, variant> read FData;
    property Fields: TArray<TDBField> read FFields;
  end;

  TEntityAbstractClass = class of TEntityAbstract;

  TEntityList<T: TEntityAbstract> = class abstract(TObjectList<T>)
  private
    FDBEngine: TDBEngine;
    FEntityClass: TEntityAbstractClass;
    function GetWherePart(aFilters: TArray<string>): string;
    function GetOrderPart(aOrder: TArray<string>): string;
    procedure FillEntityList(aFilters, aOrder: TArray<string>);
  public
    procedure DoSmth;
    constructor Create(aDBEngine: TDBEngine; aFilters, aOrder: TArray<string>); overload;
    constructor Create(aDBEngine: TDBEngine; aKeyField: string; aKeyValue: integer); overload;
    property EntityClass: TEntityAbstractClass read FEntityClass;
  end;

  TListInfo = record
    ListPointer: Pointer;
    EntityClass: TEntityAbstractClass;
    MasterKeyField: string;
    SlaveKeyField: string;
  end;

  TEntity = class abstract(TEntityAbstract)
  private
    function GetListInfo(aEntityClass: TEntityAbstractClass; out aIndx: integer): TListInfo;
  protected
    FLists: TArray<TListInfo>;
    function GetList<T: TEntityAbstract>: TEntityList<T>;
    procedure AddList(aListInfo: TListInfo);
  public
    procedure SaveAll;
  end;

implementation

uses eEntities,
  System.SysUtils;

procedure TEntityList<T>.DoSmth;
var
  Entity: T;
begin
  for Entity in Self do

end;

function TEntity.GetListInfo(aEntityClass: TEntityAbstractClass; out aIndx: integer): TListInfo;
var
  ListInfo: TListInfo;
  i: Integer;
begin
  i := 0;
  for ListInfo in FLists do
    begin
      if ListInfo.EntityClass = aEntityClass then
        begin
          Result := ListInfo;
          aIndx := i;
        end;
      Inc(i);
    end;
end;

function TEntity.GetList<T>: TEntityList<T>;
var
  ListInfo: TListInfo;
  EntityClass: TEntityAbstractClass;
  SlaveKeyValue: Integer;
  indx: Integer;
begin
  EntityClass := T;
  ListInfo := GetListInfo(EntityClass, indx);

  SlaveKeyValue := FData.Items[ListInfo.MasterKeyField];
  Result := TEntityList<T>.Create(FDBEngine, ListInfo.SlaveKeyField, ID);
  FLists[indx].ListPointer := @Result;
end;

procedure TEntity.AddList(aListInfo: TListInfo);
begin
  aListInfo.ListPointer := nil;
  FLists := FLists + [aListInfo];
end;

procedure TEntity.SaveAll;
var
  ListInfo: TListInfo;
begin
  for ListInfo in FLists do
    begin
      //EntityList := ListPointer^;
      //EntityList:=TEntityList<T>.Create();

    end;
end;

constructor TEntityList<T>.Create(aDBEngine: TDBEngine; aKeyField: string; aKeyValue: integer);
var
  Filters: TArray<string>;
  Order: TArray<string>;
begin
  Filters := [Format('%s=%d', [aKeyField, aKeyValue])];
  Order := [];

  inherited Create(True);
  FDBEngine := aDBEngine;
  FillEntityList(Filters, Order);
end;

function TEntityList<T>.GetWherePart(aFilters: TArray<string>): string;
var
  i: Integer;
begin
  Result := '1=1';
  for i := 0 to Length(aFilters) - 1 do
    begin
      Result := Result + ' AND ';
      Result := Result + aFilters[i];
    end;
end;

function TEntityList<T>.GetOrderPart(aOrder: TArray<string>): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(aOrder) - 1 do
    begin
      Result := Result + aOrder[i];
      Result := Result + ',';
    end;

  Result := Result + 'ID';
end;

procedure TEntityList<T>.FillEntityList(aFilters, aOrder: TArray<string>);
var
  sql: string;
  dsQuery: TFDQuery;
  Entity: TEntityAbstract;
begin
  FEntityClass := T;

  sql := 'select Id from %s where %s order by %s';
  sql := Format(sql, [
    FEntityClass.GetTableName,
    GetWherePart(aFilters),
    GetOrderPart(aOrder)
  ]);

  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := sql;
    FDBEngine.OpenQuery(dsQuery);
    while not dsQuery.EOF do
      begin
        Entity := FEntityClass.Create(FDBEngine, dsQuery.FieldByName('Id').AsInteger);
        Add(Entity);
        dsQuery.Next;
      end;
  finally
    dsQuery.Free;
  end;
end;

constructor TEntityList<T>.Create(aDBEngine: TDBEngine; aFilters, aOrder: TArray<string>);
begin
  inherited Create(True);
  FDBEngine := aDBEngine;
  FillEntityList(aFilters, aOrder);
end;

procedure TEntityAbstract.StoreToDB(aSQL: string);
var
  dsQuery: TFDQuery;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := aSQL;
    SetParams(dsQuery.Params);
    FDBEngine.ExecQuery(dsQuery);
  finally
    dsQuery.Free;
  end;
end;

function TEntityAbstract.GetFieldTypeByName(aFieldName: string): TFieldType;
var
  DBField: TDBField;
begin
  for DBField in FFields do
    if DBField.FieldName = aFieldName then Exit(DBField.FieldType);
end;

procedure TEntityAbstract.SetParams(aParams: TFDParams);
var
  i: Integer;
begin
  for i := 0 to aParams.Count - 1 do
    case GetFieldTypeByName(aParams[i].Name) of
      ftString: aParams[i].AsString := FData.Items[aParams[i].Name];
      ftInteger: aParams[i].AsInteger := FData.Items[aParams[i].Name];
      ftFloat: aParams[i].AsFloat := FData.Items[aParams[i].Name];
    end;
end;

function TEntityAbstract.GetKeyValueString(aFields: TArray<string>): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(aFields) - 1 do
    begin
      if i > 0 then Result := Result + ',';
      Result := Result + Format('%s = :%s', [aFields[i], aFields[i]]);
    end;
end;

procedure TEntityAbstract.InsertToDB(aChangedFields: TArray<string>);
var
  sql: string;
begin
  sql := 'insert into %s set %s';
  sql := Format(sql, [GetTableName, GetKeyValueString(aChangedFields)]);

  StoreToDB(sql);
end;

procedure TEntityAbstract.UpdateToDB(aChangedFields: TArray<string>);
var
  sql: string;
begin
  sql := 'update %s set %s where ID = :ID';
  sql := Format(sql, [GetTableName, GetKeyValueString(aChangedFields)]);

  StoreToDB(sql);
end;

function TEntityAbstract.CheckChanges(aFieldName: string; aCurrentRecord: TFDQuery): Boolean;
begin
  if aFieldName = 'ID' then Exit(False);

  if aCurrentRecord.FieldByName(aFieldName).AsVariant = FData.Items[aFieldName] then
    Result := False
  else
    Result := True;
end;

function TEntityAbstract.GetEntityRecordFromDB(aID: Integer): TFDQuery;
var
  sql: string;
begin
  Result := TFDQuery.Create(nil);

  sql := 'select * from %s where Id = :ID';
  Result.SQL.Text := Format(sql, [GetTableName]);
  Result.ParamByName('ID').AsInteger := aID;
  FDBEngine.OpenQuery(Result);
end;

procedure TEntityAbstract.SaveEntity;
var
  dsQuery: TFDQuery;
  i, ChangesCount: Integer;
  ChangedFields: TArray<string>;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery := GetEntityRecordFromDB(Self.ID);

    ChangesCount := 0;
    for i := 0 to Length(FFields) - 1 do
      if CheckChanges(FFields[i].FieldName, dsQuery) then
        begin
          ChangedFields := ChangedFields + [FFields[i].FieldName];
          Inc(ChangesCount);
        end;

    if Self.ID = 0 then
      InsertToDB(ChangedFields)
    else
      if ChangesCount >0 then
        UpdateToDB(ChangedFields);

  finally
    dsQuery.Free;
  end;
end;

destructor TEntityAbstract.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TEntityAbstract.FillEntity(aID: integer);
var
  DBField: TDBField;
  dsQuery: TFDQuery;
  Value: variant;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery := GetEntityRecordFromDB(aID);

    for DBField in FFields do
      begin
        case DBField.FieldType of
          ftString: Value := dsQuery.FieldByName(DBField.FieldName).AsString;
          ftInteger: Value := dsQuery.FieldByName(DBField.FieldName).AsInteger;
        else
          Value := dsQuery.FieldByName(DBField.FieldName).AsString;
        end;

        FData.Add(DBField.FieldName, Value);
      end;
  finally
    dsQuery.Free;
  end;
end;

procedure TEntityAbstract.AddField(aFieldName: string; aFieldType: TFieldType);
var
  DBField: TDBField;
begin
  DBField.FieldName := UpperCase(aFieldName);
  DBField.FieldType := aFieldType;

  FFields := FFields + [DBField];
end;

function TEntityAbstract.GetID: integer;
begin
  Result := FData.Items['ID'];
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine; aID: Integer = 0);
begin
  FDBEngine := aDBEngine;

  AddField('Id', ftInteger);
  InitFields;

  FData := TDictionary<string, variant>.Create;
  FillEntity(aID)
end;

end.
