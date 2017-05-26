unit API_ORM;

interface

uses
  System.Generics.Collections,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Data.DB,
  API_DB;

type
  TFreeEvent = procedure of object;

  TDBField = record
    FieldName: string;
    FieldType: TFieldType;
  end;

  TEntityAbstract = class abstract
  private
    FOnFree: TFreeEvent;
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
    procedure SaveLists; virtual;
  public
    procedure SaveEntity;
    procedure DeleteEntity;
    procedure SaveAll;
    class function GetTableName: string; virtual; abstract;
    constructor Create(aDBEngine: TDBEngine; aID: integer = 0);
    destructor Destroy; override;
    property ID: integer read GetID;
    property Data: TDictionary<string, variant> read FData;
    property Fields: TArray<TDBField> read FFields;
    property OnFree: TFreeEvent read FOnFree write FOnFree;
  end;

  TEntityAbstractClass = class of TEntityAbstract;

  TEntityList<T: TEntityAbstract> = class abstract(TObjectList<T>)
  private
    FDBEngine: TDBEngine;
    FKeyField: string;
    FKeyValue: Integer;
    FDeletedIDs: TArray<Integer>;
    function GetWherePart(aFilters: TArray<string>): string;
    function GetOrderPart(aOrder: TArray<string>): string;
    procedure FillEntityList(aFilters, aOrder: TArray<string>);
    procedure FreeList;
  public
    function FindByID(aID: integer): T;
    procedure SaveList(aKeyValue: integer);
    procedure DeleteByID(aID: integer);
    procedure DeleteByIndex(aIndex: integer);
    constructor Create(aDBEngine: TDBEngine; aFilters, aOrder: TArray<string>); overload;
    constructor Create(aOwner: TEntityAbstract; aKeyField: string; aKeyValue: integer); overload;
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

procedure TEntityList<T>.FreeList;
begin
  Self.Free;
end;

procedure TEntityAbstract.DeleteEntity;
var
  dsQuery: TFDQuery;
  sql: string;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    sql := 'delete from %s where id=:id';
    sql := Format(sql, [GetTableName]);
    dsQuery.SQL.Text := sql;
    dsQuery.ParamByName('id').AsInteger := ID;
    FDBEngine.ExecQuery(dsQuery);
  finally
    dsQuery.Free;
  end;
end;

procedure TEntityList<T>.DeleteByIndex(aIndex: integer);
var
  Entity: T;
begin
  Entity := Self.Items[aIndex];
  if Entity.ID >0 then
    FDeletedIDs := FDeletedIDs + [Entity.ID];
  Self.Delete(aIndex);
end;

procedure TEntityList<T>.DeleteByID(aID: integer);
var
  Entity: T;
begin
  Entity := FindByID(aID);
  FDeletedIDs := FDeletedIDs + [Entity.ID];
  Self.Remove(Entity);
end;

function TEntityList<T>.FindByID(aID: integer): T;
var
  Entity: T;
begin
  for Entity in Self do
    if Entity.ID = aID then Exit(Entity);
end;

procedure TEntityAbstract.SaveLists;
begin
end;

procedure TEntityList<T>.SaveList(aKeyValue: integer);
var
  Entity: T;
  DeleteEntity: TEntityAbstract;
  EntityClass: TEntityAbstractClass;
  ID, DeletedID: Integer;
begin
  if aKeyValue = 0 then aKeyValue := FKeyValue;

  for Entity in Self do
    begin
      Entity.Data[FKeyField] := FKeyValue;
      Entity.SaveAll;
    end;

  EntityClass := T;
  for DeletedID in FDeletedIDs do
    begin
      DeleteEntity := EntityClass.Create(FDBEngine, DeletedID);
      try
        DeleteEntity.DeleteEntity;
      finally
        DeleteEntity.Free;
      end;
    end;
end;

procedure TEntityAbstract.SaveAll;
begin
  SaveEntity;
  SaveLists;
end;

constructor TEntityList<T>.Create(aOwner: TEntityAbstract; aKeyField: string; aKeyValue: integer);
var
  Filters: TArray<string>;
  Order: TArray<string>;
begin
  Filters := [Format('%s=%d', [aKeyField, aKeyValue])];
  Order := [];

  inherited Create(True);
  FDBEngine := aOwner.FDBEngine;
  aOwner.OnFree := FreeList;
  FKeyField := aKeyField;
  FKeyValue := aKeyValue;
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
  EntityClass: TEntityAbstractClass;
begin
  EntityClass := T;

  sql := 'select Id from %s where %s order by %s';
  sql := Format(sql, [
    EntityClass.GetTableName,
    GetWherePart(aFilters),
    GetOrderPart(aOrder)
  ]);

  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := sql;
    FDBEngine.OpenQuery(dsQuery);
    while not dsQuery.EOF do
      begin
        Entity := EntityClass.Create(FDBEngine, dsQuery.FieldByName('Id').AsInteger);
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
  Result := ftUnknown;
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
  if Assigned(FOnFree) then FOnFree;
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
