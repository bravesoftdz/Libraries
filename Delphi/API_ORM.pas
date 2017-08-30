unit API_ORM;

interface

uses
  System.Generics.Collections,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Data.DB,
  API_DB;

type
  TEntityAbstract = class;
  TEntityAbstractClass = class of TEntityAbstract;
  TFreeEvent = procedure of object;

  TDBField = record
    FieldName: string;
    FieldType: TFieldType;
  end;

  // One - To - One Relation
  TRelation = record
    ExtKey: string;
    ForeignKey: string;
    EntityClass: TEntityAbstractClass;
  end;

  TEntityStruct = record
    TableName: string;
    FieldList: TArray<TDBField>;
    RelatedList: TArray<TRelation>;
  end;

  TEntityAbstract = class abstract
  private
    FOnFree: TFreeEvent;
    function GetEntityRecordFromDB(aID: integer): TFDQuery;
    function CheckChanges(aFieldName: string; aCurrentRecord: TFDQuery): Boolean;
    function GetKeyValueString(aFields: TArray<string>): string;
    function GetKeysString(aFields: TArray<string>): string;
    function GetValuesString(aFields: TArray<string>): string;
    function GetFieldTypeByName(aFieldName: string): TFieldType;
    function GetExtIDByExtKey(aRelation: TRelation): Integer;
    procedure FillEntity(aID: integer);
    procedure RelateExternalEntities;
    procedure SaveSlaveToMasterRelations;
    procedure SaveMasterToSlaveRelations;
    procedure SetParams(aParams: TFDParams);
    procedure StoreToDB(aSQL: string);
    procedure InsertToDB(aChangedFields: TArray<string>);
    procedure UpdateToDB(aChangedFields: TArray<string>);
    function GetID: integer;
  protected
    FDBEngine: TDBEngine;
    FFields: TArray<TDBField>;
    FRelations: TObjectDictionary<string, TEntityAbstract>;
    FData: TDictionary<string, variant>;
    procedure SaveLists; virtual;
  public
    procedure SaveEntity;
    procedure DeleteEntity;
    procedure SaveAll;
    class function GetTableName: string;
    class function GetEntityStruct: TEntityStruct; virtual; abstract;
    class procedure AddField(var aFieldList: TArray<TDBField>; aFieldName: string; aFieldType: TFieldType);
    class procedure AddRelation(var aRelatedList: TArray<TRelation>; aExtKey, aForeignKey: string;
        aEntityClass: TEntityAbstractClass);
    constructor Create(aDBEngine: TDBEngine; aID: integer = 0);
    destructor Destroy; override;
    property ID: integer read GetID;
    property Fields: TArray<TDBField> read FFields;
    property Data: TDictionary<string, variant> read FData;
    property Relations: TObjectDictionary<string, TEntityAbstract> read FRelations;
    property OnFree: TFreeEvent read FOnFree write FOnFree;
  end;

  // One - To - Many Relation
  TEntityList<T: TEntityAbstract> = class abstract(TObjectList<T>)
  private
    FDBEngine: TDBEngine;
    FKeyField: string;
    FKeyValue: Integer;
    FDeletedIDs: TArray<Integer>;
    function GetFromPart(aEntityStruct: TEntityStruct): string;
    function GetWherePart(aFilters: TArray<string>): string;
    function GetOrderPart(aOrder: TArray<string>): string;
    procedure FillEntityList(aFilters, aOrder: TArray<string>);
    procedure FreeList;
    procedure UpdateKeys(aEntity: TEntityAbstract; aKeyField: string; aValue: integer);
  public
    function FindByID(aID: integer): T;
    procedure SaveList(aKeyValue: integer);
    procedure DeleteByID(aID: integer);
    procedure DeleteByIndex(aIndex: integer);
    procedure DeleteByEntity(aEntity: TEntityAbstract);
    procedure DeleteAll;
    constructor Create(aDBEngine: TDBEngine; aFilters, aOrder: TArray<string>); overload;
    constructor Create(aOwner: TEntityAbstract; aKeyField: string; aKeyValue: integer; aOrderKey: string = ''); overload;
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

procedure TEntityAbstract.SaveMasterToSlaveRelations;
var
  Pair: TPair<string, TEntityAbstract>;
  Relation: TRelation;
begin
  for Pair in FRelations do
    for Relation in GetEntityStruct.RelatedList do
      begin
        if Relation.ForeignKey <> '' then Continue;

        if Pair.Value is Relation.EntityClass then
          begin
            Pair.Value.Data[Relation.ExtKey] := ID;
            Pair.Value.SaveAll;
          end;
      end;
end;

function TEntityAbstract.GetExtIDByExtKey(aRelation: TRelation): Integer;
var
  dsQuery: TFDQuery;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := Format('select Id from %s where %s = :FKValue',
      [aRelation.EntityClass.GetTableName, aRelation.ExtKey]
    );
    dsQuery.ParamByName('FKValue').AsInteger := ID;
    FDBEngine.OpenQuery(dsQuery);

    if not dsQuery.IsEmpty then
      Result := dsQuery.FieldByName('Id').AsInteger
    else
      Result := 0;
  finally
    dsQuery.Free;
  end;
end;

procedure TEntityList<T>.DeleteAll;
var
  Entity: T;
begin
  for Entity in Self do
    if Entity.ID > 0 then
      FDeletedIDs := FDeletedIDs + [Entity.ID];
  Self.Clear;
end;

procedure TEntityAbstract.SaveSlaveToMasterRelations;
var
  Pair: TPair<string, TEntityAbstract>;
  Relation: TRelation;
begin
  for Pair in FRelations do
    for Relation in GetEntityStruct.RelatedList do
      if Pair.Value is Relation.EntityClass then
        begin
          if Relation.ForeignKey = '' then Continue;

          Pair.Value.SaveAll;
          FData.Items[Relation.ForeignKey] := Pair.Value.Data[Relation.ExtKey];
        end;
end;

procedure TEntityList<T>.UpdateKeys(aEntity: TEntityAbstract; aKeyField: string; aValue: integer);
var
  Pair: TPair<string, TEntityAbstract>;
  RelatedEntity: TEntityAbstract;
begin
  if aEntity.Data.ContainsKey(aKeyField) then
    aEntity.Data[aKeyField] := aValue
  else
    begin
      for Pair in aEntity.FRelations do
        begin
          if Pair.Value.Data.ContainsKey(aKeyField) then
            Pair.Value.Data[aKeyField] := aValue;
        end;
    end;
end;

function TEntityList<T>.GetFromPart(aEntityStruct: TEntityStruct): string;
var
  Relation: TRelation;
  i: Integer;
  ForeignKey: string;
begin
  Result := aEntityStruct.TableName + ' t1';

  i := 1;
  for Relation in aEntityStruct.RelatedList do
    begin
      Inc(i);
      if Relation.ForeignKey = '' then
        ForeignKey := 'id'
      else
        ForeignKey := Relation.ForeignKey;

      Result := Result + Format(' left join %s t%d on t%d.%s = t1.%s',
        [Relation.EntityClass.GetTableName, i, i, Relation.ExtKey, ForeignKey]
      );
    end;
end;

procedure TEntityAbstract.RelateExternalEntities;
var
  Relation: TRelation;
  ExtID: Integer;
begin
  for Relation in GetEntityStruct.RelatedList do
    begin
      if Relation.ForeignKey = '' then
        ExtID := GetExtIDByExtKey(Relation)
      else
        ExtID := FData.Items[Relation.ForeignKey];

      if ExtID > 0 then
        FRelations.AddOrSetValue(
          Relation.EntityClass.GetTableName,
          Relation.EntityClass.Create(FDBEngine, ExtID)
        )
      else
        FRelations.AddOrSetValue(
          Relation.EntityClass.GetTableName,
          nil
        );
    end;
end;

class procedure TEntityAbstract.AddRelation(var aRelatedList: TArray<TRelation>; aExtKey, aForeignKey: string;
    aEntityClass: TEntityAbstractClass);
var
  Relation: TRelation;
begin
  Relation.ExtKey := aExtKey;
  Relation.ForeignKey := aForeignKey;
  Relation.EntityClass := aEntityClass;
  aRelatedList := aRelatedList + [Relation];
end;

class function TEntityAbstract.GetTableName: string;
begin
  Result := GetEntityStruct.TableName;
end;

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

procedure TEntityList<T>.DeleteByEntity(aEntity: TEntityAbstract);
var
  ID: Integer;
begin
  ID := aEntity.ID;
  if Self.Remove(aEntity) >= 0 then
    FDeletedIDs := FDeletedIDs + [ID];
end;

procedure TEntityList<T>.DeleteByIndex(aIndex: integer);
var
  Entity: T;
begin
  Entity := Self.Items[aIndex];
  if Entity.ID > 0 then
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
      UpdateKeys(Entity, FKeyField, aKeyValue);

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
  SaveSlaveToMasterRelations;
  SaveEntity;
  SaveMasterToSlaveRelations;
  SaveLists;
end;

constructor TEntityList<T>.Create(aOwner: TEntityAbstract; aKeyField: string; aKeyValue: Integer; aOrderKey: string = '');
var
  Filters: TArray<string>;
  Order: TArray<string>;
begin
  Filters := [Format('%s=%d', [aKeyField, aKeyValue])];

  if aOrderKey <> '' then
    Order := [aOrderKey]
  else
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

  Result := Result + 't1.ID';
end;

procedure TEntityList<T>.FillEntityList(aFilters, aOrder: TArray<string>);
var
  sql: string;
  dsQuery: TFDQuery;
  Entity: TEntityAbstract;
  EntityClass: TEntityAbstractClass;
begin
  EntityClass := T;

  sql := 'select t1.Id from %s where %s order by %s';
  sql := Format(sql, [
    GetFromPart(EntityClass.GetEntityStruct),
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

function TEntityAbstract.GetKeysString(aFields: TArray<string>): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(aFields) - 1 do
    begin
      if i > 0 then Result := Result + ',';
      Result := Result + Format('`%s`', [aFields[i]]);
    end;
end;

function TEntityAbstract.GetValuesString(aFields: TArray<string>): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(aFields) - 1 do
    begin
      if i > 0 then Result := Result + ',';
      Result := Result + ':' + aFields[i];
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
      Result := Result + Format('`%s` = :%s', [aFields[i], aFields[i]]);
    end;
end;

procedure TEntityAbstract.InsertToDB(aChangedFields: TArray<string>);
var
  sql: string;
begin
  sql := 'insert into %s (%s) values (%s)';
  sql := Format(sql, [GetTableName, GetKeysString(aChangedFields), GetValuesString(aChangedFields)]);

  StoreToDB(sql);
  FData['ID'] := FDBEngine.GetLastInsertedID;
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
      if ChangesCount > 0 then
        UpdateToDB(ChangedFields);

  finally
    dsQuery.Free;
  end;
end;

destructor TEntityAbstract.Destroy;
begin
  FData.Free;
  FRelations.Free;
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

class procedure TEntityAbstract.AddField(var aFieldList: TArray<TDBField>; aFieldName: string; aFieldType: TFieldType);
var
  DBField: TDBField;
begin
  DBField.FieldName := UpperCase(aFieldName);
  DBField.FieldType := aFieldType;

  aFieldList := aFieldList + [DBField];
end;

function TEntityAbstract.GetID: integer;
begin
  Result := FData.Items['ID'];
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine; aID: Integer = 0);
begin
  FDBEngine := aDBEngine;

  AddField(FFields, 'ID', ftInteger);
  FFields := FFields + GetEntityStruct.FieldList;

  FData := TDictionary<string, variant>.Create;
  FillEntity(aID);

  FRelations := TObjectDictionary<string, TEntityAbstract>.Create([doOwnsValues]);
  RelateExternalEntities;
end;

end.
