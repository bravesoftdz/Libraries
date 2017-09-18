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
  TListFreeMethod = procedure of object;

  TDBField = record
    FieldName: string;
    FieldType: TFieldType;
  end;

  // One - To - One Relation
  TOneRelation = record
    ForeignKeyField: string;
    ExternalKeyField: string;
    EntityClass: TEntityAbstractClass;
  end;

  // One - To - Many Relation
  {TManyRelation = record
    RelName: string;
    KeyField: string;
    ExternalForeignKeyField: string;
  end;}

  TEntityStruct = record
    TableName: string;
    FieldList: TArray<TDBField>;
    OneRelatedList: TArray<TOneRelation>;
    function GetOneRelationByTableName(aTableName: string): TOneRelation;
  end;

  TEntityAbstract = class abstract
  private
    FListFreeProcs: TArray<TMethod>;
    function CheckChanges(aFieldName: string; aCurrentRecord: TFDQuery): Boolean;
    function GetKeyValueString(aFields: TArray<string>): string;
    function GetKeysString(aFields: TArray<string>): string;
    function GetValuesString(aFields: TArray<string>): string;
    function GetFieldTypeByName(aFieldName: string): TFieldType;
    function GetExtIDByExtKey(aOneRelation: TOneRelation): Integer;
    procedure GetEntityRecordFromDB(aFDQuery: TFDQuery; aID: integer);
    procedure FillEntity(aID: integer);
    procedure RelateExternalEntities;
    procedure SaveSlaveToMasterOneRelations;
    procedure SaveMasterToSlaveOneRelations;
    procedure SetParams(aParams: TFDParams);
    procedure StoreToDB(aSQL: string);
    procedure InsertToDB(aChangedFields: TArray<string>);
    procedure UpdateToDB(aChangedFields: TArray<string>);
    function GetID: integer;
  protected
    FDBEngine: TDBEngine;
    FFields: TArray<TDBField>;
    FOneRelations: TObjectDictionary<string, TEntityAbstract>;
    FData: TDictionary<string, variant>;
    procedure SaveLists; virtual;
  public
    procedure SaveEntity;
    procedure DeleteEntity;
    procedure Assign(aSourceEntity: TEntityAbstract); virtual;
    procedure SaveAll;
    class function GetTableName: string;
    class function GetEntityStruct: TEntityStruct; virtual; abstract;
    class procedure AddField(var aFieldList: TArray<TDBField>; aFieldName: string; aFieldType: TFieldType);
    class procedure AddOneRelation(var aOneRelatedList: TArray<TOneRelation>; aExternalKeyField, aForeignKeyField: string;
        aEntityClass: TEntityAbstractClass);
    constructor Create(aDBEngine: TDBEngine; aID: integer = 0);
    destructor Destroy; override;
    property ID: integer read GetID;
    property Fields: TArray<TDBField> read FFields;
    property Data: TDictionary<string, variant> read FData;
    property OneRelations: TObjectDictionary<string, TEntityAbstract> read FOneRelations;
    property ListFreeProcs: TArray<TMethod> read FListFreeProcs write FListFreeProcs;
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
    procedure UpdateKeys(aEntity: TEntityAbstract; aKeyField: string; aValue: integer);
    procedure FreeList;
  public
    function FindByID(aID: integer): T;
    procedure SaveList(aKeyValue: integer);
    procedure Assign(aEntityList: TEntityList<T>);
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

procedure TEntityList<T>.Assign(aEntityList: TEntityList<T>);
var
  Entity, CloneEntity: TEntityAbstract;
  EntityClass: TEntityAbstractClass;
begin
  EntityClass := T;

  for Entity in aEntityList do
    begin
      CloneEntity := EntityClass.Create(FDBEngine);
      CloneEntity.Assign(Entity);

      Self.Add(CloneEntity);
    end;
end;

function TEntityStruct.GetOneRelationByTableName(aTableName: string): TOneRelation;
var
  OneRelation: TOneRelation;
begin
  for OneRelation in OneRelatedList do
    if OneRelation.EntityClass.GetTableName = aTableName then Exit(OneRelation);
end;

procedure TEntityAbstract.Assign(aSourceEntity: TEntityAbstract);
var
  Pair: TPair<string, variant>;
  OneRelPair: TPair<string, TEntityAbstract>;
  NewEntity: TEntityAbstract;
  EntityClass: TEntityAbstractClass;
begin
  // Copy Fields
  for Pair in aSourceEntity.Data do
    begin
      if Pair.Key = 'ID' then Continue;

      FData.AddOrSetValue(Pair.Key, Pair.Value);
    end;

  // Copy One To One Relations
  for OneRelPair in aSourceEntity.OneRelations do
    begin
      if OneRelPair.Value <> nil then
        begin
          EntityClass := GetEntityStruct.GetOneRelationByTableName(OneRelPair.Key).EntityClass;

          NewEntity := EntityClass.Create(FDBEngine);
          NewEntity.Assign(OneRelPair.Value);
        end
      else NewEntity := nil;

      FOneRelations.AddOrSetValue(OneRelPair.Key, NewEntity);
    end;

  // Copy One To Many Relations implicts after inherited in assign method
end;

procedure TEntityAbstract.SaveMasterToSlaveOneRelations;
var
  Pair: TPair<string, TEntityAbstract>;
  OneRelation: TOneRelation;
begin
  for Pair in FOneRelations do
    for OneRelation in GetEntityStruct.OneRelatedList do
      begin
        if OneRelation.ForeignKeyField <> '' then Continue;

        if Pair.Value is OneRelation.EntityClass then
          begin
            Pair.Value.Data[OneRelation.ExternalKeyField] := ID;
            Pair.Value.SaveAll;
          end;
      end;
end;

function TEntityAbstract.GetExtIDByExtKey(aOneRelation: TOneRelation): Integer;
var
  dsQuery: TFDQuery;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := Format('select Id from %s where %s = :FKValue',
      [aOneRelation.EntityClass.GetTableName, aOneRelation.ExternalKeyField]
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

procedure TEntityAbstract.SaveSlaveToMasterOneRelations;
var
  Pair: TPair<string, TEntityAbstract>;
  OneRelation: TOneRelation;
begin
  for Pair in FOneRelations do
    for OneRelation in GetEntityStruct.OneRelatedList do
      if Pair.Value is OneRelation.EntityClass then
        begin
          if OneRelation.ForeignKeyField = '' then Continue;

          Pair.Value.SaveAll;
          FData.Items[OneRelation.ForeignKeyField] := Pair.Value.Data[OneRelation.ExternalKeyField];
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
      for Pair in aEntity.FOneRelations do
        begin
          if Pair.Value.Data.ContainsKey(aKeyField) then
            Pair.Value.Data[aKeyField] := aValue;
        end;
    end;
end;

function TEntityList<T>.GetFromPart(aEntityStruct: TEntityStruct): string;
var
  OneRelation: TOneRelation;
  i: Integer;
  ForeignKey: string;
begin
  Result := aEntityStruct.TableName + ' t1';

  i := 1;
  for OneRelation in aEntityStruct.OneRelatedList do
    begin
      Inc(i);
      if OneRelation.ForeignKeyField = '' then
        ForeignKey := 'id'
      else
        ForeignKey := OneRelation.ForeignKeyField;

      Result := Result + Format(' left join %s t%d on t%d.%s = t1.%s',
        [OneRelation.EntityClass.GetTableName, i, i, OneRelation.ExternalKeyField, ForeignKey]
      );
    end;
end;

procedure TEntityAbstract.RelateExternalEntities;
var
  OneRelation: TOneRelation;
  ExtID: Integer;
begin
  for OneRelation in GetEntityStruct.OneRelatedList do
    begin
      if OneRelation.ForeignKeyField = '' then
        ExtID := GetExtIDByExtKey(OneRelation)
      else
        ExtID := FData.Items[OneRelation.ForeignKeyField];

      if ExtID > 0 then
        FOneRelations.AddOrSetValue(
          OneRelation.EntityClass.GetTableName,
          OneRelation.EntityClass.Create(FDBEngine, ExtID)
        )
      else
        FOneRelations.AddOrSetValue(
          OneRelation.EntityClass.GetTableName,
          nil
        );
    end;
end;

class procedure TEntityAbstract.AddOneRelation(var aOneRelatedList: TArray<TOneRelation>; aExternalKeyField, aForeignKeyField: string;
    aEntityClass: TEntityAbstractClass);
var
  OneRelation: TOneRelation;
begin
  OneRelation.ExternalKeyField := aExternalKeyField;
  OneRelation.ForeignKeyField := aForeignKeyField;
  OneRelation.EntityClass := aEntityClass;
  aOneRelatedList := aOneRelatedList + [OneRelation];
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
  SaveSlaveToMasterOneRelations;
  SaveEntity;
  SaveMasterToSlaveOneRelations;
  SaveLists;
end;

constructor TEntityList<T>.Create(aOwner: TEntityAbstract; aKeyField: string; aKeyValue: Integer; aOrderKey: string = '');
var
  Filters: TArray<string>;
  Order: TArray<string>;
  Method: TMethod;
begin
  Filters := [Format('%s=%d', [aKeyField, aKeyValue])];

  if aOrderKey <> '' then
    Order := [aOrderKey]
  else
    Order := [];

  inherited Create(True);
  FDBEngine := aOwner.FDBEngine;

  Method.Code := @TEntityList<T>.FreeList;
  Method.Data := Self;
  aOwner.ListFreeProcs := aOwner.ListFreeProcs + [Method];

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
      ftDateTime:
        begin
          aParams[i].AsDateTime := FData.Items[aParams[i].Name];
          if aParams[i].AsDateTime = 0 then
            aParams[i].Clear
        end;
      ftBoolean: aParams[i].AsBoolean := FData.Items[aParams[i].Name];
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

procedure TEntityAbstract.GetEntityRecordFromDB(aFDQuery: TFDQuery; aID: integer);
var
  sql: string;
begin
  sql := 'select * from %s where Id = :ID';
  aFDQuery.SQL.Text := Format(sql, [GetTableName]);
  aFDQuery.ParamByName('ID').AsInteger := aID;
  FDBEngine.OpenQuery(aFDQuery);
end;

procedure TEntityAbstract.SaveEntity;
var
  dsQuery: TFDQuery;
  i, ChangesCount: Integer;
  ChangedFields: TArray<string>;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    GetEntityRecordFromDB(dsQuery, Self.ID);

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
var
  Method: TMethod;
  ListFreeMethod: TListFreeMethod;
begin
  FData.Free;
  FOneRelations.Free;

  // Free Lists
  for Method in FListFreeProcs do
    begin
      ListFreeMethod := TListFreeMethod(Method);
      ListFreeMethod;
    end;

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
    GetEntityRecordFromDB(dsQuery, aID);

    for DBField in FFields do
      begin
        case DBField.FieldType of
          ftString: Value := dsQuery.FieldByName(DBField.FieldName).AsString;
          ftInteger: Value := dsQuery.FieldByName(DBField.FieldName).AsInteger;
          ftFloat: Value := dsQuery.FieldByName(DBField.FieldName).AsFloat;
          ftDateTime: Value := dsQuery.FieldByName(DBField.FieldName).AsDateTime;
          ftBoolean: Value := dsQuery.FieldByName(DBField.FieldName).AsBoolean;
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

  FOneRelations := TObjectDictionary<string, TEntityAbstract>.Create([doOwnsValues]);
  RelateExternalEntities;
end;

end.
