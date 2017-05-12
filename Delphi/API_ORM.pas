unit API_ORM;

interface

uses
  System.Generics.Collections,
  Data.DB,
  API_DB;

type
  TDBField = record
    FieldName: string;
    FieldType: TFieldType;
  end;

  TEntityAbstract = class abstract
  private
    procedure FillEntity(aID: integer);
  protected
    FDBEngine: TDBEngine;
    FFields: TArray<TDBField>;
    FData: TDictionary<string, variant>;

    function GetID: integer;

    procedure InitFields; virtual; abstract;
    procedure AddField(aFieldName: string; aFieldType: TFieldType);
  public
    procedure SaveEntity;
    class function GetTableName: string; virtual; abstract;
    constructor Create(aDBEngine: TDBEngine; aID: integer = 0);
    destructor Destroy; override;
    property ID: integer read GetID;
  end;

  TEntityAbstractClass = class of TEntityAbstract;

implementation

uses
  System.SysUtils,
  FireDAC.Comp.Client;

procedure TEntityAbstract.SaveEntity;
var
  dsQuery: TFDQuery;
begin
  dsQuery := TFDQuery.Create(nil);
  try

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
  sql: string;
  dsQuery: TFDQuery;
  Value: variant;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    sql := 'select * from %s where Id = :ID';
    dsQuery.SQL.Text := Format(sql, [GetTableName]);
    dsQuery.ParamByName('ID').AsInteger := aID;
    FDBEngine.OpenQuery(dsQuery);

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
  DBField.FieldName := aFieldName;
  DBField.FieldType := aFieldType;

  FFields := FFields + [DBField];
end;

function TEntityAbstract.GetID: integer;
begin
  Result := FData.Items['Id'];
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine; aID: Integer = 0);
begin
  FDBEngine := aDBEngine;

  AddField('Id', ftInteger);
  InitFields;

  FData := TDictionary<string, variant>.Create;
  if aID > 0 then FillEntity(aID);
end;

end.
