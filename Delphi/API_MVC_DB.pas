unit API_MVC_DB;

interface

uses
  System.Generics.Collections,
  API_MVC,
  API_DB,
  API_ORM;

type
  TModelDB = class abstract(TModelAbstract)
  protected
    FDBEngine: TDBEngine;
    function GetEntity(aEntityClass: TEntityAbstractClass; aID: integer): TEntityAbstract;
  public
    constructor Create(aData: TDictionary<string, variant>;
      aObjData: TObjectDictionary<string, TObject>); override;
  end;

  TControllerDB = class abstract(TControllerAbstract)
  private
    procedure ConnectToDB;
  protected
    FConnectParams: TConnectParams;
    FDBEngine: TDBEngine;
    FDBEngineClass: TDBEngineClass;
    FConnectOnCreate: Boolean;
    procedure InitDB; virtual; abstract;
    procedure CallModel(aModelClass: TModelClass; aProcName: string = ''); override;
    function GetConnectParams(aFileName: String): TConnectParams;
  public
    constructor Create(aMainView: TViewAbstract); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes;


function TModelDB.GetEntity(aEntityClass: TEntityAbstractClass; aID: integer): TEntityAbstract;
begin

end;

constructor TModelDB.Create(aData: TDictionary<string, variant>;
      aObjData: TObjectDictionary<string, TObject>);
begin
  inherited;

  FDBEngine := FObjData.Items['DBEngine'] as TDBEngine;
end;

procedure TControllerDB.CallModel(aModelClass: TModelClass; aProcName: string = '');
begin
  FObjData.Add('DBEngine', FDBEngine);
  inherited;
end;

function TControllerDB.GetConnectParams(aFileName: String): TConnectParams;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(aFileName);
    Result.Host := SL.Values['Host'];
    Result.DataBase := SL.Values['DataBase'];
    Result.Login := SL.Values['Login'];
    Result.Password := SL.Values['Password'];
    Result.CharacterSet := SL.Values['CharacterSet'];
  finally
    SL.Free;
  end;
end;

destructor TControllerDB.Destroy;
begin
  FDBEngine.Free;

  inherited;
end;

procedure TControllerDB.ConnectToDB;
begin
  FDBEngine.OpenConnection(FConnectParams);
end;

constructor TControllerDB.Create(aMainView: TViewAbstract);
begin
  inherited;

  Self.InitDB;
  FDBEngine := FDBEngineClass.Create;
  if FConnectOnCreate then ConnectToDB;
end;

end.
