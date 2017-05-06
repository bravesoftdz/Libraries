unit API_DBases;

interface

uses
   System.Classes
  ,FireDAC.Stan.Def
  ,FireDAC.Stan.Async
  ,FireDAC.Phys.MySQL
  ,FireDAC.Comp.Client
  ,FireDAC.DApt
  ,FireDAC.VCLUI.Wait
  ,API_Files;

type
  TDBEngine = class abstract
  private
    FDConnection: TFDConnection;
    FConnectParams: TStringList;
    procedure SetConnectParams; virtual; abstract;
  public
    procedure OpenConnection; overload;
    procedure OpenConnection(aFileName: String); overload;
    procedure OpenQuery(aQuery: TFDQuery); overload;
    procedure OpenQuery(aQuery: TFDQuery; aSQL: string); overload;
    procedure ExecQuery(aQuery: TFDQuery);
    property Connection: TFDConnection read FDConnection;
  end;

  TMySQLEngine = class(TDBEngine)
  private
    procedure SetConnectParams; override;
  public
    function GetLastInsertedId: integer;
  end;

implementation

procedure TDBEngine.OpenQuery(aQuery: TFDQuery; aSQL: string);
begin
  aQuery.SQL.Text:=aSQL;
  OpenQuery(aQuery);
end;

function TMySQLEngine.GetLastInsertedId: integer;
var
  dsQuery: TFDQuery;
begin
  Result:=0;
  dsQuery:=TFDQuery.Create(nil);
  try
    OpenQuery(dsQuery, 'select LAST_INSERT_ID()');
    Result:=dsQuery.Fields[0].AsInteger;
  finally
    dsQuery.Free;
  end;
end;

procedure TDBEngine.ExecQuery(aQuery: TFDQuery);
begin
  aQuery.Close;
  aQuery.Connection := FDConnection;
  aQuery.ExecSQL;
end;

procedure TDBEngine.OpenQuery(aQuery: TFDQuery);
begin
  aQuery.Connection:=FDConnection;
  aQuery.Open;
  aQuery.FetchAll;
end;

procedure TDBEngine.OpenConnection(aFileName: String);
begin
  FConnectParams := TStringList.Create;
  try
    FConnectParams.Text := TFilesEngine.GetTextFromFile(aFileName);
    OpenConnection;
  finally
    FConnectParams.Free;
  end;
end;

procedure TMySQLEngine.SetConnectParams;
begin
  FDConnection.Params.Values['DriverID'] := 'MySQL';
  FDConnection.Params.Values['Server'] := FConnectParams.Values['Host'];
  FDConnection.Params.Values['Database'] := FConnectParams.Values['DataBase'];
  FDConnection.Params.Values['User_Name'] := FConnectParams.Values['Login'];
  FDConnection.Params.Values['Password'] := FConnectParams.Values['Password'];
  FDConnection.Params.Values['CharacterSet'] := FConnectParams.Values['CharacterSet'];
end;

procedure TDBEngine.OpenConnection;
begin
  FDConnection := TFDConnection.Create(nil);
  SetConnectParams;
  FDConnection.LoginPrompt := False;
  FDConnection.Connected := True;
end;

end.
