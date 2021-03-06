unit API_DB;

interface

uses
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys.Intf,
  FireDAC.Phys.MSAcc,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.SQLite,
  FireDAC.DApt,
  FireDAC.UI.Intf,
  FireDAC.Comp.UI,
  FireDAC.ConsoleUI.Wait,
  FireDAC.VCLUI.Async,
  FireDAC.VCLUI.Wait;

type
  TConnectParams = record
    Host: string;
    DataBase: string;
    Login: string;
    Password: string;
    CharacterSet: string;
  end;

  TDBEngine = class abstract
  private
    FIsConnected: Boolean;
  protected
    FDConnection: TFDConnection;
    FConnectParams: TConnectParams;
    procedure SetConnectParams; virtual;
  public
    procedure OpenConnection; overload;
    procedure OpenConnection(aConnectParams: TConnectParams); overload;
    procedure CloseConnection;

    procedure OpenQuery(aQuery: TFDQuery; isFetchAll: Boolean = True); overload;
    procedure OpenQuery(aQuery: TFDQuery; aSQL: string); overload;

    procedure ExecQuery(aQuery: TFDQuery); virtual;

    function GetLastInsertedID: integer; virtual; abstract;
    destructor Destroy; override;
  end;

  TDBEngineClass = class of TDBEngine;

implementation

procedure TDBEngine.ExecQuery(aQuery: TFDQuery);
begin
  aQuery.Close;
  aQuery.Connection := FDConnection;
  aQuery.ExecSQL;
end;

procedure TDBEngine.OpenQuery(aQuery: TFDQuery; isFetchAll: Boolean = True);
begin
  aQuery.Connection := FDConnection;
  aQuery.Open;
  if isFetchAll then aQuery.FetchAll;
end;

procedure TDBEngine.OpenQuery(aQuery: TFDQuery; aSQL: string);
begin
  aQuery.SQL.Text := aSQL;
  OpenQuery(aQuery);
end;

procedure TDBEngine.OpenConnection(aConnectParams: TConnectParams);
begin
  FConnectParams := aConnectParams;
  Self.OpenConnection;
end;

procedure TDBEngine.SetConnectParams;
begin
  FDConnection.Params.Values['Server'] := FConnectParams.Host;
  FDConnection.Params.Values['Database'] := FConnectParams.DataBase;
  FDConnection.Params.Values['User_Name'] := FConnectParams.Login;
  FDConnection.Params.Values['Password'] := FConnectParams.Password;
  FDConnection.Params.Values['CharacterSet'] := FConnectParams.CharacterSet;
end;

destructor TDBEngine.Destroy;
begin
  if FIsConnected then Self.CloseConnection;

  inherited;
end;

procedure TDBEngine.CloseConnection;
begin
  FDConnection.Connected := False;
  FDConnection.Free;
  FIsConnected := False;
end;

procedure TDBEngine.OpenConnection;
begin
  FDConnection := TFDConnection.Create(nil);
  Self.SetConnectParams;
  FIsConnected := True;

  FDConnection.LoginPrompt := False;
  FDConnection.ResourceOptions.SilentMode := True;
  FDConnection.Connected := True;
end;

end.
