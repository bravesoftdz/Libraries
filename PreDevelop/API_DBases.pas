unit API_DBases;

interface

uses
  System.Classes, FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.Stan.Intf,
  FireDAC.Phys.MSAcc, FireDAC.Phys.MySQL, FireDAC.Phys.Intf, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Comp.UI, FireDAC.Comp.Client, FireDAC.ConsoleUI.Wait,
  FireDAC.VCLUI.Async, FireDAC.VCLUI.Wait;

type

  // ������� ����� ������ � �� ����� ���������� FireDAC
  TDBEngine = class abstract
  private
    FDConnection: TFDConnection;
    // ��������� ������� ���������� ����������
    procedure SetCustomConnectParams; virtual; abstract;
  public
    // ������� � ��
    procedure OpenConnection;
    // ��������� �� ��
    procedure CloseConnection;
    // ������� SQL ������ �� ������� ������
    procedure GetData(DataSet: TFDQuery; sql: string); overload;
    function GetData(sql: string): TFDQuery; overload;
    // ������ �������, ��� ������ � �����������
    procedure OpenQuery(aDataSet: TFDQuery);
    // ������� SQL ������ �� ����������� ������
    procedure SetData(sql: String); overload;
    // ������ �� �����������, ��� ������ � �����������
    procedure ExecQuery(aDataSet: TFDQuery);
    // ���-�� ������ �����������
    property Connection: TFDConnection read FDConnection;
  public
    class function StrToSQL(value: string): string;
  end;

  // ������ � �� Access
  TAccessEngine = class(TDBEngine)
  private
    FFileName: String;
    procedure SetCustomConnectParams; override;
  public
    procedure OpenConnection(FileName: String); overload;
  end;

  // ������ � �� MySQL
  TMySQLEngine = class(TDBEngine)
  private
    FConnectParams: TStringList;
    procedure SetCustomConnectParams; override;
  public
    procedure OpenConnection(FileName: String); overload;
    function GetLastInsertedId: integer;
  end;

  TDBEngineClass = class of TDBEngine;

implementation

uses
  System.SysUtils, System.RegularExpressions, API_Files;

function TMySQLEngine.GetLastInsertedId: integer;
var
  dsQuery: TFDQuery;
begin
  result:=0;
  dsQuery:=TFDQuery.Create(nil);
  try
    GetData(dsQuery,'select LAST_INSERT_ID()');
    result:=dsQuery.Fields[0].AsInteger;
  finally
    dsQuery.Free;
  end;
end;

procedure TDBEngine.OpenQuery(aDataSet: TFDQuery);
begin
  aDataSet.Close;
  aDataSet.Connection := Self.Connection;
  aDataSet.Open;
  aDataSet.FetchAll;
end;

procedure TDBEngine.ExecQuery(aDataSet: TFDQuery);
begin
  aDataSet.Close;
  aDataSet.Connection := Self.Connection;
  aDataSet.ExecSQL;
end;

class function TDBEngine.StrToSQL(value: string): string;
var
  tpFloat: Extended;
begin
  if StrToDateDef(value, 0) > 0 then // ����
  begin
    while Pos('.', value) > 0 do
      value[Pos('.', value)] := ' ';
    result := quotedstr(copy(value, 7, 4) + copy(value, 4, 2) +
      copy(value, 1, 2));
    Exit;
  end;
  if TryStrToFloat(value, tpFloat) then // �����
  begin
    if Pos(',', value) > 0 then
      value := StringReplace(value, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    result := value;
    Exit;
  end
  else // ������
  begin
    value := Trim(value);
    if Length(value) = 0 then
      value := 'NULL'
    else
      value := quotedstr(value);
    result := value;
  end;
end;

procedure TDBEngine.SetData(sql: string);
var
  dsQuery: TFDQuery;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.Connection := Self.Connection;
    dsQuery.sql.Text := sql;
    dsQuery.ExecSQL;
  finally
    dsQuery.Free;
  end;
end;

procedure TMySQLEngine.OpenConnection(FileName: string);
begin
  FConnectParams := TStringList.Create;
  try
    FConnectParams.Text := TFilesEngine.GetTextFromFile(FileName);
    Self.OpenConnection;
  finally
    FConnectParams.Free;
  end;
end;

procedure TMySQLEngine.SetCustomConnectParams;
begin
  FDConnection.Params.Values['DriverID'] := 'MySQL';
  FDConnection.Params.Values['Server'] := FConnectParams.Values['Host'];
  FDConnection.Params.Values['Database'] := FConnectParams.Values['DataBase'];
  FDConnection.Params.Values['User_Name'] := FConnectParams.Values['Login'];
  FDConnection.Params.Values['Password'] := FConnectParams.Values['Password'];
  FDConnection.Params.Values['CharacterSet'] := FConnectParams.Values['CharacterSet'];
end;

function TDBEngine.GetData(sql: string): TFDQuery;
begin
  result := TFDQuery.Create(nil);
  Self.GetData(result, sql);
end;

procedure TDBEngine.GetData(DataSet: TFDQuery; sql: string);
begin
  DataSet.Connection := Self.FDConnection;
  DataSet.sql.Text := sql;
  DataSet.Open;
  DataSet.FetchAll;
end;

procedure TDBEngine.CloseConnection;
begin
  FDConnection.Connected := False;
  FDConnection.Free;
end;

procedure TDBEngine.OpenConnection;
begin
  FDConnection := TFDConnection.Create(nil);
  Self.SetCustomConnectParams;
  FDConnection.LoginPrompt := False;
  FDConnection.Connected := True;
end;

procedure TAccessEngine.OpenConnection(FileName: string);
begin
  FFileName := FileName;
  Self.OpenConnection;
end;

procedure TAccessEngine.SetCustomConnectParams;
begin
  FDConnection.Params.Values['Database'] := FFileName;
  FDConnection.Params.Values['DriverID'] := 'MSAcc';
end;

end.

