unit API_DB_SQLite;

interface

uses
  System.SysUtils,
  FireDAC.Comp.Client,
  API_DB;

type
  TSQLiteEngine = class(TDBEngine)
  private
    FLastInsertedTable: string;
  protected
    procedure SetConnectParams; override;
    procedure ExecQuery(aQuery: TFDQuery); override;
  public
    function GetLastInsertedID: integer; override;
  end;

implementation

function Explode(aIncome: string; aDelimiter: string): TArray<string>;
var
  i: integer;
  tx: string;
begin
  tx:='';
  for i := 1 to Length(aIncome) do
    begin
      if (aIncome[i]<>aDelimiter) then tx:=tx+aIncome[i];
      if (aIncome[i]=aDelimiter) or (i=Length(aIncome)) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[Length(Result)-1]:=Trim(tx);
          tx:='';
        end;
    end;
end;

procedure TSQLiteEngine.ExecQuery(aQuery: TFDQuery);
var
  SQLWords: TArray<string>;
begin
  if Pos('INSERT INTO', UpperCase(aQuery.SQL.Text)) > 0 then
    begin
      SQLWords := Explode(aQuery.SQL.Text, ' ');
      FLastInsertedTable := SQLWords[2];
    end;

  inherited;
end;

function TSQLiteEngine.GetLastInsertedID: integer;
var
  dsQuery: TFDQuery;
begin
  Result := 0;
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := 'select seq from sqlite_sequence where name = :table';
    dsQuery.ParamByName('table').AsString := LowerCase(FLastInsertedTable);
    OpenQuery(dsQuery);
    Result := dsQuery.Fields[0].AsInteger;
  finally
    dsQuery.Free;
  end;
end;

procedure TSQLiteEngine.SetConnectParams;
begin
  inherited;
  FDConnection.Params.Values['DriverID'] := 'SQLite';

  //FDConnection.Params.Add('Pooled=false');
  //FDConnection.Params.Add('OpenMode=ReadWrite');
  //FDConnection.Params.Add('DateTimeFormat=DateTime');
  //FDConnection.Params.Add('StringFormat=Unicode');
  //FDConnection.Params.Add('Protocol=Local');
  //FDConnection.Params.Add('ForeignKeys=On');
  //FDConnection.Params.Add('LockingMode=Normal');
  //FDConnection.Params.Add('Synchronous=Full');
  //FDConnection.Params.Add('SharedCache=False');
end;

end.
