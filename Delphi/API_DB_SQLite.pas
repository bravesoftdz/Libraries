unit API_DB_SQLite;

interface

uses
  API_DB;

type
  TSQLiteEngine = class(TDBEngine)
  protected
    procedure SetConnectParams; override;
  end;

implementation

procedure TSQLiteEngine.SetConnectParams;
begin
  inherited;
  FDConnection.Params.Values['DriverID'] := 'SQLite';
end;

end.
