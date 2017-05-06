unit API_DB_MySQL;

interface

uses
  API_DB;

type
  TMySQLEngine = class(TDBEngine)
  protected
    procedure SetConnectParams; override;
  end;

implementation

procedure TMySQLEngine.SetConnectParams;
begin
  inherited;
  FDConnection.Params.Values['DriverID'] := 'MySQL';
end;

end.
