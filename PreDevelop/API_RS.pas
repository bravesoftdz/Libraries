unit API_RS;

interface

uses
  API_DBases;

type

  TRSapi = class
  public
   class function GetBtsIdByNumber(BtsNumber: String; engMySQL: TMySQLEngine = nil): Integer;
   class function GetBSCIdByNumber(BSCNumber: String; engMySQL: TMySQLEngine = nil): Integer;
  end;

implementation

uses
  FireDAC.Comp.Client;

class function TRSapi.GetBtsIdByNumber(BtsNumber: String; engMySQL: TMySQLEngine = nil): Integer;
var
  sql: string;
  DSet: TFDQuery;
  localMySQL: TMySQLEngine;
begin
  result:=0;

  if Length(BtsNumber)=1 then BtsNumber:='00'+BtsNumber;
  if Length(BtsNumber)=2 then BtsNumber:='0'+BtsNumber;

  sql:='SELECT Id FROM bts WHERE bts_number='+TMySQLEngine.StrToSQL(BtsNumber);
  DSet:=TFDQuery.Create(nil);
  try
    if engMySQL=nil then
      begin
        localMySQL:=TMySQLEngine.Create;
        localMySQL.OpenConnection;
      end
    else localMySQL:=engMySQL;

    try
      localMySQL.GetData(DSet,sql);

      if not DSet.FieldByName('Id').IsNull then
        result:=DSet.FieldByName('Id').AsInteger;

    finally
      if engMySQL=nil then
        begin
          localMySQL.CloseConnection;
          localMySQL.Free;
        end;
    end;
  finally
    DSet.Free;
  end;
end;

class function TRSapi.GetBSCIdByNumber(BSCNumber: String; engMySQL: TMySQLEngine = nil): Integer;
var
  sql: string;
  DSet: TFDQuery;
  localMySQL: TMySQLEngine;
begin
  result:=0;
  sql:='SELECT Id FROM bsc WHERE bsc_number='+TMySQLEngine.StrToSQL(BSCNumber);
  DSet:=TFDQuery.Create(nil);
  try
    if engMySQL=nil then
      begin
        localMySQL:=TMySQLEngine.Create;
        localMySQL.OpenConnection;
      end
    else localMySQL:=engMySQL;

    try
      localMySQL.GetData(DSet,sql);

      if not DSet.FieldByName('Id').IsNull then
        result:=DSet.FieldByName('Id').AsInteger;
    finally
      if engMySQL=nil then
        begin
          localMySQL.CloseConnection;
          localMySQL.Free;
        end;
    end;
  finally
    DSet.Free;
  end;
end;

end.
