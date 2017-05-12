unit API_ORM;

interface

uses
  API_DB;

type
  TEntityAbstract = class abstract
  private
    function GetID: Integer;
    procedure SetID(aValue: integer);
  protected
    FDBEngine: TDBEngine;
  public
    constructor Create(aID: integer); virtual;
    property ID: integer read GetID write SetID;
  end;

  TEntityAbstractClass = class of TEntityAbstract;

implementation

function TEntityAbstract.GetID: integer;
begin

end;

procedure TEntityAbstract.SetID(aValue: integer);
begin

end;

constructor TEntityAbstract.Create(aID: Integer);
begin

end;

end.
