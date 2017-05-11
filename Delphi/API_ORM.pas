unit API_ORM;

interface

type
  TEntityAbstract = class abstract
  protected
  public
    constructor Create(aID: integer); virtual;
  end;

  TEntityAbstractClass = class of TEntityAbstract;

implementation

constructor TEntityAbstract.Create(aID: Integer);
begin

end;

end.
