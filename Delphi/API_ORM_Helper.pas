unit API_ORM_Helper;

interface

uses
  API_ORM;

type
  TEntityHelper = class helper for TEntityAbstract
  strict private type
    TGetSourceListRef<T: TEntityAbstract> = reference to function(aEntity: T): TEntityList<T>;
    TCheckEntityRef<T: TEntityAbstract> = reference to function(aEntity: T): Boolean;
  public
    procedure RecursionSearch<T: TEntityAbstract>(aResultList: TEntityList<T>;
      aGetSourceList: TGetSourceListRef<T>; aCheckEntity: TCheckEntityRef<T> = nil); overload;
  end;

implementation

procedure TEntityHelper.RecursionSearch<T>(aResultList: TEntityList<T>;
  aGetSourceList: TGetSourceListRef<T>; aCheckEntity: TCheckEntityRef<T> = nil);
var
  Entity: T;
begin
  if Assigned(aCheckEntity) then
    begin
      if aCheckEntity(Self) then aResultList.Add(Entity)
    end
  else
    aResultList.Add(Entity);

  for Entity in aGetSourceList(Self) do
    Entity.RecursionSearch(aResultList, aGetSourceList, aCheckEntity);
end;

end.
