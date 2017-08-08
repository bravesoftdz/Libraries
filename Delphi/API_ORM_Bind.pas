unit API_ORM_Bind;

interface

uses
  System.Generics.Collections,
  API_ORM;

type
  TBindItem = record
    Control: TObject;
    Entity: TEntityAbstract;
  end;

  TBind = class
  private
    FBindArray: TArray<TBindItem>;
  public
    procedure AddBind(aControl: TObject; aEntity: TEntityAbstract);
    function GetEntityByControl(aControl: TObject): TEntityAbstract;
    function GetControlByEntity(aEntity: TEntityAbstract): TObject;
  end;

implementation

function TBind.GetControlByEntity(aEntity: TEntityAbstract): TObject;
var
  BindItem: TBindItem;
begin
  Result := nil;
  for BindItem in FBindArray do
    if BindItem.Entity = aEntity then
      Exit(BindItem.Control);
end;

procedure TBind.AddBind(aControl: TObject; aEntity: TEntityAbstract);
var
  BindItem: TBindItem;
begin
  BindItem.Control := aControl;
  BindItem.Entity := aEntity;

  FBindArray := FBindArray + [BindItem];
end;

function TBind.GetEntityByControl(aControl: TObject): TEntityAbstract;
var
  BindItem: TBindItem;
begin
  Result := nil;
  for BindItem in FBindArray do
    if BindItem.Control = aControl then
      Exit(BindItem.Entity);
end;

end.
