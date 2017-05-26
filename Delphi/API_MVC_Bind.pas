unit API_MVC_Bind;

interface

uses
  System.Generics.Collections;

type
  TBind = record
    CntrlIndex: Integer;
    EntityID: Integer;
  end;

  TBindArray = TArray<TBind>;

  TBindData = class(TDictionary<string, TBindArray>)
  public
    function GetEntID(aKey: string; aCntrlIndex: integer): integer;
    procedure AddBind(aKey: string; aCntrlIndex, aEntID: integer);
  end;

implementation

function TBindData.GetEntID(aKey: string; aCntrlIndex: integer): integer;
var
  BindArray: TBindArray;
  Bind: TBind;
begin
  Result := 0;
  BindArray := Items[aKey];

  for Bind in BindArray do
    if Bind.CntrlIndex = aCntrlIndex then Exit(Bind.EntityID);
end;

procedure TBindData.AddBind(aKey: string; aCntrlIndex, aEntID: integer);
var
  Bind: TBind;
  BindArray: TBindArray;
begin
  Bind.CntrlIndex := aCntrlIndex;
  Bind.EntityID := aEntID;

  if Self.TryGetValue(aKey, BindArray) then
    BindArray := Items[aKey]
  else
    BindArray := [];

  BindArray := BindArray + [Bind];
  AddOrSetValue(aKey, BindArray);
end;

end.
