unit API_ORM_Cntrls;

interface

uses
  System.Classes,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Forms,
  API_MVC,
  API_ORM,
  API_ORM_Bind;

  type
  TViewORM = class(TViewAbstract)
  protected
    FBind: TBind;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TEditChangeEvent = procedure(aEdit: TControl) of object;

  TEntityPanelAbstract = class abstract(TScrollBox)
  private
    FFieldsCount: integer;
    procedure CreateEntityFields;
  protected
    FEntity: TEntityAbstract;
    FAfterEditChange: TEditChangeEvent;
    function GetStringValue(aFieldName: string): string;
    function GetIntegerValue(aFieldName: string): integer;
    procedure CreateFieldControl(aDBField: TDBField; aNum: Integer);
    procedure InitPanel; virtual;
    procedure CntrlChange(Sender: TObject);
  public
    constructor Create(aOwner: TWinControl);
    procedure BuildControls(aEntity: TEntityAbstract);
    procedure ClearControls;
    property OnAfterEditChange: TEditChangeEvent read FAfterEditChange write FAfterEditChange;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Data.DB;

constructor TViewORM.Create(AOwner: TComponent);
begin
  inherited;
  FBind := TBind.Create;
end;

destructor TViewORM.Destroy;
begin
  FBind.Free;
  inherited;
end;

procedure TEntityPanelAbstract.CreateEntityFields;
var
  DBField: TDBField;
begin
  for DBField in FEntity.Fields do
    begin
      CreateFieldControl(DBField, FFieldsCount);
      Inc(FFieldsCount);
    end;
end;

procedure TEntityPanelAbstract.ClearControls;
begin
  DestroyComponents;
end;

procedure TEntityPanelAbstract.CntrlChange(Sender: TObject);
var
  FieldName: string;
  Pair: TPair<string, TEntityAbstract>;
begin
  FieldName := (Sender as TControl).Name;
  Delete(FieldName, 1, 5);

  if FEntity.Data.ContainsKey(FieldName) then
    begin
      if Sender is TEdit then
        FEntity.Data.Items[FieldName] := (Sender as TEdit).Text;
      if Sender is TColorBox then
        FEntity.Data.Items[FieldName] := (Sender as TColorBox).Selected;
    end
  else
    for Pair in FEntity.Relations do
      begin
        if Pair.Value = nil then Continue;

        if Pair.Value.Data.ContainsKey(FieldName) then
          begin
            if Sender is TEdit then
              Pair.Value.Data.Items[FieldName] := (Sender as TEdit).Text;
            if Sender is TColorBox then
              Pair.Value.Data.Items[FieldName] := (Sender as TColorBox).Selected;
          end;
      end;

  if Assigned(FAfterEditChange) then FAfterEditChange(Sender as TControl);
end;

procedure TEntityPanelAbstract.InitPanel;
begin
end;

function TEntityPanelAbstract.GetStringValue(aFieldName: string): string;
begin
  Result := FEntity.Data.Items[aFieldName];
end;

function TEntityPanelAbstract.GetIntegerValue(aFieldName: string): integer;
begin
  Result := FEntity.Data.Items[aFieldName];
end;

procedure TEntityPanelAbstract.CreateFieldControl(aDBField: TDBField; aNum: Integer);
var
  lblFieldTitle: TLabel;
  edtControl: TEdit;
  clrbxControl: TColorBox;
  CntrlName: string;
begin
  lblFieldTitle := TLabel.Create(Self);

  CntrlName := 'lbl' + aDBField.FieldName;
  if Self.FindComponent(CntrlName) = nil then
    lblFieldTitle.Name := CntrlName
  else
    lblFieldTitle.Name := CntrlName + '_2';

  lblFieldTitle.Parent := Self;
  lblFieldTitle.Left := 10;
  lblFieldTitle.Top := aNum * 42 + 6;
  lblFieldTitle.Caption := aDBField.FieldName;

  case aDBField.FieldType of
    ftInteger, ftString:
      begin
        if aDBField.FieldName.Contains('COLOR') then
          begin
            clrbxControl := TColorBox.Create(Self);

            clrbxControl.Name := 'cntrl' + aDBField.FieldName;
            clrbxControl.Parent := Self;
            clrbxControl.Left := 10;
            clrbxControl.Top := aNum * 42 + 20;
            clrbxControl.Selected := GetIntegerValue(aDBField.FieldName);
            clrbxControl.OnChange := CntrlChange;
          end
        else
          begin
            edtControl := TEdit.Create(Self);

            CntrlName := 'cntrl' + aDBField.FieldName;
            if Self.FindComponent(CntrlName) = nil then
              edtControl.Name := CntrlName
            else
              edtControl.Name := CntrlName + '_2';

            edtControl.Parent := Self;
            edtControl.Left := 10;
            edtControl.Top := aNum * 42 + 20;

            edtControl.Text := GetStringValue(aDBField.FieldName);
            edtControl.OnChange := CntrlChange;
          end;
      end;
  end;
end;

procedure TEntityPanelAbstract.BuildControls(aEntity: TEntityAbstract);
var
  Pair: TPair<string, TEntityAbstract>;
begin
  if aEntity = nil then Exit;

  InitPanel;
  FFieldsCount := 0;

  FEntity := aEntity;
  CreateEntityFields;

  for Pair in aEntity.Relations  do
    begin
      FEntity := Pair.Value;
      if Assigned(FEntity) then CreateEntityFields;
    end;

  FEntity := aEntity;
end;

constructor TEntityPanelAbstract.Create(aOwner: TWinControl);
begin
  inherited Create(aOwner);
  Self.Parent := aOwner;
  Self.Align := alClient;
end;

end.
