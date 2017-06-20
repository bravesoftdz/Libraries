unit API_ORM_Cntrls;

interface

uses
  System.Classes,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Forms,
  API_ORM;

type
  TCRUDPanelAbstract = class abstract(TPanel)
  protected
    FEntity: TEntityAbstract;
    procedure CreateFieldControl(aDBField: TDBField; aNum: Integer);
    procedure CreateButtons;
    function GetFieldValue(aFieldName: string): string;
  public
    btnApply: TButton;
    btnCancel: TButton;
    constructor Create(aOwner: TWinControl);
    destructor Destroy; override;
    procedure BuildCRUD(aEntity: TEntityAbstract);
    procedure UpdateEntity;
    property Entity: TEntityAbstract read FEntity;
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
      if Pair.Value.Data.ContainsKey(FieldName) then
        begin
          if Sender is TEdit then
            Pair.Value.Data.Items[FieldName] := (Sender as TEdit).Text;
          if Sender is TColorBox then
            Pair.Value.Data.Items[FieldName] := (Sender as TColorBox).Selected;
        end;

  FAfterEditChange(Sender as TControl);
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

destructor TCRUDPanelAbstract.Destroy;
begin
  if Assigned(FEntity) then FreeAndNil(FEntity);
  inherited;
end;

procedure TCRUDPanelAbstract.UpdateEntity;
var
  DBField: TDBField;
  edtControl: TEdit;
begin
  for DBField in FEntity.Fields do
    begin
      edtControl := Self.FindComponent('cntrl' + DBField.FieldName) as TEdit;
      if edtControl <> nil then FEntity.Data.Items[DBField.FieldName] := edtControl.Text;
    end;
end;

procedure TCRUDPanelAbstract.CreateButtons;
begin
  btnApply := TButton.Create(self);
  btnApply.Parent := Self;
  btnApply.Caption := 'Apply';
  btnApply.Left := 10;

  btnCancel := TButton.Create(self);
  btnCancel.Parent := Self;
  btnCancel.Caption := 'Cancel';
  btnCancel.Left := 90;
end;

function TCRUDPanelAbstract.GetFieldValue(aFieldName: string): string;
begin
  Result := FEntity.Data.Items[aFieldName];
end;

procedure TCRUDPanelAbstract.CreateFieldControl(aDBField: TDBField; aNum: Integer);
var
  lblFieldTitle: TLabel;
  edtControl: TEdit;
begin
  lblFieldTitle := TLabel.Create(Self);
  lblFieldTitle.Name := 'lbl' + aDBField.FieldName;
  lblFieldTitle.Parent := Self;
  lblFieldTitle.Left := 10;
  lblFieldTitle.Top := aNum * 42 + 6;
  lblFieldTitle.Caption := aDBField.FieldName;

  case aDBField.FieldType of
    ftInteger, ftString:
      begin
        edtControl := TEdit.Create(Self);
        edtControl.Name := 'cntrl' + aDBField.FieldName;
        edtControl.Parent := Self;
        edtControl.Left := 10;
        edtControl.Top := aNum * 42 + 20;

        edtControl.Text := GetFieldValue(aDBField.FieldName);
      end;
  end;
end;

procedure TCRUDPanelAbstract.BuildCRUD(aEntity: TEntityAbstract);
var
  DBField: TDBField;
  i: integer;
begin
  FEntity := aEntity;

  i := 0;
  for DBField in FEntity.Fields do
    begin
      CreateFieldControl(DBField, i);
      Inc(i);
    end;

  btnApply.Top := Self.Height - 30;
  btnCancel.Top := Self.Height - 30;
end;

constructor TCRUDPanelAbstract.Create(aOwner: TWinControl);
begin
  inherited Create(aOwner);
  Self.Parent := aOwner;
  Self.Align := alClient;

  CreateButtons;
end;

end.
