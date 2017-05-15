unit API_CRUD;

interface

uses
  System.Classes,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Controls,
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

implementation

uses
  System.SysUtils,
  Data.DB;

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
