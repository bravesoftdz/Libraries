unit API_Controls;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants
  ,System.Classes
  ,System.JSON
  ,Vcl.ComCtrls
  ,Vcl.Menus
  ,System.UITypes
  ,Vcl.Forms
  ,Vcl.StdCtrls
  ,Vcl.Buttons
  ,Vcl.Controls
  ,Vcl.Dialogs;

type
  TArrayOfInteger = array of Integer;

  TJSONClass = class of TJSONAncestor;

  // Tree View с возможностью работы с JSON
  TJSNTreeView = class(TTreeView)
  private
    FPopupMenu: TPopupMenu;
    FjsnInput:  TJSONAncestor;
    function GetJSNObject(jsnInput: TJSONAncestor; NodeLevel: TArrayOfInteger; out JSONClass: TJSONClass): TJSONAncestor;
    function GetNodeLevel: TArrayOfInteger;
    procedure JSNTreeViewOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditJSONNode(Sender: TObject);
    procedure SaveJSNNode(Key, Value: String; NodeLevel: TArrayOfInteger);
  public
    procedure LoadJSN(jsnInput: TJSONObject);
    constructor Create(AOwner: TComponent); override;
  end;

  // Форма редактирования пары/значения JSON
  TfrmJSONEdit = class(TForm)
    lblKey: TLabel;
    lblValue: TLabel;
    edtKey: TEdit;
    edtValue: TEdit;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TJSNTreeView.SaveJSNNode(Key, Value: String; NodeLevel: TArrayOfInteger);
var
  jsnParent, jsnNewObj: TJSONAncestor;
  JSONClass: TJSONClass;
  i: Integer;
  fVal: Extended;
  VarValue: Variant;
  isFloat: Boolean;
begin
  i:=NodeLevel[High(NodeLevel)];
  SetLength(NodeLevel,Length(NodeLevel)-1);
  jsnParent:=Self.GetJSNObject(FjsnInput,NodeLevel,JSONClass);
  if jsnParent=nil then jsnParent:=FjsnInput;
  isFloat:=TryStrToFloat(Value, fVal);

  if JSONClass=TJSONArray then
    begin
      //jsnNewObj:=TJSONArray(jsnParent).Create(TJSONValue.Create(Value));
    end;
  if JSONClass=TJSONObject then
    begin
      if isFloat then
        jsnNewObj:=TJSONObject(jsnParent).Pairs[i].Create(Key, TJSONNumber.Create(fVal))
      else jsnNewObj:=TJSONObject(jsnParent).Pairs[i].Create(Key, Value);
    end;
  if JSONClass=TJSONPair then
    begin
      if isFloat then
        jsnNewObj:=TJSONObject(TJSONPair(jsnParent).JsonValue).Pairs[i].Create(Key, TJSONNumber.Create(fVal))
      else jsnNewObj:=TJSONObject(TJSONPair(jsnParent).JsonValue).Pairs[i].Create(Key, Value);
    end;

  Self.Selected.Text:=jsnNewObj.ToString;
end;

function TJSNTreeView.GetNodeLevel: TArrayOfInteger;
var
  Node: TTreeNode;
  i,j: Integer;
  ReversedNodeLevel:  TArrayOfInteger;
begin
  Node:=Self.Selected;
  while Node<>nil do
    begin
      SetLength(Result,Length(Result)+1);
      Result[High(Result)]:=Node.Index;
      Node:=Node.Parent;
    end;
  SetLength(ReversedNodeLevel,Length(Result));
  j:=0;
  for i:=Length(Result)-1 downto 0 do
    begin
      ReversedNodeLevel[j]:=Result[i];
      inc(j);
    end;
  Result:=ReversedNodeLevel;
end;

procedure TJSNTreeView.EditJSONNode(Sender: TObject);
var
  frmJSONEdit: TfrmJSONEdit;
  jsnObject: TJSONAncestor;
  JSONClass: TJSONClass;
begin
  frmJSONEdit:=TfrmJSONEdit.Create(Self);
  try
    jsnObject:=GetJSNObject(FjsnInput,Self.GetNodeLevel,JSONClass);

    if JSONClass=TJSONValue then
      begin
        frmJSONEdit.lblKey.Visible:=False;
        frmJSONEdit.edtKey.Visible:=False;
        frmJSONEdit.edtValue.Text:=TJSONValue(jsnObject).Value;
      end;
    if JSONClass=TJSONPair then
      begin
        frmJSONEdit.edtKey.Text:=TJSONPair(jsnObject).JsonString.Value;
        frmJSONEdit.edtValue.Text:=TJSONPair(jsnObject).JsonValue.Value;
      end;

    if frmJSONEdit.ShowModal = mrOk then Self.SaveJSNNode(frmJSONEdit.edtKey.Text, frmJSONEdit.edtValue.Text, Self.GetNodeLevel);
  finally
    frmJSONEdit.Free;
  end;
end;

procedure TJSNTreeView.JSNTreeViewOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P : TPoint;
begin
  if Button = mbRight then
    begin
      Self.Selected:=Self.GetNodeAt(X, Y);
      if Self.Selected.HasChildren then
           FPopupMenu.Items[0].Enabled:=False
      else FPopupMenu.Items[0].Enabled:=True;

      P.X:=X;
      P.Y:=Y;
      P:=Self.ClientToScreen(P);
      FPopupMenu.Popup(P.X,P.Y);
    end;
end;

constructor TJSNTreeView.Create(AOwner: TComponent);
var
  MenuItem: TMenuItem;
begin
  inherited;
  Self.RightClickSelect:=True;
  Self.MultiSelect:=False;
  Self.ReadOnly:=True;
  Self.OnMouseUp:=Self.JSNTreeViewOnMouseUp;

  // контекстное меню
  FPopupMenu:=TPopupMenu.Create(Self);
  MenuItem:=TMenuItem.Create(PopupMenu);
  MenuItem.Caption:='Edit';
  MenuItem.OnClick:=Self.EditJSONNode;
  FPopupMenu.Items.Add(MenuItem);
  FPopupMenu.AutoPopup:=False;
  Self.PopupMenu:=FPopupMenu;
end;

function TJSNTreeView.GetJSNObject(jsnInput: TJSONAncestor; NodeLevel: TArrayOfInteger; out JSONClass: TJSONClass): TJSONAncestor;
var
  i: Integer;
  tx:string;
begin
  result:=jsnInput;

  for i := 0 to High(NodeLevel) do
    begin
      if result is TJSONPair then
        begin
          result:=TJSONPair(result).JsonValue;
          tx:=result.ToString;
        end;

      if result is TJSONArray then
        begin
          if NodeLevel[i]>TJSONArray(result).Size-1 then
            begin
              Result:=nil;
              Exit;
            end;

          result:=TJSONArray(result).Get(NodeLevel[i]);
          tx:=result.ToString;
          Continue;
        end;

      if result is TJSONObject then
        begin
          if NodeLevel[i]>TJSONObject(result).Size-1 then
            begin
              Result:=nil;
              Exit;
            end;

          result:=TJSONObject(result).Get(NodeLevel[i]);
          tx:=result.ToString;
          Continue;
        end;
    end;

  if Result is TJSONValue then JSONClass:=TJSONValue;
  if Result is TJSONPair then JSONClass:=TJSONPair;
  if Result is TJSONObject then JSONClass:=TJSONObject;
  if Result is TJSONArray then JSONClass:=TJSONArray;
end;

procedure TJSNTreeView.LoadJSN(jsnInput: TJSONObject);
var
  jsnObject: TJSONAncestor;
  isContinue: Boolean;
  TreeNode: TTreeNode;
  NodeLevel: TArrayOfInteger;
  isValueObject: Boolean;
  JSONClass: TJSONClass;
begin
  if jsnInput.Size=0 then Exit;

  isContinue:=True;
  TreeNode:=nil;
  SetLength(NodeLevel,1);
  while isContinue do
    begin
      // получаем объект, оперделяем класс объекта
      jsnObject:=Self.GetJSNObject(jsnInput, NodeLevel, JSONClass);

      // конец уровня
      if jsnObject=nil then
        begin
          SetLength(NodeLevel,Length(NodeLevel)-1);
          if Length(NodeLevel)=0 then isContinue:=False
          else
            begin
              TreeNode:=TreeNode.Parent;
              Inc(NodeLevel[High(NodeLevel)]);
            end;
          Continue;
        end;

      // если объект - пара
      if JSONClass = TJSONPair then
        begin
          isValueObject:=False;
          if (TJSONPair(jsnObject).JsonValue is TJSONObject) or (TJSONPair(jsnObject).JsonValue is TJSONArray) then
            begin
              TreeNode:=Self.Items.AddChild(TreeNode, TJSONPair(jsnObject).JsonString.ToString);

              SetLength(NodeLevel,Length(NodeLevel)+1);
              isValueObject:=True;
            end;
          if not isValueObject then
            begin
              Self.Items.AddChild(TreeNode, jsnObject.ToString);
              Inc(NodeLevel[High(NodeLevel)]);
            end;
        end;

      // если объект - объект
      if JSONClass = TJSONObject then
        begin
          TreeNode:=Self.Items.AddChild(TreeNode, '');
          SetLength(NodeLevel,Length(NodeLevel)+1);
        end;

      // если объект - значение
      if JSONClass = TJSONValue then
        begin
          Self.Items.AddChild(TreeNode, jsnObject.ToString);
          Inc(NodeLevel[High(NodeLevel)]);
        end;
    end;

  FjsnInput:=jsnInput;
end;

end.
