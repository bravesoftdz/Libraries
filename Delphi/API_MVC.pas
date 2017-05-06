unit API_MVC;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Vcl.Forms;

type
  TModelAbstract = class;
  TModelClass = class of TModelAbstract;

  TViewAbstract = class;
  TViewAbstractClass = class of TViewAbstract;

  TControllerAbstract = class;
  TControllerClass = class of TControllerAbstract;

  TEventPerfom = procedure of object;
  TEventListener = procedure(aEventMsg: string) of object;

  // Model
  TModelAbstract = class abstract
  protected
    FData: TDictionary<string, variant>;
    FObjData: TObjectDictionary<string, TObject>;
    FOnEvent: TEventListener;
    procedure CreateEvent(aEventMsg: string; aEventProc: TEventPerfom = nil);
  public
    procedure Execute; virtual; abstract;
    constructor Create(aData: TDictionary<string, variant>;
      aObjData: TObjectDictionary<string, TObject>); virtual;
    property OnEvent: TEventListener read FOnEvent write FOnEvent;
  end;

  // View
  TViewAbstract = class abstract(TForm)
  private
    FIsMainForm: Boolean;
  protected
    FController: TControllerAbstract;
    FControllerClass: TControllerClass;
    procedure InitMVC; virtual; abstract;
    procedure SendMessage(aMsg: string);
    procedure FreeAfterClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // Controller
  TControllerAbstract = class abstract
  protected
    FData: TDictionary<string, variant>;
    FObjData: TObjectDictionary<string, TObject>;
    FMainView: TViewAbstract;
    procedure PerfomViewMessage(aMsg: string; aViewSender: TViewAbstract); virtual; abstract;
    procedure EventListener(aEventMsg: string); virtual; abstract;
    procedure CallView(aViewAbstractClass: TViewAbstractClass; aIsModal: Boolean = false);
    procedure CallModel(aModelClass: TModelClass); virtual;
  public
    procedure ReceiveViewMessage(aMsg: string; aViewSender: TViewAbstract);
    constructor Create(aMainView: TViewAbstract); virtual;
    destructor Destroy; override;
  end;

implementation

procedure TModelAbstract.CreateEvent(aEventMsg: string; aEventProc: TEventPerfom = nil);
begin
  //aEventProc;
  FOnEvent(aEventMsg);
end;

constructor TModelAbstract.Create(aData: TDictionary<string, variant>;
  aObjData: TObjectDictionary<string, TObject>);
begin
  FData := aData;
  FObjData := aObjData;
end;

destructor TControllerAbstract.Destroy;
begin
  FData.Free;
  FObjData.Free;

  inherited;
end;

procedure TControllerAbstract.CallModel(aModelClass: TModelClass);
var
  Model: TModelAbstract;
begin
  Model := aModelClass.Create(FData, FObjData);
  try
    Model.OnEvent := EventListener;
    Model.Execute;
  finally
    Model.Free;
  end;
end;

procedure TViewAbstract.FreeAfterClose(Sender: TObject; var Action: TCloseAction);
begin
  Release;
end;

procedure TControllerAbstract.CallView(aViewAbstractClass: TViewAbstractClass; aIsModal: Boolean = false);
var
  View: TViewAbstract;
begin
  View := aViewAbstractClass.Create(FMainView);

  if aIsModal then
    View.ShowModal
  else
    View.Show;
end;

procedure TControllerAbstract.ReceiveViewMessage(aMsg: string; aViewSender: TViewAbstract);
begin
  FData.Clear;
  FObjData.Clear;
  PerfomViewMessage(aMsg, aViewSender);
end;

procedure TViewAbstract.SendMessage(aMsg: string);
begin
  FController.ReceiveViewMessage(aMsg, Self);
end;

constructor TControllerAbstract.Create(aMainView: TViewAbstract);
begin
  FMainView := aMainView;
  FData := TDictionary<string, variant>.Create;
  FObjData := TObjectDictionary<string, TObject>.Create;
end;

constructor TViewAbstract.Create(AOwner: TComponent);
begin
  inherited;

  if AOwner is TViewAbstract then
    begin
      Self.FController := TViewAbstract(AOwner).FController;
      Self.OnClose := Self.FreeAfterClose;
    end;

  if not Assigned(FController) then
    begin
      FIsMainForm := True;
      Self.InitMVC;
      FController := FControllerClass.Create(TViewAbstract(Self));
    end;
end;

destructor TViewAbstract.Destroy;
begin
  if FIsMainForm then
    begin
      FController.Free;
    end;

  inherited;
end;

end.
