unit API_MVC;

interface

uses
   Vcl.Forms
  ,Vcl.Dialogs
  ,System.Classes
  ,System.SysUtils
  ,System.Generics.Collections
  ,API_DBases;

type
  //////////////////////////////////////////////////////////////////////////////
  // событие модели
  TEvent = procedure(EventName: string; EventData :TDictionary<string,variant>) of object;

  //////////////////////////////////////////////////////////////////////////////
  // модель
  TModelAbstract = class abstract
  private
    FEvent: TEvent;
    FGlobals: TObjectDictionary<string,TObject>;
  protected
    FControllerMessage: string;
    FDBEngine: TDBEngine;
    FEventData: TDictionary<string,variant>;
    FData: TDictionary<string,variant>;
    FObjData: TObjectDictionary<string,TObject>;
    procedure GenerateEvent(EventCode: string);
    // переопределяется в потомках
    procedure InputDataParce; virtual;
  public
    constructor Create(Data: TDictionary<string,variant>; ObjData, aGlobals: TObjectDictionary<string,TObject>); overload;
    destructor Destroy; override;
    // реализация в потомках
    procedure Execute; virtual; abstract;
    procedure ModelInitForThreads; virtual;
  public
    property Event: TEvent read FEvent write FEvent;
    property ControllerMessage: string read FControllerMessage write FControllerMessage;
  end;

  TModelClass = class of TModelAbstract;
  TModelMethod = procedure of object;

  //////////////////////////////////////////////////////////////////////////////
  // контроллер
  TControllerAbstract = class abstract
  protected
    FDBEngine: TDBEngine;
    FSettingFileName: String;
    FSettings: TStringList;
    FGlobals: TObjectDictionary<string, TObject>;
    FData: TDictionary<string, variant>;
    FObjData: TObjectDictionary<string, TObject>;
    FModel: TModelAbstract;
    procedure CallModel(aModelClass: TModelClass); virtual;
    // реализация в потомках
    procedure EventListener(aEventName: string; aEventData: TDictionary<string,variant>); virtual; abstract;
    procedure InitController; virtual; abstract;
    procedure PrepareModel(aMessage: string); virtual; abstract;
  public
    constructor Create; overload;
    procedure SendViewMessage(aMessage: string);
    destructor Destroy; override;
    property Model: TModelAbstract read FModel write FModel;
  end;

  TControllerClass = class of TControllerAbstract;

  //////////////////////////////////////////////////////////////////////////////
  // представление
  TViewAbstract = class abstract(TForm)
  private
    procedure DeInitMVC;
  protected
    FisMainForm: Boolean;
    FisReleased: Boolean;
    FController: TControllerAbstract;
    FControllerClass: TControllerClass;
    // реализация в потомках
    procedure InitMVC; virtual; abstract;
    procedure AfterControllerCreate; virtual;
    procedure SendViewMessage(aMsg: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    property ControllerClass: TControllerClass read FControllerClass write FControllerClass;
    property isReleased: Boolean read FisReleased write FisReleased;
  end;

implementation

procedure TViewAbstract.AfterControllerCreate;
begin
end;

procedure TModelAbstract.ModelInitForThreads;
begin
end;

procedure TModelAbstract.InputDataParce;
begin
  FDBEngine:=FGlobals.Items['DBEngine'] as TDBEngine;
end;

destructor TModelAbstract.Destroy;
begin
  inherited;
  FEventData.Free;
end;

procedure TModelAbstract.GenerateEvent(EventCode: string);
begin
  FControllerMessage:='';
  if Assigned(FEvent) then FEvent(EventCode, FEventData);
end;

constructor TViewAbstract.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TViewAbstract then Self.FController:=TViewAbstract(AOwner).FController;
  Self.OnCreate:=FormCreate;
  Self.OnDestroy:=FormDestroy;
end;

constructor TModelAbstract.Create(Data: TDictionary<System.string,Variant>; ObjData, aGlobals: TObjectDictionary<string,System.TObject>);
begin
  Self.FData:=Data;
  Self.FObjData:=ObjData;
  Self.FGlobals:=aGlobals;
  Self.InputDataParce;
  FEventData:=TDictionary<string,variant>.Create;
end;

destructor TControllerAbstract.Destroy;
begin
  FSettings.Free;
  if Assigned(FDBEngine) then FDBEngine.Free;
  FGlobals.Free;
  FObjData.Free;
  FData.Free;
  inherited;
end;

constructor TControllerAbstract.Create;
begin
  try
    inherited;
    Self.InitController;

    FSettings:=TStringList.Create;
    if not FSettingFileName.IsEmpty then FSettings.LoadFromFile(FSettingFileName);

    FGlobals:=TObjectDictionary<string,TObject>.Create;
    if Assigned(FDBEngine) then FGlobals.Add('DBEngine', FDBEngine);

    FData:=TDictionary<string,variant>.Create;
    FObjData:=TObjectDictionary<string,TObject>.Create([doOwnsValues]);
  except
    On E : Exception do
      begin
        ShowMessage('Ошибка создания контроллера:'+E.Message);
        RunError;
      end;
  end;
end;

procedure TControllerAbstract.SendViewMessage(aMessage: string);
begin
  try
    // формируем данные для модели
    Self.FData.Clear;
    Self.FObjData.Clear;
    Self.PrepareModel(aMessage);
  except
    On E : Exception do
       ShowMessage('Ошибка в сообщении вида "'+aMessage+'" :'+E.Message);
  end;
end;

procedure TControllerAbstract.CallModel(aModelClass: TModelClass);
begin
  FModel:=aModelClass.Create(Self.FData, Self.FObjData, Self.FGlobals);
  try
    FModel.Event:=Self.EventListener;
    FModel.Execute;
  finally
    FreeAndNil(FModel);
  end;
end;

procedure TViewAbstract.DeInitMVC;
begin
  FController.Free;
end;

procedure TViewAbstract.FormCreate(Sender: TObject);
begin
  if not Assigned(FController) then
    begin
      Self.InitMVC;
      FController:=FControllerClass.Create;
      FisMainForm:=True;
      AfterControllerCreate;
    end;
end;

procedure TViewAbstract.FormDestroy(Sender: TObject);
begin
  if FisMainForm then Self.DeInitMVC;
end;

procedure TViewAbstract.SendViewMessage(aMsg: string);
begin
  FController.SendViewMessage(aMsg);
end;

end.
