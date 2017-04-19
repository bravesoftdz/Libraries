unit API_Threads;

interface

uses
   System.Threading
  ,System.Generics.Collections
  ,API_MVC;

type
  //////////////////////////////////////////////////////////////////////////////
  // модель c поддержкой многопоточности
  TModelThreadClass = class of TModelThread;

  TModelThread = class(TModelAbstract)
  protected
    FThreadNum: Integer; // номер текущего потока
  public
    constructor Create(Data: TDictionary<string,variant>; ObjData, aGlobals: TObjectDictionary<string,TObject>; aThreadNum: integer); overload;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // контроллер c поддержкой многопоточности
  TControllerThreadClass = class of TControllerThread;

  TControllerThread = class(TControllerAbstract)
  private
    FisThreadMode: Boolean; // флаг режима контроллера
  private
    // в режиме управл€ющего потоками контроллера
    FModelClass: TModelThreadClass; // класс модели дл€ запуска в потоке
    FControllerClass: TControllerThreadClass;
    FMessage: string;
    FControllers: TArray <TControllerThread>;
    FTasks: TArray <ITask>;
    FThreadNumToCreate: integer;
    procedure TaskProc(Sender: TObject);
    procedure InitModel(aModelClass: TModelClass);
    // в режиме подчиннЄного контроллера в отдельном потоке
  private
    FThreadNum: Integer; // номер текущего потока
  protected
    procedure CallModelInThreads(aThreadsCount: Integer; aModelClass: TModelThreadClass; aControllerClass: TControllerThreadClass; aMessage: string);
    procedure SendMessageToThreadModel(aThreadNum: Integer; aMessage: string);
    procedure CallModel(aModelClass: TModelThreadClass); overload; // вызов модели с поддержкой многопоточности
  public
    constructor Create(aThreadNum: integer); overload; // создание контроллера в режиме подчиннЄного
  end;

implementation

uses
   System.SysUtils;

constructor TModelThread.Create(Data: TDictionary<string,variant>; ObjData, aGlobals: TObjectDictionary<string,TObject>; aThreadNum: integer);
begin
  Create(Data, ObjData, aGlobals);
  FThreadNum:=aThreadNum;
end;

procedure TControllerThread.CallModel(aModelClass: TModelThreadClass);
begin
  FModel:=aModelClass.Create(Self.FData, Self.FObjData, Self.FGlobals, FThreadNum);
  try
    FModel.Event:=Self.EventListener;
    FModel.Execute;
  finally
    FreeAndNil(FModel);
  end;
end;

constructor TControllerThread.Create(aThreadNum: Integer);
begin
  Create;
  FThreadNum:=aThreadNum;
  FisThreadMode:=True;
end;

procedure TControllerThread.SendMessageToThreadModel(aThreadNum: Integer; aMessage: string);
begin
  if Length(FControllers)-1>=aThreadNum then
    if Assigned(FControllers[aThreadNum]) then FControllers[aThreadNum].Model.ControllerMessage:=aMessage;
end;

procedure TControllerThread.InitModel(aModelClass: TModelClass);
var
  Model: TModelAbstract;
begin
  Model:=aModelClass.Create(Self.FData, Self.FObjData, Self.FGlobals);
  try
    Model.ModelInitForThreads;
  finally
    Model.Free;
  end;
end;

procedure TControllerThread.TaskProc(Sender: TObject);
var
  Controller: TControllerThread;
begin
  Inc(FThreadNumToCreate);
  Controller:=FControllerClass.Create(FThreadNumToCreate);
  try
    FControllers:=FControllers+[Controller];
    Controller.SendViewMessage(FMessage);
  finally
    Controller.Free;
  end;
end;

procedure TControllerThread.CallModelInThreads(aThreadsCount: Integer; aModelClass: TModelThreadClass; aControllerClass: TControllerThreadClass; aMessage: string);
var
  i: Integer;
  Task: ITask;
  Data: TObjectDictionary<string,TObject>;
begin
  if not Self.FisThreadMode then
    begin
      FThreadNumToCreate := 0;
      for i := 1 to aThreadsCount do
        begin
          // инициализаци€
          Self.InitModel(aModelClass);

          // создание потоков
          Self.FModelClass:=aModelClass;
          Self.FControllerClass:=aControllerClass;
          Self.FMessage:=aMessage;
          Task := TTask.Create(Self, TaskProc);
          FTasks := FTasks + [Task];
          Task.Start;
        end;
    end
  else Self.CallModel(aModelClass);
end;

end.
