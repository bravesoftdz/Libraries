unit API_Yandex;

interface

uses
   System.SysUtils
  ,IdHTTP
  ,IdSSLOpenSSL;

type
  TYaTranslater = class(TObject)
  private
    FAPIKey: string;
    FAPIKeys: TArray<string>;
    FAPIKeyIndx: Integer;
    FHTTP: TIdHTTP;
    FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
    procedure HTTPInit;
    procedure UseNextAPIKey;
  public
    constructor Create(aAPIKey: string = '');
    destructor Destroy; override;
    function Translate(aLang: string; aText: string): string;
  end;

implementation

uses
   System.Classes
  ,System.JSON
  ,Dialogs;

procedure TYaTranslater.UseNextAPIKey;
begin
  Inc(FAPIKeyIndx);
  if FAPIKeyIndx < Length(FAPIKeys) then
    FAPIKey := FAPIKeys[FAPIKeyIndx]
  else
    begin
      ShowMessage('Tranlsater error: No Acceptble API Keys');
      Abort;
    end;
end;

function TYaTranslater.Translate(aLang: string; aText: string): string;
var
  PostData: TStringList;
  url, Response: string;
  jsnObject: TJSONObject;
  isContiniue: Boolean;
  i: integer;
begin
  if aText.IsEmpty then Exit('');

  url:='https://translate.yandex.net/api/v1.5/tr.json/translate';
  isContiniue:=True;
  i:=0;

  PostData:=TStringList.Create;
  try
    PostData.Add('key=' + FAPIKey);
    PostData.Add('lang=' + aLang);
    PostData.Add('text=' + aText);

    while isContiniue do
    try
      inc(i);
      Response:=FHTTP.Post(url, PostData);
      try
        jsnObject:=TJSONObject(TJSONObject.ParseJSONValue(Response));
        Result:=TJSONArray(jsnObject.Values['text']).Items[0].Value;
        isContiniue:=False;
      finally
        jsnObject.Free;
      end;
    except
      On E : Exception do
      begin
        if FHTTP.ResponseCode = 403 then  // API key запрещён
          begin
            UseNextAPIKey; // замена ключа
            PostData.Add('key=' + FAPIKey);
            i:=0;
          end;

        if i>4 then
          begin
            ShowMessage('Tranlsater error: (' + aLang + ') '+aText+', '+E.Message);
            Abort;
          end
        else
          begin
            Sleep(2000);
            Self.HTTPInit;
          end;
      end;
    end;
  finally
    PostData.Free;
  end;
end;

constructor TYaTranslater.Create(aAPIKey: string = '');
begin
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20160922T064903Z.c2b8d5733448ac48.6b8b7624d9c371f13f23dfc83d070036eb1773b8'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161111T140303Z.c39a880abc2249fa.c73910cd733c362f12a7797abbca0ba1a90c1005'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T132317Z.95f24e5e3c89edc7.d5b718c209996dd97fb81f38ff9be0de16e7b6a8'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T132654Z.b5c00f5d64ed9c95.69157f5d292cde51d87d5d1d0f52994b63111d63'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T132917Z.d70f827e53d06e56.2b67d27a881888d584c497f949652e9c4bc752ae'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T133159Z.9c68fb5f74a97be4.6632d10867a9d7940266c8181b8eaa5c6630a747'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T133403Z.2780dcc2e992c946.ba7b640c836c92955370da5fbce3b5ae3b392d84'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T133553Z.278d5e44bba8c8ed.407ae4a2338430ace21dd8bd441b632c6e04869e'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T133759Z.9d7bd55c14496798.38cf9c3b490d0185f2210c6c3a92c66d0fbace23'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T133955Z.585634ad212d04f2.bbb98280d2c70fdbaca8b4a2053a6e19f72c4018'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161112T134138Z.ad4bb016e76f37d6.fce66033940c3819e0332808f1f21c8b2fa4a60b'];
  //1x	
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T161721Z.7397ce14f536a6a5.520134660af8969080996d9c014c454e2270392b'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T161937Z.91ba9600d7e53fe5.13e426b1b0361f7253bf0049bbdd4d304b72c405'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T162128Z.8dadedce0364a1fb.8123b8c586d56581fda2e893b7fcd449f33fa5b8'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T162415Z.2d31f3df718c29c0.1dfadfd60063c5da1ed0ac8120fa9a81dfddd1e7'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T162636Z.852032449e7bef51.a816e6bcd1c6746e64484e2223aa0afc4376610b'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T163909Z.c627ee93de960bfa.fe81dce0dee15b99d3b83fbd1cbd1edf9a8bd84a'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T164042Z.0679929be2d58a31.15b0df38ae6098e87227a3f5f4d1457dd45a5c4e'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T164310Z.20042b66a2a342bf.be1b6de0e0bf1ba3b09028914341203c555a2d02'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T164452Z.03c661e4d7b603fd.0074891a6532c7fb97692d0c5aa279f62a335f76'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161113T164614Z.f088ac1d4ca146ae.8d803d962dedab057ba1415267098899da3ff4d2'];
  //2x
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T162134Z.54cd14ec13269354.1288ad134c1634c99f33fe3ccd49a10f40ad9762'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T162319Z.06394f7dc4e920cf.08e5937d8444d6127aadb141fe3d1b3429debc3f'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T162520Z.fb84fe3b47ee2bdc.826bd43305a69b17a257247991c2a611c1d24923'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T162651Z.ec9c065ee8d268e1.a12e673125be82949b7727305cffb8889e611106'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T162833Z.318f5786c1886b92.bb4275f8350d8031419edcdc62ef6db820af9c20'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T163012Z.e6a78b18c9320d5f.719db80c100c4c6bca89f31db89c7c34536e9625'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T163138Z.6fe1b5422a58028a.98188b996ad6d297af457282b33e5fce9ea4ad6a'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T163327Z.697a70886fce6b5f.213ca800af7089697f60896d7df1c5415bfc247b'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T163554Z.51fe73d305f4e0df.3d7388c7e7b7eda3a132e5950a031bcdbf117ac4'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161114T163715Z.76f807670b4c96ba.99244b17a7fc374845bf9831713ccd40828016e1'];
  //3x
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T191600Z.3a71268a494ab502.f78bbbd4504f31b95c5dbdb50249e682e6cb901d'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T191807Z.caf6b5f75a33b1f3.1ed9544e3153ec3736279fdcdb75bc38182c0403'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T192021Z.ec5a801d987d255e.a39d331d266b879cbd8bcb9c76e1c511a505c75d'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T192201Z.8891c2b1e4cfd247.6258cf07b88f9cf599e7c53eff30e25cb4a0b4fb'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T192352Z.75013ba8cfe917b4.ef0ee009b6c60414152ae4389432c5c982d9baad'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T193218Z.49b5e06613e9fe8e.4f85b4228eeeaac0de30386109d7ce64543cf67f'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T193437Z.28603d4697339e07.56085f506279b84a611ed9c00fd4daa8f7b06cec'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T193616Z.4b0c3b4880f46f36.b2d90267fcfb3b4237061a252b5af51b9b53f515'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T193749Z.105eca86dc8639ee.c3346e7933088ad6e6902cdad4724ac866190f70'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161122T193935Z.4c13aeeea5ace3cc.ea37697c5d111d93a5c9cc3639f1c6cfb9b8a841'];
  //4x
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T170551Z.c929763596acf25e.7b3c9d1b5321836108b50db020aa51d3194b5700'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T170747Z.9dba0424a36c89f0.38d29c22f512d13a53d830466766cdb124c8eb38'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T171107Z.96053195647a641a.32450fefb8ebf8922f4d6740a9b859dcb5ca4ab2'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T171256Z.2054408df493e011.18671394d27e53e9bf5262796272d7bf8ff3d89e'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T171428Z.e67b6ffa5dc95ee6.5470cafc36bb05fa00192efd5ab4347b115c228f'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T171603Z.72f71003df9b9980.99223f6db3c94a3eb7f2bf1f759af9b073e942f2'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T171736Z.c2bc79ffe936783a.42135cfc5d2c461965d5d070b9e52a0ca4b88e45'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T171854Z.e303451e35d7a47d.c35accec74cb889294a71a9701b9844dce4c3f7d'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T172042Z.0017246c52927072.a34a91ae80c7a7b9ca6f4030c0a2a999615ed6eb'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161124T172213Z.adaa1fe40b45d61f.64fc09bca16de52e164d1c13b4495c2d3b5617bd'];
  // 5x
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T064355Z.17ea7d50dd4401a4.32c1accf936d085cbf9d1d08cbcbd287cc4059c4'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T064533Z.2864415e8d0e0cd5.291ae14815a090976136ce135a65298dfd1ba50d'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T064714Z.717b99bef1037123.5f0e4949c8c967a37752050ec4e242799c1a2bc2'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T064859Z.ae2dc6bc4fb740ef.1f312e81de8b96e5a22ca558eb048ce245dacf62'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T065426Z.4f1adc38aee26c87.5d9e3cd0bf6d22c054c99c921c8a3462db11df55'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T065558Z.51e699e1cb620bfe.fc562c2ce9e46260242223368e03e85f4e38460b'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T070132Z.9cb35d9365b56ee7.f5ae5f993a0e99fe0efe3ec7d483e63271d3db52'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T070411Z.a2fc88250f7a91e6.e62b097d5c079e09b6de38b993aff4905aa1cd37'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T070523Z.9ae0ff88f878e726.eb8e4b408976ab355fe231983ef509ae9575b798'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T070650Z.2cd03b57999e5568.3868dbcfbc662b4d0538c399cae85ca974cabfd7'];
  // 6x
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T080931Z.e672e6a3b819a93e.1276f2249bb9fa8eccd8435440778df887af7d08'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T081054Z.d5b1634699913b3b.423d1881d7b4500e2e7bdc7ef85aec1a2e2430f5'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T081316Z.88beaa51fd756b4e.388574d7cdee792d41cdfae22e51f74fcef615c3'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T081437Z.d9fac28b4b053c84.5ea81187a89be007a8a014e31a55e9012d0e4ed9'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T081723Z.f69de15c151b25e1.0a1903942e1f8da73555da4b2615d76c99a33755'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T081841Z.599e51e0787e16f1.07a1e7e92409d02e3c2b9906d2d5f9e3ea7f05a1'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T081947Z.89d9395866873106.40986a94877712ac0b5d78bad4ea18b4569507af'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T082101Z.27d3aaca95985b97.5dd8b99adf819585f710d47d35d7ec19321b0c5e'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T082218Z.ec83ff4064ed8d6c.b19468167a6ec20c1d4acf70a178a4dae88d2fb0'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T082323Z.f29ab0c9fb05ad70.7161392c7cdeee361b5b23335ef2a181d31423f8'];
  // 7x
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T103808Z.23771fc121d6581d.6057a3a658630179ac76fd008ade8b32044eb5ea'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T103924Z.a6579e19485a245a.60af723cc0ee82093168b506c998966b825c77b5'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T104059Z.6cb8721a6e8899c9.bb7bd17044ff08f1e276faed85d2676aba961ec8'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T104213Z.5bc4837c6e8a5a72.8c0bdf2879c2bd3935e5a60e82c6dbca2db0ff7d'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T104720Z.0bdf216f839fda38.dd0d6f848b9da4bdbbeaf07332de7df61694d796'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T104837Z.daf5fcad41b05421.f581459bb05219249c11cdae97ec6b03a789f987'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T104943Z.befd51b37481465e.7820e8fefa39bfb21a0c9f1376931e3908e79991'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T105059Z.d6bb62c93c470168.39d0fd019590f49085cafef92f7565f374dc0949'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T141903Z.e6c2bd937b122dbb.f283078f2f23c7d6e4595e165dcf1dd6d6334c66'];
  FAPIKeys := FAPIKeys + ['trnsl.1.1.20161125T162711Z.ced8c5a22a1a08f3.fe6ea2614722736d60f3c2e0d1c86b09ca66bc33'];

  FAPIKeyIndx := 0;
  if aAPIKey.Length > 0 then
    FAPIKey := aAPIKey
  else
    FAPIKey := FAPIKeys[FAPIKeyIndx];

  Self.HTTPInit;
end;

procedure TYaTranslater.HTTPInit;
begin
  if Assigned(FHTTP) then FreeAndNil(FHTTP);
  if Assigned(FIdSSLIOHandlerSocketOpenSSL) then FreeAndNil(FIdSSLIOHandlerSocketOpenSSL);

  FHTTP := TIdHTTP.Create;
  FHTTP.HandleRedirects:=True;
  FHTTP.Request.UserAgent:='Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.62 Safari/537.36';

  FIdSSLIOHandlerSocketOpenSSL:=TIdSSLIOHandlerSocketOpenSSL.Create;
  FHTTP.IOHandler:=FIdSSLIOHandlerSocketOpenSSL;
end;

destructor TYaTranslater.Destroy;
begin
  FHTTP.Free;
  FIdSSLIOHandlerSocketOpenSSL.Free;
  inherited;
end;

end.
