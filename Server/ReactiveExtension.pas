unit ReactiveExtension;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, rstomp, Utils, Data, RequestExecutor;

type
  { TSubscriber }

  TSubscriber = class
  private
    fHost: string;
    fPasscode: string;
    fPort: Word;
    fSubscriptionPaths: string;
    fUser: string;
    fSTOMP: TRSTOMP;
    fVhost: string;
    fSubscriptionIds: TStringList;
    procedure DoRecvConnected(Frame: TRStompFrame);
    procedure DoRecvError(Frame: TRStompFrame);
    procedure DoRecvMessage(Frame: TRStompFrame);
    procedure DoRecvReceipt(Frame: TRStompFrame);
    procedure DoSubscribeTopics();
  public
    constructor Create(Host: string; Port: Word); reintroduce;
    destructor Destroy; override;

    function Connect(): Boolean;
  published
    property Host: string read fHost write fHost;
    property Port: Word read fPort write fPort;
    property User: string read fUser write fUser;
    property Passcode: string read fPasscode write fPasscode;
    property Vhost: string read fVhost write fVhost;
    property SubscriptionPaths: string read fSubscriptionPaths write fSubscriptionPaths;
  end;

implementation

uses rjson;

{ TSubscriber }

procedure TSubscriber.DoRecvReceipt(Frame: TRStompFrame);
begin

end;

procedure TSubscriber.DoSubscribeTopics;
var
  topics: TStringList;
  i: Integer;
  key: String;
begin
  topics:= TStringList.Create;
  try
		TUtils.Split(topics, fSubscriptionPaths);
    for i:= 0 to topics.Count - 1 do
    begin
      key:= TUtils.CreateRandomKey();
      while fSubscriptionIds.IndexOf(key) >= 0 do
      	key:= TUtils.CreateRandomKey();

      fSubscriptionIds.Add(key);
      fSTOMP.Subscribe(key, topics[i]);
    end;
  finally
    topics.Free;
  end;
end;

procedure TSubscriber.DoRecvError(Frame: TRStompFrame);
begin

end;

procedure TSubscriber.DoRecvConnected(Frame: TRStompFrame);
begin

end;

procedure TSubscriber.DoRecvMessage(Frame: TRStompFrame);
begin
  if not fSTOMP.Connector.Connected then
  	Exit;

  try
    //TRequestExecutor(fReqExec).Fire();
  finally
  end;
end;

constructor TSubscriber.Create(Host: string; Port: Word);
begin
  inherited Create();

  fHost:= Host;
  fPort:= Port;
  fSubscriptionIds:= TStringList.Create;
  fSTOMP:= TRStomp.Create(fHost, fPort);
end;

destructor TSubscriber.Destroy;
begin
  fSubscriptionIds.Free;
  fSTOMP.Free;
  inherited Destroy;
end;

function TSubscriber.Connect: Boolean;
begin
  fSTOMP.Login:= fUser;
  fSTOMP.Passcode:= fPasscode;
  fSTOMP.Vhost:= fVhost;
  fSTOMP.OnRecvConnected:= @DoRecvConnected;
  fSTOMP.OnRecvError:= @DoRecvError;
  fSTOMP.OnRecvMessage:= @DoRecvMessage;
  fSTOMP.OnRecvReceipt:= @DoRecvReceipt;

  Result:= fSTOMP.Connect();

  DoSubscribeTopics();
end;

end.

