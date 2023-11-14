unit ReactiveExtension;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, rstomp, Utils, Data, RequestExecutor, Connection, Router,
  Command;

type
  TCommandStomp = class(TCommand)
  public
  end;

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
    fConnectionMgr: TConnectionManager;
    fReqExec: TRequestExecutor;
    fRouter: TRouter;
    procedure DoRecvConnected(Frame: TRStompFrame);
    procedure DoRecvError(Frame: TRStompFrame);
    procedure DoRecvMessage(Frame: TRStompFrame);
    procedure DoRecvReceipt(Frame: TRStompFrame);
    procedure DoSubscribeTopics();
  public
    constructor Create(Router: TRouter; ConnectionMgr: TConnectionManager); reintroduce;
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

uses rjson, Commons;

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
var
  cmd: TCommandStomp;
begin
  if not fSTOMP.Connector.Connected then
  	Exit;

  cmd:= TCommandStomp.Create;
  cmd.Route:= fRouter.GetRoute(
  	Frame.GetHeaderValue('Document'),
    Frame.GetHeaderValue('Command-Type')
  );

  try
    try
    	fReqExec.Fire(cmd);
    except
    	on E: Exception do
      	WriteLn(E.ClassName, ': ', E.Message);
    end;
  finally
  	cmd.Free;
  end;
end;

constructor TSubscriber.Create(Router: TRouter;
  ConnectionMgr: TConnectionManager);
begin
  inherited Create();

  fRouter:= Router;
  fSubscriptionIds:= TStringList.Create;
  fSTOMP:= TRStomp.Create(
  	DataApp.Settings.MessageBroker.Values['Host'],
    StrToIntDef(DataApp.Settings.MessageBroker.Values['Port'], STOMP_PORT_DEFAULT)
  );
  fConnectionMgr:= ConnectionMgr;
  fReqExec:= TRequestExecutor.Create(fRouter, fConnectionMgr);
end;

destructor TSubscriber.Destroy;
begin
  fSubscriptionIds.Free;
  fSTOMP.Free;
  fReqExec.Free;
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

