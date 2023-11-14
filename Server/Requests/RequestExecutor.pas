unit RequestExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Router, Request, Connection, Command;

type

  { TRequestExecutor }

  TRequestExecutor = class
  private
    fRouter: TRouter;
    fConnectionMgr: TConnectionManager;
    function DoCreateRequest(Command: TCommand): TRequest;
  public
    constructor Create(Router: TRouter; ConnectionMgr: TConnectionManager); reintroduce;
    destructor Destroy; override;

    procedure Fire(Command: TCommand);
  end;

  { TRequestExecutorHTTP }


implementation

{ TRequestExecutor }

function TRequestExecutor.DoCreateRequest(Command: TCommand): TRequest;
type
  TRequestClass = class of TRequest;
var
  o: TRequestClass;
begin
  o := TRequestClass(FindClass(Command.Route.ClassName));
  Result := o.Create(Command);
end;

constructor TRequestExecutor.Create(Router: TRouter;
  ConnectionMgr: TConnectionManager);
begin
  fRouter := Router;
  fConnectionMgr:= ConnectionMgr;
end;

destructor TRequestExecutor.Destroy;
begin
  inherited Destroy;
end;

procedure TRequestExecutor.Fire(Command: TCommand);
var
  req: TRequest;
begin
  req := DoCreateRequest(Command);

  try
    req.Perform();
  finally
    req.Free;
  end;
end;

end.

