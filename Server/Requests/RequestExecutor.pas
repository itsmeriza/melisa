unit RequestExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Router, Request, DBConnection, Command;

type
  { TRequestExecutorHTTP }

  TRequestExecutorHTTP = class
  private
    fRouter: TRouter;
    fConnectionMgr: TDBConnectionManager;
    function DoCreateRequest(Command: TCommand): TRequest;
  public
    constructor Create(Router: TRouter; ConnectionMgr: TDBConnectionManager);
    destructor Destroy; override;

    procedure Fire(Command: TCommand);
  end;

implementation

uses HTTPServer;

{ TRequestExecutorHTTP }

function TRequestExecutorHTTP.DoCreateRequest(Command: TCommand): TRequest;
type
  TRequestClass = class of TRequest;
var
  o: TRequestClass;
begin
  o := TRequestClass(FindClass(Command.Route.ClassName));
  Result := o.Create(Command);
end;

constructor TRequestExecutorHTTP.Create(Router: TRouter;
  ConnectionMgr: TDBConnectionManager);
begin
  fRouter := Router;
  fConnectionMgr:= ConnectionMgr;
end;

destructor TRequestExecutorHTTP.Destroy;
begin
  inherited Destroy;
end;

procedure TRequestExecutorHTTP.Fire(Command: TCommand);
var
  req: TRequest;
begin
  req := DoCreateRequest(Command);
  if req = nil then
  begin
    TCommandHTTP(Command).ResponseInfo.ResponseNo := 400;
    Exit;
  end;

  try
    req.Perform();
  finally
    req.Free;
  end;
end;

end.

