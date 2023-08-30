unit WorkerHouseCleaner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Worker, DBConnection;

const
  DEFAULT_MAX_AGE = 60;

type

  { TWorkerHouseCleaner }

  TWorkerHouseCleaner = class(TWorker)
  private
    fOlderThan: Int64;
    fConnectionMgr: TDBConnectionManager;
  protected
    procedure DoTask(); override;
  public
    constructor Create(ConnectionMgr: TDBConnectionManager; CreateSuspended: Boolean = True); reintroduce;
  end;

implementation

uses Data;

{ TWorkerHouseCleaner }

procedure TWorkerHouseCleaner.DoTask();
begin
  fConnectionMgr.Remove(fOlderThan * 1000);
{$IFDEF DEBUG}
  WriteLn('TWorkerHouseCleaner.DoTask(), Count: ', fConnectionMgr.Count());
{$ENDIF}
end;

constructor TWorkerHouseCleaner.Create(ConnectionMgr: TDBConnectionManager;
  CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fConnectionMgr:= ConnectionMgr;
  fOlderThan:= StrToIntDef(DataApp.Settings.Connection.Values['ConnectionMaxAge'], DEFAULT_MAX_AGE*1000);
end;

end.

