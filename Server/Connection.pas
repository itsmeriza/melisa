unit Connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql56conn, MTypes, SQLBuilder,
  IdCustomHTTPServer, StrUtils, DateUtils, rstomp, Worker;

const
  DEFAULT_MAX_AGE = 60;

type
  TConnectionManager = class;

  { TCleaner }

  TCleaner = class(TWorker)
  private
    fOlderThan: Int64;
    fConnectionMgr: TConnectionManager;
  protected
    procedure DoTask(); override;
  public
    constructor Create(ConnectionMgr: TConnectionManager; CreateSuspended: Boolean = True); reintroduce;
  end;

  { TConnection }

  TConnection = class
  private
    fId: string;
    fInfo: PConnectionInfo;
    fIsUsed: Boolean;
    fLastUsed: TDateTime;
    fOwner: TConnectionManager;
  public
    constructor Create(AOwner: TConnectionManager; AInfo: PConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;

    procedure Close(); virtual;
    procedure Open(); virtual;

    property Info: PConnectionInfo read fInfo;
  published
    property Id: string read fId write fId;
    property Owner: TConnectionManager read fOwner write fOwner;
    property IsUsed: Boolean read fIsUsed write fIsUsed;
    property LastUsed: TDateTime read fLastUsed write fLastUsed;
  end;

  { TDBConnection }

  TConnectionDB = class(TConnection)
  private
    fSession: TIdHTTPSession;
  protected
    fTransaction: TSQLTransaction;
  public
    constructor Create(AOwner: TConnectionManager; AInfo: PConnectionInfo); override;
    destructor Destroy; override;

    procedure Commit(); virtual; abstract;
    procedure Rollback(); virtual; abstract;
  published
    property Session: TIdHTTPSession read fSession write fSession;
  end;

  TCustomConnectionDB = class(TConnectionDB)
  public
    property Info;
  published
    property Id;
    property Owner;
  end;

  { TConnectionSTOMP }

  TConnectionSTOMP = class(TConnection)
  private
    fStomp: TRSTOMP;
	public
    constructor Create(AOwner: TConnectionManager; AInfo: PConnectionInfo); override;
    destructor Destroy; override;

    procedure Close(); override;
    procedure Open(); override;
  end;

  { Dummy class.
    Will be replaced with mORMot's TMongoClient class. }
  TMongoClient = class

  end;

  { TSQLDBConnection }

  TSQLDBConnection = class(TCustomConnectionDB)
  private
    fConnection: TSQLConnector;
    fSQL: TSQLBuilder;
    procedure AfterConnect(Sender: TObject);
    procedure BeforeDisconnect(Sender: TObject);
  public
    constructor Create(AOwner: TConnectionManager; AInfo: PConnectionInfo); override;
    destructor Destroy; override;

    procedure Close(); override;
    procedure Open(); override;

    procedure Commit(); override;
    procedure Rollback(); override;
  published
    property Connection: TSQLConnector read fConnection write fConnection;
    property SQL: TSQLBuilder read fSQL write fSQL;
  end;

  { TNOSQLDBConnection }

  TNOSQLDBConnection = class(TCustomConnectionDB)
  private
    fConnection: TMongoClient;
  public
    procedure Open(); override;
    procedure Close(); override;
    procedure Commit(); override;
    procedure Rollback(); override;
  published
    property Connection: TMongoClient read fConnection write fConnection;
  end;

  { TConnectionManager }

  TConnectionManager = class
  private
    fConnectionInfos: TListConnectionInfo;
    fConnections: TThreadList;

    procedure DoInitConnections();
  protected
    fConnectionId: string;
    fCleaner: TCleaner;
    function DoCreateConnection(c: PConnectionInfo): TConnection; virtual;
  public
    constructor Create(AConnectionInfos: TListConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;

    function CreateConnection(AConnectionId: string): TConnection;
    function GetConnection(AConnectionId: string): TConnection;
    function Count(): Integer;
    procedure Close(Connection: TConnection);
    procedure Remove(OlderThan: Int64 = 60000);
  end;


implementation

uses Utils, Data, Commons;

{ TCleaner }

procedure TCleaner.DoTask;
begin
  fConnectionMgr.Remove(fOlderThan * 1000);
{$IFDEF DEBUG}
  WriteLn('TWorkerHouseCleaner.DoTask(), Count: ', fConnectionMgr.Count());
{$ENDIF}
end;

constructor TCleaner.Create(ConnectionMgr: TConnectionManager;
  CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fConnectionMgr:= ConnectionMgr;
  fOlderThan:= StrToIntDef(DataApp.Settings.Connection.Values['ConnectionMaxAge'], DEFAULT_MAX_AGE*1000);
end;

{ TConnectionSTOMP }

constructor TConnectionSTOMP.Create(AOwner: TConnectionManager;
  AInfo: PConnectionInfo);
begin
  inherited Create(AOwner, AInfo);

  fStomp:= TRSTOMP.Create(AInfo^.Server, AInfo^.Port);
  with fStomp do
  begin
    Vhost := fInfo^.Database;
    Login := fInfo^.Username;
    Passcode := fInfo^.Password;
  end;
end;

destructor TConnectionSTOMP.Destroy;
begin
  inherited Destroy;
end;

procedure TConnectionSTOMP.Close;
begin
  inherited;
  fStomp.Disconnect();
end;

procedure TConnectionSTOMP.Open;
begin
  inherited;
  fStomp.Connect();
end;

{ TConnectionManager }

procedure TConnectionManager.DoInitConnections;
var
  i, ii: Integer;
  c: TConnection;
  list: TList;
  ci: PConnectionInfo;
begin
{$IFDEF DEBUG}
  WriteLn('Initiate connection.');
{$ENDIF}
	for i:= 0 to DataApp.Settings.ConnectionInfos.Count() - 1 do
  begin
    ci:= DataApp.Settings.ConnectionInfos[i];
    for ii:= 0 to ci^.MinConnection -1 do
    begin
      c:= DoCreateConnection(ci);
      c.Open();

      list:= fConnections.LockList;
      try
        list.Add(c);
      finally
        fConnections.UnlockList;
      end;
    end;
  end;
{$IFDEF DEBUG}
  WriteLn('Finished to initiate connections. Count: ', Count());
{$ENDIF}
end;

function TConnectionManager.DoCreateConnection(c: PConnectionInfo): TConnection;
begin
  if AnsiContainsText(c^.DriverName, SQL_CONNECTION_DRIVER_NAME_INITIAL) then
    Result := TSQLDBConnection.Create(Self, c)
  else if AnsiContainsText(c^.DriverName, MQ_CONNECTION_DRIVER_NAME_INITIAL) then
    Result := TConnectionSTOMP.Create(Self, c);

  Result.Id:= c^.ConnectionId;
end;

constructor TConnectionManager.Create(AConnectionInfos: TListConnectionInfo);
begin
  inherited Create();
  fConnectionInfos := AConnectionInfos;
  fConnections := TThreadList.Create;
  fCleaner:= TCleaner.Create(Self);
  fCleaner.Start();

  // this procedure cause lagging response after cleaning by the worker.
  //DoInitConnections();
end;

destructor TConnectionManager.Destroy;
begin
  TUtils.ClearList(fConnections);
  fConnections.Free;

  fCleaner.Terminate;
  fCleaner.Free;
  inherited Destroy;
end;

function TConnectionManager.CreateConnection(AConnectionId: string
  ): TConnection;
var
  list: TList;
  c: PConnectionInfo;
begin
  Result := GetConnection(AConnectionId);
  if Result <> nil then
  begin
    Result.IsUsed:= True;
    Exit;
  end;

  c:= DataApp.Settings.GetConnectionInfo(AConnectionId);
  Result := DoCreateConnection(c);
  Result.IsUsed:= True;

  list:= fConnections.LockList;
  try
    list.Add(Result);
{$IFDEF DEBUG}
    WriteLn('Count: ', list.Count);
{$ENDIF}
  finally
    fConnections.UnlockList;
  end;
end;

function TConnectionManager.GetConnection(AConnectionId: string): TConnection;
var
  i: Integer;
  c: TConnection;
  list: TList;
begin
  Result := nil;

  // get free connection.
  list:= fConnections.LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      c := TConnection(list.Items[i]);
      if not c.IsUsed and AnsiSameText(c.Id, AConnectionId) then
      begin
        Result := c;
        Exit;
      end;
    end;
{$IFDEF DEBUG}
    WriteLn('Count: ', list.Count);
{$ENDIF}
  finally
    fConnections.UnlockList;
  end;
end;

function TConnectionManager.Count: Integer;
var
  list: TList;
begin
  list:= fConnections.LockList;
  try
    Result:= list.Count;
  finally
    fConnections.UnlockList;
  end;
end;

procedure TConnectionManager.Close(Connection: TConnection);
begin
  Connection.Close();
end;

procedure TConnectionManager.Remove(OlderThan: Int64);
var
  list: TList;
  i: Integer;
  c: TConnection;
  deleted: TList;
  age: Int64;
begin
  list:= fConnections.LockList;
  deleted:= Classes.TList.Create;
  try
    if list.Count = 0 then
      Exit;

    c:= TConnection(list[0]);
    for i:= 0 to list.Count - 1 do
    begin
      c:= TConnection(list[i]);
      age:= MilliSecondsBetween(Now(), c.LastUsed);
      if not c.IsUsed and (age > OlderThan) then
        deleted.Add(c);
    end;

    for i:= 0 to deleted.Count - 1 do
    begin
      c:= TConnection(deleted[i]);
      list.Remove(c);
      c.Close();
      c.Free;
    end;

{$IFDEF DEBUG}
    WriteLn('Cleared: ', deleted.Count);
{$ENDIF}

    if list.Count = 0 then
      //DoInitConnections();
  finally
    fConnections.UnlockList;
    deleted.Free;
  end;
end;

{ TConnection }

constructor TConnection.Create(AOwner: TConnectionManager;
  AInfo: PConnectionInfo);
begin
  inherited Create();
  fInfo := AInfo;
  fOwner := AOwner;
  fIsUsed:= False;
  fLastUsed:= Now();
end;

destructor TConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TConnection.Close;
begin
  fIsUsed:= False;
  fLastUsed:= Now();
end;

procedure TConnection.Open;
begin
  fIsUsed:= True;
end;

{ TNOSQLDBConnection }

procedure TNOSQLDBConnection.Open();
begin

end;

procedure TNOSQLDBConnection.Close();
begin

end;

procedure TNOSQLDBConnection.Commit();
begin

end;

procedure TNOSQLDBConnection.Rollback();
begin

end;

{ TSQLDBConnection }

procedure TSQLDBConnection.AfterConnect(Sender: TObject);
begin
  fConnection.ExecuteDirect('SET CHARACTER SET `utf8`');
  fConnection.ExecuteDirect('SET NAMES ''utf8''');
end;

procedure TSQLDBConnection.BeforeDisconnect(Sender: TObject);
begin
  // define codes to handle before after disconnect

end;

constructor TSQLDBConnection.Create(AOwner: TConnectionManager; AInfo: PConnectionInfo);
begin
  inherited Create(AOwner, AInfo);

  fConnection := TSQLConnector.Create(nil);
  with fConnection do
  begin
    HostName := fInfo^.Server;
    DatabaseName := fInfo^.Database;
    UserName := fInfo^.Username;
    Password := fInfo^.Password;
    ConnectorType := fInfo^.DriverName;
  end;

  fSQL := TSQLBuilder.Create(Self, fConnection);

  fConnection.Transaction := fTransaction;
  fConnection.AfterConnect:= @AfterConnect;
  fConnection.BeforeDisconnect:= @BeforeDisconnect;
end;

destructor TSQLDBConnection.Destroy;
begin
  fConnection.Free;
  fSQL.Free;
  inherited Destroy;
end;

procedure TSQLDBConnection.Close();
begin
  inherited;
  fConnection.Close(True);
end;

procedure TSQLDBConnection.Open();
begin
  inherited;
  fConnection.Open();
end;

procedure TSQLDBConnection.Commit();
begin
  fTransaction.Commit;
end;

procedure TSQLDBConnection.Rollback();
begin
  fTransaction.Rollback;
end;

{ TDBConnection }

constructor TConnectionDB.Create(AOwner: TConnectionManager; AInfo: PConnectionInfo);
begin
  inherited Create(AOwner, AInfo);
  fTransaction := TSQLTransaction.Create(nil);
end;

destructor TConnectionDB.Destroy;
begin
  fTransaction.Free;
  inherited Destroy;
end;

end.


