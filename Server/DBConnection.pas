unit DBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql56conn, MTypes, SQLBuilder,
  IdCustomHTTPServer, StrUtils, DateUtils;

const
  INIT_CONNECTION_COUNT = 5;

type
  TDBConnectionManager = class;

  { TDBConnection }

  TDBConnection = class
  private
    fId: string;
    fInfo: PConnectionInfo;
    fIsUsed: Boolean;
    fLastUsed: TDateTime;
    fOwner: TDBConnectionManager;
    fSession: TIdHTTPSession;
  protected
    fTransaction: TSQLTransaction;
  public
    constructor Create(AOwner: TDBConnectionManager; AInfo: PConnectionInfo); virtual;
    destructor Destroy; override;

    procedure Close(); virtual; abstract;
    procedure Open(); virtual; abstract;
    procedure Commit(); virtual; abstract;
    procedure Rollback(); virtual; abstract;

    property Info: PConnectionInfo read fInfo;
  published
    property Id: string read fId write fId;
    property Owner: TDBConnectionManager read fOwner write fOwner;
    property Session: TIdHTTPSession read fSession write fSession;
    property IsUsed: Boolean read fIsUsed write fIsUsed;
    property LastUsed: TDateTime read fLastUsed write fLastUsed;
  end;

  TCustomDBConnection = class(TDBConnection)
  public
    property Info;
  published
    property Id;
    property Owner;
  end;

  { Dummy class.
    Will be replaced with mORMot's TMongoClient class. }
  TMongoClient = class

  end;

  { TSQLDBConnection }

  TSQLDBConnection = class(TCustomDBConnection)
  private
    fConnection: TSQLConnector;
    fSQL: TSQLBuilder;
    procedure AfterConnect(Sender: TObject);
    procedure BeforeDisconnect(Sender: TObject);
  public
    constructor Create(AOwner: TDBConnectionManager; AInfo: PConnectionInfo); override;
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

  TNOSQLDBConnection = class(TCustomDBConnection)
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

  { TDBConnectionManager }

  TDBConnectionManager = class
  private
    fSession: TIdHTTPSession;
    fConnectionInfos: TListConnectionInfo;
    fConnections: TThreadList;

    function DoCreateConnection(AConnectionId: string): TDBConnection;
    procedure DoInitConnections();
  public
    constructor Create(AConnectionInfos: TListConnectionInfo); overload;
    destructor Destroy; override;

    function CreateConnection(AConnectionId: string): TDBConnection;
    function GetConnection(AConnectionId: string): TDBConnection;
    function Count(): Integer;
    procedure Close(Connection: TDBConnection);
    procedure Remove(OlderThan: Int64 = 60000);
  published
    property Session: TIdHTTPSession read fSession;
  end;

implementation

uses Utils, Data, Commons;

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

constructor TSQLDBConnection.Create(AOwner: TDBConnectionManager; AInfo: PConnectionInfo);
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
  fConnection.Close(True);
  fIsUsed:= False;
end;

procedure TSQLDBConnection.Open();
begin
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

{ TDBConnectionManager }

procedure TDBConnectionManager.DoInitConnections();
var
  i: Integer;
  c: TDBConnection;
  list: TList;
begin
{$IFDEF DEBUG}
  WriteLn('Initiate connection.');
{$ENDIF}
  for i:= 0 to INIT_CONNECTION_COUNT-1 do
  begin
    c:= DoCreateConnection(CNX_1);
    c.Open();

    list:= fConnections.LockList;
    try
      list.Add(c);
    finally
      fConnections.UnlockList;
    end;
  end;
{$IFDEF DEBUG}
  WriteLn('Finished to initiate connections. Count: ', Count());
{$ENDIF}
end;

constructor TDBConnectionManager.Create(AConnectionInfos: TListConnectionInfo);
begin
  fConnectionInfos := AConnectionInfos;
  fConnections := TThreadList.Create;

  DoInitConnections();
end;

destructor TDBConnectionManager.Destroy;
begin
  TUtils.ClearList(fConnections);
  fConnections.Free;;
  inherited Destroy;
end;

function TDBConnectionManager.CreateConnection(AConnectionId: string): TDBConnection;
var
  list: TList;
begin
  Result := GetConnection(AConnectionId);
  if Result <> nil then
  begin
    Result.IsUsed:= True;
    Exit;
  end;

  Result := DoCreateConnection(AConnectionId);
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

function TDBConnectionManager.DoCreateConnection(AConnectionId: string
  ): TDBConnection;
var
  c: PConnectionInfo;
begin
  c := DataApp.Settings.GetConnectionInfo(AConnectionId);
  if AnsiContainsText(c^.DriverName, SQL_CONNECTION_DRIVER_NAME_INITIAL) then
    Result := TSQLDBConnection.Create(Self, c)
  //else if AnsiContainsText(c^.DriverName, MQ_CONNECTION_DRIVER_NAME_INITIAL) then
  //	Result := TSTOMPConnection.Create(Self, c)
  else
    Result := TNOSQLDBConnection.Create(Self, c);

  Result.Id:= AConnectionId;
end;

function TDBConnectionManager.GetConnection(AConnectionId: string
  ): TDBConnection;
var
  i: Integer;
  c: TDBConnection;
  list: TList;
begin
  Result := nil;

  // get free connection.
  list:= fConnections.LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      c := TDBConnection(list.Items[i]);
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

function TDBConnectionManager.Count(): Integer;
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

procedure TDBConnectionManager.Close(Connection: TDBConnection);
begin
  Connection.Close();
end;

procedure TDBConnectionManager.Remove(OlderThan: Int64);
var
  list: TList;
  i: Integer;
  c: TDBConnection;
  deleted: TList;
  age: Int64;
begin
  list:= fConnections.LockList;
  deleted:= Classes.TList.Create;
  try
    if list.Count = 0 then
      Exit;

    c:= TDBConnection(list[0]);
    for i:= 0 to list.Count - 1 do
    begin
      c:= TDBConnection(list[i]);
      age:= MilliSecondsBetween(Now(), c.LastUsed);
      if not c.IsUsed and (age > OlderThan) then
        deleted.Add(c);
    end;

    for i:= 0 to deleted.Count - 1 do
    begin
      c:= TDBConnection(deleted[i]);
      list.Remove(c);
      c.Close();
      c.Free;
    end;

    if list.Count = 0 then
      DoInitConnections();

{$IFDEF DEBUG}
    WriteLn('Cleared: ', deleted.Count);
{$ENDIF}
  finally
    fConnections.UnlockList;
    deleted.Free;
  end;
end;

{ TDBConnection }

constructor TDBConnection.Create(AOwner: TDBConnectionManager; AInfo: PConnectionInfo);
begin
  fInfo := AInfo;
  fTransaction := TSQLTransaction.Create(nil);
  fOwner := AOwner;
  fIsUsed:= False;
end;

destructor TDBConnection.Destroy;
begin
  fTransaction.Free;
  inherited Destroy;
end;

end.


