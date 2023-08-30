unit HTTPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdHTTPServer, IdContext, IdCustomHTTPServer, Router,
  MTypes, UserAccess, IdSSLOpenSSL, IdGlobal, DbConnection, WorkerHouseCleaner,
  rstomp, Command;

const
  MULTIPART_IDENTIFIER = 'multipart/form-data';
  SSL_PORT = 443;
  DEFAULT_PORT = 9999;

type
  TServerInfo = record
    IP: string;
    Port: Integer;
    Connections: Integer;
    IsActive: Boolean;
  end;

  TPartType = (ptText, ptFile);

  { TPart }

  TPart = class
  private
    fContent: string;
    fContentType: string;
    fFilename: string;
    fHeaders: TStringList;
    fPartType: TPartType;
    procedure DoParseHeader(AHeaders: string);
  public
    constructor Create;
    constructor Create(AHeaders, AContent: string);
    destructor Destroy; override;

    function GetAttributeValue(AHeaderId, AAttributeId: string): string;
    function IsAttributeExist(AHeaderId, AAttributeId: string): Boolean;
    function GetHeader(AHeaderId: string): string;
  published
    property PartType: TPartType read fPartType write fPartType;
    property Headers: TStringList read fHeaders write fHeaders;
    property Content: string read fContent write fContent;
    property ContentType: string read fContentType write fContentType;
    property Filename: string read fFilename write fFilename;
  end;

  { TMultipartParser }

  TMultipartParser = class
  private
    fReqInfo: TIdHTTPRequestInfo;
    fBoundary: string;
    fMsg: string;
    fParts: TListObject;
    procedure DoParse();
  public
    constructor Create(ARequestInfo: TIdHTTPRequestInfo);
    destructor Destroy; override;

    function GetPart(AParam: string): TPart;
  published
    property Boundary: string read fBoundary;
    property Msg: string read fMsg;
    property Parts: TListObject read fParts;
  end;

  THTTPServer = class;

  TCommandHTTP = class(TCommand)
    private
      fCmd: string;
      fRequestInfo: TIdHTTPRequestInfo;
      fResponseInfo: TIdHTTPResponseInfo;
      fMultipart: TMultipartParser;
      function DoGetValue(AParam: string): string;
    public
      constructor Create(AOwner: THTTPServer; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
      destructor Destroy; override;

      function Get(AParam: string): string;
      function Post(AParam: string): string;
      function Request(AParam: string): string;
      function GetToken(): string;
      function IsSignedIn(): Boolean;
      function GetUId: string;
      function GetWpId: string;
      function GetUserAccessAddr(): string;
      function GetPostToken(): string;

      procedure SetUId(UId: string);
      procedure SetWpId(WpId: string);
      procedure SetLanguageId(LanguageId: string);
      procedure SetToken(T: string);
      procedure SetUserAccessAddr(Addr: string);
      procedure ClearToken();
    published
      property Cmd: string read fCmd write fCmd;
      property RequestInfo: TIdHTTPRequestInfo read fRequestInfo write fRequestInfo;
      property ResponseInfo: TIdHTTPResponseInfo read fResponseInfo write fResponseInfo;
    end;

  { THTTPServer }

  THTTPServer = class
  private
    fRouter: TRouter;
    fServer: TIdHTTPServer;
    fConnectionMgr: TDBConnectionManager;
    fReqExec: TObject;
    fUserAccess: TUserAccess;
    fIsUseOwnList: Boolean;
    fAllowOrigin: string;
    fAllowCredentials: string;
    fIsActive: Boolean;
    fCleaner: TWorkerHouseCleaner;

    function DoGetActive(): Boolean;
    function DoToThreadPriority(AValue: string; ADefault: TThreadPriority): TThreadPriority;
    function DoToSSLMethod(Method: string): TIdSSLVersion;
    procedure DoServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure DoServerCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure DoServerSessionEnd(Session: TIdHTTPSession);
    procedure DoQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
  public
    constructor Create(Router: TRouter);
    destructor Destroy; override;

    function GetServerInfo(): TServerInfo;

    procedure Start;
    procedure Stop;
  published
    property UserAccess: TUserAccess read fUserAccess;
    property Router: TRouter read fRouter;
    property IsActive: Boolean read DoGetActive;
    property Server: TIdHTTPServer read fServer;
    property ConnectionMgr: TDBConnectionManager read fConnectionMgr;
  end;

implementation

uses IdSocketHandle, IdSchedulerOfThreadDefault, Data, Utils, StrUtils,
  RequestExecutor, Commons;

{ TPart }

procedure TPart.DoParseHeader(AHeaders: string);
var
  i: Integer;
  tokens: TStringList;
begin
  tokens := TStringList.Create;
  try
    TUtils.Split(tokens, AHeaders, #13#10);
    for i := 0 to tokens.Count - 1 do
      fHeaders.Add(tokens[i]);
  finally
    tokens.Free;
  end;
end;

constructor TPart.Create;
begin
  fHeaders := TStringList.Create;
end;

constructor TPart.Create(AHeaders, AContent: string);
begin
  fHeaders := TStringList.Create;
  DoParseHeader(AHeaders);

  fContent := AContent;

  if IsAttributeExist('Content-Disposition', 'filename') then
    fPartType := ptFile
  else
    fPartType := ptText;
end;

destructor TPart.Destroy;
begin
  fHeaders.Free;
  inherited Destroy;
end;

function TPart.GetAttributeValue(AHeaderId, AAttributeId: string): string;
var
  count: Integer;
  posAttr, posSemicolon: SizeInt;
  header, attr: string;
begin
  Result := '';
  header := GetHeader(AHeaderId);

  count := Length(header);
  if AnsiContainsText(header, AHeaderId) then
  begin
    posAttr := Pos(AAttributeId, header);
    attr := Copy(header, posAttr, count);
    posSemicolon := Pos(';', attr);
    if posSemicolon > 0 then
      count := posSemicolon - 1;

    attr := Copy(attr, 1, count);
    Result := Copy(attr, Pos('=', attr) + 1, Length(attr));
    Result := TUtils.TrimEx(['"'], Result);
  end;
end;

function TPart.IsAttributeExist(AHeaderId, AAttributeId: string): Boolean;
begin
  Result := Pos(AAttributeId, GetHeader(AHeaderId)) > 0;
end;

function TPart.GetHeader(AHeaderId: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to fHeaders.Count - 1 do
  begin
    if AnsiContainsText(fHeaders[i], AHeaderId) then
    begin
      Result := fHeaders[i];
      Exit;
    end;
  end;
end;

{ TMultipartParser }

procedure TMultipartParser.DoParse();
var
  tokens, elements: TStringList;
  ss: TStringStream;
  len: SizeInt;
  endSign, token, headers, content: string;
  i: Integer;
  part: TPart;
begin
  tokens := TStringList.Create;
  ss := TStringStream.Create('');
  try
    len := Length(fBoundary);
    endSign := fBoundary + '--';

    ss.CopyFrom(fReqInfo.PostStream, fReqInfo.PostStream.Size);
    fMsg := Copy(ss.DataString, 1 + len, Pos(endSign, ss.DataString) - len-1);

    TUtils.Split(tokens, fMsg, fBoundary);

    for i := 0 to tokens.Count - 1 do
    begin
      token := TUtils.TrimEx([#13, #10], tokens[i]);
      elements := TStringList.Create;
      try
        TUtils.Split(elements, token, #13#10#13#10);
        headers := elements[0];
        content := '';
        if elements.Count = 2 then
          content := elements[1];

        part := TPart.Create(headers, content);
        fParts.Add(part);
      finally
        elements.Free;
      end;
    end;
  finally
    tokens.Free;
    ss.Free;
  end;
end;

constructor TMultipartParser.Create(ARequestInfo: TIdHTTPRequestInfo);
begin
  fReqInfo := ARequestInfo;
  fParts := TListObject.Create;

  if AnsiContainsText(fReqInfo.ContentType, MULTIPART_IDENTIFIER) then
  begin
    fBoundary := Copy(fReqInfo.ContentType, Pos('boundary', fReqInfo.ContentType) + 9, Length(fReqInfo.ContentType));
    fBoundary := '--' + fBoundary;

    DoParse();
  end;
end;

destructor TMultipartParser.Destroy;
begin
  TUtils.ClearList(fParts);
  fParts.Free;
  inherited Destroy;
end;

function TMultipartParser.GetPart(AParam: string): TPart;
var
  i: Integer;
  part: TPart;
begin
  Result := nil;
  for i := 0 to fParts.Count - 1 do
  begin
    part := TPart(fParts.Items[i]);
    if AnsiSameText(part.GetAttributeValue('Content-Disposition', 'name'), AParam) then
    begin
      if part.PartType = ptFile then
      begin
        part.ContentType := TUtils.TrimEx([':', #32], AnsiReplaceText(part.GetHeader('Content-Type'), 'Content-Type', ''));
        part.Filename := part.GetAttributeValue('Content-Disposition', 'filename');
      end;

      Result := part;
      Exit;
    end;
  end;
end;

{ TCommandHTTP }

function TCommandHTTP.DoGetValue(AParam: string): string;
var
  part: TPart;
begin
  Result := '';
  part := fMultiPart.GetPart(AParam);
  if part = nil then
    Exit;
  Result := part.Content;
end;

constructor TCommandHTTP.Create(AOwner: THTTPServer; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  fOwner := AOwner;
  fRequestInfo := ARequestInfo;
  fResponseInfo := AResponseInfo;
  fCmd := Format('%s %s', [
    fRequestInfo.Command,
    TUtils.IncludeURITrailing(fRequestInfo.Document)
    ]);
  fMultipart := TMultipartParser.Create(fRequestInfo);
end;

destructor TCommandHTTP.Destroy;
begin
  fMultipart.Free;
  inherited Destroy;
end;

function TCommandHTTP.Get(AParam: string): string;
begin
  Result := fRequestInfo.Params.Values[AParam];
end;

function TCommandHTTP.Post(AParam: string): string;
begin
  Result := '';
  if fRequestInfo.Params.Count > 0 then
  begin
    Result := fRequestInfo.Params.Values[AParam];
    Exit;
  end;

  if not Assigned(fRequestInfo.PostStream) then
    Exit;

  Result := DoGetValue(AParam);
end;

function TCommandHTTP.Request(AParam: string): string;
begin
  Result := Get(AParam);
  if Result = '' then
    Result := Post(AParam);
end;

function TCommandHTTP.GetToken(): string;
begin
  Result := fRequestInfo.Session.Content.Values['Token'];
end;

function TCommandHTTP.IsSignedIn(): Boolean;
begin
  Result := GetUId() <> '';
end;

function TCommandHTTP.GetUId: string;
begin
  Result := fRequestInfo.Session.Content.Values['UId'];
end;

function TCommandHTTP.GetWpId: string;
begin
  Result := fRequestInfo.Session.Content.Values['WpId'];
end;

function TCommandHTTP.GetUserAccessAddr(): string;
begin
  Result := fRequestInfo.Session.Content.Values[USER_ACCESS_KEY];
end;

function TCommandHTTP.GetPostToken(): string;
begin
  Result := fRequestInfo.RawHeaders.Values['Post-Token'];
  if Result = '' then
    Result:= Post('Post-Token');
end;

procedure TCommandHTTP.SetUId(UId: string);
begin
  fRequestInfo.Session.Content.Values['UId'] := UId;
end;

procedure TCommandHTTP.SetWpId(WpId: string);
begin
  fRequestInfo.Session.Content.Values['WpId'] := WpId;
end;

procedure TCommandHTTP.SetLanguageId(LanguageId: string);
begin
  fRequestInfo.Session.Content.Values['LanguageId']:= LanguageId;
end;

procedure TCommandHTTP.SetToken(T: string);
begin
  fRequestInfo.Session.Content.Values['Token'] := T;
end;

procedure TCommandHTTP.SetUserAccessAddr(Addr: string);
begin
  fRequestInfo.Session.Content.Values[USER_ACCESS_KEY] := Addr;
end;

procedure TCommandHTTP.ClearToken();
begin
  SetToken('');
end;

{ THTTPServer }

function THTTPServer.DoGetActive(): Boolean;
begin
  Result := fServer.Active;
end;

function THTTPServer.DoToThreadPriority(AValue: string;
  ADefault: TThreadPriority): TThreadPriority;
var
  value: Integer;
begin
  Result := ADefault;
  value := StrToIntDef(AValue, 0);
  case value of
    0: Result := tpTimeCritical;
    1: Result := tpHighest;
    2: Result := tpHigher;
    3: Result := tpNormal;
    4: Result := tpLower;
    5: Result := tpLowest;
    6: Result := tpIdle;
  end;
end;

function THTTPServer.DoToSSLMethod(Method: string): TIdSSLVersion;
begin
  if AnsiSameText(Method, 'sslvSSLv2') then
    Result := sslvSSLv2
  else if AnsiSameText(Method, 'sslvSSLv23') then
    Result := sslvSSLv23
  else if AnsiSameText(Method, 'sslvSSLv3') then
    Result := sslvSSLv3
  else if AnsiSameText(Method, 'sslvTLSv1') then
    Result := sslvTLSv1
  else if AnsiSameText(Method, 'sslvTLSv1_1') then
    Result := sslvTLSv1_1
  else if AnsiSameText(Method, 'sslvTLSv1_2') then
    Result := sslvTLSv1_2
  else
    Result := sslvTLSv1_2;
end;

procedure THTTPServer.DoServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  cmd: TCommand;
begin
  if not AContext.Connection.Connected then
    Exit;

  cmd := TCommandHTTP.Create(Self, ARequestInfo, AResponseInfo);
  cmd.Route := fRouter.GetRoute(ARequestInfo.Document, ARequestInfo.CommandType);

  try
    try
      if cmd.Route = nil then
      begin
        AResponseInfo.ResponseNo := 400;
        raise Exception.Create('Bad route of "' + ARequestInfo.Document + ' ' + ARequestInfo.Command + '"');
      end;

      AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Headers', 'Content-Type, Accept');
      AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Origin', fAllowOrigin);
      AResponseInfo.CustomHeaders.AddValue('Access-Control-Allow-Credentials', fAllowCredentials);
      AResponseInfo.ContentType := 'application/json';

      TRequestExecutorHTTP(fReqExec).Fire(cmd);
    except
      on E: Exception do
        WriteLn(E.ClassName, ': ', E.Message);
    end;
  finally
    if cmd <> nil then
    	cmd.Free;
  end;
end;

procedure THTTPServer.DoServerCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if not AContext.Connection.Connected then
    Exit;

  DoServerCommandGet(AContext, ARequestInfo, AResponseInfo);
end;

procedure THTTPServer.DoServerSessionEnd(Session: TIdHTTPSession);
begin
  if fIsUseOwnList then
    fUserAccess.Delete(Session);
end;

procedure THTTPServer.DoQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
var
  p: TIdPort;
begin
  p := StrToIntDef(DataApp.Settings.Connection.Values['SSLPort'], SSL_PORT);
  VUseSSL := APort = p;
end;

constructor THTTPServer.Create(Router: TRouter);
var
  bind: TIdSocketHandle;
  io: TIdServerIOHandlerSSLOpenSSL;
  Scheduler: TIdSchedulerOfThreadDefault;
  isUseSSL: Boolean;
begin
  fIsActive:= False;
  fRouter := Router;
  fConnectionMgr:= TDBConnectionManager.Create(DataApp.Settings.DBConnectionInfo);
  fReqExec := TRequestExecutorHTTP.Create(fRouter, fConnectionMgr);
  fServer := TIdHTTPServer.Create(nil);

  fUserAccess := TUserAccess.Create;
  fIsUseOwnList := StrToBoolDef(DataApp.Settings.Behaviour.Values['IsUseOwnUserAccessList'], TRUE);
  fAllowOrigin := DataApp.Settings.Behaviour.Values['AccessControlAllowOrigin'];
  fAllowCredentials := DataApp.Settings.Behaviour.Values['AccessControlAllowCredentials'].ToLower;

  fServer.KeepAlive := True;
  fServer.SessionState := True;
  fServer.AutoStartSession := True;
  fServer.ParseParams := True;
  if DataApp.Settings.Behaviour.Values['SessionTimeOut'] = '0' then
    fServer.SessionTimeOut:= MaxInt
  else
    fServer.SessionTimeOut := StrToIntDef(DataApp.Settings.Behaviour.Values['SessionTimeOut'], 120) * 1000;

  scheduler := TIdSchedulerOfThreadDefault.Create(nil);
  scheduler.MaxThreads := StrToIntDef(DataApp.Settings.Behaviour.Values['MaxThreads'], 100);
  scheduler.ThreadPriority := DoToThreadPriority(DataApp.Settings.Behaviour.Values['ThreadPriority'], tpNormal);
  fServer.Scheduler := scheduler;
  fServer.MaxConnections := StrToIntDef(DataApp.Settings.Connection.Values['MaxConnections'], 150);

  fServer.OnCommandGet := @DoServerCommandGet;
  fServer.OnCommandOther := @DoServerCommandOther;
  fServer.OnSessionEnd := @DoServerSessionEnd;

  isUseSSL:= StrToBoolDef(DataApp.Settings.Connection.Values['SecureConnection'], FALSE);
  if not isUseSSL then
  begin
    bind := fServer.Bindings.Add;
    bind.IP  := DataApp.Settings.Connection.Values['ListeningIP'];
    bind.Port := StrToIntDef(DataApp.Settings.Connection.Values['ListeningPort'], DEFAULT_PORT);
  end
  else begin
    io := TIdServerIOHandlerSSLOpenSSL.Create(nil);
    //io.SSLOptions.Method := DoToSSLMethod(DataApp.Settings.Connection.Values['SSLMethod']);
    io.SSLOptions.SSLVersions := [sslvSSLv2, sslvSSLv23, sslvSSLv3, sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
    io.SSLOptions.CertFile := DataApp.Settings.Connection.Values['SSLCertFile'];
    io.SSLOptions.KeyFile := DataApp.Settings.Connection.Values['SSLKeyFile'];
    io.SSLOptions.RootCertFile := DataApp.Settings.Connection.Values['SSLRootCertFile'];
    io.SSLOptions.CipherList := DataApp.Settings.Connection.Values['SSLCipherList'];
    fServer.IOHandler := io;

    bind := fServer.Bindings.Add ;
    bind.IP := DataApp.Settings.Connection.Values['ListeningIP'];
    bind.Port := StrToIntDef(DataApp.Settings.Connection.Values['SSLPort'], SSL_PORT);

    fServer.OnQuerySSLPort := @DoQuerySSLPort;
  end;

  fCleaner:= TWorkerHouseCleaner.Create(fConnectionMgr);
  fCleaner.Start;
end;

destructor THTTPServer.Destroy;
begin
  fConnectionMgr.Free;
  fReqExec.Free;

  fServer.Scheduler.Free;
  fServer.Scheduler:= nil;

  fServer.Free;
  fUserAccess.Free;

  fCleaner.Terminate;
  fCleaner.Free;
  inherited Destroy;
end;

function THTTPServer.GetServerInfo(): TServerInfo;
var
  list: TList;
begin
  Result.IP := fServer.Bindings.Items[0].IP;
  Result.Port := fServer.Bindings.Items[0].Port;

  list := fServer.Contexts.LockList;
  try
    Result.Connections := list.Count;
  finally
    fServer.Contexts.UnlockList;
  end;

  Result.IsActive := fServer.Active;
end;

procedure THTTPServer.Start;
begin
  fServer.Active := True;
end;

procedure THTTPServer.Stop;
begin
  fServer.Active := False;
end;

end.

