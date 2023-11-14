program Melisa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Crt, Commons, HTTPServer, Data, Utils, MTypes, Router,
  RequestExecutor, Request, ObjectBase, Entry, EntryUser, Connection,
  SQLBuilder, ObjectFactory, HandleDB, User, Handle, ObjectFactoryDB,
  IntfObjectFactory, IdCookieManager, Helpers, Worker,
  UserAccess, ReactiveExtension, Req10UserSignInPost, Menu, EntryMenu, Command;

var
  server: THTTPServer;
  r: TRouter;
  isDaemonized, isRun: Boolean;
  cm: TIdCookieManager;
  subscriber: TSubscriber;
  connectionMgr: TConnectionManager;

procedure DoServerInformation();
begin
  Writeln('Listening IP     : ', server.GetServerInfo.IP);
  Writeln('Listening Port   : ', server.GetServerInfo.Port);
  Writeln('Connection Count : ', server.GetServerInfo.Connections);
  Writeln('Active           : ', server.GetServerInfo.IsActive);
end;

procedure DoServerStart();
begin
  WriteLn('Server kick starting...');
  server.Start();
  WriteLn('Server started.');
end;

procedure DoServerStop();
begin
  server.Stop;
  WriteLn(MSG_SERVER_STOPPED);
end;

procedure DoServerClear();
begin
  ClrScr;
  Writeln;
  Writeln(MSG_BEWARE);
end;

procedure DoRegisterRoutes(R: TRouter);
var
  f: TextFile;
  line, cmd, uri: string;
  pos32: SizeInt;
begin
	AssignFile(f, COMMANDS_FILE);
  try
  	Reset(f);
    while not EOF(f) do
    begin
      ReadLn(f, line);

      pos32:= Pos(#32, line);
      cmd := Copy(line, 0, pos32 - 1);
      uri := Trim(Copy(line, pos32 + 1, Length(line) - pos32));
      R.RegisterRoute(uri, cmd);
    end;
  finally
  	CloseFile(f);
  end;
end;

function DoGetParamIndex(Param: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to ParamCount do
  begin
    if AnsiSameText(ParamStr(i), Param) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure DoRunDaemonized();
begin
  DoServerStart();
  while True do
    Sleep(60000);
end;

procedure DoRunPrompted();
var
  cmd: string;
begin
  Writeln;
  Writeln(MSG_BEWARE);
  WriteLn;
  Writeln('To start the server, type "start".');
  Writeln('Type "help" to see more commands.');
  Writeln;

  repeat
    Write(prompt);

    Readln(cmd);
    cmd := Trim(cmd);
    if cmd = '' then
    else begin
      if AnsiSameText(cmd, cmdHelp) then
        WriteLn(commands)
      else if AnsiSameText(cmd, cmdStart) then
        DoServerStart()
      else if AnsiSameText(cmd, cmdStop) then
        DoServerStop()
      else if AnsiSameText(cmd, cmdClear) then
        DoServerClear()
      else if AnsiSameText(cmd, cmdRestart) then
      begin
        DoServerStop();
        DoServerStart();
      end
      else if AnsiSameText(cmd, cmdInfo) then
        DoServerInformation()
      else if AnsiSameText(cmd, cmdClose) then
      else
        WriteLn('"', cmd, '" is not recognized as internal or external server command.');

      Writeln;
    end;
  until
    AnsiSameText(cmd, cmdClose);

  WriteLn(MSG_SERVER_STOPPED);

  server.Stop;
end;

procedure DoRun();
begin
  if not server.IsActive then
    DoServerStart();
end;

procedure DoPostRun();
var
  uri, endpoint: string;
begin
  try
    Writeln('Requesting to upgrade the system...');
    endpoint := '/1.0/system/upgrade/';
    uri := THelpers.GetListeningAddress() + endpoint;
    THelpers.HTTPReq(uri, nil, 'POST');
  except
  end;
end;

{$R *.res}

begin
{$IFDEF DEBUG}
 	GlobalSkipIfNoLeaks := False;
  DeleteFile(ExtractFilePath(ParamStr(0)) + '\heaptrc.txt');
  SetHeapTraceOutput('heaptrc.txt');
{$ENDIF}

  Randomize;
  isDaemonized := DoGetParamIndex(DAEMONIZED_OPT) >= 0;
  isRun := DoGetParamIndex(RUN_OPT) >= 0;

  DataApp := TDataApp.Create;
  r := TRouter.Create;
  DoRegisterRoutes(r);

  connectionMgr:= TConnectionManager.Create(DataApp.Settings.ConnectionInfos);
  server := THTTPServer.Create(r, connectionMgr);

  subscriber:= TSubscriber.Create(r, connectionMgr);
  subscriber.Vhost:= DataApp.Settings.MessageBroker.Values['Vhost'];
  subscriber.User:= DataApp.Settings.MessageBroker.Values['User'];
  subscriber.Passcode:= DataApp.Settings.MessageBroker.Values['Passcode'];
  subscriber.SubscriptionPaths:= DataApp.Settings.MessageBroker.Values['SubscriptionPaths'];
  subscriber.Connect;

  cm := TIdCookieManager.Create(nil);
  THelpers.SetGlobalCookieManager(cm);

  try
    try
      if isDaemonized then
      begin
        WriteLn('Run daemonized.');
        DoRunDaemonized();
      end
      else if isRun then
      begin
        DoRun();
        Writeln();
        Writeln('Press Enter to exit.');
        Writeln();
        Readln();
      end
      else
        DoRunPrompted();
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    DataApp.Free;
    r.Free;
    server.Free;
    connectionMgr.Free;
    subscriber.Free;
    cm.Free;
  end;
end.


