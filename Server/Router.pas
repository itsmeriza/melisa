unit Router;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTypes;

type

  { TRoute }

  TRoute = class
  private
    fClassName: string;
    fCommand: TCommandType;
    fURI: string;
  public
    class function GetCommandString(ACommand: TCommandType): string;
    class function GetCommandType(ACommand: string): TCommandType;
  published
    property URI: string read fURI write fURI;
    property Command: TCommandType read fCommand write fCommand;
    property ClassName: string read fClassName write fClassName;
  end;

  { TRouter }

  TRouter = class
  private
    fRoutes: TListObject;
  public
    constructor Create();
    destructor Destroy; override;

    function GetRoute(URI: string; Command: TCommandType): TRoute;
    function GetRoute(AClassName: string): TRoute;
    function RegisterRoute(URI, Command: string): TRoute;
    function IsExist(URI: string; Command: TCommandType): Boolean;
  end;

implementation

uses Utils, StrUtils;

{ TRouter }

constructor TRouter.Create;
begin
  fRoutes := TListObject.Create;
end;

destructor TRouter.Destroy;
begin
  TUtils.ClearList(fRoutes);
  fRoutes.Free;
end;

function TRouter.GetRoute(URI: string; Command: TCommandType): TRoute;
var
  i: Integer;
  r: TRoute;
begin
  Result := nil;
  for i := 0 to fRoutes.Count - 1 do
  begin
    r := TRoute(fRoutes.Items[i]);
    if AnsiSameText(r.URI, URI) and (r.Command = Command) then
    begin
      Result := r;
      Exit;
    end;
  end;
end;

function TRouter.GetRoute(AClassName: string): TRoute;
var
  i: Integer;
  r: TRoute;
begin
  Result := nil;
  for i := 0 to fRoutes.Count() - 1 do
  begin
    r := TRoute(fRoutes[i]);
    if r.ClassName.CompareTo(AClassName) = 0 then
    begin
      Result := r;
      Exit;
    end;
  end;
end;

function TRouter.RegisterRoute(URI, Command: string): TRoute;
var
  tokens: TStringList;
begin
  Result := GetRoute(URI, TRoute.GetCommandType(Command));
  if Result <> nil then
    Exit;

  tokens := TStringList.Create;

  try
    TUtils.Split(tokens, URI, '/');
    if tokens.Count <> 4 then
      Exit;

    Result := TRoute.Create;
    Result.ClassName := Format('TReq%s%s%s%s', [
      AnsiReplaceText(tokens[1], '.', ''),
      TUtils.CammelCase(tokens[2], '-'),
      TUtils.CammelCase(tokens[3], '-'),
      TUtils.UpCaseFirst(Command)
      ]);
    Result.URI := URI;
    Result.Command := TRoute.GetCommandType(Command);

    fRoutes.Add(Result);
  finally
    tokens.Free;
  end;
end;

function TRouter.IsExist(URI: string; Command: TCommandType): Boolean;
begin
  Result := GetRoute(URI, Command) <> nil;
end;

{ TRoute }

class function TRoute.GetCommandString(ACommand: TCommandType): string;
begin
  Result := '';
  case ACommand of
    TCommandType.hcGET: Result := 'GET';
    TCommandType.hcPOST: Result := 'POST';
    TCommandType.hcUnknown: Result:= 'MQ';
  end;
end;

class function TRoute.GetCommandType(ACommand: string): TCommandType;
begin
  Result := TCommandType.hcGET;
  if AnsiSameText(ACommand, 'GET') then
    Result := TCommandType.hcGET
  else if AnsiSameText(ACommand, 'POST') then
    Result := TCommandType.hcPOST
  else if AnsiSameText(ACommand, 'MQ') then
  	Result:= TCommandType.hcUnknown;
end;

end.

