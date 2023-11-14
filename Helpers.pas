unit Helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Data, IdCookieManager, Entry;

const
  DEFAULT_TOKEN_LENGTH = 32;

type
  TTrimmedChars = set of Char;

  { THelpers }

  THelpers = class
  public
    class function HTTPReq(URI: string; Params: TStringList; Method: string = 'GET'): string;
    class function BuildQueryStr(S: TStringList): string;
    class function TrimEx(S: string; Chars: TTrimmedChars): string;
    class function GetListeningAddress(): string;
    class function GenerateToken(): TEntryToken;
    class procedure SetGlobalCookieManager(CookieManager: TIdCookieManager);
  end;

var
  CookieManager: TIdCookieManager;

implementation

uses IdHTTP, IdSSLOpenSSL, Utils;

{ THelpers }

class function THelpers.HTTPReq(URI: string; Params: TStringList; Method: string
  ): string;
var
  http: TIdHTTP;
  io: TIdSSLIOHandlerSocketOpenSSL;
  url: string;
begin
  io := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  io.SSLOptions.Method := sslvTLSv1_2;
  io.SSLOptions.Mode := sslmClient;

  http := TIdHTTP.Create(nil);
  http.IOHandler := io;
  http.AllowCookies := True;
  http.CookieManager := CookieManager;
  try
    if Method = 'GET' then
    begin
      if Params <> nil then
      begin
        Params.Delimiter := '&';
        url := Format('%s?%s', [URI, THelpers.BuildQueryStr(Params)]);
      end
      else
        url := URI;

      Result := http.Get(url);
    end
    else if Method = 'POST' then
      Result := http.Post(URI, Params);
  finally
    io.Free;
    http.Free;
  end;
end;

class function THelpers.BuildQueryStr(S: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to S.Count - 1 do
    Result := Result + Format('%s=%s&', [S.Names[i], THelpers.TrimEx(S.Values[S.Names[i]], ['"'])]);

  Result := Copy(Result, 1, Length(Result)-1);
end;

class function THelpers.TrimEx(S: string; Chars: TTrimmedChars): string;

  function getlpos(s: string; c: TTrimmedChars): Integer;
  var
    l: Integer;
  begin
    Result := 0;
    l := Length(s);
    if l = 0 then
      Exit;

    while (s[Result+1] in c) and (Result <= l) do
      Inc(Result);
  end;

  function getrpos(s: string; c: TTrimmedChars): Integer;
  begin
    Result := Length(s);
    if Result = 0 then
      Exit;

    while (s[Result] in c) and (Result > 0) do
      Dec(Result);
  end;

var
  lpos: Integer;
begin
  lpos := getlpos(S, Chars);
  Result := Copy(S, lpos, getrpos(S, Chars));
end;

class function THelpers.GetListeningAddress(): string;
begin
  Result := Format('https://%s:%s', [
    DataApp.Settings.Connection.Values['ListeningIP'],
    DataApp.Settings.Connection.values['SSLPort']
  ]);
end;

class function THelpers.GenerateToken: TEntryToken;
begin
  Result := TEntryToken.Create;
  Result.Key := TUtils.CreateRandomKey(DEFAULT_TOKEN_LENGTH);
  Result.Length := Length(Result.Key);
end;

class procedure THelpers.SetGlobalCookieManager(CookieManager: TIdCookieManager
  );
begin
  Helpers.CookieManager := CookieManager;
end;

end.

