unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTypes, RJSON, process, DCPrijndael, DCPsha256;

const
  AES_KEY_LENGTH = 128;

type
  TChars = set of Char;

  { TUtils }

  TUtils = class
  public
    class function UpCaseFirst(word: string): string;
    class function CammelCase(s: string; d: Char): string;
    class function IncludeURITrailing(URI: string): string;
    class function TrimEx(AChars: TChars; AStr: string): string;
    class function GetDelimitedText(s: TStringList): string;
    class function IsInteger(const s: string): Boolean;
    class function CreateRandomKey(KeyMap: string; Len: Byte = 4): string; overload;
    class function CreateRandomKey(Len: Byte = 4): string; overload;
    class function CreateID(Len: Byte = 32): string;
    class function ClearStrFrom(const ClearedChars: TChars; const S: string): string;
    class function ExtractStr(const AllowedChars, S: string): string;
    class function CreateRandomPasswd(Len: Byte = 8; IsRandomLength: Boolean = False): string;
    class function AES_Encrypt(Msg, Key: string): string;
    class function AES_Decrypt(Str, Key: string): string;
    class function Base64(S: string): string;
    class function DateTimeToUNIXTimeFAST(DelphiTime: TDateTime): LongWord;
    class function UNIXTimeToDateTimeFAST(UnixTime: LongWord): TDateTime;
    class function YYYYMMDDToDateTime(yyyymmdd: string): TDateTime;
    class function StrToBool(s: string): Boolean;
    class procedure ClearList(List: TListObject);
    class procedure ClearList(List: TListConnectionInfo);
    class procedure ClearList(List: Classes.TList);
    class procedure ClearList(List: TStringList);
    class procedure ClearList(List: TThreadList);
    class procedure Split(ATokens: TStrings; AContent, ADelimiter: string);
    class procedure Split(ATokens: TStringList; AContent: string; ADelimiter: Char = ',');
    class procedure Exec(Cmd: string; Params: TStringList; IsWaitOnExit: Boolean = True);
  end;

implementation

uses IdHTTP, IdSSLOpenSSL, Base64;

{ TUtils }

class function TUtils.UpCaseFirst(word: string): string;
begin
  Result := LowerCase(word);
  if Result = '' then
    Exit;

  Result[1] := UpCase(Result[1]);
end;

class function TUtils.CammelCase(s: string; d: Char): string;
var
  p: SizeInt;
  t: string;
  eol: Boolean;
begin
  Result := '';
  eol := False;
  t := s;
  while not eol do
  begin
    p := Pos(d, t);
    if p = 0 then
    begin
      Result := UpCaseFirst(t);
      Exit;
    end;

    Result := Result + UpCaseFirst(Copy(t, 1, p-1));
    t := Copy(t, p+1, Length(t));
    eol := Pos(d, t) = 0;
    if eol then
      Result := Result + UpCaseFirst(t);
  end;
end;

class function TUtils.IncludeURITrailing(URI: string): string;
var
  s: string;
  l: Integer;
begin
  Result := URI;

  l := Length(Result);
  s := Copy(Result, l, 1);
  if s <> '/' then
    Result := Result + '/';
end;

class function TUtils.TrimEx(AChars: TChars; AStr: string): string;
var
  start, stop: Integer;
begin
  start := 1;
  while (AStr[start] in AChars) do
    Inc(start);

  stop := Length(AStr);
  while (AStr[stop] in AChars) do
    Dec(stop);

  Result := Copy(AStr, start, stop - start + 1);
end;

class function TUtils.GetDelimitedText(s: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to s.Count - 1 do
    Result := Result + s[i] + s.Delimiter;

  Result := Copy(Result, 1, Length(Result) - 1);
end;

class function TUtils.IsInteger(const s: string): Boolean;
var
  v: Int64;
begin
  Result := TryStrToInt64(s, v);
end;

class function TUtils.CreateRandomKey(KeyMap: string; Len: Byte): string;
var
  l: SizeInt;
  i: Integer;
begin
  Result := '';
  l := Length(KeyMap);
  for i := 0 to Len - 1  do
    Result := Result + KeyMap[Random(l) + 1];
end;

class function TUtils.CreateRandomKey(Len: Byte): string;
const
  KEYMAP = 'qwertyuiopasdfghjklzxcvbnm1234567890QWERTYUIOPASDFGHJKLZXCVBNM';
begin
  Result := TUtils.CreateRandomKey(KEYMAP, Len);
end;


class function TUtils.CreateID(Len: Byte): string;
var
  uid: TGUID;
  r: HRESULT;
  guid: string;
  i: Integer;
begin
  Result := '';
  if Len > 32 then
    raise Exception.Create('Can not perform id with more than 32 characters length.');

  r := CreateGUID(uid);
  if r = S_OK then
  begin
    guid := GUIDToString(uid);
    for i := 0 to Length(guid) - 1 do
      if not (guid[i+1] in ['{', '}', '-']) then
        Result := Result + guid[i+1];

    Result := LowerCase(Result);
  end;

  Result := Copy(Result, 1, Len);
end;

class function TUtils.ClearStrFrom(const ClearedChars: TChars; const S: string
  ): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to S.Length do
  begin
    if (S[i] in ClearedChars) then
      Continue;
    Result := Result + S[i];
  end;
end;

class function TUtils.ExtractStr(const AllowedChars, S: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if Pos(S[i], AllowedChars) > 0 then
      Result := Result + S[i];
end;

class function TUtils.CreateRandomPasswd(Len: Byte; IsRandomLength: Boolean
  ): string;
const
  PASSWORD_CHARS = 'qwertyuiopasdfghjklzxcvbnm1234567890QWERTYUIOPASDFGHJKLZXCVBNM!+@_#)$(%*^&:";<>?,./~`={}[]|\-';
var
  l: Byte;
begin
  if IsRandomLength then
  begin
    l := 0;
    while l = 0 do
      l := Random(Len + 1);
  end
  else
    l := Len;

  Result := TUtils.CreateRandomKey(PASSWORD_CHARS, l);
end;

class function TUtils.AES_Encrypt(Msg, Key: string): string;
var
  cipher: TDCP_rijndael;
begin
  cipher := TDCP_rijndael.Create(nil);
  try
    cipher.InitStr(Key, TDCP_sha256);
    Result := cipher.EncryptString(Msg);
  finally
    cipher.Free;
  end;
end;

class function TUtils.AES_Decrypt(Str, Key: string): string;
var
  cipher: TDCP_rijndael;
begin
  cipher := TDCP_rijndael.Create(nil);
  try
    cipher.InitStr(Key, TDCP_sha256);
    Result := cipher.DecryptString(Str);
  finally
    cipher.Free;
  end;
end;

class function TUtils.Base64(S: string): string;
begin
  Result:= EncodeStringBase64(S);
end;

class function TUtils.DateTimeToUNIXTimeFAST(DelphiTime: TDateTime): LongWord;
begin
  Result := Round((DelphiTime - 25569) * 86400);
end;

class function TUtils.UNIXTimeToDateTimeFAST(UnixTime: LongWord): TDateTime;
begin
  Result := (UnixTime / 86400) + 25569;
end;

class function TUtils.YYYYMMDDToDateTime(yyyymmdd: string): TDateTime;
var
  token: TStringList;
begin
  token:= TStringList.Create;
  try
    TUtils.Split(token, yyyymmdd, '-');
    Result := EncodeDate(
      StrToIntDef(token[0], 0),
      StrToIntDef(token[1], 0),
      StrToIntDef(token[2], 0)
    );
  finally
    token.Free;
  end;
end;

class function TUtils.StrToBool(s: string): Boolean;
begin
  Result:= TRJSONHelper.StrToBool(s);
end;

class procedure TUtils.ClearList(List: TListObject);
var
  i: Integer;
  o: TObject;
begin
  for i := 0 to List.Count - 1 do
  begin
    o := List[i];
    o.Free;
  end;
end;

class procedure TUtils.ClearList(List: TListConnectionInfo);
var
  i: Integer;
  p: PConnectionInfo;
begin
  for i := 0 to List.Count - 1 do
  begin
    p := List[i];
    Dispose(p);
  end;
end;

class procedure TUtils.ClearList(List: Classes.TList);
var
  o: TObject;
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    o := TObject(List[i]);
    o.Free;
  end;
end;

class procedure TUtils.ClearList(List: TStringList);
var
  i: Integer;
  o: TObject;
begin
  for i := 0 to List.Count - 1 do
  begin
    o := List.Objects[i];
    if Assigned(o) then
      o.Free;
  end;
end;

class procedure TUtils.ClearList(List: TThreadList);
var
  lockedList: TList;
  i: Integer;
  o: TObject;
begin
  lockedList:= List.LockList;
  try
    for i:= 0 to lockedList.Count-1 do
    begin
      o:= TObject(lockedList[i]);
      o.Free;
    end;
  finally
    List.UnlockList;
  end;
end;

class procedure TUtils.Split(ATokens: TStrings; AContent, ADelimiter: string);
var
  start, count: Integer;
  token, data: string;
  charPos, len, lenDelimiter: Integer;
  eof: Boolean;
begin
  start := 1;
  lenDelimiter := Length(ADelimiter);
  data := Trim(AContent);

  eof := False;

  while not eof do
  begin
    charPos := Pos(ADelimiter, data);
    eof := (charPos = 0);
    if eof then
      token := data
    else begin
      count := charPos - start;
      token := Copy(data, start, count);
    end;

    ATokens.Add(token);

    len := Length(data);
    data := Copy(data, charPos + lenDelimiter, len);
  end;
end;

class procedure TUtils.Split(ATokens: TStringList; AContent: string;
  ADelimiter: Char);
begin
  TRJSONHelper.Split(ATokens, AContent, ADelimiter);
end;

class procedure TUtils.Exec(Cmd: string; Params: TStringList;
  IsWaitOnExit: Boolean);
var
  p: TProcess;
  s: TStringList;
begin
  p := TProcess.Create(nil);
  s := TStringList.Create;
  try
    try
      p.Executable := Cmd;
      p.Parameters.Assign(Params);
      p.Options := [poUsePipes, poStderrToOutPut];
      if IsWaitOnExit then
        p.Options := p.Options + [poWaitOnExit];
      p.ShowWindow := swoHIDE;

      p.Execute;
      s.LoadFromStream(p.Output);
      if s.Text <> '' then
        raise Exception.Create(s.Text);
    except
      on E: Exception do
        raise;
    end;
  finally
    p.Free;
    s.Free;
  end;
end;

end.

