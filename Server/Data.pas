unit Data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTypes, IniFiles;

const
  PREFIX_CONNECTION_SECTION_DEF = 'CNX';
  SECTION_DELIMITER = '_';

type

  { TSettings }

  TSettings = class
  private
    fApplication: TStringList;
    fBehaviour: TStringList;
    fConnection: TStringList;
    fDatabase: TStringList;
    fConnectionInfos: TListConnectionInfo;
    fMail: TStringList;
    fMessageBroker: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetConnectionInfo(Id: string): PConnectionInfo;
  published
    property Application: TStringList read fApplication write fApplication;
    property Database: TStringList read fDatabase write fDatabase;
    property Connection: TStringList read fConnection write fConnection;
    property Mail: TStringList read fMail write fMail;
    property Behaviour: TStringList read fBehaviour write fBehaviour;
    property ConnectionInfos: TListConnectionInfo read fConnectionInfos write fConnectionInfos;
    property MessageBroker: TStringList read fMessageBroker write fMessageBroker;
  end;

  { TDataApp }

  TDataApp = class
  private
    fAuditTrailEnabled: Boolean;
    fSettings: TSettings;
    fSQLDict: TStringList;
    procedure DoLoadSettings();
    procedure DoLoadSQLDictionary();
    procedure DoLoadConnectionInfo(List: TListConnectionInfo; Config: TIniFile);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadAllSettings();
  published
    property Settings: TSettings read fSettings write fSettings;
    property SQLDict: TStringList read fSQLDict;
    property AuditTrailEnabled: Boolean read fAuditTrailEnabled;
  end;

var
  DataApp: TDataApp;

implementation

uses Utils;

{ TSettings }

constructor TSettings.Create;
begin
  fApplication := TStringList.Create;
  fDatabase := TStringList.Create;
  fConnection := TStringList.Create;
  fMail := TStringList.Create;
  fBehaviour := TStringList.Create;
  fConnectionInfos := TListConnectionInfo.Create;
  fMessageBroker:= TStringList.Create;
end;

destructor TSettings.Destroy;
begin
  fApplication.Free;
  fDatabase.Free;
  fConnection.Free;
  fMail.Free;
  fBehaviour.Free;

  TUtils.ClearList(fConnectionInfos);
  fConnectionInfos.Free;

  fMessageBroker.Free;

  inherited Destroy;
end;

function TSettings.GetConnectionInfo(Id: string): PConnectionInfo;
var
  i: Integer;
  c: PConnectionInfo;
begin
  Result := nil;
  for i := 0 to fConnectionInfos.Count - 1 do
  begin
    c := fConnectionInfos[i];
    if AnsiSameText(c^.ConnectionId, Id) then
    begin
      Result := c;
      Exit;
    end;
  end;
end;

{ TData }

procedure TDataApp.DoLoadSettings();
var
  f: TIniFile;
  fname: string;
begin
  fname := ExtractFileName(ParamStr(0));
  fname := ChangeFileExt(fname, '.conf');
  fname := ExtractFilePath(ParamStr(0)) + fname;
  f := TIniFile.Create(fname);
  try
    f.ReadSectionValues('Application', fSettings.Application);
    f.ReadSectionValues('Database', fSettings.Database);
    f.ReadSectionValues('Connection', fSettings.Connection);
    f.ReadSectionValues('Mail', fSettings.Mail);
    f.ReadSectionValues('Behaviour', fSettings.Behaviour);
    f.ReadSectionValues('MessageBroker', fSettings.MessageBroker);

    DoLoadConnectionInfo(fSettings.ConnectionInfos, f);
  finally
    f.Free;
  end;
end;

procedure TDataApp.DoLoadSQLDictionary();
var
  ini: TIniFile;
  section: string;
begin
  ini := TIniFile.Create(fSettings.Database.Values['SyntaxDictionary']);
  try
    section := fSettings.Database.Values['SyntaxDialect'];
    ini.ReadSectionValues(section, fSQLDict);
  finally
    ini.Free;
  end;
end;

procedure TDataApp.DoLoadConnectionInfo(List: TListConnectionInfo;
  Config: TIniFile);
var
  sections, params: TStringList;
  section: string;
  i: Integer;
  posDelimiter: SizeInt;
  info: PConnectionInfo;
begin
  sections := TStringList.Create;
  try
    Config.ReadSections(sections);
    for i := 0 to sections.Count - 1 do
    begin
      posDelimiter := Pos(SECTION_DELIMITER, sections[i]);
      section := Copy(sections[i], 1, posDelimiter - 1);
      if not AnsiSameText(section, PREFIX_CONNECTION_SECTION_DEF) then
        Continue;

      params := TStringList.Create;
      try
        Config.ReadSectionValues(sections[i], params);

        New(info);
        info^.ConnectionId := sections[i];
        info^.DriverName := params.Values['DriverName'];
        info^.Server := params.Values['Server'];
        info^.Port:= StrToIntDef(params.Values['Port'], 0);
        info^.Database := params.Values['Database'];
        info^.Username := params.Values['Username'];
        info^.Password := params.Values['Password'];
        info^.MinConnection:= StrToIntDef(params.Values['MinConnectionCount'], 1);

        List.Add(info);
      finally
        params.Free;
      end;
    end;
  finally
    sections.Free;
  end;
end;

constructor TDataApp.Create;
begin
  fSettings := TSettings.Create;
  DoLoadSettings();

  fAuditTrailEnabled:= fSettings.Behaviour.Values['EnableAuditTrail'].ToBoolean;

  fSQLDict := TStringList.Create;
  DoLoadSQLDictionary();
end;

destructor TDataApp.Destroy;
begin
  fSettings.Free;
  fSQLDict.Free;
  inherited Destroy;
end;

procedure TDataApp.LoadAllSettings();
begin
  DoLoadSettings();
  DoLoadSQLDictionary();
end;

end.

