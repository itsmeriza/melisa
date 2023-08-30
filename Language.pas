unit Language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Entry;

const
  LANGUAGE_FILE_EXTENSION = '.lang';

type

  { TEntryLanguage }

  TEntryLanguage = class(TEntry)
  private
    fFilePath: string;
    fId: Integer;
    fLanguage: string;
  published
    property Id: Integer read fId write fId;
    property Language: string read fLanguage write fLanguage;
    property FilePath: string read fFilePath write fFilePath;
  end;

  { TLanguage }

  TLanguage = class
  private
    fLanguages: TList;
    fLanguageFolder: string;

    procedure DoSearchAvailableLanguage();
  public
    constructor Create(ALanguageFolder: string);
    destructor Destroy; override;

    procedure CreateMapCaption(ALanguageDB: TStrings; ASection: string; AMap: TStrings);

    property Languages: TList read fLanguages;
    property LanguageFolder: string read fLanguageFolder;
  end;

implementation

uses Utils, IniFiles;

{ TLanguage }

procedure TLanguage.DoSearchAvailableLanguage();
var
  sr: TSearchRec;
  id: Integer;
  ext: string;
  lang: TEntryLanguage;
begin
  if FindFirst(fLanguageFolder + '\*.*', faAnyFile, sr) <> 0 then
    Exit;

  id := 0;
  while FindNext(sr) = 0 do
  begin
    ext := ExtractFileExt(sr.Name);
    if (sr.Name = '.') or (sr.Name = '..') or AnsiSameText(ext, LANGUAGE_FILE_EXTENSION) then
      Continue;

    lang := TEntryLanguage.Create;
    lang.Id := id;
    lang.Language := ChangeFileExt(sr.Name, '');
    lang.FilePath := ExtractFileName(sr.Name);

    fLanguages.Add(lang);
    Inc(id);
  end;
end;

constructor TLanguage.Create(ALanguageFolder: string);
begin
  fLanguageFolder := ALanguageFolder;
  fLanguages := Classes.TList.Create;

  DoSearchAvailableLanguage();
end;

destructor TLanguage.Destroy;
begin
  TUtils.ClearList(fLanguages);
  fLanguages.Free;
  inherited Destroy;
end;

procedure TLanguage.CreateMapCaption(ALanguageDB: TStrings; ASection: string;
  AMap: TStrings);
var
  filepath: string;
  iniFile: TIniFile;
begin
  if ALanguageDB = nil then
    Exit;

  AMap.Clear;
  filepath := '%s\%s.lang';
  filepath := Format(filepath, [fLanguages, ALanguageDB.Values['Language']]);
  iniFile :=  TIniFile.Create(filepath);
  iniFile.ReadSectionValues(ASection, AMap);
  iniFile.Free;
end;

end.


