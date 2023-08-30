unit Handle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectBase, ObjectFactory, Language;

type

  { THandle }

  THandle = class(TObjectBase)
  protected
    fCaptions: TStringList;
    fObjF: TObjectFactory;
    fLanguage: TLanguage;

    procedure DoInitLanguage(); virtual; abstract;
  public
    constructor Create(ObjF: TObjectFactory; Language: TLanguage; LanguageDbase: TStrings); virtual;
    destructor Destroy; override;
  end;

implementation

{ IHandle }

constructor THandle.Create(ObjF: TObjectFactory; Language: TLanguage;
  LanguageDbase: TStrings);
begin
  fObjF := ObjF;
  fLanguage := Language;

  fCaptions := TStringList.Create;
  if Assigned(fLanguage) then
    fLanguage.CreateMapCaption(LanguageDBase, Self.ClassName, fCaptions);

  DoInitLanguage();
end;

destructor THandle.Destroy;
begin
  fCaptions.Free;
  inherited Destroy;
end;

end.


