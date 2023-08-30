unit Entry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, RJSON;

type

  { TEntry }

  TEntry = class
  private
    fJSON: string;
  public
    function ToJSON(): string;
    procedure ClearJSON();
    procedure UpdateJSON();
  end;

  { TEntryToken }

  TEntryToken = class(TEntry)
  private
    fKey: string;
    fLength: Byte;
  published
    property Key: string read fKey write fKey;
    property Length: Byte read fLength write fLength;
  end;

  { TEntryPair }

  TEntryPair = class(TEntry)
  private
    fKey: string;
    fValue: string;
  published
    property Key: string read fKey write fKey;
    property Value: string read fValue write fValue;
  end;

  { TEntryPairDouble }

  TEntryPairDouble = class(TEntry)
  private
    fKey: string;
    fValue: Double;
  published
    property Key: string read fKey write fKey;
    property Value: Double read fValue write fValue;
  end;

  TEntryBoolean = class(TRJSONBooleanHelper)
  end;

  TEntryString = class(TRJSONStringHelper)
  end;

  TEntryDouble = class(TRJSONDoubleHelper)
  end;

implementation

{ TEntry }

function TEntry.ToJSON(): string;
begin
  if fJSON <> '' then
  begin
    Result := fJSON;
    Exit;
  end;

  fJSON := TRJSON.ToJSON(Self, joAssociate);
  Result := fJSON;
end;

procedure TEntry.ClearJSON();
begin
  fJSON:= '';
end;

procedure TEntry.UpdateJSON();
begin
  fJSON:= ToJSON();
end;

end.

