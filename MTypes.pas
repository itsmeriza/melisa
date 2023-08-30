unit MTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdCustomHTTPServer;

type

  { TList }

  generic TList<T> = class
  private
    fItems: array of T;
    fCount: Integer;
    function GetT(i: LongInt): T;
    procedure SetT(i: LongInt; AValue: T);
  public
    function Count(): Integer;
    function Delete(AIndex: Integer): T;
    procedure Remove(AItem: T);
    procedure Add(AItem: T);
    procedure Clear();

    constructor Create;
    destructor Destroy; override;

    property Items[i: LongInt]: T read GetT write SetT; default;
  end;

  TConnectionInfo = record
    ConnectionId: string;
    DriverName: string;
    Server: string;
    Database: string;
    Username: string;
    Password: string;
  end;

  PConnectionInfo = ^TConnectionInfo;

  TListConnectionInfo = specialize TList<PConnectionInfo>;
  TListObject = specialize TList<TObject>;

  TCommandType = THTTPCommandType;
  TCommandTypes = set of TCommandType;

implementation

{ TList }

function TList.GetT(i: LongInt): T;
begin
  Result := fItems[i];
end;

procedure TList.SetT(i: LongInt; AValue: T);
begin
  fItems[i] := AValue;
end;

function TList.Count(): Integer;
begin
  Result := fCount;
end;

function TList.Delete(AIndex: Integer): T;
var
  i: Integer;
begin
  Result := nil;
  if (AIndex < 0) or (AIndex > fCount - 1) then
    raise Exception.Create('List out of bound.');

  Result := fItems[AIndex];
  for i := AIndex to fCount - 1 do
    fItems[i] := fItems[i + 1];

  Dec(fCount);
  SetLength(fItems, fCount);
end;

procedure TList.Remove(AItem: T);
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
  begin
    if fItems[i] = AItem then
    begin
      Delete(i);
      Exit;
    end;
  end;
end;

procedure TList.Add(AItem: T);
begin
  SetLength(fItems, Length(fItems) + 1);
  Items[Length(fItems) - 1] := AItem;
  Inc(fCount);
end;

procedure TList.Clear();
begin
  SetLength(fItems, 0);
end;

constructor TList.Create;
begin
  fCount := 0;
end;

destructor TList.Destroy;
begin
  inherited Destroy;
end;

end.

