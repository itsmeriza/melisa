unit ObjectFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectBase, IntfObjectFactory, Utils;

type

  { TObjectFactory }

  TObjectFactory = class(TObjectBase, IObjectFactory)
  protected
    fObjects: TThreadList;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    function GetObject(ClassId: string): TObjectBase; virtual; abstract;
    function CreateObject(ClassId: string): TObjectBase; virtual; abstract;

    procedure ClearObjects();
    procedure RemoveObject(ClassId: string); virtual;
  end;

implementation

{ TObjectFactory }

constructor TObjectFactory.Create();
begin
  fObjects := TThreadList.Create;
end;

destructor TObjectFactory.Destroy();
begin
  ClearObjects();
  fObjects.Free;
  inherited Destroy();
end;

procedure TObjectFactory.ClearObjects();
var
  list: TList;
begin
  list := fObjects.LockList;
  try
    TUtils.ClearList(list);
  finally
    fObjects.UnlockList;
  end;
end;

procedure TObjectFactory.RemoveObject(ClassId: string);
var
  o: TObjectBase;
  list: TList;
begin
  o := GetObject(ClassId);
  if o = nil then
    Exit;

  list := fObjects.LockList;
  try
    list.Remove(o);
    o.Free;
  finally
    fObjects.UnlockList;
  end;
end;

end.


