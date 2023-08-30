unit ObjectFactoryDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectFactory, DBConnection, ObjectBase, HandleDB;

type

  { TObjectFactoryDB }

  TObjectFactoryDB = class(TObjectFactory)
  private
    fConnection: TDbConnection;
  public
    function CreateObject(ClassId: string): TObjectBase; override;
    function GetObject(ClassId: string): TObjectBase; override;
  published
    property Connection: TDbConnection read fConnection write fConnection;
  end;

implementation

{ TObjectFactoryDB }

function TObjectFactoryDB.CreateObject(ClassId: string): TObjectBase;
type
  THandleDBClass = class of THandleDB;
var
  tempObject: THandleDBClass;
begin
  Result := GetObject(ClassId);
  if Result <> nil then
    Exit;

  tempObject := THandleDBClass(FindClass(ClassId));
  Result := tempObject.Create(TObjectFactory(Self), fConnection);
  fObjects.Add(Result);
end;

function TObjectFactoryDB.GetObject(ClassId: string): TObjectBase;
var
  list: TList;
  i: Integer;
  handle: THandleDB;
begin
  Result := nil;
  list := fObjects.LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      handle := THandleDB(list[i]);
      if AnsiSameText(handle.ClassName, ClassId) then
      begin
        Result := handle;
        Exit;
      end;
    end;
  finally
    fObjects.UnlockList;
  end;
end;

end.


