unit UserAccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAccess }

  TAccess = class
  private
    fAccessId: string;
    fCommand: string;
  published
    property AccessId: string read fAccessId write fAccessId;
    property Command: string read fCommand write fCommand;
  end;

  { TUserAccessItem }

  TUserAccessItem = class
  private
    fAccessList: TList;
    fSession: TObject;
    fUId: string;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(var Index: Integer; Command: string): TAccess;

    procedure Add(AccessId, Command: string);
    procedure Delete(ReqId: string);
  published
    property Session: TObject read fSession write fSession;
    property UId: string read fUId write fUId;
    property AccessList: TList read fAccessList write fAccessList;
  end;

  { TUserAccess }

  TUserAccess = class
  private
    fList: TThreadList;
    procedure DoClearList();
  public
    constructor Create;
    destructor Destroy; override;

    function Get(UId: string): TList;
    function Get(var Index: Integer; Session: TObject): TUserAccessItem;

    procedure Add(E: TUserAccessItem);
    procedure Delete(UId: string);
    procedure Delete(Session: TObject);
  end;

implementation

uses Utils;

{ TUserAccessItem }

constructor TUserAccessItem.Create;
begin
  fAccessList := TList.Create;
end;

destructor TUserAccessItem.Destroy;
begin
  TUtils.ClearList(fAccessList);
  fAccessList.Free;
  inherited Destroy;
end;

function TUserAccessItem.Get(var Index: Integer; Command: string): TAccess;
var
  i: Integer;
  e: TAccess;
begin
  Result := nil;
  for i := 0 to fAccessList.Count - 1 do
  begin
    e := TAccess(fAccessList[i]);
    if AnsiSameText(e.Command, Command) then
    begin
      Index := i;
      Result := e;
      Exit;
    end;
  end;
end;

procedure TUserAccessItem.Add(AccessId, Command: string);
var
  e: TAccess;
  i: Integer;
begin
  i := 0;
  e := Get(i, Command);
  if e <> nil then
    Exit;

  e := TAccess.Create;
  e.AccessId := AccessId;
  e.Command := Command;
  fAccessList.Add(e);
end;

procedure TUserAccessItem.Delete(ReqId: string);
var
  e: TAccess;
  i: Integer;
begin
  i := 0;
  e := Get(i, ReqId);
  if e = nil then
    Exit;

  fAccessList.Delete(i);
  e.Free;
end;

{ TUserAccess }

procedure TUserAccess.DoClearList();
var
  list: TList;
begin
  list := fList.LockList;
  try
    TUtils.ClearList(list);
  finally
    fList.UnlockList;
  end;
end;

constructor TUserAccess.Create;
begin
  fList := TThreadList.Create;
end;

destructor TUserAccess.Destroy;
begin
  DoClearList();
  fList.Free;
  inherited Destroy;
end;

function TUserAccess.Get(UId: string): TList;
var
  i: Integer;
  list: TList;
  e: TUserAccessItem;
begin
  Result := nil;
  list := fList.LockList;
  try
    if list.Count = 0 then
      Exit;

    for i := 0 to list.Count - 1 do
    begin
      e := TUserAccessItem(list[i]);
      if e.UId.CompareTo(UId) = 0 then
      begin
        Result:= e.AccessList;
        Exit;
      end;
    end;
  finally
    fList.UnlockList;
  end;
end;

function TUserAccess.Get(var Index: Integer; Session: TObject): TUserAccessItem;
var
  list: TList;
  i: Integer;
  e: TUserAccessItem;
begin
  Result := nil;
  list := fList.LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      e := TUserAccessItem(list[i]);
      if e.Session = Session then
      begin
        Index := i;
        Result := e;
        Exit;
      end;
    end;
  finally
    fList.UnlockList;
  end;
end;

procedure TUserAccess.Add(E: TUserAccessItem);
begin
  fList.Add(E);
end;

procedure TUserAccess.Delete(UId: string);
var
  e: TUserAccessItem;
  list: TList;
  i: Integer;
begin
  list := fList.LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      e := TUserAccessItem(list[i]);
      if e.UId.CompareTo(UId) = 0 then
      begin
        list.Delete(i);
        e.Free;
      end;
    end;
  finally
    fList.UnlockList;
  end;
end;

procedure TUserAccess.Delete(Session: TObject);
var
  list: TList;
  e: TUserAccessItem;
  i: Integer;
begin
  list := fList.LockList;
  try
    for i := 0 to list.Count - 1 do
    begin
      e := TUserAccessItem(list[i]);
      if e.Session = Session then
      begin
        list.Delete(i);
        e.Free;
      end;
    end;
  finally
    fList.UnLockList;
  end;
end;

end.

