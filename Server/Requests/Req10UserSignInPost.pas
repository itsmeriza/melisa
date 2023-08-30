unit Req10UserSignInPost;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Request, HTTPServer, RJSON, Entry, User,
  DBConnection, Command;

const
  ERR_AlreadySignedIn = -1;
  ERR_NoCredentialDefined = -2;
  ERR_NoIdentifierDefined = -3;
  ERR_UserDoesNotExist = -4;
  ERR_PasswordDoesNotMatch = -5;

type

  { TReq10UserSignInPost }

  TReq10UserSignInPost = class(TRequestHTTP)
  private
    procedure DoFetchUserGroups(User: TUser; Groups: TList; UId: string);
    procedure DoFetchUserAccesses(User: TUser; UId: string);
    procedure DoFetchUserMenus(Menus: TList);
  protected
    procedure DoPerform(var R: TResponse); override;
  public
    constructor Create(Command: TCommand); override;
    destructor Destroy; override;
  end;

implementation

uses MTypes, EntryUser, UserAccess, Utils, Data, Menu;

{ TReq10UserSignInPost }

procedure TReq10UserSignInPost.DoFetchUserGroups(User: TUser; Groups: TList; UId: string);
var
  list: TList;
  i: Integer;
begin
  TUtils.ClearList(Groups);
  list:= User.GetGroups(UId);
  try
    if list = nil then
  	  Exit;

    Groups.Clear();
    for i:= 0 to list.Count - 1 do
      Groups.Add(list[i]);
  finally
  	list.Free;
  end;
end;

procedure TReq10UserSignInPost.DoFetchUserAccesses(User: TUser; UId: string);
var
  list: TList;
  i: Integer;
  e: TEntryUserAccess;
  ua: TUserAccessItem;
  isUseOwnList: Boolean;
begin
  list := User.GetAccessRights(UId);
  if list = nil then
    Exit;

  ua := TUserAccessItem.Create;
  ua.UId := UId;
  ua.Session := TCommandHTTP(fCommand).RequestInfo.Session;

  try
    for i := 0 to list.Count - 1 do
    begin
      e := TEntryUserAccess(list[i]);
      ua.Add(e.AccessId, e.Command);

      { actually no need to send the access right scheme to client in server mode.
        remove this code in the future.

      access := TEntryUserAccessRight.Create;
      access.AccessId := tokens[0];
      access.AccessName := e.AccessName;
      access.FormId := e.FormId;

      AccessRights.Add(access);
      }
    end;

    isUseOwnList := StrToBoolDef(DataApp.Settings.Behaviour.Values['IsUseOwnUserAccessList'], True);
    if isUseOwnList then
      THTTPServer(fCommand.Owner).UserAccess.Add(ua)
    else
      TCommandHTTP(fCommand).SetUserAccessAddr(IntToStr(QWord(ua)));
  finally
    TUtils.ClearList(list);
    list.Free;
  end;
end;

procedure TReq10UserSignInPost.DoFetchUserMenus(Menus: TList);
var
  objMenu: TMenu;
  list: TList;
begin
  objMenu:= TMenu(fObjF.CreateObject(TMenu.ClassName));

  list:= THTTPServer(fCommand.Owner).UserAccess.Get(TCommandHTTP(fCommand).GetUId());
  if list = nil then
		Exit;

  objMenu.FetchUserMenu(Menus);
end;

procedure TReq10UserSignInPost.DoPerform(var R: TResponse);
var
  credential: TDataCredential;
  data: string;
  objUser: TUser;
  e: TEntryUser;
  t: TEntryToken;
  o: TObject;
begin
  data:= TCommandHTTP(fCommand).Post('data');
  if data = '' then
  begin
    R.ErrorCode:= ERR_NoCredentialDefined;
    Exit;
  end;

  credential:= TDataCredential.Create;
  try
  	TRJSON.ToObject(credential, data);

    if credential.UserID = '' then
    begin
      R.ErrorCode := ERR_NoIdentifierDefined;
      Exit;
    end;

    objUser := TUser(fObjF.CreateObject(TUserConst.CLASS_NAME));
    if not objUser.IsExist(credential.UserID) then
    begin
      R.ErrorCode := ERR_UserDoesNotExist;
      Exit;
    end;

    e := objUser.SignIn(credential.UserID, credential.Password);
    if e = nil then
    begin
      R.ErrorCode := ERR_PasswordDoesNotMatch;
      Exit;
    end;

    TCommandHTTP(fCommand).SetUId(e.UId);
    TCommandHTTP(fCommand).SetWpId(e.EntityId);
    TCommandHTTP(fCommand).SetLanguageId('2');

    DoFetchUserAccesses(objUser, e.UId);
    DoFetchUserMenus(e.Menus);

    t:= objUser.GenerateToken();
    TCommandHTTP(fCommand).SetToken(t.Key);
    e.Token.Key:= t.Key;
    e.Token.Length:= t.Length;
    t.Free;

    R.Data.Add(e);
  finally
    credential.Free;
  end;
end;

constructor TReq10UserSignInPost.Create(Command: TCommand);
begin
  inherited Create(Command);
  fCommandTypes := [TCommandType.hcPOST];
  fIsNeedSignIn := False;
end;

destructor TReq10UserSignInPost.Destroy;
begin
  inherited Destroy;
end;

initialization

RegisterClass(TReq10UserSignInPost);

end.

