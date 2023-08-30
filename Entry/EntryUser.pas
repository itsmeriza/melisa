unit EntryUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Entry, Utils;

type

  { TEntryUserGroup }

  TEntryUserGroup = class(TEntry)
  private
    fGroupId: string;
    fLandingpage: string;
    fUserId: string;
  published
    property UserId: string read fUserId write fUserId;
    property GroupId: string read fGroupId write fGroupId;
    property LandingPage: string read fLandingpage write fLandingPage;
  end;

  { TEntryUserInfo }

  TEntryUserInfo = class(TEntry)
  private
    fIsFirstTimeLogon: Boolean;
    fLockedDate: TDateTime;
    fUserId: string;
    fWrongPasswordCount: Integer;
  published
    property UserId: string read fUserId write fUserId;
    property IsFirstTimeLogon: Boolean read fIsFirstTimeLogon write fIsFirstTimeLogon;
    property LockedDate: TDateTime read fLockedDate write fLockedDate;
    property WrongPasswordCount: Integer read fWrongPasswordCount write fWrongPasswordCount;
  end;

  { TEntryUserAccessRight }

  TEntryUserAccess = class(TEntry)
  private
    fAccessId: string;
    fAccessName: string;
    fCommand: string;
    fFormId: string;
    fModuleId: string;
    fUserId: string;
  published
    property UserId: string read fUserId write fUserId;
    property AccessId: string read fAccessId write fAccessId;
    property AccessName: string read fAccessName write fAccessName;
    property ModuleId: string read fModuleId write fModuleId;
    property Command: string read fCommand write fCommand;
  end;

  { TEntryUserLogonHistory }

  TEntryUserLogonHistory = class(TEntry)
  private
    fHistoryId: string;
    fLogoffTick: TDateTime;
    fLogonTick: TDateTime;
    fUId: string;
  published
    property HistoryId: string read fHistoryId write fHistoryId;
    property UId: string read fUId write fUId;
    property LogonTick: TDateTime read fLogonTick write fLogonTick;
    property LogoffTick: TDateTime read fLogoffTick write fLogoffTick;
  end;

  { TEntryUser }

  TEntryUser = class(TEntry)
  private
    fAccessRights: TList;
    fEmail: string;
    fEmployeeId: string;
    fEmployeeName: string;
    fEntityId: string;
    fEntityName: string;
    fFirstName: string;
    fGroups: TList;
    fLastName: string;
    fMenus: TList;
    fMobilePhone: string;
    fNotes: string;
    fToken: TEntryToken;
    fUId: string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetGroup(GId: string): TEntryUserGroup;
  published
    property UId: string read fUId write fUId;
    property Notes: string read fNotes write fNotes;
    property EntityId: string read fEntityId write fEntityId;
    property EntityName: string read fEntityName write fEntityName;
    property FirstName: string read fFirstName write fFirstName;
    property LastName: string read fLastName write fLastName;
    property EmployeeId: string read fEmployeeId write fEmployeeId;
    property EmployeeName: string read fEmployeeName write fEmployeeName;
    property Email: string read fEmail write fEmail;
    property MobilePhone: string read fMobilePhone write fMobilePhone;
    property Token: TEntryToken read fToken write fToken;
    property AccessRights: TList read fAccessRights write fAccessRights;
    property Groups: TList read fGroups write fGroups;
    property Menus: TList read fMenus write fMenus;
  end;

  { TEntryUserGetList }

  TEntryUserGetList = class(TEntry)
  private
    fEmployeeId: string;
    fEmployeeName: string;
    fExpiredDate: TDateTime;
    fUId: string;
    fUserName: string;
    fWpId: string;
    fWpName: string;
  published
    property UId: string read fUId  write fUId;
    property UserName: string read fUserName write fUserName;
    property WpId: string read fWpId write fWpId;
    property WpName: string read fWpName write fWpName;
    property ExpiredDate: TDateTime read fExpiredDate write fExpiredDate;
    property EmployeeId: string read fEmployeeId write fEmployeeId;
    property EmployeeName: string read fEmployeeName write fEmployeeName;
  end;


  {
    Data structure to hold the data sent by client
  }

  { TEntryUserGroupReq }

  TEntryUserGroupReq = class(TCollectionItem)
  private
    fGroupId: string;
    fUId: string;
  published
    property UId: string read fUId write fUId;
    property GroupId: string read fGroupId write fGroupId;
  end;

  { TEntryUserData }

  TEntryUserData = class(TEntry)
  private
    fEmployeeId: string;
    fEmployeeName: string;
    fExpiredDate: TDateTime;
    fFullName: string;
    fGroups: TCollection;
    fIsDefault: Boolean;
    fIsUnlimited: Boolean;
    fNotes: string;
    fPassword: string;
    fUId: string;
    fUserName: string;
    fWorkplaceID: string;
    fWorkplaceName: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property UId: string read fUId write fUId;
    property UserName: string read fUserName write fUserName;
    property FullName: string read fFullName write fFullName;
    property WpId: string read fWorkplaceID write fWorkplaceID;
    property WpName: string read fWorkplaceName write fWorkplaceName;
    property ExpiredDate: TDateTime read fExpiredDate write fExpiredDate;
    property EmployeeId: string read fEmployeeId write fEmployeeId;
    property EmployeeName: string read fEmployeeName write fEmployeeName;
    property Notes: string read fNotes write fNotes;
    property Password: string read fPassword write fPassword;
    property IsUnlimited: Boolean read fIsUnlimited write fIsUnlimited;
    property IsDefault: Boolean read fIsDefault write fIsDefault;
    property Groups: TCollection read fGroups write fGroups;
  end;

  { TDataCredential }

  TDataCredential = class(TEntry)
  private
    fPassword: string;
    fUserID: string;
  published
    property UserID: string read fUserID write fUserID;
    property Password: string read fPassword write fPassword;
  end;

implementation

{ TEntryUserData }

constructor TEntryUserData.Create;
begin
  fGroups := TCollection.Create(TEntryUserGroupReq);
end;

destructor TEntryUserData.Destroy;
begin
  fGroups.Clear;
  fGroups.Free;
  inherited Destroy;
end;

{ TEntryUser }

constructor TEntryUser.Create;
begin
  fAccessRights := TList.Create();
  fGroups := Classes.TList.Create();
  fMenus:= TList.Create();
  fToken:= TEntryToken.Create;
end;

destructor TEntryUser.Destroy;
begin
  TUtils.ClearList(fAccessRights);
  fAccessRights.Free;
  TUtils.ClearList(fGroups);
  fGroups.Free;
  TUtils.ClearList(fMenus);
  fMenus.Free;

  fToken.Free;
  inherited Destroy;
end;

function TEntryUser.GetGroup(GId: string): TEntryUserGroup;
var
  i: Integer;
  e: TEntryUserGroup;
begin
  Result:= nil;
  for i:= 0 to fGroups.Count - 1 do
  begin
  	e := TEntryUserGroup(fGroups[i]);
    if e.fGroupId.CompareText(e.GroupId, GId) = 0 then
    begin
      Result:= e;
      Exit;
    end;
  end;
end;

end.


