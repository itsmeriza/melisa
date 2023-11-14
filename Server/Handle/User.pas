unit User;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EntryUser, Entry, HandleDB;

type

  { TUserConst }

  TUserConst = class
    class function CLASS_NAME: string;
  end;

  { TUser }

  TUser = class(THandleDB)
  private
    procedure DoAddEntryLogonHistory(UId: string);
    procedure DoAddEntryLogonHistory(Entry: TEntryUserLogonHistory);
    //procedure DoFetchUserGroups(E: TEntryUser);
    //procedure DoUpdateEntryUserGroup(E: TEntryUserData);
    //procedure DoDeleteEntriesUserGroup(UId: string);
    //procedure DoAddEntriesUserGroup(E: TEntryUserData);
    //procedure DoAddEntryUserGroup(E: TEntryUserGroupReq);
  public
    //function GetEntry(UId: string): TEntryUser;
    function IsExist(Identifier: string): Boolean;
    function SignIn(Identifier, Password: string): TEntryUser;
    function GetAccessRights(UId: string): TList;
    function GetGroups(UId: string): Classes.TList;
    //function GetEntryUserInfo(UId: string): TEntryUserInfo;
    //function CheckPassword(UId: string; Password: string): Boolean;
    //
    //procedure Logoff(UId: string);
    //procedure AddEntry(E: TEntryUserData);
    //procedure UpdateEntry(E: TEntryUserData);
    //procedure DeleteEntry(UId: string);
    //procedure UpdatePassword(UId: string; NewPassword: string);
    procedure RecordPerform(UId, Command: string);
    //procedure FetchList(Dest: TList; Key, SysAdminGId: string; IsSysAdmin: Boolean = False);
  end;

implementation

uses Connection, Commons, DB, SQLBuilder, Utils;

{ TUserConst }

class function TUserConst.CLASS_NAME: string;
begin
  Result := 'TUser';
end;

{ TUser }

procedure TUser.DoAddEntryLogonHistory(UId: string);
var
  e: TEntryUserLogonHistory;
begin
  e := TEntryUserLogonHistory.Create;
  e.UId := UId;
  e.LogonTick := Now();
  try
    DoAddEntryLogonHistory(e);
  finally
    e.Free;
  end;
end;
//
procedure TUser.DoAddEntryLogonHistory(Entry: TEntryUserLogonHistory);
begin
  with (fConnection as TSQLDBConnection).SQL.Insert do
  begin
    Table('EES_SYS_UserLogonHistory');
    Field('szUserId');
    ParamValue(Entry.UId);
    Field('dtmLogon');
    ParamValue(FormatDateTime(MYSQL_DATETIME_FORMAT, Entry.LogonTick));

    Exec();
  end;
end;
//
//procedure TUser.DoFetchUserGroups(E: TEntryUser);
//var
//  dset: TDataSet;
//  g: TEntryUserGroup;
//begin
//  with (fConnection as TSQLDBConnection).SQL.Select do
//  begin
//    Field('szUserId, szGroupId');
//    Table('EES_SYS_UserGroup');
//    Where('szUserId');
//    ParamValue(E.UId);
//
//    dset:= Get();
//    try
//      while not dset.EOF do
//      begin
//        g:= TEntryUserGroup.Create;
//        g.UserId:= dset.FieldByName('szUserId').AsString;
//        g.GroupId:= dset.FieldByName('szGroupId').AsString;
//
//        E.Groups.Add(g);
//        dset.Next;
//      end;
//    finally
//      dset.Free;
//    end;
//  end;
//end;
//
//procedure TUser.DoUpdateEntryUserGroup(E: TEntryUserData);
//begin
//  DoDeleteEntriesUserGroup(E.UId);
//  DoAddEntriesUserGroup(E);
//end;
//
//procedure TUser.DoDeleteEntriesUserGroup(UId: string);
//begin
//  with (fConnection as TSQLDBConnection).SQL.Delete do
//  begin
//    Table('EES_SYS_UserGroup');
//    Where('szUserId');
//    ParamValue(UId);
//
//    Exec();
//  end;
//end;
//
//procedure TUser.DoAddEntriesUserGroup(E: TEntryUserData);
//var
//  i: Integer;
//  ei: TEntryUserGroupReq;
//begin
//  for i:= 0 to E.Groups.Count - 1 do
//  begin
//    ei:= E.Groups.Items[i] as TEntryUserGroupReq;
//    ei.UId:= E.UId;
//    DoAddEntryUserGroup(ei);
//  end;
//end;
//
//procedure TUser.DoAddEntryUserGroup(E: TEntryUserGroupReq);
//begin
//  with (fConnection as TSQLDBConnection).SQL.Insert do
//  begin
//    Table('EES_SYS_UserGroup');
//    Field('szUserId');
//    Field('szGroupId');
//    ParamValue(E.UId);
//    ParamValue(E.GroupId);
//
//    Exec();
//  end;
//end;
//
//function TUser.GetEntry(UId: string): TEntryUser;
//var
//  dset: TDataSet;
//begin
//  Result := nil;
//  with (fConnection as TSQLDBConnection).SQL.Select do
//  begin
//    Field('u.*, w.szWpName, e.szEmployeeName');
//    Table('EES_SYS_User u');
//    LeftJoin('EES_GEN_Workplace w', 'w.szWorkplaceId = u.szWorkplaceId');
//    LeftJoin('EES_EMPL_Employee e', 'e.szEmployeeId = u.szEmployeeId');
//    Where('u.szUserId');
//    ParamValue(UId);
//
//    dset := Get();
//    try
//      if dset.IsEmpty then
//        Exit;
//
//      Result:= TEntryUser.Create;
//      Result.UId:= dset.FieldByName('szUserId').AsString;
//      Result.UserName:= dset.FieldByName('szName').AsString;
//      Result.WpId:= dset.FieldByName('szWorkplaceId').AsString;
//      Result.WpName:= dset.FieldByName('szWpName').AsString;
//      Result.EmployeeId:= dset.FieldByName('szEmployeeId').AsString;
//      Result.EmployeeName:= dset.FieldByName('szEmployeeName').AsString;
//      Result.ExpiredDate:= dset.FieldByName('dtmExpired').AsDateTime;
//
//      DoFetchUserGroups(Result);
//    finally
//      dset.Free;
//    end;
//  end;
//end;
//
function TUser.IsExist(Identifier: string): Boolean;
var
  id: string;
  dset: TDataSet;
begin
  Result := False;

  // identifier can be user id, user name, email, or phone number.
  id := Identifier;
  with (fConnection as TSQLDBConnection).SQL.Select do
  begin
    Field('COUNT(*)');
    Table('sys_user u');
    Join('sys_user_profile p', 'p.user_id = u.id', jtINNER);

    Where('u.active', '1');

    Where('(u.id', otEqual, wtAND);
    ParamValue(id);

    Where('p.email', otEqual, wtOR);
    ParamValue(id);

    Where('p.mobile_phone', otEqual, wtOR, True);
    ParamValue(id);

    dset := Get();
    Result := dset.Fields[0].AsInteger > 0;
    dset.Free;
  end;
end;
//
function TUser.SignIn(Identifier, Password: string): TEntryUser;
var
  dset: TDataSet;
  pwd: string;
begin
  Result := nil;
  pwd := Password + PASSWORD_SALT;

  with (fConnection as TSQLDBConnection).SQL.Select do
  begin
    Field('u.*');
    Field('e.name AS entity_name');
    Field('p.first_name, p.last_name, p.email, p.mobile_phone');

    Field('AES_DECRYPT(password, UNHEX(SHA2(?, 512))) AS raw_passwd');
    ParamValue(PASSWORD_KEY);

    Table('sys_user u');
    LeftJoin('sys_user_profile p', 'p.user_id = u.id');
    LeftJoin('sys_entity e', 'e.id = u.entity_id');

    Where('u.active', '1');

    Where('(u.id', otEqual, wtAND);
    ParamValue(Identifier);

    Where('p.email', otEqual, wtOR);
    ParamValue(Identifier);

    Where('p.mobile_phone', otEqual, wtOR, True);
    ParamValue(Identifier);

    dset := Get();
    try
      if dset.IsEmpty then
        Exit;

      if not AnsiSameText(dset.FieldByName('raw_passwd').AsString, pwd) then
        Exit;

      Result := TEntryUser.Create;
      Result.UId := dset.FieldByName('id').AsString;
      Result.Notes:= dset.FieldByName('notes').AsString;
      Result.EntityId:= dset.FieldByName('entity_id').AsString;
      Result.EntityName:= dset.FieldByName('entity_name').AsString;
      Result.FirstName:= dset.FieldByName('first_name').AsString;
      Result.LastName:= dset.FieldByName('last_name').AsString;
      Result.Email:= dset.FieldByName('email').AsString;
      Result.MobilePhone:= dset.FieldByName('mobile_phone').AsString;

      //DoAddEntryLogonHistory(Result.UId);
    finally
      dset.Free;
    end;
  end;
end;
//

function TUser.GetAccessRights(UId: string): TList;
var
  dset: TDataSet;
  e: TEntryUserAccess;
begin
  Result := nil;

  with (fConnection as TSQLDBConnection).SQL.Select do
  begin
    Field('DISTINCT a.*, ug.user_id');
    Table('sys_user_group ug');
    LeftJoin('sys_group_access ga', 'ga.entity_id = ug.entity_id AND ga.group_id = ug.group_id');
    LeftJoin('sys_access a', 'a.id = ga.access_id');
    LeftJoin('sys_module m', 'm.id = a.module_id');
		Where('ug.user_id');
    ParamValue(UId);

    dset := Get();
    try
      if dset.IsEmpty then
        Exit;

      Result := TList.Create;
      while not dset.EOF do
      begin
        e := TEntryUserAccess.Create;
        e.UserId:= dset.FieldByName('user_id').AsString;
        e.AccessId := dset.FieldByName('id').AsString;
        e.AccessName := dset.FieldByName('access_name').AsString;
        e.ModuleId := dset.FieldByName('module_id').AsString;
        e.Command:= dset.FieldByName('command').AsString;

        Result.Add(e);
        dset.Next;
      end;
    finally
      dset.Free;
    end;
  end;
end;
//
function TUser.GetGroups(UId: string): Classes.TList;
var
  dset: TDataSet;
  e: TEntryUserGroup;
begin
  Result := nil;
  with (fConnection as TSQLDBConnection).SQL.Select do
  begin
    Field('ug.szUserId, ug.szGroupId, p.Route');
    Table('EES_SYS_UserGroup ug');
    LeftJoin('OBS_SYS_LandingPage p', 'p.GroupId = ug.szGroupId');
    Where('szUserId');
    ParamValue(UId);

    dset:= Get();
    try
      if dset.IsEmpty then
        Exit;

      Result:= Classes.TList.Create;
      while not dset.EOF do
      begin
        e := TEntryUserGroup.Create;
        e.UserId:= dset.FieldByName('szUserId').AsString;
        e.GroupId:= dset.FieldByName('szGroupId').AsString;
        e.LandingPage:= dset.FieldByName('Route').AsString;

        Result.Add(e);
        dset.Next;
      end;
    finally
      dset.Free;
    end;
  end;
end;
//
//function TUser.GetEntryUserInfo(UId: string): TEntryUserInfo;
//var
//  dset: TDataSet;
//begin
//  Result := nil;
//  with (fConnection as TSQLDBConnection).SQL.Select do
//  begin
//    Field('*');
//    Table('EES_SYS_UserInfo');
//    Where('szUserId');
//    ParamValue(UId);
//    Where('dtmLocked', otGreaterThanOrEqual, wtAND);
//    ParamValue(FormatDateTime(MYSQL_DATETIME_FORMAT, Now()));
//
//    dset := Get();
//    try
//      if dset.IsEmpty then
//        Exit;
//
//      Result := TEntryUserInfo.Create;
//      Result.UserId := dset.FieldByName('szUserId').AsString;
//      Result.IsFirstTimeLogon := dset.FieldByName('isFirstTimeLogon').AsBoolean;
//      Result.LockedDate := dset.FieldByName('dtmLocked').AsDateTime;
//      Result.WrongPasswordCount := dset.FieldByName('intWrongPassword').AsInteger;
//    finally
//      dset.Free;
//    end;
//  end;
//end;
//
//function TUser.CheckPassword(UId: string; Password: string): Boolean;
//var
//  dset: TDataSet;
//begin
//  Result := False;
//  Password := Password + PASSWORD_SALT;
//  with (fConnection as TSQLDBConnection).SQL.Select do
//  begin
//    Field('szHash = MD5(?) AS is_valid');
//    ParamValue(Password);
//    Table('EES_SYS_UserPasswordHistory');
//    Where('szUserId');
//    ParamValue(UId);
//    OrderBy('intSequenceNumber DESC');
//    Limit(1);
//
//    dset := Get();
//    try
//      if dset.IsEmpty then
//        Exit;
//
//      Result := dset.FieldByName('is_valid').AsBoolean;
//    finally
//      dset.Free;
//    end;
//  end;
//end;
//
//procedure TUser.Logoff(UId: string);
//begin
//  with (fConnection as TSQLDBConnection).SQL.Update do
//  begin
//    Table('EES_SYS_UserLogonHistory');
//    Field('dtmLogoff=NOW()');
//    Where('szUserId');
//    ParamValue(UId);
//    OrderBy('intSequence DESC');
//    Limit(1);
//
//    Exec();
//  end;
//end;
//
//procedure TUser.AddEntry(E: TEntryUserData);
//begin
//  if E.WpId = '' then
//    E.WpId := 'NULL';
//  if E.EmployeeId = '' then
//    E.EmployeeId := 'NULL';
//
//  with (fConnection as TSQLDBConnection).SQL.Insert do
//  begin
//    Table('EES_SYS_User');
//    Field('szUserId');
//    Field('szName');
//    Field('szWorkplaceId');
//    Field('dtmExpired');
//    Field('szEmployeeId');
//
//    ParamValue(E.UId);
//    ParamValue(E.UserName);
//    ParamValue(E.WpId);
//    ParamValue(FormatDateTime(MYSQL_DATETIME_FORMAT, E.ExpiredDate));
//    ParamValue(E.EmployeeId);
//
//    Exec();
//  end;
//
//  DoAddEntriesUserGroup(E);
//end;
//
//procedure TUser.UpdateEntry(E: TEntryUserData);
//begin
//  if E.WpId = '' then
//    E.WpId := 'NULL';
//  if E.EmployeeId = '' then
//    E.EmployeeId := 'NULL';
//
//  with (fConnection as TSQLDBConnection).SQL.Update do
//  begin
//    Table('EES_SYS_User');
//    Field('szName=?');
//    Field('szWorkplaceId=?');
//    Field('dtmExpired=?');
//    Field('szEmployeeId=?');
//    Where('szUserId');
//
//    ParamValue(E.UserName);
//    ParamValue(E.WpId);
//    ParamValue(FormatDateTime(MYSQL_DATETIME_FORMAT, E.ExpiredDate));
//    ParamValue(E.EmployeeId);
//    ParamValue(E.UId);
//
//    Exec();
//  end;
//
//  DoUpdateEntryUserGroup(E);
//end;
//
//procedure TUser.DeleteEntry(UId: string);
//begin
//  with (fConnection as TSQLDBConnection).SQL.Delete do
//  begin
//    Table('EES_SYS_User');
//    Where('szUserId');
//    ParamValue(UId);
//
//    Exec();
//  end;
//end;
//
//procedure TUser.UpdatePassword(UId: string; NewPassword: string);
//begin
//  NewPassword := NewPassword + PASSWORD_SALT;
//  with (fConnection as TSQLDBConnection).SQL.Insert do
//  begin
//    Table('EES_SYS_UserPasswordHistory');
//    Field('szUserId');
//    ParamValue(UId);
//    Field('szHash', 'MD5(?)');
//    ParamValue(NewPassword);
//
//    Exec();
//  end;
//end;
//
procedure TUser.RecordPerform(UId, Command: string);
begin
  with (fConnection as TSQLDBConnection).SQL.Insert do
  begin
    Table('EES_SYS_UserAuditTrail');
    Field('szUserId');
    ParamValue(UId);
    Field('szFormId', '"*"');
    Field('szActionId');
    ParamValue(Command);
    Field('dtmRecorded', 'NOW()');

    Exec();
  end;
end;
//
//procedure TUser.FetchList(Dest: TList; Key, SysAdminGId: string;
//  IsSysAdmin: Boolean);
//var
//  dset: TDataSet;
//  e: TEntryUserGetList;
//begin
//  with (fConnection as TSQLDBConnection).SQL.Select do
//  begin
//    Field('u.*, w.szWpName, e.szEmployeeName');
//    Table('EES_SYS_User u');
//    if not IsSysAdmin then
//    	LeftJoin('EES_SYS_UserGroup ug', 'ug.szUserId = u.szUserId');
//    LeftJoin('EES_GEN_Workplace w', 'w.szWorkplaceId = u.szWorkplaceId');
//    LeftJoin('EES_EMPL_Employee e', 'e.szEmployeeId = u.szEmployeeId');
//    Where('1', '1');
//    if fWpId <> '' then
//    begin
//      Where('u.szWorkplaceId', otEqual, wtAND);
//      ParamValue(fWpId);
//    end;
//
//    if not IsSysAdmin then
//    begin
//    	Where('ug.szGroupId', otNotEqual, wtAND);
//      ParamValue(SysAdminGId);
//    end;
//
//    if Key <> '' then
//    begin
//      Where('(u.szUserId', otLike, wtAND);
//      ParamValue(Key);
//      Where('u.szName', otLike, wtOR);
//      ParamValue(Key);
//      Where('u.szWorkplaceId', otLike, wtOR);
//      ParamValue(Key);
//      Where('w.szWpName', otLike, wtOR);
//      ParamValue(Key);
//      Where('u.szEmployeeId', otLike, wtOR);
//      ParamValue(Key);
//      Where('e.szEmployeeName', otLike, wtOR, True);
//      ParamValue(Key);
//    end;
//
//    dset:= Get();
//    if dset.IsEmpty then
//      Exit;
//
//    while not dset.EOF do
//    begin
//      e:= TEntryUserGetList.Create;
//      e.UId:= dset.FieldByName('szUserId').AsString;
//      e.UserName:= dset.FieldByName('szName').AsString;
//      e.WpId:= dset.FieldByName('szWorkplaceId').AsString;
//      e.WpName:= dset.FieldByName('szWpName').AsString;
//      e.EmployeeId:= dset.FieldByName('szEmployeeId').AsString;
//      e.EmployeeName:= dset.FieldByName('szEmployeeName').AsString;
//      e.ExpiredDate:= dset.FieldByName('dtmExpired').AsDateTime;
//
//      Dest.Add(e);
//      dset.Next;
//    end;
//  end;
//end;

initialization

RegisterClass(TUser);

end.


