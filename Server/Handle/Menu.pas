unit Menu;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, HandleDB, EntryMenu;

type

  { TMenu }

  TMenu = class(THandleDB)
  private
    function DoIsHasEdges(E: TEntryMenuItem; Flag: Boolean = False): Boolean;
    function DoGetMenuCaption(MenuId: string; LanguageId: string): string;
    procedure DoRemoveMenuItem(Target, Ref: TList);

    procedure DoFetchUserMenu(List: TList; ParentId, UserId: string; Level: Word);
    procedure DoRemoveUnusedMenu(List: TList);
  public
    procedure FetchUserMenu(List: TList);
  end;

implementation

uses Connection, SQLBuilder, UserAccess;

{ TMenu }

function TMenu.DoIsHasEdges(E: TEntryMenuItem; Flag: Boolean): Boolean;
var
  i: Integer;
  child: TEntryMenuItem;
begin
  Result:= Flag;
	for i:= 0 to E.Children.Count - 1 do
  begin
  	child:= TEntryMenuItem(E.Children[i]);
    if child.HasChild then
    	Result:= Result or DoIsHasEdges(child, Flag)
    else
      Result:= True;
  end;
end;

function TMenu.DoGetMenuCaption(MenuId: string; LanguageId: string): string;
var
  dset: TDataSet;
begin
  Result:= '';
  if (MenuId = '') or (LanguageId = '') then
		Exit;

  with (fConnection as TSQLDBConnection).SQL.Select do
	begin
    Field('caption');
    Table('sys_caption');
    Where('(menu_id', MenuId, otEqual);
    Where('language_id', LanguageId, otEqual, wtAND, True);

    dset:= Get();
    Result:= dset.FieldByName('caption').AsString;
    dset.Free;
  end;
end;

procedure TMenu.DoRemoveMenuItem(Target, Ref: TList);
var
  i: Integer;
  e: TEntryMenuItem;
begin
  for i:= 0 to Ref.Count - 1 do
  begin
    e:= TEntryMenuItem(Ref[i]);
    Target.Remove(e);
    e.Free;
  end;
end;

procedure TMenu.DoFetchUserMenu(List: TList; ParentId, UserId: string;
  Level: Word);
var
  dset: TDataSet;
  e: TEntryMenuItem;
  langId: string;
begin
  if List = nil then
    Exit;


  langId:= GetContent('LanguageId');

  with (fConnection as TSQLDBConnection).SQL.Select do
	begin
  	Field('DISTINCT m.*');
    Table('sys_menu m');
    LeftJoin('sys_group_access ga', 'ga.access_id = m.access_id');
    LeftJoin('sys_user_group ug', 'ug.entity_id = ga.entity_id AND ug.group_id = ga.group_id');
    Where('(m.parent_id');
    ParamValue(ParentId);
    Where('ISNULL(m.parent_id)', '1', otEqual, wtOR, True);
    Where('(ug.user_id', otEqual, wtAND);
    ParamValue(UserId);
    Where('(ISNULL(ug.user_id)', '1', otEqual, wtOR);
    Where('m.has_child', '1)', otEqual, wtAND, True);
    Where('m.level', Level.ToString, otEqual, wtAND);
    OrderBy('m.order');

    dset:= Get();
    try
      if dset.IsEmpty then
    		Exit;

      while not dset.EOF do
			begin
        e:= TEntryMenuItem.Create;
        e.Id:= dset.FieldByName('id').AsString;
        //e.Caption:= dset.FieldByName('caption').AsString;
        e.Caption:= DoGetMenuCaption(e.Id, GetContent('LanguageId'));
        e.Icon:= dset.FieldByName('icon').AsString;
        e.HasChild:= dset.FieldByName('has_child').AsBoolean;
        e.Route:= dset.FieldByName('route').AsString;
        e.AccessId:= dset.FieldByName('access_id').AsString;

        if e.HasChild then
         DoFetchUserMenu(e.Children, e.Id, UserId, Level+1);

        List.Add(e);
        dset.Next;
      end;
    finally
      dset.Free;
    end;
  end;
end;

procedure TMenu.DoRemoveUnusedMenu(List: TList);
var
  i: Integer;
  e: TEntryMenuItem;
  deleted: TList;
  hasEdges: Boolean;
begin
  deleted:= TList.Create;
  try
    for i:= 0 to List.Count - 1 do
    begin
      e:= TEntryMenuItem(List[i]);
      if not e.HasChild then
      begin
      	hasEdges:= True;
        Continue;
      end;

      hasEdges:= DoIsHasEdges(e);

      if e.HasChild and hasEdges then
        DoRemoveUnusedMenu(e.Children)
      else
    	  deleted.Add(e)
    end;

    DoRemoveMenuItem(List, deleted);
  finally
    deleted.Free;
  end;
end;

procedure TMenu.FetchUserMenu(List: TList);
begin
  DoFetchUserMenu(List, '', GetContent('UId'), 1);
  DoRemoveUnusedMenu(List);
end;

initialization

RegisterClass(TMenu)

end.

