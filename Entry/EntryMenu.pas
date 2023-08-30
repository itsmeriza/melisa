unit EntryMenu;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Entry;

type

  { TEntryMenuItem }

  TEntryMenuItem = class(TEntry)
  private
    fAccessId: string;
    fCaption: string;
    fChildren: TList;
    fHasChild: Boolean;
    fIcon: string;
    fId: string;
    fRoute: string;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property AccessId: string read fAccessId write fAccessId;
  published
    property Id: string read fId write fId;
    property Caption: string read fCaption write fCaption;
    property Icon: string read fIcon write fIcon;
    property Route: string read fRoute write fRoute;
    property HasChild: Boolean read fHasChild write fHasChild;
    property Children: TList read fChildren write fChildren;
  end;

implementation

uses Utils;

{ TEntryMenuItem }

constructor TEntryMenuItem.Create;
begin
  fChildren:= TList.Create;
end;

destructor TEntryMenuItem.Destroy;
begin
  TUtils.ClearList(fChildren);
  fChildren.Free;
  inherited Destroy;
end;

end.

