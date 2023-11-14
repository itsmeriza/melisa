unit Command;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Router, Connection;

type

  { TCommand }

  TCommand = class
  private
    fConnectionMgr: TConnectionManager;
  protected
    fOwner: TObject;
    fRoute: TRoute;
    fCmd: string;
  published
    property Owner: TObject read fOwner;
    property Route: TRoute read fRoute write fRoute;
    property Cmd: string read fCmd write fCmd;
    property ConnectionMgr: TConnectionManager read fConnectionMgr write fConnectionMgr;
  end;

implementation

end.

