unit HandleDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Handle, Connection, ObjectFactory,
  IdCustomHTTPServer;

type

  { THandleDB }

  THandleDB = class(THandle)
  protected
    fConnection: TConnectionDB;
    fSession: TIdHTTPSession;
    fLimitationClause: string;
    fWpId: string;
    function GetContent(Key: string): string;
    procedure DoInitLanguage(); override;
  public
    constructor Create(ObjF: TObjectFactory; Connection: TConnectionDB); reintroduce; virtual;
    destructor Destroy; override;

    property Connection: TConnectionDB read fConnection;
  end;

implementation

{ IHandleDB }

function THandleDB.GetContent(Key: string): string;
begin
  Result:= fSession.Content.Values[Key];
end;

procedure THandleDB.DoInitLanguage();
begin

end;

constructor THandleDB.Create(ObjF: TObjectFactory; Connection: TConnectionDB);
begin
  inherited Create(ObjF, nil, nil);
  fConnection := Connection;
  fSession := fConnection.Session;
  fWpId:= GetContent('WpId');
end;

destructor THandleDB.Destroy;
begin

  inherited Destroy;
end;

end.


