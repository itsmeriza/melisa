unit Request;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectBase, Entry, MTypes, Utils, Connection,
  User, ObjectFactoryDB, rstomp, Command, HTTPServer;

const
  ERR_Success = 0;
  ERR_AccessRightViolation = -996;
  ERR_InvalidToken = -997;
  ERR_NeedLogin = -998;
  ERR_Unknown = -999;

type

  { TEntryResponseError }

  TEntryResponseError = class(TEntry)
  private
    fCode: Integer;
    fMsg: string;
  published
    property Code: Integer read fCode write fCode;
    property Msg: string read fMsg write fMsg;
  end;

  { TResponse }

  TResponse = class(TEntry)
  private
    fData: Classes.TList;
    fErrorCode: Integer;
    fMsg: string;
    fReplyMsg: string;
  public
    constructor Create();
    destructor Destroy; override;

    property ReplyMsg: string read fReplyMsg write fReplyMsg;
  published
    property ErrorCode: Integer read fErrorCode write fErrorCode;
    property Data: Classes.TList read fData write fData;
    property Msg: string read fMsg write fMsg;
  end;

  { TRequest }

  TRequest = class(TObjectBase)
  private
    fConnection: TConnection;
  protected
    fCommand: TCommand;
    fConnectionId: string;
    function DoIsUserHaveAccess(): Boolean; virtual;
    procedure DoPerform(var R: TResponse); virtual; abstract;
  public
    constructor Create(Command: TCommand); reintroduce; virtual;
    destructor Destroy; override;

    procedure Perform(); virtual; abstract;
  published
    property Connection: TConnection read fConnection;
  end;

  { TRequestHTTP }

  TRequestHTTP = class(TRequest)
  private
    fReqId: string;

    procedure DoRespond(ErrCode: Integer = ERR_Success; Msg: string = '');
    procedure DoRespond(R: TResponse);
    procedure DoRecordPerform(R: TResponse);
  protected
    fObjF: TObjectFactoryDB;
    fIsUseTransaction: Boolean;
    fIsNeedSignIn: Boolean;
    fCommandTypes: TCommandTypes;
    fIsRecorded: Boolean;

    function DoIsUserHaveAccess(): Boolean; override;
  public
    constructor Create(Command: TCommand); override;
    destructor Destroy; override;

    procedure Perform(); override;
  published
    property ReqId: string read fReqId;
    property IsUseTransaction: Boolean read fIsUseTransaction write fIsUseTransaction;
    property ObjF: TObjectFactoryDB read fObjF;
  end;

implementation

uses Data, UserAccess, IdGlobal, Commons;

{ TAbstractRequest }

function TRequest.DoIsUserHaveAccess: Boolean;
begin
  Result:= False;
end;

constructor TRequest.Create(Command: TCommand);
begin
  inherited Create();

  fCommand := Command;
  fConnection:= Command.ConnectionMgr.CreateConnection(fConnectionId);
  fConnection.Open();
end;

destructor TRequest.Destroy;
begin
  fConnection.IsUsed:= False;
  fConnection.LastUsed:= Now();
  inherited Destroy;
end;

{ TResponse }

constructor TResponse.Create();
begin
  fReplyMsg := '';
  fErrorCode := ERR_Success;
  fData := Classes.TList.Create;
end;

destructor TResponse.Destroy;
begin
  TUtils.ClearList(fData);
  fData.Free;

  inherited Destroy;
end;

{ TRequestHTTP }

function TRequestHTTP.DoIsUserHaveAccess(): Boolean;
var
  isUseOwnList: Boolean;
  ua: TUserAccessItem;
  i: Integer;
begin
  Result:= inherited DoIsUserHaveAccess();
  i := 0;
  isUseOwnList := StrToBoolDef(DataApp.Settings.Behaviour.Values['IsUseOwnUserAccessList'], True);
  if isUseOwnList then
    ua := THTTPServer(fCommand.Owner).UserAccess.Get(i, TCommandHTTP(fCommand).RequestInfo.Session)
  else
    ua := TUserAccessItem(PtrUInt(TCommandHTTP(fCommand).GetUserAccessAddr().ToInteger));

  i := 0;
  Result := ua.Get(i, fCommand.Route.ClassName) <> nil;
end;

procedure TRequestHTTP.DoRespond(ErrCode: Integer; Msg: string);
var
  r: TResponse;
begin
  r := TResponse.Create();
  r.ErrorCode := ErrCode;
  r.Msg:= Msg;
  try
    TCommandHTTP(fCommand).ResponseInfo.ContentText := r.ToJson();
  finally
    r.Free;
  end;
end;

procedure TRequestHTTP.DoRespond(R: TResponse);
begin
  TCommandHTTP(fCommand).ResponseInfo.ResponseNo := 200;
  TCommandHTTP(fCommand).ResponseInfo.ContentType:= 'text/plain';
  TCommandHTTP(fCommand).ResponseInfo.CharSet:= 'utf-8';
  TCommandHTTP(fCommand).ResponseInfo.ContentLength:= -1;

  if R.ReplyMsg = '' then
    TCommandHTTP(fCommand).ResponseInfo.ContentText := R.ToJson()
  else
    TCommandHTTP(fCommand).ResponseInfo.ContentText := R.ReplyMsg;
end;

procedure TRequestHTTP.DoRecordPerform(R: TResponse);
var
  objUser: TUser;
begin
  objUser:= TUser(fObjF.CreateObject(TUser.ClassName));
  objUser.RecordPerform(TCommandHTTP(fCommand).GetUId(), TCommandHTTP(fCommand).Cmd);
end;

constructor TRequestHTTP.Create(Command: TCommand);
begin
  fConnectionId:= CNX_1;
  inherited Create(Command);

  fReqId := TCommandHTTP(fCommand).Cmd;
  fIsUseTransaction := False;
  fIsNeedSignIn := True;
  fIsRecorded:= False;

  TConnectionDB(fConnection).Session:= TCommandHTTP(fCommand).RequestInfo.Session;

  fObjF := TObjectFactoryDB.Create;
  fObjF.Connection:= TConnectionDB(fConnection);

  fCommandTypes := [TCommandType.hcPOST];
  TCommandHTTP(fCommand).ResponseInfo.CustomHeaders.Values['Allow'] := 'POST';
end;

destructor TRequestHTTP.Destroy;
begin
  fObjF.Free;
  inherited Destroy;
end;

procedure TRequestHTTP.Perform();
var
  r: TResponse;
  token: string;
begin
  if not (TCommandHTTP(fCommand).RequestInfo.CommandType in fCommandTypes) then
  begin
    TCommandHTTP(fCommand).ResponseInfo.ResponseNo := 405;
    Exit;
  end;

  if fIsNeedSignIn and not (TCommandHTTP(fCommand).IsSignedIn()) then
  begin
    DoRespond(ERR_NeedLogin);
    Exit;
  end;

  if fIsNeedSignIn and not DoIsUserHaveAccess() then
  begin
    DoRespond(ERR_AccessRightViolation);
    Exit;
  end;

  token := TCommandHTTP(fCommand).GetPostToken();

  if fIsNeedSignIn and (TCommandType.hcPOST in fCommandTypes) and ((token = '') or (token <> TCommandHTTP(fCommand).GetToken())) then
  begin
    DoRespond(ERR_InvalidToken);
    Exit;
  end;

  r := TResponse.Create;

  try
    try
      DoPerform(r);

      if DataApp.AuditTrailEnabled and fIsNeedSignIn and fIsRecorded then
        DoRecordPerform(r);

      if fIsUseTransaction then
        TConnectionDB(fConnection).Commit();

      DoRespond(r);
    except
      on E: Exception do
      begin
        if fIsUseTransaction then
          TConnectionDB(fConnection).Rollback();

        DoRespond(ERR_Unknown, E.Message);
        raise
      end;
    end;
  finally
    r.Free;
  end;
end;

end.

