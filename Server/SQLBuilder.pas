unit SQLBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Commons, DB, sqldb, IdCustomHTTPServer;

type
  TJoinType = (jtINNER, jtLEFT, jtRIGHT, jtOUTER);
  TWhereType = (wtNone, wtAND, wtOR);
  TOpType = (otNone, otEqual, otNotEqual, otLessThan, otGreaterThan, otLessThanOrEqual, otGreaterThanOrEqual, otLike, otIN);
  TLimit = record
    Start: Integer;
    Count: Integer;
  end;

  TSQLBuilder = class;

  { TSQLDB }

  TSQLDB = class(TInterfacedObject)
  private
    fParamValues: TStringList;
    procedure DoFillValues(SQL: TSQLQuery);
  protected
    fDbConnection: TSQLConnector;
    fOwner: TSQLBuilder;
    fParams: TStringList;
    function DoCreateParam(): string;
    function FetchData(ASQL: string): TDataSet;
    function Execute(ASQL: string): Integer;
    function DoGetSQL(): string; virtual; abstract;

    procedure DoClear(); virtual;
  public
    constructor Create(AOwner: TSQLBuilder; ADBConnection: TSQLConnector); virtual;
    destructor Destroy; override;

    procedure ParamValue(V: string);
  end;

  ISQLExec = interface
    ['{2274D954-42E7-41DD-A07B-896F112D44D3}']
    procedure Exec();
  end;

  ISQLLimit = interface
    ['{1E86C376-36DF-4730-A34D-2F6C5DBD1BC3}']
    procedure Limit(Start, Count: Integer);
  end;

  ISQLLimitEx = interface
    ['{04DCC868-CAB9-4321-BB9A-E8E9918C75B4}']
    procedure Limit(Count: Integer);
  end;

  { ISQLGroupBy }

  ISQLGroupBy = interface
    ['{19F66414-28C9-4A79-8C54-72C3E48F6B16}']
    procedure GroupBy(Fields: string);
  end;

  TSQL = class(TSQLDB)
  protected
    fTables: TStringList;
    fJoins: TStringList;
    fWheres: TStringList;
    fOrders: TStringList;
    fLimit: TLimit;
    fGroupBy: string;
    procedure DoClear(); override;
  protected
    fFields: TStringList;
  public
    constructor Create(AOwner: TSQLBuilder; ADBConnection: TSQLConnector); override;
    destructor Destroy; override;

    function LikeWildChar(): string;

    procedure Table(T: string);
    procedure Field(F: string);
    procedure Join(T, JoinField: string; JoinType: TJoinType = jtLeft);
    procedure LeftJoin(T, JoinField: string);
    procedure RightJoin(T, JoinField: string);
    procedure Where(F, Val: string; Op: TOpType = otEqual; WhereType: TWhereType = wtNone;
      IsBracketClose: Boolean = False);
    procedure Where(F: string; Op: TOpType = otEqual; WhereType: TWhereType = wtNone;
      IsBracketClose: Boolean = False);
    procedure OrderBy(F: string);
  end;

  { TSQLSelect }

  TSQLSelect = class(TSQL, ISQLLimit, ISQLLimitEx, ISQLGroupBy)
  private
  protected
    function DoGetSQL(): string; override;
    procedure DoClear(); override;
  public
    constructor Create(Owner: TSQLBuilder; DBConnection: TSQLConnector); override;
    destructor Destroy; override;

    function Get(): TDataSet;
    function GetSQL(): string;

    procedure Limit(Start, Count: Integer);
    procedure Limit(Count: Integer);
    procedure GroupBy(Fields: string);
  end;

  { TSQLInsert }

  TSQLInsert = class(TSQLDB, ISQLExec)
  private
    fTable: string;
    fFields: TStringList;
    fValues: TStringList;
  protected
    function DoGetSQL(): string; override;
    procedure DoClear(); override;
  public
    constructor Create(Owner: TSQLBuilder; DBConnection: TSQLConnector; AutoInsertCreatedBy: Boolean = False); reintroduce;
    destructor Destroy; override;

    procedure Table(T: string);
    procedure Field(F: string); overload;
    procedure Field(F: string; Val: string); overload;
    procedure Exec();
  end;

  { TSQLUpdate }

  TSQLUpdate = class(TSQL, ISQLExec, ISQLLimitEx)
  protected
    function DoGetSQL(): string; override;
    procedure DoClear(); override;
  public
    constructor Create(Owner: TSQLBuilder; DBConnection: TSQLConnector; AutoUpdateModifiedBy: Boolean = False); reintroduce;
    destructor Destroy; override;

    function GetSQL(): string;

    procedure Field(F: string);
    procedure Limit(Count: Integer);
    procedure Exec();
  end;

  { TSQLDelete }

  TSQLDelete = class(TSQL, ISQLExec, ISQLLimitEx)
  protected
    function DoGetSQL(): string; override;
  public
    procedure Exec();
    procedure Limit(Count: Integer);
  end;

    { TSQLBuilder }

  TSQLBuilder = class
  private
    fDelete: TSQLDelete;
    fOwner: TObject;
    fInsert: TSQLInsert;
    fSelect: TSQLSelect;
    fUpdate: TSQLUpdate;
  public
    constructor Create(Owner: TObject; DBConnection: TSQLConnector); reintroduce;
    destructor Destroy; override;
  published
    property Owner: TObject read fOwner;
    property Select: TSQLSelect read fSelect;
    property Insert: TSQLInsert read fInsert;
    property Update: TSQLUpdate read fUpdate;
    property Delete: TSQLDelete read fDelete;
  end;

implementation

uses DBConnection, Data, Utils, StrUtils;

{ TSQLDelete }

function TSQLDelete.DoGetSQL(): string;
var
  t, j, w, l: string;
begin
  Result := '';
  if (fTables.Count = 0) then
    Exit;

  t := Format('DELETE FROM %s', [TUtils.GetDelimitedText(fTables)]);

  if fJoins.Count > 0 then
    j := TUtils.GetDelimitedText(fJoins);

  if fWheres.Count > 0 then
    w := Format('WHERE %s', [TUtils.GetDelimitedText(fWheres)]);

  if fLimit.Count > 0 then
    l := Format('LIMIT %d', [fLimit.Count]);

  Result := Format('%s %s %s %s', [t, j, w, l]);
end;

procedure TSQLDelete.Exec();
begin
  try
  	Execute(DoGetSQL());
  finally
    DoClear();
  end;
end;

procedure TSQLDelete.Limit(Count: Integer);
begin
  fLimit.Start:= 0;
  fLimit.Count:= Count;
end;

{ TSQLUpdate }

function TSQLUpdate.DoGetSQL(): string;
var
  t, j, f, w, o, l: string;
begin
  Result := '';
  if (fTables.Count = 0) or (fFields.Count = 0) then
    Exit;

  t := Format('UPDATE %s', [TUtils.GetDelimitedText(fTables)]);

  if fJoins.Count > 0 then
    j := TUtils.GetDelimitedText(fJoins);

  f := Format('SET %s', [TUtils.GetDelimitedText(fFields)]);

  if fWheres.Count > 0 then
    w := Format('WHERE %s', [TUtils.GetDelimitedText(fWheres)]);

  if fOrders.Count > 0 then
    o := Format('ORDER BY %s', [TUtils.GetDelimitedText(fOrders)]);

  if fLimit.Count > 0 then
    l := Format('LIMIT %d', [fLimit.Count]);

  Result := Format('%s %s %s %s %s %s', [t, j, f, w, o, l]);
end;

procedure TSQLUpdate.DoClear();
begin
  inherited DoClear();
  fFields.Clear;
end;

constructor TSQLUpdate.Create(Owner:TSQLBuilder; DBConnection: TSQLConnector;
  AutoUpdateModifiedBy: Boolean = False);
var
  uid: string;
begin
  inherited Create(Owner, DBConnection);

  if AutoUpdateModifiedBy then
  begin
    Field('ModifiedDate=NOW()');

    uid := (fOwner.Owner as TSQLDBConnection).Owner.Session.Content.Values['UId'];
    if uid <> '' then
      Field('ModifiedBy=' + uid);
  end;
end;

destructor TSQLUpdate.Destroy;
begin
  inherited Destroy;
end;

function TSQLUpdate.GetSQL(): string;
begin
  Result := DoGetSQL();
end;

procedure TSQLUpdate.Field(F: string);
var
  param: string;
  p: Integer;
begin
  p := Pos(PARAM_REPLACEMENT_CHAR, F);
  if p > 0 then
  begin
    param := DoCreateParam();
    fParams.Add(param);

    F := AnsiReplaceText(F, PARAM_REPLACEMENT_CHAR, ':' + param);
  end;
  fFields.Add(F.Trim);
end;

procedure TSQLUpdate.Limit(Count: Integer);
begin
  fLimit.Start := 0;
  fLimit.Count := Count;
end;

procedure TSQLUpdate.Exec();
begin
  try
  	Execute(DoGetSQL());
  finally
    DoClear();
  end;
end;

{ TSQLInsert }

function TSQLInsert.DoGetSQL(): string;
var
  ins, f, v: string;
begin
  Result := '';
  if fParamValues.Count = 0 then
    Exit;

  ins := 'INSERT INTO ' + fTable;
  if fFields.Count > 0 then
    f := Format('(%s)', [TUtils.GetDelimitedText(fFields)]);

  v := Format('VALUES (%s)', [TUtils.GetDelimitedText(fValues)]);

  Result := Format('%s %s %s', [ins, f, v]);
end;

procedure TSQLInsert.DoClear();
begin
  fTable := '';
  fFields.Clear;
  fValues.Clear;
  inherited DoClear();
end;

constructor TSQLInsert.Create(Owner: TSQLBuilder; DBConnection: TSQLConnector; AutoInsertCreatedBy: Boolean = False);
var
  uid: string;
begin
  inherited Create(Owner, DBConnection);
  fFields := TStringList.Create;
  fFields.Delimiter := ',';

  fValues := TStringList.Create;
  fValues.Delimiter := ',';

  if not AutoInsertCreatedBy then
    Exit;

  uid := (fOwner.Owner as TSQLDBConnection).Owner.Session.Content.Values['UId'];
  if uid <> '' then
  begin
    Field('CreatedBy');
    ParamValue(uid);
  end;
end;

destructor TSQLInsert.Destroy;
begin
  fFields.Free;
  fValues.Free;
  inherited Destroy;
end;

procedure TSQLInsert.Table(T: string);
begin
  fTable := T.Trim;
end;

procedure TSQLInsert.Field(F: string);
var
  param: string;
begin
  param := DoCreateParam();
  fParams.Add(param);

  Field(F, ':' + param);
end;

procedure TSQLInsert.Field(F: string; Val: string);
var
  param: string;
  p: Integer;
begin
  fFields.Add(F.Trim);

  p := Pos(PARAM_REPLACEMENT_CHAR, Val);
  if p > 0 then
  begin
    param := DoCreateParam();
    fParams.Add(param);

    Val := AnsiReplaceText(Val, PARAM_REPLACEMENT_CHAR, ':' + param);
  end;

  fValues.Add(Val);
end;

procedure TSQLInsert.Exec();
begin
  try
    Execute(DoGetSQL());
  finally
    DoClear();
  end;
end;

{ TSQLSelect }

function TSQLSelect.DoGetSQL(): string;
var
  s, f, j, w, o, g, l: string;
begin
  Result:= '';

  if fFields.Count = 0 then
    Exit;

  s := 'SELECT ' + TUtils.GetDelimitedText(fFields);
  if fTables.Count > 0 then
  begin
    f := 'FROM ' + TUtils.GetDelimitedText(fTables);

    if fJoins.Count > 0 then
      j := TUtils.GetDelimitedText(fJoins);

    if fWheres.Count > 0 then
      w := 'WHERE ' + TUtils.GetDelimitedText(fWheres);

    if fGroupBy <> '' then
      g := 'GROUP BY ' + fGroupBy;

    if fOrders.Count > 0 then
      o := 'ORDER BY ' + TUtils.GetDelimitedText(fOrders);

    if fLimit.Count > 0 then
      l := Format('LIMIT %d, %d', [fLimit.Start, fLimit.Count]);
  end;

  Result := Format('%s %s %s %s %s %s %s', [s, f, j, w, g, o, l]);
end;

procedure TSQLSelect.DoClear();
begin
  inherited DoClear();
  fFields.Clear;
end;

constructor TSQLSelect.Create(Owner: TSQLBuilder; DBConnection: TSQLConnector);
begin
  inherited Create(Owner, DBConnection);
end;

destructor TSQLSelect.Destroy;
begin
  inherited Destroy;
end;

function TSQLSelect.Get(): TDataSet;
begin
  try
	  Result := FetchData(DoGetSQL());
  finally
    DoClear();
  end;
end;

function TSQLSelect.GetSQL(): string;
begin
  Result:= DoGetSQL();
end;

procedure TSQLSelect.Limit(Start, Count: Integer);
begin
  fLimit.Start := Start;
  fLimit.Count := Count;
end;

procedure TSQLSelect.Limit(Count: Integer);
begin
  Limit(0, Count);
end;

procedure TSQLSelect.GroupBy(Fields: string);
begin
  fGroupBy:= Fields;
end;

{ TSQL }

procedure TSQL.DoClear();
begin
  inherited;
  fTables.Clear;
  fJoins.Clear;
  fWheres.Clear;
  fOrders.Clear;
  fParams.Clear;
  fLimit.Start := 0;
  fLimit.Count := 0;
  fGroupBy:= '';
end;

constructor TSQL.Create(AOwner: TSQLBuilder; ADBConnection: TSQLConnector);
begin
  inherited Create(AOwner, ADBConnection);

  fTables := TStringList.Create;
  fTables.Delimiter := ',';

  fFields := TStringList.Create;
  fFields.Delimiter:= ',';

  fJoins := TStringList.Create;
  fJoins.Delimiter := #32;

  fWheres := TStringList.Create;
  fWheres.Delimiter := #32;

  fOrders := TStringList.Create;
  fOrders.Delimiter := ',';
end;

destructor TSQL.Destroy;
begin
  fTables.Free;
  fFields.Free;
  fJoins.Free;
  fWheres.Free;
  fOrders.Free;
  inherited Destroy;
end;

function TSQL.LikeWildChar(): string;
begin
  Result := DataApp.SQLDict.Values['LikeWildChar'];
end;

procedure TSQL.Table(T: string);
begin
  fTables.Add(Trim(T));
end;

procedure TSQL.Field(F: string);
var
  param: string;
  p: Integer;
begin
  p := Pos(PARAM_REPLACEMENT_CHAR, F);
  if p > 0 then
  begin
    param := DoCreateParam();
    fParams.Add(param);

    F := AnsiReplaceText(F, PARAM_REPLACEMENT_CHAR, ':' + param);
  end;

  fFields.Add(F.Trim);
end;

procedure TSQL.Join(T, JoinField: string; JoinType: TJoinType);
var
  j, jc, s: string;
begin
  case JoinType of
    jtINNER:
      j := DataApp.SQLDict.Values['InnerJoin'];
    jtLEFT:
      j := DataApp.SQLDict.Values['LeftJoin'];
    jtRIGHT:
      j := DataApp.SQLDict.Values['RightJoin'];
    jtOUTER:
      j := DataApp.SQLDict.Values['OuterJoin'];
  end;

  jc := DataApp.SQLDict.Values['JoinConditional'];
  s := Format('%s %s %s %s', [j, T, jc, JoinField]);
  fJoins.Add(Trim(s));
end;

procedure TSQL.LeftJoin(T, JoinField: string);
begin
  Join(T, JoinField);
end;

procedure TSQL.RightJoin(T, JoinField: string);
begin
  Join(T, JoinField, jtRIGHT);
end;

procedure TSQL.Where(F, Val: string; Op: TOpType; WhereType: TWhereType;
  IsBracketClose: Boolean);
var
  wt, o, v, s, b, param: string;
  p: Integer;
begin
  case WhereType of
    wtNone:
      wt := '';
    wtAND:
      wt := 'AND';
    wtOR:
      wt := 'OR';
  end;

  v := Val;
  if Op = otLike then
    v:= 'CONCAT("%", ' + Val + ', "%")';

  p := Pos(PARAM_REPLACEMENT_CHAR, Val);
  if p > 0 then
  begin
    param := DoCreateParam();
    fParams.Add(param);

    v := AnsiReplaceText(Val, PARAM_REPLACEMENT_CHAR, ':' + param);
  end;

  case Op of
    otNone:
      o := '';
    otEqual:
      o := DataApp.SQLDict.Values['OpEqual'];
    otNotEqual:
      o := DataApp.SQLDict.Values['OpNotEqual'];
    otLessThan:
      o := DataApp.SQLDict.Values['OpLessThan'];
    otGreaterThan:
      o := DataApp.SQLDict.Values['OpGreaterThan'];
    otLessThanOrEqual:
      o := DataApp.SQLDict.Values['OpLessThanOrEqual'];
    otGreaterThanOrEqual:
      o := DataApp.SQLDict.Values['OpGreaterThanOrEqual'];
    otLike:
      o := DataApp.SQLDict.Values['OpLike'];
  end;

  b := '';
  if IsBracketClose then
    b := ')';

  s := Format('%s %s %s %s%s', [wt, F, o, v, b]);
  fWheres.Add(Trim(s));
end;

procedure TSQL.Where(F: string; Op: TOpType; WhereType: TWhereType;
  IsBracketClose: Boolean);
var
  param: string;
begin
  param := DoCreateParam();
  fParams.Add(param);

  Where(F, ':'+param, Op, WhereType, IsBracketClose);
end;

procedure TSQL.OrderBy(F: string);
begin
  fOrders.Add(Trim(F));
end;

{ TSQLBuilder }

constructor TSQLBuilder.Create(Owner: TObject; DBConnection: TSQLConnector);
begin
  inherited Create;
  fOwner := Owner;
  fSelect := TSQLSelect.Create(Self, DBConnection);
  fInsert := TSQLInsert.Create(Self, DBConnection);
  fUpdate := TSQLUpdate.Create(Self, DBConnection);
  fDelete := TSQLDelete.Create(Self, DBConnection);
end;

destructor TSQLBuilder.Destroy;
begin
  fSelect.Free;
  fInsert.Free;
  fUpdate.Free;
  fDelete.Free;
  inherited Destroy;
end;

{ TSQLDB }

procedure TSQLDB.DoFillValues(SQL: TSQLQuery);
var
  i: Integer;
begin
  for i := 0 to SQL.Params.Count - 1 do
    SQL.Params.Items[i].Value := fParamValues[i];
end;

function TSQLDB.DoCreateParam(): string;
var
  i: Integer;
begin
  Result := TUtils.CreateID(SQL_PARAM_LENGTH);
  i := fParams.IndexOf(Result);
  while i >= 0 do
  begin
    Result := TUtils.CreateID(SQL_PARAM_LENGTH);
    i := fParams.IndexOf(Result);
  end;
end;

function TSQLDB.FetchData(ASQL: string): TDataSet;
begin
  Result := TSQLQuery.Create(nil);
  with TSQLQuery(Result) do
  begin
    SQLConnection := fDbConnection;
    SQL.Clear;
    SQL.Add(ASQL);

    DoFillValues(Result as TSQLQuery);

    Open;
  end;
end;

function TSQLDB.Execute(ASQL: string): Integer;
var
  qry: TSQLQuery;
begin
  qry := TSQLQuery.Create(nil);
  try
    qry.SQLConnection := fDbConnection;
    qry.SQL.Clear;
    qry.SQL.Add(ASQL);

    DoFillValues(qry);

    try
      qry.ExecSQL;
      Result := qry.RowsAffected;
    except
      raise;
    end;
  finally
    qry.Free;
  end;
end;

procedure TSQLDB.DoClear();
begin
  fParamValues.Clear;
end;

procedure TSQLDB.ParamValue(V: string);
begin
  fParamValues.Add(V);
end;

constructor TSQLDB.Create(AOwner: TSQLBuilder; ADBConnection: TSQLConnector);
begin
  fOwner := AOwner;
  fDbConnection := ADBConnection;

  fParamValues := TStringList.Create;
  fParamValues.Delimiter := ',';

  fParams := TStringList.Create;
  fParams.Delimiter := ',';
end;

destructor TSQLDB.Destroy;
begin
  fParamValues.Free;
  fParams.Free;
  inherited Destroy;
end;

end.


