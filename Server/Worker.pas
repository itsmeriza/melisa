unit Worker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TWorker }

  TWorker = class(TThread)
  protected
    fInterval: QWord;
    procedure Execute(); override;
    procedure DoTask(); virtual; abstract;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

implementation

{ TWorker }

procedure TWorker.Execute();
var
  tick: QWord;
begin
  tick:= TThread.GetTickCount64();
  while not Terminated do
  begin
    if (TThread.GetTickCount64() - tick) >= fInterval then
    begin
      DoTask();
      tick:= TThread.GetTickCount64();
    end;
    Sleep(1);
  end;
end;

constructor TWorker.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  // Default interval
  fInterval:= 60000;
end;

end.

