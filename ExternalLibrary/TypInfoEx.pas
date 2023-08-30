unit TypInfoEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo;

resourcestring

  SErrCannotWriteToProperty     = 'Cannot write to property "%s".';
  SErrCannotReadProperty        = 'Cannot read property "%s".';

function GetDynArrayProp(Instance: TObject; const PropName: string): Pointer;
function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
procedure SetDynArrayProp(Instance: TObject; const PropName: string; const Value: Pointer);
procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

implementation

function GetDynArrayProp(Instance: TObject; const PropName: string): Pointer;
begin
  Result:=GetDynArrayProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
type
  { we need a dynamic array as that type is usually passed differently from
    a plain pointer }
  TDynArray=array of Byte;
  TGetDynArrayProc=function:TDynArray of object;
  TGetDynArrayProcIndex=function(index:longint):TDynArray of object;

var
  AMethod : TMethod;

begin
  Result:=nil;
  if PropInfo^.PropType^.Kind<>tkDynArray then
    Exit;
  case (PropInfo^.PropProcs) and 3 of
    ptField:
      Result:=PPointer(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Result:=Pointer(TGetDynArrayProcIndex(AMethod)(PropInfo^.Index))
        else
          Result:=Pointer(TGetDynArrayProc(AMethod)());
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
  end;
end;

procedure SetDynArrayProp(Instance: TObject; const PropName: string; const Value: Pointer);
begin
  SetDynArrayProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

type
  { we need a dynamic array as that type is usually passed differently from
    a plain pointer }
  TDynArray=array of Byte;
  TSetDynArrayProcIndex=procedure(index:longint;const i:TDynArray) of object;
  TSetDynArrayProc=procedure(i:TDynArray) of object;

var
  AMethod: TMethod;

begin
  if PropInfo^.PropType^.Kind<>tkDynArray then
    Exit;
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptField:
      CopyArray(PPointer(Pointer(Instance)+PtrUInt(PropInfo^.SetProc)), @Value, PropInfo^.PropType, 1);
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetDynArrayProcIndex(AMethod)(PropInfo^.Index,TDynArray(Value))
        else
          TSetDynArrayProc(AMethod)(TDynArray(Value));
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
  end;
end;

end.
