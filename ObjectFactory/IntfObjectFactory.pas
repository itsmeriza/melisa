unit IntfObjectFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectBase;

type
  IObjectFactory = interface
  ['{F00B117E-6A43-4699-B209-3C36E20BB29B}']
    function GetObject(ClassId: string): TObjectBase;
    function CreateObject(ClassId: string): TObjectBase;
  end;

implementation

end.

