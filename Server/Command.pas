unit Command;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Router;

type

  { TCommand }

  TCommand = class
  protected
    fOwner: TObject;
    fRoute: TRoute;
  published
    property Owner: TObject read fOwner;
    property Route: TRoute read fRoute write fRoute;
  end;

implementation

end.

