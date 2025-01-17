{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_DimRanges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Markers,
  ATSynEdit_LineParts;
  
type
  { TATDimRanges }

  TATDimRanges = class(TATMarkers)
  public
    constructor Create; override;
    procedure Add(ALineFrom, ALineTo: integer; ADimValue: integer);
    function GetDimValue(ALine, ADefValue: integer): integer;
  end;

implementation

constructor TATDimRanges.Create;
begin
  inherited Create;
  Sorted:= true;
end;

procedure TATDimRanges.Add(ALineFrom, ALineTo: integer; ADimValue: integer);
begin
  inherited Add(
    Point(0, ALineFrom),
    Point(0, ALineTo-ALineFrom+1),
    TATMarkerTags.Init(0, ADimValue)
    );
end;

function TATDimRanges.GetDimValue(ALine, ADefValue: integer): integer;
var
  NIndex: integer;
begin
  Result:= ADefValue;
  NIndex:= FindContaining(0, ALine);
  if IsIndexValid(NIndex) then
    Result:= Items[NIndex].TagEx;
end;


end.

