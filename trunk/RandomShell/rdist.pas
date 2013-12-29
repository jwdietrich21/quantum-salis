unit rDist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

function randomExp(rate: real): real;

implementation

function randomExp(rate: real): real;
const
  RESOLUTION = 1000;
var
  unif: real;
begin
  if rate = 0 then
    randomExp := NaN
  else
  begin
    repeat
      unif := random(RESOLUTION) / RESOLUTION;
    until unif <> 0;
    randomExp := -ln(unif / rate) / rate;
  end;
end;

end.

