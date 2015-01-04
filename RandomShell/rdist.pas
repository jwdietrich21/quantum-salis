unit rDist;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Unit with algorithms for statistical distributions }

{ Version 1.0.0 }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://quantum-salis.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

function randomExp(a, rate: real): real;
function randomGamma(a, b, c: real): real;
function randomChisq(df: integer): real;
function randomF(v, w: integer): real;
function randomErlang(mean: real; k: integer): real;

implementation

function randomExp(a, rate: real): real;
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
    randomExp := a - rate * ln(unif);
  end;
end;

function randomGamma(a, b, c: real): real;
const
  RESOLUTION = 1000;
  T = 4.5;
  D = 1 + ln(T);
var
  unif: real;
  A2, B2, C2, Q, p, y: real;
  p1, p2, v, w, z: real;
  found: boolean;
begin
  A2 := 1 / sqrt(2 * c - 1);
  B2 := c - ln(4);
  Q := c + 1 / A2;
  C2 := 1 + c / exp(1);
  found := False;
  if c < 1 then
  begin
    repeat
      repeat
        unif := random(RESOLUTION) / RESOLUTION;
      until unif > 0;
      p := C2 * unif;
      if p > 1 then
      begin
        repeat
          unif := random(RESOLUTION) / RESOLUTION;
        until unif > 0;
        y := -ln((C2 - p) / c);
        if unif <= power(y, c - 1) then
        begin
          randomGamma := a + b * y;
          found := True;
        end;
      end
      else
      begin
        y := power(p, 1 / c);
        if unif <= exp(-y) then
        begin
          randomGamma := a + b * y;
          found := True;
        end;
      end;
    until found;
  end
  else if c = 1 then
    { Gamma distribution becomes exponential distribution, if c = 1 }
  begin
    randomGamma := randomExp(a, b);
  end
  else
  begin
    repeat
      repeat
        p1 := random(RESOLUTION) / RESOLUTION;
      until p1 > 0;
      repeat
        p2 := random(RESOLUTION) / RESOLUTION;
      until p2 > 0;
      v := A2 * ln(p1 / (1 - p1));
      y := c * exp(v);
      z := p1 * p1 * p2;
      w := B2 + Q * v - y;
      if (w + D - T * z >= 0) or (w >= ln(z)) then
      begin
        randomGamma := a + b * y;
        found := True;
      end;
    until found;
  end;
end;

function randomChisq(df: integer): real;
begin
  if df < 1 then randomChisq := NaN
  else
  randomChisq := randomGamma(0, 2, 0.5 * df);
end;

function randomF(v, w: integer): real;
begin
  if (v < 1) or (w < 1) then
    randomF := NaN
  else
  randomF := randomChisq(v) / v / (randomChisq(w) / w);
end;

function randomErlang(mean: real; k: integer): real;
const
  RESOLUTION = 1000;
var
  i: integer;
  unif, prod: real;
begin
  if (mean <= 0) or (k < 1) then
    randomErlang := NaN
  else
  begin
    prod := 1;
    for i := 1 to k do
    begin
      repeat
        unif := random(RESOLUTION) / RESOLUTION;
      until unif <> 0;
      prod := prod * unif;
    end;
    randomErlang := -mean * ln(prod);
  end;
end;

{ Reference:
Richard Saucier, Computer Generation of Statistical Distributions,
ARL-TR-2168, US Army Research Laboratory, Aberdeen Proving Ground, MD,
21005-5068, March 2000.
}

end.
