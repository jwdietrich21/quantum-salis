unit rDist;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides algorithms for statistical distributions }

{ Version 1.0.0 (Alpina) }

{ (c) J. W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://quantum-salis.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Math,
  qsFoundation;

type
  TGaussianMethod = (tgexact, tgpolynomial);

const
  NAN_ERROR = 'Parameter is not numeric';
  NEG_MEAN_ERROR = 'mean < 1';
  inv_sqrt_2pi = 1 / sqrt(2 * pi);

function randomExp(a, rate: real): real;
function randomGamma(a, b, c: real): real;
function randomChisq(df: integer): real;
function randomT(df: integer): real;
function randomF(v, w: integer): real;
function randomErlang(mean: real; k: integer): real;
function randomPoisson(mean: integer): integer;
function probGaussian(x, mean, stddev: extended): real;
function ProbT(t, df: extended): real;
function DensT(x, df: extended): real;
function AGaussian(x: extended; method: TGaussianMethod): real;
function AGaussian(x: extended): real;
function AGaussian(x, mean, stddev: extended; method: TGaussianMethod): real;
function AGaussian(x, mean, stddev: extended): real;

implementation

function randomExp(a, rate: real): real;
  { Generator for exponential distribution }
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
  { Generator for gamma distribution }
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
  { Generator for chi square distribution }
begin
  if df < 1 then
    randomChisq := NaN
  else
    randomChisq := randomGamma(0, 2, 0.5 * df);
end;

function randomT(df: integer): real;
  { Generator for Student's t distribution }
begin
  if df < 1 then
    randomT := NaN
  else
  begin
    randomT := randg(0, 1) / sqrt(randomChisq(df) / df);
  end;
end;

function randomF(v, w: integer): real;
  { Generator for F distribution }
begin
  if (v < 1) or (w < 1) then
    randomF := NaN
  else
    randomF := randomChisq(v) / v / (randomChisq(w) / w);
end;

function randomErlang(mean: real; k: integer): real;
  { Generator for Erlang distribution }
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

function randomPoisson(mean: integer): integer;
  { Generator for Poisson distribution (Donald Knuth's algorithm) }
const
  RESOLUTION = 1000;
var
  k: integer;
  b, l: real;
begin
  assert(mean > 0, NEG_MEAN_ERROR);
  k := 0;
  b := 1;
  l := exp(-mean);
  while b > l do
  begin
    k := k + 1;
    b := b * random(RESOLUTION) / RESOLUTION;
  end;
  randomPoisson := k - 1;
end;

function probGaussian(x, mean, stddev: extended): real;
  { Delivers the rank of x in the CDF of a Gaussian distribution }
  { Inspired by John D. Cook and by Abramowitz and Stegun }
const
  a1 = 0.254829592;
  a2 = -0.284496736;
  a3 = 1.421413741;
  a4 = -1.453152027;
  a5 = 1.061405429;
  p = 0.3275911;
var
  sign: integer;
  t, y: real;
begin
  assert(not IsNaN(x), NAN_ERROR);
  assert(not IsNaN(mean), NAN_ERROR);
  assert(not IsNaN(stddev), NAN_ERROR);
  x := (x - mean) / stddev;
  if x < 0 then
    sign := -1
  else
    sign := 1;
  x := abs(x) / (sqrt(2)); // calculates abs(erf(x))
  t := 1 / (1 + p * x);
  y := 1 - ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t * exp(-x * x);
  Result := 0.5 * (1 + sign * y);
end;

function ProbT(t, df: extended): real;
const
  tp = 2 / pi;
var
  a, b, y, z: real;
  j, s: integer;
begin
  z := 1;
  t := t * t;
  y := t / df;
  b := 1 + y;
  if df < 1 then
    Result := Nan
  else if (df >= 20) and (t < df) or (df > 200) then
  begin // asymptotic series for large or non-integer df
    if y > 1e-6 then
      y := ln(b);
    a := df - 0.5;
    b := 48 * a * a;
    y := a * y;
    y := (((((-0.4 * y - 3.3) * y - 24) * y - 85.5) / (0.8 * y * y + 100 + b) +
      y + 3) / b + 1) * sqrt(y);
    Result := 2 * AGaussian(-y);
  end
  else if (df < 20) and (t < 4) then
  begin // nested summation of "cosine" series
    a := sqrt(y);
    y := a;
    if df = 1 then
      a := 0;
  end
  else
  begin
    a := sqrt(b);
    y := a * df;
    j := 2;
    while a <> z do
    begin
      j := j + 2;
      z := a;
      y := y * (j - 1) / (b * j);
      a := a + y / (df + j);
    end;
    df := df + 2;
    z := 0;
    y := z;
    a := -a;
  end;
  s := 0;
  while s < 10000 do
  begin
    Inc(s);
    df := df - 2;
    if df > 1 then
    begin
      a := (df - 1) / (b * df) * a + y;
      continue;
    end;
    if df = 0 then
      a := a / sqrt(b)
    else
      a := (ArcTan(y) + a / b) * tp;
    Result := z - a;
  end;
end;

function DensT(x, df: extended): real;
  { Delivers the probability of x in the PDF of a t distribution }
begin
  Result := gamma((df + 1) / 2) / (sqrt(pi * df) * gamma(df / 2) *
    (1 + x ** 2 / df) ** ((df + 1) / 2));
end;

function AGaussian(x: extended; method: TGaussianMethod): real;
  { adapted from ACM algorithm 209 (Ibbetson 1963) }
var
  w, y, z: real;
begin
  case method of
    tgexact:
    begin
      Result := inv_sqrt_2pi * exp(-0.5 * x * x);
    end;
    tgpolynomial:
    begin
      if x = 0 then
        z := 0
      else
      begin
        y := abs(x) / 2;
        if y >= 3 then
          z := 1
        else if y < 1 then
        begin
          w := y * y;
          z := ((((((((0.000124818987 * w - 0.001075204047) *
            w + 0.005198775019) * w - 0.019198292004) * w + 0.059054035642) *
            w - 0.151968751364) * w + 0.319152932694) * w - 0.531923007300) *
            w + 0.797884560593) * y * 2;
        end
        else
        begin
          y := y - 2;
          z := (((((((((((((-0.000045255659 * y + 0.000152529290) *
            y - 0.000019538132) * y - 0.000676904986) * y + 0.001390604284) *
            y - 0.000794620820) * y - 0.002034254874) * y + 0.006549791214) *
            y - 0.010557625006) * y + 0.011630447319) * y - 0.009279453341) *
            y + 0.005353579108) * y - 0.002141268741) * y + 0.000535310849) *
            y + 0.999936657524;
        end;
      end;
      if x > 0 then
        Result := (z + 1) / 2
      else
        Result := (1 - z) / 2;
    end;
  end;
end;

function AGaussian(x: extended): real;
begin
  Result := AGaussian(x, tgexact);
end;

function AGaussian(x, mean, stddev: extended; method: TGaussianMethod): real;
begin
  case method of
    tgexact:
    begin
      x := (x - mean) / stddev;
      Result := inv_sqrt_2pi * exp(-0.5 * x * x);
    end;
    tgpolynomial:
    begin
      x := (x - mean) / stddev;
      Result := AGaussian(x, tgpolynomial);
    end;
  end;
end;

function AGaussian(x, mean, stddev: extended): real;
begin
  x := (x - mean) / stddev;
  Result := AGaussian(x);
end;

{ References:

1. Ibbetson D: Algorithm 209 - Gauss. Communications of the ACM 6(10)
   October 1963: 616. DOI 10.1145/367651.367664

2. Richard Saucier, Computer Generation of Statistical Distributions,
   ARL-TR-2168, US Army Research Laboratory, Aberdeen Proving Ground, MD,
   21005-5068, March 2000.

}

end.
