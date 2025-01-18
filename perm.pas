unit perm;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides algorithms for samples and permutations }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, qsFoundation;

function sample(x: TLongintvector_data; replace: boolean): TLongintvector_data;
function sample(x: TLongintvector; replace: boolean): TLongintvector_data;
function sample(x: TLongintvector_data; frozen: longint; replace: boolean): TLongintvector_data;
function sample(x: TLongintvector; frozen: longint; replace: boolean): TLongintvector_data;
function sample(min, max: longint; replace: boolean): TLongintvector_data;

implementation

function sample(x: TLongintvector_data; replace: boolean): TLongintvector_data;
var
  i, j, num1, num2: longint;
  Vector1, Vector2: TLongintvector_data;
begin
  randomize;                        // initialise random number generator
  num1 := length(x);                // length of vector to be generated
  SetLength(Vector1, num1);         // vector for original (ordered) data
  SetLength(Vector2, num1);         // vector with permuted data
  if num1 > 0 then
  begin
    for i := 0 to num1 - 1 do
    begin
      Vector1[i] := x[i];           // initialise original vector
    end;
    num2 := num1;
    for i := 1 to num1 do
    begin
      j := random(num2);            // select random element of Vector1
      Vector2[i - 1] := Vector1[j]; // and assign to next element of Vector2
      if not replace then
      begin
        Delete(Vector1, j, 1);      // remove randomly selected element
        Dec(num2);                  // and adapt size
      end;
    end;
  end;
  result := Vector2;
end;

function sample(x: TLongintvector; replace: boolean): TLongintvector_data;
begin
  result := sample(x, replace);
end;

function sample(x: TLongintvector_data; frozen: longint; replace: boolean
  ): TLongintvector_data;
var
  i, j, k, n: longint;
  roamVector, permVector: TLongintVector_data;
  frozenVector: TLogicvector;
begin
  n := length(x);
  j := 0;
  k := 0;
  SetLength(roamVector, n);
  SetLength(result, n);
  frozenVector := TLogicvector.Create;
  frozenVector.InitFalse(n);
  for i := 0 to n - 1 do
  begin
    if x[i] <> frozen then
      begin
        roamVector[j] := x[i];
        inc(j)
      end
    else
      begin
        frozenVector[i] := true;
      end;
  end;
  SetLength(roamVector, j);
  permVector := sample(roamVector, replace);
  for i := 0 to n - 1 do
  if not frozenVector[i] then
    begin
      result[i] := permVector[k];
      inc(k);
    end;
  frozenVector.Destroy;
end;

function sample(x: TLongintvector; frozen: longint; replace: boolean
  ): TLongintvector_data;
begin
  result := sample(x, frozen, replace);
end;

function sample(min, max: longint; replace: boolean): TLongintvector_data;
var
  i, j, num1: longint;
  Vector1: TLongintvector;
  Vector2: TLongintvector_data;
begin
  num1 := 1 + max - min;            // length of vector to be generated
  Vector1 := TLongintvector.Create; // vector for original (ordered) data
  Vector1.InitZero(num1);
  SetLength(Vector2, num1);         // vector with permuted data
  if num1 > 0 then
  begin
    for i := 0 to num1 - 1 do
    begin
      Vector1[i] := min + i;   // initialise original vector
    end;
    Vector2 := sample(Vector1, replace);
  end;
  Vector1.Destroy;
  result := Vector2;
end;


end.

