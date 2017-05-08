unit qsFoundation;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides fundamental types and basic operations on them }

{ Version 1.0.0 }

{ (c) J. W. Dietrich, 1994 - 2017 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2017 }

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
  Classes, SysUtils, Math;

type

  TLogicvector_data = array of boolean;
  TIntvector_data = array of integer;
  TLongintvector_data = array of longint;
  TRealvector_data = array of real;
  TExtvector_data = array of extended;

  TLogicmatrix_data = array of Tlogicvector_data;
  TIntmatrix_data = array of Tintvector_data;
  TLongintmatrix_data = array of Tlongintvector_data;
  TRealmatrix_data = array of Trealvector_data;
  TExtmatrix_data = array of Textvector_data;

  { TLogicVector }

  TLogicVector = class
    data: TLogicVector_data;
    constructor InitFalse(length: longint);
    constructor InitTrue(length: longint);
    function getlength: longint;
  end;

  { TIntVector }

  TIntVector = class
    data: TIntVector_data;
    constructor InitZero(length: longint);
    constructor InitOne(length: longint);
    function getlength: longint;
  end;

function add(const vec1: TIntVector; const vec2: TIntVector): TIntVector;

operator + (const vec1: TIntVector; const vec2: TIntVector): TIntVector;

implementation

function add(const vec1: TIntVector; const vec2: TIntVector): TIntVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
    result.Create;
  if k = l then
  begin
    setlength(result.data, k);
    for i := 0 to k - 1 do
      result.data[i] := vec1.data[i] + vec2.data[i];
  end
  else if k < l then
  begin
    setlength(result.data, l);
    for i := 0 to k - 1 do
      result.data[i] := vec1.data[i] + vec2.data[i];
    for i := k to l do
      result.data[i] := vec2.data[i];
  end
  else
  begin
    setlength(result.data, k);
    for i := 0 to l - 1 do
      result.data[i] := vec1.data[i] + vec2.data[i];
    for i := l to k do
      result.data[i] := vec1.data[i];
  end;
end;

operator + (const vec1: TIntVector; const vec2: TIntVector): TIntVector;
begin
  result := add(vec1, vec2);
end;

{ TIntVector }

constructor TIntVector.InitZero(length: longint);
var
  i: longint;
begin
  SetLength(data, length);
  for i := 0 to length - 1 do
    data[i] := 0;
end;

constructor TIntVector.InitOne(length: longint);
var
  i: longint;
begin
  SetLength(data, length);
  for i := 0 to length - 1 do
    data[i] := 1;
end;

function TIntVector.getlength: longint;
begin
  result := length(data);
end;

{ TLogicVector }

constructor TLogicVector.InitFalse(length: longint);
var
  i: longint;
begin
  SetLength(data, length);
  for i := 0 to length - 1 do
    data[i] := false;
end;

constructor TLogicVector.InitTrue(length: longint);
var
  i: longint;
begin
  SetLength(data, length);
  for i := 0 to length - 1 do
    data[i] := true;
end;

function TLogicVector.getlength: longint;
begin
  result := length(data);
end;

end.

