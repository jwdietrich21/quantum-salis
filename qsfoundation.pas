unit qsFoundation;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides fundamental types and basic operations on them }

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
  Classes, SysUtils, Math;

const

  QS_major   = 1;
  QS_minor   = 0;
  QS_release = 0;
  QS_patch = 122;
  QS_fullversion = ((QS_major * 100 + QS_minor) *
    100 + QS_release) * 100 + QS_patch;
  QS_version = '1.0.0.0';
  QS_internalversion = 'Alpina';

  MATRIX_ERROR1  = 'Incompatible matrix size';
  MATRIX_ERROR2  = 'Matrix and vector sizes incompatible';

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

  TMatrixLength = packed record
    rows, columns: longint;
  end;

  { TLogicVector }

  TLogicVector = class
    private
      Fdata: TLogicVector_data;
      function GetItem(i: longint): boolean;
      procedure SetItem(i: longint; data: boolean);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitFalse(length: longint);
      procedure InitTrue(length: longint);
      function getlength: longint;
      property data: TLogicVector_data read Fdata write Fdata;
      property items[i: longint]: boolean read GetItem write SetItem; default;
  end;

  { TIntVector }

  TIntVector = class
    private
      Fdata: TIntVector_data;
      function GetItem(i: longint): integer;
      procedure SetItem(i: longint; data: integer);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitZero(length: longint);
      procedure InitOne(length: longint);
      function getlength: longint;
      property data: TIntVector_data read Fdata write Fdata;
      property items[i: longint]: integer read GetItem write SetItem; default;
  end;

  { TLongintVector }

  TLongintVector = class
    private
      Fdata: TLongintvector_data;
      function GetItem(i: longint): longint;
      procedure SetItem(i: longint; data: longint);
    public
    constructor Create;
      destructor Destroy; override;
      procedure InitZero(length: longint);
      procedure InitOne(length: longint);
      function getlength: longint;
      property data: TLongintvector_data read Fdata write Fdata;
      property items[i: longint]: longint read GetItem write SetItem; default;
  end;

  { TRealVector }

  TRealVector = class
    private
      Fdata: TRealvector_data;
      function GetItem(i: longint): real;
      procedure SetItem(i: longint; data: real);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitZero(length: longint);
      procedure InitOne(length: longint);
      function getlength: longint;
      property data: TRealvector_data read Fdata write Fdata;
      property items[i: longint]: real read GetItem write SetItem; default;
  end;

  { TExtVector }

  TExtVector = class
    private
      Fdata: TExtvector_data;
      function GetItem(i: longint): extended;
      procedure SetItem(i: longint; data: extended);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitZero(length: longint);
      procedure InitOne(length: longint);
      function getlength: longint;
      property data: TExtvector_data read Fdata write Fdata;
      property items[i: longint]: extended read GetItem write SetItem; default;
  end;

  { TLogicMatrix }

  TLogicMatrix = class
    private
      Fdata: TLogicmatrix_data;
      function GetItem(i, j: longint): boolean;
      procedure SetItem(i, j: longint; data: boolean);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitFalse(rows, columns: longint);
      procedure InitTrue(rows, columns: longint);
      function getlength: TMatrixLength;
      property data: TLogicmatrix_data read Fdata write Fdata;
      property items[i, j: longint]: boolean read GetItem write SetItem; default;
  end;

  { TIntMatrix }

  TIntMatrix = class
    private
      Fdata: TIntmatrix_data;
      function GetItem(i, j: longint): integer;
      procedure SetItem(i, j: longint; data: integer);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitZero(rows, columns: longint);
      procedure InitOne(rows, columns: longint);
      function getlength: TMatrixLength;
      property data: TIntmatrix_data read Fdata write Fdata;
      property items[i, j: longint]: integer read GetItem write SetItem; default;
  end;

  { TLongintMatrix }

  TLongintMatrix = class
    private
      Fdata: TLongintMatrix_data;
      function GetItem(i, j: longint): integer;
      procedure SetItem(i, j: longint; data: longint);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitZero(rows, columns: longint);
      procedure InitOne(rows, columns: longint);
      function getlength: TMatrixLength;
      property data: TLongintMatrix_data read Fdata write Fdata;
      property items[i, j: longint]: longint read GetItem write SetItem; default;
  end;

  { TRealMatrix }

  TRealMatrix = class
    private
      Fdata: TRealMatrix_data;
      function GetItem(i, j: longint): real;
      procedure SetItem(i, j: longint; data: real);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitZero(rows, columns: longint);
      procedure InitOne(rows, columns: longint);
      function getlength: TMatrixLength;
      property data: TRealMatrix_data read Fdata write Fdata;
      property items[i, j: longint]: real read GetItem write SetItem; default;
  end;

  { TExtMatrix }

  TExtMatrix = class
    private
      Fdata: TExtMatrix_data;
      function GetItem(i, j: longint): extended;
      procedure SetItem(i, j: longint; data: extended);
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitZero(rows, columns: longint);
      procedure InitOne(rows, columns: longint);
      function getlength: TMatrixLength;
      property data: TExtMatrix_data read Fdata write Fdata;
      property items[i, j: longint]: extended read GetItem write SetItem; default;
  end;

function add(const vec1: TIntVector; const vec2: TIntVector): TIntVector;
function add(const vec1: TLongintVector; const vec2: TLongintVector): TLongintVector;
function add(const vec1: TRealVector; const vec2: TRealVector): TRealVector;
function add(const vec1: TExtVector; const vec2: TExtVector): TExtVector;
function sub(const vec1: TIntVector; const vec2: TIntVector): TIntVector;
function sub(const vec1: TLongintVector; const vec2: TLongintVector): TLongintVector;
function sub(const vec1: TRealVector; const vec2: TRealVector): TRealVector;
function sub(const vec1: TExtVector; const vec2: TExtVector): TExtVector;
function stimes(const scalar: integer; const vec: TIntVector): TIntVector;
function stimes(const scalar: longint; const vec: TLongintVector): TLongintVector;
function stimes(const scalar: real; const vec: TRealVector): TRealVector;
function stimes(const scalar: extended; const vec: TExtVector): TExtVector;
function stimes(const vec: TIntVector; const scalar: integer): TIntVector;
function stimes(const vec: TLongintVector; const scalar: longint): TLongintVector;
function stimes(const vec: TRealVector; const scalar: real): TRealVector;
function stimes(const vec: TExtVector; const scalar: extended): TExtVector;
function stimes(const vec1: TIntVector; const vec2: TIntVector): integer;
function stimes(const vec1: TLongintVector; const vec2: TLongintVector): longint;
function stimes(const vec1: TRealVector; const vec2: TRealVector): real;
function stimes(const vec1: TExtVector; const vec2: TExtVector): extended;
function abs(const vec: TIntVector): real; overload;
function abs(const vec: TLongintVector): real; overload;
function abs(const vec: TRealVector): real; overload;
function abs(const vec: TExtVector): extended; overload;

function trans(const mat: TIntMatrix): TIntMatrix;
function trans(const mat: TLongintMatrix): TLongintMatrix;
function trans(const mat: TRealMatrix): TRealMatrix;
function trans(const mat: TExtMatrix): TExtMatrix;
function add(const mat1, mat2: TIntMatrix): TIntMatrix;
function add(const mat1, mat2: TLongintMatrix): TLongintMatrix;
function add(const mat1, mat2: TRealMatrix): TRealMatrix;
function add(const mat1, mat2: TExtMatrix): TExtMatrix;
function sub(const mat1, mat2: TIntMatrix): TIntMatrix;
function sub(const mat1, mat2: TLongintMatrix): TLongintMatrix;
function sub(const mat1, mat2: TRealMatrix): TRealMatrix;
function sub(const mat1, mat2: TExtMatrix): TExtMatrix;
function stimes(const scalar: integer; const mat: TIntMatrix): TIntMatrix;
function stimes(const scalar: longint; const mat: TLongintMatrix): TLongintMatrix;
function stimes(const scalar: real; const mat: TRealMatrix): TRealMatrix;
function stimes(const scalar: extended; const mat: TExtMatrix): TExtMatrix;
function vtimes(const mat: tIntMatrix; const vec: TIntVector): TIntVector;
function vtimes(const mat: TLongintMatrix; const vec: TLongintVector): TLongintVector;
function vtimes(const mat: TRealMatrix; const vec: TRealVector): TRealVector;
function vtimes(const mat: TExtMatrix; const vec: TExtVector): TExtVector;
function mtimes(const mat1, mat2: tIntMatrix): tIntMatrix;
function mtimes(const mat1, mat2: TLongintMatrix): TLongintMatrix;
function mtimes(const mat1, mat2: TRealMatrix): TRealMatrix;
function mtimes(const mat1, mat2: TExtMatrix): TExtMatrix;

function diag(const mat: TIntMatrix): TIntVector; // delivers main diagonal
function diag(const mat: TLongintMatrix): TLongintVector;
function diag(const mat: TRealMatrix): TRealVector;
function diag(const mat: TExtMatrix): TExtVector;
function diag(const vec: TIntVector): TIntMatrix; // new matrix with diagonal
function diag(const vec: TLongintVector): TLongintMatrix;
function diag(const vec: TRealVector): TRealMatrix;
function diag(const vec: TExtVector): TExtMatrix;
function diag(const vec: TIntVector; rows, columns: longint): TIntMatrix;
function diag(const vec: TLongintVector; rows, columns: longint): TLongintMatrix;
function diag(const vec: TRealVector; rows, columns: longint): TRealMatrix;
function diag(const vec: TExtVector; rows, columns: longint): TExtMatrix;
function diag(length: longint): TIntMatrix; // creates identity matrix
function diag(length: longint): TLongintMatrix;
function diag(length: longint): TRealMatrix;
function diag(length: longint): TExtMatrix;

function trace(const mat: TIntMatrix): integer;
function trace(const mat: TLongintMatrix): longint;
function trace(const mat: TRealMatrix): real;
function trace(const mat: TExtMatrix): extended;

function det(const mat: TIntMatrix): integer;
function det(const mat: TLongintMatrix): longint;
function det(const mat: TRealMatrix): real;
function det(const mat: TExtMatrix): extended;

function submatrix(const mat: TIntMatrix; const row, column: longint): TIntMatrix;
function submatrix(const mat: TIntMatrix; const row, column: TLongintVector): TIntMatrix;

function lngamma(x: extended): extended;
function gamma(x: extended): extended;

operator + (const vec1: TIntVector; const vec2: TIntVector): TIntVector;
operator + (const vec1: TLongintVector; const vec2: TLongintVector): TLongintVector;
operator + (const vec1: TRealVector; const vec2: TRealVector): TRealVector;
operator + (const vec1: TExtVector; const vec2: TExtVector): TExtVector;
operator - (const vec1: TIntVector; const vec2: TIntVector): TIntVector;
operator - (const vec1: TLongintVector; const vec2: TLongintVector): TLongintVector;
operator - (const vec1: TRealVector; const vec2: TRealVector): TRealVector;
operator - (const vec1: TExtVector; const vec2: TExtVector): TExtVector;
operator * (const scalar: integer; const vec: TIntVector): TIntVector;
operator * (const scalar: longint; const vec: TLongintVector): TLongintVector;
operator * (const scalar: real; const vec: TRealVector): TRealVector;
operator * (const scalar: extended; const vec: TExtVector): TExtVector;
operator * (const vec: TIntVector; const scalar: integer): TIntVector;
operator * (const vec: TLongintVector; const scalar: longint): TLongintVector;
operator * (const vec: TRealVector; const scalar: real): TRealVector;
operator * (const vec: TExtVector; const scalar: extended): TExtVector;
operator * (const vec1: TIntVector; const vec2: TIntVector): integer;
operator * (const vec1: TLongintVector; const vec2: TLongintVector): longint;
operator * (const vec1: TRealVector; const vec2: TRealVector): real;
operator * (const vec1: TExtVector; const vec2: TExtVector): extended;

operator + (const mat1, mat2: TIntMatrix): TIntMatrix;
operator + (const mat1, mat2: TLongintMatrix): TLongintMatrix;
operator + (const mat1, mat2: TRealMatrix): TRealMatrix;
operator + (const mat1, mat2: TExtMatrix): TExtMatrix;
operator - (const mat1, mat2: TIntMatrix): TIntMatrix;
operator - (const mat1, mat2: TLongintMatrix): TLongintMatrix;
operator - (const mat1, mat2: TRealMatrix): TRealMatrix;
operator - (const mat1, mat2: TExtMatrix): TExtMatrix;
operator * (const scalar: integer; const mat: TIntMatrix): TIntMatrix;
operator * (const scalar: longint; const mat: TLongintMatrix): TLongintMatrix;
operator * (const scalar: real; const mat: TRealMatrix): TRealMatrix;
operator * (const scalar: extended; const mat: TExtMatrix): TExtMatrix;
operator * (const mat: TIntMatrix; const scalar: integer): TIntMatrix;
operator * (const mat: TLongintMatrix; const scalar: longint): TLongintMatrix;
operator * (const mat: TRealMatrix; const scalar: real): TRealMatrix;
operator * (const mat: TExtMatrix; const scalar: extended): TExtMatrix;
operator * (const mat: tIntMatrix; const vec: TIntVector): TIntVector;
operator * (const mat: TLongintMatrix; const vec: TLongintVector): TLongintVector;
operator * (const mat: TRealMatrix; const vec: TRealVector): TRealVector;
operator * (const mat: TExtMatrix; const vec: TExtVector): TExtVector;
operator * (const mat1, mat2: tIntMatrix): tIntMatrix;
operator * (const mat1, mat2: TLongintMatrix): TLongintMatrix;
operator * (const mat1, mat2: TRealMatrix): TRealMatrix;
operator * (const mat1, mat2: TExtMatrix): TExtMatrix;

implementation

function add(const vec1: TIntVector; const vec2: TIntVector): TIntVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TIntVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] + vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function add(const vec1: TLongintVector; const vec2: TLongintVector
  ): TLongintVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TLongintVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] + vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function add(const vec1: TRealVector; const vec2: TRealVector): TRealVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TRealVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] + vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function add(const vec1: TExtVector; const vec2: TExtVector): TExtVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TExtVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] + vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function sub(const vec1: TIntVector; const vec2: TIntVector): TIntVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TIntVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] - vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function sub(const vec1: TLongintVector; const vec2: TLongintVector
  ): TLongintVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TLongintVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] - vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function sub(const vec1: TRealVector; const vec2: TRealVector): TRealVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TRealVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] - vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function sub(const vec1: TExtVector; const vec2: TExtVector): TExtVector;
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := TExtVector.Create;
    if k = l then
    begin
      setlength(result.Fdata, k);
      for i := 0 to k - 1 do
        result.data[i] := vec1.data[i] - vec2.data[i];
    end
    else
      result := nil;
  end
  else
    result := nil;
end;

function stimes(const scalar: integer; const vec: TIntVector): TIntVector;
{ product of scalar with vector }
var
  i, k: longint;
begin
  k := vec.getlength;
  if k > 0 then
  begin
    result := TIntVector.Create;
    setlength(result.Fdata, k);
    for i := 0 to k - 1 do
      result.data[i] := scalar * vec.data[i];
  end
  else
    result := nil;
end;

function stimes(const scalar: longint; const vec: TLongintVector
  ): TLongintVector;
{ product of scalar with vector }
var
  i, k: longint;
begin
  k := vec.getlength;
  if k > 0 then
  begin
    result := TLongintVector.Create;
    setlength(result.Fdata, k);
    for i := 0 to k - 1 do
      result.data[i] := scalar * vec.data[i];
  end
  else
    result := nil;
end;

function stimes(const scalar: real; const vec: TRealVector): TRealVector;
{ product of scalar with vector }
var
  i, k: longint;
begin
  k := vec.getlength;
  if k > 0 then
  begin
    result := TRealVector.Create;
    setlength(result.Fdata, k);
    for i := 0 to k - 1 do
      result.data[i] := scalar * vec.data[i];
  end
  else
    result := nil;
end;

function stimes(const scalar: extended; const vec: TExtVector): TExtVector;
{ product of scalar with vector }
var
  i, k: longint;
begin
  k := vec.getlength;
  if k > 0 then
  begin
    result := TExtVector.Create;
    setlength(result.Fdata, k);
    for i := 0 to k - 1 do
      result.data[i] := scalar * vec.data[i];
  end
  else
    result := nil;
end;

function stimes(const vec: TIntVector; const scalar: integer): TIntVector;
{ product of vector with scalar }
begin
  result := stimes(scalar, vec);
end;

function stimes(const vec: TLongintVector; const scalar: longint
  ): TLongintVector;
{ product of vector with scalar }
begin
  result := stimes(scalar, vec);
end;

function stimes(const vec: TRealVector; const scalar: real): TRealVector;
{ product of vector with scalar }
begin
  result := stimes(scalar, vec);
end;

function stimes(const vec: TExtVector; const scalar: extended): TExtVector;
{ product of vector with scalar }
begin
  result := stimes(scalar, vec);
end;

function stimes(const vec1: TIntVector; const vec2: TIntVector): integer;
{ scalar product }
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := 0;
    if k = l then
    begin
      for i := 0 to k - 1 do
        result := result + vec1.data[i] * vec2.data[i];
    end
    else
      result := 0;
  end
  else
    result := 0;
end;

function stimes(const vec1: TLongintVector; const vec2: TLongintVector
  ): longint;
{ scalar product }
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := 0;
    if k = l then
    begin
      for i := 0 to k - 1 do
        result := result + vec1.data[i] * vec2.data[i];
    end
    else
      result := 0;
  end
  else
    result := 0;
end;

function stimes(const vec1: TRealVector; const vec2: TRealVector): real;
{ scalar product }
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := 0;
    if k = l then
    begin
      for i := 0 to k - 1 do
        result := result + vec1.data[i] * vec2.data[i];
    end
    else
      result := NaN;
  end
  else
    result := NaN;
end;

function stimes(const vec1: TExtVector; const vec2: TExtVector): extended;
{ scalar product }
var
  i, k, l: longint;
begin
  k := vec1.getlength;
  l := vec2.getlength;
  if (k > 0) or (l > 0) then
  begin
    result := 0;
    if k = l then
    begin
      for i := 0 to k - 1 do
        result := result + vec1.data[i] * vec2.data[i];
    end
    else
      result := NaN;
  end
  else
    result := NaN;
end;

function abs(const vec: TIntVector): real; overload;
var
  i, k: longint;
  sum: real;
begin
  k := vec.getlength;
  sum := 0;
  if k > 0 then
  for i := 0 to k - 1 do
    sum := sum + sqr(vec.data[i]);
  result := sqrt(sum);
end;

function abs(const vec: TLongintVector): real; overload;
var
  i, k: longint;
  sum: real;
begin
  k := vec.getlength;
  sum := 0;
  if k > 0 then
  for i := 0 to k - 1 do
    sum := sum + sqr(vec.data[i]);
  result := sqrt(sum);
end;

function abs(const vec: TRealVector): real; overload;
var
  i, k: longint;
  sum: real;
begin
  k := vec.getlength;
  sum := 0;
  if k > 0 then
  begin
  for i := 0 to k - 1 do
    sum := sum + sqr(vec.data[i]);
  end
  else
    sum := NaN;
  result := sqrt(sum);
end;

function abs(const vec: TExtVector): extended; overload;
var
  i, k: longint;
  sum: real;
begin
  k := vec.getlength;
  sum := 0;
  if k > 0 then
  begin
  for i := 0 to k - 1 do
    sum := sum + sqr(vec.data[i]);
  end
  else
    sum := NaN;
  result := sqrt(sum);
end;

function trans(const mat: TIntMatrix): TIntMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat2: TIntMatrix;
begin
  r1 := mat.getlength.rows;
  c1 := mat.getlength.columns;
  r2 := c1;
  c2 := r1;
  mat2 := TIntMatrix.Create;
  mat2.InitZero(r2, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat2.data[j, i] := mat.data[i, j];
      end;
  result := mat2;
end;

function trans(const mat: TLongintMatrix): TLongintMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat2: TLongintMatrix;
begin
  r1 := mat.getlength.rows;
  c1 := mat.getlength.columns;
  r2 := c1;
  c2 := r1;
  mat2 := TLongintMatrix.Create;
  mat2.InitZero(r2, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat2.data[j, i] := mat.data[i, j];
      end;
  result := mat2;
end;

function trans(const mat: TRealMatrix): TRealMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat2: TRealMatrix;
begin
  r1 := mat.getlength.rows;
  c1 := mat.getlength.columns;
  r2 := c1;
  c2 := r1;
  mat2 := TRealMatrix.Create;
  mat2.InitZero(r2, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat2.data[j, i] := mat.data[i, j];
      end;
  result := mat2;
end;

function trans(const mat: TExtMatrix): TExtMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat2: TExtMatrix;
begin
  r1 := mat.getlength.rows;
  c1 := mat.getlength.columns;
  r2 := c1;
  c2 := r1;
  mat2 := TExtMatrix.Create;
  mat2.InitZero(r2, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat2.data[j, i] := mat.data[i, j];
      end;
  result := mat2;
end;

function add(const mat1, mat2: TIntMatrix): TIntMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TIntMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TIntMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] + mat2.data[i, j];
      end;
  result := mat3;
end;

function add(const mat1, mat2: TLongintMatrix): TLongintMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TLongintMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TLongintMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] + mat2.data[i, j];
      end;
  result := mat3;
end;

function add(const mat1, mat2: TRealMatrix): TRealMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TRealMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TRealMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] + mat2.data[i, j];
      end;
  result := mat3;
end;

function add(const mat1, mat2: TExtMatrix): TExtMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TExtMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TExtMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] + mat2.data[i, j];
      end;
  result := mat3;
end;

function sub(const mat1, mat2: TIntMatrix): TIntMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TIntMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TIntMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] - mat2.data[i, j];
      end;
  result := mat3;
end;

function sub(const mat1, mat2: TLongintMatrix): TLongintMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TLongintMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TLongintMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] - mat2.data[i, j];
      end;
  result := mat3;
end;

function sub(const mat1, mat2: TRealMatrix): TRealMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TRealMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TRealMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] - mat2.data[i, j];
      end;
  result := mat3;
end;

function sub(const mat1, mat2: TExtMatrix): TExtMatrix;
var
  r1, c1, r2, c2: longint;
  i, j: longint;
  mat3: TExtMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TExtMatrix.Create;
  assert(r1 = r2, MATRIX_ERROR1);
  assert(c1 = c2, MATRIX_ERROR1);
  mat3.InitZero(r1, c1);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      begin
        mat3.data[i, j] := mat1.data[i, j] - mat2.data[i, j];
      end;
  result := mat3;
end;

function stimes(const scalar: integer; const mat: TIntMatrix): TIntMatrix;
var
  r, c: longint;
  i, j: longint;
  mat2: TIntMatrix;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  mat2 := TIntMatrix.Create;
  mat2.InitZero(r, c);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      begin
        mat2.data[i, j] := scalar * mat.data[i, j];
      end;
  result := mat2;
end;

function stimes(const scalar: longint; const mat: TLongintMatrix
  ): TLongintMatrix;
var
  r, c: longint;
  i, j: longint;
  mat2: TLongintMatrix;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  mat2 := TLongintMatrix.Create;
  mat2.InitZero(r, c);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      begin
        mat2.data[i, j] := scalar * mat.data[i, j];
      end;
  result := mat2;
end;

function stimes(const scalar: real; const mat: TRealMatrix): TRealMatrix;
var
  r, c: longint;
  i, j: longint;
  mat2: TRealMatrix;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  mat2 := TRealMatrix.Create;
  mat2.InitZero(r, c);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      begin
        mat2.data[i, j] := scalar * mat.data[i, j];
      end;
  result := mat2;
end;

function stimes(const scalar: extended; const mat: TExtMatrix): TExtMatrix;
var
  r, c: longint;
  i, j: longint;
  mat2: TExtMatrix;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  mat2 := TExtMatrix.Create;
  mat2.InitZero(r, c);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      begin
        mat2.data[i, j] := scalar * mat.data[i, j];
      end;
  result := mat2;
end;

function vtimes(const mat: tIntMatrix; const vec: TIntVector): TIntVector;
var
  r, c, l: longint;
  i, j: longint;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  l := vec.getlength;
  assert(c = l, MATRIX_ERROR2);
  result := TIntVector.Create;
  result.InitZero(r);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      result.data[i] := result.data[i] + mat.data[i, j] * vec.data[j];
end;

function vtimes(const mat: TLongintMatrix; const vec: TLongintVector
  ): TLongintVector;
var
  r, c, l: longint;
  i, j: longint;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  l := vec.getlength;
  assert(c = l, MATRIX_ERROR2);
  result := TLongintVector.Create;
  result.InitZero(r);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      result.data[i] := result.data[i] + mat.data[i, j] * vec.data[j];
end;

function vtimes(const mat: TRealMatrix; const vec: TRealVector): TRealVector;
var
  r, c, l: longint;
  i, j: longint;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  l := vec.getlength;
  assert(c = l, MATRIX_ERROR2);
  result := TRealVector.Create;
  result.InitZero(r);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      result.data[i] := result.data[i] + mat.data[i, j] * vec.data[j];
end;

function vtimes(const mat: TExtMatrix; const vec: TExtVector): TExtVector;
var
  r, c, l: longint;
  i, j: longint;
begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  l := vec.getlength;
  assert(c = l, MATRIX_ERROR2);
  result := TExtVector.Create;
  result.InitZero(r);
  for i := 0 to r - 1 do
    for j := 0 to c - 1 do
      result.data[i] := result.data[i] + mat.data[i, j] * vec.data[j];
end;

function mtimes(const mat1, mat2: tIntMatrix): tIntMatrix;
var
  r1, r2, c1, c2: longint;
  i, j, k: longint;
  mat3: TIntMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TIntMatrix.Create;
  assert(c1 = r2, MATRIX_ERROR1);
  mat3.InitZero(r1, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      for k := 0 to c2 - 1 do
        begin
          mat3.data[i, k] := mat3.data[i, k] + mat1.data[i, j] * mat2.data[j, k];
        end;
  result := mat3;
end;

function mtimes(const mat1, mat2: TLongintMatrix): TLongintMatrix;
var
  r1, r2, c1, c2: longint;
  i, j, k: longint;
  mat3: TLongintMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TLongintMatrix.Create;
  assert(c1 = r2, MATRIX_ERROR1);
  mat3.InitZero(r1, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      for k := 0 to c2 - 1 do
        begin
          mat3.data[i, k] := mat3.data[i, k] + mat1.data[i, j] * mat2.data[j, k];
        end;
  result := mat3;
end;

function mtimes(const mat1, mat2: TRealMatrix): TRealMatrix;
var
  r1, r2, c1, c2: longint;
  i, j, k: longint;
  mat3: TRealMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TRealMatrix.Create;
  assert(c1 = r2, MATRIX_ERROR1);
  mat3.InitZero(r1, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      for k := 0 to c2 - 1 do
        begin
          mat3.data[i, k] := mat3.data[i, k] + mat1.data[i, j] * mat2.data[j, k];
        end;
  result := mat3;
end;

function mtimes(const mat1, mat2: TExtMatrix): TExtMatrix;
var
  r1, r2, c1, c2: longint;
  i, j, k: longint;
  mat3: TExtMatrix;
begin
  r1 := mat1.getlength.rows;
  c1 := mat1.getlength.columns;
  r2 := mat2.getlength.rows;
  c2 := mat2.getlength.columns;
  mat3 := TExtMatrix.Create;
  assert(c1 = r2, MATRIX_ERROR1);
  mat3.InitZero(r1, c2);
  for i := 0 to r1 - 1 do
    for j := 0 to c1 - 1 do
      for k := 0 to c2 - 1 do
        begin
          mat3.data[i, k] := mat3.data[i, k] + mat1.data[i, j] * mat2.data[j, k];
        end;
  result := mat3;
end;

function diag(const mat: TIntMatrix): TIntVector;
var
  i: longint;
begin
  result := TIntVector.Create;
  result.InitZero(min(mat.getlength.columns, mat.getlength.rows));
  for i := 0 to result.getlength - 1 do
    result.data[i] := mat.data[i, i];
end;

function diag(const mat: TLongintMatrix): TLongintVector;
var
  i: longint;
begin
  result := TLongintVector.Create;
  result.InitZero(min(mat.getlength.columns, mat.getlength.rows));
  for i := 0 to result.getlength - 1 do
    result.data[i] := mat.data[i, i];
end;

function diag(const mat: TRealMatrix): TRealVector;
var
  i: longint;
begin
  result := TRealVector.Create;
  result.InitZero(min(mat.getlength.columns, mat.getlength.rows));
  for i := 0 to result.getlength - 1 do
    result.data[i] := mat.data[i, i];
end;

function diag(const mat: TExtMatrix): TExtVector;
var
  i: longint;
begin
  result := TExtVector.Create;
  result.InitZero(min(mat.getlength.columns, mat.getlength.rows));
  for i := 0 to result.getlength - 1 do
    result.data[i] := mat.data[i, i];
end;

function diag(const vec: TIntVector): TIntMatrix;
begin
  result := diag(vec, vec.getlength, vec.getlength);
end;

function diag(const vec: TLongintVector): TLongintMatrix;
begin
  result := diag(vec, vec.getlength, vec.getlength);
end;

function diag(const vec: TRealVector): TRealMatrix;
begin
  result := diag(vec, vec.getlength, vec.getlength);
end;

function diag(const vec: TExtVector): TExtMatrix;
begin
  result := diag(vec, vec.getlength, vec.getlength);
end;

function diag(const vec: TIntVector; rows, columns: longint): TIntMatrix;
var
  i: longint;
begin
  result := TIntMatrix.Create;
  result.InitZero(rows, columns);
  for i := 0 to vec.getlength - 1 do
    result.data[i, i] := vec.data[i];
end;

function diag(const vec: TLongintVector; rows, columns: longint
  ): TLongintMatrix;
var
  i: longint;
begin
  result := TLongintMatrix.Create;
  result.InitZero(rows, columns);
  for i := 0 to vec.getlength - 1 do
    result.data[i, i] := vec.data[i];
end;

function diag(const vec: TRealVector; rows, columns: longint): TRealMatrix;
var
  i: longint;
begin
  result := TREalMatrix.Create;
  result.InitZero(rows, columns);
  for i := 0 to vec.getlength - 1 do
    result.data[i, i] := vec.data[i];
end;

function diag(const vec: TExtVector; rows, columns: longint): TExtMatrix;
var
  i: longint;
begin
  result := TExtMatrix.Create;
  result.InitZero(rows, columns);
  for i := 0 to vec.getlength - 1 do
    result.data[i, i] := vec.data[i];
end;

function diag(length: longint): TIntMatrix;
var
  i: longint;
begin
  result := TIntMatrix.Create;
  result.InitZero(length, length);
  for i := 0 to length - 1 do
    result.data[i, i] := 1;
end;

function diag(length: longint): TLongintMatrix;
var
  i: longint;
begin
  result := TLongintMatrix.Create;
  result.InitZero(length, length);
  for i := 0 to length - 1 do
    result.data[i, i] := 1;
end;

function diag(length: longint): TRealMatrix;
var
  i: longint;
begin
  result := TRealMatrix.Create;
  result.InitZero(length, length);
  for i := 0 to length - 1 do
    result.data[i, i] := 1;
end;

function diag(length: longint): TExtMatrix;
var
  i: longint;
begin
  result := TExtMatrix.Create;
  result.InitZero(length, length);
  for i := 0 to length - 1 do
    result.data[i, i] := 1;
end;

function trace(const mat: TIntMatrix): integer;
var
  i: longint;
  vec: TIntVector;
begin
  result := 0;
  vec := diag(mat);
  for i := 0 to vec.getlength - 1 do
  result := result + vec.data[i];
end;

function trace(const mat: TLongintMatrix): longint;
var
  i: longint;
  vec: TLongintVector;
begin
  result := 0;
  vec := diag(mat);
  for i := 0 to vec.getlength - 1 do
  result := result + vec.data[i];
end;

function trace(const mat: TRealMatrix): real;
begin
  result := sum(diag(mat).data);
end;

function trace(const mat: TExtMatrix): extended;
begin
  result := sum(diag(mat).data);
end;

function det(const mat: TIntMatrix): integer;
var
  i: longint;
  r, c: longint;
begin
  result := 0;
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  assert(r = c, MATRIX_ERROR1);
  if r = 1 then
    result := mat.data[0, 0]
  else if r = 2 then
    result := mat.data[0, 0] * mat.data[1, 1] - mat.data[0, 1] * mat.data[1, 0]
  else if r = 3 then
    result := mat.data[0, 0] * mat.data[1, 1] * mat.data[2, 2]
              + mat.data[0, 1] * mat.data[1, 2] * mat.data[2, 0]
              + mat.data[0, 2] * mat.data[1, 0] * mat.data[2, 1]
              - mat.data[0, 2] * mat.data[1, 1] * mat.data[2, 0]
              - mat.data[0, 1] * mat.data[1, 0] * mat.data[2, 2]
              - mat.data[0, 0] * mat.data[1, 2] * mat.data[2, 1]
  else result := 0; // to be extended to higher dimensionality
end;

function det(const mat: TLongintMatrix): longint;
begin

end;

function det(const mat: TRealMatrix): real;
begin

end;

function det(const mat: TExtMatrix): extended;
begin

end;

function submatrix(const mat: TIntMatrix; const row, column: longint
  ): TIntMatrix;

var
  i, r, c, r2, c2: longint;
  mat2: TIntMatrix;

function GetSubRow(const mat: TIntMatrix; const i, column: longint): TIntVector_data;
var
  k: longint;
begin
  if column = maxlongint then
    result := mat.data[i]
  else
    begin
    SetLength(result, c2);
      if column = 0 then
        for k := 1 to c - 1 do
          result[k - 1] := mat.data[i, k]
      else if column = c then
      for k := 0 to c - 2 do
        result[k] := mat.data[i, k]
      else
        begin
          for k := 0 to column - 1 do
            result[k] := mat.data[i, k];
          for k := column + 1 to c - 1 do
            result[k - 1] := mat.data[i, k];
        end;
    end;
end;

begin
  r := mat.getlength.rows;
  c := mat.getlength.columns;
  assert((row = maxlongint) or (row < r), MATRIX_ERROR1); // row and column are zero-based
  assert((column = maxlongint) or (column < c), MATRIX_ERROR1);
  mat2 := TIntMatrix.Create;
  if row = maxlongint then
    r2 := r
  else
    r2 := r - 1;
  if column = maxlongint then
    c2 := c
  else
    c2 := c - 1;
  mat2.InitZero(r2, c2);
  if row = maxlongint then
    for i := 0 to r - 1 do
      mat2.data[i] := GetSubRow(mat, i, column)
    else
  begin
    if row = 0 then
      for i := 1 to r - 1 do
        mat2.data[i - 1] := GetSubRow(mat, i, column)
    else if row = r then
      for i := 0 to r - 2 do
        mat2.data[i] := GetSubRow(mat, i, column)
    else
      begin
        for i := 0 to row - 1 do
          mat2.data[i] := GetSubRow(mat, i, column);
        for i := row + 1 to r - 1 do
          mat2.data[i - 1] := GetSubRow(mat, i, column);
      end;
  end;
  result := mat2;
end;

function submatrix(const mat: TIntMatrix; const row, column: TLongintVector
  ): TIntMatrix;
var
  i: longint;
  mat2: TIntMatrix;
begin
  mat2 := mat;
  for i := column.getlength - 1 downto 0 do
    mat2 := submatrix(mat2, MaxLongint, column.data[i]);
  for i := row.getlength - 1 downto 0 do
    mat2 := submatrix(mat2, row.data[i], MaxLongint);
  result := mat2;
end;

function lngamma(x: extended): extended;
const
  p: array[0..6] of extended =
     (1.000000000190015, 76.18009172947146, -86.50532032941677,
      24.01409824083091, -1.231739572450155, 0.0012086509738662,
      -0.000005395239385); // Lanczos coefficients for logarithmic version
var
  i: integer;
  a, b: extended;
begin
  if x < 0.5 then  // reflection formula for small x values
    result := ln(pi / sin(pi * x)) - lngamma(1 - x)
  else
    begin
      x := x - 1;
      b := x + 5.5;
      a := p[0];
      for i := 1 to high(p) do
        a := a + p[i] / (x + i);
      result := ln(sqrt(2 * pi)) + ln(a) - b + ln(b) * (x + 0.5);
    end;
end;

function gamma(x: extended): extended;
begin
  result := exp(lngamma(x));
end;

operator + (const vec1: TIntVector; const vec2: TIntVector): TIntVector;
begin
  result := add(vec1, vec2);
end;

operator + (const vec1: TLongintVector; const vec2: TLongintVector
  ): TLongintVector;
begin
  result := add(vec1, vec2);
end;

operator + (const vec1: TRealVector; const vec2: TRealVector): TRealVector;
begin
  result := add(vec1, vec2);
end;

operator + (const vec1: TExtVector; const vec2: TExtVector): TExtVector;
begin
  result := add(vec1, vec2);
end;

operator - (const vec1: TIntVector; const vec2: TIntVector): TIntVector;
begin
  result := sub(vec1, vec2);
end;

operator - (const vec1: TLongintVector; const vec2: TLongintVector
  ): TLongintVector;
begin
  result := sub(vec1, vec2);
end;

operator - (const vec1: TRealVector; const vec2: TRealVector): TRealVector;
begin
  result := sub(vec1, vec2);
end;

operator - (const vec1: TExtVector; const vec2: TExtVector): TExtVector;
begin
  result := sub(vec1, vec2);
end;

operator * (const scalar: integer; const vec: TIntVector): TIntVector;
begin
  result := stimes(scalar, vec);
end;

operator *( const scalar: longint; const vec: TLongintVector): TLongintVector;
begin
  result := stimes(scalar, vec);
end;

operator * (const scalar: real; const vec: TRealVector): TRealVector;
begin
  result := stimes(scalar, vec);
end;

operator * (const scalar: extended; const vec: TExtVector): TExtVector;
begin
  result := stimes(scalar, vec);
end;

operator * (const vec: TIntVector; const scalar: integer): TIntVector;
begin
  result := stimes(vec, scalar);
end;

operator * (const vec: TLongintVector; const scalar: longint): TLongintVector;
begin
  result := stimes(vec, scalar);
end;

operator * (const vec: TRealVector; const scalar: real): TRealVector;
begin
  result := stimes(vec, scalar);
end;

operator * (const vec: TExtVector; const scalar: extended): TExtVector;
begin
  result := stimes(vec, scalar);
end;

operator * (const vec1: TIntVector; const vec2: TIntVector): integer;
begin
  result := stimes(vec1, vec2);
end;

operator * (const vec1: TLongintVector; const vec2: TLongintVector
  ): longint;
begin
  result := stimes(vec1, vec2);
end;

operator * (const vec1: TRealVector; const vec2: TRealVector): real;
begin
  result := stimes(vec1, vec2);
end;

operator * (const vec1: TExtVector; const vec2: TExtVector): extended;
begin
  result := stimes(vec1, vec2);
end;

operator + (const mat1, mat2: TIntMatrix): TIntMatrix;
begin
  result := add(mat1, mat2);
end;

operator + (const mat1, mat2: TLongintMatrix): TLongintMatrix;
begin
  result := add(mat1, mat2);
end;

operator + (const mat1, mat2: TRealMatrix): TRealMatrix;
begin
  result := add(mat1, mat2);
end;

operator + (const mat1, mat2: TExtMatrix): TExtMatrix;
begin
  result := add(mat1, mat2);
end;

operator - (const mat1, mat2: TIntMatrix): TIntMatrix;
begin
  result := sub(mat1, mat2);
end;

operator - (const mat1, mat2: TLongintMatrix): TLongintMatrix;
begin
  result := sub(mat1, mat2);
end;

operator - (const mat1, mat2: TRealMatrix): TRealMatrix;
begin
  result := sub(mat1, mat2);
end;

operator - (const mat1, mat2: TExtMatrix): TExtMatrix;
begin
  result := sub(mat1, mat2);
end;

operator * (const scalar: integer; const mat: TIntMatrix): TIntMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const scalar: longint; const mat: TLongintMatrix): TLongintMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const scalar: real; const mat: TRealMatrix): TRealMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const scalar: extended; const mat: TExtMatrix): TExtMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const mat: TIntMatrix; const scalar: integer): TIntMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const mat: TLongintMatrix; const scalar: longint): TLongintMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const mat: TRealMatrix; const scalar: real): TRealMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const mat: TExtMatrix; const scalar: extended): TExtMatrix;
begin
  result := stimes(scalar, mat);
end;

operator * (const mat: tIntMatrix; const vec: TIntVector): TIntVector;
begin
  result := vtimes(mat, vec);
end;

operator * (const mat: TLongintMatrix; const vec: TLongintVector): TLongintVector;
begin
  result := vtimes(mat, vec);
end;

operator * (const mat: TRealMatrix; const vec: TRealVector): TRealVector;
begin
  result := vtimes(mat, vec);
end;

operator * (const mat: TExtMatrix; const vec: TExtVector): TExtVector;
begin
  result := vtimes(mat, vec);
end;

operator*(const mat1, mat2: tIntMatrix): tIntMatrix;
begin
  result := mtimes(mat1, mat2);
end;

operator*(const mat1, mat2: TLongintMatrix): TLongintMatrix;
begin
  result := mtimes(mat1, mat2);
end;

operator*(const mat1, mat2: TRealMatrix): TRealMatrix;
begin
  result := mtimes(mat1, mat2);
end;

operator*(const mat1, mat2: TExtMatrix): TExtMatrix;
begin
  result := mtimes(mat1, mat2);
end;

{ TExtMatrix }

function TExtMatrix.GetItem(i, j: longint): extended;
begin
  Result := FData[i, j];
end;

procedure TExtMatrix.SetItem(i, j: longint; data: extended);
begin
  FData[i, j] := data;
end;

constructor TExtMatrix.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TExtMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TExtMatrix.InitZero(rows, columns: longint);
  var
    i, j: longint;
  begin
    SetLength(Fdata, rows);
    for i := 0 to rows - 1 do
    begin
      SetLength(data[i], columns);
      for j := 0 to columns - 1 do
        data[i, j] := 0;
    end;
end;

procedure TExtMatrix.InitOne(rows, columns: longint);
  var
    i, j: longint;
  begin
    SetLength(Fdata, rows);
    for i := 0 to rows - 1 do
    begin
      SetLength(data[i], columns);
      for j := 0 to columns - 1 do
        data[i, j] := 1;
    end;
end;

function TExtMatrix.getlength: TMatrixLength;
begin
  result.rows := length(Fdata);
  result.columns := length(data[0]);
end;

{ TRealMatrix }

function TRealMatrix.GetItem(i, j: longint): real;
begin
  Result := FData[i, j];
end;

procedure TRealMatrix.SetItem(i, j: longint; data: real);
begin
  FData[i, j] := data;
end;

constructor TRealMatrix.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TRealMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TRealMatrix.InitZero(rows, columns: longint);
  var
    i, j: longint;
  begin
    SetLength(Fdata, rows);
    for i := 0 to rows - 1 do
    begin
      SetLength(data[i], columns);
      for j := 0 to columns - 1 do
        data[i, j] := 0;
    end;
end;

procedure TRealMatrix.InitOne(rows, columns: longint);
  var
    i, j: longint;
  begin
    SetLength(Fdata, rows);
    for i := 0 to rows - 1 do
    begin
      SetLength(data[i], columns);
      for j := 0 to columns - 1 do
        data[i, j] := 1;
    end;
end;

function TRealMatrix.getlength: TMatrixLength;
begin
  result.rows := length(Fdata);
  result.columns := length(data[0]);
end;

{ TLongintMatrix }

function TLongintMatrix.GetItem(i, j: longint): integer;
begin
  Result := FData[i, j];
end;

procedure TLongintMatrix.SetItem(i, j: longint; data: longint);
begin
  FData[i, j] := data;
end;

constructor TLongintMatrix.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TLongintMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TLongintMatrix.InitZero(rows, columns: longint);
  var
    i, j: longint;
  begin
    SetLength(Fdata, rows);
    for i := 0 to rows - 1 do
    begin
      SetLength(data[i], columns);
      for j := 0 to columns - 1 do
        data[i, j] := 0;
    end;
end;

procedure TLongintMatrix.InitOne(rows, columns: longint);
  var
    i, j: longint;
  begin
    SetLength(Fdata, rows);
    for i := 0 to rows - 1 do
    begin
      SetLength(data[i], columns);
      for j := 0 to columns - 1 do
        data[i, j] := 1;
    end;
end;

function TLongintMatrix.getlength: TMatrixLength;
begin
  result.rows := length(Fdata);
  result.columns := length(data[0]);
end;

{ TIntMatrix }

function TIntMatrix.GetItem(i, j: longint): integer;
begin
  Result := FData[i, j];
end;

procedure TIntMatrix.SetItem(i, j: longint; data: integer);
begin
  FData[i, j] := data;
end;

constructor TIntMatrix.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TIntMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TIntMatrix.InitZero(rows, columns: longint);
var
  i, j: longint;
begin
  SetLength(Fdata, rows);
  for i := 0 to rows - 1 do
  begin
    SetLength(data[i], columns);
    for j := 0 to columns - 1 do
      data[i, j] := 0;
  end;
end;

procedure TIntMatrix.InitOne(rows, columns: longint);
var
  i, j: longint;
begin
  SetLength(Fdata, rows);
  for i := 0 to rows - 1 do
  begin
    SetLength(data[i], columns);
    for j := 0 to columns - 1 do
      data[i, j] := 1;
  end;
end;

function TIntMatrix.getlength: TMatrixLength;
begin
  result.rows := length(Fdata);
  result.columns := length(data[0]);
end;

{ TLogicMatrix }

function TLogicMatrix.GetItem(i, j: longint): boolean;
begin
  Result := FData[i, j];
end;

procedure TLogicMatrix.SetItem(i, j: longint; data: boolean);
begin
  FData[i, j] := data;
end;

constructor TLogicMatrix.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TLogicMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TLogicMatrix.InitFalse(rows, columns: longint);
var
  i, j: longint;
begin
  SetLength(Fdata, rows);
  for i := 0 to rows - 1 do
  begin
    SetLength(data[i], columns);
    for j := 0 to columns - 1 do
      data[i, j] := false;
  end;
end;

procedure TLogicMatrix.InitTrue(rows, columns: longint);
var
  i, j: longint;
begin
  SetLength(Fdata, rows);
  for i := 0 to rows - 1 do
  begin
    SetLength(data[i], columns);
    for j := 0 to columns - 1 do
      data[i, j] := true;
  end;
end;

function TLogicMatrix.getlength: TMatrixLength;
begin
  result.rows := length(Fdata);
  result.columns := length(data[0]);
end;

{ TIntVector }

function TIntVector.GetItem(i: longint): integer;
begin
  Result := FData[i];
end;

procedure TIntVector.SetItem(i: longint; data: integer);
begin
  FData[i] := data;
end;

constructor TIntVector.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TIntVector.Destroy;
begin
  inherited Destroy;
end;

procedure TIntVector.InitZero(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 0;
end;

procedure TIntVector.InitOne(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 1;
end;

function TIntVector.getlength: longint;
begin
  result := length(Fdata);
end;

{ TLongintVector }

function TLongintVector.GetItem(i: longint): longint;
begin
  Result := FData[i];
end;

procedure TLongintVector.SetItem(i: longint; data: longint);
begin
  FData[i] := data;
end;

constructor TLongintVector.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TLongintVector.Destroy;
begin
  inherited Destroy;
end;

procedure TLongintVector.InitZero(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 0;
end;

procedure TLongintVector.InitOne(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 1;
end;

function TLongintVector.getlength: longint;
begin
  result := length(Fdata);
end;

{ TRealVector }

function TRealVector.GetItem(i: longint): real;
begin
  Result := FData[i];
end;

procedure TRealVector.SetItem(i: longint; data: real);
begin
  FData[i] := data;
end;

constructor TRealVector.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TRealVector.Destroy;
begin
  inherited Destroy;
end;

procedure TRealVector.InitZero(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 0;
end;
procedure TRealVector.InitOne(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 1;
end;
function TRealVector.getlength: longint;
begin
  result := length(Fdata);
end;

{ TExtVector }

function TExtVector.GetItem(i: longint): extended;
begin
  Result := FData[i];
end;

procedure TExtVector.SetItem(i: longint; data: extended);
begin
  FData[i] := data;
end;

constructor TExtVector.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TExtVector.Destroy;
begin
  inherited Destroy;
end;

procedure TExtVector.InitZero(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 0;
end;

procedure TExtVector.InitOne(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := 1;
end;

function TExtVector.getlength: longint;
begin
  result := length(Fdata);
end;

{ TLogicVector }

function TLogicVector.GetItem(i: longint): boolean;
begin
  Result := FData[i]
end;

procedure TLogicVector.SetItem(i: longint; data: boolean);
begin
  FData[i] := data;
end;

constructor TLogicVector.Create;
begin
  inherited Create;
  SetLength(Fdata, 0);
end;

destructor TLogicVector.Destroy;
begin
  inherited Destroy;
end;

procedure TLogicVector.InitFalse(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := false;
end;

procedure TLogicVector.InitTrue(length: longint);
var
  i: longint;
begin
  SetLength(Fdata, length);
  for i := 0 to length - 1 do
    data[i] := true;
end;

function TLogicVector.getlength: longint;
begin
  result := length(Fdata);
end;

end.

{ References:

1. Collinge RM: Algorithm 31 - Gamma Function. Communications of the ACM 4(2)
   February 1961: 105. DOI 10.1145/366105.366152

2. Lipp MF: Algorithm 34 - Gamma Function. Communications of the ACM 4(2)
   February 1961: 106. DOI 10.1145/366105.366156

3. BBC BASIC algorithm for Gamma Function
   http://rosettacode.org/wiki/Gamma_function#BBC_BASIC

}
