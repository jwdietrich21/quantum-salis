unit qsTestCases;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Test Cases for FPCUnit }

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
  Classes, SysUtils, fpcunit, testutils, testregistry, Math,
  qsFoundation, DescStat;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
    procedure CodeVersionCheck;
  end;

  { THyperGeometricTestCases }

  THyperGeometricTestCases = class(TTestCase)
  published
    procedure TestGammaFunction;
    procedure TestLnGammaFunction;
  end;

  { TVectorTestCases }

  TVectorTestCases = class(TTestCase)
  published
    procedure TestLogicVectorAsFalse;
    procedure TestLogicVectorAsTrue;
    procedure TestAddIntVector1;
    procedure TestAddIntVector2;
    procedure TestAddIntVector3;
    procedure TestAddLongintVector1;
    procedure TestAddLongintVector3;
    procedure TestAddRealVector1;
    procedure TestAddRealVector3;
    procedure TestAddExtVector1;
    procedure TestAddExtVector3;
    procedure TestSubIntVector1;
    procedure TestSubIntVector2;
    procedure TestSubIntVector3;
    procedure TestSubLongintVector1;
    procedure TestSubRealVector1;
    procedure TestSubExtVector1;
    procedure TestSProdIntVector1;
    procedure TestSProdIntVector2;
    procedure TestSProdIntVector3;
    procedure TestSProdIntVector4;
    procedure TestSProdIntVector5;
    procedure TestSProdIntVector6;
    procedure TestSProdIntVector7;
    procedure TestSProdIntVector10;
    procedure TestSProdIntVector11;
    procedure TestSProdIntVector12;
    procedure TestSProdIntVector13;
    procedure TestSProdLongintVector1;
    procedure TestSProdLongintVector2;
    procedure TestSProdLongintVector3;
    procedure TestSProdLongintVector10;
    procedure TestSProdLongintVector11;
    procedure TestSProdRealVector1;
    procedure TestSProdRealVector2;
    procedure TestSProdRealVector3;
    procedure TestSProdRealVector10;
    procedure TestSProdRealVector11;
    procedure TestSProdExtVector1;
    procedure TestSProdExtVector2;
    procedure TestSProdExtVector3;
    procedure TestSProdExtVector10;
    procedure TestSProdExtVector11;
    procedure TestAbsIntVector;
    procedure TestAbsLongintVector;
    procedure TestAbsRealVector;
    procedure TestAbsExtVector;
  end;

  { TMatrixTestCases }

  TMatrixTestCases = class(TTestCase)
  published
    procedure TestLogicMatrixAsFalse;
    procedure TestLogicMatrixAsTrue;
    procedure TestIntMatrix1;
    procedure TestIntMatrix2;
    procedure TestIntMatrix3;
    procedure TestIntMatrix4;
    procedure TestIntMatrix5;
    procedure TestIntMatrix6;
    procedure TestIntMatrix7;
    procedure TestIntMatrix8;
    procedure TestIntMatrix9;
    procedure TestLongintMatrix1;
    procedure TestRealMatrix1;
    procedure TestRealMatrix2;
    procedure TestExtMatrix1;
    procedure TextExtMatrix2;
    procedure TestIntSubmatrix1;
    procedure TestIntSubmatrix2;
    procedure TestIntDet1;
    procedure TestIntDet2;
    procedure TestIntDet3;
    procedure TestVProdIntMatrix1;
    procedure TestVProdIntMatrix2;
    procedure TestVProdLongintMatrix1;
    procedure TestVProdLongintMatrix2;
    procedure TestVProdRealMatrix1;
    procedure TestVProdExtMatrix1;
    procedure TestMProdIntMatrix1;
    procedure TestMProdIntMatrix2;
    procedure TestMProdIntMatrix3;
    procedure TestMProdLongintMatrix1;
    procedure TestMProdRealMatrix1;
    procedure TestMProdExtMatrix1;
  end;

{ TDescStatTestCases }

TDescStatTestCases = class(TTestCase)
  published
    procedure TestRMS1;
    procedure TestRMS2;
  end;

implementation

{ THyperGeometricTestCases }

procedure THyperGeometricTestCases.TestGammaFunction;
begin
  AssertTrue(abs(9.513507699 - gamma(0.1)) < 0.001);
  AssertTrue(abs(4.590843712 - gamma(0.2)) < 0.001);
  AssertTrue(abs(2.991568988 - gamma(0.3)) < 0.001);
  AssertTrue(abs(2.218159543 - gamma(0.4)) < 0.001);
  AssertTrue(abs(1.772453851 - gamma(0.5)) < 0.001);
  AssertTrue(abs(1.489192249 - gamma(0.6)) < 0.001);
  AssertTrue(abs(1.298055333 - gamma(0.7)) < 0.001);
  AssertTrue(abs(1.164229714 - gamma(0.8)) < 0.001);
  AssertTrue(abs(1.068628702 - gamma(0.9)) < 0.001);
  AssertTrue(abs(1 - gamma(1)) < 0.001);
  AssertTrue(abs(0.951350770 - gamma(1.1)) < 0.001);
  AssertTrue(abs(0.918168742 - gamma(1.2)) < 0.001);
  AssertTrue(abs(0.897470696 - gamma(1.3)) < 0.001);
  AssertTrue(abs(0.887263818 - gamma(1.4)) < 0.001);
  AssertTrue(abs(0.886226925 - gamma(1.5)) < 0.001);
  AssertTrue(abs(0.893515349 - gamma(1.6)) < 0.001);
  AssertTrue(abs(0.908638733 - gamma(1.7)) < 0.001);
  AssertTrue(abs(0.931383771 - gamma(1.8)) < 0.001);
  AssertTrue(abs(0.961765832 - gamma(1.9)) < 0.001);
  AssertTrue(abs(1 - gamma(2)) < 0.001);
end;

procedure THyperGeometricTestCases.TestLnGammaFunction;
begin
  AssertEquals(0, lngamma(1));
end;

{ TMatrixTestCases }

procedure TMatrixTestCases.TestLogicMatrixAsFalse;
var
  mat: TLogicMatrix;
begin
  mat := TLogicMatrix.Create;
  mat.InitFalse(5, 3);
  AssertEquals(5, mat.getlength.rows);
  AssertEquals(3, mat.getlength.columns);
  AssertFalse(mat[1, 1]);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestLogicMatrixAsTrue;
var
  mat: TLogicMatrix;
begin
  mat := TLogicMatrix.Create;
  mat.InitTrue(2, 4);
  AssertTrue(mat[1, 1]);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix1;
var
  mat: TIntMatrix;
begin
  mat := TIntMatrix.Create;
  mat.InitOne(2, 3);
  mat[0, 1] := -3;
  mat[0, 2] := 2;
  mat.data[1, 1] := 2;  // alternative notation
  mat.data[1, 2] := 7;
  AssertEquals(-3, mat[0, 1]);
  AssertEquals(7, mat[1, 2]);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix2;
  var
    mat1, mat2, mat3: TIntMatrix;
begin
  mat1 := TIntMatrix.Create;
  mat2 := TIntMatrix.Create;
  mat1.InitOne(2, 3);
  mat2.InitOne(2, 3);
  mat1[0, 1] := -3;
  mat1[0, 2] := 2;
  mat1[1, 1] := 2;
  mat1[1, 2] := 7;
  mat2[0, 0] := 0;
  mat2[0, 1] := 3;
  mat2[0, 2] := 5;
  mat2[1, 0] := 2;
  mat2[1, 2] := -1;
  mat3 := mat1 + mat2;
  AssertEquals(1, mat3[0, 0]);
  AssertEquals(0, mat3[0, 1]);
  AssertEquals(7, mat3[0, 2]);
  AssertEquals(3, mat3[1, 0]);
  AssertEquals(3, mat3[1, 1]);
  AssertEquals(6, mat3[1, 2]);
  mat1.Destroy;
  mat2.Destroy;
  mat3.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix3;
  var
    mat1, mat2: TIntMatrix;
begin
  mat1 := TIntMatrix.Create;
  mat1.InitOne(2, 3);
  mat1[0, 1] := -3;
  mat1[0, 2] := 2;
  mat1[1, 1] := 2;
  mat1[1, 2] := 7;
  mat2 := 5 * mat1;
  AssertEquals(5, mat2[0, 0]);
  AssertEquals(-15, mat2[0, 1]);
  AssertEquals(10, mat2[0, 2]);
  AssertEquals(5, mat2[1, 0]);
  AssertEquals(10, mat2[1, 1]);
  AssertEquals(35, mat2[1, 2]);
  mat1.Destroy;
  mat2.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix4;
  var
    mat1, mat2: TIntMatrix;
begin
  mat1 := TIntMatrix.Create;
  mat1.InitOne(2, 3);
  mat1[0, 1] := -3;
  mat1[0, 2] := 2;
  mat1[1, 1] := 2;
  mat1[1, 2] := 7;
  mat2 := mat1 * 5;
  AssertEquals(5, mat2[0, 0]);
  AssertEquals(-15, mat2[0, 1]);
  AssertEquals(10, mat2[0, 2]);
  AssertEquals(5, mat2[1, 0]);
  AssertEquals(10, mat2[1, 1]);
  AssertEquals(35, mat2[1, 2]);
  mat1.Destroy;
  mat2.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix5;
  var
    mat1, mat2: TIntMatrix;
  begin
    mat1 := TIntMatrix.Create;
    mat1.InitOne(3, 2);
    mat1[1, 0] := 3;
    mat1[2, 0] := 5;
    mat1[0, 1] := 2;
    mat1[1, 1] := 4;
    mat1[2, 1] := 6;
    mat2 := trans(mat1);
    AssertEquals(1, mat2[0, 0]);
    AssertEquals(3, mat2[0, 1]);
    AssertEquals(5, mat2[0, 2]);
    AssertEquals(2, mat2[1, 0]);
    AssertEquals(4, mat2[1, 1]);
    AssertEquals(6, mat2[1, 2]);
    mat1.Destroy;
    mat2.Destroy;
  end;

procedure TMatrixTestCases.TestIntMatrix6;
var
  mat: TIntMatrix;
  vec: TIntVector;
begin
  mat := TIntMatrix.Create;
  mat.InitZero(3, 3);
  mat[0, 0] := 1;
  mat[0, 1] := 5;
  mat[0, 2] := 3;
  mat[1, 1] := 2;
  mat[1, 2] := 4;
  mat[2, 0] := 3;
  mat[2, 2] := 9;
  vec := Diag(mat);
  AssertEquals(1, vec[0]);
  AssertEquals(2, vec[1]);
  AssertEquals(9, vec[2]);
  vec.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix7;
var
  mat: TIntMatrix;
  vec: TIntVector;
begin
  vec := TIntVector.Create;
  vec.InitOne(3);
  vec[1] := 2;
  vec[2] := 3;
  mat := diag(vec);
  AssertEquals(1, mat[0, 0]);
  AssertEquals(2, mat[1, 1]);
  AssertEquals(3, mat[2, 2]);
  AssertEquals(0, mat[0, 1]);
  AssertEquals(0, mat[1, 0]);
  vec.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix8;
var
  mat: TIntMatrix;
begin
  mat := diag(3);
  AssertEquals(1, mat[0, 0]);
  AssertEquals(1, mat[1, 1]);
  AssertEquals(1, mat[2, 2]);
  AssertEquals(0, mat[0, 1]);
  AssertEquals(0, mat[1, 0]);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntMatrix9;
var
  mat: TIntMatrix;
  scalar: integer;
begin
  mat := TIntMatrix.Create;
  mat.InitZero(3, 3);
  mat[0, 0] := 1;
  mat[0, 1] := 5;
  mat[0, 2] := 3;
  mat[1, 1] := 2;
  mat[1, 2] := 4;
  mat[2, 0] := 3;
  mat[2, 2] := 9;
  scalar := trace(mat);
  AssertEquals(12, scalar);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestLongintMatrix1;
var
  mat: TLongintMatrix;
begin
  mat := TLongintMatrix.Create;
  mat.InitZero(2, 3);
  mat[0, 1] := 3;
  mat[0, 2] := 5;
  mat[1, 0] := 2;
  mat[1, 1] := 1;
  mat[1, 2] := -1;
  AssertEquals(3, mat[0, 1]);
  AssertEquals(-1, mat[1, 2]);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestRealMatrix1;
var
  mat: TRealMatrix;
begin
  mat := TRealMatrix.Create;
  mat.InitZero(2, 3);
  mat[0, 0] := -pi;
  mat[0, 1] := exp(1);
  mat[0, 2] := 1;
  mat[1, 0] := 1;
  mat[1, 2] := 2;
  AssertEquals(exp(1), mat[0, 1]);
  AssertEquals(-pi, mat[0, 0]);
  AssertEquals(2, mat[1, 2]);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestRealMatrix2;
var
  mat: TRealMatrix;
  vec: TRealVector;
begin
  vec := TRealVector.Create;
  vec.InitOne(3);
  vec[1] := exp(1);
  vec[2] := pi;
  mat := diag(vec, 5, 5);
  AssertEquals(1, mat[0, 0]);
  AssertEquals(exp(1), mat[1, 1]);
  AssertEquals(pi, mat[2, 2]);
  AssertEquals(0, mat[0, 1]);
  AssertEquals(0, mat[1, 0]);
  AssertEquals(0, mat[0, 4]);
  AssertEquals(0, mat[4, 0]);
  vec.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestExtMatrix1;
var
  mat: TExtMatrix;
begin
  mat := TExtMatrix.Create;
  mat.InitZero(3, 2);
  mat[0, 0] := 1;
  mat[0, 1] := exp(1);
  mat[1, 1] := 1;
  mat[2, 1] := pi;
  AssertEquals(exp(1), mat[0, 1]);
  AssertEquals(pi, mat[2, 1]);
  AssertEquals(0, mat[1, 0]);
  mat.Destroy;
end;

procedure TMatrixTestCases.TextExtMatrix2;
var
  mat: TExtMatrix;
  scalar: extended;
begin
  mat := TExtMatrix.Create;
  mat.InitZero(3, 3);
  mat[0, 0] := 1;
  mat[0, 1] := 5;
  mat[0, 2] := 3;
  mat[1, 1] := 2;
  mat[1, 2] := 4;
  mat[2, 0] := 3;
  mat[2, 2] := 9;
  scalar := trace(mat);
  AssertEquals(12, scalar);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntSubmatrix1;
var
  mat1, mat2: TIntMatrix;
  matdata: array[0..2, 0..3] of integer = ((1, 2, 3, 4),
                                           (5, 6, 7, 8),
                                           (9, 10, 11, 12));
begin
  mat1 := TIntMatrix.Create;
  mat1.InitZero(3, 4);
  mat1.data[0] := matdata[0];
  mat1.data[1] := matdata[1];
  mat1.data[2] := matdata[2];
  mat2 := submatrix(mat1, MaxLongint, MaxLongint);
  AssertEquals(3, mat2.getlength.rows);
  AssertEquals(4, mat2.getlength.columns);
  AssertEquals(1, mat2[0, 0]);
  AssertEquals(12, mat2[2, 3]);
  mat2.Destroy;
  mat2 := submatrix(mat1, 1, MaxLongint);
  AssertEquals(2, mat2.getlength.rows);
  AssertEquals(4, mat2.getlength.columns);
  AssertEquals(1, mat2[0, 0]);
  AssertEquals(12, mat2[1, 3]);
  mat2.Destroy;
  mat2 := submatrix(mat1, 0, MaxLongint);
  AssertEquals(2, mat2.getlength.rows);
  AssertEquals(4, mat2.getlength.columns);
  AssertEquals(5, mat2[0, 0]);
  AssertEquals(12, mat2[1, 3]);
  mat2.Destroy;
  mat2 := submatrix(mat1, 2, MaxLongint);
  AssertEquals(2, mat2.getlength.rows);
  AssertEquals(4, mat2.getlength.columns);
  AssertEquals(1, mat2[0, 0]);
  AssertEquals(8, mat2[1, 3]);
  mat2.Destroy;
  mat2 := submatrix(mat1, MaxLongint, 1);
  AssertEquals(3, mat2.getlength.rows);
  AssertEquals(3, mat2.getlength.columns);
  AssertEquals(1, mat2[0, 0]);
  AssertEquals(12, mat2[2, 2]);
  mat2.Destroy;
  mat2 := submatrix(mat1, MaxLongint, 0);
  AssertEquals(3, mat2.getlength.rows);
  AssertEquals(3, mat2.getlength.columns);
  AssertEquals(2, mat2[0, 0]);
  AssertEquals(12, mat2[2, 2]);
  mat2.Destroy;
  mat2 := submatrix(mat1, MaxLongint, 3);
  AssertEquals(3, mat2.getlength.rows);
  AssertEquals(3, mat2.getlength.columns);
  AssertEquals(1, mat2[0, 0]);
  AssertEquals(11, mat2[2, 2]);
  mat2.Destroy;
  mat2 := submatrix(mat1, 1, 1);
  AssertEquals(2, mat2.getlength.rows);
  AssertEquals(3, mat2.getlength.columns);
  AssertEquals(1, mat2[0, 0]);
  AssertEquals(12, mat2[1, 2]);
  mat2.Destroy;
  mat2 := submatrix(mat1, 2, 1);
  AssertEquals(2, mat2.getlength.rows);
  AssertEquals(3, mat2.getlength.columns);
  AssertEquals(1, mat2[0, 0]);
  AssertEquals(8, mat2[1, 2]);
  mat2.Destroy;
  mat1.Destroy;
end;

procedure TMatrixTestCases.TestIntSubmatrix2;
var
  mat1, mat2: TIntMatrix;
  vec_c, vec_r: TLongintVector;
  matdata: array[0..2, 0..3] of integer = ((1, 2, 3, 4),
                                           (5, 6, 7, 8),
                                           (9, 10, 11, 12));
  columns: array[0..1] of integer = (1, 2);
  rows: array[0..1] of integer = (0, 1);
begin
  mat1 := TIntMatrix.Create;
  mat1.InitZero(3, 4);
  mat1.data[0] := matdata[0];
  mat1.data[1] := matdata[1];
  mat1.data[2] := matdata[2];
  vec_c := TLongintVector.Create;
  vec_r := TLongintVector.Create;
  vec_c.data := columns;
  vec_r.data := rows;
  mat2 := submatrix(mat1, vec_r, vec_c);
  AssertEquals(1, mat2.getlength.rows);
  AssertEquals(2, mat2.getlength.columns);
  AssertEquals(9, mat2[0, 0]);
  AssertEquals(12, mat2[0, 1]);
  mat2.Destroy;
  vec_r.Destroy;
  vec_c.Destroy;
end;

procedure TMatrixTestCases.TestIntDet1;
var
  mat: TIntMatrix;
  scalar: integer;
begin
  mat := TIntMatrix.Create;
  mat.InitZero(1, 1);
  mat[0, 0] := 7;
  scalar := det(mat);
  AssertEquals(7, scalar);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntDet2;
var
  mat: TIntMatrix;
  scalar: integer;
begin
  mat := TIntMatrix.Create;
  mat.InitZero(2, 2);
  mat[0, 0] := 4;
  mat[0, 1] := 5;
  mat[1, 0] := 3;
  mat[1, 1] := -2;
  scalar := det(mat);
  AssertEquals(-23, scalar);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestIntDet3;
var
  mat: TIntMatrix;
  scalar: integer;
begin
  mat := TIntMatrix.Create;
  mat.InitZero(3, 3);
  mat[0, 0] := 3;
  mat[0, 1] := -1;
  mat[0, 2] := 2;
  mat[1, 1] := 3;
  mat[1, 2] := 4;
  mat[2, 0] := 1;
  mat[2, 1] := -2;
  scalar := det(mat);
  AssertEquals(14, scalar);
  mat.Destroy;
end;

procedure TMatrixTestCases.TestVProdIntMatrix1;
var
  mat: TIntMatrix;
  vec1, vec2: TIntVector;
  matdata: array[0..3, 0..2] of integer = ((3, 4, 5), (1, 2, 3), (2, 3, 6), (12, 18, 30));
  vecdata: array[0..2] of integer = (14, 10, 8);
begin
  mat := TIntMatrix.Create;
  mat.InitZero(4, 3);
  mat.data[0] := matdata[0];
  mat.data[1] := matdata[1];
  mat.data[2] := matdata[2];
  mat.data[3] := matdata[3];
  vec1 := TIntVector.Create;
  vec1.InitZero(3);
  vec1.data := vecdata;
  vec2 := mat * vec1;
  AssertEquals(122, vec2[0]);
  AssertEquals(58, vec2[1]);
  AssertEquals(106, vec2[2]);
  AssertEquals(588, vec2[3]);
  vec1.Destroy;
  vec2.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestVProdIntMatrix2;
var
  mat: TIntMatrix;
  vec1, vec2: TIntVector;
  matdata: array[0..2, 0..3] of integer = ((7, 8, 1, 3),
                                           (2, 0, 0, 1),
                                           (0, 7, 0, 0));
  vecdata: array[0..3] of integer = (1, 7, 5, 8);
begin
  mat := TIntMatrix.Create;
  mat.InitZero(3, 4);
  mat.data[0] := matdata[0];
  mat.data[1] := matdata[1];
  mat.data[2] := matdata[2];
  vec1 := TIntVector.Create;
  vec1.InitZero(4);
  vec1.data := vecdata;
  vec2 := mat * vec1;
  AssertEquals(92, vec2[0]);
  AssertEquals(10, vec2[1]);
  AssertEquals(49, vec2[2]);
  vec1.Destroy;
  vec2.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestVProdLongintMatrix1;
var
  mat: TLongintMatrix;
  vec1, vec2: TLongintVector;
  matdata: array[0..2, 0..3] of longint = ((7, 8, 1, 3),
                                           (2, 0, 0, 1),
                                           (0, 7, 0, 0));
  vecdata: array[0..3] of longint = (0, 4, 0, 2);
begin
  mat := TLongintMatrix.Create;
  mat.InitZero(3, 4);
  mat.data[0] := matdata[0];
  mat.data[1] := matdata[1];
  mat.data[2] := matdata[2];
  vec1 := TLongintVector.Create;
  vec1.InitZero(4);
  vec1.data := vecdata;
  vec2 := mat * vec1;
  AssertEquals(38, vec2[0]);
  AssertEquals(2, vec2[1]);
  AssertEquals(28, vec2[2]);
  vec1.Destroy;
  vec2.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestVProdLongintMatrix2;
var
  mat: TLongintMatrix;
  vec1, vec2: TLongintVector;
  matdata: array[0..3, 0..2] of longint = ((65, 90, 152),
                                           (35, 77, 210),
                                           (83, 124, 257),
                                           (61, 82, 104));
  vecdata: array[0..2] of longint = (150, 90, 45);
begin
  mat := TLongintMatrix.Create;
  mat.InitZero(4, 3);
  mat.data[0] := matdata[0];
  mat.data[1] := matdata[1];
  mat.data[2] := matdata[2];
  mat.data[3] := matdata[3];
  vec1 := TLongintVector.Create;
  vec1.InitZero(3);
  vec1.data := vecdata;
  vec2 := mat * vec1;
  AssertEquals(24690, vec2[0]);
  AssertEquals(21630, vec2[1]);
  AssertEquals(35175, vec2[2]);
  AssertEquals(21210, vec2[3]);
  vec1.Destroy;
  vec2.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestVProdRealMatrix1;
var
  mat: TRealMatrix;
  vec1, vec2: TRealVector;
  matdata: array[0..2, 0..2] of real = ((0.8, 0.2, 0.1),
                                        (0.1, 0.7, 0.5),
                                        (0.1, 0.1, 0.4));
  vecdata: array[0..2] of real = (1000, 4000, 2000);
begin
  mat := TRealMatrix.Create;
  mat.InitZero(3, 3);
  mat.data[0] := matdata[0];
  mat.data[1] := matdata[1];
  mat.data[2] := matdata[2];
  vec1 := TRealVector.Create;
  vec1.InitZero(3);
  vec1.data := vecdata;
  vec2 := mat * vec1;
  AssertEquals(1800, vec2[0]);
  AssertEquals(3900, vec2[1]);
  AssertEquals(1300, vec2[2]);
  vec1.Destroy;
  vec2.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestVProdExtMatrix1;
var
  mat: TExtMatrix;
  vec1, vec2: TExtVector;
  matdata: array[0..2, 0..2] of extended = ((0.8, 0.2, 0.1),
                                            (0.1, 0.7, 0.5),
                                            (0.1, 0.1, 0.4));
  vecdata: array[0..2] of extended = (1000, 4000, 2000);
begin
  mat := TExtMatrix.Create;
  mat.InitZero(3, 3);
  mat.data[0] := matdata[0];
  mat.data[1] := matdata[1];
  mat.data[2] := matdata[2];
  vec1 := TExtVector.Create;
  vec1.InitZero(3);
  vec1.data := vecdata;
  vec2 := mat * vec1;
  AssertEquals(1800, vec2[0]);
  AssertEquals(3900, vec2[1]);
  AssertEquals(1300, vec2[2]);
  vec1.Destroy;
  vec2.Destroy;
  mat.Destroy;
end;

procedure TMatrixTestCases.TestMProdIntMatrix1;
var
  mat1, mat2, mat3: TIntMatrix;
  var i, j: integer;
  mat1data: array[0..3, 0..3] of integer = ((1, -3, 7, 6),
                                            (1, 2, -2, 7),
                                            (-1, 1, 2, 1),
                                            (1, 0, -3, 3));
begin
  mat1 := TIntMatrix.Create;
  mat2 := TIntMatrix.Create;
  mat1.InitZero(4, 4);
  mat1.data[0] := mat1data[0];
  mat1.data[1] := mat1data[1];
  mat1.data[2] := mat1data[2];
  mat1.data[3] := mat1data[3];
  mat2 := diag(4);
  mat3 := mat1 * mat2;
  for i := 0 to mat1.getlength.rows - 1 do
    for j := 0 to mat1.getlength.columns - 1 do;
      AssertEquals(mat1.data[i, j], mat3.data[i, j]);
  mat1.Destroy;
  mat2.Destroy;
  mat3.Destroy;
end;

procedure TMatrixTestCases.TestMProdIntMatrix2;
var
  mat1, mat2, mat3: TIntMatrix;
  mat1data: array[0..2, 0..3] of integer = ((7, 8, 1, 3),
                                            (2, 0, 0, 1),
                                            (0, 7, 0, 0));
  mat2data: array[0..3, 0..1] of integer = ((1, 0),
                                            (7, 4),
                                            (5, 0),
                                            (8, 2));
begin
  mat1 := TIntMatrix.Create;
  mat2 := TIntMatrix.Create;
  mat1.InitZero(3, 4);
  mat2.InitZero(4, 2);
  mat1.data[0] := mat1data[0];
  mat1.data[1] := mat1data[1];
  mat1.data[2] := mat1data[2];
  mat2.data[0] := mat2data[0];
  mat2.data[1] := mat2data[1];
  mat2.data[2] := mat2data[2];
  mat2.data[3] := mat2data[3];
  mat3 := mat1 * mat2;
  AssertEquals(92, mat3[0, 0]);
  AssertEquals(10, mat3[1, 0]);
  AssertEquals(49, mat3[2, 0]);
  AssertEquals(38, mat3[0, 1]);
  AssertEquals(2, mat3[1, 1]);
  AssertEquals(28, mat3[2, 1]);
  mat1.Destroy;
  mat2.Destroy;
  mat3.Destroy;
end;

procedure TMatrixTestCases.TestMProdIntMatrix3;
var
  mat1, mat2, mat3: TIntMatrix;
  mat1data: array[0..2, 0..3] of integer = ((2, -1, -3, 0),
                                            (-7, 5, 11, 1),
                                            (-4, 2, 7, 1));
  mat2data: array[0..3, 0..2] of integer = ((1, 2, 3),
                                            (1, 1, 0),
                                            (0, 1, 2),
                                            (2, -1, -1));
begin
  mat1 := TIntMatrix.Create;
  mat2 := TIntMatrix.Create;
  mat1.InitZero(3, 4);
  mat2.InitZero(4, 3);
  mat1.data[0] := mat1data[0];
  mat1.data[1] := mat1data[1];
  mat1.data[2] := mat1data[2];
  mat2.data[0] := mat2data[0];
  mat2.data[1] := mat2data[1];
  mat2.data[2] := mat2data[2];
  mat2.data[3] := mat2data[3];
  mat3 := mat1 * mat2;
  AssertEquals(1, mat3[0, 0]);
  AssertEquals(0, mat3[1, 0]);
  AssertEquals(0, mat3[2, 0]);
  AssertEquals(0, mat3[0, 1]);
  AssertEquals(1, mat3[1, 1]);
  AssertEquals(0, mat3[2, 1]);
  AssertEquals(0, mat3[0, 2]);
  AssertEquals(0, mat3[1, 2]);
  AssertEquals(1, mat3[2, 2]);
  mat1.Destroy;
  mat2.Destroy;
  mat3.Destroy;
end;

procedure TMatrixTestCases.TestMProdLongintMatrix1;
var
  mat1, mat2, mat3: TLongintMatrix;
  mat1data: array[0..1, 0..2] of longint = ((-4, 3, 11),
                                            (1, 0, 1));
  mat2data: array[0..2, 0..1] of longint = ((1, 0),
                                            (-2, 1),
                                            (4, -3));
begin
  mat1 := TLongintMatrix.Create;
  mat2 := TLongintMatrix.Create;
  mat1.InitZero(2, 3);
  mat2.InitZero(3, 2);
  mat1.data[0] := mat1data[0];
  mat1.data[1] := mat1data[1];
  mat2.data[0] := mat2data[0];
  mat2.data[1] := mat2data[1];
  mat2.data[2] := mat2data[2];
  mat3 := mat1 * mat2;
  AssertEquals(34, mat3.data[0, 0]);
  AssertEquals(5, mat3.data[1, 0]);
  AssertEquals(-30, mat3.data[0, 1]);
  AssertEquals(-3, mat3.data[1, 1]);
  mat1.Destroy;
  mat2.Destroy;
  mat3.Destroy;
end;

procedure TMatrixTestCases.TestMProdRealMatrix1;
var
  mat1, mat2, mat3: TRealMatrix;
  mat1data: array[0..2, 0..2] of real = ((0.6, 0, 0),
                                         (0.4, 1, 0.4),
                                         (0, 0, 0.6));
  mat2data: array[0..2, 0..2] of real = ((1, 0.15, 0),
                                         (0, 0.25, 0),
                                         (0, 0.6, 1));
begin
  mat1 := TRealMatrix.Create;
  mat2 := TRealMatrix.Create;
  mat1.InitZero(3, 3);
  mat2.InitZero(3, 3);
  mat1.data[0] := mat1data[0];
  mat1.data[1] := mat1data[1];
  mat1.data[2] := mat1data[2];
  mat2.data[0] := mat2data[0];
  mat2.data[1] := mat2data[1];
  mat2.data[2] := mat2data[2];
  mat3 := mat1 * mat2;
  AssertEquals(0.6, mat3.data[0, 0]);
  AssertEquals(0.4, mat3.data[1, 0]);
  AssertEquals(0, mat3.data[2, 0]);
  AssertEquals(0.09, mat3.data[0, 1]);
  AssertEquals(0.55, mat3.data[1, 1]);
  AssertEquals(0.36, mat3.data[2, 1]);
  AssertEquals(0, mat3.data[0, 2]);
  AssertEquals(0.4, mat3.data[1, 2]);
  AssertEquals(0.6, mat3.data[2, 2]);
  mat1.Destroy;
  mat2.Destroy;
  mat3.Destroy;
end;

procedure TMatrixTestCases.TestMProdExtMatrix1;
var
  mat1, mat2, mat3: TExtMatrix;
  mat1data: array[0..2, 0..2] of extended = ((1, 0.15, 0),
                                             (0, 0.25, 0),
                                             (0, 0.6, 1));
  mat2data: array[0..2, 0..2] of extended = ((0.6, 0, 0),
                                             (0.4, 1, 0.4),
                                             (0, 0, 0.6));
begin
  mat1 := TExtMatrix.Create;
  mat2 := TExtMatrix.Create;
  mat1.InitZero(3, 3);
  mat2.InitZero(3, 3);
  mat1.data[0] := mat1data[0];
  mat1.data[1] := mat1data[1];
  mat1.data[2] := mat1data[2];
  mat2.data[0] := mat2data[0];
  mat2.data[1] := mat2data[1];
  mat2.data[2] := mat2data[2];
  mat3 := mat1 * mat2;
  AssertEquals(0.66, mat3[0, 0]);
  AssertEquals(0.1, mat3[1, 0]);
  AssertEquals(0.24, mat3[2, 0]);
  AssertEquals(0.15, mat3[0, 1]);
  AssertEquals(0.25, mat3[1, 1]);
  AssertEquals(0.6, mat3[2, 1]);
  AssertEquals(0.06, mat3[0, 2]);
  AssertEquals(0.1, mat3[1, 2]);
  AssertEquals(0.84, mat3[2, 2]);
  mat1.Destroy;
  mat2.Destroy;
  mat3.Destroy;
end;

{ TDescStatTestCases }

procedure TDescStatTestCases.TestRMS1;
var
  dat: TExtArray;
begin
  SetLength(dat, 2);
  dat[0] := 1;
  dat[1] := 2;
  AssertTrue(abs(rms(dat) - 1.58) < 0.01);
end;

procedure TDescStatTestCases.TestRMS2;
var
  vec : TExtVector;
begin
  vec := TExtVector.Create;
  vec.InitOne(2);
  vec.data[1] := 2;
  AssertTrue(abs(rms(vec) - 1.58) < 0.01);
  vec.Destroy;
end;

{ TVectorTestCases }

procedure TVectorTestCases.TestLogicVectorAsFalse;
var
  vec: TLogicVector;
begin
  vec := TLogicVector.Create;
  vec.InitFalse(5);
  AssertFalse(vec[3]);
  vec.Destroy
end;

procedure TVectorTestCases.TestLogicVectorAsTrue;
var
  vec: TLogicVector;
begin
  vec := TLogicVector.Create;
  vec.InitTrue(5);
  AssertTrue(vec[3]);
  vec.Destroy
end;

procedure TVectorTestCases.TestAddIntVector1;
const
  testLength = 13;
var
  vec1, vec2, vec3: TIntVector;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 + vec2;
  AssertEquals(1, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestAddIntVector2;
const
  testLength = 13;
var
  vec1, vec2, vec3: TIntVector;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 + vec2;
  AssertEquals(2, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestAddIntVector3;
const
  testLength1 = 3;
  testLength2 = 13;
var
  vec1, vec2, vec3: TIntVector;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  vec3 := vec1 + vec2;
  AssertTrue(vec3 = nil);
  vec1.Destroy;
  vec2.Destroy;
  if assigned(vec3) then
    vec3.Destroy;
end;

procedure TVectorTestCases.TestAddLongintVector1;
const
  testLength = 7;
var
  vec1, vec2, vec3: TLongIntVector;
begin
  vec1 := TLongIntVector.Create;
  vec2 := TLongIntVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  vec2.data[5] := 13;
  vec3 := vec1 + vec2;
  AssertEquals(2, vec3[1]);
  AssertEquals(14, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestAddLongintVector3;
const
  testLength1 = 3;
  testLength2 = 13;
var
  vec1, vec2, vec3: TLongintVector;
begin
  vec1 := TLongintVector.Create;
  vec2 := TLongintVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  vec3 := vec1 + vec2;
  AssertTrue(vec3 = nil);
  vec1.Destroy;
  vec2.Destroy;
  if assigned(vec3) then
    vec3.Destroy;
end;

procedure TVectorTestCases.TestAddRealVector1;
const
  testLength = 31;
var
  vec1, vec2, vec3: TRealVector;
begin
  vec1 := TRealVector.Create;
  vec2 := TRealVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  vec1.data[5] := 3.14;
  vec3 := vec1 + vec2;
  AssertEquals(1, vec3[17]);
  AssertEquals(4.14, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestAddRealVector3;
const
  testLength1 = 3;
  testLength2 = 13;
var
  vec1, vec2, vec3: TRealVector;
begin
  vec1 := TRealVector.Create;
  vec2 := TRealVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  vec3 := vec1 + vec2;
  AssertTrue(vec3 = nil);
  vec1.Destroy;
  vec2.Destroy;
  if assigned(vec3) then
    vec3.Destroy;
end;

procedure TVectorTestCases.TestAddExtVector1;
const
  testLength = 21;
var
  vec1, vec2, vec3: TExtVector;
begin
  vec1 := TExtVector.Create;
  vec2 := TExtVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 + vec2;
  AssertEquals(2, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestAddExtVector3;
const
  testLength1 = 3;
  testLength2 = 13;
var
  vec1, vec2, vec3: TExtVector;
begin
  vec1 := TExtVector.Create;
  vec2 := TExtVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  vec3 := vec1 + vec2;
  AssertTrue(vec3 = nil);
  vec1.Destroy;
  vec2.Destroy;
  if assigned(vec3) then
    vec3.Destroy;
end;

procedure TVectorTestCases.TestSubIntVector1;
const
  testLength = 13;
var
  vec1, vec2, vec3: TIntVector;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 - vec2;
  AssertEquals(-1, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestSubIntVector2;
const
  testLength = 13;
var
  vec1, vec2, vec3: TIntVector;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 - vec2;
  AssertEquals(0, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestSubIntVector3;
const
  testLength1 = 3;
  testLength2 = 13;
var
  vec1, vec2, vec3: TIntVector;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  vec3 := vec1 - vec2;
  AssertTrue(vec3 = nil);
  vec1.Destroy;
  vec2.Destroy;
  if assigned(vec3) then
    vec3.Destroy;
end;

procedure TVectorTestCases.TestSubLongintVector1;
const
  testLength = 13;
var
  vec1, vec2, vec3: TLongintVector;
begin
  vec1 := TLongintVector.Create;
  vec2 := TLongintVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 - vec2;
  AssertEquals(-1, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestSubRealVector1;
const
  testLength = 13;
var
  vec1, vec2, vec3: TRealVector;
begin
  vec1 := TRealVector.Create;
  vec2 := TRealVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 - vec2;
  AssertEquals(-1, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestSubExtVector1;
const
  testLength = 13;
var
  vec1, vec2, vec3: TExtVector;
begin
  vec1 := TExtVector.Create;
  vec2 := TExtVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  vec3 := vec1 - vec2;
  AssertEquals(-1, vec3[5]);
  vec1.Destroy;
  vec2.Destroy;
  vec3.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector1;
const
  testLength = 13;
var
  vec1, vec2: TIntVector;
  scalar: integer;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(0, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector2;
const
  testLength = 13;
var
  vec1, vec2: TIntVector;
  scalar: integer;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(testLength, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector3;
const
  testLength1 = 3;
  testLength2 = 7;
var
  vec1, vec2: TIntVector;
  scalar: integer;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  scalar := vec1 * vec2;
  AssertEquals(0, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector4;
const
  testLength = 2;
var
  vec1, vec2: TIntVector;
  scalar: integer;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2.InitZero(testLength);
  vec1[0] := 2;
  vec1[1] := 6;
  vec2[0] := 3;
  vec2[1] := -1;
  scalar := vec1 * vec2;
  AssertEquals(0, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector5;
const
  testLength = 2;
var
  vec1, vec2: TIntVector;
  scalar: integer;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2.InitZero(testLength);
  vec1[0] := -3;
  vec1[1] := 2;
  vec2[0] := 7;
  vec2[1] := 11;
  scalar := vec1 * vec2;
  AssertEquals(1, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector6;
const
  testLength = 3;
var
  vec1, vec2: TIntVector;
  scalar: integer;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2.InitZero(testLength);
  vec1[0] := 2;
  vec1[1] := -6;
  vec1[2] := 4;
  vec2[0] := -1;
  vec2[1] := 2;
  vec2[2] := 3;
  scalar := vec1 * vec2;
  AssertEquals(-2, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector7;
const
  testLength = 3;
var
  vec1, vec2: TIntVector;
  scalar: integer;
begin
  vec1 := TIntVector.Create;
  vec2 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2.InitZero(testLength);
  vec1.data[0] := 3;
  vec1[1] := 4;
  vec1[2] := 5;
  vec2[0] := 1;
  vec2.data[1] := -1;
  vec2.data[2] := 2;
  scalar := vec1 * vec2;
  AssertEquals(9, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector10;
const
  testLength = 13;
var
  scalar: integer;
  vec1, vec2: TintVector;
begin
  scalar := 15;
  vec1 := TIntVector.Create;
  vec1.InitOne(testLength);
  vec2 := scalar * vec1;
  AssertEquals(15, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector11;
const
  testLength = 13;
var
  scalar: integer;
  vec1, vec2: TintVector;
begin
  scalar := 17;
  vec1 := TIntVector.Create;
  vec1.InitOne(testLength);
  vec2 := vec1 * scalar;
  AssertEquals(17, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector12;
const
  testLength = 13;
var
  scalar: integer;
  vec1, vec2: TintVector;
begin
  scalar := 0;
  vec1 := TIntVector.Create;
  vec1.InitOne(testLength);
  vec2 := scalar * vec1;
  AssertEquals(0, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdIntVector13;
const
  testLength = 13;
var
  scalar: integer;
  vec1, vec2: TintVector;
begin
  scalar := 15;
  vec1 := TIntVector.Create;
  vec1.InitZero(testLength);
  vec2 := scalar * vec1;
  AssertEquals(0, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdLongintVector1;
const
  testLength = 13;
var
  vec1, vec2: TLongintVector;
  scalar: longint;
begin
  vec1 := TLongintVector.Create;
  vec2 := TLongintVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(0, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdLongintVector2;
const
  testLength = 13;
var
  vec1, vec2: TLongintVector;
  scalar: longint;
begin
  vec1 := TLongintVector.Create;
  vec2 := TLongintVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(testLength, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdLongintVector3;
const
  testLength1 = 3;
  testLength2 = 7;
var
  vec1, vec2: TLongintVector;
  scalar: longint;
begin
  vec1 := TLongintVector.Create;
  vec2 := TLongintVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  scalar := vec1 * vec2;
  AssertEquals(0, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdLongintVector10;
const
  testLength = 13;
var
  scalar: longint;
  vec1, vec2: TLongintVector;
begin
  scalar := 15;
  vec1 := TLongintVector.Create;
  vec1.InitOne(testLength);
  vec2 := scalar * vec1;
  AssertEquals(15, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdLongintVector11;
const
  testLength = 13;
var
  scalar: longint;
  vec1, vec2: TLongintVector;
begin
  scalar := 17;
  vec1 := TLongintVector.Create;
  vec1.InitOne(testLength);
  vec2 := vec1 * scalar;
  AssertEquals(17, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdRealVector1;
const
  testLength = 13;
var
  vec1, vec2: TRealVector;
  scalar: real;
begin
  vec1 := TRealVector.Create;
  vec2 := TRealVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(0, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdRealVector2;
const
  testLength = 13;
var
  vec1, vec2: TRealVector;
  scalar: real;
begin
  vec1 := TRealVector.Create;
  vec2 := TRealVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(testLength, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdRealVector3;
const
  testLength1 = 3;
  testLength2 = 7;
var
  vec1, vec2: TRealVector;
  scalar: real;
begin
  vec1 := TRealVector.Create;
  vec2 := TRealVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  scalar := vec1 * vec2;
  AssertTrue(IsNan(scalar));
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdRealVector10;
const
  testLength = 13;
var
  scalar: real;
  vec1, vec2: TRealVector;
begin
  scalar := 15.3;
  vec1 := TRealVector.Create;
  vec1.InitOne(testLength);
  vec2 := scalar * vec1;
  AssertEquals(15.3, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdRealVector11;
const
  testLength = 13;
var
  scalar: real;
  vec1, vec2: TRealVector;
begin
  scalar := 17.1;
  vec1 := TRealVector.Create;
  vec1.InitOne(testLength);
  vec2 := vec1 * scalar;
  AssertEquals(17.1, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;


procedure TVectorTestCases.TestSProdExtVector1;
const
  testLength = 13;
var
  vec1, vec2: TExtVector;
  scalar: extended;
begin
  vec1 := TExtVector.Create;
  vec2 := TExtVector.Create;
  vec1.InitZero(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(0, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdExtVector2;
const
  testLength = 13;
var
  vec1, vec2: TExtVector;
  scalar: extended;
begin
  vec1 := TExtVector.Create;
  vec2 := TExtVector.Create;
  vec1.InitOne(testLength);
  vec2.InitOne(testLength);
  scalar := vec1 * vec2;
  AssertEquals(testLength, scalar);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdExtVector3;
const
  testLength1 = 3;
  testLength2 = 7;
var
  vec1, vec2: TExtVector;
  scalar: extended;
begin
  vec1 := TExtVector.Create;
  vec2 := TExtVector.Create;
  vec1.InitOne(testLength1);
  vec2.InitOne(testLength2);
  scalar := vec1 * vec2;
  AssertTrue(IsNan(scalar));
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestSProdExtVector10;
const
  testLength = 13;
var
  scalar: extended;
  vec1, vec2: TExtVector;
begin
  scalar := 15.5;
  vec1 := TExtVector.Create;
  vec1.InitOne(testLength);
  vec2 := scalar * vec1;
  AssertEquals(15.5, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;


procedure TVectorTestCases.TestSProdExtVector11;
const
  testLength = 13;
var
  scalar: extended;
  vec1, vec2: TExtVector;
begin
  scalar := 17.9;
  vec1 := TExtVector.Create;
  vec1.InitOne(testLength);
  vec2 := vec1 * scalar;
  AssertEquals(17.9, vec2[5]);
  vec1.Destroy;
  vec2.Destroy;
end;

procedure TVectorTestCases.TestAbsIntVector;
const
  testLength = 3;
var
  vec: TIntVector;
begin
  vec := TIntVector.Create;
  vec.InitOne(testLength);
  vec.data[1] := 2;
  vec.data[2] := 2;
  AssertEquals(15, abs(-15)); // ensure that predefined abs function still works
  AssertEquals(pi, abs(-pi));
  AssertEquals(3, abs(vec));
  vec.Destroy;
end;

procedure TVectorTestCases.TestAbsLongintVector;
const
  testLength = 2;
var
  vec: TLongintVector;
begin
  vec := TLongintVector.Create;
  vec.InitZero(testLength);
  vec.data[0] := 4;
  vec.data[1] := 3;
  AssertEquals(5, abs(vec));
  vec.Destroy;
end;

procedure TVectorTestCases.TestAbsRealVector;
const
  testLength = 3;
var
  vec: TRealVector;
begin
  vec := TRealVector.Create;
  AssertTrue(IsNan(abs(vec)));
  vec.InitZero(testLength);
  vec.data[0] := 3;
  vec.data[1] := 12;
  vec.data[2] := 4;
  AssertEquals(13, abs(vec));
  vec.data[0] := 1.3;
  vec.data[1] := 2.1;
  vec.data[2] := 5.4;
  AssertTrue(abs(5.94 - abs(vec)) < 0.1);
  vec.InitOne(2);
  vec.data[0] := 1;
  vec.data[1] := 4;
  AssertTrue(abs(4.12 - abs(vec)) < 0.1);
  vec.Destroy;
end;

procedure TVectorTestCases.TestAbsExtVector;
const
  testLength = 3;
var
  vec: TExtVector;
begin
  vec := TExtVector.Create;
  AssertTrue(IsNan(abs(vec)));
  vec.InitZero(testLength);
  vec.data[0] := 3;
  vec.data[1] := 3.5;
  vec.data[2] := 2.5;
  AssertTrue(abs(5.2 - abs(vec)) < 0.1);
  vec.InitOne(2);
  vec.data[0] := 10;
  vec.data[1] := -3;
  AssertTrue(abs(10.44 - abs(vec)) < 0.1);
  vec.Destroy;
end;

{ TControlTestCases }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

procedure TControlTestCases.CodeVersionCheck;
{ The subsequent tests are compatible with QUANTUM SALIS version 1.0 }
begin
  AssertEquals(1, QS_major);
  AssertEquals(0, QS_minor);
end;

initialization

  RegisterTest(TControlTestCases);
  RegisterTest(THyperGeometricTestCases);
  RegisterTest(TVectorTestCases);
  RegisterTest(TMatrixTestCases);
  RegisterTest(TDescStatTestCases);

end.

