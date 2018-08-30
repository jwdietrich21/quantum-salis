unit qsTestCases;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Test Cases for FPCUnit }

{ Version 1.0.0 }

{ (c) J. W. Dietrich, 1994 - 2018 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2018 }

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
  Classes, SysUtils, fpcunit, testutils, testregistry,
  qsFoundation;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
    procedure CodeVersionCheck;
  end;

  { TVectorTestCases }

  TVectorTestCases = class(TTestCase)
  published
    procedure TestAddIntVector1;
    procedure TestAddIntVector2;
  end;

implementation

{ TVectorTestCases }

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
  AssertEquals(1, vec3.data[5]);
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
  AssertEquals(2, vec3.data[5]);
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
  RegisterTest(TVectorTestCases);
end.

