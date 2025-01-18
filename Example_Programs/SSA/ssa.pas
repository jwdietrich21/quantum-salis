unit SSA;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit implements algorithms and help functions for stochastic simulation }

{ Version 1.0.0 (Alpina) }

{ (c) J. W. Dietrich, 1994 - 2021 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2021 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://quantum-salis.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}
{$assertions on}

interface

uses
  Classes, SysUtils, Math, qsFoundation;

type
  TNamesArray = array of string;
  TSpeciesArray = array[0..1] of string;
  TSpeciesVector = TRealVector;
  TRateVector = TRealVector;

function Species(SMatrix: TIntMatrix; SpeciesNames: TNamesArray;
  const i: integer): TSpeciesArray;
function Reaction(const reactants, products: string): string;
function MaxMolecules(const SpeciesVector: TSpeciesVector;
  const StoichiometryMatrix: TIntMatrix): TRealVector;
function ExponentMatrix(const StoichiometryMatrix: TIntMatrix): TIntMatrix;
procedure InitGillespie(const RateVector: TRateVector;
  const StoichiometryMatrix: TIntMatrix; out ExpMatrix: TIntMatrix);
procedure GillespieStep(const tmax: integer;
  const StoichiometryMatrix, ExpMatrix: TIntMatrix;
  const RateVector: TRealVector; var t: real; var SpeciesVector: TSpeciesVector);
procedure GillespieStep(const tmax: integer;
  const StoichiometryMatrix, ExpMatrix: TIntMatrix;
  const RateVector: TRealVector; const limit: real; const limitIndex: integer;
  var t: real; var SpeciesVector: TSpeciesVector);
procedure FinishGillespie(var ExpMatrix: TIntMatrix);

implementation

function Sum(const data: array of integer): integer;
{ Like math.sum, but for integers }
var
  i, n: longint;
begin
  result := 0;
  n := length(data);
  for i := 0 to n - 1 do
    result := result + data[i];
end;

function Colsum(const data: TIntMatrix; const i: integer): integer;
var
  j, n: longint;
begin
  result := 0;
  n := data.getlength.rows;
  for j := 0 to n - 1 do
    result := result + data[j, i];
end;

function Species(SMatrix: TIntMatrix; SpeciesNames: TNamesArray;
  const i: integer): TSpeciesArray;
{ Delivers array with reactants (index 0) and products (index 1) from }
{  stoichiometry matrix }
var
  j, cell: integer;
  sym: string;
begin
  Result[0] := '0';
  Result[1] := '0';
  for j := 1 to SMatrix.getlength.rows do
  begin
    cell := SMatrix[j - 1, i - 1];
    if cell = 0 then
      sym := ''
    else if (cell = 1) or (cell = -1) then
      sym := SpeciesNames[j - 1]
    else
      sym := IntToStr(abs(cell)) + SpeciesNames[j - 1];
    if cell < 0 then // reactant
    begin
      if Result[0] = '0' then
        Result[0] := sym
      else
        Result[0] := Result[0] + ' + ' + sym;
    end
    else if cell > 0 then // product
    begin
      if Result[1] = '0' then
        Result[1] := sym
      else
        Result[1] := Result[1] + ' + ' + sym;
    end;
  end;
end;

function Reaction(const reactants, products: string): string;
{ Composes representation of reaction from reactant and product descriptions }
begin
  if (Reactants = '0') and (Products = '0') then
    Result := ''
  else
    Result := Reactants + '  -->  ' + Products;
end;

function MaxMolecules(const SpeciesVector: TSpeciesVector;
  const StoichiometryMatrix: TIntMatrix): TRealVector;
var
  i: integer;
  maxAmount: real;
begin
  Result := TRealVector.Create;
  Result.InitZero(SpeciesVector.getlength);
  maxAmount := MaxValue(SpeciesVector.data);
  for i := 0 to SpeciesVector.getlength - 1 do
    if MaxIntValue(StoichiometryMatrix.data[i]) > 0 then
      Result[i] := maxAmount * MaxIntValue(StoichiometryMatrix.data[i]);
end;

function ExponentMatrix(const StoichiometryMatrix: TIntMatrix): TIntMatrix;
{ Converts stoichiometric constants to exponents for consuming reactions }
var
  i, j, r, c: integer;
begin
  r := StoichiometryMatrix.getlength.rows;
  c := StoichiometryMatrix.getlength.columns;
  Result := TIntMatrix.Create;
  Result.InitZero(r, c);
  if assigned(StoichiometryMatrix) then
    for i := 0 to r - 1 do
      for j := 0 to c - 1 do
      begin
        if StoichiometryMatrix[i, j] < 0 then
          Result[i, j] := abs(StoichiometryMatrix[i, j]);
      end;
end;

procedure InitGillespie(const RateVector: TRateVector;
  const StoichiometryMatrix: TIntMatrix; out ExpMatrix: TIntMatrix);
begin
  ExpMatrix := ExponentMatrix(StoichiometryMatrix);
end;

procedure GillespieStep(const tmax: integer; const StoichiometryMatrix,
  ExpMatrix: TIntMatrix; const RateVector: TRealVector; const limit: real;
  const limitIndex: integer; var t: real; var SpeciesVector: TSpeciesVector);
{ Single step of SSA algorithm according to Gillespie (loop to be provided }
{ by software implementation) }
var
  i, j: integer;
  r1, r2, q: real;
  a, p: TRealVector;
  a0, dt, expsum: real;
begin
  a := TRealVector.Create;                  // Propensity vector
  a.InitZero(RateVector.getlength);
  p := TRealVector.Create;                  // Probability vector
  p.InitZero(a.getlength);
  r1 := random;                             // two random numbers in (0, 1)
  r2 := random;
  for i := 0 to RateVector.getlength - 1 do // calculate propensity function
    begin
      expsum := Colsum(ExpMatrix, i);       // any degradation specified?
      if expsum = 0 then
        a[i] := 0                           // rate=0 if no degradation
      else
      begin
        a[i] := RateVector[i];              // get rate
        for j := 0 to ExpMatrix.getlength.rows - 1 do
          if ExpMatrix[j, i] > 0 then       // a = rate * N of each species
            a[i] := a[i] * ExpMatrix[j, i] * SpeciesVector[j];
      end;
    end;
  a0 := math.sum(a.data);                   // sum of all propensities
  assert(a0 > 0, 'Empty propensity vector');
  for i := 0 to p.getlength - 1 do
    p[i] := a[i] / a0;                      // probability of reaction i
  dt := ln(1 / r1) / a0;                    // time of next reaction
  i := 0;
  q := 0;
  repeat                                    // find reaction to occur
    q := q + p[i];                          // prob of reactions up to i
    inc(i);
  until r2 < q;
      j := i - 1;                           // select reaction
  for i := 0 to SpeciesVector.getlength - 1 do
    if (limitIndex < 0) or isNaN(limit) or
       (i = limitIndex) or
       (i <> limitIndex) and (SpeciesVector[limitIndex] < limit) then
      SpeciesVector[i] := SpeciesVector[i] + StoichiometryMatrix[i, j];
  t := t + dt;
  FreeAndNil(a);
  FreeAndNil(p);
end;

procedure GillespieStep(const tmax: integer;
  const StoichiometryMatrix, ExpMatrix: TIntMatrix;
  const RateVector: TRealVector; var t: real; var SpeciesVector: TSpeciesVector);
{ simplified version without limit information }
begin
  GillespieStep(tmax, StoichiometryMatrix, ExpMatrix, RateVector, math.NaN, -1,
  t, SpeciesVector);
end;

procedure FinishGillespie(var ExpMatrix: TIntMatrix);
begin
  if assigned(ExpMatrix) then
    FreeAndNil(ExpMatrix);
end;

end.

{ References: }

{ Gillespie  DT.  A  general  method  for  numerically  simulating  the }
{ stochastic time evolution of coupled chemical reactions. J. Comput. Phys. }
{ 1976, 22:403–34 }

{ Gillespie DT. Exact stochastic simulation of coupled chemical reactions. }
{ Phys. Chem. 1977, 81, 25, 2340–2361. https://doi.org/10.1021/j100540a008 }

{ Gillespie DT. Stochastic simulation of chemical kinetics. Annu Rev Phys Chem. }
{ 2007;58:35-55. doi: 10.1146/annurev.physchem.58.032806.104637. PMID: 17037977. }

{ Jeongho Kim. Stochastic Simulation and Gillespie algorithm. Applied }
{ Mathematics Lab. 2018, }
{ https://appmath.wordpress.com/2018/03/21/stochastic-simulation-and-gillespie-algorithm/ }

{ Jeongho Kim. Gillespie algorithm. Mathematics Lab. 2018, }
{ https://appmath.wordpress.com/2018/03/21/gillespie-algorithm/ }
