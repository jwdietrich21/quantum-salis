unit ranks;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides algorithms for rank calculations }

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
  Classes, SysUtils, Math, qsFoundation, perm;

type
  TNaNSorting = (First, last, remove, keep);
  TTiesMethod = (ascend, descend, average, random, min, max);

function rank(x: TLongintVector; NanSort: TNaNSorting = last;
  TiesMethod: TTiesMethod = average): TRealvector_data;

implementation

function rank(x: TLongintVector; NanSort: TNaNSorting;
  TiesMethod: TTiesMethod): TRealvector_data;
var
  i, j, k, n: integer;
  equalCount, lessCount: TIntvector_data;
  ties, permties: TLongintVector_data;
begin
  n := x.getlength;
  SetLength(lessCount, n);
  SetLength(equalCount, n);
  SetLength(Result, n);
  for i := 0 to n - 1 do
  begin
    equalCount[i] := 0;
    lessCount[i] := 0;
    for j := 0 to n - 1 do
      case TiesMethod of
        average, min, max, random:
          if x[j] = x[i] then
            Inc(equalCount[i])
          else if x[j] < x[i] then
            Inc(lessCount[i]);
        ascend:
          if (j <= i) and (x[j] = x[i]) then
            Inc(equalCount[i])
          else if x[j] < x[i] then
            Inc(lessCount[i]);
        descend:
          if (j >= i) and (x[j] = x[i]) then
            Inc(equalCount[i])
          else if x[j] < x[i] then
            Inc(lessCount[i]);
      end;
  end;
  for i := 0 to n - 1 do
    case TiesMethod of
      average:
        Result[i] := lessCount[i] + (equalCount[i] + 1) / 2;
      min:
        Result[i] := lessCount[i] + 1;
      max, ascend, descend:
        Result[i] := lessCount[i] + equalCount[i];
      random:
        begin
          SetLength(ties, 0); { reset }
          SetLength(ties, n);
          Result[i] := lessCount[i] + 1;
          if equalCount[i] > 1 then
            begin
              k := 0;
              for j := 0 to n - 1 do
              begin
                if x[j] = x[i] then
                  begin
                    ties[j] := trunc(result[j]) + k; { always integer here }
                    inc(k);
                  end;
                permties := sample(ties, 0, false);
                if permties[j] <> 0 then
                  Result[j] := permties[j];
              end;
            end;
        end;
    end;
end;

{ References:

1. Cooke D, Craven AH, Clarke GM. Statistical Computing in Pascal. Edward
   Arnold (Publishers), London 1985

2. Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language.
   Wadsworth & Brooks/Cole.

}

end.
