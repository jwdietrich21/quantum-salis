unit DescStat;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Unit with algorithms for statistical distributions }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type
  TExtArray = array of Extended;

function sem(const data: TExtArray): real;
function median(const data: TExtArray): real;

implementation

function SortExtArray(const data: TExtArray): TExtArray;
begin
end;

function sem(const data: TExtArray): real;
  begin
    sem := stddev(data) / sqrt(length(data));
  end;

function median(const data: TExtArray): real;
var
  centralElement: integer;
  sortedData: TExtArray;
begin
  SortExtArray(data);
  centralElement := length(sortedData) div 2;
  if odd(length(sortedData)) then
    result := sortedData[centralElement]
  else
    result := (sortedData[centralElement - 1] + sortedData[centralElement]) / 2;
end;

end.

