unit DescStat;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides algorithms for descriptive statistics }

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
  Classes, SysUtils, Math,
  qsFoundation;

type
  TExtArray = array of Extended;

function mean(const data: TExtVector): extended;
function stddev(const data: TExtVector): extended;
function variance(const data: TExtVector): extended;
function sem(const data: TExtArray): extended;
function sem(const data: TExtVector): extended;
function median(const data: TExtArray): extended;
function median(const data: TExtVector): extended;
function cv(const data: TExtArray): extended;
function cv(const data: TExtVector): extended;
function range(const data: TExtArray): TExtArray;
function range(const data: TExtVector): TExtVector;
function diff(const data: TExtArray): TExtArray;
function diff(const data: TExtVector): TExtVector;
function rms(const data: TExtArray): extended;
function rms(const data: TExtVector): extended;

implementation

function SortExtArray(const data: TExtArray): TExtArray;
{ Based on Shell Sort - avoiding recursion allows for sorting of very
  large arrays, too }
var
  data2: TExtArray;
  arrayLength, i, j, k: longint;
  h: extended;
begin
  arrayLength := high(data);
  data2 := copy(data, 0, arrayLength + 1);
  k := arrayLength div 2;
  while k > 0 do
  begin
    for i := 0 to arrayLength - k do
    begin
      j := i;
      while (j >= 0) and (data2[j] > data2[j + k]) do
      begin
        h := data2[j];
        data2[j] := data2[j + k];
        data2[j + k] := h;
        if j > k then
          dec(j, k)
        else
          j := 0;
      end;
    end;
    k := k div 2
  end;
  result := data2;
end;

function mean(const data: TExtVector): extended;
begin
  mean := math.mean(data.data);
end;

function stddev(const data: TExtVector): extended;
begin
  result := math.stddev(data.data);
end;

function variance(const data: TExtVector): extended;
begin
  result := math.variance(data.data);
end;

function sem(const data: TExtArray): extended;
{ calculates the standard error of the mean of a vector of extended }
  begin
    sem := math.stddev(data) / sqrt(length(data));
  end;

function sem(const data: TExtVector): extended;
begin
  result := sem(data.data);
end;

function median(const data: TExtArray): extended;
{ calculates the median (50% quantile) of a vector of extended }
var
  centralElement: integer;
  sortedData: TExtArray;
begin
  sortedData := SortExtArray(data);
  centralElement := length(sortedData) div 2;
  if odd(length(sortedData)) then
    result := sortedData[centralElement]
  else
    result := (sortedData[centralElement - 1] + sortedData[centralElement]) / 2;
end;

function median(const data: TExtVector): extended;
begin
  result := median(data.data);
end;

function cv(const data: TExtArray): extended;
{ calculates the coefficient of variation (CV or CoV) of a vector of extended }
begin
  result := math.stddev(data) / math.mean(data);
end;

function cv(const data: TExtVector): extended;
begin
  result := cv(data.data);
end;

function range(const data: TExtArray): TExtArray;
begin
  SetLength(result, 2);
  result[0] := MinValue(data);
  result[1] := MaxValue(data);
end;

function range(const data: TExtVector): TExtVector;
begin
  result := TExtVector.Create;
  result.data := range(data.data);
end;

function diff(const data: TExtArray): TExtArray;
var
  i, l: longint;
begin
  l := length(data);
  if l > 1 then
    begin
      SetLength(result, l - 1);
      for i := 0 to l - 2 do
        result[i] := data[i + 1] - data[i];
    end
  else
    begin
      SetLength(result, 1);
      result[0] := 0;
    end;
end;

function diff(const data: TExtVector): TExtVector;
begin
  result := TExtVector.Create;
  result.data := diff(data.data);
end;

function rms(const data: TExtArray): extended;
{ delivers the root mean square or second moment of data }
begin
  result := sqrt(SumOfSquares(data) / length(data));
end;

function rms(const data: TExtVector): extended;
begin
  result := rms(data.data);
end;


end.

