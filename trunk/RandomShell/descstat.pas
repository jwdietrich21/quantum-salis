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

function sem(const data: array of Extended): real;

implementation

function sem(const data: array of Extended): real;
  begin
    sem := stddev(data) / sqrt(length(data));
  end;

end.

