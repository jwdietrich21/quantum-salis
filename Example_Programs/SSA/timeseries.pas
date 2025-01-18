unit timeseries;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Grid for displaying simulated time series }

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

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls, SSA;

type

  { TTimeSeriesForm }

  TTimeSeriesForm = class(TForm)
    StatusBar1: TStatusBar;
    TimeSeriesGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private

  public
    LastLine: integer;
    procedure SetCaptions(SpeciesNames: TNamesArray);
    procedure InsertVector(const time: real; const SpeciesVector: TSpeciesVector);
    procedure CleanGrid;
  end;

var
  TimeSeriesForm: TTimeSeriesForm;

implementation

{$R *.lfm}

{ TTimeSeriesForm }

procedure TTimeSeriesForm.FormCreate(Sender: TObject);
begin
  LastLine := 0;
end;

procedure TTimeSeriesForm.SetCaptions(SpeciesNames: TNamesArray);
var
  i, k: integer;
begin
  if assigned(TimeSeriesGrid) then
  begin
    k := length(SpeciesNames);
    TimeSeriesGrid.ColCount := k;
    for i := 1 to k - 1 do
      TimeSeriesGrid.Cells[i, 0] := SpeciesNames[i];
  end;
end;

procedure TTimeSeriesForm.InsertVector(const time: real;
  const SpeciesVector: TSpeciesVector);
var
  i: integer;
begin
  inc(LastLine);
  if TimeSeriesGrid.RowCount < LastLine + 1 then
     TimeSeriesGrid.RowCount := LastLine + 1;
  if TimeSeriesGrid.ColCount < SpeciesVector.getlength then
    TimeSeriesGrid.ColCount := SpeciesVector.getlength;
  TimeSeriesGrid.Cells[0, LastLine] := FloatToStr(time);
  for i := 0 to SpeciesVector.getlength - 1 do
    TimeSeriesGrid.Cells[i + 1, LastLine] := FloatToStr(SpeciesVector[i]);
end;

procedure TTimeSeriesForm.CleanGrid;
begin
  TimeSeriesGrid.Clean([gzNormal, gzFixedRows]);
  LastLine := 0;
end;

end.

