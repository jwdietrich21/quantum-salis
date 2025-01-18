unit Plot;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Handler for plot window }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries, SSA;

const
  Colours: array[0..9] of TColor = (clRed, clGreen, clBlue, clAqua, clFuchsia,
  clMaroon, clLime, clPurple, clTeal, clPurple);

type

  { TPlotWindow }

  TPlotWindow = class(TForm)
    SChartLineSeries1: TLineSeries;
    TSChart: TChart;
    procedure FormCreate(Sender: TObject);
  private

  public
    FLine: array of TLineSeries;
    procedure SetSeries(SpeciesNames: TNamesArray);
    procedure InsertPoint(const time: real;
      const SpeciesVector: TSpeciesVector);
    procedure CleanPlot;
  end;

var
  PlotWindow: TPlotWindow;

implementation

{$R *.lfm}

{ TPlotWindow }

procedure TPlotWindow.FormCreate(Sender: TObject);
begin
  TSChart.ClearSeries;
  FreeAndNil(FLine);
end;

procedure TPlotWindow.SetSeries(SpeciesNames: TNamesArray);
var
  i, k: integer;
begin
  if assigned(FLine) then
    TSChart.ClearSeries;
  k := length(SpeciesNames);
  SetLength(Fline, k);
  for i := 0 to k - 1 do
    begin
      FLine[i] := TLineSeries.Create(TSChart);
      with FLine[i] do
      begin
        ShowLines := true;
        ShowPoints := false;
        ShowInLegend := true;
        Name := SpeciesNames[i];
        Title := Name;
        LinePen.Width := 2;
        if i <= 10 then
          SeriesColor := Colours[i]
        else
          SeriesColor := clBlack;
      end;
      TSChart.AddSeries(Fline[i]);
    end;
end;

procedure TPlotWindow.InsertPoint(const time: real;
  const SpeciesVector: TSpeciesVector);
var
  i: integer;
begin
  for i := 0 to SpeciesVector.getlength - 1 do
    FLine[i].addXY(time, SpeciesVector[i]);
end;

procedure TPlotWindow.CleanPlot;
var
  i: integer;
begin
  if assigned(FLine) then
    for i := 0 to length(FLine) - 1 do
      FLine[i].Clear;
end;

end.

