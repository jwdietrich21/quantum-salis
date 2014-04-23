unit randshell;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ GUI shell for investigation of statistical distributions }

{ Version 1.0.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ComCtrls, Spin, ExtCtrls, Math, rDist;

type

  { TRandomShellForm }

  TRandomShellForm = class(TForm)
    ChisqButton: TButton;
    CountSpin: TSpinEdit;
    df1Label: TLabel;
    df1Spin: TSpinEdit;
    df2Label: TLabel;
    df2Spin: TSpinEdit;
    expButton: TButton;
    fButton: TButton;
    maxLabel: TLabel;
    maxSpin: TSpinEdit;
    meanLabel: TLabel;
    meanSpin: TSpinEdit;
    minLabel: TLabel;
    minSpin: TSpinEdit;
    ncpLabel: TLabel;
    ncpSpin: TSpinEdit;
    nLabel: TLabel;
    OutputMemo: TMemo;
    ParamPanel: TPanel;
    rateLabel: TLabel;
    rateSpin: TFloatSpinEdit;
    RNormButton: TButton;
    RUnifButton: TButton;
    sdLabel: TLabel;
    SDSpin: TSpinEdit;
    StatPascalVectorButton: TButton;
    StatusBar: TStatusBar;
    SVectorButton: TButton;
    tDistButton: TButton;
    ValuesGrid: TStringGrid;
    procedure expButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure meanSpinChange(Sender: TObject);
    procedure rateSpinChange(Sender: TObject);
    procedure RNormButtonClick(Sender: TObject);
    procedure RUnifButtonClick(Sender: TObject);
    procedure StatPascalVectorButtonClick(Sender: TObject);
    procedure SVectorButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  RandomShellForm: TRandomShellForm;

implementation

{$R *.lfm}

{ TRandomShellForm }

procedure DrawGridCaptions(theGrid: TStringGrid);
begin
  theGrid.Cells[0, 0] := '#';
  theGrid.Cells[1, 0] := 'Value';
end;

procedure TRandomShellForm.RUnifButtonClick(Sender: TObject);
var
  i, num: integer;
  min, max: integer;
begin
  min := minSpin.Value;
  max := maxSpin.Value;
  num := CountSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if min > max then
    MessageDlg('min > max', mtWarning, [mbOK], 0)
  else
  begin
    if num > 0 then
      for i := 1 to num do
      begin
        ValuesGrid.Cells[0, i] := IntToStr(i);
        ValuesGrid.Cells[1, i] := FloatToStr(min + random(max - min + 1));
      end;
  end;
end;

procedure TRandomShellForm.RNormButtonClick(Sender: TObject);
var
  i, num: integer;
  mean, sd: real;
begin
  mean := meanSpin.Value;
  sd := sdSpin.Value;
  num := CountSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if sd <= 0 then
    MessageDlg('SD <= 0', mtWarning, [mbOK], 0)
  else
  begin
    if num > 0 then
      for i := 1 to num do
      begin
        ValuesGrid.Cells[0, i] := IntToStr(i);
        ValuesGrid.Cells[1, i] := FloatToStr(randg(mean, sd));
      end;
  end;
end;


procedure TRandomShellForm.expButtonClick(Sender: TObject);
var
  i, num: integer;
  min, max: integer;
  rate: real;
begin
  num := CountSpin.Value;
  rate := rateSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if rate = 0 then
    MessageDlg('rate = 0', mtWarning, [mbOK], 0)
  else
  if num > 0 then
    for i := 1 to num do
    begin
      ValuesGrid.Cells[0, i] := IntToStr(i);
      ValuesGrid.Cells[1, i] := FloatToStr(randomExp(rate));
    end;
end;

procedure TRandomShellForm.FormCreate(Sender: TObject);
begin
  ValuesGrid.RowCount := 2;
  DrawGridCaptions(ValuesGrid);
end;

procedure TRandomShellForm.meanSpinChange(Sender: TObject);
begin
  if meanSpin.Value <> 0 then rateSpin.Value := 1 / meanSpin.Value;
end;

procedure TRandomShellForm.rateSpinChange(Sender: TObject);
begin
  if rateSpin.Value <> 0 then meanSpin.Value := 1 / rateSpin.Value;
end;

procedure TRandomShellForm.StatPascalVectorButtonClick(Sender: TObject);
var
  i, num: integer;
  resultString: string;
begin
  OutputMemo.Lines.Clear;
  resultString := 'combine(';
  num := ValuesGrid.RowCount;
  if num > 2 then
  begin
    for i := 1 to num - 3 do
    begin
      resultString := resultString + ValuesGrid.Cells[1, i] + ', ';
    end;
    resultString := resultString + ValuesGrid.Cells[1, num - 2];
  end;
  resultString := resultString + ');';
  OutputMemo.Lines.Add(resultString);
end;

procedure TRandomShellForm.SVectorButtonClick(Sender: TObject);
var
  i, num: integer;
  resultString: string;
begin
  OutputMemo.Lines.Clear;
  resultString := 'c(';
  num := ValuesGrid.RowCount;
  if num > 2 then
  begin
    for i := 1 to num - 3 do
    begin
      resultString := resultString + ValuesGrid.Cells[1, i] + ', ';
    end;
    resultString := resultString + ValuesGrid.Cells[1, num - 2];
  end;
  resultString := resultString + ')';
  OutputMemo.Lines.Add(resultString);
end;

end.
