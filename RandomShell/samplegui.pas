unit SampleGUI;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ GUI shell for investigation of samples and permutations }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, Spin,
  Menus, ExtCtrls, qsFoundation, perm, ranks;

type

  { TSampleForm }

  TSampleForm = class(TForm)
    CloseItem: TMenuItem;
    TiesCombo: TComboBox;
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    DeleteItem: TMenuItem;
    Divider_1_1: TMenuItem;
    Divider_1_2: TMenuItem;
    Divider_2_1: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    RankButton: TButton;
    GenerateMenu: TMenuItem;
    MainMenu1: TMainMenu;
    NewItem: TMenuItem;
    OpenItem: TMenuItem;
    PasteItem: TMenuItem;
    QuitItem: TMenuItem;
    RandomItem: TMenuItem;
    ReplaceCheckbox: TCheckBox;
    GenerateButton: TButton;
    maxLabel: TLabel;
    maxSpin: TSpinEdit;
    minLabel: TLabel;
    minSpin: TSpinEdit;
    SampleItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveItem: TMenuItem;
    UndoItem: TMenuItem;
    ValuesGrid: TStringGrid;
    procedure GenerateButtonClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure RandomItemClick(Sender: TObject);
    procedure RankButtonClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
  private

  public

  end;

var
  SampleForm: TSampleForm;

implementation

uses
  randshell;

{$R *.lfm}

{ TSampleForm }

procedure DrawGridCaptions(theGrid: TStringGrid);
begin
  theGrid.Cells[0, 0] := '#';
  theGrid.Cells[1, 0] := 'Value';
  if theGrid.ColCount > 2 then
    theGrid.Cells[2, 0] := 'Rank';
end;

procedure TSampleForm.GenerateButtonClick(Sender: TObject);
var
  i, num: integer;
  min, max: integer;
  Vector: array of longint;
  replace: boolean;
begin
  min := minSpin.Value;
  max := maxSpin.Value;
  replace := ReplaceCheckbox.Checked;
  num := 1 + max - min;
  if min >= max then
    MessageDlg('min >= max', mtWarning, [mbOK], 0)
  else
  begin
    ValuesGrid.Clear;
    ValuesGrid.RowCount := num + 1;
    DrawGridCaptions(ValuesGrid);
    Vector := sample(min, max, replace);
    if num > 0 then
      for i := 1 to num do
      begin
        ValuesGrid.Cells[0, i] := IntToStr(i);
        ValuesGrid.Cells[1, i] := IntToStr(Vector[i - 1]);
      end;
  end;
end;

procedure TSampleForm.QuitItemClick(Sender: TObject);
begin
  RandomShellForm.QuitItemClick(Sender);
end;

procedure TSampleForm.RandomItemClick(Sender: TObject);
begin
  RandomShellForm.BringToFront;
end;

procedure TSampleForm.RankButtonClick(Sender: TObject);
var
  i, r, c: integer;
  xvec: TLongintVector;
  rdata: TRealvector_data;
  TiesHandling: TTiesMethod;
  NaNSorting: TNaNSorting;
begin
  r := ValuesGrid.RowCount;
  c := ValuesGrid.ColCount;
  NaNSorting := keep;
  case TiesCombo.Caption of
  'Average':
    TiesHandling := average;
  'Ascend':
    TiesHandling := ascend;
  'Descend':
    TiesHandling := descend;
  'Min':
    TiesHandling := min;
  'Max':
    TiesHandling := max;
  'Random':
    TiesHandling := random;
  end;
  if r > 2 then
  begin
    if  c < 3 then
    begin
      ValuesGrid.ColCount := ValuesGrid.ColCount + 1;
      DrawGridCaptions(ValuesGrid);
    end;
    xvec := TLongintVector.Create;
    xvec.InitZero(r - 1);
    for i := 1 to r - 1 do
    begin
      xvec[i - 1] := StrToInt(ValuesGrid.Cells[1, i]);
    end;
    rdata := rank(xvec, NaNSorting, TiesHandling);
    xvec.Destroy;
    for i := 1 to r - 1 do
    begin
      ValuesGrid.Cells[2, i] := FloatToStr(rdata[i - 1]);
    end;
  end;
end;

procedure TSampleForm.SaveItemClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    if SaveDialog1.FilterIndex = 1 then
      ValuesGrid.SaveToCSVFile(SaveDialog1.FileName)
    else if SaveDialog1.FilterIndex = 2 then
      ValuesGrid.SaveToCSVFile(SaveDialog1.FileName, ';');
end;

end.

