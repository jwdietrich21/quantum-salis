unit GUI;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Handler for matrix definition window }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  LCLType, Types, PairSplitter, StdCtrls, ComCtrls, Menus, Math,
  qsFoundation, SSA, timeseries, Plot;

const
  downColour = TColor($F09A76);
  upColour = TColor($3C14DC);

type

  { TMainForm }

  TMainForm = class(TForm)
    AppleMenu: TMenuItem;
    LimitLabel: TLabel;
    LimitCombobox: TComboBox;
    BimolItem: TMenuItem;
    MimeItem: TMenuItem;
    UnimolItem: TMenuItem;
    PresetMenu: TMenuItem;
    VerboseCheckBox: TCheckBox;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    PairSplitter3: TPairSplitter;
    PairSplitterSide5: TPairSplitterSide;
    PairSplitterSide6: TPairSplitterSide;
    SMatrixGrid: TStringGrid;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PairSplitter2: TPairSplitter;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    ReactionMemo: TMemo;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    RedoMenuItem: TMenuItem;
    RunItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    SimulationMenu: TMenuItem;
    SpeciesGrid: TStringGrid;
    RateGrid: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure BimolItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Form(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AdaptMenus(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MimeItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure RateGridEnter(Sender: TObject);
    procedure RateGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure ReactionMemoChange(Sender: TObject);
    procedure RMatrixGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure RMatrixGridEnter(Sender: TObject);
    procedure RMatrixGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure RunItemClick(Sender: TObject);
    procedure SMatrixGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure SpeciesGridEditingDone(Sender: TObject);
    procedure SpeciesGridEnter(Sender: TObject);
    procedure SpeciesGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure SpeciesGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure UnimolItemClick(Sender: TObject);
    procedure VerboseCheckBoxChange(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure ShowAboutWindow(Sender: TObject);
    procedure SMatrixGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SMatrixGridEditingDone(Sender: TObject);
    procedure SMatrixGridExit(Sender: TObject);
    procedure SMatrixGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SMatrixGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SetHeaders(Sender: TObject);
    function ReactionNames(Sender: TObject): TNamesArray;
    function SpeciesNames(Sender: TObject): TNamesArray;
    procedure UpdateDisplay(Sender: TObject);
  private

  public
    ReactionNamesArray, SpeciesNamesArray: TNamesArray;
    InitVector, SpeciesVector: TSpeciesVector;
    StoichiometryMatrix, Exponents: TIntMatrix;
    RateVector: TRateVector;
    Verbose: boolean;
    SuggestedTime: String;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SuggestedTime := '1';
  RateGrid.RowCount := SMatrixGrid.ColCount;
  SpeciesGrid.RowCount := SMatrixGrid.RowCount;
  SetHeaders(Sender);
  StoichiometryMatrix := TIntMatrix.Create;
  RateVector := TRateVector.Create;
  InitVector := TSpeciesVector.Create;
  if VerboseCheckBox.Checked then
    Verbose := true
  else
    Verbose := false;
  AdaptMenus(Sender);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if assigned(StoichiometryMatrix) then
    FreeAndNil(StoichiometryMatrix);
  if assigned(InitVector) then
    FreeAndNil(InitVector);
  if assigned(RateVector) then
    FreeAndNil(RateVector);
end;

procedure TMainForm.Form(Sender: TObject);
begin

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if assigned(StoichiometryMatrix) then
    FreeAndNil(StoichiometryMatrix);
  if assigned(InitVector) then
    FreeAndNil(InitVector);
  if assigned(RateVector) then
    FreeAndNil(RateVector);
end;

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.RateGridEnter(Sender: TObject);
begin
  RateGrid.FixedCols := 1;
  RateGrid.FixedRows := 1;
  UpdateDisplay(Sender);
end;

procedure TMainForm.RateGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  RateGrid.FixedCols := 0;
  RateGrid.FixedRows := 0;
end;

procedure TMainForm.ReactionMemoChange(Sender: TObject);
begin

end;

procedure TMainForm.RMatrixGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  theCanvas: TCanvas;
  theNumber: integer;
begin
  theCanvas := TStringGrid(Sender).Canvas;
  theNumber := StrToIntDef(SMatrixGrid.Cells[aCol, aRow], 0);
  if (aCol > 0) and (aRow > 0) then
  begin
    if theNumber < 0 then
      theCanvas.Brush.Color := downColour
    else if theNumber > 0 then
      theCanvas.Brush.Color := upColour
    else
      theCanvas.Brush.Color := clDefault;
    theCanvas.Font.Color := clBlack;
    theCanvas.FillRect(aRect);
    theCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, TStringGrid(Sender).Cells[aCol, aRow]);
  end;
end;

procedure TMainForm.RMatrixGridEnter(Sender: TObject);
var
  aCol, aRow, theNumber: integer;
begin
  aCol := TStringGrid(Sender).Col;
  aRow := TStringGrid(Sender).Row;
  theNumber := StrToIntDef(SMatrixGrid.Cells[aCol, aRow], 0);
  if theNumber = 0 then
    MessageDlg('Warning', 'Corresponding entry in stoichiometry matrix is empty', mtWarning, [mbOK], 0);
end;

procedure TMainForm.RMatrixGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  UpdateDisplay(Sender);
end;

procedure TMainForm.RunItemClick(Sender: TObject);
var
  n, t: real;
  tmax, limitindex: integer;
  limits: TRealVector;
  timeString: String;
begin
  timeString := SuggestedTime;
  InputQuery('tmax', 'Please enter duration of simulated time (tmax):', timeString);
  limits := MaxMolecules(InitVector, StoichiometryMatrix);
  limitindex := LimitCombobox.ItemIndex - 1;
  if limitindex >= 0 then
    n := limits[limitindex]
  else
    n := -1;
  tmax := StrToIntDef(timeString, 0);
  if tmax > 0 then
  begin
    PlotWindow.SetSeries(SpeciesNamesArray);
    TimeSeriesForm.CleanGrid;
    t := 0;
    SpeciesVector := TSpeciesVector.Create;
    SpeciesVector.data := copy(InitVector.data, 0, InitVector.getlength);
    TimeSeriesForm.InsertVector(t, SpeciesVector);  // initial conditions
    InitGillespie(RateVector, StoichiometryMatrix, Exponents);
    while t < tmax do
    begin
      GillespieStep(tmax, StoichiometryMatrix, Exponents, RateVector, n, limitindex, t, SpeciesVector);
      TimeSeriesForm.InsertVector(t, SpeciesVector);
      PlotWindow.InsertPoint(t, SpeciesVector);
    end;
    FinishGillespie(Exponents);
    FreeAndNil(SpeciesVector);
  end;
  FreeAndNil(limits);
end;

procedure TMainForm.SMatrixGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  { for future extensions:
  SMatrixGrid.FixedCols := 0;
  SMatrixGrid.FixedRows := 0; }
end;

procedure TMainForm.SpeciesGridEditingDone(Sender: TObject);
begin
  UpdateDisplay(Sender);
end;

procedure TMainForm.SpeciesGridEnter(Sender: TObject);
begin
  SpeciesGrid.FixedCols := 1;
  SpeciesGrid.FixedRows := 1;
  UpdateDisplay(Sender);
end;

procedure TMainForm.SpeciesGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  SpeciesGrid.FixedCols := 0;
  SpeciesGrid.FixedRows := 0;
end;

procedure TMainForm.SpeciesGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  UpdateDisplay(Sender);
end;

procedure TMainForm.UnimolItemClick(Sender: TObject);
var
  i: integer;
begin
  SpeciesGrid.Clean;
  SMatrixGrid.Clean;
  RateGrid.Clean;
  SetHeaders(Sender);
  SpeciesGrid.Cells[0, 1] := 'A';           // Reactant name
  SpeciesGrid.Cells[0, 2] := '';
  SpeciesGrid.Cells[0, 3] := '';
  SpeciesGrid.Cells[0, 4] := '';
  SpeciesGrid.Cells[1, 1] := IntToStr(100); // Initial concentration
  SMatrixGrid.Cells[1, 1] := IntToStr(-1);  // Degradation
  for i := 0 to SMatrixGrid.Columns.Count - 1 do
    SMatrixGrid.Columns[i].Title.Caption := 'R' + IntToStr(i + 1);
  RateGrid.Cells[1, 1] := FloatToStr(0.1);  // Rate
  UpdateDisplay(Sender);
  SuggestedTime := '30';
end;

procedure TMainForm.BimolItemClick(Sender: TObject);
var
  i: integer;
begin
  SpeciesGrid.Clean;
  SMatrixGrid.Clean;
  RateGrid.Clean;
  SetHeaders(Sender);
  SpeciesGrid.Cells[0, 1] := 'A';           // Reactant names
  SpeciesGrid.Cells[0, 2] := 'B';
  SpeciesGrid.Cells[0, 3] := 'AB';          // Compound name
  SpeciesGrid.Cells[0, 4] := '';
  SpeciesGrid.Cells[1, 1] := IntToStr(100); // Initial concentrations
  SpeciesGrid.Cells[1, 2] := IntToStr(100);
  SpeciesGrid.Cells[1, 3] := IntToStr(0);
  SMatrixGrid.Cells[1, 1] := IntToStr(-1);  // Degradation
  SMatrixGrid.Cells[1, 2] := IntToStr(-1);
  SMatrixGrid.Cells[1, 3] := IntToStr(1);   // Formation
  SMatrixGrid.Cells[2, 1] := IntToStr(1);
  SMatrixGrid.Cells[2, 2] := IntToStr(1);
  SMatrixGrid.Cells[2, 3] := IntToStr(-1);
  for i := 0 to SMatrixGrid.Columns.Count - 1 do
    SMatrixGrid.Columns[i].Title.Caption := 'R' + IntToStr(i + 1);
  RateGrid.Cells[1, 1] := FloatToStr(0.1);  // Rates
  RateGrid.Cells[1, 2] := FloatToStr(0.1);
  UpdateDisplay(Sender);
  SuggestedTime := '1';
end;

procedure TMainForm.MimeItemClick(Sender: TObject);
begin
  SpeciesGrid.Clean;
  SMatrixGrid.Clean;
  RateGrid.Clean;
  SetHeaders(Sender);
  SpeciesGrid.Cells[0, 1] := 'A';                // Substrate name
  SpeciesGrid.Cells[0, 2] := 'E';                // Enzyme name
  SpeciesGrid.Cells[0, 3] := 'EA';               // Enzyme-substrate complex
  SpeciesGrid.Cells[0, 4] := 'P';                // Product name
  SMatrixGrid.Columns[0].Title.Caption := 'k1';  // Rate for E + A -> EA
  SMatrixGrid.Columns[1].Title.Caption := 'k-1'; // Rate for EA -> E + A
  SMatrixGrid.Columns[2].Title.Caption := 'k2';  // Rate for EA -> P
  SMatrixGrid.Columns[3].Title.Caption := '';
  SMatrixGrid.Columns[4].Title.Caption := '';
  SpeciesGrid.Cells[1, 1] := IntToStr(5000);     // Initial concentrations
  SpeciesGrid.Cells[1, 2] := IntToStr(1000);
  SpeciesGrid.Cells[1, 3] := IntToStr(150);
  SpeciesGrid.Cells[1, 4] := IntToStr(0);
  SMatrixGrid.Cells[1, 1] := IntToStr(-1);       // Degradation
  SMatrixGrid.Cells[1, 2] := IntToStr(-1);
  SMatrixGrid.Cells[1, 3] := IntToStr(1);        // Formation
  SMatrixGrid.Cells[1, 4] := IntToStr(0);
  SMatrixGrid.Cells[2, 1] := IntToStr(1);
  SMatrixGrid.Cells[2, 2] := IntToStr(1);
  SMatrixGrid.Cells[2, 3] := IntToStr(-1);
  SMatrixGrid.Cells[2, 4] := IntToStr(0);
  SMatrixGrid.Cells[3, 1] := IntToStr(0);
  SMatrixGrid.Cells[3, 2] := IntToStr(1);
  SMatrixGrid.Cells[3, 3] := IntToStr(-1);
  SMatrixGrid.Cells[3, 4] := IntToStr(1);
  RateGrid.Cells[1, 1] := FloatToStr(0.01);      // Rate k1
  RateGrid.Cells[1, 2] := FloatToStr(40);        // Rate k-1
  RateGrid.Cells[1, 3] := FloatToStr(10);        // Rate k2
  UpdateDisplay(Sender);
end;

procedure TMainForm.VerboseCheckBoxChange(Sender: TObject);
begin
  if VerboseCheckBox.checked then
    Verbose := true
  else
    Verbose := false;
  UpdateDisplay(Sender);
end;

procedure TMainForm.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TMainForm.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('SSA Shell, a stochastic simulator for cheminformatics' +
    LineEnding + LineEnding + 'Prerelease version 0.1');
end;

procedure TMainForm.SMatrixGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  theCanvas: TCanvas;
  theNumber: integer;
begin
  theCanvas := TStringGrid(Sender).Canvas;
  theNumber := StrToIntDef(TStringGrid(Sender).Cells[aCol, aRow], 0);
  if (aCol > 0) and (aRow > 0) then
  begin
    if theNumber < 0 then
      theCanvas.Brush.Color := downColour
    else if theNumber > 0 then
      theCanvas.Brush.Color := upColour
    else
      theCanvas.Brush.Color := clDefault;
    theCanvas.Font.Color := clBlack;
    theCanvas.FillRect(aRect);
    theCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, TStringGrid(Sender).Cells[aCol, aRow]);
  end;
  RateGrid.Invalidate;
end;

procedure TMainForm.AdaptMenus(Sender: TObject);
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  WinAboutItem.Visible := True;
  AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  RunItem.ShortCut  := ShortCut(VK_R, modifierKey);
end;

procedure TMainForm.SMatrixGridEditingDone(Sender: TObject);

begin
  SMatrixGrid.FixedCols := 1;
  SMatrixGrid.FixedRows := 1;
  UpdateDisplay(Sender);
end;

procedure TMainForm.SMatrixGridExit(Sender: TObject);
begin
  UpdateDisplay(Sender);
end;

procedure TMainForm.SMatrixGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateDisplay(Sender);
end;

procedure TMainForm.SMatrixGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  UpdateDisplay(Sender);
end;

procedure TMainForm.SetHeaders(Sender: TObject);
var
  i: integer;
begin
  SpeciesGrid.Cells[1, 0] := 'n';
  RateGrid.Cells[1, 0] := 'Rate';
  if not assigned(InitVector) then
    for i := 1 to SMatrixGrid.RowCount - 1 do
      SpeciesGrid.Cells[0, i] := chr(64 + i)
  else if ActiveControl = SpeciesGrid then
    for i := 1 to SMatrixGrid.RowCount - 1 do
      SMatrixGrid.Cells[0, i] := SpeciesGrid.Cells[0, i]
  else if ActiveControl = SMatrixGrid then
  begin
    for i := 1 to SMatrixGrid.RowCount - 1 do
      SpeciesGrid.Cells[0, i] := SMatrixGrid.Cells[0, i];
    for i := 1 to SMatrixGrid.ColCount - 1 do
      RateGrid.Cells[0, i] := SMatrixGrid.Columns[i - 1].Title.Caption;
  end
  else if ActiveControl = RateGrid then
  for i := 1 to SMatrixGrid.ColCount - 1 do
    SMatrixGrid.Columns[i - 1].Title.Caption := RateGrid.Cells[0, i];
  for i := 1 to SMatrixGrid.RowCount - 1 do
    SMatrixGrid.Cells[0, i] := SpeciesGrid.Cells[0, i];
  for i := 1 to SMatrixGrid.ColCount - 1 do
    RateGrid.Cells[0, i] := SMatrixGrid.Columns[i - 1].Title.Caption;
end;

function TMainForm.ReactionNames(Sender: TObject): TNamesArray;
var
  i, k: integer;
begin
  k := SMatrixGrid.ColCount - 1;
  SetLength(Result, k);
  for i := 0 to k - 1 do
    result[i] := SMatrixGrid.Columns[i].Title.Caption;
end;

function TMainForm.SpeciesNames(Sender: TObject): TNamesArray;
var
  i, k: integer;
begin
  k := SpeciesGrid.RowCount;
  SetLength(Result, k);
  for i := 1 to k - 1 do
    result[i - 1] := SpeciesGrid.Cells[0, i];
end;

procedure TMainForm.UpdateDisplay(Sender: TObject);
var
  i, j: integer;
  SpeciesMessage, ReactionsMessage, MatrixLine: String;
  SpeciesArray: TSpeciesArray;
begin
  RateGrid.RowCount := SMatrixGrid.ColCount;
  SpeciesGrid.RowCount := SMatrixGrid.RowCount;
  SetHeaders(Sender);
  ReactionMemo.Lines.Clear;
  if not assigned(StoichiometryMatrix) then
    StoichiometryMatrix := TIntMatrix.Create;
  if not assigned(RateVector) then
    RateVector := TRateVector.Create;
  if not assigned(InitVector) then
    InitVector := TSpeciesVector.Create;
  StoichiometryMatrix.InitZero(SMatrixGrid.RowCount - 1, SMatrixGrid.ColCount - 1);
  RateVector.InitZero(SMatrixGrid.ColCount - 1);
  InitVector.InitZero(SpeciesGrid.RowCount - 1);
  for i := 1 to SMatrixGrid.RowCount - 1 do
    for j := 1 to SMatrixGrid.ColCount - 1 do
      StoichiometryMatrix[i - 1, j - 1] := StrToIntDef(SMatrixGrid.Cells[j, i], 0);
  for i := 1 to RateGrid.RowCount - 1 do
    RateVector[i - 1] := StrToFloatDef(RateGrid.Cells[1, i], 0);
  for i := 1 to SpeciesGrid.RowCount - 1 do
    InitVector[i - 1] := StrToFloatDef(SpeciesGrid.Cells[1, i], 0);
  SpeciesNamesArray := SpeciesNames(Sender);
  ReactionNamesArray := ReactionNames(Sender);
  if Verbose then
  begin
    SpeciesMessage := 'Species: ';
    ReactionsMessage := 'Reactions: ';
    for i := 0 to length(SpeciesNamesArray) - 1 do
      SpeciesMessage := SpeciesMessage + SpeciesNamesArray[i] + ' ';
    for i := 0 to length(ReactionNamesArray) - 1 do
      ReactionsMessage := ReactionsMessage + ReactionNamesArray[i] + ' ';
    ReactionMemo.Lines.Add(SpeciesMessage);
    ReactionMemo.Lines.Add(ReactionsMessage);
    ReactionMemo.Lines.add('');
    ReactionMemo.Lines.add('Stoichiometry matrix:');
    for i := 1 to SMatrixGrid.RowCount - 1 do
      begin
        MatrixLine := '';
        for j := 1 to SMatrixGrid.ColCount - 1 do
          MatrixLine := MatrixLine + '   ' + IntToStr(StoichiometryMatrix[i - 1, j - 1]);
        ReactionMemo.Lines.add(MatrixLine);
      end;
    ReactionMemo.Lines.add('');
    ReactionMemo.Lines.add('Rate vector:');
    MatrixLine := '';
    for i := 1 to RateGrid.RowCount - 1 do
      begin
        MatrixLine := MatrixLine + '   ' + FloatToStr(RateVector[i - 1]);
       end;
    ReactionMemo.Lines.add(MatrixLine);
    ReactionMemo.Lines.Add('');
    MatrixLine := '';
    ReactionMemo.Lines.add('Initial values of species:');
    for i := 1 to InitVector.getlength do
      MatrixLine := MatrixLine + '   ' + FloatToStr(InitVector[i - 1]);
    ReactionMemo.Lines.Add(MatrixLine);
    ReactionMemo.Lines.Add('');
  end;
  for j := 1 to SMatrixGrid.ColCount - 1 do
    begin
      SpeciesArray := Species(StoichiometryMatrix, SpeciesNamesArray, j);
      ReactionMemo.Lines.Add(Reaction(SpeciesArray[0], SpeciesArray[1]));
    end;
  LimitCombobox.Items.Clear;
  LimitCombobox.Items.Add('None');
  for i := 0 to SpeciesGrid.RowCount - 1 do
    if SpeciesGrid.Cells[0, i] <> '' then
      LimitCombobox.Items.Add(SpeciesGrid.Cells[0, i]);
  if assigned(TimeSeriesForm) then
    TimeSeriesForm.SetCaptions(SpeciesNamesArray);
end;

end.

