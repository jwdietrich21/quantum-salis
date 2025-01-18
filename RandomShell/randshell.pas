unit randshell;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ GUI shell for investigation of statistical distributions }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ComCtrls, Spin, ExtCtrls, Menus, Math, LCLType,
  qsFoundation, rDist, DescStat, SampleGUI;

const
  STANDARD_LENGTH = 13;

type

  { TRandomShellForm }

  TRandomShellForm = class(TForm)
    DNormButton: TButton;
    AppleMenu: TMenuItem;
    AppleAboutItem: TMenuItem;
    HelpMenu: TMenuItem;
    WinAboutItem: TMenuItem;
    RemoveButton: TButton;
    bSpin: TFloatSpinEdit;
    AddButton: TButton;
    PTButton: TButton;
    ExplanationLabelDensity: TLabel;
    GenerateMenu: TMenuItem;
    SampleItem: TMenuItem;
    RandomItem: TMenuItem;
    ExplanationLabel: TLabel;
    ExplanationLabelCDF: TLabel;
    PoissonButton: TButton;
    cSpin: TFloatSpinEdit;
    bLabel: TLabel;
    cLabel: TLabel;
    kLabel: TLabel;
    DescStatButton: TButton;
    PNormButton: TButton;
    ClearButton: TButton;
    SaveDialog1: TSaveDialog;
    DTButton: TButton;
    vLabel: TLabel;
    wLabel: TLabel;
    kSpin: TSpinEdit;
    ErlangButton: TButton;
    GammaButton: TButton;
    ChisqButton: TButton;
    CountSpin: TSpinEdit;
    df1Label: TLabel;
    df1Spin: TSpinEdit;
    df2Label: TLabel;
    df2Spin: TSpinEdit;
    expButton: TButton;
    fButton: TButton;
    vSpin: TSpinEdit;
    MainMenu1: TMainMenu;
    maxLabel: TLabel;
    maxSpin: TSpinEdit;
    meanLabel: TLabel;
    meanSpin: TSpinEdit;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    aLabel: TLabel;
    aSpin: TFloatSpinEdit;
    UndoItem: TMenuItem;
    Divider_2_1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    DeleteItem: TMenuItem;
    PasteItem: TMenuItem;
    NewItem: TMenuItem;
    OpenItem: TMenuItem;
    Divider_1_1: TMenuItem;
    CloseItem: TMenuItem;
    SaveItem: TMenuItem;
    QuitItem: TMenuItem;
    Divider_1_2: TMenuItem;
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
    wSpin: TSpinEdit;
    procedure AdaptForPlatform;
    procedure AddButtonClick(Sender: TObject);
    procedure AppleAboutItemClick(Sender: TObject);
    procedure ChisqButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure DescStatButtonClick(Sender: TObject);
    procedure DNormButtonClick(Sender: TObject);
    procedure DTButtonClick(Sender: TObject);
    procedure ErlangButtonClick(Sender: TObject);
    procedure expButtonClick(Sender: TObject);
    procedure fButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GammaButtonClick(Sender: TObject);
    procedure meanSpinChange(Sender: TObject);
    procedure PNormButtonClick(Sender: TObject);
    procedure PoissonButtonClick(Sender: TObject);
    procedure PTButtonClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure RandomItemClick(Sender: TObject);
    procedure rateSpinChange(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure RNormButtonClick(Sender: TObject);
    procedure RUnifButtonClick(Sender: TObject);
    procedure SampleItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure StatPascalVectorButtonClick(Sender: TObject);
    procedure SVectorButtonClick(Sender: TObject);
    procedure tDistButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
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
{ Creates a uniform random number distribution }
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

procedure TRandomShellForm.SampleItemClick(Sender: TObject);
begin
  SampleForm.Show;
end;

procedure TRandomShellForm.SaveItemClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    OutputMemo.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TRandomShellForm.RNormButtonClick(Sender: TObject);
{ Creates a normal (Gaussian) random number distribution }
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
{ Creates an exponential random number distribution }
var
  i, num: integer;
  a, rate: real;
begin
  num := CountSpin.Value;
  a := aSpin.Value;
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
      ValuesGrid.Cells[1, i] := FloatToStr(randomExp(a, rate));
    end;
end;

procedure TRandomShellForm.ChisqButtonClick(Sender: TObject);
{ Creates a chi square distribution }
var
  i, num: integer;
  df: integer;
begin
  num := CountSpin.Value;
  df := df1Spin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if df < 1 then
    MessageDlg('df < 1', mtWarning, [mbOK], 0)
  else
  if num > 0 then
    for i := 1 to num do
    begin
      ValuesGrid.Cells[0, i] := IntToStr(i);
      ValuesGrid.Cells[1, i] := FloatToStr(randomChisq(df));
    end;
end;

procedure TRandomShellForm.AdaptForPlatform;
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
  NewItem.ShortCut := ShortCut(VK_N, modifierKey);
  OpenItem.ShortCut := ShortCut(VK_O, modifierKey);
  CloseItem.ShortCut := ShortCut(VK_W, modifierKey);
  SaveItem.ShortCut := ShortCut(VK_S, modifierKey);
  QuitItem.ShortCut := ShortCut(VK_Q, modifierKey);
  UndoItem.ShortCut := ShortCut(VK_Z, modifierKey);
  CutItem.ShortCut := ShortCut(VK_X, modifierKey);
  CopyItem.ShortCut := ShortCut(VK_C, modifierKey);
  PasteItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TRandomShellForm.AddButtonClick(Sender: TObject);
begin
  ValuesGrid.RowCount := ValuesGrid.RowCount + 1;
end;

procedure TRandomShellForm.AppleAboutItemClick(Sender: TObject);
begin
  WinAboutItemClick(Sender);
end;

procedure TRandomShellForm.ClearButtonClick(Sender: TObject);
var
  num: integer;
begin
  num := CountSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  OutputMemo.Lines.Clear;
end;

procedure TRandomShellForm.CloseItemClick(Sender: TObject);
begin
  QuitItemClick(Sender);
end;

procedure TRandomShellForm.ErlangButtonClick(Sender: TObject);
{ Creates variable from an Erlang distribution }
var
  i, num: integer;
  k: integer;
  mean: real;
begin
  num := CountSpin.Value;
  k := kSpin.Value;
  mean := meanSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if k < 1 then
    MessageDlg('k < 1', mtWarning, [mbOK], 0)
  else if mean <= 0 then
    MessageDlg('mean <= 0', mtWarning, [mbOK], 0)
  else
  if num > 0 then
    for i := 1 to num do
    begin
      ValuesGrid.Cells[0, i] := IntToStr(i);
      ValuesGrid.Cells[1, i] := FloatToStr(randomErlang(mean, k));
    end;
end;

procedure TRandomShellForm.fButtonClick(Sender: TObject);
{ Creates an F distribution }
var
  i, num: integer;
  v, w: integer;
begin
  num := CountSpin.Value;
  v := vSpin.Value;
  w := wSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if v < 1 then
    MessageDlg('v < 1', mtWarning, [mbOK], 0)
  else if w < 1 then
    MessageDlg('w < 1', mtWarning, [mbOK], 0)
  else
  if num > 0 then
    for i := 1 to num do
    begin
      ValuesGrid.Cells[0, i] := IntToStr(i);
      ValuesGrid.Cells[1, i] := FloatToStr(randomF(v, w));
    end;
end;

procedure TRandomShellForm.FormCreate(Sender: TObject);
begin
  AdaptForPlatform;
  ValuesGrid.RowCount := 2;
  DrawGridCaptions(ValuesGrid);
end;

procedure TRandomShellForm.GammaButtonClick(Sender: TObject);
{ Creates a Gamma distribution }
var
  i, num: integer;
  a, b, c: real;
begin
  num := CountSpin.Value;
  a := aSpin.Value;
  b := bSpin.Value;
  c := cSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if b <= 0 then
    MessageDlg('b <= 0', mtWarning, [mbOK], 0)
  else
  if c <= 0 then
    MessageDlg('c <= 0', mtWarning, [mbOK], 0)
  else
  begin
    for i := 1 to num do
    begin
      ValuesGrid.Cells[0, i] := IntToStr(i);
      ValuesGrid.Cells[1, i] := FloatToStr(randomGamma(a, b, c));
    end;
  end;
end;

procedure TRandomShellForm.tDistButtonClick(Sender: TObject);
{ Creates a t distribution }
var
  i, num: integer;
  df: integer;
begin
  num := CountSpin.Value;
  df := df1Spin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if df < 2 then
    MessageDlg('df < 2', mtWarning, [mbOK], 0)
  else
  if num > 0 then
    for i := 1 to num do
    begin
      ValuesGrid.Cells[0, i] := IntToStr(i);
      ValuesGrid.Cells[1, i] := FloatToStr(randomT(df));
    end;
end;

procedure TRandomShellForm.WinAboutItemClick(Sender: TObject);
begin
  ShowMessage('RandomShell 1.0, © 1994-2025 J. W. Dietrich and others');
end;

procedure TRandomShellForm.meanSpinChange(Sender: TObject);
begin
  if meanSpin.Value <> 0 then rateSpin.Value := 1 / meanSpin.Value;
end;

procedure TRandomShellForm.PNormButtonClick(Sender: TObject);
var
  i, count: integer;
  num, res: real;
  resultString: string;
  mean, sd: extended;
begin
  if (ValuesGrid.RowCount = 0) or (ValuesGrid.Cells[1, 1] = '') then
  begin
    MessageDlg('List empty', mtWarning, [mbOK], 0);
    exit;
  end;
  mean := meanSpin.Value;
  sd := sdSpin.Value;
  OutputMemo.Lines.Clear;
  resultString := '';
  count := ValuesGrid.RowCount;
  if count > 2 then
  begin
    for i := 1 to count - 2 do
    begin
      if ValuesGrid.Cells[1, i] <> '' then
        num := StrToFloat(ValuesGrid.Cells[1, i])
      else
        num := math.NaN;
      res := probGaussian(num, mean, sd);
      ResultString := ResultString + FloatToStr(res) + LineEnding;
    end;
  end;
  OutputMemo.Lines.Add(resultString);
end;

procedure TRandomShellForm.PoissonButtonClick(Sender: TObject);
{ creates variables from a Poisson distribution }
var
  i, num: integer;
  mean: real;
begin
  mean := meanSpin.Value;
  num := CountSpin.Value;
  ValuesGrid.Clear;
  ValuesGrid.RowCount := num + 2;
  DrawGridCaptions(ValuesGrid);
  if mean < 1 then
    MessageDlg('mean < 1', mtWarning, [mbOK], 0)
  else
  begin
    if num > 0 then
      for i := 1 to num do
      begin
        ValuesGrid.Cells[0, i] := IntToStr(i);
        ValuesGrid.Cells[1, i] := FloatToStr(randomPoisson(trunc(mean)));
      end;
  end;
end;

procedure TRandomShellForm.PTButtonClick(Sender: TObject);
var
  i, count: integer;
  num, res: real;
  resultString: string;
  df: extended;
begin
  if (ValuesGrid.RowCount = 0) or (ValuesGrid.Cells[1, 1] = '') then
  begin
    MessageDlg('List empty', mtWarning, [mbOK], 0);
    exit;
  end;
  df := df1Spin.Value;
  OutputMemo.Lines.Clear;
  resultString := '';
  count := ValuesGrid.RowCount;
  if df < 2 then
    MessageDlg('df < 2', mtWarning, [mbOK], 0)
  else if count > 2 then
  begin
    for i := 1 to count - 3 do
    begin
      if ValuesGrid.Cells[1, i] <> '' then
        num := StrToFloat(ValuesGrid.Cells[1, i])
      else
        num := math.NaN;
      res := ProbT(num, df);
      ResultString := ResultString + FloatToStr(res) + LineEnding;
    end;
  end;
  OutputMemo.Lines.Add(resultString);
end;

procedure TRandomShellForm.QuitItemClick(Sender: TObject);
begin
  application.terminate;
end;

procedure TRandomShellForm.RandomItemClick(Sender: TObject);
begin
  RandomShellForm.BringToFront;
end;

procedure TRandomShellForm.rateSpinChange(Sender: TObject);
begin
  if rateSpin.Value <> 0 then meanSpin.Value := 1 / rateSpin.Value;
end;

procedure TRandomShellForm.RemoveButtonClick(Sender: TObject);
begin
  if ValuesGrid.RowCount > 2 then
    ValuesGrid.RowCount := ValuesGrid.RowCount - 1;
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
      if ValuesGrid.Cells[1, i] <> '' then
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
      if ValuesGrid.Cells[1, i] <> '' then
        resultString := resultString + ValuesGrid.Cells[1, i] + ', ';
    end;
    resultString := resultString + ValuesGrid.Cells[1, num - 2];
  end;
  resultString := resultString + ')';
  OutputMemo.Lines.Add(resultString);
end;

procedure TRandomShellForm.DescStatButtonClick(Sender: TObject);
var
  i, num: integer;
  theData: TExtVector;
  resultString: string;
begin
  OutputMemo.Lines.Clear;
  resultString := '';
  theData := TExtVector.Create;
  num := ValuesGrid.RowCount;
  theData.InitZero(num - 2);
  if num > 2 then
  begin
    for i := 1 to num - 2 do
    begin
      if ValuesGrid.Cells[1, i] <> '' then
        theData[i - 1] := StrToFloat(ValuesGrid.Cells[1, i]);
    end;
    resultString := resultString + 'Range: ' + FloatToStr(range(theData)[0])
                                 + ' .. ' + FloatToStr(range(theData)[1])
                                 + LineEnding; // vector object form
    resultString := resultString + 'Span: ' +  FloatToStr(diff(range(theData))[0])
                                 + LineEnding;
    resultString := resultString + 'Mean: ' + FloatToStr(mean(theData))
                                 + LineEnding; // array form
    resultString := resultString + 'Median: ' + FloatToStr(median(theData))
                                 + LineEnding; // vector object form
    resultString := resultString + 'Standard Deviation: '
                                 + FloatToStr(stddev(theData))
                                 + LineEnding; // array form
    resultString := resultString + 'Variance: '
                                 + FloatToStr(variance(theData))
                                 + LineEnding; // array form
    resultString := resultSTring + 'CoV: ' + FloatToStr(cv(theData))
                                 + LineEnding; // vector object form
    resultString := resultString + 'SEM: ' + FloatToStr(sem(theData))
                                 + LineEnding; // vector object form
    resultString := resultString + 'RMS: ' + FloatToStr(rms(theData))
                                 + LineEnding; // vector object form
    OutputMemo.Lines.Add(resultString);
  end;
  theData.Destroy;
end;

procedure TRandomShellForm.DNormButtonClick(Sender: TObject);
var
  i, count: integer;
  theData: TExtVector;
  num, res, mu, sigma: real;
  resultString: string;
begin
  if (ValuesGrid.RowCount = 0) or (ValuesGrid.Cells[1, 1] = '') then
  begin
    MessageDlg('List empty', mtWarning, [mbOK], 0);
    exit;
  end;
  theData := TExtVector.Create;
  OutputMemo.Lines.Clear;
  resultString := '';
  count := ValuesGrid.RowCount;
  theData.InitZero(count - 1);
  if count > 2 then
  begin
    for i := 1 to count - 2 do
    begin
      if ValuesGrid.Cells[1, i] <> '' then
        theData[i - 1] := StrToFloat(ValuesGrid.Cells[1, i])
      else
        theData[i - 1] := math.NaN;
    end;
    mu := mean(theData);
    sigma := stddev(theData);
    for i := 1 to count - 2 do
    begin
      if ValuesGrid.Cells[1, i] <> '' then
        num := StrToFloat(ValuesGrid.Cells[1, i])
      else
        num := math.NaN;
      res := AGaussian(num, mu, sigma);
      ResultString := ResultString + FloatToStr(res) + LineEnding;
    end;
  end;
  OutputMemo.Lines.Add(resultString);
  theData.Destroy;
end;

procedure TRandomShellForm.DTButtonClick(Sender: TObject);
var
  i, count: integer;
  num, res: real;
  resultString: string;
  df: extended;
begin
  if (ValuesGrid.RowCount = 0) or (ValuesGrid.Cells[1, 1] = '') then
  begin
    MessageDlg('List empty', mtWarning, [mbOK], 0);
    exit;
  end;
  df := df1Spin.Value;
  OutputMemo.Lines.Clear;
  resultString := '';
  count := ValuesGrid.RowCount;
  if df < 2 then
    MessageDlg('df < 2', mtWarning, [mbOK], 0)
  else if count > 2 then
  begin
    for i := 1 to count - 3 do
    begin
      if ValuesGrid.Cells[1, i] <> '' then
        num := StrToFloat(ValuesGrid.Cells[1, i])
      else
        num := math.NaN;
      res := DensT(num, df);
      ResultString := ResultString + FloatToStr(res) + LineEnding;
    end;
  end;
  OutputMemo.Lines.Add(resultString);
end;


end.
