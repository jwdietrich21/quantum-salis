unit GUI;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides a GUI of a testing shell for linear regression }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  StdCtrls, ExtCtrls, PairSplitter, Spin, Menus, math, LCLType,
  qsFoundation, lm;

type

  { TForm1 }

  TForm1 = class(TForm)
    aFloatSpinEdit: TFloatSpinEdit;
    bFloatSpinEdit: TFloatSpinEdit;
    aLabel: TLabel;
    bLabel: TLabel;
    CloseItem: TMenuItem;
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    DeleteItem: TMenuItem;
    Divider_1_1: TMenuItem;
    Divider_1_2: TMenuItem;
    Divider_2_1: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    GenerateButton: TButton;
    EvalButton: TButton;
    MainMenu1: TMainMenu;
    AppleMenu: TMenuItem;
    MacAboutItem: TMenuItem;
    HelpMenu: TMenuItem;
    Divider_2_2: TMenuItem;
    WinPreferencesItem: TMenuItem;
    WinAboutItem: TMenuItem;
    NewItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenItem: TMenuItem;
    PasteItem: TMenuItem;
    QuitItem: TMenuItem;
    SaveItem: TMenuItem;
    sdLabel: TLabel;
    sdFloatSpinEdit: TFloatSpinEdit;
    nLabel: TLabel;
    ResultMemo: TMemo;
    nSpinEdit: TSpinEdit;
    StatusBar1: TStatusBar;
    UndoItem: TMenuItem;
    ValuesGrid: TStringGrid;
    procedure EvalButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure nSpinEditChange(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure AdaptMenus;
    procedure WinAboutItemClick(Sender: TObject);
  private

  public
    model: TLinMod;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.nSpinEditChange(Sender: TObject);
begin
  ValuesGrid.RowCount := nSpinEdit.Value + 1;
end;

procedure TForm1.OpenItemClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      ValuesGrid.RowCount := 1;
      case OpenDialog1.FilterIndex of
      1:
        ValuesGrid.LoadFromCSVFile(OpenDialog1.FileName);
      2:
        ValuesGrid.LoadFromCSVFile(OpenDialog1.FileName, ';');
      end;
    end;
end;

procedure TForm1.QuitItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TForm1.GenerateButtonClick(Sender: TObject);
var
  i, max, x: integer;
  a, b, y, sd: real;
  xVector, yVector: TExtVector;
begin
  max := nSpinEdit.Value;
  a := aFloatSpinEdit.Value;
  b := bFloatSpinEdit.Value;
  sd := sdFloatSpinEdit.Value;

  xVector := TExtVector.Create;
  yVector := TExtVector.Create;

  xVector.InitZero(max);
  yVector.InitZero(max);

  if max > 0 then
  for i := 1 to max do
  begin
    x := i;
    y := randg(a + b * x, sd);
    xVector[i - 1] := x;
    yVector[i - 1] := y;
  end;

  ValuesGrid.Clean;
  for i := 1 to max do
  begin
    ValuesGrid.Cells[0, i] := IntToStr(i);
    ValuesGrid.Cells[1, i] := FormatFloat('###,###.00##', xVector[i - 1]); // x
    ValuesGrid.Cells[2, i] := FormatFloat('###,###.00##', yVector[i - 1]); // y
  end;

  xVector.Destroy;
  yVector.Destroy;
end;

procedure TForm1.MacAboutItemClick(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  AdaptMenus;
  ValuesGrid.RowCount := nSpinEdit.Value + 1;
  for i := 1 to ValuesGrid.RowCount - 1 do
    ValuesGrid.Cells[0, i] := IntToStr(i);
end;

procedure TForm1.EvalButtonClick(Sender: TObject);
var
  i, max: integer;
  xVector, yVector: TExtVector;
begin
  max := ValuesGrid.RowCount;

  xVector := TExtVector.Create;
  yVector := TExtVector.Create;
  xVector.InitZero(max  - 1);
  yVector.InitZero(max - 1);

  for i := 1 to max - 1 do
  begin
    if ValuesGrid.Cells[1, i] <> '' then
      xVector[i - 1] := StrToFloatDef(ValuesGrid.Cells[1, i], NaN)
    else
      xVector[i - 1] := NaN;
    if ValuesGrid.Cells[2, i] <> '' then
      yVector[i - 1] := StrToFloatDef(ValuesGrid.Cells[2, i], NaN)
    else
      yVector[i - 1] := NaN;
  end;

  model := TLinMod.Create;
  model.x := xVector;
  model.y := yVector;

  ResultMemo.Lines.Clear;
  ResultMemo.Lines.Add('Estimated results:');
  ResultMemo.Lines.Add('');
  ResultMemo.Lines.Add('n = ' + IntToStr(model.n));
  ResultMemo.Lines.Add('a = ' + FormatFloat('###,###.00##', model.a) +
                  ' (SE = ' + FormatFloat('###,###.00##', model.sea) + ')');
  ResultMemo.Lines.Add('b = ' + FormatFloat('###,###.00##', model.b) +
                  ' (SE = ' + FormatFloat('###,###.00##', model.seb) + ')');
  ResultMemo.Lines.Add('R^2 = ' + FormatFloat('###,###.00##', model.rsq));

  model.Destroy;
  xVector.Destroy;
  yVector.Destroy;
end;

procedure TForm1.AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  Divider_2_2.Visible := False;
  WinPreferencesItem.Visible := False;
  AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  Divider_2_2.Visible := False;
  WinPreferencesItem.Visible := False;
  AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  WinAboutItem.Visible := True;
  Divider_2_2.Visible := True;
  WinPreferencesItem.Visible := True;
  AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  NewItem.ShortCut := ShortCut(VK_N, modifierKey);
  OpenItem.ShortCut := ShortCut(VK_O, modifierKey);
  CloseItem.ShortCut := ShortCut(VK_W, modifierKey);
  SaveItem.ShortCut := ShortCut(VK_S, modifierKey);
  //PrintItem.ShortCut := ShortCut(VK_P, modifierKey);
  QuitItem.ShortCut := ShortCut(VK_Q, modifierKey);
  UndoItem.ShortCut := ShortCut(VK_Z, modifierKey);
  CutItem.ShortCut := ShortCut(VK_X, modifierKey);
  CopyItem.ShortCut := ShortCut(VK_C, modifierKey);
  PasteItem.ShortCut := ShortCut(VK_V, modifierKey);
  //SelectAllItem.ShortCut := ShortCut(VK_A, modifierKey);
end;

procedure TForm1.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;


end.

