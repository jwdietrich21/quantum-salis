unit GUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  StdCtrls, ExtCtrls, PairSplitter, Spin, math;

type

  { TForm1 }

  TForm1 = class(TForm)
    aFloatSpinEdit: TFloatSpinEdit;
    bFloatSpinEdit: TFloatSpinEdit;
    aLabel: TLabel;
    bLabel: TLabel;
    GenerateButton: TButton;
    sdLabel: TLabel;
    sdFloatSpinEdit: TFloatSpinEdit;
    nLabel: TLabel;
    ResultMemo: TMemo;
    nSpinEdit: TSpinEdit;
    StatusBar1: TStatusBar;
    ValuesGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure nSpinEditChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.nSpinEditChange(Sender: TObject);
begin
  ValuesGrid.RowCount := nSpinEdit.Value + 2;
end;

procedure TForm1.GenerateButtonClick(Sender: TObject);
var
  i, max, x: integer;
  a, b, y, sd: real;
  aHat, bHat: real;
  yS: String;

  xVector, yVector: array of real;
  meanX, meanY, SAP, SAQx, SAQy, SAQyHat, Rsq: real;
begin
  max := nSpinEdit.Value;
  a := aFloatSpinEdit.Value;
  b := bFloatSpinEdit.Value;
  sd := sdFloatSpinEdit.Value;
  SAP := 0;
  SAQx := 0;
  SAQy := 0;
  SAQyHat := 0;
  SetLength(xVector, 0); // Clear
  SetLength(yVector, 0);
  SetLength(xVector, max);
  SetLength(yVector, max);

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
    ValuesGrid.Cells[1, i] := FormatFloat('###,###.00##', xVector[i - 1]); // x
    ValuesGrid.Cells[2, i] := FormatFloat('###,###.00##', yVector[i - 1]); // y
  end;

  meanX := mean(xVector);
  meanY := mean(yVector);
  for i := 1 to max do
  begin
    SAP := SAP + (xVector[i - 1] - meanX) * (yVector[i - 1] - meanY);
    SAQx := SAQx + sqr(xVector[i - 1] - meanX);
    SAQy := SAQy + sqr(yVector[i - 1] - meanX);
  end;
  bHat := SAP / SAQx;
  aHat := meanY - bHat * meanX;

  for i := 1 to max do
    SAQyHat := SAQyHat + sqr(aHat + bHat * xVector[i - 1] - meanY);

  Rsq := SAQyHat / SaQy;

  ResultMemo.Lines.Clear;
  ResultMemo.Lines.Add('Estimated results:');
  ResultMemo.Lines.Add('');
  ResultMemo.Lines.Add('n = ' + IntToStr(max));
  ResultMemo.Lines.Add('a = ' + FormatFloat('###,###.00##', aHat));
  ResultMemo.Lines.Add('b = ' + FormatFloat('###,###.00##', bHat));
  ResultMemo.Lines.Add('R^2 = ' + FormatFloat('###,###.00##', Rsq));

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ValuesGrid.RowCount := nSpinEdit.Value + 2;;
end;

end.

