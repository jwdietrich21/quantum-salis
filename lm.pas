unit lm;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ This unit provides code for linear models }

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
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, math, qsFoundation;

type

{ TLinMod }

TLinMod = class
  private
    meanX, meanY, SAP, SAQx, SAQy, SAQyHat: extended;
    MSE, SSE, RSE: extended;
    aHat, bHat: extended;
    Fx, Fy: TExtVector;
    function GetN: longint;
    function GetA: extended;
    function GetB: extended;
    function GetSEA: extended;
    function GetSEB: extended;
    function GetRsq: extended;
    procedure SetBasicMoments;
    procedure SetX(AVector: TExtVector);
    procedure SetY(AVector: TExtVector);
  public
    constructor Create;
    destructor Destroy; override;
    property x: TExtVector write SetX;
    property y: TExtVector write SetY;
    property n: longint read GetN;
    property a: extended read GetA;
    property b: extended read GetB;
    property sea: extended read GetSEA;
    property seb: extended read GetSEB;
    property rsq: extended read GetRsq;
  end;

implementation

{ TLinMod }

function TLinMod.GetN: longint;
begin
  result := Fx.getlength;
end;

function TLinMod.GetA: extended;
begin
  result := aHat;
end;

function TLinMod.GetB: extended;
begin
  result := bHat;
end;

function TLinMod.GetSEA: extended;
begin
  result := RSE * sqrt(1 / n + sqr(meanX) / SAQx);
end;

function TLinMod.GetSEB: extended;
begin
  result := sqrt(MSE) / sqrt(SAQx);
end;

function TLinMod.GetRsq: extended;
begin
  result := SAQyHat / SaQy;
end;

procedure TLinMod.SetBasicMoments;
var
  i, max: integer;
begin
  if (Fx.getlength > 0) and (Fy.getlength > 0) then
  begin
    max := Fx.getlength;
    meanX := mean(Fx.data);
    meanY := mean(Fy.data);
    SAP := 0;
    SAQx := 0;
    SAQy := 0;
    for i := 0 to max - 1 do
    begin
      SAP := SAP + (Fx[i] - meanX) * (Fy[i] - meanY);
      SAQx := SAQx + sqr(Fx[i] - meanX);
      SAQy := SAQy + sqr(Fy[i] - meanY);
    end;
    bHat := SAP / SAQx;
    aHat := meanY - bHat * meanX;
    SAQyHat := 0;
    SSE := 0;
    for i := 0 to max - 1 do
    begin
      SAQyHat := SAQyHat + sqr(aHat + bHat * Fx[i] - meanY);
      SSE := SSE + sqr(Fy[i] - (aHat + bHat * Fx[i]));
    end;
    MSE := SSE / (n - 2);
    RSE := sqrt(MSE);
  end;
end;

procedure TLinMod.SetX(AVector: TExtVector);
begin
  Fx.data := AVector.data;
  SetBasicMoments;
end;

procedure TLinMod.SetY(AVector: TExtVector);
begin
  Fy.data := AVector.data;
  SetBasicMoments;
end;

constructor TLinMod.Create;
begin
  inherited Create;
  Fx := TExtVector.Create;
  Fy := TExtVector.Create;
  Fx.InitZero(0);
  Fy.InitZero(0);
  SAP := 0;
  SAQx := 0;
  SAQy := 0;
  SAQyHat := 0;
end;

destructor TLinMod.Destroy;
begin
  if assigned(Fx) then
    Fx.Destroy;
  if assigned(Fy) then
    Fy.Destroy;
  inherited Destroy;
end;

end.

