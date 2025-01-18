unit RInterface;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Unit for envoking R commands from Lazarus / FPC }

{ Version 1.0.0 (Alpina) }

{ (c) J. W. Dietrich, 2008 - 2021 }
{ (c) Ruhr University of Bochum 2008 - 2021 }

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
  Classes, SysUtils, Process;

var
  RCommand: TProcessString;
  CmdHeader: array of TProcessString;

procedure SetPathToR(const FallBackPath: String);
function RunRCommand(const script: AnsiString; out RResult: AnsiString): boolean;
function RunRCommands(const script: array of AnsiString; out RResult: AnsiString): boolean;

implementation

procedure SetPathToR(const FallBackPath: String);
const
  WinFallBack = 'C:\Program Files\R\R-3.2.3\bin\x64\Rscript.exe';
  UnixFallBack = '/usr/local/bin/Rscript';
begin
  {$IFDEF Windows}
    SetLength(CmdHeader, 1);
    CmdHeader := ['Rscript.exe'];
    if not RunCommand('where', CmdHeader, RCommand, [poNoConsole], swoHIDE) then
    begin
      if FallBackPath = '' then
        RCommand := WinFallBack
      else
        RCommand := FallBackPath;
    end;
  {$ELSE}
  {$IFDEF UNIX}
    SetLength(CmdHeader, 1);
    CmdHeader := ['Rscript'];
    if not RunCommand('which', CmdHeader, RCommand, [poNoConsole], swoHIDE) then
    begin
       if FallBackPath = '' then
        RCommand := UnixFallBack
      else
        RCommand := FallBackPath;
   end;
  {$ENDIF}
  {$ENDIF}
end;

function RunRCommand(const script: AnsiString; out RResult: AnsiString): boolean;
var
  commandHeader: array of TProcessString;
begin
  SetLength(commandHeader, 2);
  commandHeader := ['-e', script];
  Result := RunCommand(RCommand, commandHeader, RResult, [poNoConsole], swoHIDE);
end;

function RunRCommands(const script: array of AnsiString; out RResult: AnsiString): boolean;
var
  i: integer;
  commandHeader: array of TProcessString;
begin
  SetLength(commandHeader, 2 * length(script));
  for i := 0 to length(script) - 1 do
  begin
    commandHeader[2*i] := '-e';
    commandHeader[2*i + 1] := script[i];
  end;
  Result := RunCommand(RCommand, commandHeader, RResult, [poNoConsole], swoHIDE);
end;

initialization
  SetPathToR('');

end.

