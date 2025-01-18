program TestShell;

{ QUANTUM SALIS }

{ Statistical algorithms for life sciences }

{ Simple demo program for envoking R commands from Lazarus / FPC }

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GUI, RInterface
  { you can add units after this };

{$R *.res}

begin
  {$IFDEF Windows}
    SetPathToR('\\vs-chs01\users\dietrichj\Eigene Dateien\Eigene Programme\R\R-3.2.3\bin\x64\Rscript.exe');
  {$ELSE}
    SetPathToR('/usr/local/bin/Rscript');
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Title:='Laz2R';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end.

