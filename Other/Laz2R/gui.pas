unit GUI;

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

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, LCLType, RInterface;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    AppleAboutItem: TMenuItem;
    AppleMenu: TMenuItem;
    CloseItem: TMenuItem;
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    DeleteItem: TMenuItem;
    Divider_1_1: TMenuItem;
    Divider_1_2: TMenuItem;
    Divider_2_1: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    MainMenu1: TMainMenu;
    NewItem: TMenuItem;
    OpenItem: TMenuItem;
    PasteItem: TMenuItem;
    QuitItem: TMenuItem;
    RunButton: TButton;
    CommandMemo: TMemo;
    CommandMemoLabel: TLabel;
    ResultMemolabel: TLabel;
    OutputMemo: TMemo;
    SaveItem: TMenuItem;
    Splitter1: TSplitter;
    UndoItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure AdaptForPlatform;
    procedure AppleAboutItemClick(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure CommandMemoExec(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private

  public

  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.CommandMemoExec(Sender: TObject);
var
  i: integer;
  commands: TStrings;
  command: array of AnsiString;
  ROutput: AnsiString;
  status: boolean;
begin
  OutputMemo.Lines.clear;
  commands := CommandMemo.Lines;
  SetLength(command, commands.Count);
  for i := 0 to commands.Count - 1 do
    command[i] := commands.Strings[i];
  status := RunRCommands(command, ROutput);
  if status = true then
    OutputMemo.Lines.Add(ROutput);
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  AdaptForPlatform;
end;

procedure TMainWindow.QuitItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainWindow.AdaptForPlatform;
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

procedure TMainWindow.AppleAboutItemClick(Sender: TObject);
begin
  ShowMessage('Laz2R 1.0, © 2008-2021 J. W. Dietrich');
end;

procedure TMainWindow.CloseItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainWindow.RunButtonClick(Sender: TObject);
begin
  CommandMemoExec(Sender)
end;

procedure TMainWindow.WinAboutItemClick(Sender: TObject);
begin
  AppleAboutItemClick(Sender);
end;

end.

