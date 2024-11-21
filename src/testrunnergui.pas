program aoc24testsgui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, Aoc {$include days.inc};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

