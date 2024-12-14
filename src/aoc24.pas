{
  Copyright (c) 2024 Frank Fischer <frank-fischer@shadow-soft.de>

  This program is free software: you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see  <http://www.gnu.org/licenses/>
}

{$mode objfpc}
{$H+}

program Aoc24;

uses SysUtils, Aoc, Aoc.Tests {$include days.inc};

procedure Fail(const msg: String);
begin
   writeln(stderr, 'ERROR: ', msg);
   writeln(stderr, 'Try ', ParamStr(0), ' --help');
   halt(1);
end;

procedure Fail(const fmt: String; args: array of const);
begin
   writeln(stderr, 'ERROR: ', Format(fmt, args));
   writeln(stderr, 'Try ', ParamStr(0), ' --help');
   halt(1);
end;

procedure ShowHelp;
begin
   writeln('Usage: ', ParamStr(0), ' [-v VERSION] [-r] [DAY] [INPUT]');
   writeln;
   writeln('  If no DAY is specified, all days and versions are run.');
   writeln;
   writeln('  If no INPUT file is specified, the input for each day is assumed to be');
   writeln('  ./input/DAY/input1.txt');
   writeln;
   writeln('  An input file can only be specified for a single day.');
   writeln;
   writeln('Parameters');
   writeln('  -v VERSION      run only specified version');
   writeln('  -r              update running time table in README.md');

   halt(0);
end;

var
   day: Integer = 0;
   version: Integer = 0;
   inputFileName: String = '';
   updateReadme: Boolean = false;

   i: Integer;
begin
   i := 1;
   while i <= ParamCount do begin
      if (ParamStr(i) = '-h') or (ParamStr(i) = '--help') then
         ShowHelp
      else if ParamStr(i) = '-v' then begin
         Inc(i);
         if i > ParamCount then Fail('Missing version after -v');
         if not TryStrToInt(ParamStr(i), Version) then Fail('Invalid version: %s', [ParamStr(i)]);
      end else if ParamStr(i) = '-r' then
         updateReadme := true
      else if day = 0 then begin
         if not TryStrToInt(ParamStr(i), day) then Fail('Invalid day: %s', [ParamStr(i)]);
         if (day < 1) or (day > 25) then Fail('Invalid day, got %d (expected day ∈ {1, …, 25})', [day]);
      end else if inputFileName <> '' then
         Fail('Only one INPUT file can be specified')
      else
         inputFileName := ParamStr(i);
      Inc(i);
   end;

   {$ifdef DEBUG}
   try
   {$endif}
      RunDay(day, Version, inputFileName, updateReadme);
   {$ifdef DEBUG}
   except
      on e: Exception do begin
         writeln(stderr, 'ERROR: ', e.Message);
         halt(1);
      end;
   end
   {$endif}
end.
