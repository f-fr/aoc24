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

uses Aoc, CmdArgs, Day01;

var
   args: TCmdArgs;
   day: Integer;
   inputFileName: String;
begin
   args.addParam('DAY', 'Day to run').arg(day).required;
   args.addParam('INPUT-FILE', 'Input file to read').arg(inputFileName).required;
   args.addHelp;

   try
      args.parse;
   except
      on e: ECmdArgs do begin
         writeln(stderr, 'ERROR: ', e.message);
         halt(1);
      end;
      on e: EHelp do begin
         writeln(e.message);
         halt(1);
      end;
   end;

   RunDay(day, inputFileName);
end.
