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

unit Day13;

interface

implementation

uses AOC, Classes, SysUtils, StreamEx;

type
   TVec = array[1..2] of Int64;

function Det(a, b: TVec): Int64;
begin
   result := a[1] * b[2] - a[2] * b[1];
end;

// Solve [a b]·x = c
function Solve(a, b, c: TVec; out x: TVec): Boolean;
var
   d: Int64;
begin
   d := Det(a, b);
   if d = 0 then exit(False); // let's hope there are no linearly dependent instances with a solution
   x[1] := Det(c,b);
   if x[1] mod d <> 0 then exit(False);
   x[2] := Det(a,c);
   if x[2] mod d <> 0 then exit(False);
   x[1] := x[1] div d;
   x[2] := x[2] div d;
   result := True;
end;

function Run(input: TTextReader): TResult;
var
   toks: array of String;
   a, b, c, x: TVec;

   part1: Int64 = 0;
   part2: Int64 = 0;
begin
   while not input.Eof do begin
      toks := input.ReadLine.Split([' ', ':', '+', '=', ','], TStringSplitOptions.ExcludeEmpty);
      a[1] := toks[3].toInteger;
      a[2] := toks[5].toInteger;

      toks := input.ReadLine.Split([' ', ':', '+', '=', ','], TStringSplitOptions.ExcludeEmpty);
      b[1] := toks[3].toInteger;
      b[2] := toks[5].toInteger;

      toks := input.ReadLine.Split([' ', ':', '+', '=', ','], TStringSplitOptions.ExcludeEmpty);
      c[1] := toks[2].toInteger;
      c[2] := toks[4].toInteger;

      if Solve(a, b, c, x) then part1 += x[1] * 3 + x[2] * 1;

      c[1] += 10000000000000;
      c[2] += 10000000000000;

      if Solve(a, b, c, x) then part2 += x[1] * 3 + x[2] * 1;

      input.ReadLine; // empty line
   end;

   result[1] := part1;
   result[2] := part2;
end;

initialization

   RegisterDay(13, @Run, 1);

end.
