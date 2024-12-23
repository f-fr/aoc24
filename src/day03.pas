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

unit Day03;

interface

implementation

uses AOC, Classes, SysUtils, StrUtils, regexpr;

var
   re_mul: TRegExpr;

function Run(input: TStream): TResult;
var
   data: String;
   x: Integer;
   doit: Boolean = True;
   part1: Integer = 0;
   part2: Integer = 0;
begin
   SetLength(data, input.Size);
   input.ReadBuffer(data[1], input.Size);
   if re_mul.Exec(data) then
      repeat
         if re_mul.MatchLen[3] > 0 then
            doit := True
         else if re_mul.MatchLen[4] > 0 then
            doit := False
         else begin
            x := StrToInt(re_mul.Match[1]) * StrToInt(re_mul.Match[2]);
            part1 += x;
            if doit then part2 += x;
         end
      until not re_mul.ExecNext;
   result[1] := part1;
   result[2] := part2;
end;

function Run2(input: TStream): TResult;
var
   data: String;
   i, j, x: Integer;
   doit: Boolean = True;
   part1: Integer = 0;
   part2: Integer = 0;
begin
   SetLength(data, input.Size);
   input.ReadBuffer(data[1], input.Size);
   i := 1;
   while true do begin
      j := PosSetEx(['m', 'd'], data, i);
      if j = 0 then break;
      if strlcomp(@data[j], 'mul(', 4) = 0 then begin
         i := j + 4;
         j := i;
         while (j < Length(data)) and (data[j] in DigitChars) do Inc(j);
         if (i = j) or (j = Length(data)) or (data[j] <> ',') then begin
            i := j;
            continue;
         end;
         x := StrToInt(data.SubString(i - 1, j - i));
         i := j + 1;
         j := i;
         while (j < Length(data)) and (data[j] in DigitChars) do Inc(j);
         if (i = j) or (j = Length(data)) or (data[j] <> ')') then begin
            i := j;
            continue;
         end;
         x *= StrToInt(data.SubString(i - 1, j - i));
         part1 += x;
         if doit then part2 += x;
      end else if strlcomp(@data[j], 'do()', 4) = 0 then
         doit := True
      else if strlcomp(@data[j], 'don''t()', 7) = 0 then
         doit := False;

      i := j + 1;
   end;

   result[1] := part1;
   result[2] := part2;
end;

initialization
   re_mul := TRegExpr.Create('mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don''t\(\))');

   RegisterDay(03, @Run, 1);
   RegisterDay(03, @Run2, 2);

finalization
   re_mul.Free;
end.
