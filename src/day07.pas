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

unit Day07;

interface

implementation

uses AOC, Classes, EasyCSV, StrUtils, SysUtils, Math;

function Check(x: Int64; nums: array of Integer; n: Integer; concat: Boolean = false): Boolean;
var
   y: Integer;
begin
   if n = 1 then exit(nums[0] = x);
   if x < nums[n-1] then exit(False);
   if (x mod nums[n-1] = 0) and Check(x div nums[n-1], nums, n-1, concat) then exit(True);
   if Check(x - nums[n-1], nums, n-1, concat) then exit(True);

   result := concat;
   if concat then begin
      y := nums[n-1];
      while (y > 0) and (y mod 10 = x mod 10) do begin
         x := x div 10;
         y := y div 10;
      end;

      result := (y = 0) and Check(x, nums, n-1, concat);
   end
end;

function Run(input: TCSVReader): TResult;

var
   row: TCSVReader.TRow;
   nums: array of Integer = nil;
   xs: String;
   x: Int64;
   i: Integer;
   part1: Integer = 0;
   part2: Integer = 0;
begin
   input.Delimiter := ' ';
   for row in input.Rows do begin
      SetLength(nums, Max(Length(nums), row.Count-1));
      xs := row[0];
      x := xs.SubString(0, xs.Length-1).toInt64;
      for i := 1 to row.Count-1 do nums[i-1] := row.Integers[i];
      if Check(x, nums, row.Count - 1) then begin
         part1 += x;
         part2 += x;
      end else if Check(x, nums, row.Count - 1, True) then part2 += x;
   end;
   result[1] := part1;
   result[2] := part2;
end;

initialization

   RegisterDay(07, @Run, 1);

end.
