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

uses AOC, Classes, StreamEx, StrUtils, SysUtils, Math;

function Check(x: Int64; nums: array of Integer; n: Integer): Boolean;
begin
   if n = 1 then exit(nums[0] = x);
   if x < nums[n-1] then exit(False);
   if (x mod nums[n-1] = 0) and Check(x div nums[n-1], nums, n-1) then exit(True);
   if Check(x - nums[n-1], nums, n-1) then exit(True);
   result := False;
end;

function Check2(x: Int64; nums: array of Integer; n: Integer): Boolean;
var
   y: Integer;
begin
   if n = 1 then exit(nums[0] = x);
   if x < nums[n-1] then exit(False);
   if (x mod nums[n-1] = 0) and Check2(x div nums[n-1], nums, n-1) then exit(True);
   if Check2(x - nums[n-1], nums, n-1) then exit(True);

   y := nums[n-1];
   while (y > 0) and (y mod 10 = x mod 10) do begin
      x := x div 10;
      y := y div 10;
   end;

   if (y = 0) and Check2(x, nums, n-1) then exit(True);

   result := False;
end;

function Run(input: TTextReader): TResult;

var
   line: String;
   toks: array of String;
   nums: array of Integer = nil;
   x: Int64;
   i: Integer;
begin
   result[1] := 0;
   result[2] := 0;

   for line in input do begin
      toks := line.Split([':', ' '], TStringSplitOptions.ExcludeEmpty);
      SetLength(nums, Max(Length(nums), Length(toks)));
      x := toks[0].toInt64;
      for i := 1 to High(toks) do nums[i-1] := toks[i].toInteger;
      if Check(x, nums, Length(toks) - 1) then begin
         result[1] += x;
         result[2] += x;
      end else if Check2(x, nums, Length(toks) - 1) then result[2] += x;
   end;
end;

initialization

   RegisterDay(07, @Run, 1);

end.
