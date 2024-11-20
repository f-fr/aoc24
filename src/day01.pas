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

unit Day01;

interface

implementation

uses AOC, Classes, StrUtils, SysUtils, Strings, StreamEx;

function FindNums(const line: String; fwd, names: Boolean): Int64;
const
   AllNames: array[1..9] of PChar = ('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine');

   function IsNum(i: Integer): Integer; inline;
   var
      j: Integer;
   begin
      if (line[i] >= '0') and (line[i] <= '9') then exit(Ord(line[i]) - Ord('0'));
      if names then begin
         for j := 1 to 9 do
            if strlcomp(AllNames[j], @line[i], Length(AllNames[j])) = 0 then exit(j);
      end;
      result := -1;
   end;

var
   i: Integer;
begin
   if fwd then 
      for i := 1 to Length(line) do begin
         result := IsNum(i);
         if result >= 0 then exit;
      end
   else
      for i := Length(line) downto 1 do begin
         result := IsNum(i);
         if result >= 0 then exit;
      end;

   result := 100; // nothing found, return a large number
end;

function Run(lines: TTextReader): TResult;
var
   line: String;
   i, j, p: Integer;
begin
   result[1] := 0;
   result[2] := 0;

   while not lines.Eof do begin
      lines.ReadLine(line);
      for p := 1 to 2 do begin
         i := FindNums(line, true, p = 2);
         j := FindNums(line, false, p = 2);
         result[p] += 10 * i + j;
      end;
   end;
end;

initialization

   RegisterDay(1, @Run);

end.
