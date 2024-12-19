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

unit Day19;

interface

implementation

uses AOC, Classes, SysUtils, Math, Strings;

function Run(input: TStrings): TResult;
var
   patterns: TStringArray;
   possible: array of Int64 = nil;

   str, pat: String;
   i, ii: Integer;

   part1: Integer = 0;
   part2: Int64 = 0;
begin
   patterns := input[0].Split([',', ' '], TStringSplitOptions.ExcludeEmpty);

   for i := 2 to input.Count - 1 do begin
      str := input[i];
      SetLength(possible, Max(str.Length + 1, Length(possible)));
      possible[0] := 0;
      possible[str.Length] := 1;
      for ii := str.Length-1 downto 0 do begin
         possible[ii] := 0;
         for pat in patterns do begin
            if (pat.Length + ii <= str.Length)
               and (possible[pat.Length + ii] > 0)
               and (strlcomp(@pat[1], @str[ii+1], pat.Length) = 0)
            then begin
               possible[ii] += possible[ii + pat.Length];
            end;
         end;
      end;
      if possible[0] > 0 then part1 += 1;
      part2 += possible[0];
   end;
   
   result[1] := part1;
   result[2] := part2;
end;

initialization

   RegisterDay(19, @Run, 1);

end.
