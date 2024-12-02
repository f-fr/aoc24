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

unit Day02;

interface

implementation

uses AOC, Classes, EasyCSV, Math;

function Run(input: TCSVReader): TResult;
var
   row: TCSVReader.TRow;
   i, d, mindiff, maxdiff: Integer;
   k: Integer;
begin
   input.Delimiter := ' ';

   result[1] := 0;
   result[2] := 0;
   for row in input do begin
      for k := -1 to row.Count - 1 do begin
         mindiff := High(Integer);
         maxdiff := Low(Integer);
         for i := 1 to row.Count - 1 do begin
            if i = k then
               continue
            else if i <> k + 1 then
               d := row.Integers[i] - row.Integers[i-1]
            else if i >= 2 then
               d := row.Integers[i] - row.Integers[i-2]
            else
               continue;
            mindiff := Min(d, mindiff);
            maxdiff := Max(d, maxdiff);
         end;

         if ((1 <= mindiff) and (mindiff <= maxdiff) and (maxdiff <= 3)) or
               ((-3 <= mindiff) and (mindiff <= maxdiff) and (maxdiff <= -1))
         then begin
            if k = -1 then Inc(result[1]);
            Inc(result[2]);
            break;
         end;
      end;
   end;
end;

initialization

   RegisterDay(02, @Run, 1);

end.
