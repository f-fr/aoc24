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

// This version runs in O(n). In each row we basically search for a path
// in a directed (acyclic) double-line graph. Because the graph is very
// special, we do this directly without fancy data structures.
//
// Another way to see this is dynamic programming, but I find the
// path-in-a-graph view easier to understand.
function Run2(input: TCSVReader): TResult;
var
   row: TCSVReader.TRow;
   i: Integer;
   dir, d, dskip: Integer;
begin
   input.Delimiter := ' ';

   result[1] := 0;
   result[2] := 0;
   for row in input do begin
      // trivial cases
      if row.Count <= 1 then begin
         Inc(result[1]);
         Inc(result[2]);
      end else if row.Count = 2 then begin
         d := Abs(row.Integers[1] - row.Integers[0]);
         if (1 <= d) and (d <= 3) then Inc(result[1]);
         Inc(result[2]);
      end else begin
         // try both directions (at least one should stop after at most 2 steps or so)
         for dir in TIntArray.Create(-1, 1) do begin
            d := dir;
            dskip := dir;
            for i := 1 to row.Count - 1 do begin
               // previous skip worked so far ...
               if i+1 < row.Count then begin
                  if dskip <> 0 then
                     dskip := row.Integers[i+1] - row.Integers[i];
                  if (d <> 0) and (not ((Sign(dskip) = dir) and (Abs(dskip) <= 3))) then
                     dskip := row.Integers[i+1] - row.Integers[i-1];
                  if not ((Sign(dskip) = dir) and (Abs(dskip) <= 3)) then
                     dskip := 0;
               end else if dskip = 0 then
                  dskip := d; // maybe skip the very last item

               if d <> 0 then
                  d := row.Integers[i] - row.Integers[i-1];
               if not ((Sign(d) = dir) and (Abs(d) <= 3)) then
                  d := 0;

               if (d = 0) and (dskip = 0) then break;
            end;
            if d <> 0 then begin
               Inc(result[1]);
               Inc(result[2]);
               break;
            end else if dskip <> 0 then begin
               Inc(result[2]);
               break;
            end
         end;
      end;
   end;
end;

initialization

   RegisterDay(02, @Run, 1);
   RegisterDay(02, @Run2, 2);

end.
