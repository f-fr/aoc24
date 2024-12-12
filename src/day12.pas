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

unit Day12;

interface

implementation

uses AOC, AOC.Generic, Classes, generics.collections;

function Run(grid: TGrid): TResult;
type
   TPosQueue = specialize TQueue<TPos>;
   TBoolGrid = specialize TGenGrid<Boolean>;
var
   i, j: Integer;
   area, perm, lns: Integer;
   seen: TBoolGrid = nil;
   q: TPosQueue = nil;
   c: char;
   pos, nxt: TPos;
   dir: TDir;
begin
   result[1] := 0;
   result[2] := 0;

   try
      seen := TBoolGrid.Create(grid.N, grid.M, False);

      grid.Boundary := ' ';
      seen.Boundary := True;

      q := TPosQueue.Create;

      for i := 1 to grid.N - 2 do begin
         for j := 1 to grid.M - 2 do begin
            if seen[i, j] then continue;

            q.Enqueue(TPos.Create(i, j));
            seen[i, j] := True;
            area := 0;
            perm := 0;
            lns := 0;
            while q.Count > 0 do begin
               pos := q.Dequeue;
               c := grid.At[pos];
               area += 1;
               for dir in TDir do begin
                  nxt := pos.step(dir);
                  if c <> grid.At[nxt] then begin
                     perm += 1;
                     case dir of
                        Up: if (grid.At[nxt + Left] = c) or (grid.At[pos + Left] <> c) then lns += 1;
                        Right: if (grid.At[nxt + Up] = c) or (grid.At[pos + Up] <> c) then lns += 1;
                        Down: if (grid.At[nxt + Right] = c) or (grid.At[pos + Right] <> c) then lns += 1;
                        Left: if (grid.At[nxt + Down] = c) or (grid.At[pos + Down] <> c) then lns += 1;
                     end;
                  end else if not seen.At[nxt] then begin
                     seen.At[nxt] := True;
                     q.Enqueue(nxt);
                  end
               end;
            end;

            result[1] += area * perm;
            result[2] += area * lns;
         end
      end
   finally
      seen.Free;
      q.Free;
   end
end;

initialization

   RegisterDay(12, @Run, 1);

end.
