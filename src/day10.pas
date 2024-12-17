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

unit Day10;

interface

implementation

uses AOC, AOC.Generic, Classes, generics.collections;

function Run(grid: TGrid): TResult;
type
   TPosQueue = specialize TQueue<TPos>;
   TIntGrid = specialize TGenGrid<Integer>;
var
   q: TPosQueue = nil;
   seen: TIntGrid = nil;
   cnt: TIntGrid = nil;
   i, j, k: Integer;
   pos, nxtpos: TPos;
   dir: TDir;
   cur, nxt: char;

   part1: Integer = 0;
   part2: Integer = 0;
begin
   try
      grid.Boundary := ' ';
      q := TPosQueue.Create;
      seen := TIntGrid.Create(grid.N, grid.M, -1);
      cnt := TIntGrid.Create(grid.N, grid.M);
      k := 0;
      for i := 1 to grid.N-2 do begin
         for j := 1 to grid.M-2 do begin
            if grid[i,j] <> '0' then continue;

            Inc(k); // next "generation"
            seen[i, j] := k;
            cnt[i, j] := 1;
            //q.Clear; // not necessary, queue is empty anyway
            q.Enqueue(TPos.Create(i, j));
            while q.Count > 0 do begin
               pos := q.Dequeue;
               cur := grid.At[pos];
               if cur = '9' then begin
                  part1 += 1;
                  part2 += cnt.At[pos]
               end else begin
                  nxt := Succ(cur);
                  for dir in TDir do begin
                     nxtpos := pos + dir;
                     if grid.At[nxtpos] = nxt then begin
                        if seen.At[nxtpos] < k then begin
                           cnt.At[nxtpos] := cnt.At[pos];
                           seen.At[nxtpos] := k;
                           q.Enqueue(nxtpos);
                        end else
                           cnt.At[nxtpos] := cnt.At[nxtpos] + cnt.At[pos];
                     end
                  end;
               end;
            end;
         end
      end;
      result[1] := part1;
      result[2] := part2;
   finally
      q.Free;
      seen.Free;
      cnt.Free;
   end
end;

initialization

   RegisterDay(10, @Run, 1);

end.
