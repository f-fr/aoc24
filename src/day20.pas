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

unit Day20;

interface

implementation

uses AOC, Classes, generics.collections, Math;

function Run(grid: TGrid): TResult;
type
   TPosQueue = specialize TQueue<TPos>;
var
   s, t, pos, nxt: TPos;
   st: array[1..2] of TPos;
   q: TPosQueue = nil;
   dists: array[1..2] of TIntGrid = (nil, nil);
   dist: TIntGrid = nil;
   seen: TIntGrid = nil;

   i, j, i2, j2, d, dst: Integer;
   dir: TDir;

   diff1: Integer = 100;
   diff2: Integer = 100;

   part1: Integer = 0;
   part2: Integer = 0;
begin

   s := grid.find('S');
   t := grid.find('E');
   st[1] := s;
   st[2] := t;

   try
      q := TPosQueue.Create;

      for i := 1 to 2 do begin
         dists[i] := TIntGrid.Create(grid.N, grid.M, High(Integer));
         q.Clear;
         q.Enqueue(st[i]);
         dists[i].At[st[i]] := 0;
         while q.Count > 0 do begin
            pos := q.Extract;
            d := dists[i].At[pos];
            for dir in TDir do begin
               nxt := pos + dir;
               if (grid.At[nxt] <> '#') and (dists[i].At[nxt] = High(Integer)) then begin
                  dists[i].At[nxt] := d + 1;
                  q.Enqueue(nxt);
               end;
            end;
         end;
      end;

      if grid.N <= 16 then begin // test case
         diff1 := 1;
         diff2 := 50;
      end;

      for i := 1 to grid.N - 2 do begin
         for j := 1 to grid.N - 2 do begin
            pos := TPos.Create(i, j);
            if grid[i,j] <> '#' then begin
               for i2 := Max(i-20, 1) to Min(i+20, grid.N-2) do begin
                  for j2 := Max(j-20+Abs(i-i2), 1) to Min(j+20-Abs(i-i2), grid.M-2) do begin
                     if grid[i2, j2] = '#' then continue;
                     nxt := TPos.Create(i2, j2);
                     d := pos.dist1(nxt);
                     dst := dists[1][t] - (dists[1].At[pos] + dists[2].At[nxt] + d);
                     
                     if (d <= 2) and (dst >= diff1) then part1 += 1;
                     if (d <= 20) and (dst >= diff2) then part2 += 1;
                  end;
               end;
            end
         end
      end;

      result[1] := part1;
      result[2] := part2;
   finally
      q.Free;
      dists[1].Free;
      dists[2].Free;
      seen.Free;
      dist.Free;
   end
end;

initialization

   RegisterDay(20, @Run, 1);

end.
