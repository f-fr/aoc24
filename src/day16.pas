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

unit Day16;

interface

implementation

uses AOC, AOC.Generic, Classes, PriQueue, SysUtils, Math;

function Run(grid: TGrid): TResult;
type
   TIncoming = record
      pos: TPos;
      dir: TDir;
   end;

   TPriQueue = specialize TGPriQueue<TIncoming, Integer>;
   TIntGrid = specialize TGenGrid<Integer>;
var
   pos: TPos;
   st: array[1..2] of TPos;
   dir: TDir;
   i, j, d, c: Integer;
   q: TPriQueue = nil;
   dists: array[1..2, TDir] of TIntGrid;
   incoming: TIncoming;
begin
   result[1] := 0;
   result[2] := 0;

   if not grid.TryFind('S', st[1]) then raise Exception.Create('No start found');
   if not grid.TryFind('E', st[2]) then raise Exception.Create('No end found');

   try
      q := TPriQueue.Create;

      for dir in TDir do begin
         dists[1, dir] := TIntGrid.Create(grid.N, grid.M, High(Integer));
         dists[2, dir] := TIntGrid.Create(grid.N, grid.M, High(Integer));
      end;

      for j := 1 to 2 do begin
         incoming.pos := st[j];
         incoming.dir := Left;
         q.Push(incoming, 0);
         dists[j][Left].At[st[j]] := 0;

         while q.TryPopMin(incoming, d) do begin
            pos := incoming.pos;
            dir := incoming.dir;

            for i := 1 to 4 do begin
               incoming.pos := pos;
               incoming.dir := dir;
               c := 0;
               case i of
                  1: begin // the real stop going to another field
                        incoming.pos := pos + dir;
                        incoming.dir := dir.Invert;
                        c := 1;
                     end;
                  2, 4: if incoming.pos <> st[2] then c := 1000; // turn (but not in t)
                  3: c := 0; // traverse this field without turn
               end;


               if (grid.At[pos + dir] <> '#')
                  and (d + c < dists[j][incoming.dir].At[incoming.pos])
               then begin
                  dists[j][incoming.dir].At[incoming.pos] := d + c;
                  q.Push(incoming, d + c);
               end;

               dir := dir.Clockwise;
            end;
         end;
      end;

      result[1] := dists[1][Up].At[st[2]];
      for dir in TDir do result[1] := Min(result[1], dists[1][dir].At[st[2]]);
      for i := 0 to grid.N-1 do
         for j := 0 to grid.M-1 do
            for dir in TDir do begin
               if dists[1][dir][i,j] > result[1] then continue;
               if dists[2][dir][i,j] > result[1] then continue;
               if dists[1][dir][i,j] + dists[2][dir][i,j] = result[1] then begin
                  if grid[i, j] <> 'O' then begin
                     result[2] += 1;
                     grid[i, j] := 'O';
                  end;
               end;
            end;
   finally
      q.Free;
      for dir in TDir do dists[1, dir].Free;
      for dir in TDir do dists[2, dir].Free;
   end
end;

initialization

   RegisterDay(16, @Run, 1);

end.
