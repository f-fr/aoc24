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

uses AOC, AOC.Generic, Classes, PriQueue, SysUtils, Math, generics.collections;

function Run(grid: TGrid): TResult;
type
   TIncoming = record
      pos: TPos;
      dir: TDir;
   end;
   TIncomingDist = record
      pos: TPos;
      dir: TDir;
      d: Integer;
   end;

   TPriQueue = specialize TGPriQueue<TIncoming, Integer>;
   TSimpleQueue = specialize TQueue<TIncomingDist>;
   TIntGrid = specialize TGenGrid<Integer>;
var
   pos: TPos;
   st: array[1..2] of TPos;
   dir: TDir;
   i, j, d, c: Integer;
   q: TPriQueue = nil;
   simple_q: TSimpleQueue = nil;
   dists: array[1..2, TDir] of TIntGrid;
   degs: TIntGrid = nil;
   incoming: TIncoming;
   incomingd: TIncomingDist;

   part1: Int64 = 0;
   part2: Int64 = 0;
begin
   if not grid.TryFind('S', st[1]) then raise Exception.Create('No start found');
   if not grid.TryFind('E', st[2]) then raise Exception.Create('No end found');

   try
      q := TPriQueue.Create;
      simple_q := TSimpleQueue.Create;

      for dir in TDir do begin
         dists[1, dir] := TIntGrid.Create(grid.N, grid.M, High(Integer));
         dists[2, dir] := TIntGrid.Create(grid.N, grid.M, High(Integer));
      end;

      degs := TIntGrid.Create(grid.N, grid.M, 0);
      for i := 1 to grid.N-2 do begin
         for j := 1 to grid.M-2 do begin
            if grid[i, j] = '.' then
               for dir in TDir do
                  degs[TPos.Create(i, j) + dir] := degs[TPos.Create(i, j) + dir] + 1;
         end
      end;

      for j := 1 to 2 do begin
         incoming.pos := st[j];
         incoming.dir := Left;
         q.Push(incoming, 0);
         dists[j][Left].At[st[j]] := 0;

         while true do begin
            if simple_q.Count > 0 then begin
               incomingd := simple_q.Extract;
               pos := incomingd.pos;
               dir := incomingd.dir;
               d := incomingd.d;
            end else if q.TryPopMin(incoming, d) then begin
               pos := incoming.pos;
               dir := incoming.dir;
            end else
               break;

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
                  if degs[incoming.pos] <= 2 then begin
                     incomingd.pos := incoming.pos;
                     incomingd.dir := incoming.dir;
                     incomingd.d := d + c;
                     simple_q.Enqueue(incomingd);
                  end else
                     q.Push(incoming, d + c);
               end;

               dir := dir.Clockwise;
            end;
         end;
      end;

      part1 := dists[1][Up].At[st[2]];
      for dir in TDir do part1 := Min(part1, dists[1][dir].At[st[2]]);
      for i := 0 to grid.N-1 do
         for j := 0 to grid.M-1 do begin
            if grid[i, j] = '#' then continue;
            for dir in TDir do begin
               if dists[1][dir][i,j] > part1 then continue;
               if dists[2][dir][i,j] > part1 then continue;
               if dists[1][dir][i,j] + dists[2][dir][i,j] = part1 then begin
                  if grid[i, j] <> 'O' then begin
                     part2 += 1;
                     grid[i, j] := 'O';
                  end;
               end;
            end;
         end;

      result[1] := part1;
      result[2] := part2;
   finally
      q.Free;
      simple_q.Free;
      degs.Free;
      for dir in TDir do dists[1, dir].Free;
      for dir in TDir do dists[2, dir].Free;
   end
end;

initialization

   RegisterDay(16, @Run, 1);

end.
