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

unit Day08;

interface

implementation

uses AOC, AOC.Generic, Classes, generics.collections;

function Run(grid: TGrid): TResult;
type
   TAntenna = record
      ch: char;
      pos: TPos;
   end;

   TAntennas = specialize TList<TAntenna>;
   TFlags = specialize TGenGrid<Boolean>;
var
   pnts: TFlags = nil;
   pnts2: TFlags = nil;
   antennas: TAntennas = nil;
   a: TAntenna;
   i, j, s, t: Integer;
   dx, dy: Integer;
   p: TPos;

   part1: Integer = 0;
   part2: Integer = 0;
begin
   try
      antennas := TAntennas.Create;
      pnts := TFlags.Create(grid.N, grid.M, False);
      pnts2 := TFlags.Create(grid.N, grid.M, False);

      for i := 0 to grid.N-1 do
         for j := 0 to grid.M-1 do
            if grid[i,j] <> '.' then begin
               a.ch := grid[i,j];
               a.pos.i := i;
               a.pos.j := j;
               antennas.Add(a);
            end;
      antennas.Sort;

      t := 0;
      while t < antennas.Count do begin
         s := t;
         while (t < antennas.Count) and (antennas[t].ch = antennas[s].ch) do Inc(t);
         for i := s to t-1 do
            for j := s to t-1 do begin
               if i = j then continue;
               dx := antennas[j].pos.i - antennas[i].pos.i;
               dy := antennas[j].pos.j - antennas[i].pos.j;
               p.i := antennas[j].pos.i + dx;
               p.j := antennas[j].pos.j + dy;
               if grid.IsValid(p) and not pnts.At[p] then begin
                  pnts.At[p] := True;
                  part1 += 1;
               end;

               p := antennas[j].pos;
               while grid.IsValid(p) do begin
                  if not pnts2.At[p] then begin
                     pnts2.At[p] := True;
                     part2 += 1;
                  end;
                  p.i += dx;
                  p.j += dy;
               end
            end
      end;
      result[1] := part1;
      result[2] := part2;
   finally
      antennas.Free;
      pnts.Free;
      pnts2.Free;
   end
end;

initialization

   RegisterDay(08, @Run, 1);

end.
