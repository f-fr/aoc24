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

unit Day15;

interface

implementation

uses AOC, AOC.Generic, Classes;

function Run(input: TStream): TResult;
type
   TIntGrid = specialize TGenGrid<Integer>;
var
   grid: TGrid = nil;
   grid2: TGrid = nil;
   moves: String;
   c: char;
   pos, oth, box: TPos;
   dir: TDir;

   seen: TIntGrid = nil;
   q: array of TPos; // queue for BFS
   qput, qget: Integer;

   i, j, n: Integer;

   part1: Integer = 0;
   part2: Integer = 0;
begin
   try
      grid := TGrid.ReadFromStream(input);
      pos := grid.Find('@');

      grid2 := TGrid.Create(grid.N, grid.M * 2, '.');
      n := 0;
      for i := 0 to grid.N-1 do begin
         for j := 0 to grid.M-1 do begin
            c := grid[i, j];
            if c = '.' then begin
               grid2[i, 2*j] := '.'; grid2[i, 2*j+1] := '.';
            end else if c = '#' then begin
               grid2[i, 2*j] := '#'; grid2[i, 2*j+1] := '#';
            end else if c = 'O' then begin
               Inc(n);
               grid2[i, 2*j] := '['; grid2[i, 2*j+1] := ']';
            end else if c = '@' then begin
               grid2[i, 2*j] := '@'; grid2[i, 2*j+1] := '.';
            end
         end;
      end;

      grid.At[pos] := '.';
          
      SetLength(moves, input.Size - input.Position);
      input.Read(moves[Low(moves)], Length(moves));

      for c in moves do begin
         case c of
            '^': dir := Up;
            '>': dir := Right;
            'v': dir := Down;
            '<': dir := Left;
         else
            continue;
         end;

         oth := pos;
         repeat
            oth := oth + dir;
         until grid.At[oth] <> 'O';
         if grid.At[oth] = '.' then begin
            pos += dir;
            grid.At[pos] := '.';
            if oth <> pos then grid.At[oth] := 'O';
         end;
      end;

      for i := 1 to grid.N-2 do begin
         for j := 1 to grid.M-2 do begin
            if grid[i,j] = 'O' then part1 += 100 * i + j;
         end
      end;

      // part 2
      pos := grid2.Find('@');
      grid2.At[pos] := '.';
      SetLength(q, n);
      seen := TIntGrid.Create(grid2.N, grid2.M, 0); // generational "seen"-map for BFS
      n := 0; // generation index

      for c in moves do begin
         Inc(n);
         if (c = '<') or (c = '>') then begin
            if c = '<' then dir := Left else dir := Right;
            oth := pos;
            repeat
               oth := oth + dir;
            until (grid2.At[oth] <> '[') and (grid2.At[oth] <> ']');
            if grid2.At[oth] = '.' then begin
               pos += dir;
               box := oth;
               while box <> pos do begin
                  grid2.At[box] := grid2.At[box - dir];
                  box -= dir;
               end;
               grid2.At[pos] := '.';
            end;
         end else if (c = '^') or (c = 'v') then begin
            if c = '^' then dir := Up else dir := Down;

            case grid2.At[pos + dir] of
               '.': pos += dir;
               '#': { a wall, nothing to do };
            else begin
               qput := 1;
               qget := 0;
               if grid2.At[pos + dir] = '[' then q[0] := pos + dir else q[0] := pos + dir + Left;
               while qget < qput do begin
                  box := q[qget]; Inc(qget);
                  oth := box + dir;
                  if (grid2.At[oth] = '#') or (grid2.At[oth + Right] = '#') then begin
                     qget := -1;
                     break;
                  end;
                  if grid2.At[oth] = ']' then oth += Left;
                  if (grid2.At[oth] = '[') and (seen.At[oth] < n) then begin
                     seen.At[oth] := n;
                     q[qput] := oth; Inc(qput);
                  end;

                  oth := box + dir + Right;
                  if grid2.At[oth] = '#' then begin
                     qget := -1;
                     break;
                  end;
                  if (grid2.At[oth] = '[') and (seen.At[oth] < n) then begin
                     seen.At[oth] := n;
                     q[qput] := oth; Inc(qput);
                  end;
               end;
               if qget >= 0 then begin
                  // move succeeds, move all boxes
                  // lazy, remove all boxes first, then set new boxes
                  for qget := 0 to qput - 1 do begin
                     grid2.At[q[qget]] := '.';
                     grid2.At[q[qget] + Right] := '.';
                  end;
                  for qget := 0 to qput - 1 do begin
                     grid2.At[q[qget] + dir] := '[';
                     grid2.At[q[qget] + dir + Right] := ']';
                  end;
                  // finally move the robot
                  pos += dir;
               end
            end
            end;
         end;
      end;

      for i := 1 to grid2.N-2 do begin
         for j := 1 to grid2.M-2 do begin
            if grid2[i,j] = '[' then part2 += 100 * i + j;
         end
      end;

      result[1] := part1;
      result[2] := part2;
   finally
      grid.Free;
      grid2.Free;
      seen.Free;
   end;
end;

initialization

   RegisterDay(15, @Run, 1);

end.
