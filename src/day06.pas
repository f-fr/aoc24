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

unit Day06;

interface

implementation

uses AOC, AOC.Generic, Classes, SysUtils, generics.collections, Math;

function Run(grid: TGrid): TResult;
type
   TPosList = specialize TList<TPos>;
var
   s, p, q, pnew: TPos;
   sd, d: TDir;
   i, j: Integer;
   path: TPosList = nil;
begin
   result[1] := 0;
   result[2] := 0;

   grid.Boundary := ' ';
   if not grid.TryFindOf(['<', '^', '>', 'v'], s) then raise Exception.Create('Starting position not found');

   sd := TDir.FromChar(grid.At[s]);

   p := s;
   d := sd;

   try
      path := TPosList.Create;

      while grid.At[p] <> ' ' do begin
         if grid.At[p] <> 'X' then begin
            grid.At[p] := 'X';
            if p <> s then path.Add(p);
            Inc(result[1]);
         end;
         q := p + d;
         case grid.At[q] of
            '#': begin
                    q := p;
                    d := d.Clockwise;
                 end;
            ' ': break;
         else p := q;
         end
      end;

      for pnew in path do begin
         for i := 1 to grid.N-2 do
            for j := 1 to grid.M-2 do
               if grid[i,j] <> '#' then
                  grid[i,j] := Chr(0);

         grid.At[pnew] := '#';

         p := s;
         d := sd;

         while grid.At[p] <> ' ' do begin
            if (Ord(grid.At[p]) and (1 shl Ord(d))) <> 0 then begin
               Inc(result[2]);
               break;
            end;
            grid.At[p] := Chr((Ord(grid.At[p]) or (1 shl Ord(d))));
            q := p + d;
            case grid.At[q] of
               '#': begin
                       q := p;
                       d := d.Clockwise;
                    end;
               ' ': break;
            else p := q;
            end
         end;

         grid.At[pnew] := Chr(0);
      end;
   finally
      path.Free;
   end
end;

function Run2(grid: TGrid): TResult;
type
   TPosList = specialize TList<TPos>;
   TDistGrid = specialize TGenGrid<UInt8>;
var
   s, p, q, pnew: TPos;
   sd, d: TDir;
   i, j, k: Integer;
   path: TPosList = nil;
   visited: TPosList = nil;
   gup, gright, gdown, gleft: TDistGrid;
begin
   result[1] := 0;
   result[2] := 0;

   try
      gup := TDistGrid.Create(grid.N, grid.M);
      gright := TDistGrid.Create(grid.N, grid.M);
      gdown := TDistGrid.Create(grid.N, grid.M);
      gleft := TDistGrid.Create(grid.N, grid.M);

      grid.Boundary := ' ';
      gup.Boundary := 0;
      gright.Boundary := 0;
      gdown.Boundary := 0;
      gleft.Boundary := 0;

      // for each field we store the next obstacle (or boundary) in that direction
      for i := 1 to grid.N-2 do begin
         k := 0;
         for j := 1 to grid.M - 2 do
            if grid[i,j] = '#' then k := j + 1 else gleft[i,j] := k;

         k := grid.M - 1;
         for j := grid.M - 2 downto 1 do
            if grid[i,j] = '#' then k := j - 1 else gright[i,j] := k;
      end;

      for j := 1 to grid.M-2 do begin
         k := 0;
         for i := 1 to grid.N - 2 do
            if grid[i,j] = '#' then k := i + 1 else gup[i,j] := k;

         k := grid.M - 1;
         for i := grid.N - 2 downto 1 do
            if grid[i,j] = '#' then k := i - 1 else gdown[i,j] := k;
      end;

      if not grid.TryFindOf(['<', '^', '>', 'v'], s) then raise Exception.Create('Starting position not found');

      sd := TDir.FromChar(grid.At[s]);

      p := s;
      d := sd;

      path := TPosList.Create;

      while grid.At[p] <> ' ' do begin
         if grid.At[p] <> 'X' then begin
            grid.At[p] := 'X';
            if p <> s then path.Add(p);
            Inc(result[1]);
         end;
         q := p + d;
         case grid.At[q] of
            '#': begin
                    q := p;
                    d := d.Clockwise;
                 end;
            ' ': break;
         else p := q;
         end
      end;

      for i := 1 to grid.N-2 do
         for j := 1 to grid.M-2 do
            if grid[i,j] <> '#' then grid[i, j] := Chr(0);

      visited := TPosList.Create;

      for pnew in path do begin
         for p in visited do grid.At[p] := Chr(0);
         visited.Clear;

         p := s;
         d := sd;

         // the same as above but doing "far jumps"
         while grid.At[p] <> ' ' do begin
            if ((Ord(grid.At[p])) and (1 shl Ord(d))) <> 0 then begin
               Inc(result[2]);
               break;
            end;
            grid.At[p] := Chr(((Ord(grid.At[p])) or (1 shl Ord(d))));
            visited.Add(p);
            case d of
               Up: if (p.j = pnew.j) and (pnew.i < p.i) then
                      p.i := Max(gup.At[p], pnew.i + 1)
                   else
                      p.i := gup.At[p];
               Right: if (p.i = pnew.i) and (pnew.j > p.j) then
                         p.j := Min(gright.At[p], pnew.j - 1)
                      else
                         p.j := gright.At[p];
               Down: if (p.j = pnew.j) and (pnew.i > p.i) then
                        p.i := Min(gdown.At[p], pnew.i - 1)
                     else
                        p.i := gdown.At[p];
               Left: if (p.i = pnew.i) and (pnew.j < p.j) then
                        p.j := Max(gleft.At[p], pnew.j + 1)
                     else
                        p.j := gleft.At[p];
            end;
            d := d.Clockwise;
         end;
      end;
   finally
      gup.Free;
      gright.Free;
      gdown.Free;
      gleft.Free;
      path.Free;
      visited.Free;
   end
end;

initialization

   RegisterDay(06, @Run, 1);
   RegisterDay(06, @Run2, 2);

end.
