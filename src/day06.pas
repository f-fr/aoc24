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

         k := grid.N - 1;
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
         visited.Count := 0;

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

function Run3(grid: TGrid): TResult;
type
   TDistGrid = specialize TGenGrid<UInt8>;
   TVisited = packed record
      pos: TPos;
      dir: TDir;
   end;
   TVisitedList = specialize TList<TVisited>;
var
   s, p, q, pnew: TPos;
   sd, d: TDir;
   i, j, k: Integer;
   path: TVisitedList = nil;
   visited: TVisitedList = nil;
   v: TVisited;
   gup, gright, gdown, gleft, flags: TDistGrid;
begin
   result[1] := 0;
   result[2] := 0;

   try
      gup := TDistGrid.Create(grid.N, grid.M);
      gright := TDistGrid.Create(grid.N, grid.M);
      gdown := TDistGrid.Create(grid.N, grid.M);
      gleft := TDistGrid.Create(grid.N, grid.M);
      flags := TDistGrid.Create(grid.N, grid.M);

      grid.Boundary := ' ';
      gup.Boundary := 0;
      gright.Boundary := 0;
      gdown.Boundary := 0;
      gleft.Boundary := 0;
      flags.Boundary := 32;

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

         k := grid.N - 1;
         for i := grid.N - 2 downto 1 do
            if grid[i,j] = '#' then k := i - 1 else gdown[i,j] := k;
      end;

      if not grid.TryFindOf(['<', '^', '>', 'v'], s) then raise Exception.Create('Starting position not found');

      sd := TDir.FromChar(grid.At[s]);

      p := s;
      d := sd;

      path := TVisitedList.Create;

      while grid.At[p] <> ' ' do begin
         if grid.At[p] <> 'X' then begin
            grid.At[p] := 'X';
            Inc(result[1]);
         end;
         q := p + d;
         case grid.At[q] of
            '#': d := d.Clockwise;
            ' ': break;
         else
            p := q;
            flags.At[p] := flags.At[p] or (1 shl Ord(d));
            v.pos := p;
            v.dir := d;
            path.Add(v);
         end
      end;

      for i := 1 to grid.N-2 do
         for j := 1 to grid.M-2 do
            if grid[i,j] <> '#' then grid[i, j] := '.';

      visited := TVisitedList.Create;
      visited.Capacity := path.Count;
      visited.Add(path[path.Count-1]); // ensure the last square is reset

      for k := path.Count - 1 downto 1 do begin
         // reset flags of previous iteration
         for v in visited do
            flags.At[v.pos] := flags.At[v.pos] and (not (1 shl Ord(v.dir)));
         visited.Count := 0;

         pnew := path[k].pos;
         p := path[k-1].pos;
         d := path[k-1].dir;

         flags.At[p] := flags.At[p] and not (1 shl Ord(d));

         if pnew = s then continue; // the start field is not allowed
         if flags.At[pnew] <> 0 then continue; // only the first time on the path

         // the same as above but doing "far jumps"
         while flags.At[p] <> 32 do begin
            if (flags.At[p] and (1 shl Ord(d))) <> 0 then begin
               Inc(result[2]);
               break;
            end;
            v.pos := p;
            v.dir := d;
            case d of
               Up:
                  if (p.j = pnew.j) and (pnew.i < p.i) then
                     p.i := Max(gup.At[p], pnew.i + 1)
                  else
                     p.i := gup.At[p];
               Right:
                  if (p.i = pnew.i) and (pnew.j > p.j) then
                     p.j := Min(gright.At[p], pnew.j - 1)
                  else
                     p.j := gright.At[p];
               Down:
                  if (p.j = pnew.j) and (pnew.i > p.i) then
                     p.i := Min(gdown.At[p], pnew.i - 1)
                  else
                     p.i := gdown.At[p];
               Left:
                  if (p.i = pnew.i) and (pnew.j < p.j) then
                     p.j := Max(gleft.At[p], pnew.j + 1)
                  else
                     p.j := gleft.At[p];
            end;
            d := d.Clockwise;
            if (p.i <> v.pos.i) or (p.j <> v.pos.j) then begin
               flags.At[v.pos] := flags.At[v.pos] or (1 shl Ord(v.dir));
               visited.Add(v);
            end;
         end;
      end;
   finally
      gup.Free;
      gright.Free;
      gdown.Free;
      gleft.Free;
      flags.Free;
      path.Free;
      visited.Free;
   end
end;

initialization

   RegisterDay(06, @Run, 1);
   RegisterDay(06, @Run2, 2);
   RegisterDay(06, @Run3, 3);

end.
