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

uses AOC, AOC.Generic, Classes, SysUtils;

function Run(grid: TGrid): TResult;
var
   s, p, q: TPos;
   sd, d: TDir;
   i, j, k, l: Integer;
begin
   result[1] := 0;
   result[2] := 0;

   grid.Boundary := ' ';
   if not grid.TryFindOf(['<', '^', '>', 'v'], s) then raise Exception.Create('Starting position not found');

   sd := TDir.FromChar(grid.At[s]);

   p := s;
   d := sd;

   while grid.At[p] <> ' ' do begin
      if grid.At[p] <> 'X' then begin
         grid.At[p] := 'X';
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

   for k := 1 to grid.N-2 do begin
      for l := 1 to grid.M-2 do begin
         p := TPos.Create(k, l);
         if (s = p) or (grid.At[p] = '#') then continue;

         for i := 1 to grid.N-2 do 
            for j := 1 to grid.M-2 do
               if grid[i,j] <> '#' then
                  grid[i,j] := Chr(0);

         grid[k, l] := '#';

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

         grid[k, l] := Chr(0);
      end
   end
end;

initialization

   RegisterDay(06, @Run, 1);

end.
