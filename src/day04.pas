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

unit Day04;

interface

implementation

uses AOC, Classes, SysUtils;

function Run(grid: TGrid): TResult;
var
   i,j: Integer;
   dx, dy: Integer;
begin
   result[1] := 0;
   result[2] := 0;

   grid.Boundary := '.';

   for i := 1 to grid.N-2 do begin
      for j := 1 to grid.M-2 do begin
         for dx := -1 to 1 do begin
            for dy := -1 to 1 do begin
               if (grid[i,j] = 'X') and
                     (grid[i+dx, j+dy] = 'M') and
                     (grid[i+2*dx, j+2*dy] = 'A') and
                     (grid[i+3*dx, j+3*dy] = 'S')
               then Inc(result[1]);
            end
         end;

         if (grid[i,j] = 'A') and
               (((grid[i-1, j-1] = 'M') and (grid[i+1, j+1] = 'S')) or
                ((grid[i-1, j-1] = 'S') and (grid[i+1, j+1] = 'M'))) and
               (((grid[i-1, j+1] = 'M') and (grid[i+1, j-1] = 'S')) or
                ((grid[i-1, j+1] = 'S') and (grid[i+1, j-1] = 'M')))
         then Inc(result[2]);
      end
   end
end;

function Run2(grid: TStrings): TResult;
var
   i,j: Integer;
   dx, dy: Integer;
begin
   result[1] := 0;
   result[2] := 0;

   for i := 0 to grid.Count - 1 do grid[i] := Format('.%s.', [grid[i]]);
   grid.Append(StringOfChar('.', grid[0].Length));
   grid.Insert(0, grid[grid.Count - 1]);

   for i := 1 to grid.Count-2 do begin
      for j := 2 to grid[i].Length-1 do begin
         for dx := -1 to 1 do begin
            for dy := -1 to 1 do begin
               if (grid[i][j] = 'X') and
                     (grid[i+dx][j+dy] = 'M') and
                     (grid[i+2*dx][j+2*dy] = 'A') and
                     (grid[i+3*dx][j+3*dy] = 'S')
               then Inc(result[1]);
            end
         end;

         if (grid[i][j] = 'A') and
               (((grid[i-1][j-1] = 'M') and (grid[i+1][j+1] = 'S')) or
                ((grid[i-1][j-1] = 'S') and (grid[i+1][j+1] = 'M'))) and
               (((grid[i-1][j+1] = 'M') and (grid[i+1][j-1] = 'S')) or
                ((grid[i-1][j+1] = 'S') and (grid[i+1][j-1] = 'M')))
         then Inc(result[2]);
      end
   end
end;

initialization

   RegisterDay(04, @Run, 1);
   RegisterDay(04, @Run2, 2);

end.
