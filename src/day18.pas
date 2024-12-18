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

unit Day18;

interface

implementation

uses AOC, AOC.Generic, Classes, StreamEx, SysUtils, generics.collections, priqueue;

function Run(input: TTextReader): TResult;
type
   TPosList = specialize TList<TPos>;
   TIntGrid = specialize TGenGrid<Integer>;
   TPriQueue = specialize TGPriQueue<TPos, Integer>;
var
   toks: array of String;
   nums: TPosList = nil;
   is_test: Boolean = true;
   npart1: Integer;

   grid: TIntGrid = nil;
   dist: TIntGrid = nil;
   seen: TIntGrid = nil;
   q: TPriQueue = nil;

   s, t, pos, nxt: TPos;
   dir: TDir;
   i, d: Integer;

   part1: Integer = 0;
   part2: String;
begin
   try
      nums := TPosList.Create;
      while not input.EOF do begin
         toks := input.ReadLine.split(',');
         nums.Add(TPos.Create(toks[0].toInteger, toks[1].toInteger));
         // detection of test cases
         is_test := is_test and (nums[nums.Count-1].i <= 6) and (nums[nums.Count-1].j <= 6);
      end;

      if is_test then begin
         grid := TIntGrid.Create(7, 7, High(Integer));
         npart1 := 12
      end else begin
         grid := TIntGrid.Create(71, 71, High(Integer));
         npart1 := 1024;
      end;
      for i := 0 to nums.Count - 1 do
         grid.At[nums[i]] := i + 1;

      grid.Boundary := 0;
      s := TPos.Create(1, 1);
      t := TPos.Create(grid.N - 2, grid.M - 2);

      dist := TIntGrid.Create(grid.N, grid.M, High(Integer));
      seen := TIntGrid.Create(grid.N, grid.M, -1);

      q := TPriQueue.Create;
      for i := npart1 to nums.Count do begin
         q.Clear;
         q.Push(s, 0);
         dist.At[s] := 0;
         seen.At[s] := i;
         while q.TryPopMin(pos, d) do begin
            if pos = t then break;
            for dir in TDir do begin
               nxt := pos + dir;
               if (grid.At[nxt] > i) and ((seen.At[nxt] < i) or (d + 1 < dist.At[nxt])) then begin
                  q.Push(nxt, d + 1);
                  seen.At[nxt] := i;
                  dist.At[nxt] := d + 1;
               end;
            end;
         end;

         if i = npart1 then part1 := dist.At[t];
         if seen.At[t] < i then begin
            part2 := nums[i - 1].i.toString + ',' + nums[i - 1].j.toString;
            break;
         end;
      end;

      result[1] := part1;
      result[2] := part2;
   finally
      nums.Free;
      grid.Free;
      dist.Free;
      seen.Free;
      q.Free;
   end
end;

initialization

   RegisterDay(18, @Run, 1);

end.
