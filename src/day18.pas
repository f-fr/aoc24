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

uses AOC, AOC.Generic, Classes, StreamEx, SysUtils, generics.collections;

function Run(input: TTextReader): TResult;
type
   TPosList = specialize TList<TPos>;
   TIntGrid = specialize TGenGrid<Integer>;
   TPosQueue = specialize TQueue<TPos>;
var
   toks: array of String;
   nums: TPosList = nil;
   is_test: Boolean = true;
   npart1: Integer;

   grid: TIntGrid = nil;
   dist: TIntGrid = nil;
   seen: TIntGrid = nil;
   q: TPosQueue = nil;

   s, t, pos, nxt: TPos;
   dir: TDir;
   i, d: Integer;
   a, b, m: Integer;
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

      q := TPosQueue.Create;
      // we use binary search
      a := 0; // definitely works
      b := nums.Count; // definitely does not work
      i := 0; // the generation
      while a + 1 < b do begin
         if i = 0 then
            m := npart1 // for part 1
         else
            m := (a + b) div 2;

         q.Clear;
         q.Enqueue(s);
         dist.At[s] := 0;
         seen.At[s] := i;
         while q.Count > 0 do begin
            pos := q.Extract;
            if pos = t then break;
            d := dist.At[pos];
            for dir in TDir do begin
               nxt := pos + dir;
               if (grid.At[nxt] > m) and (seen.At[nxt] < i) then begin
                  q.Enqueue(nxt);
                  seen.At[nxt] := i;
                  dist.At[nxt] := d + 1;
               end;
            end;
         end;

         if i = 0 then result[1] := dist.At[t];
         if seen.At[t] = i then a := m else b := m;
         Inc(i);
      end;

      result[2] := nums[b - 1].i.toString + ',' + nums[b - 1].j.toString;
   finally
      nums.Free;
      grid.Free;
      dist.Free;
      seen.Free;
      q.Free;
   end
end;

function Run2(input: TTextReader): TResult;
type
   TPosList = specialize TList<TPos>;
   TIntGrid = specialize TGenGrid<Integer>;
   TPosQueue = specialize TQueue<TPos>;
var
   toks: array of String;
   nums: TPosList = nil;
   is_test: Boolean = true;
   npart1: Integer;

   grid: TIntGrid = nil;
   dist: TIntGrid = nil;
   q: TPosQueue = nil;

   s, t, pos, nxt: TPos;
   dir: TDir;
   i, d: Integer;
begin
   try
      nums := TPosList.Create;
      while not input.EOF do begin
         toks := input.ReadLine.split(',');
         nums.Add(TPos.Create(toks[0].toInteger + 1, toks[1].toInteger + 1));
         // detection of test cases
         is_test := is_test and (nums[nums.Count-1].i <= 7) and (nums[nums.Count-1].j <= 7);
      end;

      if is_test then begin
         grid := TIntGrid.Create(7, 7, High(Integer));
         npart1 := 12
      end else begin
         grid := TIntGrid.Create(71, 71, High(Integer));
         npart1 := 1024;
      end;

      dist := TIntGrid.Create(grid.N, grid.M, High(Integer));

      grid.Boundary := 0;
      dist.Boundary := High(Integer);
      for i := 0 to nums.Count - 1 do
         grid.At[nums[i]] := i + 1;

      s := TPos.Create(1, 1);
      t := TPos.Create(grid.N - 2, grid.M - 2);

      q := TPosQueue.Create;
      q.Enqueue(s);
      dist.At[s] := 0;
      while q.Count > 0 do begin
         pos := q.Extract;
         if pos = t then break;
         d := dist.At[pos];
         for dir in TDir do begin
            nxt := pos + dir;
            if (grid.At[nxt] > npart1) and (d + 1 < dist.At[nxt]) then begin
               q.Enqueue(nxt);
               dist.At[nxt] := d + 1;
            end;
         end;
      end;
      result[1] := dist.At[t];

      // for part2, start a search until we're stuck, then release a (seen) block
      dist.fill(2);
      i := nums.Count;
      q.Clear;
      q.Enqueue(s);
      dist.At[s] := 0;
      while true do begin
         while q.Count > 0 do begin
            pos := q.Extract;
            if pos = t then begin
               result[2] := (nums[i].i - 1).toString + ',' + (nums[i].j - 1).toString;
               exit;
            end;
            for dir in TDir do begin
               nxt := pos + dir;
               if dist.At[nxt] = 2 then begin
                  if grid.At[nxt] > i then begin
                     q.Enqueue(nxt);
                     dist.At[nxt] := 0;
                  end else
                     dist.At[nxt] := 1;
               end;
            end;
         end;
         repeat
            Dec(i);
         until (i < 0) or (dist.At[nums[i]] = 1);
         if i < 0 then break;
         dist.At[nums[i]] := 0;
         q.Enqueue(nums[i]);
      end;
   finally
      nums.Free;
      grid.Free;
      dist.Free;
      q.Free;
   end
end;

initialization

   RegisterDay(18, @Run, 1);
   RegisterDay(18, @Run2, 2);

end.
