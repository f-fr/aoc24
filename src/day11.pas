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

unit Day11;

interface

implementation

uses AOC, Classes, EasyCSV, generics.collections;

function TrySplit(var x: Int64; out y: Int64): Boolean;
var
   a: Int64;
   i, j: Integer;
begin
   a := x;
   i := 0;
   y := -1;
   while a > 0 do begin
      Inc(i);
      a := a div 10;
   end;
   result := i mod 2 = 0;
   if not result then exit;

   i := i div 2;
   a := 0;
   for j := 1 to i do begin
      a := a * 10 + (x mod 10);
      x := x div 10;
   end;

   y := 0;
   for j := 1 to i do begin
      y := y * 10 + a mod 10;
      a := a div 10;
   end
end;

function Run(input: TCsvReader): TResult;
type
   TIntDict = specialize TDictionary<Int64, Int64>;
var
   row: TCsvReader.TRow;
   cnts: TIntDict = nil;
   cnts2: TIntDict = nil;
   tmp: TIntDict;
   p: TIntDict.TDictionaryPair;
   x, y, n: Int64;
   i, k: Integer;

   part1: Int64 = 0;
   part2: Int64 = 0;
begin
   try
      cnts := TIntDict.Create;
      cnts2 := TIntDict.Create;
      input.Delimiter := ' ';
      for row in input do begin
         for i := 0 to row.Count - 1 do begin
            x := row.Integers[i];
            if cnts.TryGetValue(x, n) then
               cnts[x] := n + 1
            else
               cnts.Add(x, 1);
         end;
      end;

      for k := 1 to 75 do begin
         cnts2.Clear;
         for p in cnts do begin
            x := p.Key;
            y := -1;

            if x = 0 then
               x := 1
            else if not TrySplit(x, y) then
               x := x * 2024;

            if cnts2.TryGetValue(x, n) then cnts2[x] := n + p.Value else cnts2.Add(x, p.Value);
            if y >= 0 then
               if cnts2.TryGetValue(y, n) then cnts2[y] := n + p.Value else cnts2.Add(y, p.Value);
         end;
         tmp := cnts;
         cnts := cnts2;
         cnts2 := tmp;

         if k = 25 then for p in cnts do part1 += p.Value;
      end;
      for p in cnts do part2 += p.Value;

      result[1] := part1;
      result[2] := part2;
   finally
      cnts.Free;
      cnts2.Free;
   end
end;

function Run2(input: TCsvReader): TResult;
type
   TIntDict = specialize TDictionary<Int64, Int64>;
   TMemo = array[1..2] of Int64;
   TMemoDict = specialize TDictionary<Int64, TMemo>;

var
   memo: TMemoDict = nil;

   function TrySplit(var x: Int64; out y: Int64): Boolean;
   var
      a, b: Int64;
      i, j: Integer;
      r: TMemo;
   begin
      if memo.TryGetValue(x, r) then begin
         result := r[1] > 0;
         if result then begin
            x := r[1];
            y := r[2];
         end;
         exit;
      end;

      a := x;
      i := 0;
      y := -1;
      while a > 0 do begin
         Inc(i);
         a := a div 10;
      end;
      result := i mod 2 = 0;
      if not result then exit;

      i := i div 2;
      a := 0;
      b := x;
      for j := 1 to i do begin
         a := a * 10 + (x mod 10);
         x := x div 10;
      end;

      y := 0;
      for j := 1 to i do begin
         y := y * 10 + a mod 10;
         a := a div 10;
      end;

      r[1] := x;
      r[2] := y;
      memo.Add(b, r);
   end;

var
   row: TCsvReader.TRow;
   cnts: TIntDict = nil;
   cnts2: TIntDict = nil;
   tmp: TIntDict;
   p: TIntDict.TDictionaryPair;
   x, y, n: Int64;
   i, k: Integer;

   part1: Int64 = 0;
   part2: Int64 = 0;
begin
   try
      cnts := TIntDict.Create;
      cnts2 := TIntDict.Create;
      memo := TMemoDict.Create;
      input.Delimiter := ' ';
      for row in input do begin
         for i := 0 to row.Count - 1 do begin
            x := row.Integers[i];
            if cnts.TryGetValue(x, n) then
               cnts[x] := n + 1
            else
               cnts.Add(x, 1);
         end;
      end;

      for k := 1 to 75 do begin
         cnts2.Clear;
         for p in cnts do begin
            x := p.Key;
            y := -1;

            if x = 0 then
               x := 1
            else if not TrySplit(x, y) then
               x := x * 2024;

            if cnts2.TryGetValue(x, n) then cnts2[x] := n + p.Value else cnts2.Add(x, p.Value);
            if y >= 0 then
               if cnts2.TryGetValue(y, n) then cnts2[y] := n + p.Value else cnts2.Add(y, p.Value);
         end;
         tmp := cnts;
         cnts := cnts2;
         cnts2 := tmp;

         if k = 25 then for p in cnts do part1 += p.Value;
      end;
      for p in cnts do part2 += p.Value;

      result[1] := part1;
      result[2] := part2;
   finally
      cnts.Free;
      cnts2.Free;
      memo.Free;
   end
end;
initialization

   RegisterDay(11, @Run, 1);
   RegisterDay(11, @Run2, 2);

end.
