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

unit Day22;

interface

implementation

uses AOC, Classes, SysUtils, StreamEx, generics.collections, Math;

function Next(x : Int64) : Int64;
const
   N = 16777216;
begin
   x := ((x * 64) xor x) mod N;
   x := ((x div 32) xor x) mod N;
   x := ((x * 2048) xor x) mod N;
   result := x;
end;

function Run(input: TTextReader): TResult;
type
   TChange = -9..9;
   TSeq = UInt32;
   TSeenDict = specialize TDictionary<TSeq, Boolean>;
   TSeqDict = specialize TDictionary<TSeq, Integer>;

var
   line: String;

   prices: TSeqDict = nil;
   seen: TSeenDict = nil;
   seq: TSeq;

   x, y: Integer;
   sum: Integer = 0;
   i, j: Integer;

   part1: Int64 = 0;
   part2: Integer = 0;
begin
   try
      prices := TSeqDict.Create;
      seen := TSeenDict.Create;

      for line in input do begin
         seen.Clear;
         x := line.toInt64;
         seq := 0;
         for i := 1 to 2000 do begin
            y := Next(x);
            seq := (seq shl 8) or UInt8((y mod 10) - (x mod 10));
            x := y;
            if i < 4 then continue;

            if not seen.ContainsKey(seq) then begin
               seen.Add(seq, True);
               if prices.TryGetValue(seq, sum) then
                  prices[seq] := sum + (x mod 10)
               else
                  prices.Add(seq, x mod 10);
            end;
         end;
         part1 += x;
      end;

      for x in prices.Values do part2 := Max(part2, x);

      result[1] := part1;
      result[2] := part2;
   finally
      prices.Free;
      seen.Free;
   end; 
end;

initialization

   RegisterDay(22, @Run, 1);

end.
