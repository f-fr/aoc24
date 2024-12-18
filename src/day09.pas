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

unit Day09;

interface

implementation

uses AOC, Classes, StreamEx, Math, PriQueue;

function Value(fileid: Integer; pos: Int64; len: Integer): Int64; inline;
begin
   result := fileid * len * (len - 1 + 2 * pos) div 2;
end;

function Run(input: TTextReader): TResult;
type
   TPriQueue = specialize TGPriQueue<Integer, Integer>;
var
   line: String;
   nums: array of Integer;
   pos, i, j, k, x, y: Integer;

   gaps: array[1..9] of TPriQueue;
   best: TPriQueue.TItem;

   part1: Int64 = 0;
   part2: Int64 = 0;
begin
   input.ReadLine(line);
   SetLength(nums, Length(line));
   for i := 1 to Length(line) do
      nums[i-1] := Ord(line[i]) - Ord('0');

   i := 0;
   j := Length(line) - 1;
   pos := 0;
   y := nums[j];
   while i < j do begin
      // add current left file block
      part1 += Value(i div 2, pos, nums[i]);
      Inc(pos, nums[i]);

      // go to space
      Inc(i);
      x := nums[i];
      while x > 0 do begin
         // copy from right to space
         k := Min(x, y);
         part1 += Value(j div 2, pos, k);
         pos += k;
         x -= k;
         y -= k;
         if y = 0 then begin
            j -= 2; // skip space
            if j <= i then break;
            y := nums[j];
         end
      end;
      // go to next block
      Inc(i);
   end;
   // remaining blocks at right end
   part1 += Value(j div 2, pos, y);

   try
      for i := Low(gaps) to High(gaps) do gaps[i] := TPriQueue.Create;
      i := 0;
      pos := 0;
      while i < Length(nums) do begin
         pos += nums[i];
         Inc(i);
         if (i < Length(nums)) and (nums[i] > 0) then begin
            gaps[nums[i]].Push(nums[i], pos);
            pos += nums[i];
         end;
         Inc(i);
      end;

      j := Length(nums) - 1;
      while j >= 0 do begin
         pos -= nums[j];

         best.value := pos;
         for i := nums[j] to 9 do begin
            if (not gaps[i].IsEmpty) and (gaps[i].Min.Value < best.value) then begin
               best := gaps[i].Min;
               assert(best.data = i);
            end;
         end;

         if best.value = pos then
            part2 += Value(j div 2, pos, nums[j]) // no gap found
         else begin
            // move to gap
            gaps[best.data].PopMin;
            // new smaller gap
            if best.data > nums[j] then gaps[best.data - nums[j]].Push(best.data - nums[j], best.value + nums[j]);
            part2 += Value(j div 2, best.value, nums[j]);
         end;

         if j > 0 then pos -= nums[j-1];
         Dec(j, 2);
      end;

      result[1] := part1;
      result[2] := part2;
   finally
      for i := Low(gaps) to High(gaps) do gaps[i].Free;
   end
end;

initialization

   RegisterDay(09, @Run, 1);

end.
