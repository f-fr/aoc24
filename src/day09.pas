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

uses AOC, Classes, StreamEx, PriQueue;

function Value(fileid, pos, len: Integer): Int64;
begin
   result := 0;
   while len > 0 do begin
      result += fileid * pos;
      Inc(pos);
      Dec(len);
   end;
end;

function Run(input: TTextReader): TResult;
type
   TPriQueue = specialize TGPriQueue<Integer, Integer>;
var
   line: String;
   nums, nums2: array of Integer;
   pos, i, j: Integer;

   fileid: Integer;

   gaps: array[1..9] of TPriQueue;
   best: TPriQueue.TItem;
begin
   result[1] := 0;
   result[2] := 0;

   input.ReadLine(line);
   SetLength(nums, Length(line));
   for i := 1 to Length(line) do
      nums[i-1] := Ord(line[i]) - Ord('0');
   nums2 := Copy(nums);

   i := 0;
   j := Length(line) - 1;
   pos := 0;
   while i < j do begin
      // add current left file block
      fileid := i div 2;
      while nums[i] > 0 do begin
         result[1] += fileid * pos;
         Inc(pos);
         Dec(nums[i]);
      end;

      // go to space
      Inc(i);
      while nums[i] > 0 do begin
         // copy from right to space
         fileid := j div 2;
         while (nums[i] > 0) and (nums[j] > 0) do begin
            result[1] += fileid * pos;
            Inc(pos);
            Dec(nums[i]);
            Dec(nums[j]);
         end;
         // check if space or right file has been exceeded
         if nums[j] = 0 then Dec(j, 2); // skip space
      end;
      // go to next block
      Inc(i);
   end;
   // remaining blocks at right end
   fileid := j div 2;
   while nums[j] > 0 do begin
      result[1] += fileid * pos;
      Inc(pos);
      Dec(nums[j]);
   end;

   try
      for i := Low(gaps) to High(gaps) do gaps[i] := TPriQueue.Create;
      i := 0;
      pos := 0;
      while i < Length(nums2) do begin
         pos += nums2[i];
         Inc(i);
         if (i < Length(nums2)) and (nums2[i] > 0) then begin
            gaps[nums2[i]].Push(nums2[i], pos);
            pos += nums2[i];
         end;
         Inc(i);
      end;

      j := Length(nums2) - 1;
      while j >= 0 do begin
         pos -= nums2[j];

         best.value := pos;
         for i := nums2[j] to 9 do begin
            if (not gaps[i].IsEmpty) and (gaps[i].Min.Value < best.value) then begin
               best := gaps[i].Min;
               assert(best.data = i);
            end;
         end;

         if best.value = pos then
            result[2] += Value(j div 2, pos, nums2[j]) // no gap found
         else begin
            // move to gap
            gaps[best.data].PopMin;
            // new smaller gap
            if best.data > nums2[j] then gaps[best.data - nums2[j]].Push(best.data - nums2[j], best.value + nums2[j]);
            result[2] += Value(j div 2, best.value, nums2[j]);
         end;

         if j > 0 then pos -= nums2[j-1];
         Dec(j, 2);
      end;
   finally
      for i := Low(gaps) to High(gaps) do gaps[i].Free;
   end
end;

initialization

   RegisterDay(09, @Run, 1);

end.
