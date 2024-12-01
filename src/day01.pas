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

unit Day01;

interface

implementation

uses AOC, Classes, SysUtils, StreamEx, generics.collections, EasyCSV;

type
   TIntDict = specialize TDictionary<Integer, Integer>;

function Run(input: TTextReader): TResult;
var
   line: String;
   toks: array of String;
   ns, ms: TIntList;
   i, j, k: Integer;
begin
   try
      ns := TIntList.Create; ns.Capacity := 1000;
      ms := TIntList.Create; ms.Capacity := 1000;
      for line in input do begin
         toks := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
         ns.Add(toks[0].ToInteger);
         ms.Add(toks[1].ToInteger);
      end;
      ns.Sort;
      ms.Sort;
      result[1] := 0;
      for i := 0 to ns.Count - 1 do begin
         result[1] += Abs(ns[i] - ms[i]);
      end;

      result[2] := 0;
      i := 0;
      j := 0;
      while i < ns.Count do begin
         k := ns[i];
         while (i + 1 < ns.Count) and (ns[i + 1] = ns[i]) do begin
            k += ns[i];
            Inc(i);
         end;

         while (j < ms.Count) and (ms[j] < ns[i]) do Inc(j);
         while (j < ms.Count) and (ms[j] = ns[i]) do begin
            result[2] += k;
            Inc(j);
         end;

         Inc(i)
      end;
   finally
      ns.Free;
      ms.Free;
   end
end;

function Tally(ns: TIntList): TIntDict;
var
   i, cnt: Integer;
begin
   result := nil;
   try
      result := TIntDict.Create;
      for i in ns do
         if result.TryGetValue(i, cnt) then
            result[i] := cnt + 1
         else
            result.Add(i, 1);
   except
      result.Free;
      raise;
   end
end;

function Run2(input: TTextReader): TResult;
var
   line: String;
   toks: array of String;
   ns, ms: TIntList;
   i, j: Integer;
   nscnt, mscnt: TIntDict;
   pair: TIntDict.TDictionaryPair;
begin
   try
      ns := TIntList.Create; ns.Capacity := 1000;
      ms := TIntList.Create; ms.Capacity := 1000;
      for line in input do begin
         toks := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
         ns.Add(toks[0].ToInteger);
         ms.Add(toks[1].ToInteger);
      end;

      ns.Sort;
      ms.Sort;
      result[1] := 0;
      for i := 0 to ns.Count - 1 do result[1] += Abs(ns[i] - ms[i]);

      // use the "tally" function, which basically just counts
      nscnt := Tally(ns);
      mscnt := Tally(ms);
      result[2] := 0;
      for pair in nscnt do
         if mscnt.TryGetValue(pair.Key, j) then
            result[2] += pair.Key * pair.Value * j;
   finally
      ns.Free;
      ms.Free;
      mscnt.Free;
      nscnt.Free;
   end
end;

// Version 1 but taking input as TStrings.
// For the numbers we use arrays instead of TIntList.
function Run3(input: TStrings): TResult;
var
   line: String;
   toks: array of String;
   ns, ms: array of Integer;
   i, j, k: Integer;
begin
   SetLength(ns, input.Count);
   SetLength(ms, input.Count);
   for i := 0 to input.Count - 1 do begin
      toks := input[i].Split(' ', TStringSplitOptions.ExcludeEmpty);
      ns[i] := toks[0].toInteger;
      ms[i] := toks[1].toInteger;
   end;

   TIntArrayHelper.Sort(ns);
   TIntArrayHelper.Sort(ms);

   result[1] := 0;
   for i := 0 to High(ns) do result[1] += Abs(ns[i] - ms[i]);

   result[2] := 0;
   i := 0;
   j := 0;
   while i < Length(ns) do begin
      k := ns[i];
      while (i + 1 < Length(ns)) and (ns[i + 1] = ns[i]) do begin
         k += ns[i];
         Inc(i);
      end;

      while (j < Length(ms)) and (ms[j] < ns[i]) do Inc(j);
      while (j < Length(ms)) and (ms[j] = ns[i]) do begin
         result[2] += k;
         Inc(j);
      end;

      Inc(i)
   end;
end;

// Version 1 but taking input as TStream using EasyCSV.
function Run4(csv: TCSVReader): TResult;
var
   row: TCSVReader.TRow;
   ns, ms: TIntList;
   i, j, k: Integer;
begin
   try
      csv.Delimiter := ' ';
      ns := TIntList.Create; ns.Capacity := 1000;
      ms := TIntList.Create; ms.Capacity := 1000;
      for row in csv.Rows do begin
         ns.Add(row.Integers[0]);
         ms.Add(row.Integers[1]);
      end;
      ns.Sort;
      ms.Sort;
      result[1] := 0;
      for i := 0 to ns.Count - 1 do result[1] += Abs(ns[i] - ms[i]);

      result[2] := 0;
      i := 0;
      j := 0;
      while i < ns.Count do begin
         k := ns[i];
         while (i + 1 < ns.Count) and (ns[i + 1] = ns[i]) do begin
            k += ns[i];
            Inc(i);
         end;

         while (j < ms.Count) and (ms[j] < ns[i]) do Inc(j);
         while (j < ms.Count) and (ms[j] = ns[i]) do begin
            result[2] += k;
            Inc(j);
         end;

         Inc(i)
      end;
   finally
      ns.Free;
      ms.Free;
   end
end;

initialization

   RegisterDay(01, @Run, 1);
   RegisterDay(01, @Run2, 2);
   RegisterDay(01, @Run3, 3);
   RegisterDay(01, @Run4, 4);

end.
