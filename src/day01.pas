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

uses AOC, Classes, SysUtils, StreamEx;

function Run(input: TTextReader): TResult;
var
   line: String;
   toks: array of String;
   ns, ms: TIntList;
   i, j, k: Integer;
begin
   try
      ns := TIntList.Create;
      ms := TIntList.Create;
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

initialization

   RegisterDay(01, @Run, 1);

end.
