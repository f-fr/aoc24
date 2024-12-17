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

unit Day17;

interface

implementation

uses AOC, Classes, SysUtils;

type
   TNum = 0..7;
   TInts = array of Int64;

function Run(input: TStrings): TResult;
var
   prg: array of TNum;
   regA, regB, regC: Int64;

   function Combo(val : TNum) : Int64; inline;
   begin
      case val of
         0,1,2,3: result := val;
         4: result := regA;
         5: result := regB;
         6: result := regC;
         else raise Exception.Create('Invalid combo operand');
      end;
   end;

   function Lit(val : TNum) : TNum; inline;
   begin
      result := val;
   end;

   function RunProg(TargetIdx : Integer = -1) : TInts;
   var
      ip : Integer = 0;
      value: TNum;
      initRegA: Int64;
   begin
      initRegA := regA;
      result := nil;
      while ip < Length(prg) do begin
         case prg[ip] of
            0: regA := regA shr combo(prg[ip + 1]);
            1: regB := regB xor lit(prg[ip + 1]);
            2: regB := combo(prg[ip + 1]) mod 8;
            3: if TargetIdx >= 0 then 
                  break // part2
               else if regA <> 0 then begin
                  ip := prg[ip + 1];
                  continue;
               end;
            4: regB := regB xor regC;
            5: begin
                  value := combo(prg[ip + 1]) mod 8;
                  if TargetIdx < 0 then
                     Insert(value, result, Length(result))
                  else if value = prg[TargetIdx] then
                     Insert(initRegA, result, Length(result));
               end;
            6: regB := regA shr combo(prg[ip + 1]);
            7: regC := regA shr combo(prg[ip + 1]);
         end;
         Inc(ip, 2);
      end;
   end;

   function Solve2(idx : Integer = 0) : TInts;
   var
      rs: TInts;
      r: Int64;
      i: Integer;
   begin
      if idx + 1 < Length(prg) then
         rs := Solve2(idx + 1)
      else
         rs := TInts.Create(0);

      result := nil;
      for r in rs do begin
         for i := 0 to 7 do begin
            regA := (r shl 3) or i;
            regB := 0;
            regC := 0;
            Insert(RunProg(idx), result, Length(result));
         end;
      end;
   end;

var
   toks: TStringArray;
   i: Integer;

   res: TInts = nil;
begin
   regA := input[0].SubString(11).ToInt64;
   regB := input[1].SubString(11).ToInt64;
   regC := input[2].SubString(11).ToInt64;

   toks := input[4].Split([':', ',', ' '], TStringSplitOptions.ExcludeEmpty);
   SetLength(prg, Length(toks) - 1);
   for i := 1 to High(toks) do prg[i - 1] := toks[i].toInteger;

   res := RunProg;
   SetLength(toks, Length(res));
   for i := 0 to High(res) do toks[i] := res[i].ToString;
   result[1] := String.Join(',', toks);
   result[2] := Solve2[0];
end;

initialization

   RegisterDay(17, @Run, 1);

end.
