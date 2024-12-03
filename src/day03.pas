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

unit Day03;

interface

implementation

uses AOC, Classes, SysUtils, regexpr;

var
   re_mul: TRegExpr;

function Run(input: TStream): TResult;
var
   s: TStringStream = nil;
   x: Integer;
   doit: Boolean = True;
begin
   result[1] := 0;
   result[2] := 0;

   try
      s := TStringStream.Create;
      s.LoadFromStream(input);
      if re_mul.Exec(s.DataString) then
         repeat
            if re_mul.MatchLen[3] > 0 then
               doit := True
            else if re_mul.MatchLen[4] > 0 then
               doit := False
            else begin
               x := StrToInt(re_mul.Match[1]) * StrToInt(re_mul.Match[2]);
               result[1] += x;
               if doit then result[2] += x;
            end
         until not re_mul.ExecNext;
   finally
      s.Free;
   end
end;

initialization
   re_mul := TRegExpr.Create('mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don''t\(\))');

   RegisterDay(03, @Run, 1);

finalization
   re_mul.Free;
end.
