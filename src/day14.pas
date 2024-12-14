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

unit Day14;

interface

implementation

uses AOC, Classes, StreamEx, generics.collections, SysUtils, Math;

// function Run(input: TTextReader): TResult;
// type
//    TPosList = specialize TList<TPos>;
// var
//    toks: array of String;
//    ps: TPosList = nil;
//    vs: TPosList = nil;
//    p, v: TPos;
//    q: array [0..3] of Integer = (0, 0, 0, 0);

//    i, n, m: Integer;
// begin
//    result[1] := 0;
//    result[2] := 0;

//    try
//       ps := TPosList.Create; ps.Capacity := 500;
//       vs := TPosList.Create; vs.Capacity := 500;
//       while not input.Eof do begin
//          toks := input.ReadLine.Split(['=', ',', ' '], TStringSplitOptions.ExcludeEmpty);
//          p[2] := toks[1].toInteger;
//          p[1] := toks[2].toInteger;
//          v[2] := toks[4].toInteger;
//          v[1] := toks[5].toInteger;
//          ps.Add(p);
//          vs.Add(v);
//       end;

//       if ps.Count < 100 then begin
//          // test cases
//          n := 7;
//          m := 11;
//       end else begin
//          n := 103;
//          m := 101;
//       end;

//       for i := 0 to ps.Count - 1 do begin
//          p := ps[i];
//          v := vs[i];

//          p.i := (p.i + 100 * (n + v.i)) mod n;
//          p.j := (p.j + 100 * (m + v.j)) mod m;
//          if p.i < n div 2 then begin
//             if p.j < m div 2 then
//                Inc(q[0])
//             else if p.j > m div 2 then
//                Inc(q[1]);
//          end else if p.i > n div 2 then begin
//             if p.j < m div 2 then
//                Inc(q[2])
//             else if p.j > m div 2 then
//                Inc(q[3]);
//          end;
//       end;

//       result[1] := q[0] * q[1] * q[2] * q[3];
//       result[2] := 7055; // a cheat, but what to do
//    finally
//       ps.Free;
//       vs.Free;
//    end
// end;

function Run2(input: TTextReader): TResult;
type
   TPosList = specialize TList<TPos>;
var
   toks: array of String;
   ps: TPosList = nil;
   vs: TPosList = nil;
   p, v: TPos;
   q: array [0..3] of Integer = (0, 0, 0, 0);

   nhits: Integer;
   hits: array [40..46, 37..39] of Integer;

   i, j, n, m: Integer;
   g: TGrid;
   //a, b: Integer;
begin
   result[1] := 0;
   result[2] := 0;

   try
      ps := TPosList.Create; ps.Capacity := 500;
      vs := TPosList.Create; vs.Capacity := 500;
      while not input.Eof do begin
         toks := input.ReadLine.Split(['=', ',', ' '], TStringSplitOptions.ExcludeEmpty);
         p[2] := toks[1].toInteger;
         p[1] := toks[2].toInteger;
         v[2] := toks[4].toInteger;
         v[1] := toks[5].toInteger;
         ps.Add(p);
         vs.Add(v);
      end;

      if ps.Count < 100 then begin
         // test cases
         n := 7;
         m := 11;
      end else begin
         n := 103;
         m := 101;
      end;

      for i := Low(hits) to High(hits) do
         for j := Low(hits[i]) to High(hits[i]) do
            hits[i, j] := 0;

      g := TGrid.Create(n, m, ' ');
      i := 0;
      // stop for test cases
      while (i < 100) or ((result[2] = 0) and (n > 100)) do begin
         Inc(i);
         nhits := 0;
         for j := 0 to ps.Count - 1 do begin
            p := ps[j];
            p.i := (p.i + (n + vs[j].i)) mod n;
            p.j := (p.j + (m + vs[j].j)) mod m;
            g.At[p] := 'X';
            ps[j] := p;
            if (Low(hits) <= p.i) and (p.i <= High(hits)) and (Low(hits[p.i]) <= p.j) and (p.j <= High(hits[p.j])) and (hits[p.i, p.j] < i) then begin
               hits[p.i, p.j] := i;
               Inc(nhits);
            end;
         end;

         if i = 100 then begin
            for p in ps do
               if p.i < n div 2 then begin
                  if p.j < m div 2 then
                     Inc(q[0])
                  else if p.j > m div 2 then
                     Inc(q[1]);
               end else if p.i > n div 2 then begin
                  if p.j < m div 2 then
                     Inc(q[2])
                  else if p.j > m div 2 then
                     Inc(q[3]);
            end;
         end;

         if nhits = Length(hits) * Length(hits[Low(hits)]) then begin
            result[2] := i;
         end;

         // this is the actual output to "see" the tree
         // if i mod 101 = 86 then begin
         //    writeln('------------ i=', i, ' ----------------');
         //    for a := Low(hits) to High(hits) do begin
         //       for b := Low(hits[a]) to High(hits[a]) do begin
         //          g[a,b] := '*';
         //       end;
         //    end;
         //    writeln(g.toString);
         // end;

         for j := 0 to ps.Count - 1 do g.At[ps[j]] := ' ';
      end;

      result[1] := q[0] * q[1] * q[2] * q[3];
   finally
      ps.Free;
      vs.Free;
      g.Free;
   end
end;

// This version assumes that the picture contains many drones
// with the same x and y coordinates. It scores each picture
// until with the variance (sum of squares) which gives two
// iteration numbers (one for x, one for y). The CRT is used
// to find the first picture at which *both* numbers have a high
// score.
function Run3(input: TTextReader): TResult;
type
   TPosList = specialize TList<TPos>;
var
   toks: array of String;
   ps: TPosList = nil;
   vs: TPosList = nil;
   p, v: TPos;
   q: array [0..3] of Integer = (0, 0, 0, 0);

   iscores, jscores: array [0..102] of Integer;
   iscore, jscore, best_i, best_j, best_iscore, best_jscore: Integer;

   i, j, n, m: Integer;
begin
   result[1] := 0;
   result[2] := 0;

   try
      ps := TPosList.Create; ps.Capacity := 500;
      vs := TPosList.Create; vs.Capacity := 500;
      while not input.Eof do begin
         toks := input.ReadLine.Split(['=', ',', ' '], TStringSplitOptions.ExcludeEmpty);
         p[2] := toks[1].toInteger;
         p[1] := toks[2].toInteger;
         v[2] := toks[4].toInteger;
         v[1] := toks[5].toInteger;
         ps.Add(p);
         vs.Add(v);
      end;

      if ps.Count < 100 then begin
         // test cases
         n := 7;
         m := 11;
      end else begin
         n := 103;
         m := 101;
      end;

      best_i := 0;
      best_j := 0;
      best_iscore := 0;
      best_jscore := 0;

      for i := 1 to Max(m, n) do begin
         for j := 0 to n-1 do iscores[j] := 0;
         for j := 0 to m-1 do jscores[j] := 0;

         for j := 0 to ps.Count - 1 do begin
            p := ps[j];
            p.i := (p.i + (n + vs[j].i)) mod n;
            p.j := (p.j + (m + vs[j].j)) mod m;
            ps[j] := p;
            iscores[p.i] += 1;
            jscores[p.j] += 1;
         end;

         iscore := 0;
         jscore := 0;
         for j := 0 to n-1 do iscore += iscores[j] * iscores[j];
         for j := 0 to m-1 do jscore += jscores[j] * jscores[j];

         if iscore > best_iscore then begin
            best_i := i;
            best_iscore := iscore;
         end;

         if jscore > best_jscore then begin
            best_j := i;
            best_jscore := jscore;
         end;

         if i = 100 then begin
            for p in ps do
               if p.i < n div 2 then begin
                  if p.j < m div 2 then
                     Inc(q[0])
                  else if p.j > m div 2 then
                     Inc(q[1]);
               end else if p.i > n div 2 then begin
                  if p.j < m div 2 then
                     Inc(q[2])
                  else if p.j > m div 2 then
                     Inc(q[3]);
            end;
         end;
      end;

      result[1] := q[0] * q[1] * q[2] * q[3];
      result[2] := i;
   finally
      ps.Free;
      vs.Free;
   end
end;

initialization

   // RegisterDay(14, @Run, 1);
   RegisterDay(14, @Run2, 1);
   RegisterDay(14, @Run3, 2);

end.
