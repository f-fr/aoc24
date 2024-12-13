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
{$modeswitch nestedprocvars}
{$H+}

unit Day05;

interface

implementation

uses AOC, Classes, EasyCSV, Sorting, generics.collections, generics.defaults, SysUtils, Math;

function Run(input: TCSVReader): TResult;
type
   TEdge = array[1..2] of Integer;
   TEdges = specialize TDictionary<TEdge, Boolean>;

var
   edges: TEdges = nil;

   function CmpByEdges(constref u, v: Integer): Integer;
   var
      e: TEdge;
   begin
      if u = v then exit(0);
      e[1] := v;
      e[2] := u;
      if edges.ContainsKey(e) then result := 1
      else result := -1;
   end;

var
   row: TCSVReader.TRow;
   nodes: TIntArray = nil;
   e: TEdge;
   i: Integer;
   valid: Boolean;

begin
   result[1] := 0;
   result[2] := 0;
   try
      edges := TEdges.Create;
      input.Delimiter := '|';
      for row in input do begin
         if row.Count < 2 then break;
         e[1] := row.Integers[0];
         e[2] := row.Integers[1];
         edges.Add(e, True);
      end;
      input.Delimiter := ',';
      for row in input do begin
         SetLength(nodes, Max(Length(nodes), row.Count));
         for i := 0 to row.Count - 1 do nodes[i] := row.Integers[i];

         valid := True;
         e[2] := nodes[0];
         for i := 1 to row.Count - 1 do begin
            e[1] := e[2];
            e[2] := nodes[i];
            if not edges.ContainsKey(e) then begin
               valid := False;
               break;
            end;
         end;
         if valid then
            result[1] += nodes[row.Count div 2]
         else begin
            specialize Sort<Integer>(nodes, @CmpByEdges, 0, row.Count);
            result[2] += nodes[row.Count div 2];
         end;
      end;
   finally
      edges.Free;
   end
end;

function Run2(input: TCSVReader): TResult;
type
   TEdge = array[1..2] of Integer;
   TEdges = array[1..99, 1..99] of Boolean;

var
   edges: TEdges;

   function CmpByEdges(constref u, v: Integer): Integer;
   begin
      if u = v then exit(0);
      if edges[v, u] then result := 1
      else result := -1;
   end;

var
   row: TCSVReader.TRow;
   nodes: TIntArray;
   i: Integer;
   valid: Boolean;

begin
   result[1] := 0;
   result[2] := 0;

   input.Delimiter := '|';

   edges := default(TEdges);
   for row in input do begin
      if row.Count < 2 then break; // the empty line contains one cell
      edges[row.Integers[0], row.Integers[1]] := True;
   end;

   input.Delimiter := ',';

   for row in input do begin
      SetLength(nodes, Max(Length(nodes), row.Count));
      for i := 0 to row.Count - 1 do nodes[i] := row.Integers[i];;

      valid := True;
      for i := 1 to row.Count - 1 do begin
         if not edges[nodes[i-1], nodes[i]] then begin
            valid := False;
            break;
         end;
      end;
      if valid then
         result[1] += nodes[row.Count div 2]
      else begin
         specialize Sort<Integer>(nodes, @CmpByEdges, 0, row.Count);
         result[2] += nodes[row.Count div 2];
      end;
   end;
end;

initialization

   RegisterDay(05, @Run, 1);
   RegisterDay(05, @Run2, 2);

end.
