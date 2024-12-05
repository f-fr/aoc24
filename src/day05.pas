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

unit Day05;

interface

implementation

uses AOC, Classes, StreamEx, generics.collections, generics.defaults, SysUtils, Math;

type
   TEdge = array[1..2] of Integer;
   TEdges = specialize TDictionary<TEdge, Boolean>;

   TCmpByEdges = class(TInterfacedObject, specialize IComparer<Integer>)
      Fedges: TEdges;
      constructor Create(Aedges: TEdges);
      function Compare(constref ALeft, ARight: Integer): Integer;
   end;

constructor TCmpByEdges.Create(Aedges: TEdges);
begin
    inherited Create;
    Fedges := Aedges;
end;

function TCmpByEdges.Compare(constref ALeft, ARight: Integer): Integer;
var
   e: TEdge;
begin
   if ALeft = ARight then exit(0);
   e[1] := ARight;
   e[2] := ALeft;
   if Fedges.ContainsKey(e) then exit(+1);
   result := -1;
end;

function Run(input: TTextReader): TResult;

var
   line: String;
   toks: array of String;
   nodes: TIntArray;
   edges: TEdges = nil;
   e: TEdge;
   CmpByEdges: TCmpByEdges = nil;
   i: Integer;
   valid: Boolean;
begin
   result[1] := 0;
   result[2] := 0;
   try
      edges := TEdges.Create;
      CmpByEdges := TCmpByEdges.Create(edges);
      for line in input do begin
         if Length(line) = 0 then break;
         toks := line.Split('|');
         e[1] := toks[0].toInteger;
         e[2] := toks[1].toInteger;
         edges.Add(e, True);
      end;
      for line in input do begin
         if Length(line) = 0 then continue;
         toks := line.Split(',');
         SetLength(nodes, Max(Length(nodes), Length(toks)));
         for i := 0 to High(toks) do nodes[i] := toks[i].toInteger;

         valid := True;
         e[2] := nodes[0];
         for i := 1 to High(toks) do begin
            e[1] := e[2];
            e[2] := nodes[i];
            if not edges.ContainsKey(e) then begin
               valid := False;
               break;
            end;
         end;
         if valid then
            result[1] += nodes[Length(toks) div 2]
         else begin
            TIntArrayHelper.Sort(nodes, CmpByEdges, 0, Length(toks));
            result[2] += nodes[Length(toks) div 2];
         end;
      end;
   finally
      edges.Free;
      CmpByEdges.Free;
   end
end;

initialization

   RegisterDay(05, @Run, 1);

end.
