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

unit Day23;

interface

implementation

uses AOC, Classes, SysUtils, StreamEx, generics.collections, Math;

function Run(input: TTextReader): TResult;
type
   TNode = Integer;
   TNodes = array of TNode;
   TEdge = array[1..2] of TNode;
   TEdges = specialize TList<TEdge>;
   TNodeArrayHelper = specialize TArrayHelper<TNode>;
   TMask = array of boolean;
var
   line: String;
   toks: array of String;

   t_nodes: array of Boolean;
   nodes: TNamesBag = nil;
   edges: TEdges = nil;
   adj: array of TNodes;
   degs, inds: array of Integer;
   max_size: Integer = 0;
   max_clq: TNodes;

   procedure MaxClique(fst: TNode; nodes: TNodes; node, sz: Integer; mask: TMask);
   var
      u, v: TNode;
      i, j: Integer;
      orig_mask: TMask;
      orig_sz: Integer;
      r: TBinarySearchResult;
   begin
      if sz <= max_size then exit; // nothing better possible
      if node = Length(nodes) then begin
         // found better solution
         max_size := sz;
         SetLength(max_clq, sz);
         max_clq[0] := fst;
         j := 1;
         for i := 0 to Length(nodes)-1 do
            if mask[i] then begin
               max_clq[j] := nodes[i];
               j += 1;
            end;
         exit;
      end;

      if not mask[node] then begin
         MaxClique(fst, nodes, node + 1, sz, mask);
         exit;
      end;

      // the current node may be used or not, try both
      // we start with keeping the node `u`
      orig_mask := Copy(mask);
      orig_sz := sz;
      u := nodes[node];
      for i := node + 1 to Length(nodes) - 1 do begin
         // unmask nodes that are not connected to `u`.
         v := nodes[i];
         if mask[i] and not TNodeArrayHelper.BinarySearch(adj[u], v, r) then begin
            mask[i] := false;
            sz -= 1;
         end;
      end;
      MaxClique(fst, nodes, node + 1, sz, mask);

      // now try dropping u
      mask := orig_mask;
      sz := orig_sz - 1;
      mask[node] := False;
      MaxClique(fst, nodes, node + 1, sz, mask);
      mask[node] := True; // don't forget to reset the mask
   end;

var
   n: Cardinal;

   u, v, w: TNode;
   e: TEdge;

   i, j: Integer;

   r: TBinarySearchResult;

   cnt1, cnt2, cnt3: Integer;

   clq: TStringList = nil;
   mask: array of Boolean;

   part1: Integer = 0;
begin
   try
      nodes := TNamesBag.Create;
      edges := TEdges.Create;

      for line in input do begin
         toks := line.Split('-');
         e[1] := nodes[toks[0]];
         e[2] := nodes[toks[1]];
         edges.Add(e);
      end;
      n := nodes.Count;

      // mark t-nodes
      SetLength(t_nodes, n);
      for u := 0 to n-1 do t_nodes[u] := nodes.Names[u].StartsWith('t');

      // count the degrees
      SetLength(degs, n);
      SetLength(adj, n);
      for u := 0 to n-1 do degs[u] := 0;
      for e in edges do begin
         degs[e[1]] += 1;
         degs[e[2]] += 1;
      end;
      // adjacency lists
      for u := 0 to n-1 do SetLength(adj[u], degs[u]);
      inds := Copy(degs);
      for e in edges do begin
         Dec(inds[e[1]]);
         adj[e[1]][inds[e[1]]] := e[2];
         Dec(inds[e[2]]);
         adj[e[2]][inds[e[2]]] := e[1];
      end;
      // finally sort the adjacency lists
      for u := 0 to n-1 do TNodeArrayHelper.Sort(adj[u]);

      // *** done reading the graph ***
      cnt1 := 0;
      cnt2 := 0;
      cnt3 := 0;
      for u := 0 to n-1 do begin
         if not t_nodes[u] then continue;
         for i := 0 to Length(adj[u]) - 1 do begin
            v := adj[u][i];
            for j := i+1 to Length(adj[u]) - 1 do begin
               w := adj[u][j];
               if not TNodeArrayHelper.BinarySearch(adj[v], w, r) then continue;
               if t_nodes[v] and t_nodes[w] then cnt3 += 1
               else if t_nodes[v] or t_nodes[w] then cnt2 += 1
               else cnt1 += 1;
            end
         end;
      end;

      part1 := cnt1 + (cnt2 div 2) + (cnt3 div 3);

      max_size := 0;
      for u := 0 to n-1 do begin
         SetLength(mask, Length(adj[u]));
         for i := 0 to Length(mask) - 1 do mask[i] := True;
         MaxClique(u, adj[u], 0, Length(mask)+1, mask);
      end;

      clq := TStringList.Create;
      for u in max_clq do clq.Add(nodes.Names[u]);
      clq.Sort;
      clq.LineBreak := ',';
      clq.SkipLastLineBreak := True;

      result[1] := part1;
      result[2] := clq.Text;
   finally
      nodes.Free;
      edges.Free;
      clq.Free;
   end
end;

initialization

   RegisterDay(23, @Run, 1);

end.
