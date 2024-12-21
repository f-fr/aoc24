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

unit Day21;

interface

implementation

uses AOC, AOC.Generic, SysUtils, Classes, StreamEx, generics.collections, priqueue;

type     
   TButton = (bUp, bRight, bDown, bLeft, bAct);

function TryButtonOf(pos : TPos; out but : TButton) : Boolean;
begin
   result := true;
   if pos = TPos.Create(0, 1) then but := bUp
   else if pos = TPos.Create(0, 2) then but := bAct
   else if pos = TPos.Create(1, 0) then but := bLeft
   else if pos = TPos.Create(1, 1) then but := bDown
   else if pos = TPos.Create(1, 2) then but := bRight
   else result := false;
end;

function ButtonToPos(but : TButton): TPos;
begin
   case but of
      bUp: result := TPos.Create(0, 1);
      bRight: result := TPos.Create(1, 2);
      bDown: result := TPos.Create(1, 1);
      bLeft: result := TPos.Create(1, 0);
      bAct: result := TPos.Create(0, 2);
   end;
end;

function NumberToPos(num : Char): TPos;
begin
   case num of
      '0': result := TPos.Create(3, 1);
      '1': result := TPos.Create(2, 0);
      '2': result := TPos.Create(2, 1);
      '3': result := TPos.Create(2, 2);
      '4': result := TPos.Create(1, 0);
      '5': result := TPos.Create(1, 1);
      '6': result := TPos.Create(1, 2);
      '7': result := TPos.Create(0, 0);
      '8': result := TPos.Create(0, 1);
      '9': result := TPos.Create(0, 2);
      'A': result := TPos.Create(3, 2);
   else
      raise Exception.CreateFmt('Invalid number: %c', [num]);
   end;
end;

function TryPress(var but : TButton; bctrl: TButton) : Boolean;
var
   p: TPos;
begin
   p := ButtonToPos(but);
   result := True;
   case bctrl of
      bUp: p += Up;
      bRight: p += Right;
      bDown: p += Down;
      bLeft: p += Left;
      bAct: exit;
   end;

   result := TryButtonOf(p, but);
end;

function TryPress(var pos : TPos; bctrl: TButton) : Boolean;
var
   p: TPos;
begin
   p := pos;
   result := True;
   case bctrl of
      bUp: p += Up;
      bRight: p += Right;
      bDown: p += Down;
      bLeft: p += Left;
      bAct: exit;
   end;

   result := (p.i >= 0) and (p.i <= 3) and (p.j >= 0) and (p.j <= 2) and (p <> TPos.Create(3,0));
   if result then pos := p;
end;

function Run(input: TStrings): TResult;
type
   TCost = Int64;
   // the cost[b1,b2] of performing b2 immediately after b1
   TCosts = array[TButton, TButton] of TCost;

   TNode = array[1..2] of TButton;
   TNodeQueue = specialize TGPriQueue<TNode, TCost>;

   TNumNode = record
      pos: TPos;
      bctrl: TButton;
   end;
   TNumDistDict = specialize TDictionary<TNumNode, TCost>;
   TNumNodeQueue = specialize TGPriQueue<TNumNode, TCost>;

var
   line: String;
   ch: Char;

   nxtcost, cost: TCosts;
   b1, b2, bctrl, x1, x2: TButton;

   q: TNodeQueue = nil;
   dists: TCosts;
   cur, nxt: TNode;

   num_q: TNumNodeQueue = nil;
   num_dists: TNumDistDict = nil;
   num_cur, num_nxt: TNumNode;
   t: TPos;

   c, d, dnxt, sum: TCost;

   k: Integer;

   part1: Integer = 0;
   part2: Int64 = 0;
begin
   try
      // on the lowest level, the cost is exactly 1 (just push b2)
      for b1 in TButton do for b2 in TButton do cost[b1, b2] := 1;

      q := TNodeQueue.Create;
      num_q := TNumNodeQueue.Create;
      num_dists := TNumDistDict.Create;

      for k := 1 to 25 do begin
         // for the next level, we use dijkstra to compute the shortest sequence
         for b1 in TButton do for b2 in TButton do nxtcost[b1, b2] := High(TCost);

         for b1 in TButton do begin
            for b2 in TButton do begin
               q.Clear;
               cur[1] := b1;
               cur[2] := bAct; // on the control level we start at 'A'
               q.Push(cur, 0);
               for x1 in TButton do for x2 in TButton do dists[x1, x2] := High(TCost);
               dists[b1, bAct] := 1;
               while q.TryPopMin(cur, d) do begin
                  if (cur[1] = b2) and (cur[2] = bAct) then break;
                  for bctrl in TButton do begin
                     c := cost[cur[2], bctrl]; // cost to press bctrl after cur[2]
                     nxt := cur;
                     if not TryPress(nxt[1], bctrl) then continue;
                     nxt[2] := bctrl;
                     if d + c < dists[nxt[1], nxt[2]] then begin
                        // found a better path nxt
                        dists[nxt[1], nxt[2]] := d +  c;
                        q.Push(nxt, d + c);
                     end;
                  end;
               end;
               nxtcost[b1, b2] := dists[b2, bAct];
            end;
         end;
         cost := nxtcost;

         if not ((k = 2) or (k = 25)) then continue;

         // now do the dijkstra on the highest level (the numeric pad) but through the sequence
         // of numbers of the input
         for line in input do begin
            sum := 0;
            num_cur.pos := TPos.Create(3, 2);
            num_cur.bctrl := bAct; // on the control level we start at 'A'
            for ch in line do begin
               t := NumberToPos(ch);

               num_q.Clear;
               num_q.Push(num_cur, 0);
               num_dists.Clear;
               num_dists.Add(num_cur, 0);
               while num_q.TryPopMin(num_cur, d) do begin
                  if (num_cur.pos = t) and (num_cur.bctrl = bAct) then begin
                     // found the target, not that `num_cur` *is* the start node for
                     // the next stop
                     sum += d;
                     break;
                  end;

                  for bctrl in TButton do begin
                     c := cost[num_cur.bctrl, bctrl]; // cost to press bctrl after cur.bctrl
                     num_nxt := num_cur;
                     if not TryPress(num_nxt.pos, bctrl) then continue;
                     num_nxt.bctrl := bctrl;
                     if (not num_dists.TryGetValue(num_nxt, dnxt)) or (d + c < dnxt) then begin
                        // found a better path to num_nxt
                        num_dists.AddOrSetValue(num_nxt, d + c);
                        num_q.Push(num_nxt, d + c);
                     end;
                  end;
               end;
            end;

            if k = 2 then
               part1 += sum * line.TrimLeft('0').TrimRight('A').ToInteger
            else
               part2 += sum * line.TrimLeft('0').TrimRight('A').ToInt64;
         end;
      end;

      result[1] := part1;
      result[2] := part2;
   finally
      q.Free;
      num_q.Free;
      num_dists.Free;
   end
end;

initialization

   RegisterDay(21, @Run, 1);

end.
