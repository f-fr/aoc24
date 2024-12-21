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

uses AOC, AOC.Generic, SysUtils, Classes, priqueue;

type     
   TButton = (bUp, bRight, bDown, bLeft, bAct);

   TNum = 0..10; // 10 means 'A'

   TButtonMappingClass = class of TButtonMapping;

   TButtonMapping = class
      class function ButtonToPos(but : Integer) : TPos; virtual; abstract;
      class function TryPosToButton(pos : TPos; out but : Integer) : Boolean; virtual; abstract;
      class function TryPress(var but : Integer; bctrl : TButton) : Boolean;
   end;

   TCtrlButtonMapping = class(TButtonMapping)
      class function ButtonToPos(but : Integer) : TPos; override;
      class function TryPosToButton(pos : TPos; out but : Integer) : Boolean; override;
   end;

   TNumButtonMapping = class(TButtonMapping)
      class function ButtonToPos(but : Integer) : TPos; override;
      class function TryPosToButton(pos : TPos; out but : Integer) : Boolean; override;
   end;

{ TButtonMapping }
class function TButtonMapping.TryPress(var but : Integer; bctrl: TButton) : Boolean;
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
   result := TryPosToButton(p, but);
end;

{ TCtrlButtonMapping }
class function TCtrlButtonMapping.ButtonToPos(but : Integer): TPos;
begin
   case TButton(but) of
      bUp: result := TPos.Create(0, 1);
      bRight: result := TPos.Create(1, 2);
      bDown: result := TPos.Create(1, 1);
      bLeft: result := TPos.Create(1, 0);
      bAct: result := TPos.Create(0, 2);
   end;
end;

class function TCtrlButtonMapping.TryPosToButton(pos : TPos; out but : Integer) : Boolean;
begin
   result := True;
   if pos = TPos.Create(0, 1) then but := Integer(bUp)
   else if pos = TPos.Create(0, 2) then but := Integer(bAct)
   else if pos = TPos.Create(1, 0) then but := Integer(bLeft)
   else if pos = TPos.Create(1, 1) then but := Integer(bDown)
   else if pos = TPos.Create(1, 2) then but := Integer(bRight)
   else result := false;
end;

{ TNumButtonMapping }
class function TNumButtonMapping.ButtonToPos(but : Integer): TPos;
begin
   case TNum(but) of
      0: result := TPos.Create(3, 1);
      1: result := TPos.Create(2, 0);
      2: result := TPos.Create(2, 1);
      3: result := TPos.Create(2, 2);
      4: result := TPos.Create(1, 0);
      5: result := TPos.Create(1, 1);
      6: result := TPos.Create(1, 2);
      7: result := TPos.Create(0, 0);
      8: result := TPos.Create(0, 1);
      9: result := TPos.Create(0, 2);
      10: result := TPos.Create(3, 2);
   end;
end;

class function TNumButtonMapping.TryPosToButton(pos : TPos; out but : Integer) : Boolean;
begin
   result := True;
   if pos = TPos.Create(3, 2) then but := 10
   else if pos = TPos.Create(0, 0) then but := 7
   else if pos = TPos.Create(0, 1) then but := 8
   else if pos = TPos.Create(0, 2) then but := 9
   else if pos = TPos.Create(1, 0) then but := 4
   else if pos = TPos.Create(1, 1) then but := 5
   else if pos = TPos.Create(1, 2) then but := 6
   else if pos = TPos.Create(2, 0) then but := 1
   else if pos = TPos.Create(2, 1) then but := 2
   else if pos = TPos.Create(2, 2) then but := 3
   else if pos = TPos.Create(3, 1) then but := 0
   else result := false;
end;

function Run(input: TStrings): TResult;
type
   TCost = Int64;
   // the cost[b1,b2] of performing b2 immediately after b1
   TCosts = array[TButton, TButton] of TCost;
   TNode = record
      pos: Integer;
      bctrl: TButton;
   end;
   TNodeQueue = specialize TGPriQueue<TNode, TCost>;

var
   q : TNodeQueue = nil;

   function Solve(mapping: TButtonMappingClass;
                  s_but, t_but : Integer; n: Integer; constref cost: TCosts) : Int64;
   var
      cur, nxt: TNode;
      dists: array[0..10, TButton] of TCost;
      i: Integer;
      bctrl: TButton;
      c, d: TCost;
   begin
      if s_but = t_but then exit(1);

      q.Clear;
      cur.pos := s_but;
      cur.bctrl := bAct; // on the control level we start at 'A'
      q.Push(cur, 0);
      for i := 0 to n-1 do for bctrl in TButton do dists[i, bctrl] := High(TCost);
      dists[s_but, bAct] := 0;
      while q.TryPopMin(cur, d) do begin
         if (cur.pos = t_but) and (cur.bctrl = bAct) then break;
         for bctrl in TButton do begin
            c := cost[cur.bctrl, bctrl]; // cost to press bctrl after cur[2]
            nxt := cur;
            if not mapping.TryPress(nxt.pos, bctrl) then continue;
            nxt.bctrl := bctrl;
            if d + c < dists[nxt.pos, nxt.bctrl] then begin
               // found a better path to nxt
               dists[nxt.pos, nxt.bctrl] := d +  c;
               q.Push(nxt, d + c);
            end;
         end;
      end;
      result := dists[t_but, bAct];
   end;

var
   line: String;
   ch: Char;

   nxtcost, cost: TCosts;
   b1, b2: TButton;

   s, t: TNum;

   sum: TCost;

   k: Integer;

   part1: Integer = 0;
   part2: Int64 = 0;
begin
   try
      // on the lowest level, the cost is exactly 1 (just push b2)
      for b1 in TButton do for b2 in TButton do cost[b1, b2] := 1;

      q := TNodeQueue.Create;

      for k := 1 to 25 do begin
         // for the next level, we use dijkstra to compute the shortest sequence
         for b1 in TButton do 
            for b2 in TButton do 
               nxtcost[b1, b2] := Solve(TCtrlButtonMapping, Ord(b1), Ord(b2), Ord(High(TButton)) - Ord(Low(TButton)) + 1, cost);
         cost := nxtcost;

         if not ((k = 2) or (k = 25)) then continue;

         // now do the dijkstra on the highest level (the numeric pad) but through the sequence
         // of numbers of the input
         for line in input do begin
            sum := 0;
            s := 10; // 'A'
            for ch in line do begin
               case ch of
                  '0'..'9': t := Ord(ch) - Ord('0');
                  'A': t := 10;
               else
                  raise Exception.CreateFmt('Invalid number %c', [ch]);
               end;
               sum += Solve(TNumButtonMapping, s, t, Ord(High(TNum)) - Ord(Low(TNum)) + 1, cost);
               s := t;
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
   end
end;

initialization

   RegisterDay(21, @Run, 1);

end.
