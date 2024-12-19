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

unit Day19;

interface

implementation

uses AOC, Classes, SysUtils, Math, Strings;

function Run(input: TStrings): TResult;
var
   patterns: TStringArray;
   possible: array of Int64 = nil;

   str, pat: String;
   i, ii: Integer;

   part1: Integer = 0;
   part2: Int64 = 0;
begin
   patterns := input[0].Split([',', ' '], TStringSplitOptions.ExcludeEmpty);

   for i := 2 to input.Count - 1 do begin
      str := input[i];
      SetLength(possible, Max(str.Length + 1, Length(possible)));
      possible[0] := 0;
      possible[str.Length] := 1;
      for ii := str.Length-1 downto 0 do begin
         possible[ii] := 0;
         for pat in patterns do begin
            if (pat.Length + ii <= str.Length)
               and (possible[pat.Length + ii] > 0)
               and (strlcomp(@pat[1], @str[ii+1], pat.Length) = 0)
            then begin
               possible[ii] += possible[ii + pat.Length];
            end;
         end;
      end;
      if possible[0] > 0 then part1 += 1;
      part2 += possible[0];
   end;
   
   result[1] := part1;
   result[2] := part2;
end;

type
   TStripe = 1..5;
   TPattern = array of TStripe;
   TTowel = array of TStripe;

   TTrie = class
   private
      type TNode = record
         next: array[TStripe] of Integer;
         valid: Boolean;
      end;

   private
      Fnodes: array of TNode;
      Fsize: Integer;

   public
      constructor Create;

      procedure Add(pattern : TPattern);
      function Root: Integer; inline;
      function IsValid(node: Integer): Boolean; inline;
      function Next(node: Integer; s: TStripe): Integer; inline;
   end;

{ TTrie }
constructor TTrie.Create;
var
   s: TStripe;
begin
   Fsize := 1;
   SetLength(Fnodes, 128);
   // add the root node
   for s in TStripe do Fnodes[0].next[s] := -1;
   Fnodes[0].valid := False;
end;

procedure TTrie.Add(pattern : TPattern);
var
   i: Integer = 0;
   node: Integer = 0;
   s: TStripe;
begin
   // traverse existing nodes
   while (i < Length(pattern)) and (Fnodes[node].next[pattern[i]] >= 0) do begin
      node := Fnodes[node].next[pattern[i]];
      Inc(i);
   end;
   // maybe add missing nodes
   while i < Length(pattern) do begin
      if Fsize = Length(Fnodes) then SetLength(Fnodes, Length(Fnodes) * 2);
      Fnodes[Fsize].Valid := False;
      for s in TStripe do Fnodes[Fsize].Next[s] := -1;
      Fnodes[node].Next[pattern[i]] := Fsize;
      node := Fsize;
      Inc(i);
      Inc(Fsize);
   end;

   Fnodes[node].Valid := True // node found
end;

function TTrie.Root: Integer; inline;
begin
   result := 0;
end;

function TTrie.IsValid(node: Integer): Boolean; inline;
begin
   result := (node >= 0) and Fnodes[node].Valid;
end;

function TTrie.Next(node: Integer; s: TStripe): Integer; inline;
begin
   result := Fnodes[node].next[s];
end;

function ConvertPattern(pattern: String) : TPattern;
var
   i: Integer;
begin
   SetLength(result, Length(pattern));
   for i := 0 to pattern.Length - 1 do
      case pattern.Chars[i] of
         'w': result[i] := Low(TStripe) + 0;
         'u': result[i] := Low(TStripe) + 1;
         'b': result[i] := Low(TStripe) + 2;
         'r': result[i] := Low(TStripe) + 3;
         'g': result[i] := Low(TStripe) + 4;
      else
         raise Exception.CreateFmt('Invalid stripe color: %c', [pattern.Chars[i]]);
      end
end;

function Run2(input: TStrings): TResult;
var
   patterns: TTrie = nil;
   possible: array of Int64 = nil;
   toks: TStringArray;

   str: TPattern;
   i, ii, iii, node: Integer;

   part1: Integer = 0;
   part2: Int64 = 0;
begin
   try
      toks := input[0].Split([',', ' '], TStringSplitOptions.ExcludeEmpty);
      patterns := TTrie.Create;
      for i := 0 to High(toks) do patterns.Add(ConvertPattern(toks[i]));

      for i := 2 to input.Count - 1 do begin
         str := ConvertPattern(input[i]);
         SetLength(possible, Max(Length(str) + 1, Length(possible)));
         possible[Length(str)] := 1;
         for ii := Length(str)-1 downto 0 do begin
            possible[ii] := 0;
            node := patterns.Root;
            for iii := ii to High(str) do begin
               node := patterns.Next(node, str[iii]);
               if node < 0 then break;
               if patterns.IsValid(node) then possible[ii] += possible[iii + 1];
            end;
         end;
         if possible[0] > 0 then part1 += 1;
         part2 += possible[0];
      end;

      result[1] := part1;
      result[2] := part2;
   finally
      patterns.Free;
   end;
end;

initialization

   RegisterDay(19, @Run, 1);
   RegisterDay(19, @Run2, 2);

end.
