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
{$modeswitch advancedrecords}
{$H+}

unit AOC.Generic;

interface

uses SysUtils, Classes;

type

   EReadError = class(Exception);

   TDir = (Up, Right, Down, Left);

   TPos = record
      i: Integer;
      j: Integer;

      procedure SetItem(idx: Integer; value: Integer); inline;
      function GetItem(idx: Integer): Integer; inline;

      function Step(d: TDir): TPos; inline;
      function Step(d: TDir; n: Integer): TPos; inline;
      function TryStep(d: TDir; n, m: Integer; out target: TPos): Boolean; inline;
      function Dist1(p: TPos): Integer;

      class function Create(Ai, Aj: Integer): TPos; static; inline;
      class operator=(a, b: TPos): Boolean; inline;
      class operator+(a: TPos; d: TDir): TPos; inline;
      class operator-(a: TPos; d: TDir): TPos; inline;

      function ToString: String;

      property Items[idx: Integer]: Integer read GetItem write SetItem; default;
   end;

   generic TRow<T> = record
   private
      type
         Ptr = ^T;

   private
      Fitems: Ptr;
      Flen: Cardinal;
      Findex: Cardinal;

   private
      function GetItem(j: Cardinal): T; inline;
      procedure SetItem(j: Cardinal; Value: T); inline;

   public
      function ToString: String;

      property Index: Cardinal read Findex;
      property Items[j: Cardinal]: T read GetItem write SetItem; default;
   end;

   generic TCol<T> = record
   private
      type
         Ptr = ^T;

   private
      Fitems: Ptr;
      Flen: Cardinal;
      Fskip: Cardinal;
      Findex: Cardinal;

   private
      function GetItem(i: Cardinal): T; inline;
      procedure SetItem(i: Cardinal; Value: T); inline;

   public
      function ToString: String;

      property Index: Cardinal read Findex;
      property Items[i: Cardinal]: T read GetItem write SetItem; default;
   end;

   generic TRowEnum<T> = record
   private
      type
         TRow = specialize TRow<T>;

   private
      Fgrid: TObject; // required because forward declaration causes compiler error
      Findex: Cardinal;

   private
      function GetCurrent: TRow;

   public
      function GetEnumerator: TRowEnum; inline;
      function MoveNext: Boolean; inline;

      property Current: TRow read GetCurrent;
   end;

   generic TColEnum<T> = record
   private
      type
         TCol = specialize TCol<T>;

   private
      Fgrid: TObject; // required because forward declaration causes compiler error
      Findex: Cardinal;

   private
      function GetCurrent: TCol;

   public
      function GetEnumerator: TColEnum; inline;
      function MoveNext: Boolean; inline;

      property Current: TCol read GetCurrent;
   end;

   generic TGenGrid<T> = class sealed
   public
      type
         TRow = specialize TRow<T>;
         TCol = specialize TCol<T>;
         TRowEnum = specialize TRowEnum<T>;
         TColEnum = specialize TColEnum<T>;

   private
      Fnrows: Cardinal;
      Fncols: Cardinal;
      Fstart: Cardinal;
      Fskip: Cardinal;
      Fboundary: T;
      FhasBoundary: Boolean;
      Fitems: array of T;

   private
      function GetItem(i, j: Cardinal): T; inline;
      procedure SetItem(i, j: Cardinal; value:T); inline;
      function GetItem(p: TPos): T; inline;
      procedure SetItem(p: TPos; value:T); inline;

      function GetRow(i: Cardinal): TRow; inline;
      function GetCol(j: Cardinal): TCol; inline;

      procedure SetBoundary(ABoundary: T);

   public
      constructor Create(nrows: Cardinal = 0; ncols: Cardinal = 0);
      constructor Create(nrows: Cardinal; ncols: Cardinal; init: T);
      constructor ReadFromStream(input: TStream);

      procedure LoadFromStream(input: TStream);
      procedure NoBoundary;

      function IsValid(p: TPos): Boolean; inline;

      function Find(value: T): TPos; inline;
      function TryFind(value: T; out pos: TPos): Boolean;
      function FindOf(values: array of T): TPos; inline;
      function TryFindOf(values: array of T; out pos: TPos): Boolean;

      // Set all elements (except for the boundary) to `value`.
      procedure Fill(value : T);

      function Rows: TRowEnum;
      function Cols: TColEnum;

      function ToString: String; override;

      property N: Cardinal read Fnrows;
      property M: Cardinal read Fncols;
      property Items[i: Cardinal; j: Cardinal]: T read GetItem write SetItem; default;
      property At[p: TPos]: T read GetItem write SetItem;
      property Row[i: Cardinal]: TRow read GetRow;
      property Col[j: Cardinal]: TCol read GetCol;
      property Boundary: T read Fboundary write SetBoundary;
      property HasBoundary: Boolean read FhasBoundary;
   end;

   generic function GCD<T>(a, b: T): T; 
   generic function GCDExt<T>(a, b: T; out afactor: T; out bfactor: T): T;
   generic function LCM<T>(a, b: T): T;
   generic function CRT<T>(a, m: array of T; out x: T): Boolean;

implementation

{ TPos }

procedure TPos.SetItem(idx: Integer; value: Integer); inline;
begin
   case idx of
      1: i := value;
      2: j := value;
   else
      assert(False, 'Invalid index');
   end
end;

function TPos.GetItem(idx: Integer): Integer; inline;
begin
   case idx of
      1: result := i;
      2: result := j;
   else
      assert(False, 'Invalid index');
      result := 0;
   end
end;

function TPos.Step(d: TDir): TPos; inline;
begin
   result := self;
   case d of
      Up: result.i -= 1;
      Right: result.j += 1;
      Down: result.i += 1;
      Left: result.j -= 1;
   end
end;

function TPos.Step(d: TDir; n: Integer): TPos; inline;
begin
   result := self;
   case d of
      Up: result.i -= n;
      Right: result.j += n;
      Down: result.i += n;
      Left: result.j -= n;
   end
end;

function TPos.TryStep(d: TDir; n, m: Integer; out target: TPos): Boolean; inline;
var
   tgt: TPos;
begin
   tgt := self.Step(d);
   result := (0 <= tgt.i) and (tgt.i < n) and (0 <= tgt.j) and (tgt.j < m);
   if result then target := tgt;
end;

function TPos.Dist1(p: TPos): Integer;
begin
   result := Abs(i - p.i) + Abs(j - p.j);
end;

class function TPos.Create(Ai, Aj: Integer): TPos;
begin
   result.i := Ai;
   result.j := Aj;
end;

class operator TPos.=(a, b: TPos): Boolean; inline;
begin
   result := (a.i = b.i) and (a.j = b.j);
end;

class operator TPos.+(a: TPos; d: TDir): TPos; inline;
begin
   result := a.Step(d);
end;

class operator TPos.-(a: TPos; d: TDir): TPos; inline;
begin
   result := a.Step(d, -1);
end;

function TPos.ToString: String;
begin
   result := Format('(%d,%d)', [i, j]);
end;

{ TRow }

function TRow.GetItem(j: Cardinal): T; inline;
begin
   assert(j < Flen);
   result := Fitems[j];
end;

procedure TRow.SetItem(j: Cardinal; Value: T);
begin
   assert(j < Flen);
   Fitems[j] := Value;
end;

function TRow.ToString: String;
var
   i: Integer;
   s: TStringList = nil;
   str: String;
begin
   try
      s := TStringList.Create;
      s.LineBreak := '';
      for i := 1 to Flen do begin
         writestr(str, self[i - 1]);
         s.Add(str);
      end;
      result := s.Text;
   finally
      s.Free;
   end
end;

{ TCol }
function TCol.GetItem(i: Cardinal): T; inline;
begin
   assert(i < Flen);
   result := Fitems[i * Fskip];
end;

procedure TCol.SetItem(i: Cardinal; Value: T);
begin
   assert(i < Flen);
   Fitems[i * Fskip] := Value;
end;

function TCol.ToString: String;
var
   i: Cardinal;
begin
   SetLength(result, Flen);
   for i := 1 to Flen do
      result[i] := Char(Fitems[(i - 1) * Fskip]);
end;

{ TGenGrid }

constructor TGenGrid.Create(nrows: Cardinal; ncols: Cardinal);
begin
   inherited Create;
   Fnrows := nrows;
   Fncols := ncols;
   Fskip := Fncols + 2;
   SetLength(Fitems, (Fnrows + 2) * (Fncols + 2));
   Fstart := Fskip + 1; // no boundary by default
   FhasBoundary := False;
end;

constructor TGenGrid.Create(nrows: Cardinal; ncols: Cardinal; init: T);
var
   i: Integer;
begin
   Create(nrows, ncols);
   for i := 0 to High(Fitems) do Fitems[i] := init;
end;

constructor TGenGrid.ReadFromStream(input: TStream);
begin
   Create;
   LoadFromStream(input);
end;

procedure TGenGrid.LoadFromStream(input: TStream);
var
   buf: array[0..1023] of char;
   bufbeg, bufend, eol, pos, nread: Integer;

   line: String;
   rws: TStringList = nil;
   i, j: Cardinal;
begin
   Fnrows := 0;
   Fncols := 0;
   try
      Fitems := nil;
      Fncols := 0;
      Fnrows := 0;

      bufbeg := 0;
      bufend := 0;

      pos := input.Position;

      while true do begin
         eol := bufbeg;
         while true do begin
            while (eol < bufend) and (buf[eol] <> LineEnding) do Inc(eol);
            if eol < bufend then break; // found end of line

            // try to fill buffer
            if bufbeg > 0 then begin
               // remove old bytes at the beginning of the buffer
               move(buf[bufbeg], buf[0], bufend - bufbeg);
               Dec(bufend, bufbeg);
               Dec(eol, bufbeg);
               bufbeg := 0;
            end;
            // buffer full ⇒ line too long
            if bufend = Length(buf) then raise Exception.CreateFmt('Line too long (max %d allowed)', [Length(buf)]);
            nread := input.Read(buf[bufend], Length(buf) - bufend);
            if nread = 0 then break; // end of file reached, this is the last line
            bufend += nread;
         end;

         SetString(line, @buf[bufbeg], eol - bufbeg);
         if eol < bufend then
            Inc(pos, eol + 1 - bufbeg)
         else
            Inc(pos, eol - bufbeg);

         bufbeg := eol + 1;

         if line = '' then begin
            input.Position := pos;
            break; // stop at first empty line (or EOF)
         end;
         if rws = nil then begin
            // first line
            Fncols := Length(line) + 2; // number of columns including boundary
            rws := TStringList.Create;
            rws.Capacity := Fncols + 2; // rough estimate
         end else
            if Length(line) + 2 <> Fncols then
               raise EReadError.CreateFmt('Row %d of grid has invalid length (got: %d, expected: %d)',
                                          [rws.Count + 1, Length(line), Fncols - 2]);
         rws.Add(line);
      end;

      // copy lines to array
      SetLength(Fitems, Fncols * (rws.Count + 2));

      for i := 0 to Fncols - 1 do Fitems[i] := Fboundary;
      Inc(Fnrows);

      j := Fncols;
      for line in rws do begin
         Fitems[j] := Fboundary;
         move(line[1], Fitems[j + 1], (Fncols - 2));
         Fitems[j + Fncols - 1] := Fboundary;
         Inc(j, Fncols);

         Inc(Fnrows);
      end;

      for i := 0 to Fncols - 1 do Fitems[j + i] := Fboundary;
      Inc(Fnrows);

      Fskip := Fncols;

      if HasBoundary then
         Fstart := 0
      else begin
         Fstart := Fskip + 1;
         Fnrows -= 2;
         Fncols -= 2;
      end;
   finally
      rws.Free;
   end;
end;

procedure TGenGrid.SetBoundary(ABoundary: T);
var
   i: Integer;
begin
   if not HasBoundary or (ABoundary <> Fboundary) then begin
      Fboundary := Aboundary;
      if not HasBoundary then begin
         Fnrows += 2;
         Fncols += 2;
         Fstart := 0;
         FhasBoundary := True;
      end;
      for i := 0 to Fncols - 1 do Fitems[i] := Fboundary;
      for i := 1 to Fnrows - 2 do begin
         Fitems[i * Fskip] := Fboundary;
         Fitems[(i + 1) * Fskip - 1] := Fboundary;
      end;
      for i := Fskip * (Fnrows - 1) to Fskip * Fnrows - 1 do Fitems[i] := Fboundary;
   end;
end;

procedure TGenGrid.NoBoundary;
begin
   if HasBoundary then begin
      Fnrows -= 2;
      Fncols -= 2;
      Fstart := Fskip + 1;
      FhasBoundary := False;
   end
end;

function TGenGrid.IsValid(p: TPos): Boolean; inline;
begin
   result := (p.i >= 0) and (p.i < N) and (p.j >= 0) and (p.j < M);
end;

function TGenGrid.GetItem(i, j: Cardinal): T;
begin
   assert(i < Fnrows);
   assert(j < Fncols);
   result := Fitems[Fstart + i * Fskip + j];
end;

procedure TGenGrid.SetItem(i, j: Cardinal; value: T);
begin
   assert(i < Fnrows);
   assert(j < Fncols);
   Fitems[Fstart + i * Fskip + j] := value;
end;

function TGenGrid.GetItem(p: TPos): T;
begin
   assert(p.i < Fnrows);
   assert(p.j < Fncols);
   result := Fitems[Fstart + p.i * Fskip + p.j];
end;

procedure TGenGrid.SetItem(p: TPos; value: T);
begin
   assert(p.i < Fnrows);
   assert(p.j < Fncols);
   Fitems[Fstart + p.i * Fskip + p.j] := value;
end;

function TGenGrid.GetRow(i: Cardinal): TRow;
begin
   assert(i < Fnrows);
   result.Findex := i;
   result.Fitems := @Fitems[Fstart + i * Fskip];
   result.Flen := Fncols;
end;

function TGenGrid.GetCol(j: Cardinal): TCol;
begin
   assert(j < Fncols);
   result.Findex := j;
   result.Fitems := @Fitems[Fstart + j];
   result.Flen := Fnrows;
   result.Fskip := Fskip;
end;

function TGenGrid.Rows: TRowEnum;
begin
   result.Fgrid := self;
   result.Findex := 0;
end;

function TGenGrid.Cols: TColEnum;
begin
   result.Fgrid := self;
   result.Findex := 0;
end;

procedure TGenGrid.Fill(value : T);
var
   i, j: Integer;
begin
   for i := 0 to N-1 do
      for j := 0 to M-1 do
         Items[i, j] := value;
end;

function TGenGrid.TryFind(value: T; out pos: TPos): Boolean;
var
   i, j: Integer;
begin
   for i := 0 to N-1 do
      for j := 0 to M-1 do
         if Items[i, j] = value then begin
            pos.i := i;
            pos.j := j;
            exit(True);
         end;
   result := False;
end;

function TGenGrid.Find(value: T): TPos; inline;
begin
   if not TryFind(value, result) then begin
      result.i := -1;
      result.j := -1;
   end
end;

function TGenGrid.TryFindOf(values: array of T; out pos: TPos): Boolean;
var
   i, j: Integer;
   c, x: T;
begin
   for i := 0 to N-1 do
      for j := 0 to M-1 do begin
         x := Items[i, j];
         for c in values do
            if x = c then begin
               pos.i := i;
               pos.j := j;
               exit(True);
            end;
      end;
   result := False;
end;

function TGenGrid.FindOf(values: array of T): TPos; inline;
begin
   if not TryFindOf(values, result) then begin
      result.i := -1;
      result.j := -1;
   end
end;

function TGenGrid.ToString: String;
var
   str: TStringList = nil;
   r: TRow;
begin
   try
      str := TStringList.Create;
      for r in Rows do str.Add(r.ToString);
      result := str.Text;
   finally
      str.Free;
   end;
end;

{ TRowEnum }

function TRowEnum.GetCurrent: TRow;
begin
   result := specialize TGenGrid<T>(Fgrid).Row[Findex - 1];
end;

function TRowEnum.MoveNext: Boolean; inline;
begin
   result := Findex < specialize TGenGrid<T>(Fgrid).N;
   if result then Inc(Findex);
end;

function TRowEnum.GetEnumerator: TRowEnum; inline;
begin
   result := self;
end;

{ TColEnum }

function TColEnum.GetCurrent: TCol;
begin
   result := specialize TGenGrid<T>(Fgrid).Col[Findex - 1];
end;

function TColEnum.MoveNext: Boolean; inline;
begin
   result := Findex < specialize TGenGrid<T>(Fgrid).M;
   if result then Inc(Findex);
end;

function TColEnum.GetEnumerator: TColEnum; inline;
begin
   result := self;
end;

generic function GCD<T>(a, b: T): T; 
var
   r0, r1, r2: T;
   //q: T;
begin
   r0 := a;
   r1 := b;
   while r1 <> 0 do begin
      //q := r0 div r1;
      r2 := r0 mod r1;
      r0 := r1;
      r1 := r2;
   end;

   result := r0;
end;

generic function LCM<T>(a, b: T): T;
begin
   result := a * (b div specialize GCD<T>(a, b));
end;

generic function GCDExt<T>(a, b: T; out afactor: T; out bfactor: T): T;
var
   r0, r1, r2, q: T;
   s0, s1, s2: T;
   t0, t1, t2: T;
begin
   r0 := a;
   r1 := b;
   s0 := 1;
   s1 := 0;
   t0 := 0;
   t1 := 1;
   while r1 <> 0 do begin
      q := r0 div r1;
      r2 := r0 mod r1;
      s2 := s0 - q * s1;
      t2 := s0 - q * t1;

      r0 := r1;
      r1 := r2;
      s0 := s1;
      s1 := s2;
      t0 := t1;
      t1 := t2;
   end;

   result := r0;
   afactor := s0;
   bfactor := t0;
end;

generic function CRT<T>(a, m: array of T; out x: T): Boolean;
var
   a0, m0: T;
   g, s, _, l: T;
   i: Integer;
begin
   assert(Length(a) = Length(m));
   if Length(a) = 0 then begin
      x := 0;
      exit(False);
   end;

   if Length(a) = 1 then begin
      x := a[0];
      exit(True);
   end;

   a0 := a[0];
   m0 := m[0];

   for i := 1 to Length(a) - 1 do begin
      g := specialize GCDExt<T>(m0, m[i], s, _);
      if a0 mod g <> a[i] mod g then begin
         x := 0;
         exit(False);
      end;
      l := m0 * (m[i] div g);
      x := (a0 - (s * m0 * ((a0 - a[i]) div g))) mod l;
      a0 := x;
      m0 := l;
      // there should be a better way to ensure 0 <= a0 < m0
      while a0 < 0 do a0 += m0;
      while a0 >= m0 do a0 -= m0;
   end;

   x := a0;
   exit(True);
end;

end.
