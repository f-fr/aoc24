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

   generic TRow<T> = record
   private
      type
         Ptr = ^T;

   private
      Fitems: Ptr;
      Flen: Cardinal;
      Findex: Cardinal;

   private
      function Get(j: Cardinal): T; inline;

   public
      function ToString: String;

      property Index: Cardinal read Findex;
      property Items[j: Cardinal]: T read Get;
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
      function Get(i: Cardinal): T; inline;

   public
      function ToString: String;

      property Index: Cardinal read Findex;
      property Items[i: Cardinal]: T read Get;
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

   generic TGenGrid<T> = class
   public
      type
         TRow = specialize TRow<T>;
         TCol = specialize TCol<T>;
         TRowEnum = specialize TRowEnum<T>;
         TColEnum = specialize TColEnum<T>;

   private
      Fnrows: Cardinal;
      Fncols: Cardinal;
      Fitems: array of T;

   private
      function GetItem(i, j: Cardinal): T; inline;
      procedure SetItem(i, j: Cardinal; value:T); inline;

      function GetRow(i: Cardinal): TRow; inline;
      function GetCol(j: Cardinal): TCol; inline;

   public
      constructor Create(nrows: Cardinal = 0; ncols: Cardinal = 0);
      constructor ReadFromStream(input: TStream);
      constructor ReadFromStreamWithBoundary(input: TStream; Boundary: T);

      procedure LoadFromStream(input: TStream);
      procedure LoadFromStreamWithBoundary(input: TStream; Boundary: T);

      function Rows: TRowEnum;
      function Cols: TColEnum;

      function ToString: String; override;

      property N: Cardinal read Fnrows;
      property M: Cardinal read Fncols;
      property Items[i: Cardinal; j: Cardinal]: T read GetItem write SetItem;
      property Row[i: Cardinal]: TRow read GetRow;
      property Col[j: Cardinal]: TCol read GetCol;
   end;

   generic function GCD<T>(a, b: T): T; 
   generic function GCDExt<T>(a, b: T; out afactor: T; out bfactor: T): T;
   generic function LCM<T>(a, b: T): T;
   generic function CRT<T>(a, m: array of T; out x: T): Boolean;

implementation

{ TRow }

function TRow.Get(j: Cardinal): T; inline;
begin
   assert(j < Flen);
   result := Fitems[j];
end;

function TRow.ToString: String;
begin
   SetString(result, Fitems, Flen);
   SetCodePage(RawByteString(result), CP_ACP, True);
end;

{ TCol }
function TCol.Get(i: Cardinal): T; inline;
begin
   assert(i < Flen);
   result := Fitems[i * Fskip];
end;

function TCol.ToString: String;
var
   i: Cardinal;
begin
   SetLength(result, Flen);
   for i := 1 to Flen do
      result[i] := Fitems[(i - 1) * Fskip];
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

{ TGenGrid }

constructor TGenGrid.Create(nrows: Cardinal; ncols: Cardinal);
begin
   inherited Create;
   Fnrows := nrows;
   Fncols := ncols;
   SetLength(Fitems, Fnrows * Fncols);
end;

constructor TGenGrid.ReadFromStream(input: TStream);
begin
   Create;
   LoadFromStream(input);
end;

constructor TGenGrid.ReadFromStreamWithBoundary(input: TStream; Boundary: T);
begin
   Create;
   LoadFromStreamWithBoundary(input, Boundary);
end;

procedure TGenGrid.LoadFromStream(input: TStream);
var
   lines: TStringList = nil;
   line: String;
begin
   Fnrows := 0;
   Fncols := 0;
   try
      lines := TStringList.Create;
      lines.LoadFromStream(input, True); // ignore the encoding
      if lines.Count = 0 then begin
         Fitems := nil;
         exit;
      end;
      Fncols := Length(lines[0]);

      SetLength(Fitems, Fncols * lines.Count);

      for line in lines do begin
         if Length(line) = 0 then break;

         if Length(line) <> Fncols then
            raise EReadError.CreateFmt('Row %d of grid has invalid length (got: %d, expected: %d)',
                                       [Fnrows + 1, Length(line), Fncols]);

         move(line[1], Fitems[Fnrows * Fncols], Fncols);

         Inc(Fnrows);
      end;
   finally
      lines.Free;
   end;
end;

procedure TGenGrid.LoadFromStreamWithBoundary(input: TStream; boundary: T);
var
   lines: TStringList = nil;
   line: String;
   i, j: Cardinal;
begin
   Fnrows := 0;
   Fncols := 0;
   try
      lines := TStringList.Create;
      lines.LoadFromStream(input, True); // ignore the encoding
      if lines.Count = 0 then begin
         Fitems := nil;
         exit;
      end;
      Fncols := Length(lines[0]) + 2;

      SetLength(Fitems, Fncols * (lines.Count + 2));
      for i := 0 to Fncols - 1 do Fitems[i] := boundary;
      Inc(Fnrows);

      j := Fncols;
      for line in lines do begin
         if Length(line) = 0 then break;

         if Length(line) + 2 <> Fncols then
            raise EReadError.CreateFmt('Row %d of grid has invalid length (got: %d, expected: %d)',
                                       [Fnrows, Length(line), Fncols - 2]);

         Fitems[j] := boundary;
         move(line[1], Fitems[j + 1], (Fncols - 2));
         Fitems[j + Fncols - 1] := boundary;
         Inc(j, Fncols);

         Inc(Fnrows);
      end;

      for i := 0 to Fncols - 1 do Fitems[j + i] := Boundary;
      Inc(Fnrows);
   finally
      lines.Free;
   end;
end;

function TGenGrid.GetItem(i, j: Cardinal): T;
begin
   assert(i < Fnrows);
   assert(j < Fncols);
   result := Fitems[i * Fncols + j];
end;

procedure TGenGrid.SetItem(i, j: Cardinal; value: T);
begin
   assert(i < Fnrows);
   assert(j < Fncols);
   Fitems[i * Fncols + j] := value;
end;

function TGenGrid.GetRow(i: Cardinal): TRow;
begin
   assert(i < Fnrows);
   result.Findex := i;
   result.Fitems := @Fitems[i * Fncols];
   result.Flen := Fncols;
end;

function TGenGrid.GetCol(j: Cardinal): TCol;
begin
   assert(j < Fncols);
   result.Findex := j;
   result.Fitems := @Fitems[j];
   result.Flen := Fnrows;
   result.Fskip := Fncols;
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

generic function GCD<T>(a, b: T): T; 
var
   r0, r1, r2, q: T;
begin
   r0 := a;
   r1 := b;
   while r1 <> 0 do begin
      q := r0 div r1;
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
      exit(False);
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
