{
  Copyright (c) 2020-2024 Frank Fischer <frank-fischer@shadow-soft.de>

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

{$mode delphi}
{$modeswitch nestedprocvars}
{$H+}

unit Sorting;
interface
uses generics.collections, generics.defaults;

type
   TComparisonNested<T> = function(constref a, b : T) : Integer is nested;

   TDelegatedComparerNested<T> = class(TInterfacedObject, IComparer<T>)
   private
      Fcompare : TComparisonNested<T>;
   public
      constructor Create(compare : TComparisonNested<T>);
      class function Construct(compare : TComparisonNested<T>) : IComparer<T>;
      function Compare(constref a, b : T) : Integer;
   end;

   procedure Sort<T>(var ary : array of T); overload;
   procedure Sort<T>(var ary : array of T; AIndex, ACount: SizeInt); overload;
   procedure Sort<T>(var ary : array of T; comparer : IComparer<T>); overload;
   procedure Sort<T>(var ary : array of T; comparer : IComparer<T>; AIndex, ACount: SizeInt); overload;
   procedure Sort<T>(var ary : array of T; compare : TComparisonFunc<T>); overload;
   procedure Sort<T>(var ary : array of T; compare : TComparisonFunc<T>; AIndex, ACount: SizeInt); overload;
   procedure Sort<T>(var ary : array of T; compare : TComparisonNested<T>); overload;
   procedure Sort<T>(var ary : array of T; compare : TComparisonNested<T>; AIndex, ACount: SizeInt); overload;

   function Sorted<T>(ary : array of T) : TArray<T>; overload;
   function Sorted<T>(ary : array of T; comparer : IComparer<T>) : TArray<T>; overload;
   function Sorted<T>(ary : array of T; compare : TComparisonFunc<T>) : TArray<T>; overload;
   function Sorted<T>(ary : array of T; compare : TComparisonNested<T>) : TArray<T>; overload;

implementation
uses
   SysUtils;

constructor TDelegatedComparerNested<T>.Create(compare : TComparisonNested<T>);
begin
   inherited Create;
   Fcompare := compare;
end;

class function TDelegatedComparerNested<T>.Construct(compare : TComparisonNested<T>) : IComparer<T>;
begin
   result := Create(compare);
end;

function TDelegatedComparerNested<T>.Compare(constref a, b : T) : Integer;
begin
   result := Fcompare(a, b);
end;

procedure Sort<T>(var ary : array of T); overload;
begin
   TArrayHelper<T>.Sort(ary);
end;

procedure Sort<T>(var ary : array of T; AIndex, ACount: SizeInt); overload;
begin
   TArrayHelper<T>.Sort(ary, TComparer<T>.Default, AIndex, ACount);
end;

procedure Sort<T>(var ary : array of T; comparer : IComparer<T>); overload;
begin
   TArrayHelper<T>.Sort(ary, comparer);
end;

procedure Sort<T>(var ary : array of T; comparer : IComparer<T>; AIndex, ACount: SizeInt); overload;
begin
   TArrayHelper<T>.Sort(ary, comparer, AIndex, ACount);
end;

procedure Sort<T>(var ary : array of T; compare : TComparisonFunc<T>); overload;
begin
   Sort<T>(ary, TComparer<T>.Construct(compare));
end;

procedure Sort<T>(var ary : array of T; compare : TComparisonFunc<T>; AIndex, ACount: SizeInt); overload;
begin
   Sort<T>(ary, TComparer<T>.Construct(compare), AIndex, ACount);
end;

procedure Sort<T>(var ary : array of T; compare : TComparisonNested<T>); overload;
begin
   Sort<T>(ary, TDelegatedComparerNested<T>.Construct(compare));
end;

procedure Sort<T>(var ary : array of T; compare : TComparisonNested<T>; AIndex, ACount: SizeInt); overload;
begin
   Sort<T>(ary, TDelegatedComparerNested<T>.Construct(compare), AIndex, ACount);
end;

function Sorted<T>(ary : array of T) : TArray<T>; overload;
var
   i: Integer;
begin
   result := [];
   SetLength(result, Length(ary));
   for i := 0 to High(ary) do result[i] := ary[i];
   Sort<T>(result);
end;

function Sorted<T>(ary : array of T; comparer : IComparer<T>) : TArray<T>; overload;
var
   i: Integer;
begin
   result := [];
   SetLength(result, Length(ary));
   for i := 0 to High(ary) do result[i] := ary[i];
   Sort<T>(result, comparer);
end;

function Sorted<T>(ary : array of T; compare : TComparisonFunc<T>) : TArray<T>; overload;
var
   cmp : IComparer<T>;
begin
   cmp := TComparer<T>.Construct(compare);
   result := Sorted<T>(ary, cmp);
end;

function Sorted<T>(ary : array of T; compare : TComparisonNested<T>) : TArray<T>; overload;
begin
   result := Sorted<T>(ary, TDelegatedComparerNested<T>.Construct(compare));
end;

end.
