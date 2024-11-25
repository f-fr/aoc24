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

unit AOC;

interface

uses AOC.Generic, generics.collections, Classes, StreamEx;

type

   TResult = array[1..2] of Int64;

   TDay = 1..25;

   { 0 means "all days" }
   TDayOrZero = 0..25;

   TNamesBag = class
   private
      type
         TNamesDict = specialize THashMap<String, Cardinal>;

   private
      Fnames: TNamesDict;

   private
      function GetOrAddName(const AName: String): Cardinal; inline;

   public
      constructor Create;
      destructor Destroy; override;

      property Items[AName: String]: Cardinal read GetOrAddName; default;
   end;

   TGrid = specialize TGenGrid<char>;

   function GCD(a, b: Cardinal): Cardinal; inline;
   function GCDExt(a, b: Integer; out afactor, bfactor: Integer): Integer; inline;
   function LCM(a, b: Cardinal): Cardinal; inline;
   function CRT(a, m: array of Integer; out x: Integer): Boolean;

   { Runner specific helpers }

type
   TStreamRunFunction = function (input: TStream): TResult;
   TStringsRunFunction = function (input: TStrings): TResult;
   TTextReaderRunFunction = function (input: TTextReader): TResult;
   TGridRunFunction = function (input: TGrid): TResult;

   TRunner = class
      function Run(stream: TStream): TResult; virtual; abstract;
   end;

   TStreamRunner = class(TRunner)
   private
      Frun: TStreamRunFunction;

   public
      constructor Create(ARun: TStreamRunFunction);
      function Run(input: TStream): TResult; override;
   end;

   TTextReaderRunner = class(TRunner)
   private
      Frun: TTextReaderRunFunction;

   public
      constructor Create(ARun: TTextReaderRunFunction);
      function Run(input: TStream): TResult; override;
   end;

   TStringsRunner = class(TRunner)
   private
      Frun: TStringsRunFunction;

   public
      constructor Create(ARun: TStringsRunFunction);
      function Run(input: TStream): TResult; override;
   end;

   TGridRunner = class(TRunner)
   private
      Frun: TGridRunFunction;

   public
      constructor Create(ARun: TGridRunFunction);
      function Run(input: TStream): TResult; override;
   end;

   TTextReaderEnumerator = record
   private
      Freader: TTextReader;
      Fline: String;

   public
      function MoveNext : Boolean;
      property Current: String read Fline;
   end;

   operator enumerator(AReader: TTextReader): TTextReaderEnumerator;

   procedure RegisterDay(day: TDay; run: TStreamRunFunction; Version: Integer = 1);
   procedure RegisterDay(day: TDay; run: TTextReaderRunFunction; Version: Integer = 1);
   procedure RegisterDay(day: TDay; run: TStringsRunFunction; Version: Integer = 1);
   procedure RegisterDay(day: TDay; run: TGridRunFunction; Version: Integer = 1);
   procedure RunDay(day: TDayOrZero; version: Integer; const inputFileName: String);

implementation

uses Aoc.Tests, SysUtils, StrUtils, DateUtils, Math
     {$ifdef TESTING}, fpcUnit, TestRegistry{$endif};

{ TNamesBag }

constructor TNamesBag.Create;
begin
   inherited;
   Fnames := TNamesDict.Create;
end;

destructor TNamesBag.Destroy;
begin
   Fnames.Free;
   inherited;
end;

function TNamesBag.GetOrAddName(const AName: String): Cardinal;
begin
   if Fnames.TryGetValue(AName, result) then exit;
   result := Fnames.Count;
   Fnames.Add(AName, result);
end;


function GCD(a, b: Cardinal): Cardinal; inline;
begin
   result := specialize GCD<Cardinal>(a, b);
end;

function GCDExt(a, b: Integer; out afactor, bfactor: Integer): Integer; inline;
begin
   result := specialize GCDExt<Integer>(a, b, afactor, bfactor);
end;

function LCM(a, b: Cardinal): Cardinal; inline;
begin
   result := specialize LCM<Cardinal>(a, b);
end;

function CRT(a, m: array of Integer; out x: Integer): Boolean;
begin
   result := specialize CRT<Integer>(a, m, x);
end;

{ TStreamRunner }

constructor TStreamRunner.Create(ARun: TStreamRunFunction);
begin
   Frun := Arun;
end;

function TStreamRunner.Run(input: TStream): TResult;
begin
   result := Frun(input);
end;

{ TTextReaderRunner }

constructor TTextReaderRunner.Create(ARun: TTextReaderRunFunction);
begin
   Frun := Arun;
end;

function TTextReaderRunner.Run(input: TStream): TResult;
var
   reader: TTextReader = nil;
begin
   try
      reader := TStreamReader.Create(input);
      result := Frun(reader);
   finally
      reader.Free;
   end;
end;

{ TStringsRunner }

constructor TStringsRunner.Create(ARun: TStringsRunFunction);
begin
   Frun := Arun;
end;

function TStringsRunner.Run(input: TStream): TResult;
var
   strings: TStringList = nil;
begin
   try
      strings := TStringList.Create;
      strings.LoadFromStream(input);
      result := Frun(strings);
   finally
      strings.Free;
   end;
end;

{ TGridRunner }

constructor TGridRunner.Create(ARun: TGridRunFunction);
begin
   Frun := Arun;
end;

function TGridRunner.Run(input: TStream): TResult;
var
   grid: TGrid = nil;
begin
   try
      grid := TGrid.ReadFromStream(input);
      result := Frun(grid);
   finally
      grid.Free;
   end;
end;

{ TTextReaderEnumerator }

function TTextReaderEnumerator.MoveNext: Boolean;
begin
   result := not Freader.Eof;
   if result then Freader.ReadLine(Fline);
end;

operator enumerator(AReader: TTextReader): TTextReaderEnumerator;
begin
   result.Freader := AReader;
end;

var
   days: array[TDay] of array of TRunner;

procedure RegisterDay(day: TDay; runner: TRunner; Version: Integer);
begin
   assert(Version > 0);
   SetLength(days[day], Max(Version, Length(days[day])));
   days[day][Version-1] := runner;
   AddTest(day, runner);
end;

procedure RegisterDay(day: TDay; run: TStreamRunFunction; Version: Integer);
begin
   RegisterDay(day, TStreamRunner.Create(run), Version);
end;

procedure RegisterDay(day: TDay; run: TTextReaderRunFunction; Version: Integer);
begin
   RegisterDay(day, TTextReaderRunner.Create(run), Version);
end;

procedure RegisterDay(day: TDay; run: TStringsRunFunction; Version: Integer);
begin
   RegisterDay(day, TStringsRunner.Create(run), Version);
end;

procedure RegisterDay(day: TDay; run: TGridRunFunction; Version: Integer);
begin
   RegisterDay(day, TGridRunner.Create(run), Version);
end;

procedure RunDay(day: TDayOrZero; Version: Integer; const inputFileName: String);
var
   input: TStream = nil;
   res: TResult;
   d, ver: Integer;

   starttime, endtime: TDateTime;
   dayTime: Double = 0;
   bestDayTime: Double = 0;
   totalTime: Double = 0;
begin
   if (day < 0) or (day > Length(days)) then
      raise EArgumentOutOfRangeException.CreateFmt('Unknown day: %d', [day]);
   if day > 0 then begin
      if (Version < 0) or (Version > Length(days[day])) or ((Version > 0) and not assigned(days[day][Version-1])) then
         raise Exception.CreateFmt('No runner for day %d, version %d found', [day, Version]);
   end else begin
      if Version <> 0 then
         raise EArgumentException.Create('Version must be 0 if running all days');
   end;

   for d := IfThen(day > 0, day, Low(days)) to IfThen(day > 0, day, High(days)) do begin
      bestDayTime := Infinity;
      for ver := 1 to Length(days[d]) do begin
         if (Version > 0) and (Version <> ver) then continue;

         try
            if inputFileName <> '' then
               input := TFileStream.Create(inputFileName, fmOpenRead)
            else
               input := TFileStream.Create(Format('input/%.2d/input1.txt', [d]), fmOpenRead);
            starttime := Now;
            res := days[d][ver-1].Run(input);
            endtime := Now;
            dayTime := MillisecondsBetween(starttime, endtime) / 1000;
            writeln(Format('day: %2d %s  part1: %10d  part2: %10d   time:%.3g',
                           [d,
                            IfThen(ver > 1, Format('v%d', [ver]), '  '),
                            res[1], res[2], dayTime]));
            bestDayTime := Min(bestDayTime, dayTime);
         finally
            FreeAndNil(input);
         end
      end;
      if bestDayTime < Infinity then totalTime += bestDayTime;
   end;

   if day = 0 then writeln(Format('Total time: %.3g', [totalTime]));
end;

{$ifdef TESTING}
{$include tests_aoc.inc}
{$endif TESTING}

var
   d: TDay;
   v: Integer;

initialization

{$ifdef TESTING}
   RegisterTests;
{$endif}

finalization

   for d in TDay do begin
      for v := Low(days[d]) to High(days[d]) do
         days[d][v].Free;
   end;

end.
