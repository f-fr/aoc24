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
{$modeswitch typehelpers}
{$H+}

unit AOC;

interface

uses AOC.Generic, generics.collections, Classes, StreamEx, EasyCSV;

type
   TResult = array[1..2] of variant;

   TDay = 1..25;

   { 0 means "all days" }
   TDayOrZero = 0..25;

   TIntList = specialize TList<Integer>;

   TIntArray = array of Integer;

   TIntArrayHelper = specialize TArrayHelper<Integer>;

   TIntGrid = specialize TGenGrid<Integer>;

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

   TDir = Aoc.Generic.TDir;
   TPos = Aoc.Generic.TPos;
   TGrid = specialize TGenGrid<char>;

   TDirHelper = type helper for TDir
      function Invert: TDir; inline;
      function Clockwise: TDir; inline;
      function CounterClockwise: TDir; inline;
      class function FromChar(ch: Char): TDir; static; inline;
   end;

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
   TCSVRunFunction = function (input: TCSVReader): TResult;

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

   TCSVRunner = class(TRunner)
   private
      Frun: TCSVRunFunction;

   public
      constructor Create(ARun: TCSVRunFunction);
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
   procedure RegisterDay(day: TDay; run: TCSVRunFunction; Version: Integer = 1);
   procedure RunDay(day: TDayOrZero; version: Integer; const inputFileName: String; updateReadme: Boolean = False);

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

{ TDirHelper }
function TDirHelper.Invert: TDir; inline;
begin
   result := TDir((Ord(self) + 2) mod 4);
end;

function TDirHelper.Clockwise: TDir; inline;
begin
   result := TDir((Ord(self) + 1) mod 4);
end;

function TDirHelper.CounterClockwise: TDir; inline;
begin
   result := TDir((Ord(self) + 3) mod 4);
end;

class function TDirHelper.FromChar(ch: Char): TDir; inline;
begin
   case ch of
      '^': result := Up;
      '>': result := Right;
      'v': result := Left;
      '<': result := Down;
      else raise Exception.CreateFmt('Invalid direction character: %c', [ch]);
   end
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

{ TCSVRunner }

constructor TCSVRunner.Create(ARun: TCSVRunFunction);
begin
   Frun := Arun;
end;

function TCSVRunner.Run(input: TStream): TResult;
var
   csv: TCSVReader = nil;
begin
   try
      csv := TCSVReader.Create(input);
      csv.OwnsStream := False;
      result := Frun(csv);
   finally
      csv.Free;
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
   AddTest(day, Version, runner);
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

procedure RegisterDay(day: TDay; run: TCSVRunFunction; Version: Integer);
begin
   RegisterDay(day, TCSVRunner.Create(run), Version);
end;

procedure RunDay(day: TDayOrZero; Version: Integer; const inputFileName: String; updateReadme: Boolean);
const
   {$ifdef CPUX86_64}
   cpu = 'AMD Ryzen 5 Pro 7530U';
   {$else}
   cpu = 'RasPi2 ARMv7 Processor rev 5';
   {$endif}
   {$ifdef CPULLVM}
   llvm = ' (LLVM)';
   {$else}
   llvm = ' (FPC native)';
   {$endif}
var
   input: TStream = nil;
   res: TResult;
   d, ver: Integer;

   starttime, endtime: TDateTime;
   dayTime: Double = 0;
   bestDayTime: Double = 0;
   totalTime: Double = 0;

   readme: TStringList = nil;
   line: String;
   i, j, tblbeg, tblend: Integer;
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

   try
      if updateReadme then begin
         readme := TStringList.Create;
         readme.LoadFromFile('README.md');

         for tblend := 0 to readme.Count - 1 do begin
            if readme[tblend].TrimLeft.StartsWith('**' + cpu + llvm) then break;
         end;
         if tblend >= readme.Count - 1 then raise Exception.Create('No table found in README.md');
         tblbeg := tblend - 1;

         // skip empty lines and total time
         while (tblbeg > 0)
               and ((readme[tblbeg].Trim.Length = 0)
                    or (readme[tblbeg].Trim.StartsWith('Total time:')))
         do Dec(tblbeg);

         // skip table
         if (tblbeg > 0) and readme[tblbeg].TrimLeft.StartsWith('|') then begin
            while (tblbeg > 0) and readme[tblbeg].TrimLeft.StartsWith('|') do Dec(tblbeg);
            tblbeg += 1;
         end else begin
            // no real table found, insert an empty line
            tblbeg += 1;
            readme.Insert(tblbeg, '');
            tblbeg += 1;
            tblend += 1;
         end;

         // remove old table except for caption
         for i := tblbeg to tblend - 1 do readme.Delete(tblbeg);

         // add table header
         line := '| day | version | %17s | %15s | time (ms)|'.Format(['part1', 'part2']);
         readme.Insert(tblbeg, '  ' + line);
         for i := 1 to line.Length do
            if line[i] <> '|' then
               line[i] := '-'
            else if i > 1 then
               line[i-1] := ':';
         line[2] := ':';
         line[8] := ':';
         readme.Insert(tblbeg+1, '  ' + line);
         readme.Insert(tblbeg + 2, '');
         tblend := tblbeg + 2;
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
               if not updateReadme then begin
                  writeln(Format('day: %2d %s  part1: %17s  part2: %15s   time:%.3g',
                                 [d,
                                  IfThen(ver > 1, Format('v%d', [ver]), '  '),
                                  res[1], res[2], dayTime]));
               end else begin
                  readme.Insert(tblend, '  | %3d | %7d | %17s | %15s | %8.3g |'.Format(
                                   [d, ver, res[1], res[2], dayTime]));
                  tblend += 1;
               end;
               bestDayTime := Min(bestDayTime, dayTime);
            finally
               FreeAndNil(input);
            end
         end;
         if bestDayTime < Infinity then totalTime += bestDayTime;
      end;

      if not updateReadme then begin
         if day = 0 then writeln(Format('Total time: %.3g', [totalTime]));
      end else begin
         readme.Insert(tblend, '');
         tblend += 1;
         readme.Insert(tblend, '  Total time: %3.g ms'.Format([totalTime]));
         readme.SaveToFile('README.md');
      end;
   finally
      readme.Free;
   end
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
