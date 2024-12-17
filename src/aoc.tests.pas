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

unit AOC.Tests;

interface

uses Aoc;

type
   TPart = 1..2;

procedure AddTest(ADay: TDay; AVersion: Integer; ARunner: TRunner);

implementation

uses fpcUnit, TestRegistry, SysUtils, StrUtils, Classes;

type

   { TDayTestCase }

   TDayTestCase = class(TTestCase)
   private
      Fday: TDay;
      Fversion: Integer;
      Frunner: TRunner;
      Fpart: TPart;
      FfilePath: String;

   public
      constructor Create(ADay: TDay; AVersion: Integer; ARunner: TRunner; APart: TPart; FilePath: String); reintroduce;
      destructor Destroy; override;

      procedure RunPartTest(part: TPart);
      procedure RunTest; override;
   end;

constructor TDayTestCase.Create(ADay: TDay; AVersion: Integer; ARunner: TRunner; APart: TPart; FilePath: String);
begin
   if AVersion = 1 then
      inherited CreateWithName(Format('Test Part %d (%s)', [APart, ExtractFileName(FilePath)]))
   else
     inherited CreateWithName(Format('Test Part %d Version %d (%s)', [APart, AVersion, ExtractFileName(FilePath)]));
   Frunner := ARunner;
   Fday := ADay;
   Fversion := AVersion;
   Fpart := APart;
   FfilePath := FilePath;
end;

destructor TDayTestCase.Destroy;
begin
   inherited;
end;

procedure TDayTestCase.RunPartTest(part: TPart);
var
   input: TStringList = nil;
   mem: TMemoryStream = nil;

   i: Integer;
   Expecteds: array of String;
   ExpectedStr: String;
   Expected: Int64;
   res: TResult;
begin
   try
      input := TStringList.Create;
      mem := TMemoryStream.Create;

      input.LoadFromFile(FFilePath);
      if input.Count <= 1 then exit; // ignore empty files
      i := input[0].IndexOf(':');
      AssertTrue('First test line must be ''EXPECTED: <number>''', (i >= 0) and ('EXPECTED' = input[0].SubString(0, i).Trim));

      if StartsStr('test_part', ExtractFileName(FfilePath)) then begin
         ExpectedStr := input[0].SubString(i + 1).Trim;
      end else begin
         // both parts test
         Expecteds := input[0].SubString(i+1).Split(' ', TStringSplitOptions.ExcludeEmpty);
         AssertEquals('Exactly two outputs expected in expectation line (EXPECTED: <number1> <number2>)', 2, Length(Expecteds));
         ExpectedStr := Expecteds[Fpart - 1];
      end;

      if TryStrToInt64(ExpectedStr, Expected) then ExpectedStr := '';

      input.Delete(0);
      mem.Clear;
      input.SaveToStream(mem);
      mem.Position := 0;

      res := Frunner.Run(mem);
      if ExpectedStr = '' then
         AssertEquals(Expected, res[part])
      else
         AssertEquals(ExpectedStr, res[part]);
   finally
      mem.Free;
      input.Free;
   end;
end;

procedure TDayTestCase.RunTest;
begin
   RunPartTest(Fpart);
end;

procedure AddTest(ADay: TDay; AVersion: Integer; ARunner: TRunner);
var
   info: TSearchRec;
   dir, filename: String;
   suite: String;
begin
   suite := Format('Day%.2d', [ADay]);
   dir := Format('input/%.2d', [Aday]);

   // scan test input files and create test cases
   if FindFirst(ConcatPaths([dir, 'test*.txt']), 0, info) <> 0 then exit;

   try
      repeat
         FileName := info.Name;
         if FileName.StartsWith('test_part') then begin
            // single part test
            case FileName[10] of
               '1': RegisterTest(suite, TDayTestCase.Create(ADay, AVersion, ARunner, 1, ConcatPaths([dir, FileName])));
               '2': RegisterTest(suite, TDayTestCase.Create(ADay, AVersion, ARunner, 2, ConcatPaths([dir, FileName])));
            else
               raise Exception.CreateFmt('Invalid test case part: %c (must be test_part1 or test_part2)', [FileName[10]]);
            end;
         end else begin
            // both parts test
            RegisterTest(suite, TDayTestCase.Create(ADay, AVersion, ARunner, 1, ConcatPaths([dir, FileName])));
            RegisterTest(suite, TDayTestCase.Create(ADay, AVersion, ARunner, 2, ConcatPaths([dir, FileName])));
         end;
      until FindNext(info) <> 0;
   finally
      FindClose(info);
   end;
end;

end.
