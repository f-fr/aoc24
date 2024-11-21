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

procedure AddTest(ADay: TDay; ARunner: TRunner);

implementation

uses fpcUnit, TestRegistry, SysUtils, StrUtils, Classes;

type
   TDayTestCase = class(TTestCase)
   private
      Fday: TDay;
      Frunner: TRunner;

   public
      constructor Create(ADay: TDay; ARunner: TRunner; APart: TPart); reintroduce;
      destructor Destroy; override;

      procedure RunPartTest(part: TPart);

   published
      procedure TestPart1;
      procedure TestPart2;
   end;

constructor TDayTestCase.Create(ADay: TDay; ARunner: TRunner; APart: TPart);
begin
   inherited CreateWithName(Format('TestPart%d', [APart]));
   Frunner := ARunner;
   Fday := ADay;
end;

destructor TDayTestCase.Destroy;
begin
   inherited;
end;

procedure TDayTestCase.RunPartTest(part: TPart);
var
   info: TSearchRec;
   dir: String;
   input: TStringList = nil;
   mem: TMemoryStream = nil;

   Expected: Int64;
   res: TResult;
begin
   dir := Format('input/%.2d', [Fday]);
   if FindFirst(ConcatPaths([dir, Format('test_part%d*.txt', [part])]), 0, info) <> 0 then exit;

   try
      input := TStringList.Create;
      mem := TMemoryStream.Create;
      repeat
         input.LoadFromFile(ConcatPaths([dir, info.Name]));
         if input.Count <= 1 then continue; // ignore empty files
         AssertTrue('First test line must be ''EXPECTED: <number>''', StartsText('EXPECTED:', input[0]));
         Expected := StrToInt64(MidStr(input[0], 10, Length(input[0])));
         input.Delete(0);
         mem.Clear;
         input.SaveToStream(mem);
         mem.Position := 0;

         res := Frunner.Run(mem);
         AssertEquals(Expected, res[part]);
      until FindNext(info) <> 0;
   finally
      mem.Free;
      input.Free;
      FindClose(info);
   end;
end;

procedure TDayTestCase.TestPart1;
begin
   RunPartTest(1);
end;

procedure TDayTestCase.TestPart2;
begin
   RunPartTest(2);
end;

procedure AddTest(ADay: TDay; ARunner: TRunner);
var
   suite: String;
begin
   suite := Format('Day%.2d', [ADay]);
   RegisterTest(suite, TDayTestCase.Create(ADay, ARunner, 1));
   RegisterTest(suite, TDayTestCase.Create(ADay, ARunner, 2));
end;

end.
