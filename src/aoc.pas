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

uses AOC.Generic, generics.collections, Classes;

type

   TResult = array[1..2] of Int64;

   TDay = 1..25;

   TRunFunction = function (input: TStream): TResult;

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

   procedure RegisterDay(day: TDay; run: TRunFunction);
   procedure RunDay(day: TDay; const inputFileName: String);

implementation

uses SysUtils, DateUtils;

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


var
   days: array[TDay] of TRunFunction;

procedure RegisterDay(day: TDay; run: TRunFunction);
begin
   days[day] := run;
end;

procedure RunDay(day: TDay; const inputFileName: String);
var
   input: TStream = nil;
   res: TResult;
   starttime, endtime: TDateTime;
begin
   try
      if days[day] = nil then
         raise Exception.CreateFmt('No runner for day %d found', [day]);

      input := TFileStream.Create(inputFileName, fmOpenRead);
      starttime := Now;
      res := days[day](input);
      endtime := Now;
      writeln(Format('day: %2d   part1: %10d  part2: %10d   time:%.3g',
                     [day, res[1], res[2], MillisecondsBetween(starttime, endtime) / 1000]));
   finally
      input.Free;
   end;
end;

end.
