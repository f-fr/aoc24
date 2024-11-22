type
   TTestNamesBag = class(TTestCase)
   private
      Bag: TNamesBag;

   public
      procedure Setup; override;
      procedure TearDown; override;

   published
      procedure TestFirstName;
      procedure TestSecondName;
   end;

   TTestGrid = class(TTestCase)
   private
      Grid: TGrid;

   public
      procedure Setup; override;
      procedure TearDown; override;

   published
      procedure TestIsEmpty;
      procedure TestSize;
      procedure TestFields;
      procedure TestSetFields;
      procedure TestRows;
      procedure TestCols;
      procedure TestBoundary;
   end;

{ TTestNamesBag }

procedure TTestNamesBag.Setup;
begin
   Bag := TNamesBag.Create;
end;

procedure TTestNamesBag.TearDown;
begin
   Bag.Free;
end;

procedure TTestNamesBag.TestFirstName;
var
   i: Integer;
begin
   for i := 1 to 10 do
      AssertEquals(0, Bag['abc']);
end;

procedure TTestNamesBag.TestSecondName;
var
   i: Integer;
begin
   for i := 1 to 10 do begin
      AssertEquals(0, Bag['abc']);
      AssertEquals(1, Bag['def']);
   end;
end;

{ TTestGrid }

procedure TTestGrid.Setup;
var
   mem: TStringStream;
begin
   try
      Grid := TGrid.Create;
      mem := TStringStream.Create('123' + LineEnding + '456' + LineEnding);
      Grid.LoadFromStream(mem);

      AssertEquals(2, Grid.N);
      AssertEquals(3, Grid.M);
   finally
      mem.Free;
   end;
end;

procedure TTestGrid.TearDown;
begin
   Grid.Free;
end;

procedure TTestGrid.TestIsEmpty;
begin
   Grid.Free;
   Grid := TGrid.Create;
   AssertEquals(0, Grid.N);
   AssertEquals(0, Grid.M);
   AssertFalse(Grid.HasBoundary);
end;

procedure TTestGrid.TestSize;
begin
   AssertEquals(2, Grid.N);
   AssertEquals(3, Grid.M);
end;

procedure TTestGrid.TestFields;
begin
   AssertEquals('1', Grid[0, 0]);
   AssertEquals('2', Grid[0, 1]);
   AssertEquals('3', Grid[0, 2]);
   AssertEquals('4', Grid[1, 0]);
   AssertEquals('5', Grid[1, 1]);
   AssertEquals('6', Grid[1, 2]);
end;

procedure TTestGrid.TestSetFields;
begin
   Grid[1,2] := 'u';
   Grid.Row[1][0] := 'a';
   Grid.Col[0][0] := 'T';
   AssertEquals('T', Grid[0, 0]);
   AssertEquals('2', Grid[0, 1]);
   AssertEquals('3', Grid[0, 2]);
   AssertEquals('a', Grid[1, 0]);
   AssertEquals('5', Grid[1, 1]);
   AssertEquals('u', Grid[1, 2]);
end;

procedure TTestGrid.TestRows;
var
   i: Integer = 0;
   row: TGrid.TRow;
begin
   AssertEquals('1', Grid.Row[0][0]);
   AssertEquals('2', Grid.Row[0][1]);
   AssertEquals('3', Grid.Row[0][2]);
   AssertEquals('4', Grid.Row[1][0]);
   AssertEquals('5', Grid.Row[1][1]);
   AssertEquals('6', Grid.Row[1][2]);

   for row in Grid.Rows do begin
      AssertEquals(i, row.Index);
      if i = 0 then begin
         AssertEquals('1', row[0]);
         AssertEquals('2', row[1]);
         AssertEquals('3', row[2]);
      end else begin
         AssertEquals('4', row[0]);
         AssertEquals('5', row[1]);
         AssertEquals('6', row[2]);
      end;
      Inc(i);
   end;
end;

procedure TTestGrid.TestCols;
var
   i: Integer = 0;
   col: TGrid.TCol;
begin
   AssertEquals('1', Grid.Col[0][0]);
   AssertEquals('2', Grid.Col[1][0]);
   AssertEquals('3', Grid.Col[2][0]);
   AssertEquals('4', Grid.Col[0][1]);
   AssertEquals('5', Grid.Col[1][1]);
   AssertEquals('6', Grid.Col[2][1]);

   for col in Grid.Cols do begin
      AssertEquals(i, col.Index);
      if i = 0 then begin
         AssertEquals('1', col[0]);
         AssertEquals('4', col[1]);
      end else if i = 1 then begin
         AssertEquals('2', col[0]);
         AssertEquals('5', col[1]);
      end else begin
         AssertEquals('3', col[0]);
         AssertEquals('6', col[1]);
      end;
      Inc(i);
   end
end;

procedure TTestGrid.TestBoundary;
var
   i: Integer;
   b: char;
begin
   for b in ['#', 'x'] do begin
      Grid.Boundary := b;
      AssertTrue(Grid.HasBoundary);
      AssertEquals(b, Grid.Boundary);
      AssertEquals(4, Grid.N);
      AssertEquals(5, Grid.M);
      AssertEquals('1', Grid[1, 1]);
      AssertEquals('2', Grid[1, 2]);
      AssertEquals('3', Grid[1, 3]);
      AssertEquals('4', Grid[2, 1]);
      AssertEquals('5', Grid[2, 2]);
      AssertEquals('6', Grid[2, 3]);
      for i := 0 to Grid.N - 1 do AssertEquals(b, Grid[i, 0]);
      for i := 0 to Grid.N - 1 do AssertEquals(b, Grid[i, Grid.M-1]);
      for i := 0 to Grid.M - 1 do AssertEquals(b, Grid[0, i]);
      for i := 0 to Grid.M - 1 do AssertEquals(b, Grid[Grid.N-1, i]);
   end;

   Grid.NoBoundary;
   TestFields;
end;

procedure RegisterTests;
begin
   RegisterTest(TTestNamesBag);
   RegisterTest(TTestGrid);
end;
