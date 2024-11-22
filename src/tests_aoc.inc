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

procedure RegisterTests;
begin
   RegisterTest(TTestNamesBag);
end;
