unit PriQueue;
{$mode objfpc}
{$modeswitch advancedrecords}
interface

type
   // A Heap is a priority queue with support for `DecreaseKey`.
   generic TGHeap<T, Val> = class
   type
      TItem = packed record
         data: T;
         value: Val;
      private
         // Position of this item on the heap.
         // If the item is not on the heap, it is the next element on the free
         // list.
         pos: Cardinal;
      end;

      TRef = record
      private
         idx : Cardinal;
      end;

   const
      MINCAP = 1024;

   private
      // The indices of the items.
      Fheap: array of Cardinal;
      // The data of the elements.
      Felems: array of TItem;
      // The first free element.
      Ffree: Cardinal;
      // The number of elements in the heap.
      Fsize: Cardinal;

   public
      constructor Create;
      destructor Destroy; override;

      function Push(const data: T; value: Val) : TRef;
      procedure DecreaseKey(const item : TRef; value: Val);

      function Min: TItem; inline;
      function TryMin(out item: TItem): Boolean; inline;
      function PopMin: TItem;
      function TryPopMin(out item: TItem): Boolean;
      function IsEmpty: Boolean; inline;

      property Size: Cardinal read Fsize;

   private
      class function Max(a, b: Cardinal): Cardinal;
   end;

implementation

constructor TGHeap.Create;
begin
   Fsize := 0;
   Ffree := High(Cardinal);
end;

destructor TGHeap.Destroy;
begin
end;

class function TGHeap.Max(a, b: Cardinal): Cardinal;
begin
   if a > b then result := a else result := b;
end;

function TGHeap.Push(const data: T; value: Val) : TRef;
var
   idx: Cardinal;
   i: Integer;
begin
   if Ffree = High(Cardinal) then begin
      assert(Fsize = Length(Fheap));
      SetLength(Fheap, Max(16, Fsize * 2));
      SetLength(Felems, Max(16, Fsize * 2));
      // add new elements to the free list
      for i := Fsize + 1 to High(Felems) do Felems[i-1].pos := i;
      Felems[High(Felems)].pos := High(Cardinal);
      Ffree := Fsize;
   end;

   assert(Fsize < Length(Fheap));

   // take element from free list
   idx := Ffree;
   Ffree := Felems[idx].pos;
   assert(idx <> Ffree);

   // put element at the end of the heap
   Felems[idx].data := data;
   Felems[idx].pos := Fsize;
   Felems[idx].value := value;
   Fheap[Fsize] := idx;
   result.idx := idx;
   inc(Fsize);

   DecreaseKey(result, value);
end;

procedure TGHeap.DecreaseKey(const item: TRef; value: Val);
var
   pos, parent_pos: Cardinal;
   idx, parent_idx: Cardinal;
begin
   idx := item.idx;
   pos := Felems[idx].pos;

   assert(value <= Felems[idx].value);

   while pos > 0 do begin
      parent_pos := (pos - 1) div 2;
      parent_idx := Fheap[parent_pos];
      if Felems[parent_idx].value < value then break;
      Fheap[pos] := parent_idx;
      Felems[parent_idx].pos := pos;
      pos := parent_pos;
   end;

   Fheap[pos] := idx;
   Felems[idx].pos := pos;
   Felems[idx].value := value;
end;

function TGHeap.TryMin(out item: TItem): Boolean; inline;
begin
   result := Fsize > 0;
   if result then item := Felems[Fheap[0]];
end;

function TGHeap.Min: TItem;
begin
   assert(Fsize > 0);
   result := Felems[Fheap[0]];
end;

function TGHeap.TryPopMin(out item: TItem): Boolean;
var
   minidx, lastidx: Cardinal;
   lastvalue: Val;
   i, ileft, iright, inxt: Cardinal;
begin
   result := Fsize > 0;
   if not result then exit(False);
   if Fsize = 1 then begin
      item := Felems[Fheap[0]];

      minidx := Fheap[0];
      Felems[minidx].pos := Ffree;
      Ffree := minidx;

      Fsize := 0;

      exit;
   end;

   minidx := Fheap[0];
   item := Felems[minidx];

   dec(Fsize);
   lastidx := Fheap[Fsize];
   lastvalue := Felems[lastidx].value;

   i := 0;
   while true do begin
      ileft := 2 * i + 1;
      iright := ileft + 1;
      { No children -> found insert position }
      if ileft >= Fsize then break;
      { Find smaller of both children }
      if iright >= Fsize then
         inxt := ileft
      else if Felems[Fheap[ileft]].value < Felems[Fheap[iright]].value then
         inxt := ileft
      else
         inxt := iright;
      { if the smaller children is larger than value -> found insert position }
      if Felems[Fheap[inxt]].value >= lastvalue then break;
      { move this child up }
      Fheap[i] := Fheap[inxt];
      Felems[Fheap[inxt]].pos := i;
      i := inxt;
   end;
   Fheap[i] := lastidx;
   Felems[lastidx].pos := i;
   Felems[minidx].pos := Ffree;
   Ffree := minidx;
end;

function TGHeap.PopMin: TItem;
begin
   assert(Fsize > 0);
   TryPopMin(result);
end;

function TGHeap.IsEmpty: Boolean;
begin
   result := Fsize = 0;
end;

end.
