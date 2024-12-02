{
  Copyright (c) 2020, 2021 Frank Fischer <frank-fischer@shadow-soft.de>

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

{ Simple and fast reading and writing of CSV files.

  This unit contains a simple API for reading and writing CSV files
  row by row.

  **Reading a CSV file** is done with the class TCSVReader:

  @includeCode(../examples/readcsv.pas)

  **Writing a CSV file** is done with the class `TCSVWriter`:

  @includeCode(../examples/writecsv.pas) }

unit EasyCSV;

interface

uses Classes, SysUtils, Types;

const
   // Minimal buffer size of `TCSVReader`.
   DefaultMinBufSize = 4*1024;
   // Maximal default buffer size of `TCSVReader`.
   DefaultMaxBufSize = 64*1024;
   DefaultQuoteChar = '"';
   DefaultDelimiter = ',';
   // By default CR, LF, CRLF and LFCR are all considered line endings.
   DefaultLineEndings: array of String = (#10, #13, #10#13, #13#10);
   DefaultIgnoreOuterWhitespace = true;

type
   // Exception raised if some (format) error occurred when reading a CSV file.
   ECSVReadError = class(Exception);

   // This error is raised if the buffer is too small for the
   // current row. This exception is raised if the implementation of `EnlargeMaxBufferSize`
   // does not increase the maximal buffer size.
   EBufferTooSmall = class(ECSVReadError);

   { Reader for CSV files.

     TCSVReader is the main class to read a CSV file row by row. There
     are two possible ways to read a row:

     1. Read the next row using ReadRow.

     2. Use the @link(Rows) enumerator to iterate over all rows.

     In both cases the cells in each row are returned in their
     original order and are access with indices starting at 0.

     If the CSV file has column headers in the first row, the cells in
     a row can be accessed using their names with ReadNamedRow or
     the or the NamedRows enumerator. The headers of the columns to be
     retrieved are passed to either method and the cells of the row
     are returned in the order of the headers.

     **Example:**

     @includeCode(../examples/readcsv.pas)

     See the corresponding methods for some examples.
   }
   TCSVReader = class
   private
   type
      // A single sell in the current line buffer.
      TCell = record
         // The start position of the cell.
         beg : Integer;
         // The number of characters in the cell.
         count : Integer;
      end;

   public
   type
      { A single row in the CSV file.

        The cells of this row start at index 0.
      }
      TRow = record
      private
         Fbeg : PChar;
         Fcells : array of TCell;
         FnumCells : Integer;
         FcodePage : TSystemCodePage;
         FformatSettings: ^TFormatSettings;

         procedure RaiseConvert(i : Integer; const what: String);
         function TryGetValue(i : Integer; out n : Integer): Boolean;
         function TryGetValue(i : Integer; out n : Int64): Boolean;

         function GetField(i : Integer): String; inline;
         function GetInt(i : Integer) : Integer; inline;
         function GetInt64(i : Integer) : Int64; inline;
         function GetDouble(i : Integer) : Double; inline;
{$ifdef uselibc}
         function GetDoubleC(i : Integer) : Double; inline;
{$endif}

      public
         // Return row as array.
         function ToArray : TStringArray; inline;

         { Return row as integer array with all cells converted to integer.

           @raises(EConvertError if a cell is not a valid integer) }
         function ToIntegerArray : TIntegerDynArray; inline;

         { Return row as double array with all cells converted to double.

           @raises(EConvertError if a cell is not a valid double) }
         function ToDoubleArray : TDoubleDynArray; inline;
{$ifdef uselibc}
         { Return row as double array with all cells converted to double using libc `strtod`.

           @raises(EConvertError if a cell is not a valid double) }
         function ToDoubleArrayC : TDoubleDynArray; inline;
{$endif}

         // Access the i-th cell as a string.
         property strings[i : Integer] : String read GetField; default;
         { Access the i-th cell as an integer.

           @raises(EConvertError if the cell is not a valid integer) }
         property integers[i : Integer] : Integer read GetInt;
         { Access the i-th cell as an 64-bit integer.

           @raises(EConvertError if the cell is not a valid integer) }
         property integers64[i : Integer] : Int64 read GetInt64;
         { Access the i-th cell as a double.

           @raises(EConvertError if the cell is not a valid double) }
         property doubles[i : Integer] : Double read GetDouble;
{$ifdef uselibc}
         { Access the i-th cell as a double converting with libc `strtod`.

           @raises(EConvertError if the cell is not a valid double) }
         property doublesc[i : Integer] : Double read GetDoubleC;
{$endif}
         // The number of cells in this row.
         property Count : Integer read FnumCells;
      end;

      // Enumerator over all rows of a CSV file.
      TRowEnum = record
      private
         Freader : TCSVReader;
         Fcur : TRow;

      public
         property Current: TRow read Fcur;
         function MoveNext : Boolean; inline;

         function GetEnumerator: TRowEnum; inline;
      end;

      { Enumerator over all rows of a CSV file.

        The cells of the row are ordered by header names.
      }
      TNamedEnum = record
      private
         Freader : TCSVReader;
         Fcur : TRow;

      public
         property Current: TRow read Fcur;
         function MoveNext : Boolean; inline;

         function GetEnumerator: TNamedEnum; inline;
      end;

   private
      type
         TPos = class
            pos : Integer;

            constructor Create(p : Integer);
         end;

   private
      FminBufSize, FmaxBufSize : Integer;
      Fbuffer: array of char;
      Fbeg : PChar; {< Pointer to first character in Fbuffer of current row }
      Fcur : Integer; {< Position of the current character }
      Fbegnxt : Integer; {< Position of the beginning of the next row }
      Fskip : Integer; {< Amount of characters the current char should be moved back in buffer }
      Fnremaining : Integer; {< Number of remaining chars in buffer after Fbeg }

      Fcells : array of TCell;
      FnumCells : Integer;

      FnumRows : Integer;
      FminNumCols : Integer;
      FmaxNumCols : Integer;
      FminBndCols : Integer;
      FmaxBndCols : Integer;

      FparseHeaders : Boolean;
      FheadersParsed : Boolean;
      Fheaders : array of String;
      FheaderIndices : TStringList;

      FnamedHeaders : array of String;
      FnamedIndices : array of Integer;
      { True if the headers have been set explicitly, not by `NamedRows`.

        If the source is changed, explicitly set headers are preserved.
      }
      FnamedHeadersExplicit : Boolean;

      Fdelimiter : char;
      FquoteChar : char;
      FlineEndings : array of String;
      FmaxLenLineEnding: Integer;
      FlineEndingsFirst: set of char;
      FignoreOuterWhitespace : Boolean;
      FformatSettings: TFormatSettings;

      Fstream : TStream;
      FownsStream : Boolean;

      FcodePage : TSystemCodePage;

   private
      procedure SetSourceText(str : String);
      procedure SetSource(stream : TStream);
      procedure SetSourceFile(filename : String);

      procedure SetLineEnding(eol: String);
      procedure SetLineEndings(eols: TStringArray);
      function GetLineEnding: String;
      function GetLineEndings: TStringArray;

      function GetNumHeaders : Integer;
      function GetHeader(col : Integer) : String;
      function GetHeaders : TStringArray;
      procedure ParseHeaders;
      { This method updates the named headers mapping.

        It is called automatically if
        1. The header row is parsed.
        2. The named headers are changed. }
      procedure UpdateNamedHeaders;
      procedure SetNamedHeaders(headers : TStringArray); inline;
      procedure SetNamedHeaders(headers : TStringArray; explicit : Boolean);

      function GetNumRows : Integer;
      procedure SetMinMaxColumns(ncols : Integer);

      function NextRow : Boolean;
      // Read the next input character.
      //
      // @param(ch the next character)
      // @param(eol @true if and only if the next character(s) is a line-ending)
      // @param(inquote @true if the next character is within a quoted string)
      //
      // If @code(eol) is true then @code(ch) is not set. If
      // @code(inquote) is @true then line ending characters are
      // parsed as regular characters and do not cause @code(eol) to
      // be @true (except at the EOF).
      //
      // @returns(@true if a new character has been read and @false at
      // EOF)
      function NextChar(out ch : char; out eol : Boolean; inquote : Boolean = False) : Boolean;

      procedure Reset;

   protected
      // Called to increase the maximal buffer size.
      //
      // This method can be overwritten to increase the maximal buffer
      // size by setting the `MaxBufferSize` property. If the size is not increased,
      // a `EBufferTooSmall` exception is raised. The default
      // implementation does nothing.
      procedure EnlargeMaxBufferSize; virtual;

   public
      // Create a new uninitialized CSV reader.
      constructor Create;
      // Create a CSV reader reading from the given input stream.
      constructor Create(stream : TStream);
      // Create a CSV reader reading from the given named file.
      constructor Create(filename : String);

      destructor Destroy; override;

      { Set the CSV input text as a string.

        The reader is reset to parse the given string.

        The codepage is changed to the string's codepage. }
      property SourceText : String write SetSourceText;

      { Set the CSV input stream.

        The reader is reset to parse CSV data from the given stream. }
      property Source : TStream write SetSource;

      { Set the CSV input file.

        The reader is reset to parse CSV data from the given named file. }
      property SourceFile : String write SetSourceFile;

      { Return the index of a column with the given header.

        If there is no column with that header, the function returns - 1. }
      function IndexOfHeader(header : String) : Integer;

      { Read the next row.

        @param(row the next row)
        @returns(@true if and only if there is a next row and @false at EOF)

        **Example:**
        ```pascal
        var
          row : TCSVReader.TRow;
          i : Integer;
        begin
           ...
           while reader.ReadRow(row) do begin
             for i := 0 to row.Count do write(row[i], ' ');
             writeln;
           end
        end
        ```
      }
      function ReadRow(out row : TRow) : Boolean;

      { Read the next row as string array.

        @param(elems the next row)
        @returns(@true if and only if there is a next row and @false at EOF)

        **Example:**
        ```pascal
        var
          row : array of String;
          i : Integer;
        begin
           ...
           while reader.ReadRow(row) do begin
             for i := 0 to High(row) do write(row[i], ' ');
             writeln;
           end
        end
        ```
      }
      function ReadRow(out elems : TStringArray) : Boolean;

      { Read the next row in order of the requested headers.

        The requested headers are set by the NamedColumns property. Only the
        elements according to these headers are returned in row in order of
        the headers.

        @param(elems the next row)
        @returns(@true if and only if there is a next row and @false at EOF)

        **Example:**
        ```pascal
        var
          row : TCSVReader.TRow;
        begin
           ...
           reader.NamedHeaders := ['Name', 'First Name'];
           while reader.ReadRow(row) do begin
             write('Name: ', row[0]);
             write('First Name: ', row[1]);
           end
        end
        ```
      }
      function ReadNamedRow(out row : TRow) : Boolean;

      { Read the next row as string array in order of the requested headers.

        The requested headers are set by the NamedColumns property. Only the
        elements according to these headers are returned in row in order of
        the headers.

        **Example:**
        ```pascal
        var
          row : array of String;
          i : Integer;
        begin
           ...
           reader.NamedHeaders := ['Name', 'First Name'];
           while reader.ReadRow(row) do begin
             write('Name: ', row[0]);
             write('First Name: ', row[1]);
           end
        end
        ```
      }
      function ReadNamedRow(out elems : TStringArray) : Boolean;

      { @noAutoLinkHere
        Enumerate all rows

        **Example:**
        ```pascal
        var
          row : TCSVReader.TRow;
          i : Integer;
        begin
           ...
           for row in reader.Rows do begin
             for i := 0 to High(row) do write(row[i], ' ');
             writeln;
           end
        end
        ```
      }
      function Rows : TRowEnum;

      // Return the row enumerator.
      function GetEnumerator : TRowEnum; inline;

      { Return an enumerator retrieving the columns with the specified
        `NamedHeaders`.

        **Example:**
        ```pascal
        var
          row : TCSVReader.TRow;
        begin
           ...
           reader.NamedHeaders := ['Name', 'First Name'];
           for row in reader.NamedRows do begin
             write('Name: ', row[0]);
             write('First Name: ', row[1]);
           end
        end
        ```
      }
      function NamedRows : TNamedEnum;

      { Return an enumerator retrieving the columns with the given header `names`.

        This function basically sets `NamedHeaders` to `names` and the calls
        `NamedRows`.

        **Example:**
        ```pascal
        var
          row : TCSVReader.TRow;
        begin
           ...
           for row in reader.NamedRows(['Name', 'First Name']) do begin
             write('Name: ', row[0]);
             write('First Name: ', row[1]);
           end
        end
        ```
      }
      function NamedRows(names : array of String) : TNamedEnum;

      // Whether the first row of CSV file should be parsed as column headers.
      property HasHeader : Boolean read FparseHeaders write FparseHeaders;
      // The number of headers, i.e. the number of cells in the first row.
      property NumHeaders : Integer read GetNumHeaders;
      // The headers, i.e. the contents of the fields in the first row.
      property Header[col : Integer] : String read GetHeader;
      // The headers, i.e. the contents of the fields in the first row, as array.
      property Headers : TStringArray read GetHeaders;

      { The header names to be retrieved in order.

        The elements corresponding to the specified headers in this order can be
        retrieved by ReadNamedRow or NamedRows.

        Because retrieving named headers requires headers, setting this property
        also enables HasHeader. }
      property NamedHeaders : TStringArray read FnamedHeaders write SetNamedHeaders;

      // The separator of cells in a row (default: DefaultDelimiter)
      property Delimiter : char read Fdelimiter write Fdelimiter;
      // The quote character for each cell (default: DefaultQuoteChar)
      property QuoteChar : char read FquoteChar write FquoteChar;
      // The (first) delimiter of a row (default: DefaultLineEndings).
      //
      // This returns the first entry in the list of line endings.
      property LineEnding : String read GetLineEnding write SetLineEnding;
      // A list of possible line endings (default: DefaultLineEndings).
      property LineEndings : TStringArray read GetLineEndings write SetLineEndings;
      // Whether spaces at the beginning and end of each cell should
      // be removed (default: DefaultIgnoreOuterWhitespace).
      property IgnoreOuterWhitespace : Boolean read FignoreOuterWhitespace write FignoreOuterWhitespace;

      { The minimal number of columns in a row read so far.

        When set each row must have at least this number of rows,
        otherwise an ECSVReadError is raised. }
      property MinColumns : Integer read FminNumCols write FminBndCols;
      { The maximal number of columns in a row read so far.

        When set each row must have at most this number of rows,
        otherwise an ECSVReadError is raised. }
      property MaxColumns : Integer read FmaxNumCols write FmaxBndCols;

      // Set the minimal and maximal number of columns.
      property NumColumns : Integer write SetMinMaxColumns;

      // The number of rows read so far.
      property NumRows : Integer read GetNumRows;

      { The minimal size of the internal row buffer (default: DefaultMinBufSize) }
      property MinBufferSize : Integer read FminBufSize write FminBufSize;

      { The maximal size of the internal row buffer (default: DefaultMaxBufSize) }
      property MaxBufferSize : Integer read FmaxBufSize write FmaxBufSize;

      { Whether this reader owns the input stream

        If @true the input stream will be deleted if this reader is
        deleted. }
      property OwnsStream : Boolean read FownsStream write FownsStream;

      { Set the codepage of the input file or stream.

        The default is the system's default codepage `DefaultSystemCodePage`. }
      property CodePage : TSystemCodePage read FcodePage write FcodePage;

      { The format settings used to read floating-point numbers.
        (default: DefaultFormatSettings)}
      property FormatSettings: TFormatSettings read FformatSettings write FformatSettings;
   end;

   // Exception raised if some error occurred during writing a CSV file.
   ECSVWriteError = class(Exception);

   { A simple class to write CSV files row by row.

     Each row can either be written at once by calling AddRow or it
     can written cell by cell using AddCells and termined by EndLine.

     **Example:**

     @includeCode(../examples/writecsv.pas)
   }
   TCSVWriter = class
   private
      Fstream : TStream;
      FownsStream : Boolean;

      Fdelimiter : char;
      FquoteChar : char;
      FlineEnding : String;
      FforceQuote : Boolean;

      FnumRows : Integer;
      FnumRowCells : Integer;
      FnumCells : Integer;

   private
      procedure SetOutput(stream : TStream);
      procedure SetOutputFile(filename : String);

      procedure SetLineEnding(eol: String);

      procedure WriteChar(ch : char); inline;
      procedure WriteCell(s : String);

   public
      // Create a new uninitialized CSV writer.
      constructor Create; reintroduce;
      // Create a CSV writer writing to the given stream.
      constructor Create(stream : TStream);
      // Create a CSV writer writing to the given named file.
      constructor Create(filename : String);

      destructor Destroy; override;

      { Add a complete row of string cells.

        A line ending is added after the last cell. }
      procedure AddRow(elems : array of String);

      { Add a complete row of typed cells.

        A line ending is added after the last cell. }
      procedure AddRow(elems : array of const);

      { Add one or more cells to the current row }
      procedure AddCells(elems : array of const);

      { Add one or more cells to the current row }
      procedure AddCells(elems : array of String);

      { Add one cells to the current row }
      procedure AddCell(elem : String);

      { End the current row.

        If this method is called before any cell has been written, nothing
        happens. In other words, the file cannot start with empty rows. }
      procedure EndRow;

      { Set the output stream }
      property Output : TStream write SetOutput;

      { Set the output to the given named file }
      property OutputFile : String write SetOutputFile;

      { The separator between cells in a row (default: DefaultDelimiter)}
      property Delimiter : char read Fdelimiter write Fdelimiter;
      { The quote character (default: DefaultQuoteChar) }
      property QuoteChar : char read FquoteChar write FquoteChar;
      { Force quoting all arguments (default: false) }
      property ForceQuote: Boolean read FforceQuote write FforceQuote;
      { The row separator (default: System.LineEnding).

        If set to the empty string the system default line ending is used. }
      property LineEnding : String read FlineEnding write SetLineEnding;

      { Whether the writer owns the output stream.

        If the stream is owned then it is freed when the writer is deleted. }
      property OwnsStream : Boolean read FownsStream write FownsStream;
   end;


implementation

uses Math, StrUtils, bufstream;

const
   HTAB = #9;
   SPACE = #32;
   WhiteSpace = [HTAB, SPACE];

constructor TCSVReader.TPos.Create(p : Integer);
begin
   pos := p;
end;

function TCSVReader.TRow.GetField(i : Integer): String; inline;
begin
   SetString(result, Fbeg + Fcells[i].beg, Fcells[i].Count);
   SetCodePage(RawByteString(result), FcodePage, False);
   SetCodePage(RawByteString(result), CP_ACP, True);
end;

{$ifdef uselibc}
{$linklib c}

function strtol(p: PChar; endptr: PPChar; base: Integer): Int32; cdecl; external;
function strtoll(p: PChar; endptr: PPChar; base: Integer): Int64; cdecl; external;
function strtod(p: PChar; endptr: PPChar): Double; cdecl; external;

function TCSVReader.TRow.TryGetValue(i : Integer; out n : Integer) : Boolean;
var
   endptr : PChar;
   cnt : Integer;
begin
   n := strtol(Fbeg + Fcells[i].beg, @endptr, 10);
   cnt := Fcells[i].count;
   result := (endptr = nil) or ((cnt > 0) and (endptr = Fbeg + Fcells[i].beg + cnt));
end;

function TCSVReader.TRow.TryGetValue(i : Integer; out n : Int64) : Boolean;
var
   endptr : PChar;
   cnt : Integer;
begin
   n := strtoll(Fbeg + Fcells[i].beg, @endptr, 10);
   cnt := Fcells[i].count;
   result := (endptr = nil) or ((cnt > 0) and (endptr = Fbeg + Fcells[i].beg + cnt));
end;

{$else}

function TCSVReader.TRow.TryGetValue(i : Integer; out n : Integer) : Boolean;
const
   prefix: set of char = ['$', '&', '%', 'x', 'X', #0];
var
   code: Word;
begin
   // We need to ensure that the string does not contain a base prefix
   if (Fbeg[Fcells[i].beg] in prefix) or
         ((Fbeg[Fcells[i].beg] = '0') and ((Fbeg[Fcells[i].beg + 1] = 'x') or (Fbeg[Fcells[i].beg + 1] = 'X')))
   then
      result := False
   else begin
      Val(Fbeg + Fcells[i].beg, n, code);
      result := code = 0;
   end
end;

function TCSVReader.TRow.TryGetValue(i : Integer; out n : Int64) : Boolean;
const
   prefix: set of char = ['$', '&', '%', 'x', 'X', #0];
var
   code: Word;
begin
   // We need to ensure that the string does not contain a base prefix
   if (Fbeg[Fcells[i].beg] in prefix) or
         ((Fbeg[Fcells[i].beg] = '0') and ((Fbeg[Fcells[i].beg + 1] = 'x') or (Fbeg[Fcells[i].beg + 1] = 'X')))
   then
      result := False
   else begin
      Val(Fbeg + Fcells[i].beg, n, code);
      result := code = 0;
   end
end;

{$endif}

procedure TCSVReader.TRow.RaiseConvert(i : Integer; const what: String);
begin
   raise EConvertError.Create('Cannot convert ' + GetField(i) + ' to ' + what);
end;

function TCSVReader.TRow.GetInt(i : Integer) : Integer; inline;
begin
   if not TryGetValue(i, result) then
      raiseConvert(i, 'Integer');
end;

function TCSVReader.TRow.GetInt64(i : Integer) : Int64; inline;
begin
   if not TryGetValue(i, result) then
      raiseConvert(i, 'Int64');
end;

function TCSVReader.TRow.GetDouble(i : Integer) : Double; inline;
begin
   if not TextToFloat(Fbeg + Fcells[i].beg, result, fvDouble, FformatSettings^) then
      raiseConvert(i, 'Double');
end;

{$ifdef uselibc}
function TCSVReader.TRow.GetDoubleC(i : Integer) : Double; inline;
var
   endptr : PChar;
begin
   result := strtod(Fbeg + Fcells[i].beg, @endptr);
   if not ((endptr = nil) or (endptr = Fbeg + Fcells[i].beg + Fcells[i].count)) then
      raiseConvert(i, 'Double');
end;
{$endif}

function TCSVReader.TRow.ToArray : TStringArray;
var
   i : Integer;
begin
   SetLength(result, FnumCells);
   for i := 0 to FnumCells - 1 do result[i] := GetField(i);
end;

function TCSVReader.TRow.ToIntegerArray : TIntegerDynArray;
var
   i : Integer;
begin
   SetLength(result, FnumCells);
   for i := 0 to FnumCells - 1 do result[i] := integers[i];
end;

function TCSVReader.TRow.ToDoubleArray : TDoubleDynArray;
var
   i : Integer;
begin
   SetLength(result, FnumCells);
   for i := 0 to FnumCells - 1 do result[i] := doubles[i];
end;

{$ifdef uselibc}
function TCSVReader.TRow.ToDoubleArrayC : TDoubleDynArray;
var
   i : Integer;
begin
   SetLength(result, FnumCells);
   for i := 0 to FnumCells - 1 do result[i] := doublesc[i];
end;
{$endif}

function TCSVReader.TRowEnum.MoveNext : Boolean;
begin
   result := Freader.ReadRow(Fcur);
end;

function TCSVReader.TRowEnum.GetEnumerator: TRowEnum;
begin
   result := self;
end;

function TCSVReader.TNamedEnum.MoveNext : Boolean;
begin
   result := Freader.ReadNamedRow(Fcur);
end;

function TCSVReader.TNamedEnum.GetEnumerator: TNamedEnum;
begin
   result := self;
end;

constructor TCSVReader.Create;
begin
   inherited;

   SetLength(Fbuffer, 0);
   FminBufSize := DefaultMinBufSize;
   FmaxBufSize := DefaultMaxBufSize;

   FquoteChar := DefaultQuoteChar;
   Fdelimiter := DefaultDelimiter;
   LineEndings := DefaultLineEndings;
   FignoreOuterWhitespace := DefaultIgnoreOuterWhitespace;

   FheaderIndices := TStringList.Create;
   FheaderIndices.Sorted := true;
   FheaderIndices.ownsObjects := true;
   FnamedHeadersExplicit := False;

   FownsStream := false;

   FcodePage := CP_ACP;
   FformatSettings := DefaultFormatSettings;

   Reset;
end;

procedure TCSVReader.Reset;
begin
   if Length(Fbuffer) > FmaxBufSize then
      SetLength(Fbuffer, FmaxBufSize);

   if Length(Fbuffer) > 0 then
      Fbeg := @Fbuffer[High(Fbuffer)]
   else
      Fbeg := nil;

   Fbegnxt := 0;
   Fcur := -1;
   Fskip := 0;
   Fnremaining := 0;

   if (Fstream <> nil) and (Fstream.Position > 0) then Fstream.Position := 0;

   FminNumCols := High(FminNumCols);
   FmaxNumCols := 0;
   FnumRows := 0;
   FheadersParsed := false;
   if not FnamedHeadersExplicit then begin
      SetLength(Fnamedheaders, 0);
      SetLength(FnamedIndices, 0);
   end;
   FheaderIndices.Clear;
   SetLength(Fheaders, 0);
end;

constructor TCSVReader.Create(stream : TStream);
begin
   Create;
   SetSource(stream);
end;

constructor TCSVReader.Create(filename : String);
begin
   Create;
   SetSourceFile(filename);
end;

destructor TCSVReader.Destroy;
begin
   FheaderIndices.Free;
   if FownsStream then FreeAndNil(Fstream);
   inherited;
end;

procedure TCSVReader.EnlargeMaxBufferSize;
begin
end;

procedure TCSVReader.SetMinMaxColumns(ncols : Integer);
begin
   MinColumns := ncols;
   MaxColumns := ncols;
end;

function TCSVReader.GetNumHeaders: Integer;
begin
   ParseHeaders;
   result := Length(Fheaders);
end;

function TCSVReader.IndexOfHeader(header : String) : Integer;
var
   i : Integer;
begin
   ParseHeaders;
   i := FheaderIndices.IndexOf(header);
   if i >= 0 then
      result := (FheaderIndices.objects[i] as TPos).pos
   else
      result := -1;
end;

function TCSVReader.GetHeader(col : Integer) : String;
begin
   ParseHeaders;
   result := Fheaders[col];
end;

function TCSVReader.GetHeaders : TStringArray;
begin
   ParseHeaders;
   result := Fheaders;
end;

procedure TCSVReader.ParseHeaders;
var
   i : Integer;
begin
   if FparseHeaders and not FheadersParsed then begin
      FheadersParsed := true;
      ReadRow(Fheaders);
      for i := 0 to High(Fheaders) do
         FheaderIndices.AddObject(Fheaders[i], TPos.Create(i));

      UpdateNamedHeaders;
   end;
end;

procedure TCSVReader.UpdateNamedHeaders;
var
   i, pos : Integer;
begin
   if FheadersParsed then begin
      SetLength(FnamedIndices, Length(Fheaders));
      for i := 0 to High(FnamedIndices) do FnamedIndices[i] := -1;
      for i := 0 to High(FnamedHeaders) do begin
         pos := IndexOfHeader(FnamedHeaders[i]);
         if pos < 0 then
            raise ECSVReadError.Create(Format('Column ''%s'' not found', [FnamedHeaders[i]]));
         if FnamedIndices[pos] >= 0 then
            raise ECSVReadError.Create(Format('Duplicate header name ''%s''', [FnamedHeaders[i]]));
         FnamedIndices[pos] := i;
      end
   end
end;

procedure TCSVReader.SetNamedHeaders(headers : TStringArray; explicit : Boolean);
begin
   FnamedHeaders := headers;
   if Length(headers) > 0 then begin
      FnamedHeadersExplicit := explicit;
      FparseHeaders := true;
   end else
      FnamedHeadersExplicit := False;
   UpdateNamedHeaders;
end;

procedure TCSVReader.SetNamedHeaders(headers : TStringArray); inline;
begin
   SetNamedHeaders(headers, True);
end;

function TCSVReader.GetNumRows : Integer;
begin
   if FparseHeaders and FheadersParsed then
      result := FnumRows - 1
   else
      result := FnumRows;
end;

function TCSVReader.NextRow : Boolean;
var
   ch : char;
   eol : Boolean;
   inquote : Boolean = false;
   beg, len, quotelen : Integer;
begin
   if Fstream = nil then raise ECSVReadError.Create('No input stream set');

   // Go the end of previous line.
   Fbeg := Fbeg + Fbegnxt;
   Fnremaining := Fnremaining - Fbegnxt;
   Fbegnxt := 0;
   Fcur := -1;
   FnumCells := 0;

   result := false;

   // parse cells of the current row
   while NextChar(ch, eol) do begin
      // Maybe enlarge number of cells.
      if FnumCells >= Length(Fcells) then SetLength(Fcells, FnumCells + 1);

      // skip whitespace at the beginning
      if FignoreOuterWhitespace then
         while (not eol) and (ch in WhiteSpace) and NextChar(ch, eol) do;

      // this is the first position
      beg := Fcur;
      quotelen := 0;

      // parse the current cell
      while true do begin
         if (not eol) and (ch = FquoteChar) then begin
            if not inquote then begin
               // start a new quote
               inquote := true;
               inc(Fskip); // skip the quote character
            end else if not NextChar(ch, eol, True) then begin
               // reached end of file -> quote complete
               inquote := false;
               break;
            end else if ch = FquoteChar then
               // a second quote -> escape quote character
               inc(Fskip) // skip one of the two quotes, keep the other
            else begin
               inc(Fskip); // skip the closing quote
               dec(Fcur); // we need to reparse that non-quote character
               quotelen := Fcur - Fskip - beg + 1;
               inquote := false;
            end
         end else if not inquote and (eol or (ch = Fdelimiter)) then break;

         // in the middle of a cell, fetch next char
         if not NextChar(ch, eol, inquote) then begin
            // reached the end of file -> this counts as line ending
            if inquote then
               raise ECSVReadError.Create('Unexpected end of file (quote not closed)');
            break;
         end
      end;

      // Reached the end of current cell. The current character is either
      // a delimiter or a line ending so does not belong to the cell.
      len := Fcur - beg - Fskip;

      // skip whitespace at the end of the cell
      if FignoreOuterWhitespace then
         while (len > quotelen) and (Fbeg[beg + len - 1] in Whitespace) do dec(len);

      Fcells[FnumCells].beg := beg;
      Fcells[FnumCells].count := len;

      // Put a 0-char at the position of the delimiter to mark the end of the cell
      Fbeg[beg + len] := #0;

      // start next cell
      inc(FnumCells);
      Fskip := 0;

      // reaching the end of the row
      if eol then begin
         result := true;
         inc(FnumRows);
         break;
      end
   end;
end;

function TCSVReader.NextChar(out ch : char; out eol : Boolean; inquote : Boolean = False) : Boolean;
var
   newsize: Integer;
   minsize: Integer;
   atbegin: Boolean;
   maxEol: Integer;
   i: Integer;
begin
   eol := False;

   // We need at space for at least 4 characters because it may be a
   // BOM marker consisting of 3 bytes. It should also be large enough to
   // contain a full line ending (usually 1 or 2 chars).
   minsize := Max(4, FmaxLenLineEnding);
   while Fcur + minsize >= Fnremaining do begin
      // reached the end of the buffer
      if (Fbeg <> nil) and (Fbeg > @Fbuffer[0]) then
         // move data to beginning of buffer
         move(Fbeg^, Fbuffer[0], Fnremaining);

      // Now all remaining data is at the beginning of the buffer.
      // Check if the unparsed part of the buffer is large enough to contain at least
      // an end-of-line character (or a BOM-marker).
      if Fcur + minsize >= Length(Fbuffer) then begin
         // buffer is full/too small, enlarge buffer
         newsize := Min(Max(Length(Fbuffer) * 2, FminBufSize), FmaxBufSize);
         if newsize <= Length(Fbuffer) then begin
            // Buffer cannot be enlarged because the maximal buffer size has been reached.
            // Try to increase the maximal buffer size.
            EnlargeMaxBufferSize;
            newsize := Min(Max(Length(Fbuffer) * 2, FminBufSize), FmaxBufSize);
            if newsize <= Length(Fbuffer) then
               // The maximal buffer size has not been increase, so raise an error.
               raise EBufferTooSmall.Create('Buffer too small for current row. Maybe overwrite `EnlargeMaxBufferSize`?');
         end;
         SetLength(Fbuffer, newsize);
      end;
      atbegin := Fstream.Position = 0;
      Fbeg := @Fbuffer[0];
      // Fill rest of buffer with new data.
      Fnremaining := Fnremaining + Fstream.Read(Fbuffer[Fnremaining], Length(Fbuffer) - Fnremaining);
      // Still no data available -> reached end of file.
      if Fcur + 1 >= Fnremaining then begin
         inc(Fcur); // current points at the character "after the end"
         ch := #0;
         eol := True; // the end of file counts as line ending
         Fbegnxt := Fcur;
         exit(False);
      end;

      // If we are not at the beginning of the stream, we are done now.
      if not atbegin then break;

      // Test if there is a BOM marker at the beginning of the file.
      assert(Fcur = -1);
      assert(Fbeg = @Fbuffer[0]);
      if (Fnremaining >= 3) and (Byte(Fbeg[0]) = $EF) and (Byte(Fbeg[1]) = $BB) and (Byte(Fbeg[2]) = $BF) then begin
         FcodePage := CP_UTF8;
         inc(Fbeg, 3);
         dec(Fnremaining, 3);
      end else if (Fnremaining >= 2) and (Byte(Fbeg[0]) = $FE) and (Byte(Fbeg[1]) = $FF) then
         raise ECSVReadError.Create('Found UTF-16(BE) BOM-marker. Cannot handle UTF-16 encoded streams.')
      else if (Fnremaining >= 2) and (Byte(Fbeg[0]) = $FF) and (Byte(Fbeg[1]) = $FE) then
         raise ECSVReadError.Create('Found UTF-16(LE) BOM-marker. Cannot handle UTF-16 encoded streams.')
      else
         // No BOM-marker, so we are done.
         break;

      // Because we skipped the BOM-marker, we must check again if the remaining buffer is large
      // enough, so we loop.
   end;

   // ok, we have another character
   result := true;

   // check if the next character is a line-ending
   if not inquote and (Fbeg[Fcur + 1] in FlineEndingsFirst) then begin
      if FmaxLenLineEnding = 1 then begin
         // single character line ending -> done
         Fbegnxt := Fcur + 2;
         eol := True;
      end else begin
         // multi-character line ending
         maxEol := 0;
         for i := 0 to High(FlineEndings) do
            if (Fcur + Length(FlineEndings[i]) <= Fnremaining) and
               (strlcomp(@Fbeg[Fcur + 1], PChar(FlineEndings[i]), Length(FlineEndings[i])) = 0)
            then
               maxEol := Max(maxEol, Length(FlineEndings[i]));

         if maxEol > 0 then begin
            Fbegnxt := Fcur + 1 + maxEol;
            eol := True;
         end;
      end;
   end;

   inc(Fcur);
   ch := Fbeg[Fcur];

   if Fskip > 0 then Fbeg[Fcur - Fskip] := ch;
end;

procedure TCSVReader.SetSourceText(str : String);
begin
   SetSource(TStringStream.Create(str));
   FownsStream := true;
   FcodePage := StringCodePage(str);
end;

procedure TCSVReader.SetSource(stream : TStream);
begin
   if FownsStream then FreeAndNil(Fstream);
   Fstream := stream;
   Reset;
end;

procedure TCSVReader.SetSourceFile(filename : String);
begin
   SetSource(TFileStream.Create(filename, fmOpenRead));
   FownsStream := True;
end;

procedure TCSVReader.SetLineEnding(eol: String);
begin
   FlineEndings := [eol];
   FlineEndingsFirst := [eol[1]];
   FmaxLenLineEnding := Length(eol);
end;

procedure TCSVReader.SetLineEndings(eols: TStringArray);
var
   lend: String;
begin
   if Length(eols) > 0 then
      FlineEndings := Copy(eols)
   else
      FlineEndings := DefaultLineEndings;
   FmaxLenLineEnding := 0;
   FlineEndingsFirst := [];
   for lend in FlineEndings do begin
      FmaxLenLineEnding := Max(FmaxLenLineEnding, Length(lend));
      include(FlineEndingsFirst, lend[1]);
   end;
end;

function TCSVReader.GetLineEnding: String;
begin
   result := FlineEndings[0];
end;

function TCSVReader.GetLineEndings: TStringArray;
begin
   result := FlineEndings;
end;

function TCSVReader.ReadRow(out row : TRow) : Boolean;
begin
   ParseHeaders;

   if not NextRow then exit(false);

   FminNumCols := Min(FminNumCols, FnumCells);
   FmaxNumCols := Max(FmaxNumCols, FnumCells);

   if (FmaxBndCols > 0) and (FnumCells > FmaxBndCols) then
      raise ECSVReadError.Create(Format('Too many columns in row %d (expected at most %d, got %d)',
                                        [FnumRows, FmaxBndCols, FnumCells]));

   if FminBndCols > FnumCells then
      raise ECSVReadError.Create(Format('Too few columns in row %d (expected at least %d, got %d)',
                                        [FnumRows, FminBndCols, FnumCells]));

   row.Fbeg := Fbeg;
   row.Fcells := Fcells;
   row.FnumCells := FnumCells;
   row.FcodePage := FcodePage;
   row.FformatSettings := @FformatSettings;

   result := true;
end;

function TCSVReader.ReadRow(out elems : TStringArray) : Boolean;
var
   row : TRow;
begin
   result := ReadRow(row);
   if result then elems := row.ToArray;
end;

function TCSVReader.ReadNamedRow(out row : TRow) : Boolean;
var
   i, pos : Integer;
begin
   ParseHeaders;

   if Length(FnamedIndices) = 0 then
      raise ECSVReadError.Create('Header names must be set for `ReadNamedRow`');

   if not NextRow then exit(false);

   FminNumCols := Min(FminNumCols, FnumCells);
   FmaxNumCols := Max(FmaxNumCols, FnumCells);

   if (FmaxBndCols > 0) and (FnumCells > FmaxBndCols) then
      raise ECSVReadError.Create(Format('Too many columns in row %d (expected at most %d, got %d)',
                                        [FnumRows, FmaxBndCols, FnumCells]));

   if FminBndCols > FnumCells then
      raise ECSVReadError.Create(Format('Too few columns in row %d (expected at least %d, got %d)',
                                        [FnumRows, FminBndCols, FnumCells]));

   row.FcodePage := FcodePage;
   row.Fbeg := Fbeg;
   row.FnumCells := Min(FnumCells, Length(FnamedHeaders));
   SetLength(row.Fcells, Length(FnamedIndices));
   for i := 0 to Min(High(FnamedIndices), FnumCells - 1) do begin
      pos := FnamedIndices[i];
      if pos >= 0 then row.Fcells[pos] := Fcells[i];
   end;
   for i := Min(Length(FnamedIndices), FnumCells) to High(FnamedIndices) do begin
      pos := FnamedIndices[i];
      if pos >= 0 then row.Fcells[pos].count := 0;
   end;

   result := true;
end;

function TCSVReader.ReadNamedRow(out elems : TStringArray) : Boolean;
var
   row : TRow;
begin
   result := ReadNamedRow(row);
   if result then elems := row.ToArray;
end;

function TCSVReader.Rows : TRowEnum;
begin
   result.Freader := self;
end;

function TCSVReader.NamedRows : TNamedEnum;
begin
   result.Freader := self;
end;

function TCSVReader.NamedRows(names : array of String) : TNamedEnum;
var
   nheaders : TStringArray;
   i : Integer;
begin
   SetLength(nheaders, Length(names));
   for i := 0 to High(names) do nheaders[i] := names[i];
   SetNamedHeaders(nheaders, False);
   result := self.NamedRows;
end;

function TCSVReader.GetEnumerator : TRowEnum;
begin
   result := Rows;
end;


{ TCSVWriter }

constructor TCSVWriter.Create;
begin
   inherited;
   FownsStream := false;
   Fstream := nil;

   Fdelimiter := DefaultDelimiter;
   FlineEnding := System.LineEnding;
   FquoteChar := DefaultQuoteChar;
   FforceQuote := False;

   FnumRows := 0;
   FnumRowCells := 0;
   FnumCells := 0;
end;

constructor TCSVWriter.Create(stream : TStream);
begin
   Create;
   SetOutput(stream);
end;

constructor TCSVWriter.Create(filename : String);
begin
   Create;
   SetOutputFile(filename);
end;

destructor TCSVWriter.Destroy;
begin
   if FownsStream then FreeAndNil(Fstream);
end;

procedure TCSVWriter.SetOutput(stream : TStream);
begin
   if (Fstream <> nil) and (FownsStream) then FreeAndNil(Fstream);
   Fstream := stream;
end;

procedure TCSVWriter.SetOutputFile(filename : String);
begin
   SetOutput(TBufferedFileStream.Create(filename, fmCreate));
   FownsStream := true;
end;

procedure TCSVWriter.SetLineEnding(eol: String);
begin
   if Length(eol) > 0 then
      FlineEnding := eol
   else
      FlineEnding := System.LineEnding;
end;

procedure TCSVWriter.WriteChar(ch : char);
begin
   Fstream.WriteBuffer(ch, sizeof(ch));
end;

procedure TCSVWriter.WriteCell(s : String);
var
   b, e : Integer;
begin
   if Length(s) = 0 then begin
      if FforceQuote then begin
         WriteChar(FquoteChar);
         WriteChar(FquoteChar);
      end;
      inc(FnumCells);
      inc(FnumRowCells);
      exit;
   end;
   if FforceQuote
      or (s[Low(s)] in WhiteSpace)
      or (s[High(s)] in WhiteSpace)
      or (PosSet([Fdelimiter, FquoteChar], s) > 0)
      or ((FlineEnding <> '') and (Pos(FlineEnding, s) > 0))
      or ((FlineEnding = '') and (PosSet([#10, #13], s) > 0))
   then begin
      WriteChar(FquoteChar);
      b := 1;
      while true do begin
         e := PosEx(FquoteChar, s, b);
         if e = 0 then break;
         Fstream.WriteBuffer(s[b], e - b + 1);
         WriteChar(FquoteChar);
         b := e + 1;
      end;
      if b <= Length(s) then
         Fstream.WriteBuffer(s[b], Length(s) - b + 1);
      WriteChar(FquoteChar);
   end else
      Fstream.WriteBuffer(s[Low(s)], Length(s));

   inc(FnumCells);
   inc(FnumRowCells);
end;

procedure TCSVWriter.AddRow(elems : array of String);
var
   s : String;
begin
   if Fstream = nil then raise ECSVWriteError.Create('No output stream set');
   for s in elems do begin
      if FnumRowCells > 0 then WriteChar(Fdelimiter);
      WriteCell(s);
   end;
   EndRow;
end;

procedure TCSVWriter.AddRow(elems : array of const);
begin
   AddCells(elems);
   EndRow;
end;

procedure TCSVWriter.AddCell(elem : String);
begin
   if FnumRowCells > 0 then WriteChar(Fdelimiter);
   WriteCell(elem);
end;

procedure TCSVWriter.AddCells(elems : array of String);
var
   i : Integer;
begin
   for i := 0 to High(elems) do begin
      if FnumRowCells > 0 then WriteChar(Fdelimiter);
      WriteCell(elems[i]);
   end
end;

procedure TCSVWriter.AddCells(elems : array of const);
var
   v : TVarRec;
begin
   if Fstream = nil then raise ECSVWriteError.Create('No output stream set');
   for v in elems do begin
      if FnumRowCells > 0 then WriteChar(Fdelimiter);
      case v.vtype of
         vtInteger   : WriteCell(v.vinteger.toString);
         vtBoolean   : WriteCell(v.vBoolean.toString);
         vtChar      : WriteCell(v.vChar);
         vtWideChar  : WriteCell(AnsiString(v.vWideChar));
         vtExtended  : WriteCell(FloatToStr(v.vExtended^));
         vtString    : WriteCell(v.vString^);
         vtPChar     : WriteCell(v.vPChar);
         vtObject    : WriteCell(v.vObject.toString);
         vtPWideChar : WriteCell(AnsiString(v.vPWideChar));
         vtAnsiString: WriteCell(AnsiString(v.vAnsiString));
         vtCurrency  : WriteCell(CurrToStr(v.vCurrency^));
         vtVariant   : WriteCell(v.vVariant^.toString);
         vtInt64     : WriteCell(v.vInt64^.toString);
         vtQWord     : WriteCell(v.vQWord^.toString);
      end;
   end;
end;

const
   LineEndingStr: String = LineEnding;

procedure TCSVWriter.EndRow;
begin
   if FnumCells > 0 then begin
      if FlineEnding <> '' then
         Fstream.WriteBuffer(FlineEnding[1], Length(LineEnding))
      else
         Fstream.WriteBuffer(LineEndingStr[1], Length(LineEndingStr));
      inc(FnumRows);
      FnumRowCells := 0;
   end
end;

end.
