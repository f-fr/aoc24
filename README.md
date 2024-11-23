# Advent of Code 2024

My solution, this time in [FreePascal][fp]. My personal goal is to keep the running
time for each day well below 0.1s (on my Raspberry Pi 2).

## Creating templates for new days

There is a helper shell-script to create empty files for each day:

    ./newday.sh XX
    
where XX is a number 1, 2, ... creates a new unit `DayXX` (in file
`src/dayXX.pas`) along with empty test inputs
`input/XX/test_part1.txt` and `input/XX/test_part2.txt`. Furthermore,
the script download the instance file from the
[Advent-of-Code-website][aoc]. However, this works only if there is a
`.session` file in the root directory of the project (not contained in
the repository) that contains a valid session-key.

The script also registers the new file in [Mercurial][hg] (sorry, I
don't use git).

## Compiling

There are three predefined ways to compile everything:

### justfile / direct compiler invocation

There are several predefined targets for `just`, that call the compiler directly:

1. `just build`: compile in debug mode
2. `just release`: compile in release mode
3. `just llvm`: compile with LLVM backend in release mode (requires the `fpc-llvm` executable)
4. `just run`: compile in release mode and run all days
5. `just run-llvm`: compile in release mode with LLVM backend and run all days
6. `just test`: compile and run all tests
7. `just clean`: clean up

### fppkg

Run

    fppkg build

in the base directory. Note that this will compile the program with the default
options (which may not be release mode).

### lazbuild

If you use the Lazarus-IDE, you can compile/run everything from within
the IDE. Alternatively one may use `lazbuild` to compile the projects
from the command line:

1. `lazbuild aoc24.lpi`: compile the main runner
2. `lazbuild aoc24tests.lpi`: compile the command line test runner
3. `lazbuild aoc24testsgui.lpi`: compile the gui test runner

## Running the days

There is a single executable `bin/*/aoc24` to run (and time) the solutions of a
single day or all days:

1. `aoc24`: run all versions of all days. The total time is the sum of
   the fastest versions of each day.
2. `aoc24 DAY`: (`DAY` is a number 1, 2, ...) Run the solution of day `DAY`.
3. `aoc24 -v VER DAY`: Run version `VER` of day `DAY`.

Yes, there can be multiple solutions for a single day!

## Running the tests

For each day there are unit tests for both parts. There are two
test-runner executables for running all tests:

1. `aoc24tests`: run all tests with output to the command line
2. `aoc24testsgui`: run all tests in a simple graphical GUI

The former is suitable to be run from the command line. The latter works better
when running the tests from Lazarus.

[aoc]: https://adventofcode.com
[fp]: https://freepascal.org
[hg]: https://mercurial-scm.org

