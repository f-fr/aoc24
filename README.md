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

## Results

The following tables list the running times for all days on different
platforms (x86_64, raspi2) and compiler backends (native and LLVM).
The total running time is the sum of the *fastest* version for each
day.

  | day | version |           part1 |           part2 | time (ms)|
  |:---:|:-------:|----------------:|----------------:|---------:|
  |   1 |       1 |         1223326 |        21070419 |    0.001 |
  |   1 |       2 |         1223326 |        21070419 |        0 |
  |   1 |       3 |         1223326 |        21070419 |    0.001 |
  |   1 |       4 |         1223326 |        21070419 |        0 |
  |   2 |       1 |             432 |             488 |    0.001 |
  |   2 |       2 |             432 |             488 |        0 |
  |   2 |       3 |             432 |             488 |    0.001 |
  |   2 |       4 |             432 |             488 |    0.001 |
  |   3 |       1 |       185797128 |        89798695 |        0 |
  |   3 |       2 |       185797128 |        89798695 |        0 |
  |   4 |       1 |            2534 |            1866 |    0.001 |
  |   4 |       2 |            2534 |            1866 |    0.001 |
  |   5 |       1 |            5651 |            4743 |    0.001 |
  |   5 |       2 |            5651 |            4743 |        0 |
  |   6 |       1 |            4982 |            1663 |    0.355 |
  |   6 |       2 |            4982 |            1663 |    0.014 |
  |   6 |       3 |            4982 |            1663 |    0.004 |
  |   7 |       1 |  66343330034722 | 637696070419031 |    0.002 |
  |   8 |       1 |             354 |            1263 |        0 |
  |   9 |       1 |   6461289671426 |   6488291456470 |    0.001 |
  |  10 |       1 |             617 |            1477 |        0 |
  |  11 |       1 |          235850 | 279903140844645 |    0.018 |
  |  11 |       2 |          235850 | 279903140844645 |    0.019 |
  |  12 |       1 |         1370100 |          818286 |    0.001 |
  |  13 |       1 |           27105 | 101726882250942 |        0 |
  |  14 |       1 |       218619120 |            7055 |     0.08 |
  |  14 |       2 |       218619120 |             103 |    0.001 |

  Total time: 0.028 ms

  **AMD Ryzen 5 Pro 7530U (FPC native)**

  | day | version |           part1 |           part2 | time (ms)|
  |:---:|:-------:|----------------:|----------------:|---------:|
  |   1 |       1 |         1223326 |        21070419 |        0 |
  |   1 |       2 |         1223326 |        21070419 |    0.001 |
  |   1 |       3 |         1223326 |        21070419 |        0 |
  |   1 |       4 |         1223326 |        21070419 |    0.001 |
  |   2 |       1 |             432 |             488 |        0 |
  |   2 |       2 |             432 |             488 |    0.001 |
  |   2 |       3 |             432 |             488 |        0 |
  |   2 |       4 |             432 |             488 |    0.001 |
  |   3 |       1 |       185797128 |        89798695 |        0 |
  |   3 |       2 |       185797128 |        89798695 |        0 |
  |   4 |       1 |            2534 |            1866 |        0 |
  |   4 |       2 |            2534 |            1866 |    0.001 |
  |   5 |       1 |            5651 |            4743 |    0.001 |
  |   5 |       2 |            5651 |            4743 |        0 |
  |   6 |       1 |            4982 |            1663 |    0.112 |
  |   6 |       2 |            4982 |            1663 |    0.007 |
  |   6 |       3 |            4982 |            1663 |    0.003 |
  |   7 |       1 |  66343330034722 | 637696070419031 |    0.002 |
  |   8 |       1 |             354 |            1263 |        0 |
  |   9 |       1 |   6461289671426 |   6488291456470 |        0 |
  |  10 |       1 |             617 |            1477 |    0.001 |
  |  11 |       1 |          235850 | 279903140844645 |    0.014 |
  |  11 |       2 |          235850 | 279903140844645 |    0.014 |
  |  12 |       1 |         1370100 |          818286 |        0 |
  |  13 |       1 |           27105 | 101726882250942 |        0 |
  |  14 |       1 |       218619120 |            7055 |    0.044 |
  |  14 |       2 |       218619120 |             103 |    0.001 |

  Total time: 0.021 ms

  **AMD Ryzen 5 Pro 7530U (LLVM)**

  | day | version |           part1 |           part2 | time (ms)|
  |:---:|:-------:|----------------:|----------------:|---------:|
  |   1 |       1 |         1223326 |        21070419 |     0.01 |
  |   1 |       2 |         1223326 |        21070419 |    0.014 |
  |   1 |       3 |         1223326 |        21070419 |     0.01 |
  |   1 |       4 |         1223326 |        21070419 |    0.008 |
  |   2 |       1 |             432 |             488 |    0.019 |
  |   2 |       2 |             432 |             488 |    0.012 |
  |   2 |       3 |             432 |             488 |     0.01 |
  |   2 |       4 |             432 |             488 |    0.018 |
  |   3 |       1 |       185797128 |        89798695 |    0.005 |
  |   3 |       2 |       185797128 |        89798695 |    0.002 |
  |   4 |       1 |            2534 |            1866 |    0.006 |
  |   4 |       2 |            2534 |            1866 |    0.019 |
  |   5 |       1 |            5651 |            4743 |    0.011 |
  |   5 |       2 |            5651 |            4743 |    0.008 |
  |   6 |       1 |            4982 |            1663 |     5.42 |
  |   6 |       2 |            4982 |            1663 |    0.191 |
  |   6 |       3 |            4982 |            1663 |    0.091 |
  |   7 |       1 |  66343330034722 | 637696070419031 |    0.038 |
  |   8 |       1 |             354 |            1263 |    0.001 |
  |   9 |       1 |   6461289671426 |   6488291456470 |    0.011 |
  |  10 |       1 |             617 |            1477 |    0.004 |
  |  11 |       1 |          235850 | 279903140844645 |    0.486 |
  |  11 |       2 |          235850 | 279903140844645 |    0.369 |
  |  12 |       1 |         1370100 |          818286 |    0.015 |
  |  13 |       1 |           27105 | 101726882250942 |     0.01 |
  |  14 |       1 |       218619120 |            7055 |    0.005 |
  |  14 |       2 |       218619120 |            7055 |     1.49 |

  Total time: 0.58 ms

  **RasPi2 ARMv7 Processor rev 5 (FPC native)**

[aoc]: https://adventofcode.com
[fp]: https://freepascal.org
[hg]: https://mercurial-scm.org

