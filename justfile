build:
    mkdir -p bin/x86_64-linux-debug
    mkdir -p units/x86_64-linux-debug
    fpc -dDEBUG -gh -FEbin/x86_64-linux-debug -FUunits/x86_64-linux-debug -Fusrc src/aoc24

release:
    mkdir -p bin/x86_64-linux-release
    mkdir -p units/x86_64-linux-release
    fpc -dRELEASE -duselibc -O3 -XX -CX -Xs -FEbin/x86_64-linux-release -FUunits/x86_64-linux-release -Fusrc src/aoc24

llvm:
    mkdir -p bin/x86_64-linux-llvm
    mkdir -p units/x86_64-linux-llvm
    fpc-llvm -dRELEASE -duselibc -O3 -XX -CX -Xs -FEbin/x86_64-linux-llvm -FUunits/x86_64-linux-llvm -Fusrc src/aoc24

run-dbg: build
    bin/x86_64-linux-debug/aoc24

run: release
    bin/x86_64-linux-release/aoc24

run-llvm: llvm
    bin/x86_64-linux-llvm/aoc24

table: release llvm
    bin/x86_64-linux-release/aoc24 -r
    bin/x86_64-linux-llvm/aoc24 -r

test:
    mkdir -p bin/x86_64-linux-debug
    mkdir -p units/x86_64-linux-debug
    fpc -dDEBUG -dTESTING -gh -oaoc24tests -FEbin/x86_64-linux-debug -FUunits/x86_64-linux-debug -Fusrc src/testrunner
    bin/x86_64-linux-debug/aoc24tests --format=plain --all

clean:
    fppkg clean
    rm -f fpmake fpmake.o
    rm -rf bin units
    rm -f src/*.o
    rm -f src/*.ppu
