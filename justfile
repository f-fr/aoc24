build:
    mkdir -p bin/x86_64-linux-debug
    mkdir -p units/x86_64-linux-debug
    fpc -dDEBUG -gh -FEbin/x86_64-linux-debug -FUunits/x86_64-linux-debug -Fusrc src/aoc24

release:
    mkdir -p bin/x86_64-linux-release
    mkdir -p units/x86_64-linux-release
    fpc -dRELEASE -O3 -XX -CX -Xs -FEbin/x86_64-linux-release -FUunits/x86_64-linux-release -Fusrc src/aoc24

llvm:
    mkdir -p bin/x86_64-linux-llvm
    mkdir -p units/x86_64-linux-llvm
    fpc-llvm -dRELEASE -O3 -XX -CX -Xs -FEbin/x86_64-linux-llvm -FUunits/x86_64-linux-llvm -Fusrc src/aoc24

test:
    mkdir -p bin/x86_64-linux-debug
    mkdir -p units/x86_64-linux-debug
    fpc -dDEBUG -oaoc24tests -FEbin/x86_64-linux-debug -FUunits/x86_64-linux-debug -Fusrc src/testrunner
    bin/x86_64-linux-debug/aoc24tests --format=plain --all

clean:
    fppkg clean
    rm -rf fpmake
    rm -rf bin units
    rm -f src/*.o
    rm -f src/*.ppu
