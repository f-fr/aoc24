build:
    mkdir -p bin/x86_64-linux
    fpc -dDEBUG -obin/x86_64-linux/aoc24 -Fusrc src/aoc24

release:
    mkdir -p bin/x86_64-linux
    fpc -dRELEASE -obin/x86_64-linux/aoc24 -Fusrc src/aoc24

llvm:
    mkdir -p bin/x86_64-linux
    fpc-llvm -dRELEASE -obin/x86_64-linux/aoc24 -Fusrc src/aoc24

test:
    mkdir -p bin/x86_64-linux
    fpc -dDEBUG -obin/x86_64-linux/aoc24tests -Fusrc src/testrunner
    bin/x86_64-linux/aoc24tests --format=plain --all

clean:
    fppkg clean
    rm -rf fpmake
    rm -f src/*.o
    rm -f src/*.ppu
