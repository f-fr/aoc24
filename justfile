build:
    mkdir -p bin/x86_64-linux
    fpc -dDEBUG -obin/x86_64-linux/aoc24 -Fusrc src/aoc24

release:
    mkdir -p bin/x86_64-linux
    fpc -dRELEASE -obin/x86_64-linux/aoc24 -Fusrc src/aoc24

llvm:
    mkdir -p bin/x86_64-linux
    fpc-llvm -dRELEASE -obin/x86_64-linux/aoc24 -Fusrc src/aoc24

clean:
    fppkg clean
    rm -rf fpmake
