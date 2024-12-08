#!/bin/bash

pth=$(dirname -- "$0")
day=$(printf "%02d" "$1")
pushd $pth >& /dev/null
src="src/day${day}.pas"

SESSIONCOOKIE=$(cat .session)

if [ "$day" != "00" ]; then
    # new unit from template
    cp -i "src/dayTPL.pas" $src
    sed -i -e "s/DAY/$day/" $src
    hg add $src

    # download input
    mkdir -p "input/$day"
    curl "https://adventofcode.com/2024/day/$1/input" --compressed -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:120.0) Gecko/20100101 Firefox/120.0' -H "Cookie: session=$SESSIONCOOKIE" > input/$day/input1.txt

    # create empty testcase
    [ -f input/$day/test_part.txt ] || echo "EXPECTED: 4 2" > input/$day/test.txt
fi

# Auto generate files for all known days

DAYS=$(find src -maxdepth 1 -name "day??.pas" | sed -e 's!src/day\(.*\)\.pas!\1!' | sort)

if  [ ! -z "$DAYS" ]; then
    printf ", Day%s\n" $DAYS > src/days.inc
else
    echo > src/days.inc
fi

# edit lpi to add day??.pas source files
for lpi in aoc24*.lpi; do
    # first delete all existing src/day??.pas files as sources
    xml ed --inplace --delete '//ProjectOptions/Units/Unit[Filename[starts-with(@Value, "src/day") and contains(@Value, "pas")]]' "$lpi"
    # add all known days again
    for d in $DAYS; do
        xml ed --inplace \
            --subnode '//ProjectOptions/Units' -t elem --name "Unit" \
            --var newunit '$prev' \
            --subnode '$newunit' --type elem --name "Filename" \
            --insert '$prev' --type attr --name "Value" --value $(printf "src/day%s.pas" $d) \
            --subnode '$newunit' --type elem --name "IsPartOfProject" \
            --insert '$prev' --type attr --name "Value" --value "True" \
            --subnode '$newunit' --type elem --name "UnitName" \
            --insert '$prev' --type attr --name "Value" --value $(printf "Day%s" $d) \
            "$lpi"
    done
done

popd >& /dev/null
