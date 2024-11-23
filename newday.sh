#!/bin/bash

pth=$(dirname -- "$0")
day=$(printf "%02d" "$1")
pushd $pth >& /dev/null
src="src/day${day}.pas"

SESSIONCOOKIE=$(cat .session)

# all known days
DAYS=$(ls src/day??.pas | sed -e 's!src/day\(.*\)\.pas!\1!')

cp -i "src/dayTPL.pas" $src
sed -i -e "s/DAY/$day/" $src
printf ", Day%02d\n" $DAYS > src/days.inc
hg add $src

mkdir -p "input/$day"
curl "https://adventofcode.com/2023/day/$1/input" --compressed -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:120.0) Gecko/20100101 Firefox/120.0' -H "Cookie: session=$SESSIONCOOKIE" > input/$day/input1.txt

# edit lpi to add day??.pas source files
for lpi in aoc24*.lpi; do
    # first delete all existing src/day??.pas files as sources
    xml ed --inplace --delete '//ProjectOptions/Units/Unit[Filename[starts-with(@Value, "src/day") and contains(@Value, "pas")]]' "$lpi"
    # add all known days again
    for day in $DAYS; do
        xml ed --inplace \
            --subnode '//ProjectOptions/Units' -t elem --name "Unit" \
            --var newunit '$prev' \
            --subnode '$newunit' --type elem --name "Filename" \
            --insert '$prev' --type attr --name "Value" --value $(printf "src/day%02d.pas" $day) \
            --subnode '$newunit' --type elem --name "IsPartOfProject" \
            --insert '$prev' --type attr --name "Value" --value "True" \
            --subnode '$newunit' --type elem --name "UnitName" \
            --insert '$prev' --type attr --name "Value" --value $(printf "Day%02d" $day) \
            "$lpi"
    done
done

# create empty testcases
[ -f input/$day/test_part1.txt ] || echo "EXPECTED: 42" > input/$day/test_part1.txt
[ -f input/$day/test_part2.txt ] || echo "EXPECTED: 42" > input/$day/test_part2.txt

popd >& /dev/null
