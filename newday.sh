#!/bin/bash

pth=$(dirname -- "$0")
day=$(printf "%02d" "$1")
pushd $pth >& /dev/null
src="src/day${day}.pas"

SESSIONCOOKIE=$(cat .session)

cp -i "src/dayTPL.pas" $src
sed -i -e "s/DAY/$day/" $src
ls src/day??.pas | sed -e 's!src/day\(.*\)\.pas!, Day\1!' > src/days.inc
hg add $src

mkdir -p "input/$day"
curl "https://adventofcode.com/2023/day/$1/input" --compressed -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:120.0) Gecko/20100101 Firefox/120.0' -H "Cookie: session=$SESSIONCOOKIE" > input/$day/input1.txt

popd >& /dev/null
