#!/bin/bash

echo "Building Tupler."
make all

echo "Building Tests."
make test

mkdir test/out

echo "Executing Tests."

for name in test/*.beam;
do
	var=$(basename $name .beam)
	./tupler ${name} > test/out/${var}.out.txt
	echo "./tupler ${name} > out/${var}.out.txt"
done

echo "Parsing Results."

for name in test/out/*.out.txt;
do
	var=$(basename $name .out.txt)
	if [[ -e "test/correct/${var}.out.txt" ]] ; then
		diff ${name}  test/correct/${var}.out.txt > aux.txt
		if [[ -s "./aux.txt" ]] ; then
			echo "ERROR: @ $name"
			cat aux.txt
		fi
	else
		echo "New FILE without base test case inserted: $name"
	fi
done

echo "Cleaning."
rm aux.txt
cd test
rm -Rf out
rm *.beam
cd ..
