#!/bin/bash

echo "Building Recorder."
make all

echo "Building Tests."
make test

mkdir test/out

echo "Executing Tests."

for name in test/*.beam;
do
	var=$(basename $name .beam)
	./recorder ${name} > test/out/${var}.out.txt
	echo "./recorder ${name} > out/${var}.out.txt"
done

echo "Parsing Results."

passed=0
failed=0
unknown=0

for name in test/out/*.out.txt;
do
	var=$(basename $name .out.txt)
	if [[ -e "test/correct/${var}.out.txt" ]] ; then
		diff test/correct/${var}.out.txt ${name} > aux.txt
		if [[ -s "./aux.txt" ]] ; then
			let failed+=1
			echo "ERROR: @ $name"
			cat aux.txt
		else
			let passed+=1
		fi
	else
		let unknown+=1
		echo "New FILE without base test case inserted: $name"
	fi
done

total=$(( failed + passed + unknown ))
echo "Quick Statistics for $total files. Passed: $passed, Failed: $failed, Unknown: $unknown "

echo "Cleaning."
rm aux.txt
cd test
rm -Rf out
rm *.beam
cd ..
