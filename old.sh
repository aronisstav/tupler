#!/bin/bash

echo "Building Tupler"
make all

echo "Building Tests"
make test

echo "Testing simple cases"
./tupler test/record1.beam > record1.out.txt
./tupler test/record2.beam > record2.out.txt
./tupler test/record3.beam > record3.out.txt
./tupler test/record4.beam > record4.out.txt
./tupler test/record5.beam > record5.out.txt
./tupler test/record6.beam > record6.out.txt
./tupler test/record7.beam > record7.out.txt
./tupler test/record8.beam > record8.out.txt
./tupler test/record9.beam > record9.out.txt
./tupler test/record10.beam > record10.out.txt
./tupler test/record11.beam > record11.out.txt
./tupler test/record12.beam > record12.out.txt
./tupler test/record13.beam > record13.out.txt
./tupler test/record14.beam > record14.out.txt
./tupler test/record15.beam > record15.out.txt
./tupler test/record16.beam > record16.out.txt

#sr1=`cat ./record1.out.txt | awk '/Within the scope of/{print $1}'`
#if [ "$sr1" == '' ]; then
#	echo "Simple Test: 1. Failed. Warning escaped"
#elif [ "$sr1" != '10' ]; then
#	echo "Simple Test: 1. Failed. Illegal Warning printed"
#else
#	echo "Simple Test: 1. PASSED"
#fi

if [[ -s "./record1.out.txt" ]] ; then
	echo "Simple Test: 1. PASSED"
else
	echo "Simple Test: 1. Failed. Warning escaped"
fi

if [[ -s "./record2.out.txt" ]] ; then
	echo "Simple Test: 2. PASSED"
else
	echo "Simple Test: 2. Failed. Warning escaped"
fi

if [[ -s "./record3.out.txt" ]] ; then
	echo "Simple Test: 3. Failed. Illegal Warning printed"
else
	echo "Simple Test: 3. PASSED"
fi

if [[ -s "./record4.out.txt" ]] ; then
	echo "Simple Test: 4. PASSED"
else
	echo "Simple Test: 4. Failed. Warning escaped"
fi

if [[ -s "./record5.out.txt" ]] ; then
	echo "Simple Test: 5. PASSED"
else
	echo "Simple Test: 5 Failed. Warning escaped"
fi

if [[ -s "./record6.out.txt" ]] ; then
	echo "Simple Test: 6. PASSED"
else
	echo "Simple Test: 6. Failed. Warning escaped"
fi

if [[ -s "./record7.out.txt" ]] ; then
	echo "Simple Test: 7. Failed. Illegal Warning printed"
else
	echo "Simple Test: 7. PASSED"
fi

if [[ -s "./record8.out.txt" ]] ; then
	echo "Simple Test: 8. PASSED"
else
	echo "Simple Test: 8. Failed. Warning escaped"
fi

if [[ -s "./record9.out.txt" ]] ; then
	echo "Simple Test: 9. PASSED"
else
	echo "Simple Test: 9. Failed. Warning escaped"
fi

if [[ -s "./record10.out.txt" ]] ; then
	echo "Simple Test: 10. PASSED"
else
	echo "Simple Test: 10. Failed. Warning escaped"
fi

if [[ -s "./record11.out.txt" ]] ; then
	echo "Simple Test: 11. PASSED"
else
	echo "Simple Test: 11. Failed. Warning escaped"
fi

if [[ -s "./record12.out.txt" ]] ; then
	echo "Simple Test: 12. PASSED"
else
	echo "Simple Test: 12. Failed. Warning escaped"
fi

if [[ -s "./record13.out.txt" ]] ; then
	echo "Simple Test: 13. Failed. Illegal Warning printed"
else
	echo "Simple Test: 13. PASSED"
fi

if [[ -s "./record14.out.txt" ]] ; then
	echo "Simple Test: 14. Failed. Illegal Warning printed"
else
	echo "Simple Test: 14. PASSED"
fi

if [[ -s "./record15.out.txt" ]] ; then
	echo "Simple Test: 15. Failed. Illegal Warning printed"
else
	echo "Simple Test: 15. PASSED"
fi

echo "Testing function calls"
./tupler test/record_fun1.beam > record_fun1.out.txt
./tupler test/record_fun2.beam > record_fun2.out.txt

if [[ -s "./record_fun1.out.txt" ]] ; then
	echo "Function Calls Test: 1. PASSED"
else
	echo "Function Calls Test: 1. Failed. Warning escaped"
fi

if [[ -s "./record_fun2.out.txt" ]] ; then
	echo "Function Calls Test: 2. Failed. Illegal Warning printed"
else
	echo "Function Calls Test: 2. PASSED"
fi

echo "Testing complex cases"
./tupler test/test1.beam > test1.out.txt
./tupler test/tuple1.beam > tuple1.out.txt
./tupler test/tuple_fun1.beam > tuple_fun1.out.txt

if [[ -s "./test1.out.txt" ]] ; then
	echo "Complex Test: 1. PASSED"
else
	echo "Complex Test: 1. Failed. Warning escaped"
fi

echo "Cleaning."
rm *.out.txt
cd test
rm *.beam
cd ..
