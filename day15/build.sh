#!/bin/bash

rm -f fun

csc -o fun fun.scm

cp -v fun fun1
cp -v fun fun2
cp -v fun fun3
cp -v fun fun4
cp -v fun fun5
cp -v fun fun6
cp -v fun fun7
cp -v fun fun8
# cp -v fun fun9
# cp -v fun fun10
# cp -v fun fun11
# cp -v fun fun12
# cp -v fun fun13
# cp -v fun fun14
# cp -v fun fun15
# cp -v fun fun16


./fun1 1 &
./fun2 2 &
./fun3 3 &
./fun4 4 &
./fun5 5 &
./fun6 6 &
./fun7 7 &
./fun8 8 &
# ./fun9 9 &
# ./fun10 10 &
# ./fun11 11 &
# ./fun12 12 &
# ./fun13 13 &
# ./fun14 14 &
# ./fun15 15 &
# ./fun16 16 &





