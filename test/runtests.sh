#!/bin/bash

rm testcases/*.log

for FILE in testcases/*.xml
do
  BN=$(basename $FILE)
  sipp -trace_err -trace_msg -m 1 -sf $FILE -t u1 -l 1 127.0.0.1:1234 &> /dev/null
  if test $? -ne 0
    then
        echo -e "\033[1;31mTest $BN: FAIL\033[0m"
    else
        echo -e "\033[1;32mTest $BN: SUCCESS\033[0m"
    fi
done

exit 0
x
