#!/bin/bash

#>results.txt
for FILE in testcases/*.xml
do
  BN=$(basename $FILE)
  echo Launching test $BN
  sipp -trace_err -m 1 -sf $FILE -t u1 -l 1 127.0.0.1:1234
  if test $? -ne 0
    then
        echo -e "\e[31mTest $BN: FAIL\e[0m"
    else
        echo -e "\e[31mTest $BN: SUCCESS\e[0m"
    fi
done
exit 0
