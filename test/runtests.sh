#!/bin/bash


#sipp -trace_err -r 10 -rate_scale 1 -trace_msg -sf testcases/01_uac.xml -t u1 -l 1 127.0.0.1:1234
sipp -trace_err -r 10 -rate_scale 1 -trace_msg -sf testcases/02_uas.xml -t u1 -l 1 127.0.0.1:1234

# rm testcases/*.log
# for FILE in testcases/*.xml
# do
#   BN=$(basename $FILE)
#   sipp -trace_err -r 10 -m 1 -rate_scale 1 -trace_msg -sf $FILE -t u1 -l 1 127.0.0.1:1234 &> /dev/null
#   if test $? -ne 0
#     then
#         echo -e "\033[1;31mTest $BN: FAIL\033[0m"
#     else
#         echo -e "\033[1;32mTest $BN: SUCCESS\033[0m"
#     fi
# done

# exit 0
