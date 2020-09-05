#! /bin/bash

#### check_scan_parse_output.sh
#### 3DLDF-2.0.3/src/check_scan_parse_output.sh

#### Created by Laurence D. Finston (LDF) Sun May 19 16:48:34 CEST 2013

#echo "Arg 1 == $1"
#echo "Arg 2 == $2"

if test -e "$2"
then
   echo "$2 exists"
   diff --brief --ignore-all-space --ignore-blank-lines \
        --ignore-matching-lines='^[[:space:]]*\(#line\|/\*\)' $1 $2 
   r=$?
   if test $r -eq 1
   then
      echo "$2 has changed."
      mv $1 $2
   else
      echo "$2 is unchanged."
   fi
else
   echo "$2 does not exist"
   mv $1 $2
fi 

exit 0
