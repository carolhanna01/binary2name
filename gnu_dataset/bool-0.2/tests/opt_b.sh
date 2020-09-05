#! /bin/sh
# 
# option -b

: ${srcdir=.}

failures=0

# should return 1 found no match
num=`${BOOL} -b 'c' context.txt | ${AWK} -F: '{print $1}'`
if test $num -ne 34 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return 1 found no match
num=`${BOOL} -b 'l' context.txt | ${AWK} -F: '{print $1}'`
if test $num -ne 108 ; then
  echo "Test #2 failed"
  failures=1
fi

exit $failures
