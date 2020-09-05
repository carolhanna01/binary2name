#! /bin/sh
# 
# option -n

: ${srcdir=.}

failures=0

# should return 1
num=`${BOOL} -n 'c' context.txt | ${AWK} -F: '{print $1}'`
if test $num -ne 1 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return 2
num=`${BOOL} -n 'l' context.txt | ${AWK} -F: '{print $1}'`
if test $num -ne 2 ; then
  echo "Test #2 failed"
  failures=1
fi

exit $failures
