#! /bin/sh
# 
# option -s

: ${srcdir=.}

failures=0

# should return an error
match=`${BOOL} 'a' unknown.txt 2>&1`
if test -z "$match" ; then
  echo "Test #1 failed"
  failures=1
fi

# should return nothing
match=`${BOOL} -s 'a' unknown.txt 2>&1`
if test -n "$match" ; then
  echo "Test #2 failed"
  failures=1
fi

exit $failures
