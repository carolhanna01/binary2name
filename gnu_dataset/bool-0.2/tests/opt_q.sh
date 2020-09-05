#! /bin/sh
# 
# option -q

: ${srcdir=.}

failures=0

# should return match
match=`${BOOL} 'a' single.txt`
if test "$match" != "a" ; then
  echo "Test #1 failed"
  failures=1
fi

# should return nothing
match=`${BOOL} -q 'a' single.txt`
if test -n "$match" ; then
  echo "Test #2 failed"
  failures=1
fi

exit $failures
