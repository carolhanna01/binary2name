#! /bin/sh
# 
# option -i

: ${srcdir=.}

failures=0

# should match
match=`echo "abc" | ${BOOL} 'a'`
if test "$match" != "abc" ; then
  echo "Test #1 failed"
  failures=1
fi

# should not match
match=`echo "abc" | ${BOOL} 'A'`
if test -n "$match" ; then
  echo "Test #2 failed"
  failures=1
fi

# should match
match=`echo "abc" | ${BOOL} -i 'A'`
if test "$match" != "abc" ; then
  echo "Test #3 failed"
  failures=1
fi

# should not match
match=`echo "ABC" | ${BOOL} 'a'`
if test -n "$match" ; then
  echo "Test #4 failed"
  failures=1
fi

# should match
match=`echo "ABC" | ${BOOL} -i 'a'`
if test "$match" != "ABC" ; then
  echo "Test #5 failed"
  failures=1
fi

exit $failures
