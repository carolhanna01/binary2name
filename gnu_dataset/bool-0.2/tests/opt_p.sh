#! /bin/sh
# 
# option -p
# option -P

: ${srcdir=.}

failures=0

# should not return pattern
pat=`echo 'abc' | ${BOOL} 'b' | ${AWK} -F: '{print $1}'`
if test "$pat" = "b" ; then
  echo "Test #1 failed"
  failures=1
fi

# should return pattern
pat=`echo 'abc' | ${BOOL} 'b or k' | ${AWK} -F: '{print $1}'`
if test "$pat" != "b" ; then
  echo "Test #2 failed"
  failures=1
fi

# should not return pattern
pat=`echo 'abc' | ${BOOL} -p 'b' | ${AWK} -F: '{print $1}'`
if test "$pat" = "b" ; then
  echo "Test #3 failed"
  failures=1
fi

# should not return pattern
pat=`echo 'abc' | ${BOOL} -p 'b or k' | ${AWK} -F: '{print $1}'`
if test "$pat" = "b" ; then
  echo "Test #4 failed"
  failures=1
fi

# should return pattern
pat=`echo 'abc' | ${BOOL} -P 'b' | ${AWK} -F: '{print $1}'`
if test "$pat" != "b" ; then
  echo "Test #5 failed"
  failures=1
fi

# should not return pattern
pat=`${BOOL} -P 'b' empty.txt | ${AWK} -F: '{print $1}'`
if test -n "$pat" ; then
  echo "Test #6 failed"
  failures=1
fi

exit $failures
