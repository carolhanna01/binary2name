#! /bin/sh
# 
# option -F

: ${srcdir=.}

failures=0

# should return 2 matches
num=`echo "a and b" | ${BOOL} 'a and n' \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 2 ; then
  echo "Test #1 failed"
  failures=1
fi

# should not return a match
num=`echo "a and b" | ${BOOL} -F 'a and n' \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 0 ; then
  echo "Test #2 failed"
  failures=1
fi

# should return 2 matches
num=`echo "a and b" | ${BOOL} 'a and b' \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 2 ; then
  echo "Test #3 failed"
  failures=1
fi

# should return 1 match
num=`echo "a and b" | ${BOOL} -F 'a and b' \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 1 ; then
  echo "Test #4 failed"
  failures=1
fi

exit $failures
