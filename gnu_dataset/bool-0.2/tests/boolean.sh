#!/bin/sh
#
# boolean expressions

: ${srcdir=.}

failures=0

# should return 0 found a match
echo "a b c" | ${BOOL} 'a AND c' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return 0 found a match
echo "a b c" | ${BOOL} 'a OR c' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #2 failed"
  failures=1
fi

# should return 0 found a match
echo "a b c" | ${BOOL} 'a NOT d' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #3 failed"
  failures=1
fi

# should return 0 found a match
echo "a b c" | ${BOOL} 'a NEAR b' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #4 failed"
  failures=1
fi

# should return 0 found a match
echo "a b c" | ${BOOL} 'a NEAR c' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #5 failed"
  failures=1
fi

# should return 0 found a match
echo "a b c" | ${BOOL} 'a NEAR b NEAR c' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #6 failed"
  failures=1
fi

exit $failures
