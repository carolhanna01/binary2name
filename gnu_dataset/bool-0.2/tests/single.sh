#! /bin/sh
#
# single character input

: ${srcdir=.}

failures=0

# should return 0 found a match
echo 'a' | ${BOOL} 'a' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return 0 found a match
${BOOL} 'a' single.txt > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #2 failed"
  failures=1
fi

# should return 1 found no match
echo 'a' | ${BOOL} 'b' > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #3 failed"
  failures=1
fi

# should return 1 found no match
${BOOL} 'b' single.txt > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #4 failed"
  failures=1
fi

exit $failures
