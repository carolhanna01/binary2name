#! /bin/sh
#
# option -D

: ${srcdir=.}

failures=0

# should match
${BOOL} 'a near k' context.txt > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #1 failed"
  failures=1
fi

# should not match
${BOOL} 'a near l' context.txt > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #2 failed"
  failures=1
fi

# should match
${BOOL} -D 5 'a near f' context.txt > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #3 failed"
  failures=1
fi

# should not match
${BOOL} -D 5 'a near g' context.txt > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #4 failed"
  failures=1
fi

# should match
${BOOL} -D 12 'a near m' context.txt > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #5 failed"
  failures=1
fi

# should match
${BOOL} -D 12 'a near n' context.txt > /dev/null
if test $? -ne 1 ; then
  echo "Test #6 failed"
  failures=1
fi

exit $failures
