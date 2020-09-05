#! /bin/sh
#
# empty input

: ${srcdir=.}

failures=0

# should return 1 found no match
echo "" | ${BOOL} -F 'one' > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return 1 found no match
${BOOL} 'one' empty.txt > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #2 failed"
  failures=1
fi

exit $failures
