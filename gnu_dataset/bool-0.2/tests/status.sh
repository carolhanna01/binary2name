#!/bin/sh
#
# status codes:
# 0 match found
# 1 no match

: ${srcdir=.}

failures=0

# should return 0, found a match
echo "abcd" | ${BOOL} -F 'abc' > /dev/null 2>&1
if test $? -ne 0 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return 1, found no match
echo "abcd" | ${BOOL} -F 'zbc' > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #2 failed"
  failures=1
fi

# should return 1, no such file
${BOOL} 'abc' unknown.txt > /dev/null 2>&1
if test $? -ne 1 ; then
  echo "Test #3 failed"
  failures=1
fi

exit $failures
