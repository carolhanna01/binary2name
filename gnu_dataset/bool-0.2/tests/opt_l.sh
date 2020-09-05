#! /bin/sh
# 
# option -l
# option -L

: ${srcdir=.}

failures=0

# should return 1 lines, found a match
num=`${BOOL} -l 'a' single.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 1 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return 0 lines, found no match
num=`${BOOL} -l 'a' empty.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 0 ; then
  echo "Test #2 failed"
  failures=1
fi

# should return 1 line, found a match
num=`${BOOL} -l 'a' single.txt empty.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 1 ; then
  echo "Test #3 failed"
  failures=1
fi

# should return 1 line, found a match
num=`${BOOL} -l 'a' empty.txt single.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 1 ; then
  echo "Test #4 failed"
  failures=1
fi

# should return 0 lines, found a match
num=`${BOOL} -L 'a' single.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 0 ; then
  echo "Test #5 failed"
  failures=1
fi

# should return 1 line, found no match
num=`${BOOL} -L 'a' empty.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 1 ; then
  echo "Test #6 failed"
  failures=1
fi

# should return 1 line, found a match
num=`${BOOL} -L 'a' single.txt empty.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 1 ; then
  echo "Test #7 failed"
  failures=1
fi

# should return 1 line, found a match
num=`${BOOL} -L 'a' empty.txt single.txt \
  | ${AWK} 'BEGIN{i=0}; {i++}; END{print i}'`
if test $num -ne 1 ; then
  echo "Test #8 failed"
  failures=1
fi

exit $failures
