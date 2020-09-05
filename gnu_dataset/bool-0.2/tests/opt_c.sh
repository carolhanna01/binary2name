#! /bin/sh
# 
# option -c

: ${srcdir=.}

failures=0

# should return count of 1
count=`${BOOL} -c 'a' context.txt`
if test $count -ne 1 ; then
  echo "Test #1 failed"
  failures=1
fi

# should return count of 30
count=`${BOOL} -c 'b' context.txt`
if test $count -ne 30 ; then
  echo "Test #2 failed"
  failures=1
fi

# should return count of 1
count=`${BOOL} -c 'c' context.txt`
if test $count -ne 1 ; then
  echo "Test #3 failed"
  failures=1
fi

# should return count of 15
count=`${BOOL} -c 'd' context.txt`
if test $count -ne 15 ; then
  echo "Test #4 failed"
  failures=1
fi

# should return count of 1
count=`${BOOL} -c 'e' context.txt`
if test $count -ne 1 ; then
  echo "Test #5 failed"
  failures=1
fi

# should return count of 15
count=`${BOOL} -c 'f' context.txt`
if test $count -ne 15 ; then
  echo "Test #6 failed"
  failures=1
fi

# should return count of 1
count=`${BOOL} -c 'g' context.txt`
if test $count -ne 1 ; then
  echo "Test #7 failed"
  failures=1
fi

# should return count of 1
count=`${BOOL} -c 'h' context.txt`
if test $count -ne 1 ; then
  echo "Test #8 failed"
  failures=1
fi

# should return count of 15
count=`${BOOL} -c 'i' context.txt`
if test $count -ne 15 ; then
  echo "Test #9 failed"
  failures=1
fi

# should return count of 1
count=`${BOOL} -c 'j' context.txt`
if test $count -ne 1 ; then
  echo "Test #10 failed"
  failures=1
fi

# should return count of 15
count=`${BOOL} -c 'k' context.txt`
if test $count -ne 15 ; then
  echo "Test #11 failed"
  failures=1
fi

# should return count of 1
count=`${BOOL} -c 'l' context.txt`
if test $count -ne 1 ; then
  echo "Test #12 failed"
  failures=1
fi

# should return count of 30
count=`${BOOL} -c 'm' context.txt`
if test $count -ne 30 ; then
  echo "Test #13 failed"
  failures=1
fi

# should return count of 1
count=`${BOOL} -c 'n' context.txt`
if test $count -ne 1 ; then
  echo "Test #14 failed"
  failures=1
fi

exit $failures
