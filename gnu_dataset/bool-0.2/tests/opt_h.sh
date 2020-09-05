#! /bin/sh
# 
# option -h
# option -H

: ${srcdir=.}

failures=0

# should not return filename
file=`${BOOL} 'a' single.txt | ${AWK} -F: '{print $1}'`
if test "$file" = "single.txt" ; then
  echo "Test #1 failed"
  failures=1
fi

# should return filename
file=`${BOOL} 'a' single.txt empty.txt | ${AWK} -F: '{print $1}'`
if test "$file" != "single.txt" ; then
  echo "Test #2 failed"
  failures=1
fi

# should not return filename
file=`${BOOL} -h 'a' single.txt | ${AWK} -F: '{print $1}'`
if test "$file" = "single.txt" ; then
  echo "Test #3 failed"
  failures=1
fi

# should not return filename
file=`${BOOL} -h 'a' single.txt empty.txt | ${AWK} -F: '{print $1}'`
if test "$file" = "single.txt" ; then
  echo "Test #4 failed"
  failures=1
fi

# should return filename
file=`${BOOL} -H 'a' single.txt | ${AWK} -F: '{print $1}'`
if test "$file" != "single.txt" ; then
  echo "Test #5 failed"
  failures=1
fi

# should return filename
file=`${BOOL} -H 'a' single.txt empty.txt | ${AWK} -F: '{print $1}'`
if test "$file" != "single.txt" ; then
  echo "Test #6 failed"
  failures=1
fi

exit $failures
