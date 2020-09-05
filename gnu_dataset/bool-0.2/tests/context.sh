#! /bin/sh
#
# single character input

: ${srcdir=.}

failures=0
line="a b ccccccccccccccccccccccccccccccccccccccc d e";

# |*----|
match=`echo $line | ${BOOL} "a"`
if test "$match" != "$line" ; then
  echo "Test #1 failed"
  failures=1
fi

# |----*|
match=`echo $line | ${BOOL} "e"`
if test "$match" != "$line" ; then
  echo "Test #2 failed"
  failures=1
fi

# |*----|
# |-*---|
match=`echo $line | ${BOOL} "a near b"`
if test "$match" != "$line" ; then
  echo "Test #3 failed"
  failures=1
fi

# |---*-|
# |----*|
match=`echo $line | ${BOOL} "d near e"`
if test "$match" != "$line" ; then
  echo "Test #4 failed"
  failures=1
fi

# |--*--|
#   |--*--|
line="bbbbbbbbbbbbbbbbbbb c ddddddddddddddd e fffffffffffffff g h"
match=`${BOOL} "c near e" context.txt`
if test "$match" != "$line" ; then
  echo "Test #5 failed"
  failures=1
fi

# |--*--|
#     |--*--|
line="ddddddddd e fffffffffffffff g h iiiiiiiiiiiiiii j kkkkkkkkkk"
match=`${BOOL} "e near j" context.txt`
if test "$match" != "$line" ; then
  echo "Test #6 failed"
  failures=1
fi

# |*----|
#     |--*--|
line="a bbbbbbbbbbbbbbbbbbbbbbbbbb... iiiiiiiiiiii j kkkkkkkkkkkkk"
match=`${BOOL} "a near j" context.txt`
if test "$match" != "$line" ; then
  echo "Test #7 failed"
  failures=1
fi

# |--*--|
#     |----*|
line="dddddddddddd e fffffffffffff... mmmmmmmmmmmmmmmmmmmmmmmmmm n"
match=`${BOOL} "e near n" context.txt`
if test "$match" != "$line" ; then
  echo "Test #8 failed"
  failures=1
fi

# |--*--|
#      |--*--|
line="bbbbbbbbbbbb c ddddddddddddd... kkkkkkkkkkkk l mmmmmmmmmmmmm"
match=`${BOOL} "c near l" context.txt`
if test "$match" != "$line" ; then
  echo "Test #9 failed"
  failures=1
fi

exit $failures
