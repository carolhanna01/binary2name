#! /bin/bash

# the next program must exit with error

if ./test_multiple -s "foo" -s "bar" -s "hello" -i 100 -i 200 -s "world" ; then true; else false; fi
