#! /bin/bash

# the next program must exit with error

if ./test_all_opts -r "foo"; then true; else false; fi
