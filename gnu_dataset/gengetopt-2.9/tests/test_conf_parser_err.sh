#! /bin/bash

# the next program must exit with error

if ./test_conf_parser -r "bar" -i 100 -c ./test_conf_err.conf; then false; else true; fi
