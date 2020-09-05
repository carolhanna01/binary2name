#! /bin/bash

# the next program must exit with error

if ./test_conf_parser -r "bar" -i 100 -c ./test_conf.conf; then true; else false; fi
