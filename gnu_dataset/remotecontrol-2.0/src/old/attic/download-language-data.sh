#!/bin/bash

cd "`dirname $0`"

cd ../dat

curl http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry > lang.dat

