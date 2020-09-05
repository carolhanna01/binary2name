#!/bin/bash

cd `dirname $0`/..

phpuml -o doc/uml/bouml/ -f htmlnew src web
