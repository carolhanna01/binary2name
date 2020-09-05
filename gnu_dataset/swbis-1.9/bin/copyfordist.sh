#!/bin/bash

distdir="$1"

if [ ! -d "$distdir" ]; then
	exit 1
fi

cat catalog/dfiles/files |
sed -e 's@^[^/]*/@@' |
grep -v -e '^catalog/' -e '^$' |
cat |
tar cf - --files-from=- --no-recursion |
(cd $distdir && tar xpf -)


