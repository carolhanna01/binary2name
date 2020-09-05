#!/usr/bin/python
# vim: expandtab:

# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

import sys, shutil, os.path
todir = sys.argv[-1]
del sys.argv[-1]
for fn in sys.argv[1:]:
    head, tail = os.path.split(fn)
    if os.path.isdir(fn):
        continue
    if head:
        if not os.path.exists(os.path.join(todir, head)):
            os.makedirs(os.path.join(todir, head))
    shutil.copy(fn, os.path.join(todir, fn))
