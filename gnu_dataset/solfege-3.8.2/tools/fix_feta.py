#!/usr/bin/env python

# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

"""
This script was used to get white background on the pixmaps.
"""

import os
for fn in os.listdir("feta"):
	s = open(os.path.join("feta", fn), "r").read()
	s = s.replace("\"a c #FFF\"", "\"a c #FF\"")
	f = open(os.path.join("feta", fn), "w")
	f.write(s)
	f.close()
