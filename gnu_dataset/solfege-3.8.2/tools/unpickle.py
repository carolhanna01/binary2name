#!/usr/bin/python

import pickle
import pprint
import sys

print sys.argv
f = file(sys.argv[1], 'r')
d = pickle.load(f)
f.close()
pprint.pprint(d)
