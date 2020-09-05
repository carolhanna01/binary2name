#!/usr/bin/python

import os
import sys
sys.path.insert(0, ".")
import src.cfg
import src.gethomedir
src.cfg.initialise("default.config", None,
    os.path.join(src.gethomedir.get_home_dir(), ".solfegerc"))
import src.i18n
src.i18n.setup(".")
import src.rhythm

img_str = """%i:<inlinemediaobject>
      <imageobject>
        <imagedata fileref="../../graphics/rhythm-%s.png" format="PNG"/>
      </imageobject>
      <textobject>
       <phrase>%s</phrase>
      </textobject>
    </inlinemediaobject>"""
f = open("help/C/rhythmtable.xml", "w")
print >> f, "<para>"
for i, r in enumerate(src.abstract.RhythmAddOnClass.RHYTHMS):
    print >> f, img_str % (i, r.replace(" ", ""), r),
    if i != len(src.abstract.RhythmAddOnClass.RHYTHMS) - 1:
        print >> f, ", "
print >> f, "</para>"
f.close()
