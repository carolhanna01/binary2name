#!/usr/bin/python

# Warning: This setup.py script is *only* used to create the MS Windows
# installer. It will break on a real OS.

import glob
from distutils.core import setup
import py2exe
import src.configureoutput
import sys, os

def get_files(file_spec):
    path = os.path.normpath(file_spec)
    ret = filter(lambda x: not x.endswith('CVS'), glob.glob(path))
    ret = filter(os.path.isfile, ret)
    return ret

def mo_files():
    v = []
    for dir in glob.glob("share/locale/*/LC_MESSAGES"):
        v.append((dir, glob.glob(os.path.join(dir, '*'))))
    return v

def help_files():
    v = []
    for dir in glob.glob("help/*"):
        if os.path.isdir(dir):
            v.append((dir, glob.glob(os.path.join(dir, '*.html'))))
    for dir in glob.glob("help/*/figures"):
        if os.path.isdir(dir):
            v.append((dir, glob.glob(os.path.join(dir, '*.png'))))
    for dir in glob.glob("help/*/ly"):
        if os.path.isdir(dir):
            v.append((dir, glob.glob(os.path.join(dir, '*.png'))))
    return v

def scale_files():
    v = []
    v.append(('help/C/scales', glob.glob('help/C/scales/*.html')))
    for dir in ('help/C/scales/images/quadriads',
        'help/C/scales/images/triads',
        'help/C/scales/images/modes/chords',
        'help/C/scales/images/modes/scales'):
        v.append((dir, glob.glob(os.path.join(dir, '*.png'))))
    return v

setup(name="solfege",
      version=src.configureoutput.VERSION_STRING,
      author="Tom Cato Amundsen",
      author_email="tca@gnu.org",
      url="http://www.solfege.org",
      data_files=[('graphics', get_files('graphics/*.png')),
                  ('soundcard', ['soundcard/winmidi.pyd']),
                  ('feta', get_files('feta/*.xpm')),]
                  + help_files()
                  + mo_files()
                  + scale_files()
                  + [
                  ('lesson-files', get_files('lesson-files/*')),
		  ('lesson-files/share', get_files('lesson-files/share/*')),
		  ('lesson-files/include', get_files('lesson-files/include/*')),
                  ('.', ['solfege.py', 'solfegedebug.py', 'default.config', 'solfege.gtkrc',
                         'AUTHORS.txt', 'COPYING.txt', 'README.txt',
                         'INSTALL.txt', 'INSTALL.win32.txt', 'FAQ.txt',
                         'solfege.bat', 'ui.xml', 'helpbrowser.xml',
                         'help-menu.xml',
                         'learningtree.txt'])
      ],
      packages=['mpd', 'src', 'soundcard'],
#      scripts=["solfege.py", "solfegedebug.py"],
      windows=["solfege.py"],
      console=["solfegedebug.py"],
)
