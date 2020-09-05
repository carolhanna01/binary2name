# Do not edit. -*- buffer-read-only: t; -*- vi: set ro:
# This file is created automatically from modules/stub.am.
# To recreate, run bootstrap from the top of Dico source tree.
# This file is part of GNU Dico
# Copyright (C) 1998-2019 Sergey Poznyakoff
#
# GNU Dico is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Dico is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Dico.  If not, see <http://www.gnu.org/licenses/>.

if GUILE_COND
  GUILE_DIR=guile
endif

if LDAP_COND
  LDAP_DIR=ldap
endif

if PAM_COND
  PAM_DIR=pam
endif

if PCRE_COND
  PCRE_DIR=pcre
endif

if PYTHON_COND
  PYTHON_DIR=python
endif

if WORDNET_COND
  WORDNET_DIR=wordnet
endif

SUBDIRS=\
 dict.org\
 gcide\
 $(GUILE_DIR)\
 $(LDAP_DIR)\
 metaphone2\
 nprefix\
 outline\
 $(PAM_DIR)\
 $(PCRE_DIR)\
 $(PYTHON_DIR)\
 stratall\
 substr\
 word\
 $(WORDNET_DIR)

EXTRA_DIST = template.py template.scm stub.am template/README template/Makefile.am template/module.c template/tests/testsuite.at template/tests/atlocal.in template/tests/Makefile.am
