#------------------------------->  Makefile  <--------------------------------#
#- Copyright (C) 199x by International Computer Science Institute            -#
#- This file is part of the GNU Sather package. It is free software; you may -#
#- redistribute  and/or modify it under the terms of the  GNU General Public -#
#- License (GPL)  as  published  by the  Free  Software  Foundation;  either -#
#- version 3 of the license, or (at your option) any later version.          -#
#- This  program  is distributed  in the  hope that it will  be  useful, but -#
#- WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY -#
#- or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        -#
#- The license text is also available from:  Free Software Foundation, Inc., -#
#- 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     -#
#------------->  Please email comments to <bug-sather@gnu.org>  <-------------#

# The makefile checks if the file /u/sather/lib/libmeikoam.a exists.
# If so, it uses this one instead of recompiling the am library
# This is used at the ICSI for cross compiling.

all: libam.a

libam.a:	../../pSather/am/*.[ch]
	@ echo Making am library for platform meiko;		\
	if [ -f /u/sather/lib/libmeikoam.a ] ; then		\
		cp /u/sather/lib/libmeikoam.a libam.a;		\
		cp ../../pSather/am/am.h .;			\
	else							\
	   (cd ../../pSather/am; touch Makefile; 		\
		HEADERDIR=../../Platforms/meiko 		\
		LIBDIR=../../Platforms/meiko 			\
		PCFLAGS="-Wall -DMEIKO -DAM_THREADS -DSOLARIS_THREADS -D_REENTRANT" \
		CFLAGS="-g -O2  -I/opt/meiko/include"						\
		PLIBS="-L/opt/meiko/lib -L/u/sather/lib -lposix4 -lthread2.3 -lew -lelan -ldl"	\
		$(MAKE) install)				\
	fi

clean:
	@echo Cleaning System/pSather/am...
	@(cd ../../pSather/am; $(MAKE) clean)
	@-$(RM) libam.a
	@-$(RM) am.h

test:
