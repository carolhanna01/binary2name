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

#
# Some install variables
#

include ../../Make.config
include ../../pSather.options
HEADERDIR=../../include
LIBDIR=../../lib
PSATHERLIB=$(LIBDIR)/libpSather.a
CFLAGS=$(PCFLAGS) $(PSATHER) -I$(HEADERDIR) 
LFLAGS=-L$(LIBDIR) -lpSather -lam $(PLIBS)

sort:	sort.o mktable.o sort.h $(PSATHERLIB)
	gcc -o sort -g sort.o mktable.o $(LFLAGS)

$(PSATHERLIB):	../../pSather.options
		cd ../../pSather ; $(MAKE) install ; cd ../test/sort

mktable.o:	mktable.c sort.h
sort.o:		sort.c sort.h

clean:
	@-rm *.o 2> /dev/null

.c.o:;
	gcc $(CFLAGS) -c $*.c

