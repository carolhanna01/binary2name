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

AS     =gcc
CC     =gcc
CPP    =/lib/cpp
# -Wa,-xarch=v8plusa must be used to generate code for access to 
# UltraSPARC performance counters
#CFLAGS =-I. -I../../include -g -O -Wa,-xarch=v8plusa
CFLAGS =-I. -I../../include -g -O

qtmds.o: qtmds.s
	$(AS) -c qtmds.s

lwp.o: lwp.h lwp.c

hwperf.o: hwperf_src.s
	$(CPP) hwperf_src.s > hwperf.s
	gcc -c -Wa,-xarch=v8plusa hwperf.s -o hwperf.o

clean:
	rm *.o
