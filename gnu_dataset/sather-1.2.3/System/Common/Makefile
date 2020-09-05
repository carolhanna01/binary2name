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

# Build the standard things used in all platforms

#GC_CFLAGS=-O -DSILENT -DNO_SIGNALS

# Changes needed for Alpha
#CPP= /usr/ucb/cpp -C -P -DALPHA
#GC_CC=cc 

LIBRARY=../../Library
PLIBRARY=../../pLibrary

all:	CONFIG 

TMPFILE=CONFIG.tmp.c

CONFIG: Makefile ../../Makefile CONFIG.proto $(LIBRARY)/Library.config $(LIBRARY)/Base/*.config $(LIBRARY)/Strings/*.config $(LIBRARY)/Containers/*.config $(LIBRARY)/Ext/*.config $(LIBRARY)/IO/*.config $(LIBRARY)/System/*.config Makefile $(PLIBRARY)/pSather/*.config
	@echo Creating Common/CONFIG...
	@echo "-- This file has been created by using cpp on CONFIG.proto" > $(TMPFILE)
	@echo "-- in this directory and by using many variables defined " >> $(TMPFILE)
	@echo "-- in the top level Makefile. Please do not edit it" >> $(TMPFILE)
	@echo "#define CONFIG_DEFAULT_PLATFORM \"$(DEFAULT_PLATFORM)\"" >> $(TMPFILE)
	@echo -n "#define CONFIG_PLATFORMS \"" >> $(TMPFILE)
	@echo -n                           $(PLATFORMS) | sed "s/ //;s/,/\",\"/" >> $(TMPFILE)
	@echo                                                                    "\"" >> $(TMPFILE)
	@echo "#include \"CONFIG.proto\"" >> $(TMPFILE)
	@$(CPP) -undef $(TMPFILE) > CONFIG
	@$(RM) $(TMPFILE)

# The following did not work, because of predefined macros. Unfortunately, I could not find a way to 
# remove predefined macros while still keep the macros defined at the command line
#	@$(CPP) -Uunix -DCONFIG_DEFAULT_PLATFORM='$(DEFAULT_PLATFORM)' -DCONFIG_PLATFORMS='$(PLATFORMS)' CONFIG.proto >> CONFIG

clean:
	$(RM) CONFIG $(TMPFILE)
	cd ActiveThreads; $(MAKE) clean
	cd Brahma; $(MAKE) clean
	cd Siva; $(MAKE) clean
