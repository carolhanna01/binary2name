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

beware:		
	@echo "-----------------------------------------------------------"; 
	@echo "Please, use one of the following targets:"; 
	@echo "sparc-solaris, i386-solaris, axp-osf, hppa-hpux, mips-irix5"; 
	@echo "i386-linux, i386-win32";
	@echo "-----------------------------------------------------------"; 
	@exit 1; 

sparc-solaris:		
	$(MAKE) all ARCH=sparc-solaris

i386-solaris:	
	$(MAKE) all ARCH=i386-solaris

i386-linux:	
	$(MAKE) all ARCH=i386-linux

i386-win32:	
	$(MAKE) all ARCH=i386-win32

hppa-hpux:		
	$(MAKE) all ARCH=hppa-hpux

axp-osf:
	$(MAKE) all ARCH=axp-osf

mips-irix5:
	$(MAKE) all ARCH=mips-irix5

all:		
	$(MAKE) -C src $(ARCH)

tests: all
	$(MAKE) -C tests ARCH=$(ARCH)		

clean:
	rm -f src/*.o 
	rm -f md/*/*.o
	rm -f schedulers/*.o
	rm -rf lib
	$(MAKE) -C tests clean
