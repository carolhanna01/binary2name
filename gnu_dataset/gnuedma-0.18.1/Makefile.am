#   EDMA: Entorno de Desarrollo Modular y Abierto
#   Object Oriented and Componentware Framework  
#
#   Copyright (C) 1998, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
#             by David Martínez Oliveira
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

SUBDIRS =src classes tests tools doc debian


bin_SCRIPTS = edma-config edma-env.sh

EXTRA_DIST=config edma32.cfg sysdat32.cfg edma-config\
	edma-config.in edma-env.sh.in edma_test.cfg edma_examples.cfg \
	include/edma.h include/portable.h FAQ BUGS version.m4 \
	install-components.sh class_list.lst COPYING.LESSER \
	class_list.lst class_ex_list.lst class_test_list.lst


configdir = ${prefix}/etc
config_DATA= edma32.cfg sysdat32.cfg

install-data-hook:
	echo "Registering EDMA Foundation Classes"
	chmod +x install-components.sh
	./install-components.sh

## Deleted these files when make maintainer-clean
MAINTAINERCLEANFILES =  Makefile.in aclocal.m4 configure config.cache
 
maintainer-clean-local:
	rm -fr $(ac_aux_dir)
	rm -f @PACKAGE@-*.tar.gz
