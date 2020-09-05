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

CC=gcc
CFLAGS=`edma-config --cflags-exe`
LIBS=`edma-config --libs-exe` -g -Wall

all: shallow_cloning
shallow_cloning: shallow_cloning.c
	$(CC) $(CFLAGS) $< -o $@ $(LIBS)
configure:
	echo "No configuration is necessary."
clean:
	rm -f core *.o *~ shallow_cloning
