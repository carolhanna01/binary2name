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

def update (a, b):
	print "----------------------------------------------------------";
	print "-- Pyhton Update Script-----------------------------------";
	print "-- HELLO_WORLD Class Version 2.0";
	print "-- Updating object ", a, "->", b;
	print "----------------------------------------------------------";
	num = edma.get_sint32 (b, "num");
	print "- Old Number value is ", num;
	num1 = num / 2;
	print "- New Number value is ", num1
	edma.set_sint32 (a, "num1", num1);
	edma.set_sint32 (a, "num2", num1);
	print "- Attaching '(Version 2.0)' to string property"
	old_str = edma.get_strz (b, "str");
	print "Old String ", old_str
	if old_str :
		old_str += " (Version 2.0)";
	else: 
		old_str ="NULL STRING";
	edma.set_strz (a, "str_new", old_str);
	print "---[Object Updated] --------------------------------------";

