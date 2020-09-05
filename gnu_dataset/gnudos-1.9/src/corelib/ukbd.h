/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: ukbd.h
 *    This file is part of the GnuDOS project.
 *
 *    The GnuDOS project is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    The GnuDOS project is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with the GnuDOS project.  If not, see <http://www.gnu.org/licenses/>.
 */    
#ifndef __U_KBD_H
#define __U_KBD_H

#include "kbd.h"
/* mask values for bit pattern of first byte in multi-byte
     UTF-8 sequences: 
       192 - 110xxxxx - for U+0080 to U+07FF 
       224 - 1110xxxx - for U+0800 to U+FFFF 
       240 - 11110xxx - for U+010000 to U+1FFFFF */
extern unsigned short mask[];// = {192, 224, 240};

#ifndef __BOOL_DEFINED_
#define __BOOL_DEFINED_
typedef int bool;
#endif

char *ugetKey();
char *ugetKeyUnderConsole();
char *ugetKeyUnderX();

//the Unicode character returned by ugetKey()
char uc[5];

#endif /* __U_KBD_H */
