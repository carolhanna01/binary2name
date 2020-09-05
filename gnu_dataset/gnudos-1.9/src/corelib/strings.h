/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: strings.h
 *    This file is part of the GnuDOS project.
 *
 *    GnuDOS is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    GnuDOS is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with GnuDOS.  If not, see <http://www.gnu.org/licenses/>.
 */    
#ifndef __STRINGS_H
#define __STRINGS_H

#include <string.h>

/* define the String type */
typedef char *str;

/* Get the index of a char in a string */
int indexof(str string, char chr);
/* Get the next index of a char in a string */
int nindexof(char chr);
/* Get the last index of a char in a string */
int lindexof(str string, char chr);

/* Get the substring starting at .. */
str substr(str string, int start);
/* Get the substring starting at .. with a length of .. */
str nsubstr(str string, int start, int length);
/* Trim the lefthand whitespace chars */
str ltrim(str string);
/* Trim the righthand whitespace chars */
str rtrim(str string);
/* Trim the whitespace chars from both sides */
str trim(str string);

/* Convert string to uppercase letters */
str uppercase(str string);
/* Convert string to lowercase letters */
str lowercase(str string);

#endif /* __STRINGS_H */