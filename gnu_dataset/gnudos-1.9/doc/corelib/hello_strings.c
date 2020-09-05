/* 
 *    Programmed By: Mohammed Isam Mohammed [mohammed_isam1984@yahoo.com]
 *    Copyright 2014 (c)
 * 
 *    file: hello_strings.c
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
#include <stdio.h>
#include "console/strings.h"

int main(int argc, char **argv) 
{
    printf("Hello World");
    str s;
    s = "Hello world";
    printf("\n%s", s);
    printf("\n%d", indexof(s, 'H'));
    printf("\n%d", nindexof('H'));
    printf("\n%d", lindexof(s, 'H'));
    printf("\n%s", substr(s, 4));
    printf("\n%s", nsubstr(s, 4, 5));
    return 0;
}
