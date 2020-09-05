/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather package. It is free software; you may */
/* redistribute  and/or modify it under the terms of the  GNU General Public */
/* License (GPL)  as  published  by the  Free  Software  Foundation;  either */
/* version 3 of the license, or (at your option) any later version.          */
/* This  program  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.        */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#ifndef _FOO_H_
#define _FOO_H_

typedef struct
{
	int attribute_a;
	float attribute_b;
	char attribute_c;
}
FOO;

typedef FOO* C_FOO;

C_FOO c_foo;
int c_foo_int;
#endif _FOO_H_
