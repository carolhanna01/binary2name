/*------------------------->  ANSI C - sourcefile  <-------------------------*/
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

#include <string.h>
#include <malloc.h>
#include "foo.h"

int TEST_C_INT_plus( int a, int b );
int TEST_C_INT_times( int a, int b );

int TEST_C_INT_plus( int a, int b )
{
	return a + b;
}

int TEST_C_INT_times( int a, int b )
{
	return a * b;
}

void set_array_chars( char* cptr, int len )
{
	strcpy( cptr, "sather" );
}

void set_array_ints( int* ptr, int len )
{
	int i;
	for ( i = 0;i < len;i++ ) {
		ptr[ i ] = i;
	}
}

extern int callback1( int a, int b );
extern float callback2( float a, float b );
extern float callback3( int* a, int* b );
int test_callback1( int a, int b )
{
	return callback1( a, b );
}

int test_callback2( float a, float b )
{
	return callback2( a, b );
}

int test_callback3( int* a, int* b )
{
	return callback3( a, b );
}

void TEST_C_INT_swap( int *a, int* b )
{
	int tmp;
	tmp = *a;
	*a = *b;
	*b = tmp;
}

C_FOO create_foo()
{
	c_foo = ( C_FOO ) malloc( sizeof( FOO ) );
	return c_foo;
}


typedef struct
{
	int attribute_int;
	float attribute_float;
}
BAR_struct;

typedef BAR_struct* BAR;

float global_bar_float;

BAR create_bar()
{
	BAR bar = ( BAR ) malloc( sizeof( BAR_struct ) );
	bar->attribute_int = 99;
	bar->attribute_float = 99.0;
	return bar;
}

void set_bar( BAR bar, int i, float f )
{
	bar->attribute_int = i;
	bar->attribute_float = f;
}
