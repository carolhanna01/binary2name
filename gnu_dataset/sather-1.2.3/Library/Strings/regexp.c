/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#include <stdio.h>
#define STDC_HEADERS 1
#define HAVE_STRING_H 1
#define HAVE_ALLOCA_H 1
#include "regex.h"
#include "regex.c"

void* C_REGEXP_compile( void* string, void* nocase )
{
	int res, flags;
	regex_t *r;
	/*  re_syntax_options = RE_SYNTAX_EGREP; */
	r = ( regex_t* ) malloc( sizeof( regex_t ) );
	flags = REG_EXTENDED | ( ( ( int ) nocase != 0 ) ? REG_ICASE : 0 );
	res = regcomp( r, ( char* ) string, flags );
	if ( !res )
		return ( void* ) r;
	free( r );
	return ( void* ) 0;
}

void C_REGEXP_match( void* regexpr, void* string, void* beg, void* end )
{
	int res;
	regmatch_t match;
	res = regexec( ( regex_t* ) regexpr, ( char* ) string,
	               1, &match, 0 );
	if ( res ) {
		*( int* ) beg = -1;
		*( int* ) end = -1;
	} else {
		*( int* ) beg = ( int ) ( match.rm_so );
		*( int* ) end = ( int ) ( match.rm_eo );
	}
}

void C_REGEXP_free( void* regexpr )
{
	regfree( ( regex_t* ) regexpr );
}


