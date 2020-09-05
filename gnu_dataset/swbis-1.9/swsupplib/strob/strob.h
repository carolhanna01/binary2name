/* strob.h
 */

/* 
   Copyright (C) 1995,1996,1997,1998  James H. Lowe, Jr.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef strob_19990124_h
#define strob_19990124_h


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* STROB: object and methods for unlimited length string object */

#define STROB_INITLENGTH 32
#define STROB_LENGTHINCR 156
#define STROB_DO_APPEND 1
#define STROB_NO_APPEND 0

typedef struct {
	unsigned char *str_;
	char *tok_;	/* Used by strob_strtok */
	int length_;	/* current length, not including the last NULL */
	int extra_;	/* the lookahead allocation amount */
	int reserve_;	/* the total length of reserved memory */
	char in_use_;
} STROB;


/* STROB INTERNAL FUNCTIONS */
/* static STROB * strob_reopen_if ( size_t reqd_length, STROB * strb ); */


/*-------------------------------------------------------------*/
/*------- STROB  API (PUBLIC FUNCTIONS)------------------------*/

STROB *		strob_open	(size_t initial_size);
char *		strob_release	(STROB * strb);
int 		strob_close	(STROB * strb);
STROB *		strob_reopen	(size_t new_length, STROB * strb);
STROB *		strob_trunc	(STROB * strb);
char *		strob_get_str	(STROB * strb);
int 		strob_get_reserve(STROB * strb);
int 		strob_get_length(STROB * strb);
void 		strob_set_reserve(STROB * strb, int res);
void 		strob_set_length(STROB * strb, int len);
void 		strob_set_memlength(STROB * strb, int len);


/*----*/
STROB *		strob_cpy	(STROB * s, STROB * ct);
STROB *		strob_cat	(STROB * s, STROB * ct);
int 		strob_cmp	(STROB * s, STROB * ct);


char * 		strob_strstrtok(STROB * buf, char *s, const char * delim);
char * 		strob_strtok(STROB * buf, char *s, const char * delim);
char *		strob_chomp	(STROB * strb);
char *		strob_strcat	(STROB * strb, char *str);
char * 		strob_charcat(STROB * strb, int ch);
char * 		strob_strncat(STROB * strb, char *str, size_t len);
char *		strob_strcpy	(STROB * strb, char *str);
char *		strob_strncpy	(STROB * strb, char *str, size_t len);
int 		strob_strcmp	(STROB * strb, char *str);
char *		strob_strchar	(STROB * strb, int index);
char *		strob_strstr	(STROB * strb, char *str);
char *		strob_strrchr	(STROB * strb, int c);
size_t 		strob_strlen	(STROB * strb);
void 		strob_chr	(STROB * strb, int ch);
void 		strob_chr_index	(STROB * strb, int index, int ch);
int 		strob_get_char	(STROB * strb, int index);
char * 		strob_strcpy_at_offset(STROB * strb, int offset, char *str);

void * 		strob_memcpy(STROB *strb, void * ct, size_t n);
void *		strob_memcpy_at(STROB *strb, void * ct, size_t n, size_t offset);
void		strob_append_hidden_null(STROB *strb);
void *		strob_memmove(STROB *strb, void * ct, size_t n);
void *		strob_memmove_to(STROB *strb, size_t dst_offset,
						void * ct, size_t n);
void *		strob_memcat(STROB *strb, void * ct, size_t n);
int 		strob_sprintf(STROB * sb, int do_append, char * format, ...);
int 		strob_snprintf(STROB * sb, int do_append, size_t size, char * format, ...);
int 		strob_vsprintf(STROB * sb, int do_append,
						char * format, va_list ap);
char *		strob_str	(STROB * strb);
int 		strob_length	(STROB * strb);
void *		strob_memset(STROB *strb, int c, size_t n);


/* Depricated names */
char *		strob_catstr	(STROB * strb, char *str);
char *		strob_cpystr	(STROB * strb, int offset, char *str);
int 		strob_setlen	(STROB * strb, int len);

#endif
