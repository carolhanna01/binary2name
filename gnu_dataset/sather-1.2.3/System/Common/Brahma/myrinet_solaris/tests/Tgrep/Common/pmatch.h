/*
 * PMATCH.H - manifests and typedefs for use which pmatch
 *
 * pattern allows certian metachars:
 *		. - match any character
 *		* - match 0 or more occurrences of pervious char
 *		+ - match 1 or more occurrences of pervious char.
 *		^ - match at begining of string
 *		$ - match end of string
 *		[ - start of character class
 *		] - end of character class
 *		( - start of a new pattern
 *		) - end of a new pattern
 *		@(n)c - match <c> at column <n>
 *		| - match either pattern
 *		\ - escape any special characters
 *	   	\c - escape any special characters
 *		\o - turn on any special characters
 *
 * witten: Marc Staveley, Aug/82
 * last modified: Marc Staveley, May/84
 */

#ifndef PMATCH_DEFINED
#define PMATCH_DEFINED

/* #include "mydefs.h" */
/* #include <types.h> */

#include "brahma.h"

typedef struct _pattern_ {
	u_int	p_type;			/* the type of metacharacter */
	char	p_c;			/* the character to match */
	char	*p_ccl;			/* the class of characters to match */
	int	p_col;			/* character must be at column col */
	struct _pattern_ *p_next;	/* ^ to next element in pattern */
	struct _pattern_ *p_alt;	/* ^ to alternate element in pattern */
} PATTERN;

extern	char	*pmatch();
extern	PATTERN	*makepat();
extern	void	freepat();

#endif /* PMATCH_DEFINED */
