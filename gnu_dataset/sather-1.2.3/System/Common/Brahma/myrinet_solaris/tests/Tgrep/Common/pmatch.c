/*
 * PMATCH - tell if a patern extists in a string
 *
 * Usage: #include <pmatch.h>
 *
 *	char	*pmatch( char *pattern, char *string, int len );
 *
 * pattern allows certian metachars:
 *			. - match any character
 *			* - match 0 or more occurrences of pervious char
 *			+ - match 1 or more occurrences of pervious char.
 *			^ - match at begining of string
 *			$ - match end of string
 *			[ - start of character class
 *			] - end of character class
 *			( - start of a new pattern
 *			) - end of a new pattern
 *		@(n)c - match <c> at column <n>
 *			| - match either pattern
 *			\ - escape any special characters
 *		   \c - escape any special characters
 *		   \o - turn on any special characters
 *
 * witten: Marc Staveley, Aug/82
 * converted from 'B' to 'C': Marc Staveley, Apr/84.
 * last modified: Marc Staveley, May/84
 * Last modified: Richard Marejka, Jan/94
 *
 * TABS=4
 */

/* Copyright (c) 1982-1993, 1994  Marc Staveley				*/
/* This program may be used, copied, modified, and redistributed freely */
/* for ANY purpose, so long as this notice remains intact. 		*/

/*
 * Include Files
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "brahma.h"
/*
 * Constants & Macros
 */

#if !defined(isodigit)
#	define	isodigit(c)	(strchr("01234567",(c)))
#endif

#define CCL_SIZE	128	/* maximum number of chars in a class */
#define ESC_CHR		'\\'

/*
 * Note: The order defined here must match exactly with the _metachars string.
 */

#define M_END			0x0001
#define M_BEGINNING		0x0002
#define M_ANY			0x0004
#define M_CLOSURE 		0x0008
#define M_CLOSURE1		0x0010
#define M_CCL			0x0020
#define M_RE			0x0040
#define M_OR			0x0080
#define M_COL			0x0100
#define M_CASELESS		0x0200
				/* the next two types must always be last */
#define M_CHAR			0x0400
#define M_NCCL			0x0800

/*
 * Data Declarations
 */

typedef struct _pattern_ {
	struct _pattern_	*next;	/* ^ to next element in pattern */
	unsigned int	type;		/* the type of metacharacter */
	char		*ccl;		/* the class of characters to match */
	int		col;		/* character must be at column col to match */
	struct _pattern_	*alt;	/* ^ to alternate element in pattern */
	char		c;		/* the character to match */
} PATTERN;

/*
 * External Declarations
 */

static	char		*_metachars	= "$^.*+[(|@d";
#if 0
static	char		*start		= NULL;
#endif
static	unsigned int	all_metas	= M_END
					| M_BEGINNING
					| M_ANY
					| M_CLOSURE
					| M_CLOSURE1
					| M_CCL
					| M_RE
					| M_OR
					| M_COL
					| M_CASELESS;


/*
 * External References
 */

static	char    *match( char *, PATTERN *, char * );
extern	void     freepat( PATTERN * );

#define warning	printf	/* [RW] Jan/94 */
/* extern	void	 warning( char *, ... ); */

/*
 * PMATCH - match a pattern against a string.
 *
 */

char *
pmatch( register PATTERN *pattern, register char *string, int *len ) 
{
    char		*end;
    char		*start;

    start = string;		/* an anchor for ^ and @(nn) */

    for ( ; *string; ++string )
	if ( end = match( string, pattern, start ) ) {
	    if ( len )
		*len	= end - string;
	    return( string );
	}
    return(NULL);
}

/*
 * MATCH - decide if 'pat' is valid for 'string'
 *
 * Usage: char	*match( char *, PATTERN *, char * )
 */

static char *
match(register char *s, register PATTERN *pat, register char *start )
{
    char		*startclosure, *end;

    for ( ; pat; pat = pat->next ) {
	switch ( pat->type ) {
	case M_CHAR		:	/* match a character */
	    if ( pat->c == *s++ )
		continue;
	    break;

	case M_BEGINNING	:	/* match the begining of line */
	    if ( s == start )
		continue;
	    break;

	case M_END		:	/* match end of line	*/
	    if ( *s == '\0' || *s == '\n' )
		continue;
	    break;

	case M_ANY		:	/* match any chracter */
	    if ( *s != '\0' && *s != '\n') {
		++s;
		continue;
	    }
	    break;
#if !defined(PM_DEBUG)
	case M_CLOSURE	:	/* this is the bitch */
	case M_CLOSURE1	:
	    startclosure	= ( pat->type == M_CLOSURE1 )? s + 1 : s;
/*
 * eat up characters
 */
	    for ( ; *s && ( end = match( s, pat->alt, start ) ); s=end )
		;
/*
 * try to match the rest of the line
 */
	    for ( ; s >= startclosure; s-- )
		if ( end = match( s, pat->next, start ) )
		    return( end );

	    break;

	case M_RE		:
	    if ( s = match( s, pat->alt, start ) )
		continue;
	    break;

	case M_OR		:
	    if ( ( end = match( s, pat->next, start ) ) ||
		 ( end = match( s, pat->alt, start ) ) )
		return( end );
	    break;

	case M_CASELESS	:	/* match a case-less character */
	    if ( pat->c == tolower( *s ) ) {
		++s;
		continue;
	    }
	    break;

	case M_CCL		:	/* match a class of characters */
	    if ( strchr( pat->ccl, *s++ ) )
		continue;
	    break;

	case M_NCCL		:	/* NOT character class */
	    if (*s != '\0' && !strchr( pat->ccl, *s ) ) {
		++s;
		continue;
	    }
	    break;

	case M_COL		:	/* see if at this column */
	    if ( pat->col == s - start + 1 )
		continue;
	    break;
#endif	/* !defined(PM_DEBUG)	*/
	default		:
	    warning( "match: can't happen\n" );
	}
	return( NULL );

    }
    return( s );
}

/*
 * MAKEPAT - put in special characters for pmatch()
 *
 */

#define IF_META(m) if( !(on_metas & (m)) ) goto normal

PATTERN *
makepat( char *string, char *metas )
{
    PATTERN	*pattern, *oldpat;
    register PATTERN	*pat;
    char		*temp, *ccl;
    register char		*s;
    unsigned int	 orig_metas, on_metas, paren;
    int		 i;

    if ( !string || !*string )
	return( NULL );

    if ( metas != NULL ) {
	register char	*sp;
#ifdef PM_DEBUG
	warning( "makepat: which metas to use??\n" );
#endif
	for ( orig_metas=0; *metas; ++metas ) {
	    if ( !( sp = strchr( _metachars, *metas ) ) )
		return( NULL );

	    orig_metas	|= ( 1 << ( sp - _metachars ) );
	}
    }
    else
	orig_metas	= all_metas;

    on_metas	= orig_metas;
    s		= string;
    pattern		= ( PATTERN * ) at_calloc( 1, sizeof( PATTERN ) );
    oldpat		= pat	= pattern;
    temp		= (char *) at_malloc( CCL_SIZE * sizeof( char ) );

    for ( ; *s; s++ ) {
	switch ( *s ) {
	case ESC_CHR	:
	    ++s;
	    if ( tolower( *s ) == 'c' ) {	/* take next char
						 * literally */
		pat->type	= M_CHAR;
		pat->c		= *++s;
	    }
	    else if ( tolower( *s ) == 'o' ) {
                /* 
		 * Turn on meta meaning of next char
		 */
		register char	*sp;

		if ( !( sp = strchr( _metachars, *++s ) ) )
		    goto fail;

		on_metas	|= ( 1 << ( sp - _metachars ) );
		continue;
	    }
	    else if ( isodigit( s[0] ) &&
		      isodigit( s[1] ) && isodigit( s[2] ) ) {
		pat->type	= M_CHAR;
		pat->c		= (char) strtol( s, NULL, 8 );
		s		+= 2;
	    }
	    else {	/* take this char literally */
		pat->type	= M_CHAR;
		pat->c		= *s;
	    }
	    break;

	case '$'		:	/* End Of String */
	    IF_META(M_END);
	    if ( s[1] != '\0' && s[1] != '|' && s[1] != ')' && s[1] != '\n' )
		goto fail;

	    pat->type	= M_END;
	    break;

	case '^'		:	/* Beginning Of String */
	    IF_META(M_BEGINNING);
	    if ( s != string && s[-1] != '|' && s[-1] != '(' )
		goto fail;

	    pat->type	= M_BEGINNING;
	    break;

	case '.'		:	/* match any char */
	    IF_META( M_ANY );
	    pat->type	= M_ANY;
	    break;

	case '*'		:	/* zero or more */
	    IF_META( M_CLOSURE );
	    pat->type	= M_CLOSURE;
	    goto closure;

	case '+'		:	/* one or more */
	    IF_META( M_CLOSURE1 );
	    pat->type	= M_CLOSURE1;
	closure:
	    if ( pat == pattern || oldpat->type & ( M_CLOSURE | M_CLOSURE1 ) )
		goto fail;

	    pat->alt	= pat;	/* pay attention now */
	{
	    PATTERN	tmp	= *pat;
	    *pat	= *oldpat;
	    *oldpat	= tmp;
	}
	    pat->next	= NULL;
	    pat		= oldpat;
	    break;

	case '['		:	/* character class */
	    IF_META( M_CCL );

	    if ( s[1] == '^' ) {
		pat->type	= M_NCCL;
		++s;
	    }
	    else
		pat->type	= M_CCL;

	    ccl	= pat->ccl	= (char *) at_malloc( CCL_SIZE * sizeof( char ) );

	    while(1) {
		if (*++s == ESC_CHR) {	/* take next character
					 * literally */
		    ++s;
		    if (tolower(*s) == 'c')	/* SHOULD WE CHECK FOR
						 * OCTAL HERE ?? */
			*ccl++ = *++s;
		    else
			*ccl++ = *s;
		}
		else if (*s == '-') {	/* ranges */
		    if (ccl == pat->ccl || s[1] == ']')	/*not at beg. or end */
			*ccl++ = *s;
		    else {
			if (*--ccl >= *++s)
			    goto fail;
			for (; *ccl < *s; ++ccl)
			    ccl[1] = *ccl + 1;
			++ccl;
		    }
		}
		else if ( *s == ']') {	/* at the end */
		    *ccl	= '\0';
		    break;
		}
		else if ( *s == '\0' )
		    goto fail;
		else
		    *ccl++	= *s;
	    }
	    break;

	case '('		:	/* another pattern */
	    IF_META( M_RE	);

	    i	= -1;
	    paren	= 1;

	    while(1){
		if ( *++s == ESC_CHR ) {
		    temp[++i]	= *s++;

		    if ( tolower( *s ) == 'c' )
			temp[++i]	= *s++;
		}
		else if ( *s == '(' )
		    paren++;

		else if ( *s == ')' && !--paren )
		    break;

		else if ( *s == '\0' )
		    goto fail;

		temp[++i]	= *s;
	    }
	    temp[++i]	= '\0';
	    pat->alt	= makepat( temp, metas );
	    pat->type	= M_RE;
	    break;

	case '|'		:	/* either of two */
	    IF_META( M_OR );

	    if ( *(++s) ) {
		if ( !( pat->alt = makepat( s, metas ) ) )
		    goto fail;
	    }

	    pat->type	= M_OR;

	    if ( pat != pattern ) {
		oldpat->next	= NULL;
		pat->next	= pattern;
	    }
	    goto ret;

	case '@'		:	/* at a column */
	    IF_META( M_COL );

	    if ( *++s != '(' )
		goto fail;

	    for ( i=0, ++s; isdigit( *s ); i++ )
		temp[i]	= *s++;

	    if ( *s != ')' )
		goto fail;

	    pat->col	= atoi( temp );

	    if ( pat->col == 0 )
		goto fail;

	    pat->type	= M_COL;
	    break;

	default:	/* just a character */
	normal:
	    if (on_metas & M_CASELESS) {
		pat->type = M_CASELESS;
		pat->c = tolower(*s);
	    }
	    else {
		pat->type = M_CHAR;
		pat->c = *s;
	    }
	    break;
	}

	oldpat		= pat;
	oldpat->next	= pat = (PATTERN *) at_calloc( 1, sizeof( PATTERN ) );
	on_metas	= orig_metas;
    }

    oldpat->next = NULL;
    at_free(pat);
 ret:
    at_free(temp);
    return (pattern);

 fail:
#ifdef PM_DEBUG
    warning("makpat: failed on char: %c\n", *s);
#endif
    freepat(pattern);
    return (NULL);
}

/*
 * FREEPAT - free a pattern
 */

void
freepat( register PATTERN *pat )
{
    if ( pat->next )
	freepat( pat->next );

    if ( pat->alt )
	freepat( pat->alt );

    if ( pat->ccl )
	at_free( pat->ccl );

    at_free( pat );

    return;
}





#ifdef TESTLIB

/*
 * PRINTPAT - print out a pattern in readable form.
*/

#include <stdio.h>

void 
printpat (PATTERN *pat)
{
	for (; pat; pat = pat->next) {
		switch (pat->type) {
		    case M_CLOSURE:
			printpat(pat->alt);
			printf("\\O*");
			break;

		    case M_CLOSURE1:
			printpat(pat->alt);
			printf("\\O+");
			break;

		    case M_RE:
			printf("\\O(");
			printpat(pat->alt);
			putchar(')');
			break;

		    case M_OR:
			printpat(pat->next);
			printf("\\O|");
			printpat(pat->alt);
			return;

		    case M_CHAR:	/* match a character */
			printf("\\C%c", pat->c);
			break;

		    case M_CASELESS:	/* match a character, ignore case */
			putchar(pat->c);
			break;

		    case M_BEGINNING:	/* match the beginning of line */
			printf("\\O^");
			break;

		    case M_END:/* match end of line */
			printf("\\O$");
			break;

		    case M_ANY:/* match any chracter */
			printf("\\O.");
			break;

		    case M_CCL:/* match a class of characters */
			printf("\\O[%s]", pat->ccl);
			break;

		    case M_NCCL:	/* NOT character class */
			printf("\\O[\\O^%s]", pat->ccl);
			break;

		    case M_COL:/* see if we are at this column */
			printf("\\O@(%d)", pat->col);
			break;

		    default:
			warning("PRINTPAT: can't happen\n");
		}
	}
}


	int
main( int argc, char *argv[] ) {
#if 0
	int             len;
	char            temp[80], *start;
	PATTERN        *pat;

	printf("string: '%s'\n", argv[2]);

	printf("pat: '%s' ==> '", argv[1]);
	if ((pat = makepat(argv[1], NULL)) == NULL) {
		printf("Invalid pattern syntax'\n");
		exit( 0 );
	}
	printpat(pat);
	printf("'\n");

	if (!(start = pmatch(pat, argv[2], &len)))
		printf("FAILED");

	else {
		strncpy(temp, start, len);
		printf("Matched: '%s'\n", temp);
	}
#else

	register PATTERN	*pat;
		 int		 len;
		 char		 buf[BUFSIZ];
	register char		*bufp	= buf;
	register char		*s;

	if ( argc != 2 )
		printf( "usage: pmatch pattern <file\n" );

	pat	= makepat( argv[1], NULL );
#if defined(PM_DEBUG)
	printpat( pat );
	putchar( '\n' );
#endif

	while ( gets( buf ) )
		if ( s = pmatch( pat, bufp, &len ) )
			puts( bufp );

	return( 0 );
#endif
}

#endif				/* TESTLIB */
