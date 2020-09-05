/*
 * Boyer - Moore string pattern matching.
 *
 * Usage:	#include <pmatch.h>
 *
 *		BM_PATTERN	*bm_makepat( pattern );
 *		char		*bm_pmatch( pat, string );
 *		void		 bm_freepat( pat );
 *
 *		char		*pattern, *string;
 *		BM_PATTERN	*pat;
 *
 * written: RWMarejka; 85.12.16
 * From: "Data Structure Techniques" by Thomas A. Standish
 *		section 7.5.2.2 "The Boyer - Moore Algorithm"
 */

#define _REENTRANT
#include <string.h>
#include <malloc.h>
#include "brahma.h"

#if !defined(_DEBUG)
#	define	_DEBUG		0
#endif

#if !defined(TEST)
#	define	TEST		0
#endif

#define	loop	while(1)
#define	ALPHASIZ	128
#define	FALSE		0
#define	TRUE		1
#define	MAX(a,b)	((a)>(b)?(a):(b))

typedef struct bm_pattern {	/* Boyer - Moore pattern		*/
	short		 p_m;		/* length of pattern string	*/
	short		 p_r[ALPHASIZ];	/* "r" vector			*/
	short		*p_R;		/* "R" vector			*/
	char		*p_pat;		/* pattern string		*/
} BM_PATTERN;

char		*bm_pmatch( BM_PATTERN *, char * );
BM_PATTERN	*bm_makepat( char * );
void		 bm_freepat( BM_PATTERN * );


/*
 * BM_MAKEPAT - make a Boyer - Moore pattern.
 *
 */

	BM_PATTERN *
bm_makepat( char *p ) {
	register int		 i, j, k, l;
		 short		 m;
		 short		*r, *R;
		 char		*s;
		 int		 unify;
		 BM_PATTERN	*pat;

	pat		= (BM_PATTERN *) at_calloc( 1, sizeof( BM_PATTERN ) );

	if ( !pat )
		return( NULL );

	pat->p_m	= strlen( p );

	if( !(  pat->p_pat = strdup( p ) ) ) {
		at_free( pat );
		return( NULL );
	}

	pat->p_R	= (short *) at_calloc( pat->p_m + 1, sizeof( short ) );
	r		= pat->p_r;
	R		= pat->p_R;
	m		= pat->p_m;

	if ( !R ) {
		at_free( pat->p_pat );
		at_free( pat );
		return( NULL );
	}

	for ( i=0; i < ALPHASIZ; ++i )	/* compute "r"			*/
		if ( s = strrchr( p, i ) )
			r[i]	= m - ( s - p ) - 1;
		else
			r[i]	= m;

	for ( j=0; j < m; ++j ) {	/* compute "R"			*/
		k	= j;		/* set maximum k		*/
		loop {		/* look for unification		*/
			if ( ( k > 0 ) && ( p[k-1] == p[j] ) ) {
				--k;
				continue;
			}

			l	= k + m - j - 1;
			i	= m - 1;
			unify	= FALSE;

			loop {	/* test one possibility		*/

				if ( ( i == j ) || ( l < 1 ) ) {
					unify	= TRUE;
					break;
				}
				else if ( p[i] != p[l] ) {
					unify	= FALSE;
					break;
				}
				else
					( --i, --l );
			}
			if ( unify == TRUE ) {	/* did unify pattern	*/
				R[j]	= m - k;
				break;
			}
			else {		/* failed, decrement and go on	*/
				--k;
				continue;
			}
		}			/* end of loop		*/
	}				/* end of for			*/

	return( pat );
}

/*
 * BM_PMATCH - look for a pattern in given string.
 *
 */

	char *
bm_pmatch( BM_PATTERN *pat, register char *s ) {
	register short		 i, j, n;
		 short		 m;
		 short		*r, *R;
		 char		*p;		/* ptr to pattern "p"	*/
		 char		*S;		/* ptr to start of "s"	*/
		 char		*Pend;		/* ptr to end of pattern*/

	n	= strlen( s );		/* initialize			*/
	p	= pat->p_pat;
	r	= pat->p_r;
	R	= pat->p_R;
	m	= pat->p_m;
	i	= m - 1;
	S	= s;
	Pend	= &(pat->p_pat[i]);
	s	= &S[i];

	loop {
		if ( i >= n )		/* failure			*/
			return( NULL );
		else			/* restart			*/
			( j = m - 1, p = Pend );

		loop {
#if _DEBUG
	printf( "S(i: %d): '%c', P(j: %d): '%c'\n", i, s[i], j, p[j] );
#endif
			if ( j == (-1) )	/* success		*/
				return( s );

			if ( *s == *p ) {	/* compare		*/
				( --j, --i );
				( --s, --p );
			}
			else {		/* advance i			*/
#if _DEBUG
	printf( "advance i: R[j: %d]: %d, r[S(i: %d): '%c'): %d\n",
		j, R[j], i, s[i], r[s[i]] );
#endif
				i	+= MAX(R[j],r[*s]);
				s	 = &S[i];
				break;
			}
		}			/* end of loop		*/
	}				/* end of loop		*/
/* NOTREACHED */
}

/*
 * BM_FREEPAT - free a Boyer - Moore pattern.
 *
 */

void
bm_freepat(BM_PATTERN	*pattern)
{
	at_free( pattern->p_pat );
	at_free( (char *) pattern->p_R   );
	at_free( (char *) pattern        );

	return;
}

#if TEST

#include <stdio.h>

main( int argc, char *argv[] ) {
	char		buf[BUFSIZ];
	int		 lineno;
	BM_PATTERN	*pat;

	pat	= bm_makepat( argv[1] );
	lineno	= 1;

	while ( gets( buf ) ) {
		if ( bm_pmatch( pat, buf ) )
			printf( "%d: %s\n", lineno, buf );

		++lineno;
	}
	exit( 0 );
}
#endif	/* TEST		*/
