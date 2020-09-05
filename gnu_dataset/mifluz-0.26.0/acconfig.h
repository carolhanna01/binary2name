/*

 Part of the ht://Dig package   <http://www.htdig.org/>
 Copyright (c) 1999, 2000, 2001 The ht://Dig Group
 For copyright details, see the file COPYING in your distribution
 or the GNU General Public License version 2 or later
 <http://www.gnu.org/copyleft/gpl.html>

*/
/* Defined in case the compiler doesn't have TRUE and FALSE constants */

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

@TOP@

@BOTTOM@

#ifndef HAVE_TRUE
#define true  TRUE
#endif
#ifndef HAVE_FALSE
#define false FALSE
#endif

#ifndef HAVE_BOOL
typedef char bool;
#endif

/*
 * Don't step on the namespace.  Other libraries may have their own
 * implementations of these functions, we don't want to use their
 * implementations or force them to use ours based on the load order.
 */
#ifndef	HAVE_GETCWD
#define	getcwd		__db_Cgetcwd
#endif
#ifndef	HAVE_MEMCMP
#define	memcmp		__db_Cmemcmp
#endif
#ifndef	HAVE_MEMCPY
#define	memcpy		__db_Cmemcpy
#endif
#ifndef	HAVE_MEMMOVE
#define	memmove		__db_Cmemmove
#endif
#ifndef	HAVE_RAISE
#define	raise		__db_Craise
#endif
#ifndef HAVE_SNPRINTF
#define	snprintf	__db_Csnprintf
#endif
#ifndef	HAVE_STRERROR
#define	strerror	__db_Cstrerror
#endif
#ifndef HAVE_VSNPRINTF
#define	vsnprintf	__db_Cvsnprintf
#endif
#ifndef HAVE_STRNCOLL
#define	strncoll	__db_Cstrncoll
#endif

/*
 * Big-file configuration.
 */
#ifdef	HAVE_FILE_OFFSET_BITS
#ifndef _FILE_OFFSET_BITS
#define	_FILE_OFFSET_BITS	64
#endif /* _FILE_OFFSET_BITS */
#endif /* HAVE_FILE_OFFSET_BITS */

#ifdef	HAVE_LARGEFILE_SOURCE
#ifndef _LARGEFILE_SOURCE
#define	_LARGEFILE_SOURCE
#endif /* _LARGEFILE_SOURCE */
#endif /* HAVE_LARGEFILE_SOURCE */
