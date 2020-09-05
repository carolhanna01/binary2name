/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */
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



/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define if you want a debugging version. */
/* #undef DEBUG */

/* Define if you want a version that logs read operations. */
/* #undef DEBUG_ROP */

/* Define if you want a version that logs write operations. */
/* #undef DEBUG_WOP */

/* Define if you want a version with run-time diagnostic checking. */
/* #undef DIAGNOSTIC */

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define if you have the bool type. */
#define HAVE_BOOL /**/

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define if you C++ compiler doesn't knows false */
#define HAVE_FALSE /**/

/* Define if fcntl/F_SETFD denies child access to file descriptors. */
#define HAVE_FCNTL_F_SETFD /**/

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define if building big-file environment (e.g., AIX, HP/UX, Solaris). */
/* #undef HAVE_FILE_OFFSET_BITS */

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the `getuid' function. */
#define HAVE_GETUID 1

/* Define if you have the iconv() function and it works. */
#define HAVE_ICONV 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define if building big-file environment (Linux). */
/* #undef HAVE_LARGEFILE_SOURCE */

/* Define to 1 if you have the `unac' library (-lunac). */
/* #undef HAVE_LIBUNAC */

/* Define to 1 if you have the `z' library (-lz). */
#define HAVE_LIBZ 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the `localtime_r' function. */
#define HAVE_LOCALTIME_R 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the `memcmp' function. */
#define HAVE_MEMCMP 1

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mlock' function. */
#define HAVE_MLOCK 1

/* Define to 1 if you have the `mmap' function. */
#define HAVE_MMAP 1

/* Define to 1 if you have the `munlock' function. */
#define HAVE_MUNLOCK 1

/* Define to 1 if you have the `munmap' function. */
#define HAVE_MUNMAP 1

/* Mutex */
/* #undef HAVE_MUTEX_68K_GCC_ASSEMBLY */

/* Mutex */
/* #undef HAVE_MUTEX_AIX_CHECK_LOCK */

/* Mutex */
/* #undef HAVE_MUTEX_ALPHA_GCC_ASSEMBLY */

/* Mutex */
/* #undef HAVE_MUTEX_FCNTL */

/* Mutex */
/* #undef HAVE_MUTEX_HPPA_GCC_ASSEMBLY */

/* Mutex */
/* #undef HAVE_MUTEX_HPPA_MSEM_INIT */

/* Mutex */
/* #undef HAVE_MUTEX_IA64_GCC_ASSEMBLY */

/* Mutex */
/* #undef HAVE_MUTEX_MSEM_INIT */

/* Mutex */
#define HAVE_MUTEX_PTHREAD /**/

/* Mutex */
#define HAVE_MUTEX_PTHREADS /**/

/* Mutex */
/* #undef HAVE_MUTEX_RELIANTUNIX_INITSPIN */

/* Mutex */
/* #undef HAVE_MUTEX_SCO_X86_CC_ASSEMBLY */

/* Mutex */
/* #undef HAVE_MUTEX_SEMA_INIT */

/* Mutex */
/* #undef HAVE_MUTEX_SGI_INIT_LOCK */

/* Mutex */
/* #undef HAVE_MUTEX_SOLARIS_LOCK_TRY */

/* Mutex */
/* #undef HAVE_MUTEX_SOLARIS_LWP */

/* Mutex */
/* #undef HAVE_MUTEX_SPARC_GCC_ASSEMBLY */

/* Mutex */
/* #undef HAVE_MUTEX_TAS */

/* Mutex */
#define HAVE_MUTEX_THREADS /**/

/* Mutex */
/* #undef HAVE_MUTEX_UI_THREADS */

/* Mutex */
/* #undef HAVE_MUTEX_UTS_CC_ASSEMBLY */

/* Mutex */
/* #undef HAVE_MUTEX_X86_GCC_ASSEMBLY */

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the `pread' function. */
/* #undef HAVE_PREAD */

/* Define to 1 if you have the `pstat_getdynamic' function. */
/* #undef HAVE_PSTAT_GETDYNAMIC */

/* Define to 1 if you have the `pwrite' function. */
/* #undef HAVE_PWRITE */

/* Define to 1 if you have the `qsort' function. */
#define HAVE_QSORT 1

/* Define to 1 if you have the `raise' function. */
#define HAVE_RAISE 1

/* Define to 1 if you have the `sched_yield' function. */
#define HAVE_SCHED_YIELD 1

/* Define to 1 if you have the `select' function. */
#define HAVE_SELECT 1

/* Define to 1 if you have the `shmget' function. */
#define HAVE_SHMGET 1

/* Define to 1 if you have the `snprintf' function. */
#define HAVE_SNPRINTF 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the `strcasestr' function. */
#define HAVE_STRCASESTR 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the `strftime' function. */
#define HAVE_STRFTIME 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strncasecmp' function. */
#define HAVE_STRNCASECMP 1

/* Define to 1 if you have the `strncoll' function. */
/* #undef HAVE_STRNCOLL */

/* Define to 1 if you have the `strptime' function. */
#define HAVE_STRPTIME 1

/* Define if the function strptime is declared in <time.h>. */
#define HAVE_STRPTIME_DECL /**/

/* Define to 1 if you have the `strstr' function. */
#define HAVE_STRSTR 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the `sysconf' function. */
#define HAVE_SYSCONF 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/file.h> header file. */
#define HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the `timegm' function. */
#define HAVE_TIMEGM 1

/* Define if you C++ compiler doesn't knows true. */
#define HAVE_TRUE /**/

/* Define to 1 if you have the <unac.h> header file. */
/* #undef HAVE_UNAC_H */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `vsnprintf' function. */
#define HAVE_VSNPRINTF 1

/* Define to 1 if you have the `yield' function. */
/* #undef HAVE_YIELD */

/* Define to 1 if you have the <zlib.h> header file. */
#define HAVE_ZLIB_H 1

/* Define as const if the declaration of iconv() needs const. */
#define ICONV_CONST 

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "mifluz-bugs@gnu.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "mifluz"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "mifluz 0.26.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "mifluz"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "0.26.0"

/* Define if your sprintf returns a pointer, not a length. */
/* #undef SPRINTF_RET_CHARPNT */

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if the `S_IS*' macros in <sys/stat.h> do not work properly. */
/* #undef STAT_MACROS_BROKEN */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Define if using the dmalloc debugging malloc package */
/* #undef WITH_DMALLOC */

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
/* #undef YYTEXT_POINTER */

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef mode_t */

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

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
