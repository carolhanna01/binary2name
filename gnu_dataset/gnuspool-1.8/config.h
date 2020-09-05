/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define if Motif combo boxes don't work */
/* #undef BROKEN_COMBOBOX */

/* Define if Motif renditions don't work */
/* #undef BROKEN_RENDITION */

/* Define if Motif spin boxes don't work properly */
/* #undef BROKEN_SPINBOX */

/* Define if read from a raw terminal can return zero bytes */
/* #undef BROKEN_TERM_READ */

/* Define if "delcurterm" is unreliable and causes core dumps */
#define BUGGY_DELCURTERM 1

/* Define if signal(SIGCLD, SIG_IGN) cannot be relied upon */
/* #undef BUGGY_SIGCLD */

/* Define if "sprintf" returns a char* rather than an int */
/* #undef CHARSPRINTF */

/* Define if curses messes-up overlapping subwindows */
/* #undef CURSES_OVERLAP_BUG */

/* Define to 1 if the `getpgrp' function requires zero arguments. */
#define GETPGRP_VOID 1

/* Define to 1 if you have the `atexit' function. */
#define HAVE_ATEXIT 1

/* Define to 1 if you have the `bcopy' function. */
#define HAVE_BCOPY 1

/* Define to 1 if you have the declaration of `tzname', and to 0 if you don't.
   */
/* #undef HAVE_DECL_TZNAME */

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the `fchmod' function. */
#define HAVE_FCHMOD 1

/* Define to 1 if you have the `fchown' function. */
#define HAVE_FCHOWN 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `fgetc' function. */
#define HAVE_FGETC 1

/* Define to 1 if you have the `ftruncate' function. */
#define HAVE_FTRUNCATE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `c_s' library (-lc_s). */
/* #undef HAVE_LIBC_S */

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you support file names longer than 14 characters. */
#define HAVE_LONG_FILE_NAMES 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkdir' function. */
#define HAVE_MKDIR 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the `rename' function. */
#define HAVE_RENAME 1

/* Define to 1 if you have the `rmdir' function. */
#define HAVE_RMDIR 1

/* Define to 1 if you have the `seteuid' function. */
#define HAVE_SETEUID 1

/* Define to 1 if you have the `setreuid' function. */
#define HAVE_SETREUID 1

/* Define to 1 if you have the `sigaction' function. */
#define HAVE_SIGACTION 1

/* Define to 1 if you have the `sigset' function. */
#define HAVE_SIGSET 1

/* Define to 1 if you have the `sigvec' function. */
/* #undef HAVE_SIGVEC */

/* Define to 1 if you have the `sigvector' function. */
/* #undef HAVE_SIGVECTOR */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if `tm_zone' is a member of `struct tm'. */
#define HAVE_STRUCT_TM_TM_ZONE 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/filio.h> header file. */
/* #undef HAVE_SYS_FILIO_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/mman.h> header file. */
#define HAVE_SYS_MMAN_H 1

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

/* Define if we are using the terminfo library not termcap */
#define HAVE_TERMINFO 1

/* Define to 1 if you have the <termios.h> header file. */
#define HAVE_TERMIOS_H 1

/* Define to 1 if you have the <termio.h> header file. */
#define HAVE_TERMIO_H 1

/* Define if terminfo supports the TIGETSTR call */
#define HAVE_TIGETSTR 1

/* Define to 1 if your `struct tm' has `tm_zone'. Deprecated, use
   `HAVE_STRUCT_TM_TM_ZONE' instead. */
#define HAVE_TM_ZONE 1

/* Define to 1 if you don't have `tm_zone' but do have the external array
   `tzname'. */
/* #undef HAVE_TZNAME */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `waitpid' function. */
#define HAVE_WAITPID 1

/* Define to 1 if you have the <wait.h> header file. */
#define HAVE_WAIT_H 1

/* Define if Motif version has renditions in */
/* #undef HAVE_XMRENDITION */

/* Define to 1 if you have the <Xm/ComboBox.h> header file. */
/* #undef HAVE_XM_COMBOBOX_H */

/* Define to 1 if you have the <Xm/SpinB.h> header file. */
/* #undef HAVE_XM_SPINB_H */

/* Define if kernel permits setuid to shuffle between uid and euid */
#define ID_SWAP 1

/* Define to set initial job allocation */
#define INITJALLOC 20000

/* Define to set initial printer allocation */
#define INITPALLOC 200

/* Define if motif vn 2 */
/* #undef MOTIF_VN2 */

/* Define to compile for network version */
#define NETWORK_VERSION 1

/* Define if kernel doesn't honour suid bit for root */
/* #undef NHONSUID */

/* Define if DialogTemplates aren't defined in motif libraries */
/* #undef NO_DTEMPLATE */

/* Define if curses.h doesn't include termio.h */
#define NO_TERMIO_IN_CURSES 1

/* Define to size of message queue */
#define NUMXBUFS 409

/* Define if on AIX */
/* #undef OS_AIX */

/* Define if on AIX 4.3 */
/* #undef OS_AIX_4_3 */

/* Define if on AIX 5.0 */
/* #undef OS_AIX_5_0 */

/* Define if on BSDI */
/* #undef OS_BSDI */

/* Define if on DGUX */
/* #undef OS_DGUX */

/* Define if on DPX_2 */
/* #undef OS_DPX_2 */

/* Define if on Dynix (not ptx) */
/* #undef OS_DYNIX */

/* Define if on FreeBSD */
/* #undef OS_FREEBSD */

/* Define if on HPUX */
/* #undef OS_HPUX */

/* Define if on HPUX Itanium */
/* #undef OS_HPUX_IA64 */

/* Define if on GNU/Linux */
#define OS_LINUX 1

/* Define if on OSF1 */
/* #undef OS_OSF1 */

/* Define if on PTX */
/* #undef OS_PTX */

/* Define if on Ultrix */
/* #undef OS_ULTRIX */

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "src/spshed.c"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "src/spshed.c 1.8"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "src-spshed-c"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.8"

/* Define type for process ids */
#define PIDTYPE pid_t

/* Indicate that the poll(2) call is available on sockets */
/* #undef POLLSOCKETS */

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Indicate that scheduler must run as root - funny "kill" sematics */
/* #undef RUN_AS_ROOT */

/* Define to turn on SCO-style C2 security stuff */
/* #undef SCO_SECURITY */

/* Define to 1 if the `setpgrp' function takes no argument. */
#define SETPGRP_VOID 1

/* Indicate that we are using shadow password files */
#define SHADOW_PW 1

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `int *', as computed by sizeof. */
#define SIZEOF_INT_P 8

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of `unsigned', as computed by sizeof. */
#define SIZEOF_UNSIGNED 4

/* The size of `unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG 8

/* The size of `unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT 2

/* Define type for final arg of accept etc thanq HP */
#define SOCKLEN_T socklen_t

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define if stdio.h defines sys_errlist */
#define SYS_ERRLIST_IN_STDIO_H 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Define to use file locking instead of semaphores */
#define USING_FLOCK 1

/* Define to use Memory-mapped files rather than shared memory */
#define USING_MMAP 1

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

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef gid_t */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef uid_t */

/* Define this if the C preprocessor doesn't */
/* #undef unix */

#ifdef OS_HPUX
#ifdef	OS_HPUX_IA64
#ifndef	CID_T_DEFINED
#define	CID_T_DEFINED 1
typedef	int	cid_t;
#endif
#endif
#define _XOPEN_SOURCE_EXTENDED 1
#include <stdarg.h>
#endif

#if	SIZEOF_LONG != 4
#if	SIZEOF_INT != 4
error: no basic signed 4-byte
#else
#define	LONG	int
#endif
#else
#define	LONG	long
#endif

#if	SIZEOF_UNSIGNED_LONG != 4
#if	SIZEOF_UNSIGNED != 4
error: no basic unsigned 4-byte
#else
#define	ULONG	unsigned
#endif
#else
#define	ULONG	unsigned long
#endif

#if	SIZEOF_SHORT != 2 || SIZEOF_UNSIGNED_SHORT != 2
error: no basic 2-byte
#else
#define	SHORT	short
#define	USHORT	unsigned short
#endif

#if	SIZEOF_INT_P != SIZEOF_INT
#define	INT_TO_XTPOINTER(X)	(XtPointer)(long)(X)
#define	XTPOINTER_TO_INT(X)	(int)(long)(X)
#else
#define	INT_TO_XTPOINTER(X)	(XtPointer)(X)
#define	XTPOINTER_TO_INT(X)	(int)(X)
#endif

#define	ROOTID	0
#ifdef	__GNUC__
#define	NORETURN_FUNC	__attribute__ ((noreturn))
#define	MAINFN_TYPE	int
#else
#define	NORETURN_FUNC
#define	MAINFN_TYPE
#endif

