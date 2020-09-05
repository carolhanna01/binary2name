/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */


/* Supported event notification types */
#define IFACE_INOTIFY 0  /* inotify - Linux */
#define IFACE_KQUEUE  1  /* kqueue  - BSD */


/* Define to 1 if translation of program messages to the user's native
   language is requested. */
#define ENABLE_NLS 1

/* Define to 1 if you have the MacOS X function CFLocaleCopyCurrent in the
   CoreFoundation framework. */
/* #undef HAVE_CFLOCALECOPYCURRENT */

/* Define to 1 if you have the MacOS X function CFPreferencesCopyAppValue in
   the CoreFoundation framework. */
/* #undef HAVE_CFPREFERENCESCOPYAPPVALUE */

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
#define HAVE_DCGETTEXT 1

/* Define to 1 if you have the `getdtablesize' function. */
#define HAVE_GETDTABLESIZE 1

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the `getopt_long' function. */
#define HAVE_GETOPT_LONG 1

/* Define if the GNU gettext() function is already present or preinstalled. */
#define HAVE_GETTEXT 1

/* Define if you have the iconv() function and it works. */
#define HAVE_ICONV 1

/* Define to 1 if you have the `inotify_init' function. */
#define HAVE_INOTIFY_INIT 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `kqueue' function. */
/* #undef HAVE_KQUEUE */

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `rfork' function. */
/* #undef HAVE_RFORK */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `sysconf' function. */
#define HAVE_SYSCONF 1

/* Define to 1 if you have the <sys/event.h> header file. */
/* #undef HAVE_SYS_EVENT_H */

/* Define to 1 if you have the <sys/inotify.h> header file. */
#define HAVE_SYS_INOTIFY_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define as const if the declaration of iconv() needs const. */
#define ICONV_CONST 

/* Name of package */
#define PACKAGE "direvent"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "bug-direvent@gnu.org.ua"

/* Define to the full name of this package. */
#define PACKAGE_NAME "GNU Direvent"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "GNU Direvent 5.2"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "direvent"

/* Define to the home page for this package. */
#define PACKAGE_URL "http://www.gnu.org.ua/software/direvent"

/* Define to the version of this package. */
#define PACKAGE_VERSION "5.2"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* event notification interface to use */
#define USE_IFACE IFACE_INOTIFY

/* Version number of package */
#define VERSION "5.2"

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
/* #undef YYTEXT_POINTER */
