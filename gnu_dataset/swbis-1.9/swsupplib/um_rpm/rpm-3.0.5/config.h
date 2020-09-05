/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */
/* acconfig.h
   This file is in the public domain.

   Descriptive text for the C preprocessor macros that
   the distributed Autoconf macros can define.
   No software package will use all of them; autoheader copies the ones
   your configure.in uses into your configuration header file templates.

   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  Although this order
   can split up related entries, it makes it easier to check whether
   a given entry is in the file.

   Leave the following blank line there!!  Autoheader needs it.  */

/* Define to the name of the distribution.  */
#define PACKAGE "rpm"

/* Define to the version of the distribution.  */
#define VERSION "3.0.5"

/* Define to 1 if ANSI function prototypes are usable.  */
#define PROTOTYPES 1

/* Define to 1 if NLS is requested.  */
/* #undef ENABLE_NLS */

/* Define as 1 if you have catgets and don't want to use GNU gettext.  */
/* #undef HAVE_CATGETS */

/* Define as 1 if you have gettext and don't want to use GNU gettext.  */
/* #undef HAVE_GETTEXT */

/* Define to 1 if you have the stpcpy function.  */
#define HAVE_STPCPY 1

/* Define if your locale.h file contains LC_MESSAGES.  */
/* #undef HAVE_LC_MESSAGES */

/* Define as 1 if you have mntctl() (only aix?) */
/* #undef HAVE_MNTCTL */

/* Define as 1 if <netdb.h> defines h_errno */
#define HAVE_HERRNO 1

/* Define as 1 if <sys/stat.h> defines S_ISLNK */
#define HAVE_S_ISLNK 1

/* Define as 1 if <sys/stat.h> defines S_IFSOCK */
#define HAVE_S_IFSOCK 1

/* Define as 1 if <sys/stat.h> defines S_ISSOCK */
#define HAVE_S_ISSOCK 1

/* Define as 1 if we need timezone */
/* #undef NEED_TIMEZONE */

/* Define as 1 if we need myrealloc */
/* #undef NEED_MYREALLOC */

/* Define as one if we need to include <strings.h> (along with <string.h>) */
/* #undef NEED_STRINGS_H */

/* Define as 1 if you have getmntinfo_r() (only osf?) */
/* #undef HAVE_GETMNTINFO_R */

/* Define as 1 if you have "struct mnttab" (only sco?) */
/* #undef HAVE_STRUCT_MNTTAB */

/* Define as 1 if you have lchown() */
#define HAVE_LCHOWN 1

/* Define as 1 if chown() follows symlinks and you don't have lchown() */
/* #undef CHOWN_FOLLOWS_SYMLINK */

/* Define if the patch call you'll be using is 2.1 or older */
/* #undef HAVE_OLDPATCH_21 */

/* Define as 1 if your zlib has gzseek() */
#define HAVE_GZSEEK 1

/* Define to the full path name of the bzip2 library (libbz2.a) */
/* #undef BZIP2LIB */

/* Define as 1 if you bzip2 1.0 */
/* #undef HAVE_BZ2_1_0 */

/* A full path to a program, possibly with arguments, that will create a
   directory and all necessary parent directories, ala `mkdir -p'        */
#define MKDIR_P "/bin/mkdir -p"

/* Full path to rpm locale directory (usually /usr/share/locale) */
#define LOCALEDIR "/usr/local/share/locale"

/* Full path to rpm global configuration directory (usually /usr/lib/rpm) */
#define RPMCONFIGDIR "/usr/local/lib/rpm"

/* Full path to rpm system configuration directory (usually /etc/rpm) */
#define SYSCONFIGDIR "/etc/rpm"

/* Full path to find-provides script (usually /usr/lib/rpm/find-provides) */
#define FINDPROVIDES "/usr/local/lib/rpm/find-provides"

/* Full path to find-requires script (usually /usr/lib/rpm/find-requires) */
#define FINDREQUIRES "/usr/local/lib/rpm/find-requires"

/* Full path to rpmpopt configuration file (usually /usr/lib/rpm/rpmpopt) */
#define LIBRPMALIAS_FILENAME "/usr/local/lib/rpm/rpmpopt"

/* Full path to rpmrc configuration file (usually /usr/lib/rpm/rpmrc) */
#define LIBRPMRC_FILENAME "/usr/local/lib/rpm/rpmrc"

/* Full path to macros configuration file (usually /usr/lib/rpm/macros) */
#define MACROFILES "/usr/local/lib/rpm/macros"

/* statfs in <sys/statvfs.h> (for solaris 2.6+ systems) */
#define STATFS_IN_SYS_STATVFS 1

/* statfs in <sys/vfs.h> (for linux systems) */
/* #undef STATFS_IN_SYS_VFS */

/* statfs in <sys/mount.h> (for Digital Unix 4.0D systems) */
/* #undef STATFS_IN_SYS_MOUNT */

/* statfs in <sys/statfs.h> (for Irix 6.4 systems) */
/* #undef STATFS_IN_SYS_STATFS */

/* define if struct statfs has the f_bavail member */
#define STATFS_HAS_F_BAVAIL 1

/* define if the statfs() call takes 4 arguments */
/* #undef STAT_STATFS4 */

/* Absolute path to rpm top_sourcedir. */
#define TOP_SOURCEDIR "/home/carolhanna/Security_Project/gnu_dataset/swbis-1.9/swsupplib/um_rpm/rpm-3.0.5"

/* define if support rpm-1.0 packages is desired */
/* #undef ENABLE_V1_PACKAGES */

/* define if experimental support rpm-4.0 packages is desired */
/* #undef ENABLE_V5_PACKAGES */

/* Use the included glob.c? */
/* #undef USE_GNU_GLOB */

/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */


/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
/* #undef ENABLE_NLS */

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <argz.h> header file. */
/* #undef HAVE_ARGZ_H */

/* Define to 1 if you have the `basename' function. */
#define HAVE_BASENAME 1

/* Define to 1 if you have the <bzlib.h> header file. */
/* #undef HAVE_BZLIB_H */

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
/* #undef HAVE_DCGETTEXT */

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
/* #undef HAVE_DLFCN_H */

/* Define to 1 if you don't have `vprintf' but do have `_doprnt.' */
/* #undef HAVE_DOPRNT */

/* Define to 1 if you have the <err.h> header file. */
#define HAVE_ERR_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `feof_unlocked' function. */
/* #undef HAVE_FEOF_UNLOCKED */

/* Define to 1 if you have the `fgets_unlocked' function. */
/* #undef HAVE_FGETS_UNLOCKED */

/* Define to 1 if your system has a working `fnmatch' function. */
#define HAVE_FNMATCH 1

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the `getc_unlocked' function. */
/* #undef HAVE_GETC_UNLOCKED */

/* Define to 1 if you have the `getegid' function. */
/* #undef HAVE_GETEGID */

/* Define to 1 if you have the `geteuid' function. */
/* #undef HAVE_GETEUID */

/* Define to 1 if you have the `getgid' function. */
/* #undef HAVE_GETGID */

/* Define to 1 if you have the <getopt.h> header file. */
#define HAVE_GETOPT_H 1

/* Define to 1 if you have the `getpagesize' function. */
/* #undef HAVE_GETPAGESIZE */

/* Define to 1 if you have the `getpassphrase' function. */
/* #undef HAVE_GETPASSPHRASE */

/* Define if the GNU gettext() function is already present or preinstalled. */
/* #undef HAVE_GETTEXT */

/* Define to 1 if you have the `getuid' function. */
/* #undef HAVE_GETUID */

/* Define to 1 if you have the `getwd' function. */
#define HAVE_GETWD 1

/* Define to 1 if you have the `glob' function. */
#define HAVE_GLOB 1

/* Define to 1 if you have the <glob.h> header file. */
#define HAVE_GLOB_H 1

/* Define to 1 if you have the <grp.h> header file. */
#define HAVE_GRP_H 1

/* Define if you have the iconv() function. */
/* #undef HAVE_ICONV */

/* Define to 1 if you have the `inet_aton' function. */
#define HAVE_INET_ATON 1

/* Define if <inttypes.h> exists and doesn't clash with <sys/types.h>. */
#define HAVE_INTTYPES_H 1

/* Define if <inttypes.h> exists, doesn't clash with <sys/types.h>, and
   declares uintmax_t. */
/* #undef HAVE_INTTYPES_H_WITH_UINTMAX */

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
/* #undef HAVE_LANGINFO_CODESET */

/* Define if your <locale.h> file defines LC_MESSAGES. */
/* #undef HAVE_LC_MESSAGES */

/* Define to 1 if you have the <libio.h> header file. */
#define HAVE_LIBIO_H 1

/* Define to 1 if you have the `port' library (-lport). */
/* #undef HAVE_LIBPORT */

/* Define to 1 if you have the <limits.h> header file. */
/* #undef HAVE_LIMITS_H */

/* Define to 1 if you have the <locale.h> header file. */
/* #undef HAVE_LOCALE_H */

/* Define to 1 if you have the <machine/types.h> header file. */
/* #undef HAVE_MACHINE_TYPES_H */

/* Define to 1 if you have the <malloc.h> header file. */
/* #undef HAVE_MALLOC_H */

/* Define to 1 if you have the <mcheck.h> header file. */
#define HAVE_MCHECK_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mempcpy' function. */
/* #undef HAVE_MEMPCPY */

/* Define to 1 if you have a working `mmap' system call. */
/* #undef HAVE_MMAP */

/* Define to 1 if you have the <mntent.h> header file. */
#define HAVE_MNTENT_H 1

/* Define to 1 if you have the `mtrace' function. */
#define HAVE_MTRACE 1

/* Define to 1 if you have the `munmap' function. */
/* #undef HAVE_MUNMAP */

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the <netdb.h> header file. */
#define HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in_systm.h> header file. */
#define HAVE_NETINET_IN_SYSTM_H 1

/* Define to 1 if you have the <nl_types.h> header file. */
/* #undef HAVE_NL_TYPES_H */

/* Define to 1 if you have the `personality' function. */
#define HAVE_PERSONALITY 1

/* Define to 1 if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* Define to 1 if you have the <pwd.h> header file. */
#define HAVE_PWD_H 1

/* Define to 1 if you have the `realpath' function. */
#define HAVE_REALPATH 1

/* Define to 1 if you have the `regcomp' function. */
#define HAVE_REGCOMP 1

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV 1

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the <stddef.h> header file. */
/* #undef HAVE_STDDEF_H */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define if <stdint.h> exists, doesn't clash with <sys/types.h>, and declares
   uintmax_t. */
/* #undef HAVE_STDINT_H_WITH_UINTMAX */

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `stpcpy' function. */
#define HAVE_STPCPY 1

/* Define to 1 if you have the `stpncpy' function. */
#define HAVE_STPNCPY 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the `strcspn' function. */
#define HAVE_STRCSPN 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strncasecmp' function. */
#define HAVE_STRNCASECMP 1

/* Define to 1 if you have the `strspn' function. */
#define HAVE_STRSPN 1

/* Define to 1 if you have the `strstr' function. */
#define HAVE_STRSTR 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/mman.h> header file. */
#define HAVE_SYS_MMAN_H 1

/* Define to 1 if you have the <sys/mntctl.h> header file. */
/* #undef HAVE_SYS_MNTCTL_H */

/* Define to 1 if you have the <sys/mnttab.h> header file. */
/* #undef HAVE_SYS_MNTTAB_H */

/* Define to 1 if you have the <sys/mount.h> header file. */
#define HAVE_SYS_MOUNT_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/param.h> header file. */
/* #undef HAVE_SYS_PARAM_H */

/* Define to 1 if you have the <sys/resource.h> header file. */
#define HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/stdtypes.h> header file. */
/* #undef HAVE_SYS_STDTYPES_H */

/* Define to 1 if you have the <sys/systemcfg.h> header file. */
/* #undef HAVE_SYS_SYSTEMCFG_H */

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
#define HAVE_SYS_UTSNAME_H 1

/* Define to 1 if you have the <sys/vmount.h> header file. */
/* #undef HAVE_SYS_VMOUNT_H */

/* Define to 1 if you have the <sys/wait.h> header file. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the `tsearch' function. */
/* #undef HAVE_TSEARCH */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define if you have the unsigned long long type. */
/* #undef HAVE_UNSIGNED_LONG_LONG */

/* Define to 1 if you have the <utime.h> header file. */
#define HAVE_UTIME_H 1

/* Define to 1 if you have the `vprintf' function. */
#define HAVE_VPRINTF 1

/* Define to 1 if you have the <zlib.h> header file. */
#define HAVE_ZLIB_H 1

/* Define to 1 if you have the `__argz_count' function. */
/* #undef HAVE___ARGZ_COUNT */

/* Define to 1 if you have the `__argz_next' function. */
/* #undef HAVE___ARGZ_NEXT */

/* Define to 1 if you have the `__argz_stringify' function. */
/* #undef HAVE___ARGZ_STRINGIFY */

/* Define as const if the declaration of iconv() needs const. */
/* #undef ICONV_CONST */

/* Define if integer division by zero raises signal SIGFPE. */
/* #undef INTDIV0_RAISES_SIGFPE */

/* Define to 1 if `major', `minor', and `makedev' are declared in <mkdev.h>.
   */
/* #undef MAJOR_IN_MKDEV */

/* Define to 1 if `major', `minor', and `makedev' are declared in
   <sysmacros.h>. */
/* #undef MAJOR_IN_SYSMACROS */

/* Name of package */
#define PACKAGE "rpm"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME ""

/* Define to the full name and version of this package. */
#define PACKAGE_STRING ""

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME ""

/* Define to the version of this package. */
#define PACKAGE_VERSION ""

/* Define if <inttypes.h> exists and defines unusable PRI* macros. */
/* #undef PRI_MACROS_BROKEN */

/* Define if compiler has function prototypes */
#define PROTOTYPES 1

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
        STACK_DIRECTION > 0 => grows toward higher addresses
        STACK_DIRECTION < 0 => grows toward lower addresses
        STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Version number of package */
#define VERSION "3.0.5"

/* Define to 1 if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define as `__inline' if that's what the C compiler calls it, or to nothing
   if it is not supported. */
/* #undef inline */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef mode_t */

/* Define to `long' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to unsigned long or unsigned long long if <inttypes.h> and
   <stdint.h> don't define. */
/* #undef uintmax_t */
