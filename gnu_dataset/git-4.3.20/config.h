/* config.h.  Generated automatically by configure.  */
/* config.h.in.  Generated automatically from configure.in by autoheader.  */
/* $Id: config.h.top,v 1.3 1999/01/16 22:37:15 tudor Exp $ */
#include <version.h>

/* Define to get the GNU functionality in the C header files.  */
#define _GNU_SOURCE

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
/* #undef _ALL_SOURCE */
#endif

/* Define to empty if the keyword does not work.  */
/* #undef const */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef gid_t */

/* Define if you support file names longer than 14 characters.  */
#define HAVE_LONG_FILE_NAMES 1

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#define HAVE_SYS_WAIT_H 1

/* Define if on MINIX.  */
/* #undef _MINIX */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define to `long' if <sys/types.h> doesn't define.  */
/* #undef off_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define if the system does not provide POSIX.1 features except
   with this defined.  */
/* #undef _POSIX_1_SOURCE */

/* Define if you need to in order for stat and other things to work.  */
/* #undef _POSIX_SOURCE */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define if the `S_IS*' macros in <sys/stat.h> do not work properly.  */
/* #undef STAT_MACROS_BROKEN */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef uid_t */

/* Define if you want to disable various consistency checkings.  */
#define NDEBUG 1

/* Define if you have GCC.  */
#define HAVE_GCC 1

/* Define if you have a termcap library.  */
#define HAVE_LIBTERMCAP 1

/* Define if you have a terminfo library.  */
/* #undef HAVE_LIBTERMINFO */

/* Define if your system is Linux.  */
/* #undef HAVE_LINUX */

/* Define if you have a dumb C compiler, like the one running on B.O.S.  */
/* #undef HAVE_DUMB_CC */

/* Define if you have POSIX compatible terminal interface.  */
#define HAVE_POSIX_TTY 1

/* Define if you have System V compatible terminal interface.  */
#define HAVE_SYSTEMV_TTY 1

/* Define if you have BSD compatible terminal interface.  */
/* #undef HAVE_BSD_TTY */

/* Define if you have the utsname system call.  */
#define HAVE_UTSNAME 1

/* Define if you have the TIOCGWINSZ ioctl system call.  */
#define HAVE_WINSZ 1

/* Define if you have two-argument statfs with statfs.bsize member
   (AIX, 4.3BSD).  */
/* #undef STAT_STATFS2_BSIZE */

/* Define if you have two-argument statfs with statfs.fsize member
   (4.4BSD and NetBSD).  */
/* #undef STAT_STATFS2_FSIZE */

/* Define if you have two-argument statfs with struct fs_data (Ultrix).  */
/* #undef STAT_STATFS2_FS_DATA */

/* Define if you have 3-argument statfs function (DEC OSF/1).  */
/* #undef STAT_STATFS3_OSF1 */

/* Define if you have four-argument statfs (AIX-3.2.5, SVR3).  */
/* #undef STAT_STATFS4 */

/* Define if you have the statvfs system call.  */
#define STAT_STATVFS 1

/* Define if you have a broken sys/filesys.h header file.  */
/* #undef BROKEN_SYS_FILSYS_H */

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the lstat function.  */
#define HAVE_LSTAT 1

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the putenv function.  */
#define HAVE_PUTENV 1

/* Define if you have the readlink function.  */
#define HAVE_READLINK 1

/* Define if you have the rename function.  */
#define HAVE_RENAME 1

/* Define if you have the setenv function.  */
#define HAVE_SETENV 1

/* Define if you have the sigaction function.  */
#define HAVE_SIGACTION 1

/* Define if you have the statvfs function.  */
#define HAVE_STATVFS 1

/* Define if you have the strcasecmp function.  */
#define HAVE_STRCASECMP 1

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

/* Define if you have the strncasecmp function.  */
#define HAVE_STRNCASECMP 1

/* Define if you have the strstr function.  */
#define HAVE_STRSTR 1

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <stddef.h> header file.  */
#define HAVE_STDDEF_H 1

/* Define if you have the <stdlib.h> header file.  */
#define HAVE_STDLIB_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/dustat.h> header file.  */
/* #undef HAVE_SYS_DUSTAT_H */

/* Define if you have the <sys/filsys.h> header file.  */
/* #undef HAVE_SYS_FILSYS_H */

/* Define if you have the <sys/mount.h> header file.  */
#define HAVE_SYS_MOUNT_H 1

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/param.h> header file.  */
#define HAVE_SYS_PARAM_H 1

/* Define if you have the <sys/sioctl.h> header file.  */
/* #undef HAVE_SYS_SIOCTL_H */

/* Define if you have the <sys/statfs.h> header file.  */
#define HAVE_SYS_STATFS_H 1

/* Define if you have the <sys/statvfs.h> header file.  */
#define HAVE_SYS_STATVFS_H 1

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <sys/vfs.h> header file.  */
#define HAVE_SYS_VFS_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the <values.h> header file.  */
#define HAVE_VALUES_H 1

/* Define if you have the readline library (-lreadline).  */
#define HAVE_LIBREADLINE 1

/* Define if you have the sun library (-lsun).  */
/* #undef HAVE_LIBSUN */

/* Define if you have the ucb library (-lucb).  */
/* #undef HAVE_LIBUCB */

/* Name of package */
#define PACKAGE "git"

/* Version number of package */
#define VERSION "4.3.20"

/* $Id: config.h.bot,v 1.2 1998/03/02 08:33:14 tudor Exp $ */
/* See the file PROBLEMS for details on this.  */
#ifdef HAVE_LINUX
#define HAVE_LSTAT 1
#define HAVE_LSTAT 1
#endif

