/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* A GNU-like <dirent.h>.
   Copyright (C) 2006-2010 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef _GL_DIRENT_H

#if __GNUC__ >= 3
#pragma GCC system_header
#endif

/* The include_next requires a split double-inclusion guard.  */
#include_next <dirent.h>

#ifndef _GL_DIRENT_H
#define _GL_DIRENT_H

/* Get ino_t.  Needed on some systems, including glibc 2.8.  */
#include <sys/types.h>

/* The definition of _GL_ARG_NONNULL is copied here.  */
/* _GL_ARG_NONNULL((n,...,m)) tells the compiler and static analyzer tools
   that the values passed as arguments n, ..., m must be non-NULL pointers.
   n = 1 stands for the first argument, n = 2 for the second argument etc.  */
#ifndef _GL_ARG_NONNULL
# if (__GNUC__ == 3 && __GNUC_MINOR__ >= 3) || __GNUC__ > 3
#  define _GL_ARG_NONNULL(params) __attribute__ ((__nonnull__ params))
# else
#  define _GL_ARG_NONNULL(params)
# endif
#endif

/* The definition of _GL_WARN_ON_USE is copied here.  */
#ifndef _GL_WARN_ON_USE

# if 4 < __GNUC__ || (__GNUC__ == 4 && 3 <= __GNUC_MINOR__)
/* A compiler attribute is available in gcc versions 4.3.0 and later.  */
#  define _GL_WARN_ON_USE(function, message) \
extern __typeof__ (function) function __attribute__ ((__warning__ (message)))

# else /* Unsupported.  */
#  define _GL_WARN_ON_USE(function, message) \
extern int _gl_warn_on_use
# endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Declare overridden functions.  */

#if 0
# define closedir rpl_closedir
extern int closedir (DIR *) _GL_ARG_NONNULL ((1));
#endif

#if 1
# if !1 && !defined dirfd
/* Return the file descriptor associated with the given directory stream,
   or -1 if none exists.  */
extern int dirfd (DIR *dir) _GL_ARG_NONNULL ((1));
# endif
#elif defined GNULIB_POSIXCHECK
# undef dirfd
# if HAVE_RAW_DECL_DIRFD
_GL_WARN_ON_USE (dirfd, "dirfd is unportable - "
                 "use gnulib module dirfd for portability");
# endif
#endif

#if 1
# if 0
#  undef fdopendir
#  define fdopendir rpl_fdopendir
# endif
# if !1 || 0
/* Open a directory stream visiting the given directory file
   descriptor.  Return NULL and set errno if fd is not visiting a
   directory.  On success, this function consumes fd (it will be
   implicitly closed either by this function or by a subsequent
   closedir).  */
extern DIR *fdopendir (int fd);
# endif
#elif defined GNULIB_POSIXCHECK
# undef fdopendir
# if HAVE_RAW_DECL_FDOPENDIR
_GL_WARN_ON_USE (fdopendir, "fdopendir is unportable - "
                 "use gnulib module fdopendir for portability");
# endif
#endif

#if 0
# define opendir rpl_opendir
extern DIR * opendir (const char *) _GL_ARG_NONNULL ((1));
#endif

#if 0
/* Scan the directory DIR, calling FILTER on each directory entry.
   Entries for which FILTER returns nonzero are individually malloc'd,
   sorted using qsort with CMP, and collected in a malloc'd array in
   *NAMELIST.  Returns the number of entries selected, or -1 on error.  */
# if !1
extern int scandir (const char *dir, struct dirent ***namelist,
                    int (*filter) (const struct dirent *),
                    int (*cmp) (const struct dirent **, const struct dirent **))
     _GL_ARG_NONNULL ((1, 2, 4));
# endif
#elif defined GNULIB_POSIXCHECK
# undef scandir
# if HAVE_RAW_DECL_SCANDIR
_GL_WARN_ON_USE (scandir, "scandir is unportable - "
                 "use gnulib module scandir for portability");
# endif
#endif

#if 0
/* Compare two 'struct dirent' entries alphabetically.  */
# if !1
extern int alphasort (const struct dirent **, const struct dirent **)
     _GL_ARG_NONNULL ((1, 2));
# endif
#elif defined GNULIB_POSIXCHECK
# undef alphasort
# if HAVE_RAW_DECL_ALPHASORT
_GL_WARN_ON_USE (alphasort, "alphasort is unportable - "
                 "use gnulib module alphasort for portability");
# endif
#endif

#ifdef __cplusplus
}
#endif


#endif /* _GL_DIRENT_H */
#endif /* _GL_DIRENT_H */
