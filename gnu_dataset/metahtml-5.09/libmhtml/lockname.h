/* lockname.h: -*- C -*-  How to lock a file. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Tue Jun 11 01:56:39 1996.

   This file is part of <Meta-HTML>(tm), a system for the rapid deployment
   of Internet and Intranet applications via the use of the Meta-HTML
   language.

   Copyright (c) 1995, 1996, Brian J. Fox (bfox@ai.mit.edu).
   Copyright (c) 1996, Universal Access Inc. (http://www.ua.com).

   Meta-HTML is free software; you can redistribute it and/or modify
   it under the terms of the UAI Free Software License as published
   by Universal Access Inc.; either version 1, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   UAI Free Software License for more details.

   You should have received a copy of the UAI Free Software License
   along with this program; if you have not, you may obtain one by
   writing to:

   Universal Access Inc.
   129 El Paseo Court
   Santa Barbara, CA
   93101  */

#if !defined (_LOCKNAME_H_)
#define _LOCKNAME_H_ 1

#if defined (__cplusplus)
extern "C"
{
#endif

#if defined (HAVE_FCNTL_H)
#  include <fcntl.h>
#else
#  if defined (HAVE_SYS_FCNTL_H)
#    include <sys/fcntl.h>
#  endif
#endif

#if !defined (SEEK_SET)
#  define SEEK_SET 0
#endif

#if !defined (LOCK_SH)
#  define LOCK_SH 0x01
#endif
#if !defined (LOCK_EX)
#  define LOCK_EX 0x02
#endif
#if !defined (LOCK_NB)
#  define LOCK_NB 0x04
#endif
#if !defined (LOCK_UN)
#  define LOCK_UN 0x08
#endif

#if defined (HAVE_FLOCK)

#  define LOCKFILE(fd) flock (fd, LOCK_EX)
#  define READLOCKFILE(fd) flock (fd, LOCK_SH)
#  define UNLOCKFILE(fd) flock (fd, LOCK_UN)

#else  /* !HAVE_FLOCK */

extern int LOCKFILE (int fd);
extern int UNLOCKFILE (int fd);
extern int READLOCKFILE (int fd);

#endif /* !HAVE_FLOCK */

extern char *db_lockname (char *input);

#if defined (__cplusplus)
}
#endif

#endif /* !_LOCKNAME_H_ */
