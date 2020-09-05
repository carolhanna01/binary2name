/* memmove.c: Replacement for systems that don't have it in libc.a. */

/*  Copyright (c) 1995 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Sat Jul 22 14:32:13 1995.  */

#if defined (HAVE_CONFIG_H)
#  include <config.h>
#endif

#if !defined (HAVE_MEMMOVE)
#include <sys/types.h>

extern void bcopy (char *b1, char *b2, int length);

void *
memmove (void *dst, const void *src, size_t count)
{
  void *result = dst;
  bcopy ((char *)src, (char *)dst, (int)count);
  return (result);
}
#endif /* !HAVE_MEMMOVE */

   
