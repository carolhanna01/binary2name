/* common.h - macros needed by all files, including other .h files;
              this means that it must be included in dominion.h before
              almost all other files.
 */

#ifndef _COMMON_H
#define _COMMON_H

/* get the control char corresponding to char c; checking if c is
   lower case or upper before converting.  NOTE: will probably only
   work for ascii. */
#define CTL(c) ( (c<'a') ? (c - 'A' + 1) : (c - 'a' + 1) )
  /* the DELETE key */
#define DEL ((char) 0x7F)

  /* BSD seems to not have beep(), or at least sunOS4.0 does not.
     so we have a macro called NOBEEP for such machines.
   */
#ifndef HAVE_BEEP
# define beep() (putchar(CTL('G')))
#endif /* HAVE_BEEP */

  /* here we account for the differences between BSD
   * and SYSV random number implementations
   */
#if HAVE_SRANDOM
# define SRND(x) srandom(x)
# define RND() random()
#else /* HAVE_SRANDOM */
# ifdef HAVE_SRAND48
#  define SRND(x) srand48(x)
#  define RND() lrand48()
# else
#  MUST_HAVE_SRANDOM_OR_SRAND48
# endif /* HAVE_SRAND48 */
#endif /* HAVE_SRANDOM */

#endif /* _COMMON_H */
