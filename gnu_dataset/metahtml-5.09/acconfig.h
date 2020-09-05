/* acconfig.h: -*- C -*-  For "autoheader".. */

/*  Copyright (c) 1996 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Thu Aug 22 18:09:27 1996.  */

/* Defined every time by configure. */
#undef MHTML_SYSTEM_TYPE

/* Defined on those systems for which ODBC compilation with the UDBC client
   is desired. */
#undef OPENLINK_UDBC_CLIENT

/* Defined only when compiing on some brand of Solaris system. */
#undef Solaris

/* Defined when your system has a define or typedef for sig_t. */
#undef HAVE_TYPE_SIG_T

/* Defined when your system has a define or typedef for time_t. */
#undef HAVE_TYPE_TIME_T

/* Defined when the system has a working dlopen () call. */
#undef HAVE_DLOPEN

/* Defined when the system has a working shl_load () call instead. */
#undef USE_SHL_LOAD

/* Defined when the system has cbrt() in libm. */
#undef HAVE_CBRT

@BOTTOM@
#if defined (Solaris)
#  include <arch/solaris/prototypes.h>
#endif

#if defined (HAVE_TIME_H)
#  define METAHTML_PROFILER 1
#endif

#if defined (HAVE_LIBCRYPT) && (!defined (HAVE_CRYPT))
#  define HAVE_CRYPT 1
#endif

