#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifndef HAVE_STRNCOLL

#include <string.h>
#include <clib.h>

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif
#include <stdlib.h>
#ifndef HAVE_ALLOCA_H
char *alloca ();
#endif
int strncoll(const char* a, const char* b, int len)
{
  char* tmpa =  (char*) malloc (len+1);
  char* tmpb = (char*) malloc (len+1);
  memcpy(tmpa, a, len);
  tmpa[len] = '\0';
  memcpy(tmpb, b, len);
  tmpb[len] = '\0';
  int lose=strcoll(tmpa, tmpb);
  free(tmpa); 
  free(tmpb);
  return lose;
}

#endif /* HAVE_STRNCOLL */
