/*
 Part of the ht://Dig package   <http://www.htdig.org/>
 Copyright (c) 1999, 2000, 2001 The ht://Dig Group
 For copyright details, see the file COPYING in your distribution
 or the GNU General Public License version 2 or later
 <http://www.gnu.org/copyleft/gpl.html>
*/
#ifndef _clib_h_
#define _clib_h_

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef HAVE_GETCWD
char *getcwd(char *, size_t);
#endif

#ifndef HAVE_MEMCMP
int memcmp(const void *, const void *, size_t);
#endif

#ifndef HAVE_MEMCPY
void *memcpy(void *, const void *, size_t);
#endif

#ifndef HAVE_MEMMOVE
void *memmove(void *, const void *, size_t);
#endif

#ifndef HAVE_RAISE
int raise (int);
#endif

#ifndef HAVE_SNPRINTF
int snprintf(char *, size_t, const char *, ...);
#endif

#ifndef HAVE_STRERROR
char *strerror(int);
#endif

#ifndef HAVE_VSNPRINTF
int vsnprintf(char *, size_t, const char *, ...);
#endif

#ifndef HAVE_STRNCOLL
int strncoll(const char* a, const char* b, int len);
#endif

#ifndef HAVE_STRCASESTR
const char *strcasestr(const char *s, const char *pattern);
#endif

#ifndef HAVE_STRCASECMP
int strcasecmp(const char *str1, const char *str2);
#endif

#ifndef HAVE_STRNCASECMP
int strncasecmp(const char *str1, const char *str2);
#endif

#ifdef __cplusplus
}
#endif

#endif /* _clib_h_ */
