#ifndef _CONFIG_H
#define _CONFIG_H

/* Bleah.  glibc strikes again!  All I want is popen(), damn it. */
#ifdef __linux__
#define _GNU_SOURCE
#endif

#define __EXTENSIONS__

/* Include the autoconf-generated configuration header. */
#include "autoconf.h"

#endif
