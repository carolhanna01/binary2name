/* Code posted on the GNU autoconf mailinglist by Philip Willoughby,
   30/08/2002, modified 04/03/2004 Pascal Haakmat */

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/*
 * Returns the number of CPUs or 0 if it couldn't be determined.
 */

int cpucount() {
    long nprocs = -1;
    long nprocs_max = -1;
#ifdef _WIN32
#ifndef _SC_NPROCESSORS_ONLN
    SYSTEM_INFO info;
    GetSystemInfo(&info);
#define sysconf(a) info.dwNumberOfProcessors
#define _SC_NPROCESSORS_ONLN
#endif
#endif
#ifdef _SC_NPROCESSORS_ONLN
    nprocs = sysconf(_SC_NPROCESSORS_ONLN);
    if (nprocs < 1)
        {
            fprintf(stderr, "Could not determine number of CPUs online:\n%s\n",
                    strerror (errno));
            return 0;
        }
    nprocs_max = sysconf(_SC_NPROCESSORS_CONF);
    if (nprocs_max < 1)
        {
            fprintf(stderr, "Could not determine number of CPUs configured:\n%s\n",
                    strerror (errno));
            return 0;
        }
    return nprocs_max;
#else
    fprintf(stderr, "Could not determine number of CPUs");
    return 0;
#endif
}
