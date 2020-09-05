/* files.h.in
 *
 * file locations
 *
 */

#ifndef FILES_H
#define FILES_H

#include "config.h"

#include <sys/types.h>

#ifdef LINUX_MULTIFORMAT
# include "linux-acct.h"
#else
# include <stdint.h> /* GNU/kFreeBSD */
# include <sys/acct.h>
# if defined __FreeBSD__ || defined __FreeBSD_kernel__
#  include <osreldate.h>
#  if defined __FreeBSD_kernel__
#   define __FreeBSD_version __FreeBSD_kernel_version
#  endif /* __FreeBSD_kernel__ */
#  if __FreeBSD_version >= 700100 /* FreeBSD 7.0-STABLE */
#   define acct acctv2
#   define ac_flag ac_flagx
#  endif
# endif
#endif

#include <utmp.h>

#define WTMP_FILE_LOC "/var/log/wtmp"
#define ACCT_FILE_LOC "/var/log/account/pacct"
#define SAVACCT_FILE_LOC "/var/log/account/savacct"
#define USRACCT_FILE_LOC "/var/log/account/usracct"

/* Workaround for a kernel includes problem */
#if defined(__linux__) && defined(__alpha__)
#undef AHZ
#define AHZ 1024
#endif

#ifndef AHZ
#define AHZ 64
#endif

#endif /* ! FILES_H */
