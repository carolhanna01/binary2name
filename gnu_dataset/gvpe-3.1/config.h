/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */


#ifndef CONFIG_H__
#define CONFIG_H__

#ifdef __cplusplus
   using namespace std;
#endif



/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Select the auth digest algorithm to use. */
#define ENABLE_AUTH EVP_sha512

/* Select the symmetric cipher to use. */
#define ENABLE_CIPHER EVP_aes_128_ctr

/* Enable compression support. */
#define ENABLE_COMPRESSION 1

/* DNS tunnel protocol support. */
/* #undef ENABLE_DNS */

/* Select the HMAC digest algorithm to use. */
#define ENABLE_HMAC EVP_sha1

/* http proxy connect support. */
#define ENABLE_HTTP_PROXY 1

/* ICMP protocol support. */
#define ENABLE_ICMP 1

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
#define ENABLE_NLS 1

/* POSIX thread support. */
#define ENABLE_PTHREADS 1

/* TCP protocol support. */
#define ENABLE_TCP 1

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <arpa/inet.h> header file. */
#define HAVE_ARPA_INET_H 1

/* Define to 1 if you have the `asprintf' function. */
#define HAVE_ASPRINTF 1

/* Define to 1 if you have the <clocale> header file. */
#define HAVE_CLOCALE 1

/* Define to 1 if you have the `clock_gettime' function. */
#define HAVE_CLOCK_GETTIME 1

/* Define to 1 to use the syscall interface for clock_gettime */
/* #undef HAVE_CLOCK_SYSCALL */

/* Cygwin */
/* #undef HAVE_CYGWIN */

/* Define to 1 if you have the `daemon' function. */
#define HAVE_DAEMON 1

/* Darwin (MacOS/X) */
/* #undef HAVE_DARWIN */

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
#define HAVE_DCGETTEXT 1

/* Define to 1 if you have the `epoll_ctl' function. */
#define HAVE_EPOLL_CTL 1

/* Define to 1 if you have the `eventfd' function. */
#define HAVE_EVENTFD 1

/* Define to 1 if you have the <ext/hash_map> header file. */
#define HAVE_EXT_HASH_MAP 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if the floor function is available */
#define HAVE_FLOOR 1

/* FreeBSD */
/* #undef HAVE_FREEBSD */

/* Define if the GNU gettext() function is already present or preinstalled. */
#define HAVE_GETTEXT 1

/* Define to 1 if you have the `get_current_dir_name' function. */
#define HAVE_GET_CURRENT_DIR_NAME 1

/* Define if you have the iconv() function. */
/* #undef HAVE_ICONV */

/* Define to 1 if you have the `inotify_init' function. */
#define HAVE_INOTIFY_INIT 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `kqueue' function. */
/* #undef HAVE_KQUEUE */

/* Define to 1 if you have the `gmp' library (-lgmp). */
/* #undef HAVE_LIBGMP */

/* Define to 1 if you have the `nsl' library (-lnsl). */
/* #undef HAVE_LIBNSL */

/* Define to 1 if you have the `rt' library (-lrt). */
/* #undef HAVE_LIBRT */

/* Define to 1 if you have the `socket' library (-lsocket). */
/* #undef HAVE_LIBSOCKET */

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Linux */
#define HAVE_LINUX 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mlockall' function. */
#define HAVE_MLOCKALL 1

/* Define to 1 if you have the `nanosleep' function. */
#define HAVE_NANOSLEEP 1

/* NetBSD */
/* #undef HAVE_NETBSD */

/* Define to 1 if you have the <netinet/in.h> header file. */
#define HAVE_NETINET_IN_H 1

/* Define to 1 if you have the <netinet/in_systm.h> header file. */
#define HAVE_NETINET_IN_SYSTM_H 1

/* Define to 1 if you have the <netinet/ip.h> header file. */
#define HAVE_NETINET_IP_H 1

/* Define to 1 if you have the <netinet/tcp.h> header file. */
#define HAVE_NETINET_TCP_H 1

/* Define to 1 if you have the <net/ethernet.h> header file. */
#define HAVE_NET_ETHERNET_H 1

/* Define to 1 if you have the <net/if.h> header file. */
#define HAVE_NET_IF_H 1

/* OpenBSD */
/* #undef HAVE_OPENBSD */

/* Define to 1 if you have the `poll' function. */
#define HAVE_POLL 1

/* Define to 1 if you have the <poll.h> header file. */
#define HAVE_POLL_H 1

/* Define to 1 if you have the `port_create' function. */
/* #undef HAVE_PORT_CREATE */

/* Define to 1 if you have the <port.h> header file. */
/* #undef HAVE_PORT_H */

/* Define to 1 if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* Define to 1 if you have the `select' function. */
#define HAVE_SELECT 1

/* Define to 1 if you have the `signalfd' function. */
#define HAVE_SIGNALFD 1

/* socklen_t available */
#define HAVE_SOCKLEN_T 1

/* Solaris/SunOS */
/* #undef HAVE_SOLARIS */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strsignal' function. */
#define HAVE_STRSIGNAL 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* struct addrinfo available */
#define HAVE_STRUCT_ADDRINFO 1

/* Define to 1 if you have the <syslog.h> header file. */
#define HAVE_SYSLOG_H 1

/* Define to 1 if you have the <sys/cygwin.h> header file. */
/* #undef HAVE_SYS_CYGWIN_H */

/* Define to 1 if you have the <sys/epoll.h> header file. */
#define HAVE_SYS_EPOLL_H 1

/* Define to 1 if you have the <sys/eventfd.h> header file. */
#define HAVE_SYS_EVENTFD_H 1

/* Define to 1 if you have the <sys/event.h> header file. */
/* #undef HAVE_SYS_EVENT_H */

/* Define to 1 if you have the <sys/file.h> header file. */
#define HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/inotify.h> header file. */
#define HAVE_SYS_INOTIFY_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/mman.h> header file. */
#define HAVE_SYS_MMAN_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/signalfd.h> header file. */
#define HAVE_SYS_SIGNALFD_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <tr1/unordered_map> header file. */
#define HAVE_TR1_UNORDERED_MAP 1

/* Universal tun/tap driver present */
#define HAVE_TUNTAP 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `unsetenv' function. */
#define HAVE_UNSETENV 1

/* Size of HMAC in each packet in bytes. */
#define HMACLENGTH 12

/* kernel interface subtype */
#define IFSUBTYPE "linux"

/* kernel interface type */
#define IFTYPE "native"

/* Location of if_tun.h */
/* #undef LINUX_IF_TUN_H */

/* Maximum MTU supported. */
#define MAX_MTU (1500 + 14)

/* Name of package */
#define PACKAGE "gvpe"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "gvpe"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "gvpe 3.1"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "gvpe"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "3.1"

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Size of RSA keys. */
#define RSABITS 3072

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Version number of package */
#define VERSION "3.1"

/* Enable GNU extensions */
#define _GNU_SOURCE 1

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */


typedef unsigned char u8;
typedef signed char s8;

#if __CYGWIN__

typedef unsigned short u16;
typedef unsigned int u32;
typedef signed short s16;
typedef signed int s32;

#else
#include <inttypes.h>

/* old modula-2 habits */
typedef uint16_t u16;
typedef uint32_t u32;
typedef int16_t s16;
typedef int32_t s32;
#endif

#endif

#if HAVE_CLOCALE
# define CLOCALE <clocale>
#else
# define CLOCALE <locale.h>
#endif

