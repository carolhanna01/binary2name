/* 
 * fcgios.h --
 *
 *      Description of file.
 *
 *
 *  Copyright (c) 1996 Open Market, Inc.
 *  All rights reserved.
 *
 *  This file contains proprietary and confidential information and
 *  remains the unpublished property of Open Market, Inc. Use, 
 *  disclosure, or reproduction is prohibited except as permitted by 
 *  express written license agreement with Open Market, Inc. 
 *
 *  Bill Snapper
 *  snapper@openmarket.com
 */
#ifndef _FCGIOS_H
#define _FCGIOS_H

#if defined (HAVE_CONFIG_H)
#  include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>
#if defined (HAVE_SYS_FILE_H)
#  include <sys/file.h>
#endif

#if defined (HAVE_FCNTL_H)
#  include <fcntl.h>
#else
#  if defined (HAVE_SYS_FCNTL_H)
#    include <sys/fcntl.h>
#  endif
#endif
#if defined (HAVE_BSTRING_H)
#  include <bstring.h>
#endif
#include <sys/ioctl.h>
#if defined (Solaris) && defined (HAVE_SYS_TTOLD_H)
#  include <sys/ttold.h>
#endif

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#if defined (__WINNT__)		/* CYGWIN defines sig_t in netdb.h. */
#  if !defined (HAVE_TYPE_SIG_T)
#    define HAVE_TYPE_SIG_T 1
#  endif
#endif

#include <sys/wait.h>

#if defined (TIME_WITH_SYS_TIME)
#  include <sys/time.h>
#  include <time.h>
#else
#  if defined (HAVE_SYS_TIME_H)
#    include <sys/time.h>
#  else
#    if defined (HAVE_TIME_H)
#      include <time.h>
#    endif
#  endif
#endif

#include <sys/stat.h>

#if defined (HAVE_DIRENT_H)
#  include <dirent.h>
#  define D_NAMELEN(x) strlen((x)->d_name)
#else
#  define dirent direct
#  define D_NAMELEN(dirent) (dirent)->d_namlen
#  if defined (HAVE_SYS_NDIR_H)
#    include <sys/ndir.h>
#  endif
#  if defined (HAVE_SYS_DIR_H)
#    include <sys/dir.h>
#  endif
#  if defined (HAVE_NDIR_H)
#    include <ndir.h>
#  endif
#endif

#if defined (NOTDEF)
    #if defined (sgi) || defined (Solaris)
    #define d_namlen d_reclen
    #endif

    #if defined (__linux__) && !defined (d_namlen)
    #define d_namlen d_reclen
    #endif

    #if defined (__WINNT__)
    #  define D_NAMELEN(x) strlen ((x)->d_name)
    #else
    #  define D_NAMELEN(x) (x)->d_namlen
    #endif
#endif /* NOTDEF */

#ifdef _WIN32
#define OS_Errno GetLastError()
#define OS_SetErrno(err) SetLastError(err)
#ifndef DLLAPI
#define DLLAPI __declspec(dllimport)
#endif
#else
#define DLLAPI
#define OS_Errno errno
#define OS_SetErrno(err) errno = (err)
#endif

#ifdef _WIN32
#include <io.h>
#endif

#ifndef STDIN_FILENO
#define STDIN_FILENO  0
#endif


#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifndef X_OK
#define X_OK       0x01
#endif

#ifdef _WIN32
#ifndef O_NONBLOCK
#define O_NONBLOCK     0x0004  /* no delay */
#endif
#endif

#if defined (c_plusplus) || defined (__cplusplus)
extern "C" {
#endif

#ifndef _CLIENTDATA
#   if defined(__STDC__) || defined(__cplusplus)
    typedef void *ClientData;
#   else
    typedef int *ClientData;
#   endif /* __STDC__ */
#define _CLIENTDATA
#endif

typedef void (*OS_AsyncProc) (ClientData clientData, int len);

DLLAPI int OS_LibInit(int stdioFds[3]);
DLLAPI void OS_LibShutdown(void);
DLLAPI int OS_CreateLocalIpcFd(char *bindPath);
DLLAPI int OS_FcgiConnect(char *bindPath);
DLLAPI int OS_Read(int fd, char * buf, size_t len);
DLLAPI int OS_Write(int fd, char * buf, size_t len);
DLLAPI int OS_SpawnChild(char *execPath, int listenFd);
DLLAPI int OS_AsyncReadStdin(void *buf, int len, OS_AsyncProc procPtr, 
                      ClientData clientData);
DLLAPI int OS_AsyncRead(int fd, int offset, void *buf, int len,
		 OS_AsyncProc procPtr, ClientData clientData);
DLLAPI int OS_AsyncWrite(int fd, int offset, void *buf, int len, 
		  OS_AsyncProc procPtr, ClientData clientData);
DLLAPI int OS_Close(int fd);
DLLAPI int OS_CloseRead(int fd);
DLLAPI int OS_DoIo(struct timeval *tmo);
DLLAPI int OS_FcgiIpcAccept(char *clientAddrList);
DLLAPI int OS_IpcClose(int ipcFd);
DLLAPI int OS_IsFcgi(void);
DLLAPI void OS_SetFlags(int fd, int flags);

#if defined (__cplusplus) || defined (c_plusplus)
} /* terminate extern "C" { */
#endif

#endif /* _FCGIOS_H */
