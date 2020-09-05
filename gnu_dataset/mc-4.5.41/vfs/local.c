#include <config.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <fcntl.h>

#include "utilvfs.h"

#include "vfs.h"
#include "local.h"

/* Note: Some of this functions are not static. This has rather good
 * reason: exactly same functions would have to appear in sfs.c. This
 * saves both computer's memory and my work.  <pavel@ucw.cz>
 * */
    
static void *
local_open (vfs *me, char *file, int flags, int mode)
{
    int *local_info;
    int fd;

    fd = open (file, NO_LINEAR(flags), mode);
    if (fd == -1)
	return 0;

    local_info = g_new (int, 1);
    *local_info = fd;
    
    return local_info;
}

int
local_read (void *data, char *buffer, int count)
{
    int n;

    if (!data)
	return -1;
    
    while ((n = read (*((int *) data), buffer, count)) == -1){
#ifdef EAGAIN
	if (errno == EAGAIN) continue;
#endif
#ifdef EINTR
	if (errno == EINTR) continue;
#endif
	return -1;
    }
    return n;
}

int
local_close (void *data)
{
    int fd;

    if (!data)
	return -1;
    
    fd =  *(int *) data;
    g_free (data);
    return close (fd);
}

int
local_errno (vfs *me)
{
    return errno;
}

static void *
local_opendir (vfs *me, char *dirname)
{
    DIR **local_info;
    DIR *dir;

    dir = opendir (dirname);
    if (!dir)
	return 0;

    local_info = (DIR **) g_new (DIR *, 1);
    *local_info = dir;
    
    return local_info;
}

static int
local_telldir (void *data)
{
    return telldir( *(DIR **) data );
}

static void
local_seekdir (void *data, int offset)
{
    seekdir( *(DIR **) data, offset );
}

static void *
local_readdir (void *data)
{
    return readdir (*(DIR **) data);
}

static int
local_closedir (void *data)
{
    int i;

    i = closedir (* (DIR **) data);
    if (data)
	g_free (data);
    return i;
}

static int
local_stat (vfs *me, char *path, struct stat *buf)
{
    return stat (path, buf);
}

static int
local_lstat (vfs *me, char *path, struct stat *buf)
{
#ifndef HAVE_STATLSTAT
    return lstat (path,buf);
#else
    return statlstat (path, buf);
#endif
}

int
local_fstat (void *data, struct stat *buf)
{
    return fstat (*((int *) data), buf);    
}

static int
local_chmod (vfs *me, char *path, int mode)
{
    return chmod (path, mode);
}

static int
local_chown (vfs *me, char *path, int owner, int group)
{
    return chown (path, owner, group);
}

static int
local_utime (vfs *me, char *path, struct utimbuf *times)
{
    return utime (path, times);
}

static int
local_readlink (vfs *me, char *path, char *buf, int size)
{
    return readlink (path, buf, size);
}

static int
local_unlink (vfs *me, char *path)
{
    return unlink (path);
}

static int
local_symlink (vfs *me, char *n1, char *n2)
{
    return symlink (n1, n2);
}

static int
local_write (void *data, char *buf, int nbyte)
{
    int fd;
    int n;

    if (!data)
	return -1;
    
    fd = * (int *) data;
    while ((n = write (fd, buf, nbyte)) == -1){
#ifdef EAGAIN
	if (errno == EAGAIN) continue;
#endif
#ifdef EINTR
	if (errno == EINTR) continue;
#endif
	break;
    }
    return n;
}

static int
local_rename (vfs *me, char *a, char *b)
{
    return rename (a, b);
}

static int
local_chdir (vfs *me, char *path)
{
    return chdir (path);
}

int
local_lseek (void *data, off_t offset, int whence)
{
    int fd = * (int *) data;

    return lseek (fd, offset, whence);
}

static int
local_mknod (vfs *me, char *path, int mode, int dev)
{
    return mknod (path, mode, dev);
}

static int
local_link (vfs *me, char *p1, char *p2)
{
    return link (p1, p2);
}

static int
local_mkdir (vfs *me, char *path, mode_t mode)
{
    return mkdir (path, mode);
}

static int
local_rmdir (vfs *me, char *path)
{
    return rmdir (path);
}

static vfsid
local_getid (vfs *me, char *path, struct vfs_stamping **parent)
{
    *parent = NULL;
    return (vfsid) -1; /* We do not free local fs stuff at all */
}

static int
local_nothingisopen (vfsid id)
{
    return 0;
}

static void
local_free (vfsid id)
{
}

static char *
local_getlocalcopy (vfs *me, char *path)
{
    return g_strdup (path);
}

static void
local_ungetlocalcopy (vfs *me, char *path, char *local, int has_changed)
{
}

#ifdef HAVE_MMAP
caddr_t
local_mmap (vfs *me, caddr_t addr, size_t len, int prot, int flags, void *data, off_t offset)
{
    int fd = * (int *)data;

    return mmap (addr, len, prot, flags, fd, offset);
}

int
local_munmap (vfs *me, caddr_t addr, size_t len, void *data)
{
    return munmap (addr, len);
}
#endif

static int
local_which (vfs *me, char *path)
{
    return 0;		/* Every path which other systems do not like is expected to be ours */
}

vfs vfs_local_ops = {
    NULL,	/* This is place of next pointer */
    "Local filesystem",
    0,		/* flags */
    NULL,	/* prefix */
    NULL,	/* data */
    0,		/* errno */
    NULL,
    NULL,
    NULL,
    local_which,

    local_open,
    local_close,
    local_read,
    local_write,
    
    local_opendir,
    local_readdir,
    local_closedir,
    local_telldir,
    local_seekdir,

    local_stat,
    local_lstat,
    local_fstat,

    local_chmod,
    local_chown,
    local_utime,

    local_readlink,
    local_symlink,
    local_link,
    local_unlink,

    local_rename,
    local_chdir,
    local_errno,
    local_lseek,
    local_mknod,
    
    local_getid,
    local_nothingisopen,
    local_free,
    
    local_getlocalcopy,
    local_ungetlocalcopy,
    
    local_mkdir,
    local_rmdir,
    
    NULL,
    NULL
#ifdef HAVE_MMAP
    ,local_mmap,
    local_munmap
#endif
};
