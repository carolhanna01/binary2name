#ifndef DIRENTRY_H
#define DIRENTRY_H

#include <config.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>

#include "vfs.h"


#define LINK_FOLLOW 15
#define LINK_NO_FOLLOW -1

/* For vfs_s_find_entry */
#define FL_NONE 0
#define FL_MKDIR 1
#define FL_MKFILE 2
#define FL_DIR 4

/* For open_super */
#define FL_NO_OPEN 1

/* For vfs_s_entry_from_path */
#define FL_FOLLOW 1
#define FL_DIR 4

typedef struct vfs_s_entry {
    struct vfs_s_entry **prevp, *next;
    struct vfs_s_inode *dir;	/* Directory we are in - needed for invalidating directory when file in it changes */
    char *name;			/* Name of this entry */
    struct vfs_s_inode *ino;	/* ... and its inode */
    int magic;
#define ENTRY_MAGIC 0x014512563
} vfs_s_entry;

typedef struct vfs_s_inode {
    struct vfs_s_entry *subdir;
    struct vfs_s_super *super;
    struct stat st;		/* Parameters of this inode */
    char *linkname;		/* Symlink's contents */
    char *localname;		/* Filename of local file, if we have one */
    int flags;

    vfs_s_entry *ent;		/* ftp needs this backpointer; don't use if you can avoid it */

    union {
	struct {
	    long data_offset;
	} tar;
	struct {
	    struct timeval timestamp;
	    struct stat local_stat;
	} fish;
    } u;
    int magic;
#define INODE_MAGIC 0x93451656
} vfs_s_inode;

typedef struct vfs_s_super {
    struct vfs_s_super **prevp, *next;
    vfs *me;
    vfs_s_inode *root;
    char *name;		/* My name, whatever it means */
    int fd_usage;	/* Number of open files */
    int ino_usage;	/* Usage count of this superblock */
    int want_stale;	/* If set, we do not flush cache properly */

    union {
	struct {
	    int fd;
	    struct stat tarstat;
	} tar;
	struct {
	    int sockr, sockw;
	    char *home, *cwdir;
	    char *host, *user;
	    char *password;
	    int flags;
	} fish;
	struct {
	    int sockr, sockw;
	    char *home, *cwdir;
	    int isbinary;
	} ftp;
    } u;
    int magic;
#define SUPER_MAGIC 0x915ac312
} vfs_s_super;

typedef struct vfs_s_fh {
    struct vfs_s_inode *ino;
    long pos;			/* This is for module's use */
    int handle;	/* This is for module's use, but if != -1, will be mc_close()d */
    int changed; /* Did this file change? */
    int linear;	 /* Is that file open with O_LINEAR? */
    union {
	struct {
	    int got, total;
	} fish;
    } u;
    int magic;
#define FH_MAGIC 0x91324682
} vfs_s_fh;

struct vfs_s_data {
    struct vfs_s_super *supers;
    int inode_counter;
    dev_t rdev;
    FILE *logfile;

    int (*init_inode) (vfs *me, vfs_s_inode *ino);	/* optional */
    void (*free_inode) (vfs *me, vfs_s_inode *ino);	/* optional */
    int (*init_entry) (vfs *me, vfs_s_entry *entry);	/* optional */

    void* (*archive_check) (vfs *me, char *name, char *op);	/* optional */
    int (*archive_same) (vfs *me, vfs_s_super *psup, char *archive_name, char *op, void *cookie);
    int (*open_archive) (vfs *me, vfs_s_super *psup, char *archive_name, char *op);
    void (*free_archive) (vfs *me, vfs_s_super *psup);

    int (*fh_open) (vfs *me, vfs_s_fh *fh, int flags, int mode);
    int (*fh_close) (vfs *me, vfs_s_fh *fh);

    vfs_s_entry* (*find_entry) (vfs *me, vfs_s_inode *root, char *path, int follow, int flags);
    int (*dir_load) (vfs *me, vfs_s_inode *ino, char *path);
    int (*dir_uptodate) (vfs *me, vfs_s_inode *ino);
    int (*file_store) (vfs *me, vfs_s_super *super, char *path, char *localname);

    int (*linear_start) (vfs *me, vfs_s_fh *fh, int from);
    int (*linear_read) (vfs *me, vfs_s_fh *fh, void *buf, int len);
    void (*linear_close) (vfs *me, vfs_s_fh *fh);
};

/* entries and inodes */
vfs_s_inode *vfs_s_new_inode         (vfs *me, vfs_s_super *super,
				      struct stat *initstat);
vfs_s_entry *vfs_s_new_entry         (vfs *me, char *name, vfs_s_inode *inode);
void         vfs_s_free_entry        (vfs *me, vfs_s_entry *ent);
void         vfs_s_insert_entry      (vfs *me, vfs_s_inode *dir,
				      vfs_s_entry *ent);
struct stat *vfs_s_default_stat      (vfs *me, mode_t mode);

void         vfs_s_add_dots          (vfs *me, vfs_s_inode *dir,
				      vfs_s_inode *parent);
vfs_s_entry *vfs_s_generate_entry    (vfs *me, char *name,
				      struct vfs_s_inode *parent, mode_t mode);
vfs_s_entry *vfs_s_automake          (vfs *me, vfs_s_inode *dir, char *path,
				      int flags);
vfs_s_entry *vfs_s_find_entry_tree   (vfs *me, vfs_s_inode *root, char *path,
				      int follow, int flags);
vfs_s_entry *vfs_s_find_entry_linear (vfs *me, vfs_s_inode *root, char *path,
				      int follow, int flags);
vfs_s_inode *vfs_s_find_inode        (vfs *me, vfs_s_inode *root, char *path,
				      int follow, int flags);
vfs_s_inode *vfs_s_find_root         (vfs *me, vfs_s_entry *entry);
vfs_s_entry *vfs_s_resolve_symlink   (vfs *me,  vfs_s_entry *entry, char *path,
				      int follow);

/* superblock games */
vfs_s_super *vfs_s_new_super         (vfs *me);
void vfs_s_free_super                (vfs *me, vfs_s_super *super); 

/* outside interface */
char *vfs_s_get_path_mangle          (vfs *me, char *inname, vfs_s_super **archive,
				      int flags);
char *vfs_s_get_path                 (vfs *me, char *inname, vfs_s_super **archive,
				      int flags);
void  vfs_s_invalidate               (vfs *me, vfs_s_super *super);
char *vfs_s_fullpath                 (vfs *me, vfs_s_inode *ino);

/* readdir & friends */
vfs_s_super *vfs_s_super_from_path   (vfs *me, char *name);
vfs_s_inode *vfs_s_inode_from_path   (vfs *me, char *name, int flags);
void *vfs_s_opendir (vfs *me, char *dirname);
void *vfs_s_readdir (void *data);
int vfs_s_telldir (void *data);
void vfs_s_seekdir (void *data, int offset);
int vfs_s_closedir (void *data);
int vfs_s_chdir (vfs *me, char *path);

/* stat & friends */
int vfs_s_stat (vfs *me, char *path, struct stat *buf);
int vfs_s_lstat (vfs *me, char *path, struct stat *buf);
int vfs_s_fstat (void *fh, struct stat *buf);
int vfs_s_readlink (vfs *me, char *path, char *buf, int size);
void *vfs_s_open (vfs *me, char *file, int flags, int mode);
int vfs_s_read (void *fh, char *buffer, int count);
int vfs_s_write (void *fh, char *buffer, int count);
int vfs_s_lseek (void *fh, off_t offset, int whence);
int vfs_s_close (void *fh);

/* mc support */
void vfs_s_fill_names (vfs *me, void (*func)(char *));
int vfs_s_ferrno(vfs *me);
void vfs_s_dump(vfs *me, char *prefix, vfs_s_inode *ino);
char *vfs_s_getlocalcopy (vfs *me, char *path);

/* stamping support */
vfsid vfs_s_getid (vfs *me, char *path, struct vfs_stamping **parent);
int vfs_s_nothingisopen (vfsid id);
void vfs_s_free (vfsid id);
int vfs_s_setctl (vfs *me, char *path, int ctlop, char *arg);

/* network filesystems support */
int vfs_s_select_on_two (int fd1, int fd2);
int vfs_s_get_line (vfs *me, int sock, char *buf, int buf_len, char term);
int vfs_s_get_line_interruptible (vfs *me, char *buffer, int size, int fd);

/* If non-null, FREE */
#define ifree(ptr) do { if (ptr) g_free(ptr); } while (0)

#define MEDATA ((struct vfs_s_data *) me->data)
#define ERRNOR(a, b) do { me->verrno = a; return b; } while (0)
#define FH ((struct vfs_s_fh *) fh)
#define FH_SUPER FH->ino->super

#define LS_NOT_LINEAR 0
#define LS_LINEAR_CLOSED 1
#define LS_LINEAR_OPEN 2

#endif
