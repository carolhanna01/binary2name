/*  otar.c - read and write tar headers
 */
   
/*
   Copyright (C) 1985, 1992, 1993, 1994, 1996, 1997, 1999, 2000, 2001, 
   2003, 2004 Free Software Foundation, Inc.
   Copyright (C) 2003,2004,2005,2010 Jim Lowe

   Portions of this code are derived from code 
   copyrighted by the Free Software Foundation.  Retention of their 
   copyright ownership is required by the GNU GPL and does *NOT* signify 
   their support or endorsement of this work.
       jhl

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#define FILENEEDDEBUG 1
#undef FILENEEDDEBUG

#include "swuser_config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>
#include <time.h>
#include <tar.h>
#include "system.h"
#include "swlib.h"
#include "to_oct.h"
#include "filetypes.h"
#include "cpiohdr.h"
#include "tarhdr.h"
#include "ahs.h"
#include "taru.h"
#include "uxfio.h"
#include "swutilname.h"

#define TARRECORDSIZE 512
#define FALSE 0
#define TRUE 1

#ifndef bzero
#define bzero(s, n)     memset ((s), 0, (n))
#endif

#define CHKBLANKS       "        "      /* 8 blanks, no null */

/* Return nonzero iff all the bytes in BLOCK are NUL.
   SIZE is the number of bytes to check in BLOCK; it must be a
   multiple of sizeof (long).  */

#define NOBODY_ID		((uid_t)(07777777))
#define CACHE_LEN		10
#define SNAME_LEN		32

static int dbpair_user_nset = 0;
static int dbpair_group_nset = 0;

static char * g_pwent_msg[] = {"user", "group"};

typedef struct {
	int idM;
	char nameM[TARU_SYSDBNAME_LEN];
	int in_sysM;
} SYSDBPAIR;

static SYSDBPAIR userCacheM[CACHE_LEN];
static SYSDBPAIR groupCacheM[CACHE_LEN];

#include "debug_config.h"
/*
#define TARNEEDDEBUG 1
#undef TARNEEDDEBUG

#ifdef TARNEEDDEBUG
#define TAR_E_DEBUG(format) SWBISERROR("TAR DEBUG: ", format)
#define TAR_E_DEBUG2(format, arg) SWBISERROR2("TAR DEBUG: ", format, arg)
#define TAR_E_DEBUG3(format, arg, arg1) SWBISERROR3("TAR DEBUG: ", format, arg, arg1)
#else
#define TAR_E_DEBUG(arg)
#define TAR_E_DEBUG2(arg, arg1)
#define TAR_E_DEBUG3(arg, arg1, arg2)
#endif
*/

static size_t
split_long_name (const char *name, size_t length)
{
  size_t i;

  if (length > PREFIX_FIELD_SIZE)
    length = PREFIX_FIELD_SIZE+1;
  for (i = length - 1; i > 0; i--)
    if (ISSLASH (name[i]))
      break;
  return i;
}

static
void
error_msg_control(char * name, int id, char * dbname)
{
	static STROB * tmp = NULL;
	static STROB * store = NULL;
	if (tmp == NULL) tmp = strob_open(80);
	if (store == NULL) store = strob_open(80);

	if (name) {
		strob_sprintf(tmp, 0, "%s: %s name [%s] not found in system db\n",
					 swlib_utilname_get(), dbname, name);
	} else {
		strob_sprintf(tmp, 0,
			"%s: %s id [%d] not found in system db\n",
				swlib_utilname_get(), dbname, (int)(id));
	}
	if (strstr(strob_str(store), strob_str(tmp))) {
		;
	} else {
		strob_strcat(store, strob_str(tmp));	
		fprintf(stderr, "%s", strob_str(tmp));
	}	
}

static 
int 
null_block(long *block, int size)
{
	register long *p = block;
	register int i = size / sizeof(long);

	while (i--)
		if (*p++)
			return 0;
	return 1;
}

static
void 
dbcache_set(int * dbpair_plen, SYSDBPAIR * cache, char * name, int id, int in_sys)
{
	int cell;
	if (*dbpair_plen >= CACHE_LEN) {
		*dbpair_plen = 1;
		cell = 0;
	} else {
		cell = *dbpair_plen;
		(*dbpair_plen)++;
	}	
	cache[cell].idM = id;
	strncpy(cache[cell].nameM, name, TARU_SYSDBNAME_LEN -1);
	cache[cell].nameM[TARU_SYSDBNAME_LEN -1] = '\0';
	cache[cell].in_sysM = in_sys;
}

static
SYSDBPAIR * 
dbcache_lookup(int * dbpair_len, SYSDBPAIR * cache, char * name, int id)
{
	int i;
	SYSDBPAIR * pair;

	for(i=0; i < *dbpair_len; i++) {
		pair = cache + i;
		if (name && strcmp(name, pair->nameM) == 0) {
			return pair;
		}
		if (id >= 0 && pair->idM == id) {
			return pair;
		}
	}
	return NULL;
}

static
char *
return_name_by_cache(int * dbpair_len, SYSDBPAIR * cache, int id, int * is_in_sysdb)
{
	SYSDBPAIR * pair;
	pair = dbcache_lookup(dbpair_len, cache, NULL, id);
	if (!pair) return NULL;
	*is_in_sysdb = pair->in_sysM;
	return pair->nameM;
}


static
int 
return_id_by_cache(int * dbpair_len, SYSDBPAIR * cache, char * name, int * is_in_sysdb)
{
	SYSDBPAIR * pair;
	pair = dbcache_lookup(dbpair_len, cache, name, -1);
	if (!pair) return -1;
	*is_in_sysdb = pair->in_sysM;
	return pair->idM;
}

static 
char *
l_tar_idcache_getuser(long uid, char *name)
{
	struct passwd *pwent;
	pwent = getpwuid(uid);
	if (!pwent) {
		*name = '\0';
		return (char *) NULL;
	}
	strncpy(name, pwent->pw_name, SNAME_LEN);
	name[31]='\0';
	return name;
}

static 
int 
l_cache_getuidbyname(char *user, long *puid)
{
	int ret;
	uid_t id;
	ret = taru_get_uid_by_name(user, &id);
	*puid = (long)id;
	return ret;
}

static 
int 
l_getuidbyname(char *user, long *puid)
{
	struct passwd *pwent;
	pwent = getpwnam(user);
	if (!pwent) {
		return -1;
	}
	*puid = pwent->pw_uid;
	return 0;
}

static 
char *
l_tar_idcache_getgroup(long gid, char *name)
{
	struct group *pwent;
	pwent = getgrgid(gid);
	if (!pwent) {
		*name ='\0';
		return (char *) NULL;
	}
	strncpy(name, pwent->gr_name, SNAME_LEN);
	name[31]='\0';
	return name;
}

static 
int 
l_cache_getgidbyname(char *user, long *pgid)
{
	int ret;
	gid_t id;
	ret = taru_get_gid_by_name(user, &id);
	*pgid = (long)id;
	return ret;
}

static 
int 
l_getgidbyname(char *group, long *pgid)
{
	struct group *pwent;
	pwent = getgrnam(group);
	if (!pwent) {
		return -1;
	}
	*pgid = pwent->gr_gid;
	return 0;
}

static int 
get_pwent(
		int idx,
		long id, 
		char *userkey, 
		char *buf, 
		long *pid, 
		int  (*v_get_id)(char *, long *),
		char * (*v_get_name)(long, char *),
		int * dbpair_len, 
		SYSDBPAIR * cache
	)
{
	int is_in_sysdb = 0;
	if (userkey) {
		int n;
		if (strlen(userkey) == 0) return -1;
		n = return_id_by_cache(dbpair_len, cache, userkey, &is_in_sysdb);
		if (n >= 0) {
			/*
			 * The lookup was found in cache but it
			 * may not be in /etc/passwd or /etc/group
			 * i.e. The user is looking up a username that
			 * is not in the system data files.
			 */
			*pid = n;
			return is_in_sysdb;
		}	
		n = (*(v_get_id))(userkey, pid);
		if (n == 0) {
			/*
			 * found in system database
			 */
			dbcache_set(dbpair_len, cache, userkey, *pid, 0 /*in sys db*/);
		} else {
			/*
			 * Not found
			 */
			*pid = AHS_UID_NOBODY;
			error_msg_control(userkey, -1, g_pwent_msg[idx]);
			dbcache_set(dbpair_len, cache, userkey, *pid, -1 /*not in sys db*/);
			return -1;
		}
		return n;
	} else {
		char * cname;
		cname = return_name_by_cache(dbpair_len, cache, id, &is_in_sysdb);
		if (cname) {
			if (strlen(cname) >  TARU_SYSDBNAME_LEN - 1) {
				fprintf(stderr, "%s: user name too long for ustar headers: %s\n", swlib_utilname_get(), cname);

			}
			strncpy(buf, cname, TARU_SYSDBNAME_LEN - 1);
			buf[TARU_SYSDBNAME_LEN - 1] = '\0';
			return is_in_sysdb;
		}
		if ((*(v_get_name))(id, buf)) {
			/*
			 * found in system database
			 */
			dbcache_set(dbpair_len, cache, buf, id, 0);
			return 0;
		} else {
			/*
			 * Not found
			 */
			strncpy(buf, AHS_USERNAME_NOBODY, 10);
			dbcache_set(dbpair_len, cache, buf, id, -1);
			error_msg_control((char*)NULL, id, g_pwent_msg[idx]);
			return -1;
		}
	}
}

/* ============================================================ */
/* ================  Public Routines  ========================= */
/* ============================================================ */

void
taru_mode_to_chars(mode_t v, char *p, size_t s, int termch)
{
	mode_to_chars(v, p, s, POSIX_FORMAT, termch);
}

int 
taru_get_uid_by_name(char *username, uid_t *puid)
{
	long pid;
	int ret;
	E_DEBUG("");
	ret = get_pwent(0, 0, username, NULL,
			&pid, 
			l_getuidbyname, 
			l_tar_idcache_getuser,
			&dbpair_user_nset,
			userCacheM
		);
	*puid = (uid_t)(pid);
	return ret;
}

int 
taru_get_user_by_uid(uid_t uid, char * buf)
{
	int ret;
	E_DEBUG("");
	ret = get_pwent(0, uid, NULL, buf, NULL,
			l_getuidbyname, 
			l_tar_idcache_getuser,
			&dbpair_user_nset,
			userCacheM
		);
	return ret;
}

int 
taru_get_gid_by_name(char *groupname, gid_t *guid)
{
	int ret;
	long pid;
	E_DEBUG("");
	ret =  get_pwent(1, 0, groupname, NULL,
			&pid,
			l_getgidbyname, 
			l_tar_idcache_getgroup,
			&dbpair_group_nset,
			groupCacheM
		);
	*guid = (gid_t)(pid);
	return ret;
}

int 
taru_get_group_by_gid(gid_t gid, char * buf)
{
	int ret;
	E_DEBUG("");
	ret = get_pwent(1, gid, NULL, buf, NULL,
			l_getgidbyname, 
			l_tar_idcache_getgroup,
			&dbpair_group_nset,
			groupCacheM
		);
	return ret;
}

int
taru_split_name_ustar(TARU * taru, struct tar_header *tar_hdr,
			char * name, int tar_iflags)
{
	size_t length = strlen(name);
	size_t i;
	char * uh = (char*)tar_hdr;

	memset(uh + THB_BO_name, '\0', NAME_FIELD_SIZE);
	memset(uh + THB_BO_prefix, '\0', PREFIX_FIELD_SIZE);

	if (length > PREFIX_FIELD_SIZE + NAME_FIELD_SIZE + 1) {
		fprintf(stderr, "%s: name too long (max): %s\n", swlib_utilname_get(), name);
		return -1;
	}
	
	i = split_long_name(name, length);
	if (length - i - 1 > NAME_FIELD_SIZE)
	{
		fprintf(stderr, "%s: name too long (cannot be split): (i=%d) [%s]\n", swlib_utilname_get(), i, name);
		return -1;
	}
	if (i == 0) {
		return 0;	
	}
	memcpy((void*)(uh+THB_BO_name), name + i + 1, length - i - 1);
	memcpy((void*)(uh+THB_BO_prefix), name, i);
	return 0;
}

int 
taru_set_filetype_from_tartype(char ch, mode_t * mode, char * filename)
{

	(*mode) &= (~(S_IFMT));
	switch (ch) {
	case REGTYPE:
	case CONTTYPE:		/* For now, punt.  */
		(*mode) |= CP_IFREG;
		break;
	case DIRTYPE:
		(*mode) |= CP_IFDIR;
		break;
	case CHRTYPE:
		(*mode) |= CP_IFCHR;
		break;
	case BLKTYPE:
		(*mode) |= CP_IFBLK;
		break;
#ifdef CP_IFIFO
	case FIFOTYPE:
		(*mode) |= CP_IFIFO;
		break;
#endif
	case SYMTYPE:
#ifdef CP_IFLNK
		(*mode) |= CP_IFLNK;
		break;
		/* Else fall through.  */
#endif
	case LNKTYPE:
		(*mode) |= CP_IFREG;
		break;
	case AREGTYPE:
		/* Old tar format; if the last char in filename
		   is '/' then it is a directory, otherwise it's a regular
		   file.  */
		if (filename == (char*)NULL) {
			fprintf(stderr, 
		"taru_set_filetype_from_tartype():"
		" warning: AREGTYPE: filename not given, assuming REGTYPE.\n");
			(*mode) |= CP_IFREG;
		} else {
			if (filename[strlen(filename) - 1] == '/')
				(*mode) |= CP_IFDIR;
			else
				(*mode) |= CP_IFREG;
		}
		break;
	case NOTDUMPEDTYPE:
		(*mode) |= CP_IFSOCK;
		break;
	default:
		(*mode) |= CP_IFREG;
		fprintf(stderr, 
		"%s: warning:  typeflag [%c] not supported, ignoring file\n", swlib_utilname_get(), ch);
		return 1;
	}
	return 0;
}

static void
tarui_get_filetypes (mode_t mode, int * cpio_mode, char *tarflag)
{
	if (S_ISREG(mode)) {
		*tarflag=REGTYPE;
		*cpio_mode= CP_IFREG;
	} else if (S_ISDOOR(mode)) {
		*tarflag=NOTDUMPEDTYPE;
		*cpio_mode= CP_IFSOCK;
	} else if (S_ISDIR(mode)) {
		*tarflag=DIRTYPE;
		*cpio_mode = CP_IFDIR;
#ifdef S_ISBLK
	} else if (S_ISBLK(mode)) {
		*tarflag=BLKTYPE;
		*cpio_mode= CP_IFBLK;
#endif
#ifdef S_ISCHR
	} else if (S_ISCHR(mode)) {
		*tarflag=CHRTYPE;
		*cpio_mode= CP_IFCHR;
#endif
#ifdef S_ISFIFO
	} else if (S_ISFIFO(mode)) {
		*tarflag=FIFOTYPE;
		*cpio_mode= CP_IFIFO;
#endif
#ifdef S_ISLNK
	} else if (S_ISLNK(mode)) {
		*tarflag=SYMTYPE;
		*cpio_mode= CP_IFLNK;
#endif
#ifdef S_ISSOCK
	} else if (S_ISSOCK(mode)) {
		*tarflag=NOTDUMPEDTYPE;
		*cpio_mode= CP_IFSOCK;
#endif
#ifdef S_ISNWK
	} else if (S_ISNWK(mode)) {
		*tarflag=REGTYPE;
		*cpio_mode= CP_IFNWK;
#endif
	} else {
		fprintf (stderr,"%s: unrecognized type in mode: %d\n", swlib_utilname_get(), (int)mode);	
		*tarflag=(char)(-1);
		*cpio_mode= -1;
	}
}

int 
taru_get_tar_filetype (mode_t mode)
{
	char  c;
	int  cm;
	tarui_get_filetypes(mode, &cm, &c);
	return (int)c;
}

int
taru_get_cpio_filetype (mode_t mode)
{
	char  c;
	int  cm;
	tarui_get_filetypes(mode, &cm, &c);
	return cm;
}

/* Read a tar header, including the file name, from file descriptor IN_DES
   into FILE_HDR.  */
int
taru_read_in_tar_header2(TARU * taru, struct new_cpio_header *file_hdr,
			int in_des, char * fheader_buffer,
			int * eoa, int tarheaderflags, int buf_len)
{
	mode_t modet;
	int retval = 0;
	char * long_link_buffer;
	int long_link_bytes = 0;
	long bytes_skipped = 0;
	int warned = FALSE;
	union tar_record tar_rec;
	struct tar_header *tar_hdr = (struct tar_header *) &tar_rec;
	char * aregfilename;
	char * tmpname;
	char * header_buffer = fheader_buffer;
	int do_record_header = 0;
	int tar_iflags_retain_header_id = 0;
	int buffer_offset;
	int tmpret;
	long uidp;
	long gidp;

	buffer_offset = 0;
	if (taru) {
		tar_iflags_retain_header_id = (tarheaderflags & TARU_TAR_RETAIN_HEADER_IDS);
	}
	if (eoa) {
		*eoa = 0;
	}
	long_link_buffer = NULL;
	
	if (taru) {
		do_record_header = taru->do_record_headerM;
	}
	if (do_record_header) {
		taru->header_lengthM = 0;	
	}

	E_DEBUG("");	
	GNU_L_REREAD:	
	
	if (header_buffer == NULL) {
		tmpret = taru_tape_buffered_read(in_des, (void *)(&tar_rec), TARRECORDSIZE);
		if (tmpret > 0 && tmpret != TARRECORDSIZE) {
			fprintf(stderr, "%s: otar.c: short read in read_in_tar_header2 return=%d\n", swlib_utilname_get(), tmpret);
			return -1;
		} else if (tmpret == 0) {
			return 0;
		} else if (tmpret < 0) {
			fprintf(stderr, "%s: otar.c: read error from read_in_tar_header2 return=%d\n", swlib_utilname_get(), tmpret);
			return -1;
		} else {
			; /* Ok */
		}
	} else {
		memcpy((void *) (&tar_rec), header_buffer+buffer_offset, TARRECORDSIZE);
	}

	E_DEBUG("");	
	if (do_record_header) {
		strob_set_memlength(taru->headerM, taru->header_lengthM + 512);
		memcpy((void*)(strob_str(taru->headerM) + taru->header_lengthM),
				 (void*)(&tar_rec), 512);
		taru->header_lengthM += 512;
	}

	retval += 512;

	E_DEBUG("");	
	/* Check for a block of 0's.  */
	if (null_block((long *) &tar_rec, TARRECORDSIZE)) {
#if 0
		/* Found one block of 512 0's. If the next block is also all 0's
		   then this is the end of the archive.  If not, assume the
		   previous block was all corruption and continue reading
		   the archive.  */
		/* Commented out because GNU tar sometimes creates archives with
		   only one block of 0's at the end.  This happened for the
		   cpio 2.0 distribution!  */
		if (header_buffer)
			return 0;
		taru_tape_buffered_read(in_des,
					(void *)&tar_rec, TARRECORDSIZE);
		if (null_block((long *) &tar_rec, TARRECORDSIZE))
#endif
		{
			ahsStaticSetTarFilename(file_hdr,
					CPIO_INBAND_EOA_FILENAME);
			if (eoa) *eoa = 1;
			return 1024;
		}
#if 0
		bytes_skipped = TARRECORDSIZE;
#endif
	}

	E_DEBUG("");	
	if (*(((char*)tar_hdr) + 156) == 'L') {
		if (header_buffer && buf_len == TARRECORDSIZE) {
			/*
			* your screwed.
			*/
			fprintf(stderr,
			"fatal error, can not read GNU type L extension\n");
			exit(24);
		} else if (header_buffer && buf_len > TARRECORDSIZE) {
			taru_otoumax(tar_hdr->size, &file_hdr->c_filesize);
			if (file_hdr->c_filesize > 511) {
				/*
				* pretend can't be done. At the moment
				* filenames this long are not supported.
				*/
				fprintf(stderr,
	"fatal error, can not read GNU type L extension because file name\n"
	"is too long.\n");
				exit(24);
			}
			long_link_bytes += 512;
			retval += 512;

			E_DEBUG("");	
			/* Sanity check */
			if (retval + long_link_bytes != buf_len) {
				fprintf(stderr, "fatal error in taru_in_tar_header2: type L error\n");
				exit(24);
			}
			long_link_buffer = malloc(TARRECORDSIZE + 1);
			memset(long_link_buffer, '\0', TARRECORDSIZE + 1);
			memcpy(long_link_buffer, header_buffer+512, 512);
			long_link_buffer[TARRECORDSIZE] = '\0';

			if (do_record_header) {
				strob_set_memlength(taru->headerM,
						taru->header_lengthM + 512);
				memcpy((void*)(strob_str(taru->headerM) +
							 taru->header_lengthM),
					(void*)(long_link_buffer), 512);
				taru->header_lengthM += 512;
			}
			buffer_offset = TARRECORDSIZE + TARRECORDSIZE;
			goto GNU_L_REREAD;
		} else {
			/* 
			* read the data block which contains the filename.
			*/
			E_DEBUG("");	
			taru_otoumax(tar_hdr->size, &file_hdr->c_filesize);

			if (file_hdr->c_filesize > 511) {
				/*
				* pretend can't be done. At the moment
				* filenames this long are not supported.
				*/
				fprintf(stderr,
	"fatal error, can not read GNU type L extension because file name\n"
	"is too long.\n");
				exit(24);

			}
			long_link_buffer = malloc(TARRECORDSIZE + 1);
			memset(long_link_buffer, '\0', TARRECORDSIZE + 1);
			SWLIB_ALLOC_ASSERT(long_link_buffer != NULL);

			if (taru_tape_buffered_read(in_des,
						(void *)long_link_buffer,
						TARRECORDSIZE) !=
							TARRECORDSIZE) {
				fprintf(stderr, 
				"error reading long link data block, fatal.\n");
				exit(25);
			}
			long_link_bytes += 512;
			long_link_buffer[TARRECORDSIZE] = '\0';

			if (do_record_header) {
				strob_set_memlength(taru->headerM,
						taru->header_lengthM + 512);
				memcpy((void*)(strob_str(taru->headerM) +
							 taru->header_lengthM),
					(void*)(long_link_buffer), 512);
				taru->header_lengthM += 512;
			}
			goto GNU_L_REREAD;
		}
	}

	E_DEBUG("");	
	while (1) {
		E_DEBUG("");	
		taru_otoul(tar_hdr->chksum, &file_hdr->c_chksum);

		if (file_hdr->c_chksum != taru_tar_checksum(tar_hdr)) {
			if (!header_buffer) {

		/* If the checksum is bad, skip 1 byte and try again.  When
		   we try again we do not look for an EOF record (all zeros),
		   because when we start skipping bytes in a corrupted archive
		   the chances are pretty good that we might stumble across
		   2 blocks of 512 zeros (that probably is not really the last
		   record) and it is better to miss the EOF and give the user
		   a "premature EOF" error than to give up too soon on a
		   corrupted archive.  */
				if (!warned) {
					fprintf(stderr,
					"%s: invalid header: checksum error\n",
					swlib_utilname_get());
					warned = TRUE;
				}
				if (tarheaderflags & TARU_TAR_FRAGILE_FORMAT)
						return -2;
				bcopy(((char *) &tar_rec) + 1,
						(char *) &tar_rec,
				   		   TARRECORDSIZE - 1);
				taru_tape_buffered_read(in_des,
			(void *)(((char *)(&tar_rec)) + (TARRECORDSIZE - 1)),
					1);
				++bytes_skipped;
				continue;


			}
			/* not header buffer */ 
			else {
				return -3;
			}

		}	/* bad check sum */

		if (long_link_buffer == NULL) {
			/*
			* not GNU type L.
			*/
			tmpname = taru_dup_tar_name((void*)tar_hdr);
			ahsStaticSetTarFilename(file_hdr, tmpname);
		} else {
			/*
			* Type 'L' long link.
			*/
			ahsStaticSetTarFilename(file_hdr, long_link_buffer);
			tmpname = ahsStaticGetTarFilename(file_hdr);
			free(long_link_buffer);
			long_link_buffer = NULL;
		}
		E_DEBUG("");	
		file_hdr->c_namesize = strlen(tmpname) + 1;

		if ((char)(tar_hdr->typeflag) == LNKTYPE) {
			file_hdr->c_nlink = 2;
		} else {
			file_hdr->c_nlink = 1;
		}

		taru_otoul(tar_hdr->mode, &(file_hdr->c_mode));
		file_hdr->c_mode = file_hdr->c_mode & 07777;
		
		E_DEBUG("");	
		ahsStaticSetTarUsername(file_hdr, tar_hdr->uname);
		if (
			tar_iflags_retain_header_id == 0 &&
			l_cache_getuidbyname(tar_hdr->uname, &uidp) == 0
		) {
			E_DEBUG("");	
			file_hdr->c_uid = uidp;
		} else {
			E_DEBUG("");	
			taru_otoul(tar_hdr->uid, &file_hdr->c_uid);
		}
		E_DEBUG("");	
		taru_set_filetype_from_tartype((char)(tar_hdr->typeflag),
			(modet=(mode_t)(file_hdr->c_mode), &modet),
				tmpname);
	
		E_DEBUG("");	
		switch ((int)(tar_hdr->typeflag)) {
			case LNKTYPE:
				file_hdr->c_is_tar_lnktype = 1;
				break;
			default:
				file_hdr->c_is_tar_lnktype = 0;
		}
	
		E_DEBUG("");	
		ahsStaticSetTarGroupname(file_hdr, tar_hdr->gname);
		if (
			tar_iflags_retain_header_id == 0 &&
			l_cache_getgidbyname(tar_hdr->gname, &gidp) == 0
		) {
			file_hdr->c_gid = gidp;
		} else {
			taru_otoul(tar_hdr->gid, &file_hdr->c_gid);
		}

		taru_otoumax(tar_hdr->size, &file_hdr->c_filesize);
		taru_otoul(tar_hdr->mtime, &file_hdr->c_mtime);
		taru_otoul(tar_hdr->devmajor,
				(unsigned long *) &file_hdr->c_rdev_maj);
		taru_otoul(tar_hdr->devminor,
				(unsigned long *) &file_hdr->c_rdev_min);
		E_DEBUG("");	
		ahsStaticSetTarLinkname(file_hdr, (char *)(NULL));

		switch (tar_hdr->typeflag) {
		case REGTYPE:
		case CONTTYPE:	/* For now, punt.  */
		default:
			file_hdr->c_mode |= CP_IFREG;
			break;
		case DIRTYPE:
			file_hdr->c_mode |= CP_IFDIR;
			break;
		case CHRTYPE:
			file_hdr->c_mode |= CP_IFCHR;

		/* If a POSIX tar header has a valid linkname it's always 
		   supposed to set typeflag to be LNKTYPE.  System V.4 tar 
		   seems to be broken, and for device files with multiple
		   links it puts the name of the link into linkname,
		   but leaves typeflag as CHRTYPE, BLKTYPE, FIFOTYPE, etc.  */

			ahsStaticSetTarLinkname(file_hdr, tar_hdr->linkname);

		/* Does POSIX say that the filesize must be 0 for devices?  We
		   assume so, but HPUX's POSIX tar sets it to be 1 which causes
		   us problems (when reading an archive we assume we can always
		   skip to the next file by skipping filesize bytes).  For 
		   now at least, it's easier to clear filesize for devices,
		   rather than check everywhere we skip in copyin.c.  */

			file_hdr->c_filesize = 0;
			break;
		case BLKTYPE:
			file_hdr->c_mode |= CP_IFBLK;
			ahsStaticSetTarLinkname(file_hdr, tar_hdr->linkname);
			file_hdr->c_filesize = 0;
			break;
#ifdef CP_IFIFO
		case FIFOTYPE:
			file_hdr->c_mode |= CP_IFIFO;
			ahsStaticSetTarLinkname(file_hdr, tar_hdr->linkname);
			file_hdr->c_filesize = 0;
			break;
#endif
		case SYMTYPE:
#ifdef CP_IFLNK
			file_hdr->c_mode |= CP_IFLNK;
			ahsStaticSetTarLinkname(file_hdr, tar_hdr->linkname);
			file_hdr->c_filesize = 0;
			break;
			/* Else fall through.  */
#endif
		case LNKTYPE:
			file_hdr->c_mode |= CP_IFREG;
			ahsStaticSetTarLinkname(file_hdr, tar_hdr->linkname);
			file_hdr->c_filesize = 0;
			break;
		case AREGTYPE:
			/* Old tar format; if the last char in 
			   filename is '/' then it is
			   a directory, otherwise it's a regular file.  */
			aregfilename = ahsStaticGetTarFilename(file_hdr);
			if (aregfilename[strlen(aregfilename) - 1] == '/')
				file_hdr->c_mode |= CP_IFDIR;
			else
				file_hdr->c_mode |= CP_IFREG;
			break;
		}
		break;
	}
	if (bytes_skipped > 0) {
		fprintf(stderr,
			"%s: warning: skipped %ld bytes of junk\n", 
				swlib_utilname_get(), bytes_skipped);
		if (tarheaderflags & TARU_TAR_FRAGILE_FORMAT) return -4;
        }
	return retval + long_link_bytes;
}

int
taru_write_out_tar_header2(TARU * taru, struct new_cpio_header *file_hdr,
			int out_des, char *header_buffer,
			char * username, char * groupname, int tar_iflags)
{
	int ret;
	union tar_record tar_rec;
	struct tar_header *tar_hdr = (struct tar_header *) &tar_rec;
	char * u_name_buffer;
	char * u_ent_buffer;
	unsigned long sum;
	int termch;
	char * tmpp;
	int tar_iflags_like_star;
	int tar_iflags_like_pax;
	int tar_iflags_numeric_uids;
	int tar_iflags_like_oldgnu;
	int tar_iflags_like_oldgnuposix;
	int tar_iflags_like_gnu; 
	int do_record_header = 0;
	
	if (!taru) {
		fprintf(stderr,
			"fatal internal error: taru is NULL, this needs to be fixed\n");
		return -1;
	}
	
	if (*((char*)(taru)) != 'A') {
		fprintf(stderr,
	"%s: fatal error: taru is uninitialized, this needs to be fixed\n",
		swlib_utilname_get());
		return -1;
	}
	
	E_DEBUG3("ENTERING fd=%d, name=[%s]", out_des, ahsStaticGetTarFilename(file_hdr));
	E_DEBUG3("username=[%s] groupname=[%s]", username, groupname);
	E_DEBUG3("file_hdr: uid=[%d] gid=[%d]", (int)(file_hdr->c_uid), (int)(file_hdr->c_gid));


	bzero((char *) &tar_rec, TARRECORDSIZE);

	tar_iflags_like_star = (tar_iflags & TARU_TAR_BE_LIKE_STAR);
	tar_iflags_like_pax = (tar_iflags & TARU_TAR_BE_LIKE_PAX);
	tar_iflags_numeric_uids = (tar_iflags & TARU_TAR_NUMERIC_UIDS);
	tar_iflags_like_gnu = (tar_iflags & TARU_TAR_GNU_GNUTAR);
	tar_iflags_like_oldgnu = (tar_iflags & TARU_TAR_GNU_OLDGNUTAR);
	tar_iflags_like_oldgnuposix = (tar_iflags & TARU_TAR_GNU_OLDGNUPOSIX);
	E_DEBUG2("tar_iflags_numeric_uids is %d", tar_iflags_numeric_uids);
	
	do_record_header = taru->do_record_headerM;

	if (tar_iflags_like_star) {
		termch = '\040';  /* space */
	} else {
		termch = 0;   /* NUL */
	}

	strob_strcpy(taru->u_name_bufferM,
			ahsStaticGetTarFilename(file_hdr));

	if (strob_strlen(taru->u_name_bufferM) == 0) {
		fprintf(stderr, "internal error\n");
		return -1;
	}


	/*
	 * Add a trailing slash if a directory
	 */
	switch (file_hdr->c_mode & CP_IFMT) {
		case CP_IFDIR:
			if (
				*(strob_str(taru->u_name_bufferM) +
					strob_strlen(taru->u_name_bufferM) - 1) != '/'
			) {
				if (
					(strob_strlen(taru->u_name_bufferM) >= TARNAMESIZE - 1) &&
					(tar_iflags_like_oldgnu || tar_iflags_like_oldgnuposix)
				) {
					/*
					 * Don't un-NUL terminate the name field for OLD gnu formats.
					 */
					;
				} else {
					strob_strcat(taru->u_name_bufferM, "/");
				}
			}
		break;
	}

	u_name_buffer = strob_str(taru->u_name_bufferM);
	u_ent_buffer = strob_str(taru->u_ent_bufferM);
	/*
	 * the pathname is now in (char*)u_name_buffer
	 */

	if (taru_set_new_name(taru, tar_hdr, -1, u_name_buffer)) {
		fprintf(stderr, "%s: %s not dumped\n", swlib_utilname_get(), u_name_buffer);
		return -13;
	}

	if (tar_iflags_like_oldgnu) {
		MODE_TO_OLDGNU_CHARS(file_hdr->c_mode, tar_hdr->mode, termch);
	} else {
		MODE_TO_CHARS(file_hdr->c_mode, tar_hdr->mode, termch);
	}

	UID_TO_CHARS(file_hdr->c_uid, tar_hdr->uid, termch);
	GID_TO_CHARS(file_hdr->c_gid, tar_hdr->gid, termch);
	OFF_TO_CHARS(file_hdr->c_filesize,  tar_hdr->size, termch);
	TIME_TO_CHARS(file_hdr->c_mtime, tar_hdr->mtime, termch);

	*(tar_hdr->version) = '0';
	*(tar_hdr->version + 1)= '0';

	switch (file_hdr->c_mode & CP_IFMT) {
	case CP_IFREG:
		tmpp = ahsStaticGetTarLinkname(file_hdr);
		if (tmpp && strlen(tmpp)) {
			/*
			 * This code branch handles hard links
			 */
			/* 
			 * makes sure that linkname is shorter than
			 * TARLINKNAMESIZE.  
			 */
			if (strlen(tmpp) > TARLINKNAMESIZE) {
				fprintf(stderr,
					"%s, link name [%s] too long for tar.\n",
						swlib_utilname_get(), tmpp);
				E_DEBUG("LEAVING");
				return -4;
			}
			strncpy(tar_hdr->linkname, tmpp, TARLINKNAMESIZE);
			SIZE_TO_CHARS(0, tar_hdr->size, termch);
			tar_hdr->typeflag = LNKTYPE;
		} else {
			tar_hdr->typeflag = REGTYPE;
		}
		break;
	case CP_IFDIR:
		tar_hdr->typeflag = DIRTYPE;
		break;
	case CP_IFCHR:
		tar_hdr->typeflag = CHRTYPE;
		break;
	case CP_IFBLK:
		tar_hdr->typeflag = BLKTYPE;
		break;
#ifdef CP_IFSOCK
	case CP_IFSOCK:	/* GNU Tar does this, makes a SOCK a FIFO type */
#endif
#ifdef CP_IFIFO
	case CP_IFIFO:
		tar_hdr->typeflag = FIFOTYPE;
		break;
#endif				/* CP_IFIFO */
#ifdef CP_IFLNK
	case CP_IFLNK:
		tar_hdr->typeflag = SYMTYPE;

		tmpp = ahsStaticGetTarLinkname(file_hdr);
		if (strlen(tmpp) > TARLINKNAMESIZE) {
			fprintf(stderr, "%s: link name [%s] too long for tar.\n",
					swlib_utilname_get(), tmpp);
			E_DEBUG("LEAVING");
			return -5;
		}
		strncpy(tar_hdr->linkname, tmpp, TARLINKNAMESIZE);
		SIZE_TO_CHARS(0,  tar_hdr->size, termch);
		break;
#endif				/* CP_IFLNK */

	default:
		if (file_hdr->c_mode == 0 &&
			strcmp(ahsStaticGetTarFilename(file_hdr),
						GNU_LONG_LINK) == 0) {
			tar_hdr->typeflag = 'L';
		} else {
			fprintf(stderr, 
	"bad CP_?? filetype in taru_write_out_tar_header2: mode = %d\n",
				(int)(file_hdr->c_mode));
		}
		break;
	} /*--- end switch ---*/


	if (
		tar_iflags_like_gnu == 0 && 
		tar_iflags_like_oldgnu == 0
	) {
		strncpy(tar_hdr->magic, TMAGIC, TMAGLEN);
		strncpy(tar_hdr->magic + TMAGLEN, TVERSION, TVERSLEN);
	} else {
		/*
		 * GNU tar 1.13.25 and tar 1.15.1 --format=gnu do it like this
		 */
		strncpy(tar_hdr->magic, "ustar ", 6);
		strncpy(tar_hdr->magic + 6, " ", 2);
	}

	if ((username == NULL || strlen(username) == 0) &&
				file_hdr->c_cu != TARU_C_BY_UNONE) {
		E_DEBUG("username nil");
		l_tar_idcache_getuser(file_hdr->c_uid, u_ent_buffer);
		strncpy(tar_hdr->uname, u_ent_buffer, SNAME_LEN);
	} else {
		uid_t puid;
		if (
			file_hdr->c_cu == TARU_C_BY_USYS || 
			file_hdr->c_cu == TARU_C_BY_UNAME
			) 
			{
			E_DEBUG2("finding uid for username = %s", username);
			if (taru_get_uid_by_name(username, &puid)) {
				puid = NOBODY_ID;
				E_DEBUG3("user name [%s] not found setting uid to %d", username, (int)puid);
			}
			swlib_strncpy(tar_hdr->uname, username, SNAME_LEN);
		} else if (
			file_hdr->c_cu == TARU_C_BY_UID
			) 
			{
			puid = file_hdr->c_uid;
			E_DEBUG2("finding user for uid  %d", (int)puid);
			if (tar_iflags_numeric_uids == 0) {
				taru_get_user_by_uid(puid, u_ent_buffer);
				swlib_strncpy(tar_hdr->uname, u_ent_buffer, SNAME_LEN);
			} else {
				swlib_strncpy(tar_hdr->uname, "", SNAME_LEN);
			}
		} else {
			E_DEBUG("Using user and uid with no lookups");
			puid = file_hdr->c_uid;
			swlib_strncpy(tar_hdr->uname, username, SNAME_LEN);
		}	
		UID_TO_CHARS(puid, tar_hdr->uid, termch);
	}

	if ((groupname == NULL || strlen(groupname) == 0)
				&& file_hdr->c_cg != TARU_C_BY_GNONE) {
		E_DEBUG("groupname nil");
		l_tar_idcache_getgroup(file_hdr->c_gid, u_ent_buffer);
		strncpy(tar_hdr->gname, u_ent_buffer, SNAME_LEN);
	} else {
		gid_t pgid;

		if (
			file_hdr->c_cg == TARU_C_BY_GSYS || 
			file_hdr->c_cg == TARU_C_BY_GNAME
			) 
			{
			E_DEBUG2("finding gid for groupname=%s", groupname);
			swlib_strncpy(tar_hdr->gname, groupname, SNAME_LEN);
			if (taru_get_gid_by_name(groupname, &pgid)) {
				pgid = NOBODY_ID;
				E_DEBUG3("group name [%s] not found setting uid to %d", groupname, (int)pgid);
			}
		} else if (
			file_hdr->c_cg == TARU_C_BY_GID
			) 
			{
			pgid = file_hdr->c_gid;
			E_DEBUG2("finding gname for gid  %d", (int)pgid);
			if (tar_iflags_numeric_uids == 0) {
				taru_get_group_by_gid(pgid, u_ent_buffer);
				swlib_strncpy(tar_hdr->gname, u_ent_buffer, SNAME_LEN);
			} else {
				swlib_strncpy(tar_hdr->gname, "", SNAME_LEN);
			}
		} else {
			E_DEBUG("Using user and gid with no lookups");
			pgid = file_hdr->c_gid;
			swlib_strncpy(tar_hdr->gname, groupname, SNAME_LEN);
		}	
		GID_TO_CHARS(pgid, tar_hdr->gid, termch);
	}

	if (tar_iflags_numeric_uids) {
		memset((void*)tar_hdr->uname, '\0', SNAME_LEN);
		memset((void*)tar_hdr->gname, '\0', SNAME_LEN);
	}

	if (tar_hdr->typeflag == CHRTYPE || tar_hdr->typeflag == BLKTYPE) {
		MAJOR_TO_CHARS(file_hdr->c_rdev_maj, tar_hdr->devmajor, termch);
		MINOR_TO_CHARS(file_hdr->c_rdev_min, tar_hdr->devminor, termch);
	} else {
		if (tar_iflags_like_pax) {
			strncpy(tar_hdr->devmajor, "0000000", 8);
			strncpy(tar_hdr->devminor, "0000000", 8);
		} else if (tar_iflags_like_star) {
			strncpy(tar_hdr->devmajor, "0000000 ", 8);
			strncpy(tar_hdr->devminor, "0000000 ", 8);
		} else if (
			tar_iflags_like_gnu ||
			tar_iflags_like_oldgnu ||
			tar_iflags_like_oldgnuposix
			)
		{
			/*
			 * GNU tar 1.13.25 leaves the fields null (zero length strings)
			 * padded with 0x00.
			 */
			;
		} else {
			/*
			 * Now, as of Jan 2005 version>0.420, the default is like GNU tar 1.15.1
			 */
			strncpy(tar_hdr->devmajor, "0000000", 8);
			strncpy(tar_hdr->devminor, "0000000", 8);
		}
	}

	memcpy(tar_hdr->chksum, CHKBLANKS, sizeof(tar_hdr->chksum));
	sum = taru_tar_checksum(tar_hdr);

	if (tar_iflags_like_pax || tar_iflags_like_star) {
		/* This mimics pax v3.0 */
		uintmax_to_chars(sum, tar_hdr->chksum, 8, POSIX_FORMAT, termch);
	} else {
		uintmax_to_chars(sum, tar_hdr->chksum, 7, POSIX_FORMAT, termch);
	}

	if (header_buffer) {
		if (do_record_header) {
			strob_set_memlength(taru->headerM, TARRECORDSIZE);
			memcpy((void*)(strob_str(taru->headerM)), (void*)(&tar_rec), TARRECORDSIZE);
			taru->header_lengthM = TARRECORDSIZE;
		}
		memcpy(header_buffer, (void *) &tar_rec, 512);
		E_DEBUG("LEAVING");
		return 512;
	} else {
		if (do_record_header) {
			strob_set_memlength(taru->headerM, TARRECORDSIZE);
			memcpy((void*)(strob_str(taru->headerM)), (void*)(&tar_rec), TARRECORDSIZE);
			taru->header_lengthM = TARRECORDSIZE;
		}
		ret = taru_safewrite(out_des, (void *) &tar_rec, TARRECORDSIZE);
		E_DEBUG("LEAVING");
		return ret;
	}
}

long
taru_from_oct (int digs, char *where)
{
  long value;

  while (ISSPACE (*where))
    {                           /* skip spaces */
      where++;
      if (--digs <= 0)
        return -1;              /* all blank field */
    }
  value = 0;
  while (digs > 0 && ISODIGIT (*where))
    {

      /* Scan til nonoctal.  */

      value = (value << 3) | (*where++ - '0');
      --digs;
    }

  if (digs > 0 && *where && !ISSPACE (*where))
    return -1;                  /* ended on non-space/nul */

  return value;
}

void 
taru_to_oct(register long value, register int digits,
			register char *  where)
{
  --digits;                     /* Leave the trailing NUL slot alone.  */
  where[--digits] = ' ';        /* Put in the space, though.  */

  /* Produce the digits -- at least one.  */
  do
    {
      where[--digits] = '0' + (char) (value & 7); /* One octal digit.  */
      value >>= 3;
    }
  while (digits > 0 && value != 0);

  /* Add leading spaces, if necessary.  */
  while (digits > 0)
    where[--digits] = ' ';
}

/*
* Decimal ascii to unsigned long
*     return 0 if conversion OK
*     return 1 if failed to convert string to end.
*     return 2 if overflow.
*/
int
taru_datoul (char * s, unsigned long *n)
{
  unsigned long oldval = 0;
  unsigned long val = 0;
	
  while (*s == ' ')
    ++s;

  while (*s >= '0' && *s <= '9' && val >= oldval) {
          /* fprintf(stderr, "%lu %lu\n", oldval, val); */
          oldval = val;
          val = 10 * val + *s++ - '0';
  }
  if (oldval && ((val / oldval) < 10)) {
	fprintf(stderr, "%s, taru_datoul : conversion overflow\n", swlib_utilname_get());
	return 2;
  }
  while (*s == ' ') ++s;
  *n = val;
  return (!(*s == '\0'));
}

/*
* Octal Acsii to unsigned long
*/
int
taru_otoul (char * s, unsigned long *n)
{
  unsigned long val = 0;

  while (*s == ' ')
    ++s;
  while (*s >= '0' && *s <= '7')
    val = 8 * val + *s++ - '0';
  while (*s == ' ')
    ++s;
  *n = val;
  return *s == '\0';
}

int
taru_otoumax (char * s, uintmax_t *n)
{
  uintmax_t val = 0;

  while (*s == ' ')
    ++s;
  while (*s >= '0' && *s <= '7')
    val = 8 * val + *s++ - '0';
  while (*s == ' ')
    ++s;
  *n = val;
  return *s == '\0';
}

int taru_tarheader_check ( char * buffer ) {
  unsigned long sumis, sum;

  if ( strncmp (buffer + 257, TMAGIC, 5 )) {
    return 1;
  }
  sumis = (unsigned long) taru_from_oct ( 8 , buffer + 148);
  sum = taru_tar_checksum ((void *) buffer);
  if ( sum != sumis ) {
	return -1;
  }
  return 0;
}

unsigned long taru_tar_checksum (void * hdr)
{
  
  struct tar_header *tar_hdr = (struct tar_header*)(hdr); 
  
  unsigned long sum = 0;
  char *p = (char *) tar_hdr;
  char *q = p + TARRECORDSIZE;
  int i;

  while (p < tar_hdr->chksum)
    sum += *p++ & 0xff;
  for (i = 0; i < 8; ++i)
    {
      sum += ' ';
      ++p;
    }
  while (p < q)
    sum += *p++ & 0xff;
  return sum;
}

int
taru_tape_skip_padding (int in_file_des, uintmax_t offset, 
			enum archive_format archive_format_in)
{
  int pad;

  if (archive_format_in == arf_crcascii || archive_format_in == arf_newascii)
    pad = (4 - (offset % 4)) % 4;
  else if (archive_format_in == arf_binary || archive_format_in == arf_hpbinary)
    pad = (2 - (offset % 2)) % 2;
  else if (archive_format_in == arf_tar || archive_format_in == arf_ustar)
    pad = (512 - (offset % 512)) % 512;
  else
    pad = 0;

  if (pad != 0) {
    if (in_file_des < 0) {
   	return pad; 
    } else { 
    	return taru_read_amount(in_file_des, pad);
    }
  }
  return 0;
}
