/*  taru.c  Low-level tarball utility functions 

   Copyright (C) 1998, 1999  Jim Lowe

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


#include "swuser_config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include "cpiohdr.h"
#include "strob.h"
#include "tarhdr.h"
#include "ahs.h"
#include "swlib.h"
#include "swutilname.h"
#include "uxfio.h"
#include "taru.h"
#include "ls_list.h"
#include "taruib.h"

TARU * taru_create(void)
{
	TARU * taru = (TARU*)malloc(sizeof(TARU));
	taru->idM = 'A';
	taru->headerM = strob_open(1024);
	taru->header_lengthM = 0;
	taru->taru_tarheaderflagsM = 0;
	taru->do_record_headerM = 0;	
	taru->u_name_bufferM = strob_open(8);
	taru->u_ent_bufferM = strob_open(64);
	taru->nullfdM = swbis_devnull_open("/dev/null", O_RDWR, 0);
	/* taru->do_md5M = 0; */
	taru->md5bufM = strob_open(64);
	strob_set_memlength(taru->md5bufM, 64);
	taru->linkrecord_disableM = 0;
	taru->preview_fdM = -1;
	taru->preview_levelM = TARU_PV_0;
	taru->preview_bufferM = strob_open(64);
	return taru;
}

void
taru_delete(TARU * taru)
{
	strob_close(taru->headerM);
	strob_close(taru->u_name_bufferM);
	strob_close(taru->u_ent_bufferM);
	strob_close(taru->md5bufM);
	strob_close(taru->preview_bufferM);
	swbis_devnull_close(taru->nullfdM);
	free(taru);
}

void
taru_set_header_recording(TARU * taru, int n)
{
	taru->do_record_headerM = n;	
}

char *
taru_get_recorded_header(TARU * taru, int * len)
{
	if (len) *len = taru->header_lengthM;
	return strob_str(taru->headerM);
}

void 
taru_clear_header_buffer(TARU * taru)
{
	taru->header_lengthM = 0;
}

intmax_t
taru_hdr_get_filesize(struct new_cpio_header * file_hdr)
{
	static int check_off_t = 0;

	if (file_hdr->c_filesize >= LLONG_MAX) {
		SWLIB_FATAL("c_filesize too big: >=LLONG_MAX");
	}
	if (check_off_t == 0) {
		struct stat st;
		if (sizeof(st.st_size) < 5) {
			check_off_t = 1;
		} else {
			check_off_t = 2;
		}
	}
	if (check_off_t == 1 && file_hdr->c_filesize >= LONG_MAX) {
		SWLIB_FATAL("c_filesize too big: >=LONG_MAX");
	}
	return (intmax_t)(file_hdr->c_filesize);
}

void 
taru_append_to_header_buffer(TARU * taru, char * buf, int len) 
{
	char * s;
	strob_set_length(taru->headerM, taru->header_lengthM + len + 1);
	s = strob_str(taru->headerM);
	memcpy(s + taru->header_lengthM, buf, len);
	taru->header_lengthM += len;
}

void
taru_digs_print(FILE_DIGS * digs, STROB * buffer)
{

	if (digs->do_md5 == DIGS_ENABLE_ON)
		strob_sprintf(buffer, STROB_DO_APPEND, " [%s]", digs->md5);
	else
		strob_sprintf(buffer, STROB_DO_APPEND, " []");

		
	if (digs->do_sha1 == DIGS_ENABLE_ON)
		strob_sprintf(buffer, STROB_DO_APPEND, " [%s]", digs->sha1);
	else
		strob_sprintf(buffer, STROB_DO_APPEND, " []");
}

void
taru_digs_delete(FILE_DIGS * digs)
{
	free(digs);
}

FILE_DIGS *
taru_digs_create(void)
{
	FILE_DIGS * digs = malloc(sizeof(FILE_DIGS));
	if (!digs) return digs;
	taru_digs_init(digs, DIGS_ENABLE_OFF, 0);
	return digs;
}

void
taru_digs_init(FILE_DIGS * digs, int enable, int poison)
{
	digs->do_poisonM = poison;
	digs->md5[0] = '\0';
	digs->sha1[0] = '\0';
	digs->sha512[0] = '\0';
	digs->size[0] = '\0';
	if (enable >= 0) {
		digs->do_md5 = enable;
		digs->do_sha1 = enable;
		digs->do_sha512 = enable;
		digs->do_size = enable;
	}
}

void
taru_init_header_digs(struct new_cpio_header * file_hdr)
{
	if (file_hdr->digsM == NULL)
		file_hdr->digsM = taru_digs_create();
	else
		taru_digs_init(file_hdr->digsM, DIGS_ENABLE_OFF, 0);
}

void
taru_free_header_digs(struct new_cpio_header * file_hdr)
{
	if (file_hdr->digsM) {
		taru_digs_delete(file_hdr->digsM);
		file_hdr->digsM = NULL;
	}
}

void
taru_init_header(struct new_cpio_header * file_hdr)
{
	file_hdr->c_mode = 0400;
       	file_hdr->c_uid = AHS_ID_NOBODY;
       	file_hdr->c_gid = AHS_ID_NOBODY;
       	file_hdr->c_nlink = 0;
       	file_hdr->c_mtime = time((time_t*)(NULL));
       	file_hdr->c_filesize = 0;
       	file_hdr->c_dev_maj = 0;
       	file_hdr->c_dev_min = 0;
       	file_hdr->c_rdev_maj = 0;
       	file_hdr->c_rdev_min = 0;
       	file_hdr->c_cu = TARU_C_BY_USYS;
       	file_hdr->c_cg = TARU_C_BY_GSYS;
       	file_hdr->c_is_tar_lnktype = -1;
	ahsStaticSetTarGroupname(file_hdr, "");
	ahsStaticSetTarUsername(file_hdr, "");
	ahsStaticSetTarLinkname(file_hdr, "");
	ahsStaticSetTarFilename(file_hdr, "");
	file_hdr->usage_maskM = 0;
	taru_init_header_digs(file_hdr);
}

struct new_cpio_header *
taru_make_header(void)
{
	struct new_cpio_header *hnew;
	hnew = malloc(sizeof(struct new_cpio_header));
       	hnew->c_name = NULL;
       	hnew->c_tar_linkname = NULL;
       	hnew->c_username = NULL;
       	hnew->c_groupname = NULL;
	hnew->digsM = NULL;
	taru_init_header(hnew);
	return hnew;
}

void
taru_set_tarheader_flag(TARU * taru, int flag, int n) {
	if (n) {
		taru->taru_tarheaderflagsM |= flag;
	} else {
		taru->taru_tarheaderflagsM &= ~flag;
	}
}

void
taru_set_preview_level(TARU * taru, int preview_level)
{
	taru->preview_levelM = preview_level;
}

int
taru_get_preview_level(TARU * taru)
{
	return taru->preview_levelM;
}

void
taru_set_preview_fd(TARU * taru, int fd)
{
	taru->preview_fdM = fd;
}

int
taru_get_preview_fd(TARU * taru)
{
	return taru->preview_fdM;
}

void 
taru_free_header(struct new_cpio_header *h)
{
	taru_free_header_digs(h);
	ahsStaticSetTarGroupname(h, NULL); /* these have effect of calling */
	ahsStaticSetTarUsername(h, NULL);  /* strob_close() */
	ahsStaticSetTarLinkname(h, NULL);
	ahsStaticSetTarFilename(h, NULL);
	swbis_free(h);
}

int
taru_print_tar_ls_list(STROB * buf, struct new_cpio_header * file_hdr, int vflag)
{
	int type;

	strob_strcpy(buf, "");
	type = taru_get_tar_filetype(file_hdr->c_mode);

	if (
		(file_hdr->c_is_tar_lnktype == 1) ||
		(
			type == REGTYPE &&
			strlen(ahsStaticGetTarLinkname(file_hdr))
		)
	) {
		type = LNKTYPE;
	}
	if (type >= 0)
		ls_list_to_string(
			/* name */ ahsStaticGetTarFilename(file_hdr),
			/* ln_name */ ahsStaticGetTarLinkname(file_hdr),
			file_hdr,
			(time_t)(0),
			buf,
			ahsStaticGetTarUsername(file_hdr),
			ahsStaticGetTarGroupname(file_hdr),
			type,
			vflag);
	else
		fprintf(stderr, "%s: unrecognized file type in mode [%d] for file: %s\n",
			swlib_utilname_get(), (int)(file_hdr->c_mode), ahsStaticGetTarFilename(file_hdr));

	/* Test line:  taru_digs_print(file_hdr->digsM, buf); */
	strob_strcat(buf, "\n");
	return 0;
}

void
taru_write_preview_line(TARU * taru, struct new_cpio_header *file_hdr)
{
	int fd;
	int flag;
	int level;
	char * filename;
	STROB * buffer;

	flag = 0;
	if (
		taru->preview_fdM < 0 ||
		taru->preview_levelM == TARU_PV_0
	) {
		return;
	}
	
	fd = taru->preview_fdM;
	level = taru->preview_levelM;
	buffer = taru->preview_bufferM;
	filename = ahsStaticGetTarFilename(file_hdr);

	if (isatty(fd)) {
		flag = ls_list_get_encoding_flag();
		ls_list_set_encoding_by_lang();
	}


	if (level >= TARU_PV_3) {
		taru_print_tar_ls_list(buffer, file_hdr, LS_LIST_VERBOSE_L2);
	} else if (level >= TARU_PV_2) {
		taru_print_tar_ls_list(buffer, file_hdr, LS_LIST_VERBOSE_L1);
	} else if (level >= TARU_PV_1) {
		/* strob_sprintf(buffer, 0, "%s\n", filename); */
		strob_strcpy(buffer, "");
		ls_list_safe_print_to_strob(filename, buffer, 0);
		strob_strcat(buffer, "\n");
	} else {
		strob_strcpy(buffer, "");
	}
	if (strob_strlen(buffer)) {
		uxfio_write(fd, strob_str(buffer), strob_strlen(buffer));
	}
	if (isatty(fd)) {
		ls_list_set_encoding_flag(flag);
	}
}

int
taru_set_tar_header_policy(TARU * taru, char * user_format, int * p_arf_format)
{
	int xx;
	int * format;
	if (!p_arf_format)
		format = &xx;
	else
		format = p_arf_format;
	if (!strcmp(user_format,"ustar")) {
		/*
		 * Default POSIX ustar format
		 * GNU tar-1.15.1 --format=ustar
		 */
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUTAR, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_LONG_LINKS, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_BE_LIKE_PAX, 0);
  		*format=arf_ustar;
	} else if (
		!strcmp(user_format,"gnu") ||
		!strcmp(user_format,"gnutar") ||
		0
	) {
		/*
		 * same as GNU tar 1.15.1 --format=gnu
		 */
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUPOSIX, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUTAR, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_BE_LIKE_PAX, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_LONG_LINKS, 1);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_BLOCKSIZE_B1, 1);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_GNUTAR, 1);
  		*format=arf_ustar;
	} else if (!strcmp(user_format,"ustar.star")) {
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUTAR, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUPOSIX, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_BE_LIKE_PAX, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_BE_LIKE_STAR, 1);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_BLOCKSIZE_B1, 1);
  		*format=arf_ustar;
	} else if (
		!strcmp(user_format,"oldgnu") ||
		0
	) {
		/*
		 * GNU tar-1.13.25 -b1  // default compilation
		 */
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUTAR, 1);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_LONG_LINKS, 1);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_BLOCKSIZE_B1, 1);
  		*format=arf_ustar;
	} else if (!strcmp(user_format,"ustar0")) {
		/*
		 * GNU tar-1.13.25 --posix -b1
		 */
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUTAR, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_LONG_LINKS, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_BE_LIKE_PAX, 0);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_BLOCKSIZE_B1, 1);
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_OLDGNUPOSIX, 1);
  		*format=arf_ustar;
	} else if (!strcmp(user_format,"bsdpax3")) {
		/*
		 * Emulate /bin/pax as found of some GNU and BSD systems.
		 */
		taru_set_tarheader_flag(taru, TARU_TAR_GNU_BLOCKSIZE_B1, 1);
		taru_set_tarheader_flag(taru, TARU_TAR_BE_LIKE_PAX, 0);
  		*format=arf_ustar;
	} else if (!strcmp(user_format,"newc")) {
		*format=arf_newascii;
	} else if (!strcmp(user_format,"crc")) {
  		*format=arf_crcascii;
	} else if (!strcmp(user_format,"odc")) {
 		*format=arf_oldascii;
	} else {
		fprintf (stderr,"%s: unrecognized format: %s\n", swlib_utilname_get(), user_format);
		return -1;
	}
	return 0;
}

int
taru_check_devno_for_tar(struct new_cpio_header * file_hdr)
{
	if (file_hdr->c_rdev_maj > 2097151)  /* & 07777777 != file_hdr->c_rdev_maj) */
		return 1;
	if (file_hdr->c_rdev_min > 2097151)  /* & 07777777 != file_hdr->c_rdev_min) */
		return 2;
	return 0;
}

int
taru_check_devno_for_system(struct new_cpio_header * file_hdr)
{
	int d = makedev(file_hdr->c_rdev_maj, file_hdr->c_rdev_min);
	if ((int)(file_hdr->c_rdev_maj) != (int)minor(d))
		return 1;
	if ((int)(file_hdr->c_rdev_min) != (int)minor(d))
		return 2;
	return 0;
}
