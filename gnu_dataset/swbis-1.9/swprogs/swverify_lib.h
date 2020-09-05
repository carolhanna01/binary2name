/* swverify_lib.h - swverify routines
  
 Copyright (C) 2007 James H. Lowe, Jr.

 COPYING TERMS AND CONDITIONS:
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
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*/

#ifndef swverify_lib_200609_h
#define swverify_lib_200609_h

#include "swuser_config.h"
#include "swuser_assert_config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vplob.h"
#include "usgetopt.h"
#include "swcommon0.h"
#include "swextopt.h"
#include "swutillib.h"
#include "swverid.h"
#include "globalblob.h"
	

#define SWVERIFY_RP_STATUS_NO_INTEGRITY	4

#ifndef DEFAULT_PAX_REM 
#define DEFAULT_PAX_REM "tar"
#endif


#define INL_ATT_SIZE (sizeof(char*) + sizeof(int) + sizeof(int))

#define INL_NAME_OFFSET			0
#define INL_LINKNAME_OFFSET		((int)((char*)(&(AA.linknameM)) - ((char*)(&AA))))
#define INL_SIZE_OFFSET 		((int)((char*)(&(AA.sizeM)) - ((char*)(&AA))))
#define INL_PERMISSIONS_OFFSET 		((int)((char*)(&(AA.permissionsM)) - ((char*)(&AA))))
#define INL_OWNERS_OFFSET 		((int)((char*)(&(AA.ownersM)) - ((char*)(&AA))))
#define INL_MDATE_OFFSET 		((int)((char*)(&(AA.mdateM)) - ((char*)(&AA))))
#define INL_MD5_OFFSET 			((int)((char*)(&(AA.md5M)) - ((char*)(&AA))))
#define INL_SHA1_OFFSET 		((int)((char*)(&(AA.sha1M)) - ((char*)(&AA))))
#define INL_SHA512_OFFSET 		((int)((char*)(&(AA.sha512M)) - ((char*)(&AA))))

				/* ranked from OK to absolute worst */
#define INL_STATUS_UNSET	5	/*  (OK) */
#define INL_STATUS_0_0_NA	4	/*  (OK) missing from system and INFO */
#define INL_STATUS_1_0_NA	3	/*  if present in INFO and empty in system */
#define INL_STATUS_1_1_EQ	2	/*  if identical */
#define INL_STATUS_0_1_NA	1	/*  if present in system, but missing from INFO */
#define INL_STATUS_1_1_NEQ	0	/*  (worst) present in INFO and system and unequal */
#define INL_STATUS_MISSING_FILE -1	/* even worst */

typedef struct {
	char * addrM;
	int lenM;
	int statusM;
} INFOITEM;

typedef struct {
	INFOITEM nameM;
	INFOITEM linknameM;
	INFOITEM sizeM;
	INFOITEM permissionsM;
	INFOITEM ownersM;
	INFOITEM mdateM;
	INFOITEM md5M;
	INFOITEM sha1M;
	INFOITEM sha512M;
} INFOLINE;


INFOLINE * swverify_inl_create(void);
void swverify_inl_delete(INFOLINE*);
int swverify_inl_parse(INFOLINE*, char * line);

int swverify_write_source_copy_script2(GB * G, int ofd, char * sourcepath, int do_get_file_type, int vlv,
		int delaytime, int nhops, char * pax_write_command_key, char * hostname, char * blocksize);

int swverify_write_source_copy_script(GB * G, int ofd, char * sourcepath, int do_get_file_type, int vlv,
		int delaytime, int nhops, char * pax_write_command_key, char * hostname, char * blocksize);


int swverify_list_distribution_archive(GB * G, struct extendedOptions * opta,
	VPLOB * swspecs, char * target_path, int target_nhops, int efd);

SWI * swverify_list_create_swi(GB * G, struct extendedOptions * opta, VPLOB * swspecs, char * target_path);
int swverify_ls_fileset_from_iscat(SWI * swi);
int swverify_looper_sr_payload(GB * G, char * target_path, SWICOL * swicol, SWICAT_SC * sc, SWICAT_SR * sr,
			int ofd, int ifd, int *, SWUTS * uts, char * pax_write_command_key, int *);

#endif
