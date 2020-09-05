/* readinheader.c - read in an archive header

   Copyright (C) 1990, 1991, 1992 Free Software Foundation, Inc.
   Copyright (C) 2002 Jim Lowe

   Portions of this code are derived from code (found in GNU cpio)
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
#include <sys/types.h>
#include <sys/stat.h>
#include "filetypes.h"
#include "system.h"
#include "cpiohdr.h"
#include "fnmatch_u.h"
#include "uxfio.h"
#include "ahs.h"
#include "uinfile.h"
#include "taru.h"
#include "swlib.h"
#include "strob.h"
#include "hllist.h"
#include "taruib.h"
#include "taru_debug.h"
#include "swutilname.h"
/* #include "swgp.h" */

#ifndef HAVE_LCHOWN
#define lchown chown
#endif

/* #define TARU_DEBUG_LINKS */

void taru_read_in_binary (struct new_cpio_header *  file_hdr, int in_des);
void taru_swab_array (char * ptr, int count);

/* Return 16-bit integer I with the bytes swapped.  */
#define swab_short(i) ((((i) << 8) & 0xff00) | (((i) >> 8) & 0x00ff))

intmax_t
taru_pump_amount2(int discharge_fd, int suction_fd,
			intmax_t amount, int adjunct_ofd)
{
	intmax_t ret;
	intmax_t i = amount;
	if ((ret=swlib_i_pipe_pump(suction_fd, discharge_fd,
				&i, adjunct_ofd, NULL, taru_tape_buffered_read))) {
		return -1;
	}
	return i;
}

intmax_t
taru_read_amount(int suction_fd, intmax_t amount)
{
	return taru_pump_amount2(-1, suction_fd, amount, -1);
}

ssize_t
taru_tape_buffered_read(int fd, void * buf, size_t pcount)
{
	int taruib_fd;
	int count;
	
	E_DEBUG("ENTERING");
	
	taruib_fd = taruib_get_fd();
	count = uxfio_sfa_read(fd, buf, pcount);

	E_DEBUG2("file stats on descriptor %d", fd);

	if (taruib_fd > 0 && count > 0) {
		char * buffer = taruib_get_buffer();
		int data_len = taruib_get_datalen();	
		int buffer_reserve = taruib_get_nominal_reserve() - data_len;	
		char * buffer_dst;

		if (buffer_reserve < count &&
				taruib_get_overflow_release() == 0) {
			buffer_dst = buffer;
			taruib_set_datalen(count);
			uxfio_write(taruib_fd, buffer_dst, data_len); 
		} else {
			if (data_len + (int)pcount > taruib_get_reserve()) {
				fprintf(stderr,
		"taru_tape_buffered_read fatal error: stop, stop, stop.\n");
				exit(2);
			}
			buffer_dst = buffer + data_len;
			taruib_set_datalen(data_len + count);
		}
		memcpy(buffer_dst, buf, count);
	}
	E_DEBUG("LEAVING");
	return count;
}

int 
taru_read_header (TARU * taru, struct new_cpio_header * file_hdr,
		int in_des, enum archive_format format, int * eoa, int flags)
{
	int retval;
	char * link_name;
	int ret;

	E_DEBUG("ENTERING");
	E_DEBUG2("fildes=%d", in_des);

	ret=taru_read_in_header(taru, file_hdr, in_des, format, eoa, flags);
	if (ret < 0)
		return -1;
	retval = ret;
	if (format != arf_tar && format != arf_ustar) {
		if ((file_hdr->c_mode & CP_IFMT) == CP_IFLNK) {
			link_name = (char *)malloc((unsigned int)
						file_hdr->c_filesize + 1);
			link_name[(int)(file_hdr->c_filesize)] = '\0';
			ret = taru_tape_buffered_read(in_des, link_name, (size_t)(file_hdr->c_filesize));
			if (ret < 0) return -retval;
			retval += ret;
			ret = taru_tape_skip_padding(in_des,
						file_hdr->c_filesize, format);
			if (ret < 0) return -retval;
			retval += ret;
			ahsStaticSetTarLinkname(file_hdr, link_name);
		} else {
			ahsStaticSetTarLinkname(file_hdr, NULL);
		}
	}
	E_DEBUG("LEAVING");
	return retval;
}


int 
taru_read_in_header (TARU * taru, struct new_cpio_header * file_hdr,
			int in_des, enum archive_format archive_format_in0,
						int * p_eoa, int flags)
{
  long bytes_skipped = 0;	/* Bytes of junk found before magic number.  */
  int ret;
  int retval = 0;
  /* Search for a valid magic number.  */

  E_DEBUG("ENTERING");
  if (archive_format_in0 == arf_unknown)
    {
      fprintf (stderr, " format unknown in read_in_header.\n");
      return -1;
    }

  if (archive_format_in0 == arf_tar || archive_format_in0 == arf_ustar)
    {
        E_DEBUG("");
	if ((retval=taru_read_in_tar_header2(taru, file_hdr, in_des,
					(char*)(NULL), p_eoa, flags, TARRECORDSIZE)) < 0) {
		E_DEBUG("LEAVING");
      		return -1;
	} else {
		E_DEBUG("LEAVING");
		return retval;
    	}
    }

  ahsStaticSetTarLinkname(file_hdr, NULL);

  if ((retval=taru_tape_buffered_read(in_des, (void*)file_hdr, (size_t)(6))) != 6){
        E_DEBUG("");
    	fprintf (stderr, "error reading magic. retval= %d\n", retval);
	E_DEBUG("LEAVING");
  	return -1; 
  } 
  
  while (1)
    {
      if (archive_format_in0 == arf_newascii
	  && !strncmp ((char *) file_hdr, "070701", 6))
	{
          E_DEBUG("");
	  if (bytes_skipped > 0)
	    fprintf (stderr, 
			"warning: skipped %ld bytes of junk", bytes_skipped);
	  if ((ret = taru_read_in_new_ascii(taru, file_hdr, in_des,
						archive_format_in0)) < 0) {
	    	fprintf (stderr, "error from taru_read_in_new_ascii");
		E_DEBUG("LEAVING");
	  	return -retval;
	  }
          if (!strcmp(ahsStaticGetTarFilename(file_hdr),
					CPIO_INBAND_EOA_FILENAME)) 
	  	if (p_eoa) 
			*p_eoa = 1;
	  retval += ret;
	  break;
	}
      if (archive_format_in0 == arf_crcascii
	  && !strncmp ((char *) file_hdr, "070702", 6))
	{
          E_DEBUG("");
	  if (bytes_skipped > 0)
	    fprintf (stderr, 
		"warning: skipped %ld bytes of junk", bytes_skipped);
	  if ((ret = taru_read_in_new_ascii(taru, file_hdr, in_des,
						archive_format_in0)) < 0)  {
		E_DEBUG("LEAVING");
	  	return -retval;
	  }
          if (!strcmp(ahsStaticGetTarFilename(file_hdr),
			CPIO_INBAND_EOA_FILENAME)) 
	  	if (p_eoa)
			*p_eoa = 1;
	  retval += ret;
	  break;
	}
      if ( (archive_format_in0 == arf_oldascii ||
				archive_format_in0 == arf_hpoldascii)
	  			&& !strncmp ((char *) file_hdr, "070707", 6))
	{
          E_DEBUG("");
	  if (bytes_skipped > 0)
	    fprintf  (stderr , 
			"warning: skipped %ld bytes of junk", bytes_skipped);
	  ret = taru_read_in_old_ascii2(taru, file_hdr, in_des, (char*)(NULL));
	  if (ret <= 0) {
		E_DEBUG("LEAVING");
		return -retval;
	  }
          if (!strcmp(ahsStaticGetTarFilename(file_hdr),
			CPIO_INBAND_EOA_FILENAME)) 
	  	if (p_eoa)
			*p_eoa = 1;
	  retval += ret;
	  break;
	}
      if ((archive_format_in0 == arf_binary ||
			archive_format_in0 == arf_hpbinary)
	  && (file_hdr->c_magic == 070707
	      || file_hdr->c_magic == swab_short ((unsigned short) 070707)))
	{
          E_DEBUG("");
	  /* Having to skip 1 byte because of word alignment is normal.  */
	  fprintf(stderr, "arf_binary arf_hpbinary not supported.\n");
	  break; 
	  /* 
	  if (bytes_skipped > 0)
	    fprintf  (stderr,
			"warning: skipped %ld bytes of junk", bytes_skipped);
	  ret = taru_read_in_binary(file_hdr, in_des);
	  if (ret <= 0) {
		E_DEBUG("LEAVING");
		return -retval;
	  }
	  retval += ret;
	  break;
	  */
	}
      bytes_skipped++;
      memmove ((char *) file_hdr, (char *) file_hdr + 1, 5); 
      if ((ret = taru_tape_buffered_read(in_des, (void*) ((char*)file_hdr + 5), (size_t)(1))) <= 0) {
	    fprintf  (stderr, 
		"error: header magic not found and subsequent read error.\n");
	    E_DEBUG("LEAVING");
            return -retval;
      }
      retval += ret;
    }
  E_DEBUG("LEAVING");
  return retval;
}

