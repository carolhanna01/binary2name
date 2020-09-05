/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1997
*
*	File:	FileJPEG.cpp
*
******************************************************************/

#ifdef SUPPORT_JPEG

#include <ctype.h>
extern "C" {
#include "cdjpeg.h"
}
#include "FileJPEG.h"

static const char * const cdjpeg_message_table[] = {
#include "cderror.h"
  NULL
};

METHODDEF(void) ErrorExit(j_common_ptr cinfo)
{
  (*cinfo->err->output_message) (cinfo);
}

FileJPEG::FileJPEG(char *filename)
{	
	imgBuffer = NULL;
	width = height = 0;
	
	load(filename);
}

bool FileJPEG::load(char *filename)
{	
	imgBuffer = NULL;
	width = height = 0;

	struct jpeg_decompress_struct cinfo;
	struct jpeg_error_mgr jerr;

	djpeg_dest_ptr dest_mgr = NULL;

	/* Initialize the JPEG decompression object with default error handling. */
	jpeg_std_error(&jerr);
	jerr.error_exit = ErrorExit;
	cinfo.err = &jerr;
	jpeg_create_decompress(&cinfo);

	/* Add some application-specific error messages (from cderror.h) */
	jerr.addon_message_table = cdjpeg_message_table;
	jerr.first_addon_message = JMSG_FIRSTADDONCODE;
	jerr.last_addon_message = JMSG_LASTADDONCODE;

	FILE *fp = fopen(filename, READ_BINARY);
	if (!fp) 
		return false;

	/* Specify data source for decompression */
	jpeg_stdio_src(&cinfo, fp);

	/* Read file header, set default decompression parameters */
	jpeg_read_header(&cinfo, TRUE);

//	if (cinfo.err->msg_code != 0)
//		return false;

	width = cinfo.image_width;
	height = cinfo.image_height;
	imgBuffer = new RGBColor[width*height];

	/* Start decompressor */
	jpeg_start_decompress(&cinfo);

	/* Process data */
	unsigned char	**buffer = new unsigned char *[1]; 
	int				scanline = 0;

	while (cinfo.output_scanline < cinfo.output_height) {
		buffer[0] = (unsigned char *)imgBuffer[width*scanline];
		jpeg_read_scanlines(&cinfo, buffer, 1);
		scanline++;
	}

	jpeg_finish_decompress(&cinfo);
	jpeg_destroy_decompress(&cinfo);

	/* Close files, if we opened them */
	if (fp)
		fclose(fp);

	return true;
}

FileJPEG::~FileJPEG()
{
	if (imgBuffer)
		delete []imgBuffer;
}

#endif
