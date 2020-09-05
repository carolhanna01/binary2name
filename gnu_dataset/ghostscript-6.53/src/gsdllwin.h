/* Copyright (C) 1994-1996, Russell Lang.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
  to anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU General Public License for full details.
  
  Everyone is granted permission to copy, modify and redistribute GNU
  Ghostscript, but only under the conditions described in the GNU General
  Public License.  A copy of this license is supposed to have been given
  to you along with GNU Ghostscript so you can know your rights and
  responsibilities.  It should be in a file named COPYING.  Among other
  things, the copyright notice and this notice must be preserved on all
  copies.
*/

/*$RCSfile: gsdllwin.h,v $ $Revision: 1.2.2.1 $ */
/* gsdll extension for Microsoft Windows platforms */

#ifndef gsdllwin_INCLUDED
#  define gsdllwin_INCLUDED

/* DLL exported functions */
/* for load time dynamic linking */
HGLOBAL GSDLLAPI gsdll_copy_dib(unsigned char GSFAR * device);
HPALETTE GSDLLAPI gsdll_copy_palette(unsigned char GSFAR * device);
void GSDLLAPI gsdll_draw(unsigned char GSFAR * device, HDC hdc, LPRECT dest,
			 LPRECT src);
int GSDLLAPI gsdll_get_bitmap_row(unsigned char *device,
				  LPBITMAPINFOHEADER pbmih,
				  LPRGBQUAD prgbquad, LPBYTE * ppbyte,
				  unsigned int row);

/* Function pointer typedefs */
/* for run time dynamic linking */
typedef HGLOBAL (GSDLLAPI * PFN_gsdll_copy_dib)(unsigned char GSFAR *);
typedef HPALETTE (GSDLLAPI * PFN_gsdll_copy_palette)(unsigned char GSFAR *);
typedef void (GSDLLAPI * PFN_gsdll_draw) (unsigned char GSFAR *, HDC, LPRECT,
					  LPRECT);
typedef int (GSDLLAPI * PFN_gsdll_get_bitmap_row)
     (unsigned char *device, LPBITMAPINFOHEADER pbmih, LPRGBQUAD prgbquad,
      LPBYTE * ppbyte, unsigned int row);

#endif /* gsdllwin_INCLUDED */
