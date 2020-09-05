/* Copyright (C) 1989, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: ifont.h,v $ $Revision: 1.4.2.1 $ */
/* Interpreter internal font representation */

#ifndef ifont_INCLUDED
#  define ifont_INCLUDED

#include "gsccode.h"		/* for gs_glyph, NUM_KNOWN_ENCODINGS */
#include "gsstype.h"		/* for extern_st */

/* The external definition of fonts is given in the PostScript manual, */
/* pp. 91-93. */

/* The structure given below is 'client data' from the viewpoint */
/* of the library.  font-type objects (t_struct/st_font, "t_fontID") */
/* point directly to a gs_font.  */

typedef struct font_data_s {
    ref dict;			/* font dictionary object */
    ref BuildChar;
    ref BuildGlyph;
    ref Encoding;
    ref CharStrings;
    union _fs {
	struct _f1 {
	    ref OtherSubrs;	/* from Private dictionary */
	    ref Subrs;		/* from Private dictionary */
	    ref GlobalSubrs;	/* from Private dictionary, */
	    /* for Type 2 charstrings */
	} type1;
	struct _f42 {
	    ref sfnts;
	    ref CIDMap;		/* for CIDFontType 2 fonts */
	    ref GlyphDirectory;
	} type42;
	struct _fc0 {
	    ref GlyphDirectory;
	    ref GlyphData;	/* (if preloaded) string or array of strings */
	    ref DataSource;	/* (if not preloaded) reusable stream */
	} cid0;
    } u;
} font_data;

/*
 * Even though the interpreter's part of the font data actually
 * consists of refs, allocating it as refs tends to create sandbars;
 * since it is always allocated and freed as a unit, we can treat it
 * as an ordinary structure.
 */
/* st_font_data is exported for zdefault_make_font in zfont.c. */
extern_st(st_font_data);
#define public_st_font_data()	/* in zbfont.c */\
  gs_public_st_ref_struct(st_font_data, font_data, "font_data")
#define pfont_data(pfont) ((font_data *)((pfont)->client_data))
#define pfont_dict(pfont) (&pfont_data(pfont)->dict)

/* Registered encodings, for the benefit of platform fonts, `seac', */
/* and compiled font initialization. */
/* This is a t_array ref that points to the encodings. */
#define registered_Encodings_countof NUM_KNOWN_ENCODINGS
extern ref registered_Encodings;

#define registered_Encoding(i) (registered_Encodings.value.refs[i])
#define StandardEncoding registered_Encoding(0)

/* Internal procedures shared between modules */

/* In zchar.c */
int font_bbox_param(P2(const ref * pfdict, double bbox[4]));

/* In zfont.c */
#ifndef gs_font_DEFINED
#  define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
int font_param(P2(const ref * pfdict, gs_font ** ppfont));
bool zfont_mark_glyph_name(P2(gs_glyph glyph, void *ignore_data));

#endif /* ifont_INCLUDED */
