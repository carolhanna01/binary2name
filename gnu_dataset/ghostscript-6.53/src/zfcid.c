/* Copyright (C) 1996, 1997, 1998, 1999, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: zfcid.c,v $ $Revision: 1.10.2.2 $ */
/* CID-keyed font utilities */
#include "ghost.h"
#include "oper.h"
#include "gsmatrix.h"
#include "gxfcid.h"
#include "bfont.h"
#include "icid.h"
#include "idict.h"
#include "idparam.h"
#include "ifcid.h"
#include "store.h"

/* Get the CIDSystemInfo of a CIDFont. */
int
cid_font_system_info_param(gs_cid_system_info_t *pcidsi, const ref *prfont)
{
    ref *prcidsi;

    if (dict_find_string(prfont, "CIDSystemInfo", &prcidsi) <= 0)
	return_error(e_rangecheck);
    return cid_system_info_param(pcidsi, prcidsi);
}

/* Get the additional information for a CIDFontType 0 or 2 CIDFont. */
int
cid_font_data_param(os_ptr op, gs_font_cid_data *pdata, ref *pGlyphDirectory)
{
    int code;
    ref *pgdir;

    check_type(*op, t_dictionary);
    if ((code = cid_font_system_info_param(&pdata->CIDSystemInfo, op)) < 0 ||
	(code = dict_int_param(op, "CIDCount", 0, max_int, -1,
			       &pdata->CIDCount)) < 0
	)
	return code;
    /*
     * If the font doesn't have a GlyphDirectory, GDBytes is required.
     * If it does have a GlyphDirectory, GDBytes may still be needed for
     * CIDMap: it's up to the client to check this.
     */
    if (dict_find_string(op, "GlyphDirectory", &pgdir) <= 0) {
	/* Standard CIDFont, require GDBytes. */
	make_null(pGlyphDirectory);
	return dict_int_param(op, "GDBytes", 1, MAX_GDBytes, 0,
			      &pdata->GDBytes);
    }
    if (r_has_type(pgdir, t_dictionary) || r_is_array(pgdir)) {
	/* GlyphDirectory, GDBytes is optional. */
	*pGlyphDirectory = *pgdir;
	code = dict_int_param(op, "GDBytes", 1, MAX_GDBytes, 1,
			      &pdata->GDBytes);
	if (code == 1) {
	    pdata->GDBytes = 0;
	    code = 0;
	}
	return code;
    } else {
	return_error(e_typecheck);
    }
}
