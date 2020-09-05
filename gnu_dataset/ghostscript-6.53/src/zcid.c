/* Copyright (C) 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: zcid.c,v $ $Revision: 1.3.2.1 $ */
/* CMap and CID-keyed font services */
#include "ghost.h"
#include "errors.h"
#include "gxcid.h"
#include "icid.h"		/* for checking prototype */
#include "idict.h"
#include "idparam.h"
#include "oper.h"

/* Get the information from a CIDSystemInfo dictionary. */
int
cid_system_info_param(gs_cid_system_info_t *pcidsi, const ref *prcidsi)
{
    ref *pregistry;
    ref *pordering;
    int code;

    if (!r_has_type(prcidsi, t_dictionary))
	return_error(e_typecheck);
    if (dict_find_string(prcidsi, "Registry", &pregistry) <= 0 ||
	dict_find_string(prcidsi, "Ordering", &pordering) <= 0
	)
	return_error(e_rangecheck);
    check_read_type_only(*pregistry, t_string);
    check_read_type_only(*pordering, t_string);
    pcidsi->Registry.data = pregistry->value.const_bytes;
    pcidsi->Registry.size = r_size(pregistry);
    pcidsi->Ordering.data = pordering->value.const_bytes;
    pcidsi->Ordering.size = r_size(pordering);
    code = dict_int_param(prcidsi, "Supplement", 0, max_int, -1,
			  &pcidsi->Supplement);
    return (code < 0 ? code : 0);
}
