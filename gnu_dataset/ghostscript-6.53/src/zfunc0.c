/* Copyright (C) 1997, 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: zfunc0.c,v $ $Revision: 1.3.2.1 $ */
/* PostScript language interface to FunctionType 0 (Sampled) Functions */
#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsdsrc.h"
#include "gsfunc.h"
#include "gsfunc0.h"
#include "stream.h"		/* for files.h */
#include "files.h"
#include "ialloc.h"
#include "idict.h"
#include "idparam.h"
#include "ifunc.h"

/* Check prototype */
build_function_proc(gs_build_function_0);

/* Finish building a FunctionType 0 (Sampled) function. */
int
gs_build_function_0(const ref *op, const gs_function_params_t * mnDR,
		    int depth, gs_function_t ** ppfn, gs_memory_t *mem)
{
    gs_function_Sd_params_t params;
    ref *pDataSource;
    int code;

    *(gs_function_params_t *) & params = *mnDR;
    params.Encode = 0;
    params.Decode = 0;
    params.Size = 0;
    if ((code = dict_find_string(op, "DataSource", &pDataSource)) <= 0)
	return (code < 0 ? code : gs_note_error(e_rangecheck));
    switch (r_type(pDataSource)) {
	case t_string:
	    data_source_init_string2(&params.DataSource,
				     pDataSource->value.const_bytes,
				     r_size(pDataSource));
	    break;
	case t_file: {
	    stream *s;

	    check_read_known_file_else(s, pDataSource, return_error,
				       return_error(e_invalidfileaccess));
	    if (!(s->modes & s_mode_seek))
		return_error(e_ioerror);
	    data_source_init_stream(&params.DataSource, s);
	    break;
	}
	default:
	    return_error(e_rangecheck);
    }
    if ((code = dict_int_param(op, "Order", 1, 3, 1, &params.Order)) < 0 ||
	(code = dict_int_param(op, "BitsPerSample", 1, 32, 0,
			       &params.BitsPerSample)) < 0 ||
	((code = fn_build_float_array(op, "Encode", false, true, &params.Encode, mem)) != 2 * params.m && (code != 0 || params.Encode != 0)) ||
	((code = fn_build_float_array(op, "Decode", false, true, &params.Decode, mem)) != 2 * params.n && (code != 0 || params.Decode != 0))
	) {
	goto fail;
    } {
	int *ptr = (int *)
	    gs_alloc_byte_array(mem, params.m, sizeof(int), "Size");

	if (ptr == 0) {
	    code = gs_note_error(e_VMerror);
	    goto fail;
	}
	params.Size = ptr;
	code = dict_ints_param(op, "Size", params.m, ptr);
	if (code != params.m)
	    goto fail;
    }
    code = gs_function_Sd_init(ppfn, &params, mem);
    if (code >= 0)
	return 0;
fail:
    gs_function_Sd_free_params(&params, mem);
    return (code < 0 ? code : gs_note_error(e_rangecheck));
}
