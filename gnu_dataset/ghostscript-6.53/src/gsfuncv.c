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

/*$RCSfile: gsfuncv.c,v $ $Revision: 1.2.2.1 $ */
/* "Vanilla" Function support */
#include "gx.h"
#include "gserrors.h"
#include "gsfuncv.h"
#include "gsparam.h"
#include "gxfunc.h"

/* GC descriptor */
private_st_function_Va();

/*
 * Test whether a Vanilla function is monotonic.  (This information is
 * provided at function definition time.)
 */
private int
fn_Va_is_monotonic(const gs_function_t * pfn_common,
		   const float *lower, const float *upper,
		   gs_function_effort_t effort)
{
    const gs_function_Va_t *const pfn =
	(const gs_function_Va_t *)pfn_common;

    return pfn->params.is_monotonic;
}

/* Free the parameters of a Vanilla function. */
void
gs_function_Va_free_params(gs_function_Va_params_t * params,
			   gs_memory_t * mem)
{
    gs_free_object(mem, params->eval_data, "eval_data");
    fn_common_free_params((gs_function_params_t *) params, mem);
}

/* Allocate and initialize a Vanilla function. */
int
gs_function_Va_init(gs_function_t ** ppfn,
		    const gs_function_Va_params_t * params,
		    gs_memory_t * mem)
{
    static const gs_function_head_t function_Va_head = {
	function_type_Vanilla,
	{
	    NULL,			/* filled in from params */
	    (fn_is_monotonic_proc_t) fn_Va_is_monotonic,
	    gs_function_get_info_default,
	    fn_common_get_params,	/****** WHAT TO DO ABOUT THIS? ******/
	    (fn_free_params_proc_t) gs_function_Va_free_params,
	    fn_common_free
	}
    };
    int code;

    *ppfn = 0;			/* in case of error */
    code = fn_check_mnDR((const gs_function_params_t *)params, 1, params->n);
    if (code < 0)
	return code;
    {
	gs_function_Va_t *pfn =
	    gs_alloc_struct(mem, gs_function_Va_t, &st_function_Va,
			    "gs_function_Va_init");

	if (pfn == 0)
	    return_error(gs_error_VMerror);
	pfn->params = *params;
	pfn->head = function_Va_head;
	pfn->head.procs.evaluate = params->eval_proc;
	pfn->head.is_monotonic = params->is_monotonic;
	*ppfn = (gs_function_t *) pfn;
    }
    return 0;
}
