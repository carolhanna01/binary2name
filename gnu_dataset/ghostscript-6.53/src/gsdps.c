/* Copyright (C) 1997, 1998 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gsdps.c,v $ $Revision: 1.2.2.1 $ */
/* Display PostScript extensions */
#include "gx.h"
#include "gserrors.h"
#include "gsdps.h"
#include "gspath.h"		/* for newpath */
#include "gxdevice.h"		/* for gxcpath.h */
#include "gzpath.h"		/* for gzcpath.h */
#include "gzstate.h"
#include "gzcpath.h"

/* ---------------- View clipping ---------------- */

/* Forward references */
private int common_viewclip(P2(gs_state *, int));

int
gs_initviewclip(gs_state * pgs)
{
    gx_clip_path *pcpath = pgs->view_clip;

    if (pcpath != 0 && pcpath->rule != 0) {
	gx_cpath_reset(pcpath);
	pcpath->rule = 0;
    }
    return 0;
}

int
gs_viewclip(gs_state * pgs)
{
    return common_viewclip(pgs, gx_rule_winding_number);
}

int
gs_eoviewclip(gs_state * pgs)
{
    return common_viewclip(pgs, gx_rule_even_odd);
}

/* This code is (almost) copied from common_clip in gspath.c. */
/* Someday we'll find a way to merge them. */
private int
common_viewclip(gs_state * pgs, int rule)
{
    gs_fixed_rect bbox;
    gx_clip_path rpath;
    int code;
    gx_clip_path *pcpath = pgs->view_clip;

    if (pcpath == 0) {
	pcpath = gx_cpath_alloc(pgs->memory, "gs_[eo]viewclip");
	if (pcpath == 0)
	    return_error(gs_error_VMerror);
	pgs->view_clip = pcpath;
    }
    if ((code = gx_path_bbox(pgs->path, &bbox)) < 0)
	return code;
    gx_cpath_init_local(&rpath, pgs->memory);
    code = gx_cpath_from_rectangle(&rpath, &bbox);
    if (code >= 0)
	code = gx_cpath_clip(pgs, &rpath, pgs->path, rule);
    if (code < 0) {
	gx_cpath_free(&rpath, "gs_[eo]viewclip");
	return code;
    }
    rpath.rule = rule;
    gx_cpath_assign_free(pcpath, &rpath);
    gs_newpath(pgs);
    return 0;
}

int
gs_viewclippath(gs_state * pgs)
{
    gx_path cpath;
    gx_clip_path *pcpath = pgs->view_clip;
    int code;

    gx_path_init_local(&cpath, pgs->memory);
    if (pcpath == 0 || pcpath->rule == 0) {
	/* No view clip path is active: fabricate one. */
	gs_fixed_rect box;

	code = gx_default_clip_box(pgs, &box);
	if (code < 0)
	    return code;
	code = gx_path_add_rectangle(&cpath, box.p.x, box.p.y,
				     box.q.x, box.q.y);
    } else {
	code = gx_cpath_to_path(pcpath, &cpath);
    }
    if (code < 0)
	return code;
    return gx_path_assign_free(pgs->path, &cpath);
}
