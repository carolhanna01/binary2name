/* Copyright (C) 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gxshade4.h,v $ $Revision: 1.2.2.1 $ */
/* Internal definitions for triangle shading rendering */

#ifndef gxshade4_INCLUDED
#  define gxshade4_INCLUDED

#define mesh_max_depth (16 * 3 + 1)	/* each recursion adds 3 entries */
typedef struct mesh_frame_s {	/* recursion frame */
    mesh_vertex_t va, vb, vc;	/* current vertices */
    bool check_clipping;
} mesh_frame_t;
/****** NEED GC DESCRIPTOR ******/

/*
 * Define the fill state structure for triangle shadings.  This is used
 * both for the Gouraud triangle shading types and for the Coons and
 * tensor patch types.
 *
 * The shading pointer is named pshm rather than psh in case subclasses
 * also want to store a pointer of a more specific type.
 */
#define mesh_fill_state_common\
  shading_fill_state_common;\
  const gs_shading_mesh_t *pshm;\
  gs_fixed_rect rect;\
  int depth;\
  mesh_frame_t frames[mesh_max_depth]
typedef struct mesh_fill_state_s {
    mesh_fill_state_common;
} mesh_fill_state_t;
/****** NEED GC DESCRIPTOR ******/

/* Initialize the fill state for triangle shading. */
void mesh_init_fill_state(P5(mesh_fill_state_t * pfs,
			     const gs_shading_mesh_t * psh,
			     const gs_rect * rect,
			     gx_device * dev, gs_imager_state * pis));

/* Fill one triangle in a mesh. */
void mesh_init_fill_triangle(P5(mesh_fill_state_t * pfs,
				const mesh_vertex_t *va,
				const mesh_vertex_t *vb,
				const mesh_vertex_t *vc, bool check_clipping));
int mesh_fill_triangle(P1(mesh_fill_state_t * pfs));

#endif /* gxshade4_INCLUDED */
