/* Copyright (C) 1998 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: sdcparam.h,v $ $Revision: 1.2.2.1 $ */
/* DCT filter parameter setting and reading interface */

#ifndef sdcparam_INCLUDED
#  define sdcparam_INCLUDED

/*
 * All of these procedures are defined in sdcparam.c and are only for
 * internal use (by sddparam.c and sdeparam.c), so they are not
 * documented here.
 */

int s_DCT_get_params(P3(gs_param_list * plist, const stream_DCT_state * ss,
			const stream_DCT_state * defaults));
int s_DCT_get_quantization_tables(P4(gs_param_list * plist,
				     const stream_DCT_state * pdct,
				     const stream_DCT_state * defaults,
				     bool is_encode));
int s_DCT_get_huffman_tables(P4(gs_param_list * plist,
				const stream_DCT_state * pdct,
				const stream_DCT_state * defaults,
				bool is_encode));

int s_DCT_byte_params(P5(gs_param_list * plist, gs_param_name key, int start,
			 int count, UINT8 * pvals));
int s_DCT_put_params(P2(gs_param_list * plist, stream_DCT_state * pdct));
int s_DCT_put_quantization_tables(P3(gs_param_list * plist,
				     stream_DCT_state * pdct,
				     bool is_encode));
int s_DCT_put_huffman_tables(P3(gs_param_list * plist, stream_DCT_state * pdct,
				bool is_encode));

#endif /* sdcparam_INCLUDED */
