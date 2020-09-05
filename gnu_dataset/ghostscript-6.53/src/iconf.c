/* Copyright (C) 1995, 1996, 1997, 1998, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: iconf.c,v $ $Revision: 1.2.2.1 $ */
/* Configuration-dependent tables and initialization for interpreter */
#include "stdio_.h"		/* stdio for stream.h */
#include "gstypes.h"
#include "gsmemory.h"		/* for iminst.h */
#include "gconf.h"
#include "iref.h"
#include "ivmspace.h"
#include "opdef.h"
#include "ifunc.h"
#include "iminst.h"

/* Define the default values for an interpreter instance. */
const gs_main_instance gs_main_instance_init_values =
{gs_main_instance_default_init_values};

/* Set up the .ps file name string array. */
/* We fill in the lengths at initialization time. */
#define ref_(t) struct { struct tas_s tas; t value; }
#define string_(s,len)\
 { { (t_string<<r_type_shift) + a_readonly + avm_foreign, len }, s },
#define psfile_(fns,len) string_(fns,len)
const ref_(const char *) gs_init_file_array[] = {
#include "gconf.h"
    string_(0, 0)
};
#undef psfile_

/* Set up the emulator name string array similarly. */
#define emulator_(ems,len) string_(ems,len)
const ref_(const char *) gs_emulator_name_array[] = {
#include "gconf.h"
    string_(0, 0)
};
#undef emulator_

/* Set up the function type table similarly. */
#define function_type_(i,proc) extern build_function_proc(proc);
#include "gconf.h"
#undef function_type_
#define function_type_(i,proc) {i,proc},
const build_function_type_t build_function_type_table[] = {
#include "gconf.h"
    {0}
};
#undef function_type_
const uint build_function_type_table_count =
    countof(build_function_type_table) - 1;

/* Initialize the operators. */
	/* Declare the externs. */
#define oper_(xx_op_defs) extern const op_def xx_op_defs[];
oper_(interp_op_defs)		/* Interpreter operators */
#include "gconf.h"
#undef oper_
 
const op_def *const op_defs_all[] = {
#define oper_(defs) defs,
    oper_(interp_op_defs)	/* Interpreter operators */
#include "gconf.h"
#undef oper_ 
    0
};
const uint op_def_count = (countof(op_defs_all) - 1) * OP_DEFS_MAX_SIZE;
