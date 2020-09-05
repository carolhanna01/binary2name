/* Copyright (C) 1996-2002 artofcode LLC.  All rights reserved.
  
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

/* $Id: gscdef.c,v 1.9.2.6 2002/02/11 20:47:56 giles Exp $ */
/* Configuration scalars */

#include "std.h"
#include "gscdefs.h"		/* interface */
#include "gconf.h"		/* for #defines */

/* ---------------- Miscellaneous system parameters ---------------- */

/* All of these can be set in the makefile. */
/* Normally they are all const; see gscdefs.h for more information. */

#ifndef GS_BUILDTIME
#  define GS_BUILDTIME\
	0			/* should be set in the makefile */
#endif
CONFIG_CONST long gs_buildtime = GS_BUILDTIME;

#ifndef GS_COPYRIGHT
#  define GS_COPYRIGHT\
	"Copyright (C) 2002 artofcode LLC, Benicia, CA. All rights reserved."
#endif
const char *CONFIG_CONST gs_copyright = GS_COPYRIGHT;

#ifndef GS_PRODUCTFAMILY
#  define GS_PRODUCTFAMILY\
	"GNU Ghostscript"
#endif
const char *CONFIG_CONST gs_productfamily = GS_PRODUCTFAMILY;

#ifndef GS_PRODUCT
#  define GS_PRODUCT\
	GS_PRODUCTFAMILY ""
#endif
const char *CONFIG_CONST gs_product = GS_PRODUCT;

const char *
gs_program_name(void)
{
    return gs_product;
}

/* GS_REVISION must be defined in the makefile. */
CONFIG_CONST long gs_revision = GS_REVISION;

long
gs_revision_number(void)
{
    return gs_revision;
}

/* GS_REVISIONDATE must be defined in the makefile. */
CONFIG_CONST long gs_revisiondate = GS_REVISIONDATE;

#ifndef GS_SERIALNUMBER
#  define GS_SERIALNUMBER\
	42			/* a famous number */
#endif
CONFIG_CONST long gs_serialnumber = GS_SERIALNUMBER;

/* ---------------- Installation directories and files ---------------- */

/* Here is where the library search path, the name of the */
/* initialization file, and the doc directory are defined. */
/* Define the documentation directory (only used in help messages). */
const char *const gs_doc_directory = GS_DOCDIR;

/* Define the default library search path. */
const char *const gs_lib_default_path = GS_LIB_DEFAULT;

/* Define the interpreter initialization file. */
const char *const gs_init_file = GS_INIT;
