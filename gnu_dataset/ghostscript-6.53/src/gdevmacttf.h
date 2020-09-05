/* Copyright (c) 2000 artofcode LLC.  All rights reserved.

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


/* Datatypes for simpler access to data structures in TrueType fonts */


/* Font Directory Component */

typedef struct {
	UInt32		tagName;
	UInt32		checkSum;
	UInt32		offset;
	UInt32		length;
} TTFontDirComponent;



/* Font Directory */

typedef struct {
	UInt32				version;
	UInt16				numTables;
	UInt16				searchRange;
	UInt16				entrySelector;
	UInt16				rangeShift;
	TTFontDirComponent	components[1];	/* an array of numTables components */
} TTFontDir;



/* Tag definitions */
#define		TTF_FONT_NAMING_TABLE	'name'



/* Font Naming Table */

typedef struct {
	UInt16				formatSelector;
	UInt16				numNames;
	UInt16				stringAreaOffset;
	UInt16				platformID;
	UInt16				platformSpecificID;
	UInt16				languageID;
	UInt16				nameID;
	UInt16				length;
	UInt16				offset;
} TTFontNamingTable;

