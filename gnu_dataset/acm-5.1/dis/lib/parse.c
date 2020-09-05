/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1996-1998, Riley Rainey (rrainey@ix.netcom.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <dis/dis.h>
#include <string.h>
#include <stdlib.h>

#define DMAX 16  /* maximum number of delimiter chars */
#define BMAX 64  /* maximum incoming string length */

/*
 *  Generate a DIS entity ID from a string
 *
 *  C-style hexidecimal numbers may be used in the input stream:
 *
 *    1/1/0xfffe, and 0xff/0xff/0x1 are both valid
 *
 *  Example invalid strings:
 *
 *    1/1/1,000    Entity ID field contains an invalid character
 *    1/1/1000000  Entity ID field > 0xffff
 *
 *  Return values:
 *
 *    0 success
 *    1 parse error
 *    2 incoming string buffer too large; (max is 64 characters)
 *    3 one or more of the fields contains an invalid value (<0 or >0xffff)
 *    4 invalid character in string
 */

int
DISParseEntityID (dis_entity_id *p, 
				  const char * buf, 
				  int bufsize,
				  const char *delim)
{
	char pdelim[DMAX+1];
	char tbuf[BMAX+1];
	char *cur, *next, *endptr;
	long rval;
	int result = 1;

	memset ( p, 0, sizeof(dis_entity_id));

	/*
	 *  Buffer too large?
	 */

	if (bufsize > BMAX ) {
		return 2;
	}

	strncpy ( tbuf, buf, BMAX );
	tbuf[BMAX] = '\0';

	if (delim) {
		strncpy(pdelim, delim, DMAX);
		pdelim[DMAX] = '\0';
	}
	else {
		strcpy( pdelim, ":./" );
	}

	cur = tbuf;

	next = strpbrk ( cur, pdelim );

	if ( next != NULL ) {

		/*
		 * Once we get a delimiter, all other delimeters must match
		 */

		pdelim[0] = *next;
		pdelim[1] = '\0';

		/*
		 *  Get Site ID
		 */

		endptr = next;
		rval = strtol ( cur, &endptr, 0 );

		if (rval < 0 || rval > 0xffff) {
			return 3;
		}
		else {
			p->sim_id.site_id = (unsigned short) rval;
		}

		/*
		 *  Ensure strtol stopped parsing at the correct spot
		 */

		if ( endptr != next ) {
			return 4;
		}

		cur = next+1;

		next = strpbrk ( cur, pdelim );

		if ( next != NULL ) {

			/*
			 *  Get application ID
			 */

			endptr = next;
			rval = strtol ( cur, &endptr, 0 );

			if (rval < 0 || rval > 0xffff) {
				return 3;
			}
			else {
				p->sim_id.application_id = (unsigned short) rval;
			}

			/*
			 *  Ensure strtol stopped parsing at the correct spot
			 */

			if ( endptr != next ) {
				return 4;
			}

			/*
			 *  Get Entity ID
			 */

			cur = next+1;

			rval = strtol ( cur, NULL, 0 );

			if (rval < 0 || rval > 0xffff) {
				return 3;
			}
			else {
				p->entity_id = (unsigned short) rval;
			}

			result = 0;

		}
	}

	return result;
}







