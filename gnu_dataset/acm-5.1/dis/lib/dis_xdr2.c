/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1996, Riley Rainey (rainey@netcom.com)
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

#include <rpc/types.h>
#include <rpc/xdr.h>
#include <sys/types.h>
#ifndef WIN32
#include <netinet/in.h>
#endif

bool_t    xdr_byte_opaque(XDR * xdrs, caddr_t cp, u_int cnt);

/*
 *  These xdr_byte_ routines act just as their xdr_ equivalents except that
 *  input data is not required to be aligned on a 4-byte boundary.  This
 *  implies that data alignment is the responsibility of the protocol
 *  designer.
 */

bool_t
xdr_byte_long(XDR * xdrs, void *vcp)
{
	long      u;
	long int *cp = (long int *) vcp;

	u = htonl(*cp);
	if (!xdr_byte_opaque(xdrs, (caddr_t) & u, 4)) {
		return (FALSE);
	}
	*cp = ntohl(u);
	return (TRUE);
}

bool_t
xdr_byte_u_long(XDR * xdrs, void * vcp)
{
	u_long    u, *cp = (u_long *) vcp;

	u = htonl(*cp);
	if (!xdr_byte_opaque(xdrs, (caddr_t) & u, 4)) {
		return (FALSE);
	}
	*cp = ntohl(u);
	return (TRUE);
}

bool_t
xdr_byte_short(XDR * xdrs, void *vcp)
{
	short     u;
	short	*cp = (short *) vcp;

	u = htons(*cp);
	if (!xdr_byte_opaque(xdrs, (caddr_t) & u, 2)) {
		return (FALSE);
	}
	*cp = ntohs(u);
	return (TRUE);
}

bool_t
xdr_byte_u_short(XDR * xdrs, void * vcp)
{
	u_short   u;
	u_short	*cp = (u_short *) vcp;

	u = htons(*cp);
	if (!xdr_byte_opaque(xdrs, (caddr_t) & u, 2)) {
		return (FALSE);
	}
	*cp = ntohs(u);
	return (TRUE);
}

bool_t
xdr_byte_char(XDR * xdrs, void *vcp)
{
	char      i, *cp = (char *) vcp;

	i = (*cp);
	if (!xdr_byte_opaque(xdrs, &i, 1)) {
		return (FALSE);
	}
	*cp = i;
	return (TRUE);
}

bool_t
xdr_byte_u_char(XDR * xdrs, void * vcp)
{
	u_char    u, *cp = (u_char *) vcp;

	u = (*cp);
	if (!xdr_byte_opaque(xdrs, &u, 1)) {
		return (FALSE);
	}
	*cp = u;
	return (TRUE);
}

bool_t
xdr_byte_opaque(register XDR * xdrs, caddr_t cp, register u_int cnt)
{

	/*
	 * if no data we are done
	 */
	if (cnt == 0)
		return (TRUE);

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETBYTES(xdrs, cp, cnt)) {
			return (FALSE);
		}
		return (TRUE);
	}

	if (xdrs->x_op == XDR_ENCODE) {
		if (!XDR_PUTBYTES(xdrs, cp, cnt)) {
			return (FALSE);
		}
		return (TRUE);
	}

	if (xdrs->x_op == XDR_FREE) {
		return (TRUE);
	}

	return (FALSE);
}
