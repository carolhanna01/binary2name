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

/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#if defined(LIBC_SCCS) && !defined(lint)
/*static char *sccsid = "from: @(#)xdr_mem.c 1.19 87/08/11 Copyr 1984 Sun Micro"; */
/*static char *sccsid = "from: @(#)xdr_mem.c    2.1 88/07/29 4.0 RPCSRC"; */
static char *rcsid = "$Id: xdr_umem.c,v 1.1.1.1 2005/10/28 14:46:47 k0ro Exp $";

#endif

/*
 * xdr_mem.h, XDR implementation using memory buffers.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * If you have some data to be interpreted as external data representation
 * or to be converted to external data representation in a memory buffer,
 * then this is the package for you.
 *
 */

#include <rpc/types.h>
#include <rpc/xdr.h>
#ifndef WIN32
#include <netinet/in.h>
#endif

static bool_t xdrumem_getlong(register XDR * xdrs, long int *lp);
static bool_t xdrumem_putlong(register XDR * xdrs, long int *lp);
static bool_t xdrumem_getbytes(register XDR * xdrs, caddr_t addr, register u_int len);
static bool_t xdrumem_putbytes(register XDR * xdrs, caddr_t addr, register u_int len);
static u_int xdrumem_getpos(register XDR * xdrs);
static bool_t xdrumem_setpos(register XDR * xdrs, u_int pos);
static long *xdrumem_inline(register XDR * xdrs, int len);
static void xdrumem_destroy(void);

static struct xdr_ops xdrumem_ops =
{
	xdrumem_getlong,
	xdrumem_putlong,
	xdrumem_getbytes,
	xdrumem_putbytes,
	xdrumem_getpos,
	xdrumem_setpos,
	xdrumem_inline,
	xdrumem_destroy
};

/*
 * The procedure xdrumem_create initializes a stream descriptor for a
 * memory buffer.  
 */
void
xdrumem_create(register XDR * xdrs, caddr_t addr, u_int size, enum xdr_op op)
{

	xdrs->x_op = op;
	xdrs->x_ops = &xdrumem_ops;
	xdrs->x_private = xdrs->x_base = addr;
	xdrs->x_handy = size;
}

static void
xdrumem_destroy(void)
	/*XDR *xdrs; */
{
}

static    bool_t
xdrumem_getlong(register XDR * xdrs, long int *lp)
{
	long      tmp;

	if ((xdrs->x_handy -= sizeof(long)) < 0)
		          return (FALSE);
	bcopy(xdrs->x_private, (char *) &tmp, sizeof(long));

	*lp = (long) ntohl((u_long) tmp);
	xdrs->x_private += sizeof(long);

	return (TRUE);
}

static    bool_t
xdrumem_putlong(register XDR * xdrs, long int *lp)
{
	long      tmp;

	if ((xdrs->x_handy -= sizeof(long)) < 0)
		          return (FALSE);

	tmp = (long) htonl((u_long) (*lp));
	bcopy((char *) &tmp, xdrs->x_private, sizeof(long));
	xdrs->x_private += sizeof(long);

	return (TRUE);
}

static    bool_t
xdrumem_getbytes(register XDR * xdrs, caddr_t addr, register u_int len)
{

	if ((xdrs->x_handy -= len) < 0)
		return (FALSE);
	bcopy(xdrs->x_private, addr, len);
	xdrs->x_private += len;
	return (TRUE);
}

static    bool_t
xdrumem_putbytes(register XDR * xdrs, caddr_t addr, register u_int len)
{

	if ((xdrs->x_handy -= len) < 0)
		return (FALSE);
	bcopy(addr, xdrs->x_private, len);
	xdrs->x_private += len;
	return (TRUE);
}

static    u_int
xdrumem_getpos(register XDR * xdrs)
{

	return ((u_int) xdrs->x_private - (u_int) xdrs->x_base);
}

static    bool_t
xdrumem_setpos(register XDR * xdrs, u_int pos)
{
	register caddr_t newaddr = xdrs->x_base + pos;
	register caddr_t lastaddr = xdrs->x_private + xdrs->x_handy;

	if ((long) newaddr > (long) lastaddr)
		return (FALSE);
	xdrs->x_private = newaddr;
	xdrs->x_handy = (int) lastaddr - (int) newaddr;
	return (TRUE);
}

static long *
xdrumem_inline(register XDR * xdrs, int len)
{
	long     *buf = 0;

	if (xdrs->x_handy >= len) {
		xdrs->x_handy -= len;
		buf = (long *) xdrs->x_private;
		xdrs->x_private += len;
	}
	return (buf);
}
