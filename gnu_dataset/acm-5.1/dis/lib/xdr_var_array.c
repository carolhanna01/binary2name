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
/*static char *sccsid = "from: @(#)xdr_array.c 1.10 87/08/11 Copyr 1984 Sun Micro"; */
/*static char *sccsid = "from: @(#)xdr_array.c  2.1 88/07/29 4.0 RPCSRC"; */
static char *rcsid = "$Id: xdr_var_array.c,v 1.1.1.1 2005/10/28 14:46:47 k0ro Exp $";

#endif

/*
 * xdr_var_array.c from :
 *
 * xdr_array.c, Generic XDR routines implementation.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * These are the "non-trivial" xdr primitives used to serialize and de-serialize
 * arrays.  See xdr.h for more info on the interface to xdr.
 */

#include <stdio.h>
#include <stdlib.h>
#include <rpc/types.h>
#include <rpc/xdr.h>

#define LASTUNSIGNED	((u_int)0-1)

extern int fprintf(FILE *, const char *,...);

/*
 * XDR an array of arbitrary elements
 * *addrp is a pointer to the array, size is the number of elements.
 * If addrp is NULL (sizep * elsize) bytes are allocated.
 * elsize is the size (in bytes) of each element, and elproc is the
 * xdr procedure to call to handle each element of the array.
 */
bool_t
xdr_var_array(register XDR * xdrs, caddr_t * addrp, u_int size, u_int maxsize, u_int elsize, xdrproc_t elproc)
				/* array pointer */
				/* number of elements */
				/* max numberof elements */
				/* size in bytes of each element */
				/* xdr routine to handle each element */
{
	register u_int i;
	register caddr_t target = *addrp;
	register u_int c;			/* the actual element count */
	register bool_t stat = TRUE;
	register u_int nodesize;

	c = size;
	if ((c > maxsize) && (xdrs->x_op != XDR_FREE)) {
		return (FALSE);
	}
	nodesize = c * elsize;

	/*
	 * if we are deserializing, we may need to allocate an array.
	 * We also save time by checking for a null array if we are freeing.
	 *
	 * * Always allocate storage when decoding. *
	 */
	if (target == NULL || xdrs->x_op == XDR_DECODE)
		switch (xdrs->x_op) {
		case XDR_DECODE:
			if (c == 0)
				return (TRUE);
			*addrp = target = mem_alloc(nodesize);
			if (target == NULL) {
				(void) fprintf(stderr,
							   "xdr_array: out of memory\n");
				return (FALSE);
			}
			bzero(target, nodesize);
			break;

		case XDR_FREE:
			return (TRUE);

		case XDR_ENCODE:
			break;
		}

	/*
	 * now we xdr each element of array
	 */
	for (i = 0; (i < c) && stat; i++) {
		stat = (*elproc) (xdrs, target, LASTUNSIGNED);
		target += elsize;
	}

	/*
	 * the array may need freeing
	 */
	if (xdrs->x_op == XDR_FREE) {
		mem_free(*addrp, nodesize);
		*addrp = NULL;
	}
	return (stat);
}
