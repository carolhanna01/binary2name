/*------------------------->  ANSI C - sourcefile  <-------------------------*/
/* Copyright (C) 1991-93 by Stephen Crane                                    */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

/*
 * bm.c -- benchmark the lwp implementation.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 */

#include <sys/time.h>

#include "lwp.h"
struct sem *s;

void f (int n)
{
	while (n--)
		yieldp ();
	signals (s);
	suicidep ();
}

void g (void)
{
	suicidep ();
}

main ()
{
	int n, t;
	struct timeval ts, te;

	initlp (1);
	s = creats (0);
	creatp (1, f, 4096, 5000, 0, 0);
	creatp (1, f, 4096, 5000, 0, 0);
	gettimeofday (&ts, (struct timezone *)0);
	waits (s);
	waits (s);
	gettimeofday (&te, (struct timezone *)0);
	t = 1000*(te.tv_sec-ts.tv_sec) + (te.tv_usec-ts.tv_usec)/1000;
	free (s);
	printf ("Context switching: %d ms /10000\n", t);

	gettimeofday (&ts, (struct timezone *)0);
	for (n=10000; n--; )
		creatp (2, g, 4096, 0, 0, 0);
	gettimeofday (&te, (struct timezone *)0);
	t = 1000*(te.tv_sec-ts.tv_sec) + (te.tv_usec-ts.tv_usec)/1000;
	printf ("Process creation: %d ms /10000\n", t);
}
