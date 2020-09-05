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
 * timer.c -- an example program to demonstrate lightweight
 * processes and time.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 */

#include <stdio.h>
#include <unistd.h>
#include <stddef.h>
#include "lwp.h"

struct sem *s_end;

void f ()
{
	int i; char ch='O';
	for (i=1; i<1000; i++) {
		delayp (250*i);
		write (1, &ch, 1);
	}
	signals (s_end);
	suicidep ();
}

void g ()
{
	int i; char ch=' ';
	for (i=1; i<1000; i++) {
		delayp (350*i);
		write (1, &ch, 1);
	}
	signals (s_end);
	suicidep ();
}

int main ()
{
	initlp (1);
	s_end = creats (0);
	creatp (2, g, 4096, 0, 0, 0);
	creatp (2, f, 4096, 0, 0, 0);
	waits (s_end);
	waits (s_end);
	return 0;
}
