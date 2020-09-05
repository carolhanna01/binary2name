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
 * producer.c -- an example program to demonstrate lightweight
 * processes, semaphores and time.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 */

#include <stdio.h>
#include <unistd.h>
#include <stddef.h>
#include <math.h>
#include <signal.h>

#include "lwp.h"

struct sem *s_empty, *s_full, *s_end;
char c;

void f (int n, char *p)
{
	while (n--) {
		waits (s_empty);
		c = *p++;
		putchar (c);
		fflush (stdout);
		signals (s_full);
		delayp (1000000);
	}
	signals (s_end);
	suicidep ();
}

void g (int n)
{
	while (n--) {
		waits (s_full);
		putchar (c);
		fflush (stdout);
		signals (s_empty);
	}
	signals (s_end);
	suicidep ();
}

int main (int argc, char **argv)
{
	char *str = "Hello world\n";
	int n = strlen (str);
	/* init_stack_dump(argv[0]); */

	initlp (4);
	s_empty = creats (1);
	s_full = creats (0);
	s_end = creats (0);
	creatp (2, g, 2048, n, (char **)0, 0);
	creatp (2, f, 2048, n, (char **)str, 0);
	while(1) {
		printf("-");
		fflush(stdout);
		delayp(10000);
	}
	for (n=2; n--; )
		waits (s_end);
	return 0;
}
