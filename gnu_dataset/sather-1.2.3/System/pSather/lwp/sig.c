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
 * sig.c -- share signals among threads.
 *
 * author: Stephen Crane, (jsc@doc.ic.ac.uk), Department of Computing,
 * Imperial College of Science, Technology and Medicine, 180 Queen's
 * Gate, London SW7 2BZ, England.
 */

#include <malloc.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>

#include "lwp.h"

extern int select(int,fd_set *,fd_set *,fd_set *,struct timeval *);
extern int getdtablesize(void);
#ifndef __linux__
extern void bzero(char *,int);
#endif

static struct sig_info *info;
static int w, n;
static fd_set fds;
static struct sigaction os;

/*
 * iohan -- main handler for sigio.  dispatches signal to
 * first ready descriptor.
 */
static void iohan ()
{
#ifdef __linux__
	fd_set fdtmp;
#else
	struct fd_set fdtmp;
#endif
	struct timeval t;
	struct sig_info *p;

	fdtmp = fds;
	t.tv_sec = t.tv_usec = 0;
	select (w, &fdtmp, (fd_set *)0, (fd_set *)0, &t);
	for (p=info; p; p=p->next)
		if (FD_ISSET(p->des, &fdtmp)) {
			p->han (p->ctx, p->des);
			break;
		}
}

/*
 * sigioset -- install handler for SIGIO
 */
int sigioset (int fd, void (*handler) (void *, int), void *context)
{
	struct sig_info *p;
	struct sigaction s;

	if (!n) {
		w = getdtablesize ();
		FD_ZERO(&fds);
		s.sa_handler = &iohan;
/*		s.sa_mask = 0; -- Nobbi: had to change this for Linux 2.2.11: */
		sigemptyset(&s.sa_mask);
		s.sa_flags = SA_INTERRUPT;
		sigaction (SIGIO, &s, &os);
	}
	if (n++ == w)
		return (-1);
	p = (struct sig_info *)malloc (sizeof(struct sig_info));
	p->next = info;
	p->han = handler;
	p->des = fd;
	p->ctx = context;
	info = p;
	FD_SET(fd, &fds);
	return (0);
}

/*
 * sigioclr -- remove handler for SIGIO
 */
int sigioclr (int fd)
{
	struct sig_info *p, *q;

	for (p=info, q=0; p; q=p, p=p->next)
		if (p->des == fd)
			break;
	if (!p) return (-1);

	if (q) q->next = p->next;
	else info = p->next;
	FD_CLR(fd, &fds);

	free (p);
	if (!--n)
		sigaction (SIGIO, &os, 0);
	return (0);
}
