/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char revid[] = "$Id: os_root.c,v 1.3 2000/06/28 16:47:02 loic Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <unistd.h>
#endif

#include "db_int.h"

/*
 * CDB___os_isroot --
 *	Return if user has special permissions.
 *
 * PUBLIC: int CDB___os_isroot __P((void));
 */
int
CDB___os_isroot()
{
#ifdef HAVE_GETUID
	return (getuid() == 0);
#else
	return (0);
#endif
}
