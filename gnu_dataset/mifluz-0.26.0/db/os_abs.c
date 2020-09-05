/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char revid[] = "$Id: os_abs.c,v 1.3 2000/06/28 16:47:02 loic Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>
#endif

#include "db_int.h"

/*
 * CDB___os_abspath --
 *	Return if a path is an absolute path.
 *
 * PUBLIC: int CDB___os_abspath __P((const char *));
 */
int
CDB___os_abspath(path)
	const char *path;
{
	return (path[0] == '/');
}
