/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char revid[] = "$Id: os_errno.c,v 1.3 2000/06/28 16:47:02 loic Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <errno.h>
#endif

#include "db_int.h"

/*
 * CDB___os_get_errno --
 *	Return the value of errno.
 *
 * PUBLIC: int CDB___os_get_errno __P((void));
 */
int
CDB___os_get_errno()
{
	/* This routine must be able to return the same value repeatedly. */
	return (errno);
}

/*
 * CDB___os_set_errno --
 *	Set the value of errno.
 *
 * PUBLIC: void CDB___os_set_errno __P((int));
 */
void
CDB___os_set_errno(evalue)
	int evalue;
{
	errno = evalue;
}
