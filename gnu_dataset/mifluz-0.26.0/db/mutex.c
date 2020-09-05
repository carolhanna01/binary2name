/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char revid[] = "$Id: mutex.c,v 1.4 2014/04/17 20:27:31 sebdiaz Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>
#endif

#include "db_int.h"

/*
 * CDB___db_mutex_alloc --
 *	Allocate and initialize a mutex.
 *
 * PUBLIC: int CDB___db_mutex_alloc __P((DB_ENV *, REGINFO *, MUTEX **));
 */
int
CDB___db_mutex_alloc(dbenv, infop, storep)
	DB_ENV *dbenv;
	REGINFO *infop;
	MUTEX **storep;
{	if(infop){}
	int ret;

	/*
	 * If the architecture supports mutexes in heap memory, use that
	 * memory.  If it doesn't, we have to allocate space in a region.
	 */
#ifdef MUTEX_NO_MALLOC_LOCKS
	R_LOCK(dbenv, infop);
	ret = CDB___db_shalloc(infop->addr, sizeof(MUTEX), MUTEX_ALIGN, storep);
	R_UNLOCK(dbenv, infop);
#else
	COMPQUIET(dbenv, NULL);
	COMPQUIET(infop, NULL);
	ret = CDB___os_calloc(dbenv, 1, sizeof(MUTEX), storep);
#endif
	if (ret != 0)
		CDB___db_err(dbenv, "Unable to allocate memory for mutex");
	return (ret);
}

/*
 * CDB___db_mutex_free --
 *	Free a mutex.
 *
 * PUBLIC: void CDB___db_mutex_free __P((DB_ENV *, REGINFO *, MUTEX *));
 */
void
CDB___db_mutex_free(dbenv, infop, mutexp)
	DB_ENV *dbenv;
	REGINFO *infop;
	MUTEX *mutexp;
{	if(infop||dbenv){}
#ifdef MUTEX_NO_MALLOC_LOCKS
	R_LOCK(dbenv, infop);
	CDB___db_shalloc_free(infop->addr, mutexp);
	R_UNLOCK(dbenv, infop);
#else
	COMPQUIET(dbenv, NULL);
	COMPQUIET(infop, NULL);
	CDB___os_free(mutexp, sizeof(*mutexp));
#endif
}
