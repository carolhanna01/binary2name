/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char revid[] = "$Id: mp_fset.c,v 1.4 2014/04/17 20:27:31 sebdiaz Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#endif

#ifdef  HAVE_RPC
#include "db_server.h"
#endif

#include "db_int.h"
#include "db_shash.h"
#include "mp.h"

#ifdef HAVE_RPC
#include "gen_client_ext.h"
#include "rpc_client_ext.h"
#endif

/*
 * CDB_memp_fset --
 *	Mpool page set-flag routine.
 */
int
CDB_memp_fset(dbmfp, pgaddr, flags)
	DB_MPOOLFILE *dbmfp;
	void *pgaddr;
	u_int32_t flags;
{
	BH *bhp;
	DB_ENV *dbenv;
	DB_MPOOL *dbmp;
	MPOOL *c_mp;
	int ret;

	dbmp = dbmfp->dbmp;
	dbenv = dbmp->dbenv;
	

#ifdef HAVE_RPC
	if (F_ISSET(dbenv, DB_ENV_RPCCLIENT))
		return (__dbcl_memp_fset(dbmfp, pgaddr, flags));
#endif

	PANIC_CHECK(dbenv);

	/* Validate arguments. */
	if (flags == 0)
		return (CDB___db_ferr(dbenv, "CDB_memp_fset", 1));

	if ((ret = CDB___db_fchk(dbenv, "CDB_memp_fset", flags,
	    DB_MPOOL_DIRTY | DB_MPOOL_CLEAN | DB_MPOOL_DISCARD)) != 0)
		return (ret);
	if ((ret = CDB___db_fcchk(dbenv, "CDB_memp_fset",
	    flags, DB_MPOOL_CLEAN, DB_MPOOL_DIRTY)) != 0)
		return (ret);

	if (LF_ISSET(DB_MPOOL_DIRTY) && F_ISSET(dbmfp, MP_READONLY)) {
		CDB___db_err(dbenv, "%s: dirty flag set for readonly file page",
		    CDB___memp_fn(dbmfp));
		return (EACCES);
	}

	/* Convert the page address to a buffer header. */
	bhp = (BH *)((u_int8_t *)pgaddr - SSZA(BH, buf));

	/* Convert the buffer header to a cache. */
	c_mp = BH_TO_CACHE(dbmp, bhp);

	R_LOCK(dbenv, dbmp->reginfo);

	if (LF_ISSET(DB_MPOOL_CLEAN) && F_ISSET(bhp, BH_DIRTY)) {
		++c_mp->stat.st_page_clean;
		--c_mp->stat.st_page_dirty;
		F_CLR(bhp, BH_DIRTY);
	}
	if (LF_ISSET(DB_MPOOL_DIRTY) && !F_ISSET(bhp, BH_DIRTY)) {
		--c_mp->stat.st_page_clean;
		++c_mp->stat.st_page_dirty;
		F_SET(bhp, BH_DIRTY);
	}
	if (LF_ISSET(DB_MPOOL_DISCARD))
		F_SET(bhp, BH_DISCARD);

	R_UNLOCK(dbenv, dbmp->reginfo);
	return (0);
}
