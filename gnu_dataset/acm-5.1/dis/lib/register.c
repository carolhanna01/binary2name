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

#include <dis/dis.h>
#include <rpc/rpc.h>
#include "simmgr.h"
#include <stdlib.h>

static struct timeval TIMEOUT =
{25, 0};

CLIENT   *simx_clnt;
int       simx_initialized;

int
SIMxRegisterApplication(char *server_host, char *site_name,
			   unsigned int application_id, dis_simulation_addr * result)
{
	simx_register_app_args args;
	simx_register_app_result res;
	char     *p;

	args.site_name = site_name;
	args.application_id = application_id;

	if (!server_host) {
		if ((p = getenv("SIMX_HOST")) != (char *) NULL) {
			server_host = p;
		}
		else {
			server_host = "localhost";
		}
	}

	simx_clnt =
		clnt_create(server_host, SIMx_PROGRAM, SIMx_VERSION, "udp");

	if (simx_clnt == NULL) {
		return SIMx_NO_SERVER;
	}

	simx_initialized = 1;

	if (clnt_call(simx_clnt, SIMxRegisterApplicationP,
				  (xdrproc_t) xdr_simx_register_app_args, (char *) &args,
				  (xdrproc_t) xdr_simx_register_app_result, (char *) &res,
				  TIMEOUT) != RPC_SUCCESS) {
		return SIMx_TIMEOUT;
	}

	*result = res.result;
	return res.status_code;
}
