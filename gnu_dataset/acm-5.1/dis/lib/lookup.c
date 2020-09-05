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
#include <string.h>
#include "simmgr.h"

static struct timeval TIMEOUT =
{25, 0};

extern CLIENT *simx_clnt;

extern bool_t xdr_simx_lookup_complete_entity_type_result(register XDR * xdrs, simx_lookup_complete_entity_type_result * objp);

typedef struct _client_names_cache_entry {
	dis_entity_type key;
	simx_lookup_complete_entity_type_result result;
	struct _client_names_cache_entry *next;
	struct _client_names_cache_entry *prev;
} client_names_cache_entry;

static client_names_cache_entry *cache_list_head = 0, *cache_list_tail = 0;
static int cache_list_count = 0;
static int cache_list_max = 32;

int
SIMxLookupEntityName(dis_entity_type * p, char *result, int size)
{
	simx_lookup_entity_type_result res;

	res.value = NULL;

	if (clnt_call(simx_clnt, SIMxLookupEntityNameP,
				  (xdrproc_t) xdr_dis_entity_type, (char *) p,
				  (xdrproc_t) xdr_simx_lookup_entity_type_result, (char *) &res,
				  TIMEOUT) != RPC_SUCCESS) {
		return SIMx_TIMEOUT;
	}

	strncpy(result, res.value, size);
	free(res.value);
	return res.status_code;
}

int
SIMxLookupEntityNames(dis_entity_type * p, char *result[7], int size)
{
	simx_lookup_complete_entity_type_result res;
	client_names_cache_entry *cp;

/*
 *  First, check the client's local cache of query results.
 */

	for (cp = cache_list_head; cp; cp = cp->next) {
		if (memcmp((char *) &cp->key, (char *) p,
				   sizeof(cp->key)) == 0) {

/*
 *  Found a match in the cache, move it to the head of the cache list.
 */

			if (cache_list_head != cp) {
				if (cache_list_tail == cp) {
					cache_list_tail = cp->prev;
				}
				if (cp->prev) {
					cp->prev->next = cp->next;
				}
				if (cp->next) {
					cp->next->prev = cp->prev;
				}

/*
 *  There are always at least two entries in the cache if we get to here,
 *  so we can be a bit sloppy about how we insert the cache entry onto the
 *  head of the list
 */
				cp->next = cache_list_head;
				cp->prev = 0;
				cp->next->prev = cp;
				cache_list_head = cp;
			}

/*
 *  Return result
 */

			res = cp->result;
			strncpy(result[0], res.kind_value, size);
			strncpy(result[1], res.domain_value, size);
			strncpy(result[2], res.country_value, size);
			strncpy(result[3], res.category_value, size);
			strncpy(result[4], res.subcategory_value, size);
			strncpy(result[5], res.specific_value, size);
			strncpy(result[6], res.extra_value, size);
			return res.status_code;
		}
	}

/*
 *  Wasn't in the cache -- consult the server via RPC
 */

	res.status_code = 0;
	res.kind_value = NULL;
	res.domain_value = NULL;
	res.country_value = NULL;
	res.category_value = NULL;
	res.subcategory_value = NULL;
	res.specific_value = NULL;
	res.extra_value = NULL;

	if (clnt_call(simx_clnt, SIMxLookupEntityNamesP,
				  (xdrproc_t) xdr_dis_entity_type, (char *) p,
			  (xdrproc_t) xdr_simx_lookup_complete_entity_type_result, (char *) &res,
				  TIMEOUT) != RPC_SUCCESS) {
		return SIMx_TIMEOUT;
	}

	strncpy(result[0], res.kind_value, size);
	strncpy(result[1], res.domain_value, size);
	strncpy(result[2], res.country_value, size);
	strncpy(result[3], res.category_value, size);
	strncpy(result[4], res.subcategory_value, size);
	strncpy(result[5], res.specific_value, size);
	strncpy(result[6], res.extra_value, size);

/*
 *  Add new cache entry (or replace the least used one) with the results
 *  of this query
 */

	if (cache_list_count == cache_list_max) {
		cp = cache_list_tail;
		free(cp->result.kind_value);
		free(cp->result.domain_value);
		free(cp->result.country_value);
		free(cp->result.category_value);
		free(cp->result.subcategory_value);
		free(cp->result.specific_value);
		free(cp->result.extra_value);
	}
	else {
		cp = (client_names_cache_entry *)
			malloc(sizeof(client_names_cache_entry));
		cp->next = 0;
		cp->prev = cache_list_tail;
		cache_list_tail = cp;
		++cache_list_count;
	}
	cp->key = *p;
	cp->result = res;

#ifdef notdef
	cp->result.kind_value = strdup(cp->result.kind_value);
	cp->result.domain_value = strdup(cp->result.domain_value);
	cp->result.country_value = strdup(cp->result.country_value);
	cp->result.category_value = strdup(cp->result.category_value);
	cp->result.subcategory_value = strdup(cp->result.subcategory_value);
	cp->result.specific_value = strdup(cp->result.specific_value);
	cp->result.extra_value = strdup(cp->result.extra_value);

	xdr_free(xdr_simx_lookup_complete_entity_type_result, (char *) &res);
#endif
	return res.status_code;
}
