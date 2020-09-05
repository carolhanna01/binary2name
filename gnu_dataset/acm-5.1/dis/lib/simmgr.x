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

#ifdef RPC_HDR
%#include <rpc/rpc.h>
%#include <dis/dis.h>
#endif

#define SIMMGR_HANDLE_SIZE	4

enum simx_stat {
	SIMx_SUCCESS	=	0,
	SIMx_ERROR	=	1,
	SIMx_NO_SERVER 	=	2,
	SIMx_TIMEOUT 	=	4,
	SIMx_NOT_FOUND	=	5
	};
	
struct simx_register_app_args {
	string		site_name<64>;
	unsigned short	application_id;
	};

struct simx_register_app_result {
	simx_stat		status_code;
	dis_simulation_addr	result;
	};

struct simx_entity_type_attr {
	dis_entity_type	*type;
	string		attr<256>;
	};

struct simx_lookup_entity_type_result {
	simx_stat	status_code;
	string		value<>;
	};

struct simx_lookup_complete_entity_type_result {
	simx_stat	status_code;
	string		kind_value<>;
	string		domain_value<>;
	string		country_value<>;
	string		category_value<>;
	string		subcategory_value<>;
	string		specific_value<>;
	string		extra_value<>;
	};
	
typedef string simx_string<256>;

struct simx_session_handle {
	opaque data[SIMMGR_HANDLE_SIZE];
	dis_simulation_addr	address;
	};

union simx_startres switch (simx_stat result) {

case SIMx_SUCCESS:
	simx_session_handle	handle;

default:
	void;
};

program SIMx_PROGRAM {

	version SIMx_VERSION {

	simx_register_app_result
	SIMxRegisterApplicationP (simx_register_app_args)	= 1;

	simx_lookup_entity_type_result
	SIMxLookupEntityNameP (dis_entity_type)			= 2;

	dis_entity_type
	SIMxLookupEntityTypeFromPatternP (simx_string)		= 3;

	simx_lookup_entity_type_result
	SIMxLookupEntityAttributeP (simx_entity_type_attr)	= 4;

	simx_lookup_complete_entity_type_result
	SIMxLookupEntityNamesP (dis_entity_type)		= 5;

	} = 1;

} = 300;
