/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1996-1998, Riley Rainey (rainey@netcom.com)
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
/*
 *  DIS 2.04 Protocol definition
 *
 *  Copyright (c) 1995, Riley Rainey (riley@netcom.com)
 *
 *  The following Protocol Data Unit (PDU) classes are defined in this file:
 *
 *  Entity Information/Interaction
 *  Warfare
 *  Simulation Management
 *  Distributed Emission Regeneration
 *
 *  Note that some DIS PDUs or related data structures do not conform to
 *  the strict RPCGEN model of data layout.  In those cases, XDR routines
 *  had to be generated manually (see dis_xdr1.c).  Definitions for those
 *  structures are included included in this file, protected by
 *  "#ifdef RPC_HDR" directives.
 *
 *  Also note that we post-process the rpcgen output before it gets
 *  compiled.  Specifically, XDR says that short's and char's must be padded
 *  to 4-byte boundaries in a network data stream.  This padding was short-
 *  circuited by creating xdr_byte_* routines that look just like the
 *  corresponding xdr_* routine (i.e. there is an xdr_byte_short routine
 *  that does what xdr_short does except that it omits the padding).  This
 *  change implies that padding must be defined in the protocol definition
 *  (in this file) where needed.  The xdr_byte_* routines are defined in
 *  dis_xdr2.c.
 */

#ifdef RPC_XDR
%#define _DIS_PRIVATE	1
%#include <dis/dis_xdr.h>
#endif

struct dis_float_vector {
	float			x;
	float			y;
	float			z;
	};

struct dis_angular_vel_vector {
	float			x;
	float			y;
	float			z;
	};

struct dis_linear_acc_vector {
	float			x;
	float			y;
	float			z;
	};

struct dis_linear_vel_vector {
	float			x;
	float			y;
	float			z;
	};

struct dis_entity_coord_vector {
	float			x;
	float			y;
	float			z;
	};

struct dis_entity_type  {
	unsigned char		kind;
	unsigned char		domain;
	unsigned short		country;
	unsigned char		category;
	unsigned char		subcategory;
	unsigned char		specific;
	unsigned char		extra;
	};

struct dis_entity_marking {
	unsigned char		charset;
	unsigned char		marking[11];
	};

struct dis_fixed_datum {
	unsigned long		datum_id;
	unsigned long	ulong_value;
	};

#ifdef RPC_HDR
%typedef struct {
%	unsigned long		datum_id;
%	unsigned long		value_length;
%	union {
%	    double		    double_value;
%	    dis_entity_type	entity_type_value;
%	    unsigned char	*ptr_value;
%	} value;
%	} dis_variable_datum;
%
%typedef struct {
%	unsigned 		time:31; /* 1.676... usec units */
%	unsigned		type:1;	 /* type: 0=relative, 1=absolute */
%	} dis_timestamp;
#endif

struct dis_simulation_addr {
	unsigned short		site_id;
	unsigned short		application_id;
	};

struct dis_emitter_system {
	unsigned short		name;
	unsigned char		function;
	unsigned char		id;
	};

struct dis_entity_id  {
	dis_simulation_addr	sim_id;
	unsigned short		entity_id;
	};

struct dis_euler_angles {
	float			psi;
	float			theta;
	float			phi;
	};
	
struct dis_event_id {
	dis_simulation_addr	sim_id;
	unsigned short		event_id;
	};

struct dis_fundamental_parameters {
	float			freq;
	float			freq_range;
	float			erp;
	float			prf;
	float			pulse_width;
	float			beam_azimuth_center;
	float			beam_azimuth_sweep;
	float			beam_elev_center;
	float			beam_elev_sweep;
	float			beam_sweep_sync;
	};

struct dis_modulation_type {
	unsigned short		spread_spectrum;
	unsigned short		major_type;
	unsigned short		detail;
	unsigned short		system;
	};

struct dis_pdu_header {
	unsigned char		protocol_version;
	unsigned char		exercise_id;
	unsigned char		pdu_type;
	unsigned char		protocol_family;
	dis_timestamp		time_stamp;
	unsigned short		length;
	unsigned short		padding;
	};

struct dis_double_vector {
	double			x;
	double			y;
	double			z;
	};

struct dis_world_coordinates {
	double			x;
	double			y;
	double			z;
	};

struct dis_relative_coordinates {
	float			x;
	float			y;
	float			z;
	};

struct dis_antenna_location {
	dis_world_coordinates		ant_location;
	dis_relative_coordinates	relative;
	};

struct dis_beam_antenna_pattern {
	dis_euler_angles	direction;
	float			azimuth_width;
	float			elev_width;
	unsigned char		reference_system;
	unsigned char		pad[3];
	float			Ez;
	float			Ex;
	float			phase;
	};

struct dis_spherical_harmonic_antenna_pattern {
	unsigned char		pattern;
	float			coefficients;
	unsigned char		ref_system;
	};

#ifdef RPC_HDR
%typedef union {
%	double	d;
%	float	f[2];
%	char	c[8];
%	long	l[2];
%	short	s[4];
%	} dis_parm_value;
%
%typedef struct {
%	unsigned char		type;
%	unsigned char		change;
%	unsigned short		id;
%	unsigned long		parm_type;
%	dis_parm_value		value;
%	} dis_articulation_parm;
#endif

struct dis_time {
	unsigned long		hour;	/* hours since 1 Jan, 1970 */
	dis_timestamp		rel;
	};

struct dis_burst_descriptor {
	dis_entity_type		munition;
	unsigned short		warhead;
	unsigned short		fuze;
	unsigned short		quantity;
	unsigned short		rate;
	};

struct dis_dead_reckoning {
	unsigned char		algorithm;
	unsigned char		other[15];
	dis_linear_acc_vector	linear_acc;
	dis_angular_vel_vector	angular_vel;
	};

typedef unsigned long dis_capabilities;

struct dis_supply_quantity {
	dis_entity_type		entity;
	float			quantity;
	};

typedef unsigned long dis_entity_appearance;

/*
 *  PDUs
 */

struct dis_entity_state_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		id;
	unsigned char		force_id;
	unsigned char		art_parm_count;	
	dis_entity_type		type;
	dis_entity_type		alt_type;
	dis_linear_vel_vector	vel;
	dis_world_coordinates	pos;
	dis_euler_angles	orientation;
	dis_entity_appearance	appearance;
	dis_dead_reckoning	dr_parm;
	dis_entity_marking	marking;
	dis_capabilities	capabilities;
	dis_articulation_parm	art_parm[art_parm_count];
	};

struct dis_collision_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		id;
	dis_entity_id		collision_id;
	dis_event_id		event;
	dis_linear_vel_vector	vel;
	unsigned long		mass;
	dis_entity_coord_vector loc;
	};

struct dis_fire_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		firing_id;
	dis_entity_id		target_id;
	dis_entity_id		munition_id;
	dis_event_id		event;
	unsigned long		fire_mission_index;
	dis_world_coordinates	pos;
	dis_burst_descriptor	burst;
	dis_linear_vel_vector	vel;
	float			range;		/* meters */
	};

struct  dis_detonation_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		firing_id;
	dis_entity_id		target_id;
	dis_entity_id		munition_id;
	dis_event_id		event;
	dis_linear_vel_vector	vel;
	dis_world_coordinates	pos;
	dis_burst_descriptor	burst;
	dis_entity_coord_vector loc;
	unsigned char		result;
	unsigned char		num_art_parms;
	unsigned short		pad;
	dis_articulation_parm	art_parm[num_art_parms];
	};

/*
 *  Repair/Resupply
 */

typedef unsigned char	dis_service_type;
typedef unsigned short	dis_repair_type;
typedef unsigned char	dis_repair_result;

struct dis_service_request_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		requestor_id;
	dis_entity_id		server_id;
	dis_service_type	requested_service;
	unsigned char		num_supply_types;
	unsigned short		pad;
	dis_supply_quantity	supplies[num_supply_types];
	};

struct dis_resupply_offer_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		receiver_id;
	dis_entity_id		supplier_id;
	unsigned char		num_supply_types;
	unsigned char		pad[3];
	dis_supply_quantity	supplies[num_supply_types];
	};

struct dis_resupply_received_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		receiver_id;
	dis_entity_id		supplier_id;
	unsigned char		num_supply_types;
	unsigned char		pad[3];
	dis_supply_quantity	supplies[num_supply_types];
	};

struct dis_resupply_cancel_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		receiver_id;
	dis_entity_id		supplier_id;
	};

struct dis_repair_complete_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		receiver_id;
	dis_entity_id		supplier_id;
	dis_repair_type		repair;
	unsigned short		pad;
	};

struct dis_repair_response_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		receiver_id;
	dis_entity_id		supplier_id;
	dis_repair_result	result;
	unsigned char		pad[3];
	};

/*
 *  Simulation Management
 */

typedef unsigned long dis_request_id;

struct dis_create_entity_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_request_id		request_id;
	};

struct dis_remove_entity_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_request_id		request_id;
	};

struct dis_start_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_time		real_time;
	dis_time		sim_time;
	dis_request_id		request_id;
	};

struct dis_stop_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_time		real_time;
	unsigned char		reason;
	unsigned char		behavior;
	unsigned char		pad[2];
	dis_request_id		request_id;
	};

struct dis_acknowledge_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	unsigned short		acknowledge_flag;
	unsigned short		resp_flag;
	dis_request_id		request_id;
	};

struct dis_data_query_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_time		interval;
	dis_request_id		request_id;
	unsigned long		num_fixed_data;
	unsigned long		num_variable_data;
	unsigned long		fixed_datum_id[num_fixed_data];
	unsigned long		variable_datum_id[num_variable_data];
	};

struct dis_datum_spec_record {
	unsigned long		num_fixed_data;
	unsigned long		num_variable_data;
	dis_fixed_datum		fixed_datum[num_fixed_data];
	dis_variable_datum	variable_datum[num_variable_data];
	};

struct dis_set_data_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_request_id		request_id;
	dis_datum_spec_record	datum_info;	
	};

struct dis_data_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_request_id		request_id;
	dis_datum_spec_record	datum_info;	
	};

struct dis_event_report_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	dis_event_id		event_type;
	dis_datum_spec_record	datum_info;	
	};

struct dis_message_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		orig_id;
	dis_entity_id		recv_id;
	unsigned long		pad;
	unsigned long		num_variable_data;
	dis_variable_datum	variable_datum[num_variable_data];	
	};

/*
 *  Distributed Emission Regeneration PDUs
 */

struct dis_track_info {
	dis_entity_id		target;
	unsigned char		emitter_id;
	unsigned char		beam_id;
	};

struct dis_beam_info {
	unsigned char		beam_data_length;
	unsigned char		beam_id;
	unsigned short		beam_parm_index;
	dis_fundamental_parameters fundamental;
	unsigned char		beam_function;
	unsigned char		num_targets;
	unsigned char		high_density_track_jam;
	unsigned char		pad;
	unsigned long		jamming_mode;
	dis_track_info		tracked_target[num_targets];
	};

struct dis_em_system_info {
	unsigned char		sys_data_length;
	unsigned char		num_beams;
	unsigned short		pad;
	dis_emitter_system	emitter_system;
	dis_entity_coord_vector	location;
	dis_beam_info		beam[num_beams];
	};

struct dis_em_emission_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		emitter_id;
	dis_event_id		event;
	unsigned char		state_update;
	unsigned char		num_systems;
	unsigned short		pad;
	dis_em_system_info	system[num_systems];
	};

struct dis_designator_pdu {
	dis_pdu_header		hdr;
	dis_entity_id		designating_id;
	unsigned short		code_name;
	dis_entity_id		designated_id;
	unsigned char		pad;
	unsigned char		code;
	float			power;		/* watts */
	float			wavelength;	/* microns */
	dis_entity_coord_vector spot_rel;
	dis_world_coordinates	spot_pos;
	};

/*
 *  These two PDUs came to me from CALSPAN; I have no idea where they
 *  actually originated.
 */

struct dis_experimental_request_control_pdu {
	dis_pdu_header  hdr;
	dis_entity_id	requesting_id;
	dis_entity_id	control_target_id;
};

struct dis_experimental_grant_control_pdu {
	dis_pdu_header  hdr;
	dis_entity_id	granting_id;
	dis_entity_id	control_target_id;
};

struct dis_transfer_control_pdu {
    dis_pdu_header  hdr;
    dis_entity_id   orig_id;
    dis_entity_id   recv_id;
    unsigned long   request_id;
    unsigned char   reliability_service;
    unsigned char   transfer_type;
    dis_entity_id   target_id;
    unsigned long   num_record_sets;
    };

#ifdef RPC_HDR
%typedef union {
%	dis_pdu_header		hdr;
%	dis_entity_state_pdu	entity_state;
%	dis_collision_pdu	collision;
%
%	dis_fire_pdu		fire;
%	dis_detonation_pdu	detonation;
%
%	dis_create_entity_pdu	create_entity;
%	dis_remove_entity_pdu	remove_entity;
%	dis_start_pdu		start;
%	dis_stop_pdu		stop;
%	dis_acknowledge_pdu	acknowledge;
%	dis_data_query_pdu	data_query;
%	dis_set_data_pdu	set_data;
%	dis_data_pdu		data;
%	dis_event_report_pdu	event_report;
%	dis_message_pdu		message;
%
%	dis_em_emission_pdu	em_emission;
%	dis_designator_pdu	designator;
%   dis_transfer_control_pdu transfer_control;
%	dis_experimental_request_control_pdu request_control;
%	dis_experimental_grant_control_pdu grant_control;
%	} dis_pdu;
#endif
