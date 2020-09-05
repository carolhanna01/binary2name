#include <rpc/rpc.h>
#include <disp.h>
#define _DIS_PRIVATE 1
#include <dis/dis_xdr.h>


bool_t
xdr_dis_float_vector(xdrs, objp)
	XDR *xdrs;
	dis_float_vector *objp;
{
	if (!xdr_byte_float(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_angular_vel_vector(xdrs, objp)
	XDR *xdrs;
	dis_angular_vel_vector *objp;
{
	if (!xdr_byte_float(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_linear_acc_vector(xdrs, objp)
	XDR *xdrs;
	dis_linear_acc_vector *objp;
{
	if (!xdr_byte_float(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_linear_vel_vector(xdrs, objp)
	XDR *xdrs;
	dis_linear_vel_vector *objp;
{
	if (!xdr_byte_float(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_entity_coord_vector(xdrs, objp)
	XDR *xdrs;
	dis_entity_coord_vector *objp;
{
	if (!xdr_byte_float(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_entity_type(xdrs, objp)
	XDR *xdrs;
	dis_entity_type *objp;
{
	if (!xdr_byte_u_char(xdrs, &objp->kind)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->domain)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->country)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->category)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->subcategory)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->specific)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->extra)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_entity_marking(xdrs, objp)
	XDR *xdrs;
	dis_entity_marking *objp;
{
	if (!xdr_byte_u_char(xdrs, &objp->charset)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->marking, 11, sizeof(byte_u_char), xdr_byte_u_char)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_fixed_datum(xdrs, objp)
	XDR *xdrs;
	dis_fixed_datum *objp;
{
	if (!xdr_byte_u_long(xdrs, &objp->datum_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->ulong_value)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_simulation_addr(xdrs, objp)
	XDR *xdrs;
	dis_simulation_addr *objp;
{
	if (!xdr_byte_u_short(xdrs, &objp->site_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->application_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_emitter_system(xdrs, objp)
	XDR *xdrs;
	dis_emitter_system *objp;
{
	if (!xdr_byte_u_short(xdrs, &objp->name)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->function)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_entity_id(xdrs, objp)
	XDR *xdrs;
	dis_entity_id *objp;
{
	if (!xdr_dis_simulation_addr(xdrs, &objp->sim_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->entity_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_euler_angles(xdrs, objp)
	XDR *xdrs;
	dis_euler_angles *objp;
{
	if (!xdr_byte_float(xdrs, &objp->psi)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->theta)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->phi)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_event_id(xdrs, objp)
	XDR *xdrs;
	dis_event_id *objp;
{
	if (!xdr_dis_simulation_addr(xdrs, &objp->sim_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->event_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_fundamental_parameters(xdrs, objp)
	XDR *xdrs;
	dis_fundamental_parameters *objp;
{
	if (!xdr_byte_float(xdrs, &objp->freq)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->freq_range)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->erp)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->prf)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->pulse_width)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->beam_azimuth_center)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->beam_azimuth_sweep)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->beam_elev_center)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->beam_elev_sweep)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->beam_sweep_sync)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_modulation_type(xdrs, objp)
	XDR *xdrs;
	dis_modulation_type *objp;
{
	if (!xdr_byte_u_short(xdrs, &objp->spread_spectrum)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->major_type)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->detail)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->system)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_pdu_header(xdrs, objp)
	XDR *xdrs;
	dis_pdu_header *objp;
{
	if (!xdr_byte_u_char(xdrs, &objp->protocol_version)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->exercise_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->pdu_type)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->protocol_family)) {
		return (FALSE);
	}
	if (!xdr_dis_timestamp(xdrs, &objp->time_stamp)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->length)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->padding)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_double_vector(xdrs, objp)
	XDR *xdrs;
	dis_double_vector *objp;
{
	if (!xdr_byte_double(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_double(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_double(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_world_coordinates(xdrs, objp)
	XDR *xdrs;
	dis_world_coordinates *objp;
{
	if (!xdr_byte_double(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_double(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_double(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_relative_coordinates(xdrs, objp)
	XDR *xdrs;
	dis_relative_coordinates *objp;
{
	if (!xdr_byte_float(xdrs, &objp->x)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->y)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->z)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_antenna_location(xdrs, objp)
	XDR *xdrs;
	dis_antenna_location *objp;
{
	if (!xdr_dis_world_coordinates(xdrs, &objp->ant_location)) {
		return (FALSE);
	}
	if (!xdr_dis_relative_coordinates(xdrs, &objp->relative)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_beam_antenna_pattern(xdrs, objp)
	XDR *xdrs;
	dis_beam_antenna_pattern *objp;
{
	if (!xdr_dis_euler_angles(xdrs, &objp->direction)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->azimuth_width)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->elev_width)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->reference_system)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->pad, 3, sizeof(byte_u_char), xdr_byte_u_char)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->Ez)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->Ex)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->phase)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_spherical_harmonic_antenna_pattern(xdrs, objp)
	XDR *xdrs;
	dis_spherical_harmonic_antenna_pattern *objp;
{
	if (!xdr_byte_u_char(xdrs, &objp->pattern)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->coefficients)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->ref_system)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_time(xdrs, objp)
	XDR *xdrs;
	dis_time *objp;
{
	if (!xdr_byte_u_long(xdrs, &objp->hour)) {
		return (FALSE);
	}
	if (!xdr_dis_timestamp(xdrs, &objp->rel)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_burst_descriptor(xdrs, objp)
	XDR *xdrs;
	dis_burst_descriptor *objp;
{
	if (!xdr_dis_entity_type(xdrs, &objp->munition)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->warhead)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->fuze)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->quantity)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->rate)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_dead_reckoning(xdrs, objp)
	XDR *xdrs;
	dis_dead_reckoning *objp;
{
	if (!xdr_byte_u_char(xdrs, &objp->algorithm)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->other, 15, sizeof(byte_u_char), xdr_byte_u_char)) {
		return (FALSE);
	}
	if (!xdr_dis_linear_acc_vector(xdrs, &objp->linear_acc)) {
		return (FALSE);
	}
	if (!xdr_dis_angular_vel_vector(xdrs, &objp->angular_vel)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_capabilities(xdrs, objp)
	XDR *xdrs;
	dis_capabilities *objp;
{
	if (!xdr_byte_u_long(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_supply_quantity(xdrs, objp)
	XDR *xdrs;
	dis_supply_quantity *objp;
{
	if (!xdr_dis_entity_type(xdrs, &objp->entity)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->quantity)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_entity_appearance(xdrs, objp)
	XDR *xdrs;
	dis_entity_appearance *objp;
{
	if (!xdr_byte_u_long(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_entity_state_pdu(xdrs, objp)
	XDR *xdrs;
	dis_entity_state_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->force_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->art_parm_count)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_type(xdrs, &objp->type)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_type(xdrs, &objp->alt_type)) {
		return (FALSE);
	}
	if (!xdr_dis_linear_vel_vector(xdrs, &objp->vel)) {
		return (FALSE);
	}
	if (!xdr_dis_world_coordinates(xdrs, &objp->pos)) {
		return (FALSE);
	}
	if (!xdr_dis_euler_angles(xdrs, &objp->orientation)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_appearance(xdrs, &objp->appearance)) {
		return (FALSE);
	}
	if (!xdr_dis_dead_reckoning(xdrs, &objp->dr_parm)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_marking(xdrs, &objp->marking)) {
		return (FALSE);
	}
	if (!xdr_dis_capabilities(xdrs, &objp->capabilities)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->art_parm,
		objp->art_parm_count, objp->art_parm_count, sizeof(dis_articulation_parm), xdr_dis_articulation_parm)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_collision_pdu(xdrs, objp)
	XDR *xdrs;
	dis_collision_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->collision_id)) {
		return (FALSE);
	}
	if (!xdr_dis_event_id(xdrs, &objp->event)) {
		return (FALSE);
	}
	if (!xdr_dis_linear_vel_vector(xdrs, &objp->vel)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->mass)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_coord_vector(xdrs, &objp->loc)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_fire_pdu(xdrs, objp)
	XDR *xdrs;
	dis_fire_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->firing_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->target_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->munition_id)) {
		return (FALSE);
	}
	if (!xdr_dis_event_id(xdrs, &objp->event)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->fire_mission_index)) {
		return (FALSE);
	}
	if (!xdr_dis_world_coordinates(xdrs, &objp->pos)) {
		return (FALSE);
	}
	if (!xdr_dis_burst_descriptor(xdrs, &objp->burst)) {
		return (FALSE);
	}
	if (!xdr_dis_linear_vel_vector(xdrs, &objp->vel)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->range)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_detonation_pdu(xdrs, objp)
	XDR *xdrs;
	dis_detonation_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->firing_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->target_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->munition_id)) {
		return (FALSE);
	}
	if (!xdr_dis_event_id(xdrs, &objp->event)) {
		return (FALSE);
	}
	if (!xdr_dis_linear_vel_vector(xdrs, &objp->vel)) {
		return (FALSE);
	}
	if (!xdr_dis_world_coordinates(xdrs, &objp->pos)) {
		return (FALSE);
	}
	if (!xdr_dis_burst_descriptor(xdrs, &objp->burst)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_coord_vector(xdrs, &objp->loc)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->result)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->num_art_parms)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->pad)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->art_parm,
		objp->num_art_parms, objp->num_art_parms, sizeof(dis_articulation_parm), xdr_dis_articulation_parm)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_service_type(xdrs, objp)
	XDR *xdrs;
	dis_service_type *objp;
{
	if (!xdr_byte_u_char(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_repair_type(xdrs, objp)
	XDR *xdrs;
	dis_repair_type *objp;
{
	if (!xdr_byte_u_short(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_repair_result(xdrs, objp)
	XDR *xdrs;
	dis_repair_result *objp;
{
	if (!xdr_byte_u_char(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_service_request_pdu(xdrs, objp)
	XDR *xdrs;
	dis_service_request_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->requestor_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->server_id)) {
		return (FALSE);
	}
	if (!xdr_dis_service_type(xdrs, &objp->requested_service)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->num_supply_types)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->pad)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->supplies,
		objp->num_supply_types, objp->num_supply_types, sizeof(dis_supply_quantity), xdr_dis_supply_quantity)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_resupply_offer_pdu(xdrs, objp)
	XDR *xdrs;
	dis_resupply_offer_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->receiver_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->supplier_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->num_supply_types)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->pad, 3, sizeof(byte_u_char), xdr_byte_u_char)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->supplies,
		objp->num_supply_types, objp->num_supply_types, sizeof(dis_supply_quantity), xdr_dis_supply_quantity)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_resupply_received_pdu(xdrs, objp)
	XDR *xdrs;
	dis_resupply_received_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->receiver_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->supplier_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->num_supply_types)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->pad, 3, sizeof(byte_u_char), xdr_byte_u_char)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->supplies,
		objp->num_supply_types, objp->num_supply_types, sizeof(dis_supply_quantity), xdr_dis_supply_quantity)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_resupply_cancel_pdu(xdrs, objp)
	XDR *xdrs;
	dis_resupply_cancel_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->receiver_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->supplier_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_repair_complete_pdu(xdrs, objp)
	XDR *xdrs;
	dis_repair_complete_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->receiver_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->supplier_id)) {
		return (FALSE);
	}
	if (!xdr_dis_repair_type(xdrs, &objp->repair)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->pad)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_repair_response_pdu(xdrs, objp)
	XDR *xdrs;
	dis_repair_response_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->receiver_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->supplier_id)) {
		return (FALSE);
	}
	if (!xdr_dis_repair_result(xdrs, &objp->result)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->pad, 3, sizeof(byte_u_char), xdr_byte_u_char)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_request_id(xdrs, objp)
	XDR *xdrs;
	dis_request_id *objp;
{
	if (!xdr_byte_u_long(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_create_entity_pdu(xdrs, objp)
	XDR *xdrs;
	dis_create_entity_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_remove_entity_pdu(xdrs, objp)
	XDR *xdrs;
	dis_remove_entity_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_start_pdu(xdrs, objp)
	XDR *xdrs;
	dis_start_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_time(xdrs, &objp->real_time)) {
		return (FALSE);
	}
	if (!xdr_dis_time(xdrs, &objp->sim_time)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_stop_pdu(xdrs, objp)
	XDR *xdrs;
	dis_stop_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_time(xdrs, &objp->real_time)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->reason)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->behavior)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->pad, 2, sizeof(byte_u_char), xdr_byte_u_char)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_acknowledge_pdu(xdrs, objp)
	XDR *xdrs;
	dis_acknowledge_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->acknowledge_flag)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->resp_flag)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_data_query_pdu(xdrs, objp)
	XDR *xdrs;
	dis_data_query_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_time(xdrs, &objp->interval)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->num_fixed_data)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->num_variable_data)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->fixed_datum_id,
		objp->num_fixed_data, objp->num_fixed_data, sizeof(byte_u_long), xdr_byte_u_long)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->variable_datum_id,
		objp->num_variable_data, objp->num_variable_data, sizeof(byte_u_long), xdr_byte_u_long)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_datum_spec_record(xdrs, objp)
	XDR *xdrs;
	dis_datum_spec_record *objp;
{
	if (!xdr_byte_u_long(xdrs, &objp->num_fixed_data)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->num_variable_data)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->fixed_datum,
		objp->num_fixed_data, objp->num_fixed_data, sizeof(dis_fixed_datum), xdr_dis_fixed_datum)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->variable_datum,
		objp->num_variable_data, objp->num_variable_data, sizeof(dis_variable_datum), xdr_dis_variable_datum)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_set_data_pdu(xdrs, objp)
	XDR *xdrs;
	dis_set_data_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	if (!xdr_dis_datum_spec_record(xdrs, &objp->datum_info)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_data_pdu(xdrs, objp)
	XDR *xdrs;
	dis_data_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_request_id(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	if (!xdr_dis_datum_spec_record(xdrs, &objp->datum_info)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_event_report_pdu(xdrs, objp)
	XDR *xdrs;
	dis_event_report_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_dis_event_id(xdrs, &objp->event_type)) {
		return (FALSE);
	}
	if (!xdr_dis_datum_spec_record(xdrs, &objp->datum_info)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_message_pdu(xdrs, objp)
	XDR *xdrs;
	dis_message_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->pad)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->num_variable_data)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->variable_datum,
		objp->num_variable_data, objp->num_variable_data, sizeof(dis_variable_datum), xdr_dis_variable_datum)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_track_info(xdrs, objp)
	XDR *xdrs;
	dis_track_info *objp;
{
	if (!xdr_dis_entity_id(xdrs, &objp->target)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->emitter_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->beam_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_beam_info(xdrs, objp)
	XDR *xdrs;
	dis_beam_info *objp;
{
	if (!xdr_byte_u_char(xdrs, &objp->beam_data_length)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->beam_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->beam_parm_index)) {
		return (FALSE);
	}
	if (!xdr_dis_fundamental_parameters(xdrs, &objp->fundamental)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->beam_function)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->num_targets)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->high_density_track_jam)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->pad)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->jamming_mode)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->tracked_target,
		objp->num_targets, objp->num_targets, sizeof(dis_track_info), xdr_dis_track_info)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_em_system_info(xdrs, objp)
	XDR *xdrs;
	dis_em_system_info *objp;
{
	if (!xdr_byte_u_char(xdrs, &objp->sys_data_length)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->num_beams)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->pad)) {
		return (FALSE);
	}
	if (!xdr_dis_emitter_system(xdrs, &objp->emitter_system)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_coord_vector(xdrs, &objp->location)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->beam,
		objp->num_beams, objp->num_beams, sizeof(dis_beam_info), xdr_dis_beam_info)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_em_emission_pdu(xdrs, objp)
	XDR *xdrs;
	dis_em_emission_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->emitter_id)) {
		return (FALSE);
	}
	if (!xdr_dis_event_id(xdrs, &objp->event)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->state_update)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->num_systems)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->pad)) {
		return (FALSE);
	}
	if (!xdr_var_array(xdrs, (char **)&objp->system,
		objp->num_systems, objp->num_systems, sizeof(dis_em_system_info), xdr_dis_em_system_info)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_designator_pdu(xdrs, objp)
	XDR *xdrs;
	dis_designator_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->designating_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_short(xdrs, &objp->code_name)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->designated_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->pad)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->code)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->power)) {
		return (FALSE);
	}
	if (!xdr_byte_float(xdrs, &objp->wavelength)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_coord_vector(xdrs, &objp->spot_rel)) {
		return (FALSE);
	}
	if (!xdr_dis_world_coordinates(xdrs, &objp->spot_pos)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_experimental_request_control_pdu(xdrs, objp)
	XDR *xdrs;
	dis_experimental_request_control_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->requesting_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->control_target_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_experimental_grant_control_pdu(xdrs, objp)
	XDR *xdrs;
	dis_experimental_grant_control_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->granting_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->control_target_id)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_dis_transfer_control_pdu(xdrs, objp)
	XDR *xdrs;
	dis_transfer_control_pdu *objp;
{
	if (!xdr_dis_pdu_header(xdrs, &objp->hdr)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->orig_id)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->recv_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->request_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->reliability_service)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->transfer_type)) {
		return (FALSE);
	}
	if (!xdr_dis_entity_id(xdrs, &objp->target_id)) {
		return (FALSE);
	}
	if (!xdr_byte_u_long(xdrs, &objp->num_record_sets)) {
		return (FALSE);
	}
	return (TRUE);
}


