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

/*
 *  This file was manually generated.  It was NOT created by RPCGEN.
 *  It contains xdr definitions for structures that do not fit the
 *  convetional RPCGEN structure model for variable length vectors or
 *  union definitions.
 */

#include <rpc/rpc.h>
#include <dis/dis.h>
#define _DIS_PRIVATE	1
#include <dis/dis_xdr.h>

bool_t
xdr_dis_variable_datum(XDR * xdrs, dis_variable_datum * objp)
{
	static char pad[8];			/* force padding to be zeroes */
	int       padbytes;

	if (!xdr_u_long(xdrs, &objp->datum_id)) {
		return (FALSE);
	}
	if (!xdr_u_long(xdrs, &objp->value_length)) {
		return (FALSE);
	}

	switch (objp->datum_id) {

	/*
	 * Double variable data items
	 */

	case DatumGeocentricCoordinatesX:
	case DatumGeocentricCoordinatesY:
	case DatumGeocentricCoordinatesZ:
		if (!xdr_double(xdrs, &objp->value.double_value)) {
			return (FALSE);
		}
		break;

	/* 
	 * Datum type defaults to String
	 */

	default:
		if (!xdr_vector(xdrs, (char *) &objp->value.ptr_value,
					    (unsigned int) objp->value_length / 8,
					    sizeof(char),
                        (xdrproc_t) xdr_u_char)) {
			return (FALSE);
		}
		break;
	}

	padbytes = 8 - (objp->value_length % 8);
	if (padbytes != 0) {
		if (!xdr_vector(xdrs, pad, padbytes, sizeof(char), (xdrproc_t) xdr_char)) {
			return (FALSE);
		}
	}
	return TRUE;
}

bool_t
xdr_dis_articulation_parm(XDR * xdrs, dis_articulation_parm * objp)
{
	if (!xdr_byte_u_char(xdrs, &objp->type)) {
		return (FALSE);
	}
	if (!xdr_byte_u_char(xdrs, &objp->change)) {
		return (FALSE);
	}
	if (!xdr_byte_short(xdrs, &objp->id)) {
		return (FALSE);
	}
	if (!xdr_byte_long(xdrs, (char *) &objp->type)) {
		return (FALSE);
	}
	switch (objp->type) {
	default:
		objp->value.l[0] = 0;
		objp->value.l[1] = 0;
	}
	return TRUE;
}

bool_t
xdr_dis_timestamp(XDR * xdrs, dis_timestamp * objp)
{
	if (!xdr_u_long(xdrs, (unsigned long *) objp)) {
		return (FALSE);
	}
	return TRUE;
}

bool_t
xdr_dis_pdu(XDR * xdrs, dis_pdu * objp)
{
	u_long    pos;

	if (xdrs->x_op == XDR_DECODE) {

		pos = xdr_getpos(xdrs);

		if (!xdr_dis_pdu_header(xdrs, (dis_pdu_header *) objp)) {
			return (FALSE);
		}

		if (!xdr_setpos(xdrs, pos)) {
			return (FALSE);
		}
	}

	switch (objp->hdr.pdu_type) {
	case PDUTypeEntityState:
		if (!xdr_dis_entity_state_pdu(xdrs, (dis_entity_state_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeCollision:
		if (!xdr_dis_collision_pdu(xdrs, (dis_collision_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeFire:
		if (!xdr_dis_fire_pdu(xdrs, (dis_fire_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeDetonation:
		if (!xdr_dis_detonation_pdu(xdrs, (dis_detonation_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeCreateEntity:
		if (!xdr_dis_create_entity_pdu(xdrs,
									   (dis_create_entity_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeRemoveEntity:
		if (!xdr_dis_remove_entity_pdu(xdrs,
									   (dis_remove_entity_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeStopFreeze:
		if (!xdr_dis_stop_pdu(xdrs, (dis_stop_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeStartResume:
		if (!xdr_dis_start_pdu(xdrs, (dis_start_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeEmission:
		if (xdrs->x_op == XDR_DECODE) {
			((dis_em_emission_pdu *) objp)->system = NULL;
		}
		if (!xdr_dis_em_emission_pdu(xdrs,
									 (dis_em_emission_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeTransferControl:
		if (!xdr_dis_transfer_control_pdu(xdrs, (dis_transfer_control_pdu *) objp)) {
			return (FALSE);
		}
		break;
	case PDUTypeAcknowledge:
		if (!xdr_dis_acknowledge_pdu(xdrs, (dis_acknowledge_pdu *) objp)) {
			return (FALSE);
		}
		break;
	}
	return TRUE;
}
