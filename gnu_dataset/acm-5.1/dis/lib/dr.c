/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1997-1998, Riley Rainey (rainey@netcom.com)
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
#include <math.h>

void
DISEulerToMatrix(dis_euler_angles * ea, VMatrix * m)
{

	double sinPhi, cosPhi, sinTheta, cosTheta, sinPsi, cosPsi;

	sinPhi = sin(ea->phi);
	cosPhi = cos(ea->phi);
	sinTheta = sin(ea->theta);
	cosTheta = cos(ea->theta);
	sinPsi = sin(ea->psi);
	cosPsi = cos(ea->psi);

	m->m[0][0] = cosTheta * cosPsi;
	m->m[0][1] = sinPhi * sinTheta * cosPsi - cosPhi * sinPsi;
	m->m[0][2] = cosPhi * sinTheta * cosPsi + sinPhi * sinPsi;
	m->m[1][0] = cosTheta * sinPsi;
	m->m[1][1] = sinPhi * sinTheta * sinPsi + cosPhi * cosPsi;
	m->m[1][2] = cosPhi * sinTheta * sinPsi - sinPhi * cosPsi;
	m->m[2][0] = -sinTheta;
	m->m[2][1] = sinPhi * cosTheta;
	m->m[2][2] = cosPhi * cosTheta;
	m->m[0][3] = m->m[1][3] = m->m[2][3] = 0.0;
	m->m[3][0] = m->m[3][1] = m->m[3][2] = 0.0;
	m->m[3][3] = 1.0;
}

void
DISProcessNewDRParameters(dis_entity_state_pdu * pdu, dis_dr_parameters * dr)
{
	switch (pdu->dr_parm.algorithm) {
	case DISDRMethodRPB:
	case DISDRMethodRVB:
	case DISDRMethodRPW:
	case DISDRMethodRVW:
		DISGenerateDRParameters(pdu, dr);
		break;

	case DISDRMethodStatic:
	case DISDRMethodFPW:
	case DISDRMethodFVW:
	case DISDRMethodFPB:
	case DISDRMethodFVB:
		break;

	case DISDRMethodOther:
	default:
		break;
	}

	dr->pdu = *pdu;
	DISEulerToMatrix(&pdu->orientation, &dr->R0);
}

void
DISGenerateDRParameters(dis_entity_state_pdu * pdu, dis_dr_parameters * dr)
{
	double    ox, oy, oz;
	double    ax = 0.0, ay = 0.0, az = 0.0;
	double    omega;

	ox = pdu->dr_parm.angular_vel.x;
	oy = pdu->dr_parm.angular_vel.y;
	oz = pdu->dr_parm.angular_vel.z;
	omega = sqrt(ox * ox + oy * oy + oz * oz);

	if (omega > 0.0) {
		ax = ox / omega;
		ay = oy / omega;
		az = oz / omega;
	}

	dr->omega = omega;

	dr->skew.m[0][0] = dr->skew.m[1][1] = dr->skew.m[2][2] = 0.0;
	dr->skew.m[1][0] = -az;
	dr->skew.m[0][1] = az;
	dr->skew.m[2][0] = ay;
	dr->skew.m[0][2] = -ay;
	dr->skew.m[2][1] = -ax;
	dr->skew.m[1][2] = ax;

	dr->aat.m[0][0] = ax * ax;
	dr->aat.m[1][0] = ax * ay;
	dr->aat.m[2][0] = ax * az;
	dr->aat.m[0][1] = ay * ax;
	dr->aat.m[1][1] = ay * ay;
	dr->aat.m[2][1] = ay * az;
	dr->aat.m[0][2] = az * ax;
	dr->aat.m[1][2] = az * ay;
	dr->aat.m[2][2] = az * az;
}

void
DISComputeDRPosition(dis_dr_parameters * dr,
					 double dT,
					 dis_world_coordinates * pos,
					 dis_linear_vel_vector * vel,
					 VMatrix * orientation)
{
	VMatrix   DR;
	double    hDTSqr;
	dis_entity_state_pdu * pdu = &dr->pdu;
	*vel = pdu->vel;

	/* Position */

	switch (dr->pdu.dr_parm.algorithm) {

	case DISDRMethodRPB:
	case DISDRMethodRVB:
		/* todo: position */
		/* todo: orientation */
		break;

	case DISDRMethodFPB:
	case DISDRMethodFVB:
		/* todo: position */
		*orientation = dr->R0;
		break;

	case DISDRMethodRPW:
		pos->x = pdu->pos.x + pdu->vel.x * dT;
		pos->y = pdu->pos.y + pdu->vel.y * dT;
		pos->z = pdu->pos.z + pdu->vel.z * dT;

		DISComputeDRMatrix(dr, dT, &DR);
		DISMatrixMultByRank(&DR, &dr->R0, orientation, 3);
		break;

	case DISDRMethodRVW:
		hDTSqr = 0.5 * dT * dT;
		pos->x = pdu->pos.x + pdu->vel.x * dT +
			pdu->dr_parm.linear_acc.x * hDTSqr;
		pos->y = pdu->pos.y + pdu->vel.y * dT +
			pdu->dr_parm.linear_acc.y * hDTSqr;
		pos->z = pdu->pos.z + pdu->vel.z * dT +
			pdu->dr_parm.linear_acc.z * hDTSqr;
		vel->x = (float) (pdu->vel.x + pdu->dr_parm.linear_acc.x * dT);
		vel->y = (float) (pdu->vel.y + pdu->dr_parm.linear_acc.y * dT);
		vel->z = (float) (pdu->vel.z + pdu->dr_parm.linear_acc.z * dT);

		DISComputeDRMatrix(dr, dT, &DR);
		DISMatrixMultByRank(&DR, &dr->R0, orientation, 3);

		break;

	case DISDRMethodStatic:
		*pos = pdu->pos;
		*orientation = dr->R0;
		break;

	case DISDRMethodFPW:
		pos->x = pdu->pos.x + pdu->vel.x * dT;
		pos->y = pdu->pos.y + pdu->vel.y * dT;
		pos->z = pdu->pos.z + pdu->vel.z * dT;

		*orientation = dr->R0;

		break;

	case DISDRMethodFVW:
		hDTSqr = 0.5 * dT * dT;
		pos->x = pdu->pos.x + pdu->vel.x * dT +
			pdu->dr_parm.linear_acc.x * hDTSqr;
		pos->y = pdu->pos.y + pdu->vel.y * dT +
			pdu->dr_parm.linear_acc.y * hDTSqr;
		pos->z = pdu->pos.z + pdu->vel.z * dT +
			pdu->dr_parm.linear_acc.z * hDTSqr;
		vel->x = (float) (pdu->vel.x + pdu->dr_parm.linear_acc.x * dT);
		vel->y = (float) (pdu->vel.y + pdu->dr_parm.linear_acc.y * dT);
		vel->z = (float) (pdu->vel.z + pdu->dr_parm.linear_acc.z * dT);

		*orientation = dr->R0;

		break;

	case DISDRMethodOther:
		/* how on earth would we handle this? callbacks, perhaps? */
		break;

	default:
		/* how on earth would we handle this? callbacks, perhaps? */
		break;
	}

}

void
DISComputeDRMatrix(dis_dr_parameters * dr, double dT, VMatrix * m)
{
	double    theta = dr->omega * dT;
	double    cosTheta = cos(theta);
	double    sinTheta = sin(theta);
	double    Icos, x = (1.0 - cosTheta);
	int       i, j;

	for (i = 0; i < 3; ++i) {
		for (j = 0; j < 3; ++j) {
			if (i == j) {
				Icos = cosTheta;
			}
			else {
				Icos = 0.0;
			}
			m->m[i][j] = Icos -
				dr->skew.m[i][j] * sinTheta +
				dr->aat.m[i][j] * x;
		}
	}

	m->m[0][3] = m->m[1][3] = m->m[2][3] = 0.0;
	m->m[3][0] = m->m[3][1] = m->m[3][2] = 0.0;
	m->m[3][3] = 1.0;
}

void
DISGetDRThresholds(dis_dr_parameters * dr, double *time, double *location, double *orientation)
{
	*time = dr->timeThreshold;
	*location = dr->locationThreshold;
	*orientation = dr->orientationThreshold;
}

void
DISSetDRThresholds(dis_dr_parameters * dr, double time, double location, double orientation)
{
	dr->timeThreshold = time;
	dr->locationThreshold = location;
	dr->orientationThreshold = orientation;
}

int
DISTestDRThresholds(dis_dr_parameters *dr, double delta,
			dis_world_coordinates *loc,
			dis_euler_angles *current_orientation_ea)
{
	int result = 0;
	dis_world_coordinates dr_loc, d_loc;
	dis_linear_vel_vector dr_vel;
	double d_squared, d1, d2, d3, orientation_error_squared;
	VMatrix dr_orientation, cur_orientation;

	if (delta > dr->timeThreshold) {
		result |= DR_TIME;
	}
	else {

		DISComputeDRPosition(dr,
					 delta,
					 &dr_loc,
					 &dr_vel,
					 &dr_orientation);

		d_loc.x = loc->x - dr_loc.x;
		d_loc.y = loc->y - dr_loc.y;
		d_loc.z = loc->z - dr_loc.z;
		d_squared = d_loc.x * d_loc.x + d_loc.y * d_loc.y + d_loc.z * d_loc.z;

		if (d_squared > dr->locationThreshold * dr->locationThreshold) {
			result |= DR_LOCATION;
		}

		DISEulerToMatrix (current_orientation_ea, &cur_orientation);
		
		d1= dr_orientation.m[0][0] * cur_orientation.m[0][0] +
			dr_orientation.m[0][1] * cur_orientation.m[0][1] +
			dr_orientation.m[0][2] * cur_orientation.m[0][2]   ;
		d2= dr_orientation.m[1][0] * cur_orientation.m[1][0] +
			dr_orientation.m[1][1] * cur_orientation.m[1][1] +
			dr_orientation.m[1][2] * cur_orientation.m[1][2]   ;
		d3= dr_orientation.m[2][0] * cur_orientation.m[2][0] +
			dr_orientation.m[2][1] * cur_orientation.m[2][1] +
			dr_orientation.m[2][2] * cur_orientation.m[2][2]   ;

		d1 = 1.0 - d1 * d1;
		d2 = 1.0 - d2 * d2;
		d3 = 1.0 - d3 * d3;

		orientation_error_squared = d1 * d1 + d2 * d2 + d3 * d3;

		if (orientation_error_squared > 
			dr-> orientationThreshold * dr-> orientationThreshold) {
			result |= DR_ORIENTATION;
		}

	}

	return result;
}
