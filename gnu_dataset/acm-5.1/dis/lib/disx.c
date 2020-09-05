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
#include <simmgr.h>
#include <stdio.h>

/*
 *  This table defines the correct protocol family based on the pdu type
 */

static unsigned char pdu_family[256] =
{
	0, 1, 2, 2, 1, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5,  /*   0..15 */
	5, 5, 5, 5, 5, 5, 5, 6, 6, 4, 4, 0, 0, 0, 0, 0,  /*  16..31 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /*  32..47 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /*  48..63 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /*  64..79 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /*  80..95 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /*  96..111 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 112..127 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 128..143 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 144..159 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 160..175 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* 176..191 */
};

static int protocol_version = DISProtocolVersionIEEE1278_95;

int
DISxSetProtocolVersion(int version)
{
	int result = protocol_version;

	protocol_version = version;
	return result;
}

int
DISxSetPDUProtocolFamily (int pdu_type, int protocol_family)
{
	int result;

	if (pdu_type < 0 || pdu_type > 255) {
		return -1;
	}

	result = pdu_family[pdu_type];
	pdu_family[pdu_type] = protocol_family;
	return result;
}

int       DISxPortNumber = -1;

DISxApplicationInfo *
DISxInitializeApplication(unsigned int exercise_id,
						  unsigned int site_id,
						  unsigned int application_id)
{
	char      name[64];
	int       result;

	DISxApplicationInfo *p = (DISxApplicationInfo *)
	malloc(sizeof(DISxApplicationInfo));

	if (!p) {
		return p;
	}
	p->hdr.protocol_version = protocol_version;
	p->hdr.exercise_id = exercise_id;
	p->hdr.padding = 0;

	p->last_event = 0;
	p->last_entity = 0;
	p->last_request = 0;

	p->xcvr = DISOpenTransceiver(DISxPortNumber);
	if (!p->xcvr) {
		free(p);
		return NULL;
	}
	if (DISSetNBIOState(p->xcvr, 1) != 0) {
		free(p);
		return NULL;
	}
	if (site_id != 0 && application_id != 0) {
		p->id.site_id = site_id;
		p->id.application_id = application_id;
		result = SIMx_SUCCESS;
	}
	else {

/*
 *  if the site_id is zero, then the site name can be looked-up
 */
		if (site_id == 0) {
			SIMxGetSiteName(name, sizeof(name));
		}
		else {
			sprintf(name, "0x%x", site_id);
		}

		result = SIMxRegisterApplication((char *) NULL,
										 name, application_id, &p->id);
	}

	return (result == SIMx_SUCCESS) ? p : NULL;
}

void
DISxGetSimulationAddress(DISxApplicationInfo * info,
						 dis_simulation_addr * p)
{
	*p = info->id;
}

void
DISxSetExerciseID(DISxApplicationInfo * info,
				  int id)
{
	info->hdr.exercise_id = id;
}

int
DISxWritePDU(DISxApplicationInfo * info, dis_pdu * p)
{
	p->hdr.protocol_version = info->hdr.protocol_version;
	p->hdr.exercise_id = info->hdr.exercise_id;
	p->hdr.protocol_family = pdu_family[p->hdr.pdu_type];
	/* don't set time here until there is a function to set
	   the value (time) and type (relative/absolute) of the time
	   in the DISx library
	   DISGetTimestamp(&p->hdr.time_stamp); */
	return DISWritePDU(info->xcvr, p);
}

int
DISxReadPDU(DISxApplicationInfo * info, dis_pdu * p)
{
	return DISReadPDU(info->xcvr, p);
}

void
DISxCloseApplication(DISxApplicationInfo * info)
{
	DISCloseTransceiver(info->xcvr);
	free(info);
}

dis_request_id
DISxIssueRequestID( DISxApplicationInfo * info )
{
	dis_request_id result;

    result = ++info->last_request;
	if ( info->last_request == 0xfffffff ) {
		info->last_request = 0;
	}
	return result;
}

