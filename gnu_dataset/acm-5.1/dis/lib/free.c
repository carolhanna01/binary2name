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

void
DISFreePDUComponents(dis_pdu * p)
{
	int       i, j;

	switch (p->hdr.pdu_type) {
	case PDUTypeEmission:
		{
			dis_em_emission_pdu *pdu = (dis_em_emission_pdu *) p;
			dis_em_system_info *s = pdu->system;
			dis_beam_info *b;

			for (i = 0; i < pdu->num_systems; ++i, ++s) {
				b = s->beam;
				for (j = 0; j < s->num_beams; ++j, ++b) {
					if (b->num_targets > 0) {
						free((char *) b->tracked_target);
					}
				}
				if (s->num_beams > 0) {
					free((char *) s->beam);
				}
			}
			if (pdu->num_systems > 0) {
				free((char *) pdu->system);
			}
		}
		break;

	case PDUTypeEntityState:
		{
			dis_entity_state_pdu *pdu = (dis_entity_state_pdu *) p;

			if (pdu->art_parm_count > 0) {
				free(pdu->art_parm);
			}
		}
		break;

	default:
		break;
	}
}
