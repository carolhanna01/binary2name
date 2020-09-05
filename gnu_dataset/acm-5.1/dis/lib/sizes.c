/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1996,1998, Riley Rainey (rainey@netcom.com)
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

#include <rpc/rpc.h>
#include <dis/dis.h>

void
DISAddPDUSizes(dis_pdu * p)
{
	int       i, j;

	switch (p->hdr.pdu_type) {
	case PDUTypeEmission:
		{
			dis_em_emission_pdu *pdu = (dis_em_emission_pdu *) p;
			dis_em_system_info *s = pdu->system;
			dis_beam_info *b;
			unsigned long len;

			for (i = 0; i < pdu->num_systems; ++i, ++s) {
				b = s->beam;
				len = 0;
				for (j = 0; j < pdu->system[i].num_beams; ++j, ++b) {
					b->beam_data_length = 13 + b->num_targets * 2;
					len += b->beam_data_length;
				}
				s->sys_data_length = (u_char) (5 + len);
			}
		}
		break;

	default:
		break;
	}
}
