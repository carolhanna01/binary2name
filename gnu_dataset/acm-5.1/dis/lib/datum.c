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
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#include <stdlib.h>

dis_variable_datum *
DISCreateVariableDatumString(dis_datum_type id, char *s, int str_len)
{
	dis_variable_datum *p;
	int       len = ((str_len + 7) / 8) * 8;

	p = (dis_variable_datum *)
		malloc(sizeof(dis_variable_datum) + len);

	if (!p) {
		return p;
	}

	p->datum_id = id;
	p->value_length = str_len * 8;
	p->value.ptr_value = (unsigned char *) (p + 1);

	memset(p->value.ptr_value, 0, len);
	memcpy(p->value.ptr_value, s, str_len);
	return p;
}

void
DISInitializeDatumInfo ( dis_datum_spec_record *pd )
{
	memset (pd, 0, sizeof(dis_datum_spec_record));
}
