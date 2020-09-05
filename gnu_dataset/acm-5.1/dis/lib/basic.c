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

#include <sys/types.h>
#include <dis/dis.h>
#include <stdlib.h>

#ifdef WIN32
typedef __int64 my_quad_t;

#else
typedef long long my_quad_t;

#endif

/*
 *  The NPSNET code handles timestamps in a different way than the
 *  DIS protocol document describes.
 */

#if defined(NPSNET_COMPAT)
int       DIS_NPSNET_COMPAT = 1;

#else
int       DIS_NPSNET_COMPAT = 0;

#endif

#define MILLION			1000000
#define dis_timestamp_const	2147483646L		/* 2 ^ 31 - 1 */

#ifdef WIN32
struct timezone {
	int       keep_compiler_happy;
};

#endif

int
DISGetRealTime(dis_time * result)
{
	struct timeval t;
	struct timezone tz;
	my_quad_t i;

	if (gettimeofday(&t, &tz) != 0) {
		return -1;
	}
	result->hour = t.tv_sec / 3600;
	i = (t.tv_sec % 3600) * MILLION + t.tv_usec;
	i = (i * dis_timestamp_const / 3600) / MILLION;
	result->rel.time = (unsigned int) i;
	result->rel.type = 0;
	return 0;
}

int
DISGetTimestamp(dis_timestamp * result)
{

#if defined(NPSNET_COMPAT)
	time_t    i = time(0);

	result->time = i >> 1;
	result->type = i & 1;
#else
	struct timeval t;
	struct timezone tz;
	my_quad_t i;

	if (gettimeofday(&t, &tz) != 0) {
		return -1;
	}
	i = (t.tv_sec % 3600) * MILLION + t.tv_usec;
	i = (i * dis_timestamp_const / 3600) / MILLION;
	result->time = (unsigned int) i;
	result->type = 0;
#endif
	return 0;
}

void
DISTimestampToTimeval(dis_timestamp * in, struct timeval *out)
{
#if defined(NPSNET_COMPAT)
	out->tv_sec = (in->time << 1) + in->type;
	out->tv_usec = 0;
#else
	my_quad_t i;

	i = (my_quad_t) in->time * MILLION * 3600 / dis_timestamp_const;
	out->tv_sec = (long) ( i / MILLION);
	out->tv_usec = (long) ( i % MILLION );
#endif
}

void
DISTimeToTimeval(dis_time * in, struct timeval *out)
{
	my_quad_t i;

	i = (my_quad_t) in->rel.time * MILLION / dis_timestamp_const;
	out->tv_sec = (long) ( in->hour * 3600 + i / MILLION );
	out->tv_usec = (long) ( i % MILLION );
}

DISResult
DISAddArticulationParm(dis_pdu * p, dis_articulation_parm * parm, int *parmID)
{
	dis_entity_state_pdu *esp = (dis_entity_state_pdu *) p;

	int       n = esp->art_parm_count + 1;

	if (esp->art_parm_count == 0) {
		esp->art_parm = (dis_articulation_parm *)
			malloc(sizeof(dis_articulation_parm));
	}
	else {
		esp->art_parm = (dis_articulation_parm *)
			realloc(esp->art_parm, sizeof(dis_articulation_parm) * n);
	}

/*
 *  Return an error if the memory could not be allocated
 */

	if (esp->art_parm == (dis_articulation_parm *) NULL) {
		esp->art_parm_count = 0;
		return DISResultNoMemory;
	}

	esp->art_parm[esp->art_parm_count] = *parm;
	esp->art_parm_count = n;
	*parmID = n;
	return DISResultOK;
}
