/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1996,1998, Riley Rainey (rrainey@ix.netcom.com)
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
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923
#endif

/*
 *  In the DIS 2.0 coordinate system:
 *
 *      positive Z axis is North;
 *      positive X axis points to 0N, 0E;
 *      positive Y axis points to 0N 90E.
 *
 *  So, North latitudes are positive; East longitudes are positive.
 *
 *  The world is considered a perfect ellipsoid based on the WGS84
 *  standard -- no correction is made to take into account height differences
 *  between the ellpsoid and the geoid.
 *
 *  "The Surveying Handbook", edited by Brinker and Minnick contains a decent
 *  discussion of the technical issues required to understand what's
 *  going on in this code.
 */

/*
 *  Shift location d meters on a given geodetic course (radians)
 */

void
DISUpdateWorldCoordinates(WorldCoordinates * p,
						  double cos_course, double sin_course, double d_meters)
{
	double    n1, n2, m1;
	double    sin_lat, sin_lat_sqr, tan_lat, sin_course_sqr;
	double    delta_latitude, delta_longitude, d_sqr, cos_lat;
	double    B, C, /* D, */ E, h, sin_newlat;

/*  Increase our height to the height above the reference ellipsoid */

	double    wgs84_a = WGS84_MAJOR + p->z;

	sin_lat = sin(p->latitude);
	sin_lat_sqr = sin_lat * sin_lat;
	cos_lat = cos(p->latitude);
	tan_lat = sin_lat / cos_lat;
	sin_course_sqr = sin_course * sin_course;
	d_sqr = d_meters * d_meters;

	n1 = wgs84_a / sqrt(1.0 - WGS84_ECC_SQR * sin_lat_sqr);
	m1 = (wgs84_a * (1.0 - WGS84_ECC_SQR)) /
		pow(1.0 - WGS84_ECC_SQR * sin_lat_sqr, 1.5);

	B = 1.0 / m1;

	h = d_meters * B * cos_course;

	C = tan_lat / (2.0 * m1 * n1);

#ifdef notdef
	D = (3.0 * WGS84_ECC_SQR * sin_lat * cos_lat) /
		(2.0 * (1.0 - WGS84_ECC_SQR * sin_lat_sqr));
#endif

	E = (1.0 + 3.0 * tan_lat * tan_lat) *
		(1.0 - WGS84_ECC_SQR * sin_lat_sqr) / (6.0 * wgs84_a * wgs84_a);

	delta_latitude = d_meters * B * cos_course -
		d_sqr * C * sin_course_sqr -
		h * d_sqr * E * sin_course_sqr;

	p->latitude += delta_latitude;
	if (p->latitude > M_PI_2) {
		p->latitude -= M_PI_2;
	}
	else if (p->latitude < -M_PI_2) {
		p->latitude += M_PI_2;
	}

	sin_newlat = sin(p->latitude);

	n2 = wgs84_a / sqrt(1.0 - WGS84_ECC_SQR * sin_newlat * sin_newlat);

	delta_longitude = (d_meters * sin_course) / (n2 * cos(p->latitude));

	p->longitude += delta_longitude;
	if (p->longitude > M_PI) {
		p->longitude -= M_PI;
	}
	else if (p->longitude < -M_PI) {
		p->longitude += M_PI;
	}
}

/*
 *  Shift location d_meters meters on a given geodetic course (radians)
 *  returns new outbound heading correct for the new location in delta_course_rad
 */

void
DISUpdateWorldCoordinatesEx(WorldCoordinates * p,
						  double cos_course, double sin_course, double d_meters,
						  double * delta_course_rad )
{
	double    n1, n2, m1;
	double    sin_lat, sin_lat_sqr, tan_lat, sin_course_sqr;
	double    delta_latitude, delta_longitude, d_sqr, cos_lat;
	double    B, C, /* D, */ E, h, sin_newlat;
	double    old_latitude, phi_m, sin_phi_m, cos_phi_m;

/* arc-seconds per rad */
	const double rho = 206264.8062470964;

/*  Increase our height to the height above the reference ellipsoid */

	double    wgs84_a = WGS84_MAJOR + p->z;

	sin_lat = sin(p->latitude);
	sin_lat_sqr = sin_lat * sin_lat;
	cos_lat = cos(p->latitude);
	tan_lat = sin_lat / cos_lat;
	sin_course_sqr = sin_course * sin_course;
	d_sqr = d_meters * d_meters;

	n1 = wgs84_a / sqrt(1.0 - WGS84_ECC_SQR * sin_lat_sqr);
	m1 = (wgs84_a * (1.0 - WGS84_ECC_SQR)) /
		pow(1.0 - WGS84_ECC_SQR * sin_lat_sqr, 1.5);

	B = 1.0 / m1;

	h = d_meters * B * cos_course;

	C = tan_lat / (2.0 * m1 * n1);

#ifdef notdef
	D = (3.0 * WGS84_ECC_SQR * sin_lat * cos_lat) /
		(2.0 * (1.0 - WGS84_ECC_SQR * sin_lat_sqr));
#endif

	E = (1.0 + 3.0 * tan_lat * tan_lat) *
		(1.0 - WGS84_ECC_SQR * sin_lat_sqr) / (6.0 * wgs84_a * wgs84_a);

	delta_latitude = d_meters * B * cos_course -
		d_sqr * C * sin_course_sqr -
		h * d_sqr * E * sin_course_sqr;

	old_latitude = p->latitude;

	p->latitude += delta_latitude;
	if (p->latitude > M_PI_2) {
		p->latitude -= M_PI_2;
	}
	else if (p->latitude < -M_PI_2) {
		p->latitude += M_PI_2;
	}

	phi_m = old_latitude + delta_latitude / 2.0;
	sin_phi_m = sin(phi_m);
	cos_phi_m = cos(phi_m);

	sin_newlat = sin(p->latitude);

	n2 = wgs84_a / sqrt(1.0 - WGS84_ECC_SQR * sin_newlat * sin_newlat);

	delta_longitude = (d_meters * sin_course) / (n2 * cos(p->latitude));

	*delta_course_rad = delta_longitude * sin_phi_m / cos(delta_latitude / 2.0) +
		delta_longitude * (sin_phi_m * cos_phi_m * cos_phi_m) / rho;

	p->longitude += delta_longitude;
	if (p->longitude > M_PI) {
		p->longitude -= M_PI;
	}
	else if (p->longitude < -M_PI) {
		p->longitude += M_PI;
	}
}


/*
 *  Convert cartesian geocentric coordinates into WGS84 geodetic lat/lon/z
 */

void
DISGeocentricToWorldCoordinates(dis_world_coordinates * loc,
								WorldCoordinates * p)
{
	double    a_sqr = WGS84_MAJOR * WGS84_MAJOR, b_sqr = WGS84_MINOR * WGS84_MINOR;
	double    w, x, x_sqr, z, delta_x, cos_x;
	double    f, f_prime, w0, z0;

	w = sqrt(loc->x * loc->x + loc->y * loc->y);
	z = loc->z;

/*
 *  x is the sine of the parametric latitude.  Use the sine of the geocentric
 *  latitude as the initial guess.
 */

	if (w == 0.0 && z == 0.0) {
		p->latitude = 0.0;
		p->longitude = 0.0;
		p->z = 0.0;
		return;
	}

	x = z / sqrt(w * w + z * z);

/*
 *  Compute x with accuracy that will yield a lat/lon accuracy of
 *  about 0.0001 arc-seconds (~ 0.10 foot).
 */

	for (delta_x = 1.0; fabs(delta_x) > 4.8E-10;) {

		x_sqr = x * x;

		cos_x = sqrt(1.0 - x_sqr);

		f = 2.0 * (WGS84_MAJOR * x * w - a_sqr * x * cos_x - WGS84_MINOR * cos_x * z +
				   b_sqr * cos_x * x);

		f_prime = 2.0 * (a_sqr + 2.0 * (a_sqr * x_sqr) - WGS84_MAJOR * w * x_sqr +
						 b_sqr - 2.0 * b_sqr * x_sqr + WGS84_MINOR * x * z);

		delta_x = f / f_prime;
		x -= delta_x;
	}

	z0 = WGS84_MINOR * x;
	w0 = WGS84_MAJOR * sqrt(1.0 - x * x);

	p->z = sqrt((z - z0) * (z - z0) + (w - w0) * (w - w0));
	p->latitude = atan(z0 / (w0 * (1.0 - WGS84_ECC_SQR)));
	p->longitude = atan2(loc->y, loc->x);
}

/*
 *  Convert WGS84 geodetic lat/lon/z into cartesian geocentric coordinates
 */

void
DISWorldCoordinatesToGeocentric(WorldCoordinates * w,
								dis_world_coordinates * p)
{
	double    N, N1;
	double    cos_latitude, sin_latitude;

	sin_latitude = sin(w->latitude);
	cos_latitude = cos(w->latitude);

/*
 *  N is the length of the normal line segment from the surface to the
 *  spin axis.
 */

	N = WGS84_MAJOR / sqrt(1.0 - (WGS84_ECC_SQR * sin_latitude * sin_latitude));

/*
 *  N1 lengthens the normal line to account for height above the surface
 */

	N1 = N + w->z;

	p->x = N1 * cos_latitude * cos(w->longitude);
	p->y = N1 * cos_latitude * sin(w->longitude);
	p->z = (((WGS84_MINOR * WGS84_MINOR) / (WGS84_MAJOR * WGS84_MAJOR)) * N + w->z) * sin_latitude;
}

char     *
DISLatitudeToString(char *s, double la, LatLongDisplayFormat mode)
{

	int       d, m;
	double    dla, dmin, dsec;
	double    round_dms = 1.0 / (36000.0 * 2.0);
	double    round_dm = 1.0 / (600.0 * 2.0);
	char     *ns;

	round_dms = round_dm = 0.0;

	switch (mode) {

	case LLM_DMS:
		ns = (la >= 0.0) ? "N" : "S";
		dla = RADtoDEG(fabs(la)) + round_dms;
		d = (int) dla;
		dmin = (dla - (double) d) * 60.0;
		m = (int) dmin;
		dsec = (dmin - (double) m) * 60.0;
		sprintf(s, "%d %d %.1f %s", d, m, dsec, ns);
		break;

	case LLM_DM:
		ns = (la >= 0.0) ? "N" : "S";
		dla = RADtoDEG(fabs(la)) + round_dm;
		d = (int) dla;
		dmin = (dla - (double) d) * 60.0;
		sprintf(s, "%d %.1f %s", d, dmin, ns);
		break;

	case LLM_D:
		ns = (la >= 0.0) ? "N" : "S";
		dla = RADtoDEG(fabs(la)) + 0.05;
		sprintf(s, "%.1f %s", dla, ns);
		break;

	case LLM_SIGNED_D:
		sprintf(s, "%.1f", RADtoDEG(la));
		break;
	}

	return s;

}

char     *
DISLongitudeToString(char *s, double lo, LatLongDisplayFormat mode)
{

	int       d, m;
	double    dlo, dmin, dsec;
	double    round_dms = 1.0 / (36000.0 * 2.0);
	double    round_dm = 1.0 / (600.0 * 2.0);
	char     *ew;

	round_dms = round_dm = 0.0;

	switch (mode) {

	case LLM_DMS:
		ew = (lo >= 0.0) ? "E" : "W";
		dlo = RADtoDEG(fabs(lo)) + round_dms;
		d = (int) dlo;
		dmin = (dlo - (double) d) * 60.0;
		m = (int) dmin;
		dsec = (dmin - (double) m) * 60.0;
		sprintf(s, "%d %d %.1f %s", d, m, dsec, ew);
		break;

	case LLM_DM:
		ew = (lo >= 0.0) ? "E" : "W";
		dlo = RADtoDEG(fabs(lo)) + round_dm;
		d = (int) dlo;
		dmin = (dlo - (double) d) * 60.0;
		sprintf(s, "%d %.1f %s", d, dmin, ew);
		break;

	case LLM_D:
		ew = (lo >= 0.0) ? "E" : "W";
		dlo = RADtoDEG(fabs(lo)) + 0.05;
		sprintf(s, "%.1f %s", dlo, ew);
		break;

	case LLM_SIGNED_D:
		sprintf(s, "%.1f", RADtoDEG(lo));
		break;

	}

	return s;

}

#define STATE_INITIAL	0
#define STATE_WORD	1
#define STATE_INTEGER	2
#define STATE_FLOAT	3

typedef enum {
	EndOfFile,
	TOKEN_FLOAT,
	TOKEN_LONG,
	TOKEN_DASH,
	TOKEN_NORTH,
	TOKEN_SOUTH,
	TOKEN_EAST,
	TOKEN_WEST
} token_id;

typedef union {
	double    double_value;
	long      long_value;
} lex_val;

static lex_val lex_value;

struct lex_record {
	char     *s;
	FILE     *f;
	int       lookahead_valid;
	int       lookahead;
	int       stack_top;
	lex_val   value_stack[16];
};

static int
input(struct lex_record *p)
{
	int       val;

	if (p->lookahead_valid) {
		p->lookahead_valid = 0;
		val = p->lookahead;
	}
	else if (p->s) {
		val = *(p->s)++;
	}
	else {
		val = fgetc(p->f);
	}
	return val;
}

#define push_value(p, type, val) \
	p->value_stack[p->stack_top++].type = val

#define pop_value(p, type) (p->value_stack[--p->stack_top].type)

#define unput(p, c)	{ p->lookahead = c; p->lookahead_valid = 1; }

#define InitializeLexRecord(p)	{ p->lookahead_valid = 0; }

static char token[256];
static int token_length = 0;

static    token_id
NextTokenx(struct lex_record *p)
{
	register int c, state = STATE_INITIAL;

	token_length = 0;

	while ((c = input(p)) != EOF) {

		switch (state) {

		case STATE_INITIAL:

			if (isspace(c)) {
				continue;
			}
			else if (isdigit(c)) {
				token[token_length++] = c;
				state = STATE_INTEGER;
			}
			else if (c == '.') {
				token[token_length++] = c;
				state = STATE_FLOAT;
			}
			else {
				token[0] = c;
				token[1] = '\0';
#ifdef DEBUG
				printf("other %s\n", token);
#endif
				switch (c) {
				case '-':
					return TOKEN_DASH;
				case 'n':
				case 'N':
					return TOKEN_NORTH;
				case 'e':
				case 'E':
					return TOKEN_EAST;
				case 's':
				case 'S':
					return TOKEN_SOUTH;
				case 'w':
				case 'W':
					return TOKEN_WEST;
/*
 *  invalid character
 */
				default:
					return EndOfFile;
				}
			}
			break;

		case STATE_INTEGER:
		case STATE_FLOAT:
			if (isspace(c) ||
				c == '-' ||
				toupper(c) == 'N' ||
				toupper(c) == 'S' ||
				toupper(c) == 'W' ||
				toupper(c) == 'E') {
				token[token_length] = '\0';
				unput(p, c);
				if (state == STATE_INTEGER) {
					lex_value.long_value = atoi(token);
					return TOKEN_LONG;
				}
				else {
					lex_value.double_value = atof(token);
					return TOKEN_FLOAT;
				}
			}
			else {
				if (c == '.') {
					state = STATE_FLOAT;
				}
				token[token_length++] = c;
			}
			break;

		default:
			token[token_length++] = c;
			break;
		}
	}

	return EndOfFile;
}

static    token_id
NextToken(struct lex_record *p)
{
	token_id  t;

	t = NextTokenx(p);

#ifdef DEBUG
	printf("token %s\n", token);
#endif
	return t;
}

static int
ParseLatitude(struct lex_record *p)
{
	double    x = 0.0;
	double    divider = 1.0;
	int       int_valid = 1;
	token_id  t;

	t = NextToken(p);
	for (;;) {
		switch (t) {
		case TOKEN_NORTH:
			lex_value.double_value = x;
			return 0;

		case TOKEN_SOUTH:
			lex_value.double_value = -x;
			return 0;

		case TOKEN_LONG:
			if (int_valid) {
				x += lex_value.long_value / divider;
				divider *= 60.0;
				t = NextToken(p);
				if (t == TOKEN_DASH) {
					t = NextToken(p);
				}
			}
			else {
				return -1;
			}
			break;

		case TOKEN_FLOAT:
			int_valid = 0;
			x += lex_value.double_value / divider;
			divider *= 60.0;
			t = NextToken(p);
			if (t == TOKEN_DASH) {
				t = NextToken(p);
			}
			break;
		default:
			return -1;
		}
	}
}

static int
ParseLongitude(struct lex_record *p)
{
	double    x = 0.0;
	double    divider = 1.0;
	int       t, int_valid = 1;

	t = NextToken(p);
	for (;;) {
		switch (t) {
		case TOKEN_EAST:
			lex_value.double_value = x;
			return 0;

		case TOKEN_WEST:
			lex_value.double_value = -x;
			return 0;

		case TOKEN_LONG:
			if (int_valid) {
				x += lex_value.long_value / divider;
				divider *= 60.0;
				t = NextToken(p);
				if (t == TOKEN_DASH) {
					t = NextToken(p);
				}
			}
			else {
				return -1;
			}
			break;

		case TOKEN_FLOAT:
			int_valid = 0;
			x += lex_value.double_value / divider;
			divider *= 60.0;
			t = NextToken(p);
			if (t == TOKEN_DASH) {
				t = NextToken(p);
			}
			break;

		default:
			return -1;
		}
	}
}

char     *
DISStringToLatLong(char *s, WorldCoordinates * w)
{
	struct lex_record p;

	p.s = s;
	p.lookahead_valid = 0;

	if (ParseLatitude(&p) != 0) {
		return 0;
	}
	w->latitude = DEGtoRAD(lex_value.double_value);

	if (ParseLongitude(&p) != 0) {
		return 0;
	}
	w->longitude = DEGtoRAD(lex_value.double_value);
	w->z = 0.0;
	return p.s;
}
