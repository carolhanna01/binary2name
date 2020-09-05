/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1991-1998  Riley Rainey
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

#ifndef _dis_h
#define _dis_h

#include <dis/disenum.h>
#include <dis/disenum2.h>
#include <dis/disenum3.h>
#include <rpc/types.h>

#if defined(__cplusplus)
extern    "C" {
#endif

#include <dis/disp.h>

#if defined(__cplusplus)
};

#endif
#include <dis/simx.h>
#include <dis/datum.h>
#include <sys/types.h>
#ifdef WIN32
/*#include <crtdbg.h>*/
#else
#include <sys/time.h>
#include <netinet/in.h>
#endif
#include <math.h>

#if defined(__cplusplus)
extern    "C" {
#endif

/*
 *  This is a bit of a hack, we're borrowing the VMatrix definition
 *  from the Vlib, but I don't want to make DIS/x completely dependent
 *  on Vlib, sooo ...
 */

#if defined(__Vlib) || defined (_vlibmath_h)
#include <Vlibmath.h>
#else
	typedef struct _VMatrix {
		double    m[4][4];
	} VMatrix;
#endif

#ifdef WIN32
#define inline __inline
#endif

	static inline void
	          DISMatrixMultByRank(VMatrix * Mt1, VMatrix * Mt2, VMatrix * R, int rank) {
		register int I, J, K;
		register double x;

		for       (I = 0; I < rank; ++I)
			for       (J = 0; J < rank; ++J) {
				x = 0.0;
				for (K = 0; K < rank; ++K)
					x += Mt1->m[K][J] * Mt2->m[I][K];
				R->m[I][J] = x;
			}
	}
	typedef struct _dis_dr_parameters {
		double    timeThreshold;        /* [ seconds ]  */
		double    locationThreshold;    /* [ meters ]  */
		double    orientationThreshold; /* [ radians ] */
		double    omega;		/* angular velocity magnitude */
		VMatrix   R0;			/* orientation based on euler angles in entity state */
		VMatrix   skew;
		VMatrix   aat;
		dis_entity_state_pdu pdu;	/* saved entity state PDU */
	} dis_dr_parameters;

#define DR_TIME         0x01
#define DR_LOCATION     0x02
#define DR_ORIENTATION  0x04

	typedef struct _DISDestinationAddress {
		struct sockaddr_in addr;
		int       type;			/* 0=broadcast; 1=other */
	} DISDestinationAddress;

	typedef struct _DISTransceiver {
		int       s;
		DISDestinationAddress dest[32];
		int       num_dest;
	} DISTransceiver;

	typedef enum {
		DISResultOK = 0,
		DISResultError = 1,
		DISResultNoMemory = 2
	} DISResult;

	typedef struct _DISxApplicationInfo {
		DISTransceiver *xcvr;
		dis_pdu_header hdr;
		dis_simulation_addr id;
		u_short   last_event;
		u_short   last_request;
		u_short   last_entity;
	} DISxApplicationInfo;

	extern DISxApplicationInfo *
	          DISxInitializeApplication(unsigned int exercise_id,
										unsigned int site_id,
										unsigned int application_id);

	extern void DISxCloseApplication(DISxApplicationInfo *);
	extern int DISxReadPDU(DISxApplicationInfo *, dis_pdu *);
	extern int DISxWritePDU(DISxApplicationInfo *, dis_pdu *);
	extern void DISxGetSimulationAddress(DISxApplicationInfo * info,
										 dis_simulation_addr * p);
	extern dis_entity_id *DISxIssueEntityID(DISxApplicationInfo * info,
											dis_entity_id * p);
	extern dis_event_id *DISxIssueEventID(DISxApplicationInfo * info,
										  dis_event_id * p);
	extern dis_request_id DISxIssueRequestID( DISxApplicationInfo * info );

	extern DISResult DISAddArticulationParm(dis_pdu * p,
											dis_articulation_parm * parm,
											int *parmID);

	extern int DISSetNBIOState(DISTransceiver *, int);
	extern DISTransceiver *DISOpenTransceiver(int port);

	extern void DISCloseTransceiver(DISTransceiver *);
	extern int DISReadPDU(DISTransceiver *, dis_pdu *);
	extern int DISWritePDU(DISTransceiver *, dis_pdu *);

	extern void DISFreePDUComponents(dis_pdu *);
	extern void DISAddPDUSizes(dis_pdu *);

	extern int DISGetRealTime(dis_time * result);
	extern int DISGetTimestamp(dis_timestamp * result);
	extern void DISTimeToTimeval(dis_time * in, struct timeval *out);
	extern void DISTimestampToTimeval(dis_timestamp * in, struct timeval *out);

/*
 *  Round world stuff
 */

	typedef struct _worldcoordinates {
		double    latitude;		/* [radians, north positive] */
		double    longitude;	/* [radians, east positive] */
		double    z;			/* above reference ellipsoid [meters] */
	} WorldCoordinates;

	typedef enum {
		LLM_DMS,				/* dd mm ss.s [EWNS] */
		LLM_DM,					/* dd mm [EWNS] */
		LLM_D,					/* dd [EWNS] */
		LLM_SIGNED_D
	} LatLongDisplayFormat;

#ifndef M_PI
#define M_PI    3.14159265358979323846
#endif

#ifndef DEGtoRAD
#define DEGtoRAD(x)	((x) * M_PI / 180.0)
#define RADtoDEG(x)	((x) * 180.0 / M_PI)
#endif

#define SECtoRAD(x)	((x) * M_PI / (180.0 * 3600.0))

#define WGS84_MAJOR	6378137.0	/* meters */
#define WGS84_MINOR	6356752.3142	/* meters */
#define WGS84_ECC	0.081819190928906199466		/* eccentricity */
#define WGS84_ECC_SQR	0.006694380004260806515		/* eccentricity squared */

	extern void
	          DISUpdateWorldCoordinates(WorldCoordinates * p,
						double sin_course, double cos_course, double d_meters);
	
	extern void
			  DISUpdateWorldCoordinatesEx(WorldCoordinates * p,
						double cos_course, double sin_course, double d_meters,
						double * delta_course_rad );
	extern char *
	          DISLatitudeToString(char *s, double la, LatLongDisplayFormat mode);
	extern char *
	          DISLongitudeToString(char *s, double lo, LatLongDisplayFormat mode);
	extern void
	          DISGeocentricToWorldCoordinates(dis_world_coordinates * p, WorldCoordinates * w);
	extern void
	          DISWorldCoordinatesToGeocentric(WorldCoordinates * w, dis_world_coordinates * p);
	extern char *
	          DISStringToLatLong(char *s, WorldCoordinates * w);

	extern void
	          DISProcessNewDRParameters(dis_entity_state_pdu * pdu, dis_dr_parameters * dr);
	extern void
	          DISGenerateDRParameters(dis_entity_state_pdu * pdu, dis_dr_parameters * dr);
	extern void
	          DISComputeDRMatrix(dis_dr_parameters * dr, double dT, VMatrix * m);
	extern void
	          DISComputeDRPosition(dis_dr_parameters * dr,
								   double dT,
								   dis_world_coordinates * pos,
								   dis_linear_vel_vector * vel,
								   VMatrix * orientation);
	extern void
		DISGetDRThresholds(dis_dr_parameters *, double *time,
			double *location, double *orientation);
	extern void
		DISSetDRThresholds(dis_dr_parameters *, double time,
			double location, double orientation);
	extern int
		DISTestDRThresholds(dis_dr_parameters *, double time,
			dis_world_coordinates *current_location,
			dis_euler_angles *current_orientation);

	extern int DISParseEntityID (dis_entity_id *p, 
								 const char * buf, 
								 int bufsize,
								 const char *delim);

	extern int
	    DISxSetProtocolVersion(int version);

	extern int
	    DISxSetPDUProtocolFamily (int pdu_type, int protocol_family);

	extern DISxApplicationInfo * dis_getApplicationInfo(void);

	extern void DISInitializeDatumInfo (dis_datum_spec_record *pd);

#ifdef WIN32
	extern int gettimeofday (struct timeval *, struct timezone *);
#endif

#if defined(__cplusplus)
};

#endif

#endif
