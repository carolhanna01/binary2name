/*
 *  acm : an aerial combat simulator for X
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
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */
#include "pm.h"
#include "box.h"
#include <stdio.h>
#include <string.h>

static FILE *bbin = 0, *bbout = 0;

short     rp_map[MAXPLAYERS], rm_map[MAXPROJECTILES];
short     pp_map[MAXPLAYERS], pm_map[MAXPROJECTILES];

static char *no_room =
"No room in player table to add another black box object.\n";

void
newBlackBoxCraft(int id, int type, char *name)
{
	register craft *c;
	register int i, max;
	register short *p;

	/*  per PREfix */
	if (type != CT_PLANE && type != CT_DRONE &&
		type != CT_MISSILE && type != CT_CANNON) {
		printf("Invalid craft type passed to newBlackBoxCraft()\n");
	}

	switch (type) {

	case CT_PLANE:
	case CT_DRONE:
		if ((i = newPlane(name, -1)) >= 0) {
			c = &ptbl[i];
			c->type = type;
			c->flags = FL_BLACK_BOX;
			strcpy(c->name, "black-box");
			strcpy(c->display, "*none*");
			pp_map[id] = i;
		}
		else {
			fprintf(stderr, no_room);
		}
		return;
/*NOTREACHED */ break;

	case CT_MISSILE:
	case CT_CANNON:
		c = mtbl;
		max = MAXPROJECTILES;
		p = rm_map;
		break;
	}

	for (i = 0; i < max; ++i, ++c) {
		if (c->type == CT_FREE) {
			c->type = type;
			c->flags = FL_BLACK_BOX;
			c->cinfo = lookupCraft(name);
			p[id] = i;
		}
	}
}

void
startBlackBoxRecording(void)
{
	int       i;

	if ((bbout = fopen("./black_box_output", "w")) == (FILE *) NULL) {
		fprintf(stderr, "unable to open black box recording file\n");
	}
	for (i = 0; i < MAXPLAYERS; ++i) {
		rp_map[i] = -1;
	}
	for (i = 0; i < MAXPROJECTILES; ++i) {
		rm_map[i] = -1;
	}
}

void
endBlackBoxRecording(void)
{
	fclose(bbout);
	bbout = (FILE *) NULL;
}

void
startBlackBoxPlayback(void)
{
	int       i;

	if ((bbin = fopen("./black_box_input", "r")) == (FILE *) NULL) {
		fprintf(stderr, "unable to open black box playback file\n");
	}
	for (i = 0; i < MAXPLAYERS; ++i) {
		pp_map[i] = -1;
	}
	for (i = 0; i < MAXPROJECTILES; ++i) {
		pm_map[i] = -1;
	}
}

/*
 *  Update items under the control of black box playback
 */

void
blackBoxInput(void)
{
	register int i;
	BBRecord  rec;
	craft    *c;

	if (bbin) {
		while (fread((char *) &rec, BB_HDR_SIZE, 1, bbin) == 1) {

			c = (rec.table == 0) ? &ptbl[pp_map[rec.id]] :
				&mtbl[pm_map[-(int) rec.id]];

			switch (rec.rectype) {

			case BB_TYPE_SHORT_STATE:
				fread((char *) &rec.u.short_state,
					  sizeof(rec.u.short_state), 1, bbin);
				if (pp_map[rec.id] == -1)
					break;
				c->prevSg = c->Sg;
				c->w = rec.u.short_state.w;
				c->Sg = rec.u.short_state.Sg;
				c->Cg = rec.u.short_state.Cg;
				c->curRoll = rec.u.short_state.roll;
				c->curPitch = rec.u.short_state.pitch;
				c->curHeading = rec.u.short_state.heading;
				break;

			case BB_TYPE_ADD_OBJECT:
				fread((char *) &rec.u.object,
					  sizeof(rec.u.object), 1, bbin);
				newBlackBoxCraft(rec.id, rec.u.object.type,
								 rec.u.object.name);
				break;

			case BB_TYPE_DELETE_OBJECT:
				pp_map[rec.id] = -1;
				killPlayer(c);
				break;

			case BB_TYPE_END_OF_FRAME:
				return;

			default:
				fprintf(stderr, "unknown rectype in\
 black box recording: %d\n", rec.rectype);
				fclose(bbin);
				bbin = (FILE *) NULL;
				break;
			}
		}
		fclose(bbin);
		bbin = (FILE *) NULL;
		for (i = 0; i < MAXPLAYERS; ++i) {
			if (pp_map[i] != -1)
				killPlayer(&ptbl[pp_map[i]]);
		}
	}
}

/*
 *  Write out black box records
 */

void
blackBoxOutput(void)
{
	register int i;
	register craft *c;
	BBRecord  rec;

	if (bbout) {
		for (i = 0, c = ptbl; i < MAXPLAYERS; ++i, ++c) {

			if (c->type != CT_FREE) {

				if (rp_map[i] == -1) {
					rp_map[i] = i;
					rec.rectype = BB_TYPE_ADD_OBJECT;
					rec.table = 0;
					rec.id = i;
					rec.u.object.type = c->type;
					strcpy(rec.u.object.name,
						   c->cinfo->name);
					fwrite((char *) &rec, BB_HDR_SIZE, 1, bbout);
					fwrite((char *) &rec.u.object,
						   sizeof(rec.u.object), 1, bbout);
				}
				rec.rectype = BB_TYPE_SHORT_STATE;
				rec.table = 0;
				rec.id = i;
				rec.u.short_state.w = c->w;
				rec.u.short_state.Sg = c->Sg;
				rec.u.short_state.Cg = c->Cg;
				rec.u.short_state.roll = c->curRoll;
				rec.u.short_state.pitch = c->curPitch;
				rec.u.short_state.heading = c->curHeading;
				fwrite((char *) &rec, BB_HDR_SIZE, 1, bbout);
				fwrite((char *) &rec.u.object,
					   sizeof(rec.u.short_state), 1, bbout);
			}
		}
		for (i = 0; i < MAXPROJECTILES; ++i) {
			rm_map[i] = -1;
		}

		rec.rectype = BB_TYPE_END_OF_FRAME;
		rec.id = 0;
		fwrite((char *) &rec, BB_HDR_SIZE, 1, bbout);
	}
}

void
blackBoxKillPlayer(int id)
{
	BBRecord  rec;

	if (bbout) {
		rec.rectype = BB_TYPE_DELETE_OBJECT;
		rec.id = id;
		fwrite((char *) &rec, BB_HDR_SIZE, 1, bbout);
	}
}
