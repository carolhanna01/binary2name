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
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <Alib.h>
#include <stdio.h>

extern void EdgeTableToScanLine(AWindow * w), ScanLineDifference(AWindow * w);

void
FrameComplete(AWindow * w)
{

	register ScanLine *tmp;
	register int i;

#ifdef DEBUG
	register int cstop;

#endif

/*
 *  Convert the edge table to scan line segments.
 */

	EdgeTableToScanLine(w);

/*
 *  Determine the differences between this frame and the previous one
 *  an plot the differences.
 */

	ScanLineDifference(w);

/*
 *  This frame now becomes the previous frame; clear the new current frame.
 */

	tmp = w->lastScanLine;
	if (w->doubleBuffered) {
		w->lastScanLine = w->otherLastScanLine;
		w->otherLastScanLine = w->scanLine;
	}
	else {
		w->lastScanLine = w->scanLine;
	}
	w->scanLine = tmp;

	for (i = 0; i < (w->height + 1); ++i) {
		w->scanLine[i].count = 0;
		w->edges[i].head = (Edge *) NULL;
		w->lines[i].head = (Edge *) NULL;
	}

/*
 *  Release the allocated color segments for what was the last frame.
 */

	if (w->curPool == 0) {
		w->curPool = 1;
#ifdef DEBUG
		cstop = w->CSTop1;
#endif
		w->CSTop1 = 0;
	}
	else if (w->curPool == 1) {
		if (w->doubleBuffered) {
			w->curPool = 2;
#ifdef DEBUG
			cstop = w->CSTop2;
#endif
			w->CSTop2 = 0;
		}
		else {
			w->curPool = 0;
#ifdef DEBUG
			cstop = w->CSTop0;
#endif
			w->CSTop0 = 0;
		}
	}
	else {
		w->curPool = 0;
#ifdef DEBUG
		cstop = w->CSTop0;
#endif
		w->CSTop0 = 0;
	}

/*
 *  Release the allocated elements of the edge pool.
 */

#ifdef DEBUG
	printf("color segments = %d; edges = %d\n", cstop, w->EPTop);
#endif

	w->EPTop = 0;

	w->ymin = 0x7FFF;
	w->ymax = -1;

/*
 *  Perform any graphics-dependent finish-up
 */

#ifdef X11
	AX11FlushBufferedSegments(w);
#endif

#ifdef DEBUG
	printf("End of Frame\n");
#endif

}
