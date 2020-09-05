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

#include <math.h>
#include <Vlib.h>
#include <VFont.h>
#include <VRoman.h>

void
VDrawRotatedStrokeString(v, x, y, str, len, scale, zinfo)
Viewport *v;
register int x, y;
register char *str;
register int len;
register double angle;
register int scale;
ZInfo    *zinfo;
{

	register int c, i, k, m, sina, cosa;
	register VGlyphVertex *p;
	register int x1, y1, x2, y2;

	sina = sin(angle) * 16384.0;
	cosa = cos(angle) * 16384.0;

	for (; len > 0; --len) {

		if ((c = *str++) < 128) {
			k = VRomanGlyph[c].path_start;
			for (i = 0; i < VRomanGlyph[c].path_count; ++i, ++k) {
				p = &VRomanVertex[VRomanPath[k].vertex_start];
				x1 = x + p->x * scale / 25600;
				y1 = y - p->y * scale / 25600;
				++p;
				for (m = 1; m < VRomanPath[k].vertex_count; ++m, ++p) {
					x2 = x + p->x * scale / 25600;
					y2 = y - p->y * scale / 25600;
					DrawLine(v->w, x1, y1, x2, y2, zinfo);
					x1 = x2;
					y1 = y2;
				}
			}

			x += VRomanGlyph[c].glyph_width * scale / 25600;

		}
	}
}
