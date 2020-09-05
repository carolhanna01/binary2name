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


#include <Vlib.h>
#include <VFont.h>
#include <VRoman.h>
/*ARGSUSED */
int
VFontWidthPixels(Viewport * v, int scale)
{
	return VRomanGlyph['A'].glyph_width * scale / 25600;
}

void
VDrawStrokeString(Viewport * v, int x, int y,
				  unsigned char *str, int len, int scale, ZInfo * zinfo)
{

	register int c, i, k, m;
	register VGlyphVertex *p;
	register int x1, y1, x2, y2;

	for (; len > 0; --len) {

		if ((c = *str++) < 128) {
			k = VRomanGlyph[c].path_start;
			for (i = 0; i < VRomanGlyph[c].path_count; ++i, ++k) {
				p = &VRomanVertex[VRomanPath[k].vertex_start];
				x1 = p->x * scale / 25600 + x;
				y1 = y - p->y * scale / 25600;
				++p;
				for (m = 1; m < VRomanPath[k].vertex_count; ++m, ++p) {
					x2 = p->x * scale / 25600 + x;
					y2 = y - p->y * scale / 25600;
					v->DrawLine(v, x1, y1, x2, y2, zinfo->color);
					x1 = x2;
					y1 = y2;
				}
			}

			x += VRomanGlyph[c].glyph_width * scale / 25600;

		}
	}
}

void
VGetStrokeString(Viewport * v, int x, int y, Segment * seg, int *nseg,
				 unsigned char *str, int len, int scale)
{

	register int c, i, k, m, count;
	register VGlyphVertex *p;
	register int x1, y1, x2, y2;
	Segment  *pseg;

	count = *nseg;

	for (; len > 0; --len) {

		if ((c = *str++) < 128) {
			k = VRomanGlyph[c].path_start;
			for (i = 0; i < VRomanGlyph[c].path_count; ++i, ++k) {
				p = &VRomanVertex[VRomanPath[k].vertex_start];
				x1 = p->x * scale / 25600 + x;
				y1 = y - p->y * scale / 25600;
				++p;
				for (m = 1; m < VRomanPath[k].vertex_count; ++m, ++p) {
					x2 = p->x * scale / 25600 + x;
					y2 = y - p->y * scale / 25600;
					pseg = &seg[count++];
					pseg->x1 = x1;
					pseg->x2 = x2;
					pseg->y1 = y1;
					pseg->y2 = y2;
					x1 = x2;
					y1 = y2;
				}
			}

			x += VRomanGlyph[c].glyph_width * scale / 25600;

		}
	}

	*nseg = count;
}
