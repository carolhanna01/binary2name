/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


#ifndef _MAVLIB_D3D_INCLUDE
#define _MAVLIB_D3D_INCLUDE

#ifdef __cplusplus
extern "C" {
#endif

#define MAVLIB_WIDTH 640
#define MAVLIB_HEIGHT 480
#define MAVLIB_BITS 16
#define MAVLIB_ZBITS 16
#define GFX_MAX_TEXTURE 1000

/* gfx funcs */
void mavlib_gfxRenderStateSet (void);
void mavlib_gfxFullscreen (MAV_window *w);
D3DMATRIX mavlib_convertMavMatrix (MAV_matrix);

/* D3D globals */
extern LPDIRECT3D8 mavlib_D3D;
extern LPDIRECT3DDEVICE8 mavlib_D3DDevice;
extern LPDIRECT3DTEXTURE8 mavlib_textures[GFX_MAX_TEXTURE];
extern MAV_texture *mavlib_mavTextures[GFX_MAX_TEXTURE];
extern D3DFORMAT mavlib_buffFormat;
extern D3DFORMAT mavlib_texRGBA;
extern D3DFORMAT mavlib_texRGB;
extern D3DMATRIX mavlib_D3DProj;
extern D3DMATRIX mavlib_D3DWorld;
extern int mavlib_2d_x;
extern int mavlib_2d_y;
extern int mavlib_col_r;
extern int mavlib_col_g;
extern int mavlib_col_b;
extern int mavlib_gfx_fullscreen;

/* D3D error traps */
/* call without checking */
#define MAVD3D(a,b) a;
/* exit on error */
#define MAVD3DERR(a,b) if (a != D3D_OK) {fprintf(stderr, "Error: %s\n",b); exit (1);}
/* warn on error */
#define MAVD3DWARN(a,b) if (a != D3D_OK) {fprintf(stderr, "Warning: %s\n",b);}

/* end of D3D globals */

#ifdef __cplusplus
}
#endif

#endif


