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

#ifndef _MAV_GFX_INCLUDE
#define _MAV_GFX_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAVERIK_EXPORTS
#define MAVAPI __declspec(dllexport) 
#else
#define MAVAPI __declspec(dllimport) 
#endif
#else
#define MAVAPI
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>

/* Window manager routines */

MAVAPI void mav_gfxWindowOpen(int id, int x, int y, int w, int h, char *name, char *disp, int wmp, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret);
MAVAPI void mav_gfxWindowClose(int id);
MAVAPI void mav_gfxWindowSet(int id);
MAVAPI void mav_gfxWindowBuffersSwap(void);
MAVAPI void mav_gfxWindowResGet(int *x, int *y);
MAVAPI int  mav_gfxWindowEventGet(int *info);
MAVAPI int  mav_gfxWindowEventPeek(void);
MAVAPI int  mav_gfxWindowPointerGet(int id, int *x, int *y, int *rx, int *ry, int *buts);
MAVAPI void mav_gfxWindowPointerSet(int win, int x, int y);
MAVAPI int  mav_gfxWindowKeyGet(int key);
MAVAPI int  mav_gfxWindowFontSet(char *s, int font, int *width);
MAVAPI void mav_gfxWindowStringDisplay(char *s, int font);



/* Graphics routines */

#define MAV_PROJECTION 1
#define MAV_MODELVIEW 2
#define MAV_PROJANDVIEW 3
#define MAV_FRONT 1
#define MAV_BACK 2
#define MAV_BLEND_OFF 0
#define MAV_BLEND_1 1
#define MAV_POLYGON_LINE 0
#define MAV_POLYGON_FILL 1
#define MAV_ACCUM_ACCUM 1
#define MAV_ACCUM_LOAD 2
#define MAV_ACCUM_RETURN 3
#define MAV_ACCUM_ADD 4
#define MAV_ACCUM_MULT 5
#define MAV_DLISTS_COMPILE 1
#define MAV_DLISTS_COMPILE_AND_EXECUTE 2
#define MAV_FOG_NONE 0
#define MAV_FOG_LINEAR 1
#define MAV_FOG_EXP 2
#define MAV_FOG_EXP2 3

MAVAPI void mav_gfxClipPlaneSet(int id, MAV_clipPlane cp);
MAVAPI void mav_gfxClipPlanesSet(MAV_clipPlanes *cp);
MAVAPI void mav_gfxClipPlaneEnable(int id);
MAVAPI void mav_gfxClipPlaneDisable(int id);
MAVAPI void mav_gfxClearC(void);
MAVAPI void mav_gfxClearZ(void);
MAVAPI void mav_gfxClearA(void);
MAVAPI void mav_gfxClearCZ(void);
MAVAPI void mav_gfxBackgroundColourSet(float r, float g, float b);
MAVAPI void mav_gfxDepthTestSet(int v);
MAVAPI void mav_gfxDepthMaskSet(int v);
MAVAPI void mav_gfxNormalizeSet(int v);
MAVAPI void mav_gfxBackfaceCullSet(int v);
MAVAPI int  mav_gfxBackfaceCullGet(void);
MAVAPI void mav_gfxBufferReadSet(int buf);
MAVAPI void mav_gfxPixelRead(int x, int y, int w, int h, uint32_t *d);
MAVAPI void mav_gfxPixelReadUByte(int x, int y, int w, int h, unsigned char *d);
MAVAPI void mav_gfxPixelDraw(int w, int h, float *v);
MAVAPI void mav_gfxViewPortSet(int x, int y, int w, int h);
MAVAPI void mav_gfxRasterPosSet(MAV_vector v);
MAVAPI void mav_gfxRasterPos2DSet(float x, float y);
MAVAPI void mav_gfxLineWidthSet(float wd);
MAVAPI float mav_gfxLineWidthGet(void);
MAVAPI void mav_gfxLineStippleSet(int factor, unsigned short pattern);
MAVAPI void mav_gfxFogSet(int type, float data1, float data2, float r, float g, float b);
MAVAPI int mav_gfxVisualInfoGet(int *r, int *g, int *b, int *a, int *d, int *db, int *ar, int *ag, int *ab, int *aa, int *sb, int *msb);
MAVAPI void mav_gfxPolygonModeSet(int v);
MAVAPI void mav_gfxMultiSampleSet(int v);
MAVAPI void mav_gfxFinish(void);
MAVAPI void mav_gfxFlush(void);

MAVAPI void mav_gfxMatrixMode(int mode);
MAVAPI void mav_gfxMatrixLoad(MAV_matrix m);
MAVAPI void mav_gfxMatrixPush(void);
MAVAPI void mav_gfxMatrixPop(void);
MAVAPI void mav_gfxMatrixMult(MAV_matrix m);
MAVAPI void mav_gfxMatrixTranslate(MAV_vector v);
MAVAPI void mav_gfxMatrixScale(float x, float y, float z);
MAVAPI void mav_gfxPerspectiveSet(float ncp, float fcp, float fov, float aspect);
MAVAPI void mav_gfxOrthogonalSet(float left, float right, float top, float bottom, float nr, float fr);
MAVAPI MAV_matrix mav_gfxMatrixGet(void);

MAVAPI void mav_gfxPolygonBegin(void);
MAVAPI void mav_gfxPolygonEnd(void);
MAVAPI void mav_gfxTrianglesBegin(void);
MAVAPI void mav_gfxTrianglesEnd(void);
MAVAPI void mav_gfxStripQBegin(void);
MAVAPI void mav_gfxStripQEnd(void);
MAVAPI void mav_gfxStripTBegin(void);
MAVAPI void mav_gfxStripTEnd(void);
MAVAPI void mav_gfxLineClosedBegin(void);
MAVAPI void mav_gfxLineClosedEnd(void);
MAVAPI void mav_gfxLineBegin(void);
MAVAPI void mav_gfxLineEnd(void);
MAVAPI void mav_gfxMeshTBegin(void);
MAVAPI void mav_gfxMeshTEnd(void);

MAVAPI void mav_gfxVertex(MAV_vector v);
MAVAPI void mav_gfxNormal(MAV_vector n);
MAVAPI void mav_gfxTexCoord(MAV_texCoord t);

MAVAPI void mav_gfxColouringModeUse(MAV_palette *p, int mode);
MAVAPI void mav_gfxColourSet(MAV_colour col);
MAVAPI void mav_gfxColourUse(MAV_colour col);
MAVAPI void mav_gfxMaterialSet(MAV_material mat);
MAVAPI void mav_gfxMaterialUse(MAV_material mat);
MAVAPI void mav_gfxTextureSet(MAV_texture *tex, MAV_texEnvFn pTexEnv);
MAVAPI void mav_gfxTextureUse(MAV_texture tex, MAV_texEnvFn pTexEnv);
MAVAPI void mav_gfxLightingModelSet(MAV_lightingModel lm);
MAVAPI void mav_gfxLightingModelUse(MAV_lightingModel lm);
MAVAPI void mav_gfxLightSet(MAV_light l);
MAVAPI void mav_gfxLightUse(MAV_light l);
MAVAPI void mav_gfxLightPos(MAV_light l);

MAVAPI void mav_gfxBlendSet(int v);
MAVAPI void mav_gfxTextureEnv1Set(int v);
MAVAPI void mav_gfxTextureEnv2Set(int v);
MAVAPI void mav_gfxAccumSet(int mode, float val);

MAVAPI int  mav_gfxListsNew(int range);
MAVAPI void mav_gfxListNew(int list, int mode);
MAVAPI void mav_gfxListEnd(void);
MAVAPI void mav_gfxListExec(int list);
MAVAPI void mav_gfxListsExec(int n, int *lists);
MAVAPI void mav_gfxListsDelete(int list, int range);



/* Voodoo specific routines */

MAVAPI void mav_gfx3DfxModeSet(int fullscreen);
MAVAPI int  mav_gfx3DfxBoardSet(int bd);



/* Options */

MAVAPI extern int mav_opt_trackMatrix;
MAVAPI extern int mav_opt_texComps;



/* Info on graphics vendor */

MAVAPI extern char *mav_gfx_vendor;
MAVAPI extern char *mav_gfx_renderer;
MAVAPI extern char *mav_gfx_version;



/* Module initialise */

MAVAPI int mav_gfxModuleInit(void);
MAVAPI char *mav_gfxModuleID(void);

#ifdef __cplusplus
}
#endif
#endif
