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


#include "mavlib_gfx.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int mav_opt_trackMatrix= MAV_FALSE;
int mav_opt_texComps= 4;

int mavlib_matrixmode= MAV_MODELVIEW;
int mavlib_voodoo= MAV_FALSE;

char *mav_gfx_vendor= NULL;
char *mav_gfx_renderer= NULL;
char *mav_gfx_version= NULL;



/* These routines are no more than empty functions */

void mav_gfxClipPlaneSet(int id, MAV_clipPlane cp)
{
}

void mav_gfxClipPlanesSet(MAV_clipPlanes *cp)
{
}

void mav_gfxClipPlaneEnable(int id)
{
}

void mav_gfxClipPlaneDisable(int id)
{
}

void mav_gfxBackgroundColourSet(float r, float g, float b)
{
}

void mav_gfxClearC(void)
{
}

void mav_gfxClearZ(void)
{
}

void mav_gfxClearA(void)
{
}

void mav_gfxClearCZ(void)
{
}

void mav_gfxPolygonModeSet(int i)
{
}

void mav_gfxMultiSampleSet(int i)
{
}

void mav_gfxFinish(void)
{
}

void mav_gfxFlush(void)
{
}

void mav_gfxMatrixPush(void)
{
}

void mav_gfxMatrixPop(void)
{
}

void mav_gfxMatrixMult(MAV_matrix im)
{
}

void mav_gfxMatrixMode(int mode)
{
}

MAV_matrix mav_gfxMatrixGet(void)
{
  MAV_matrix rv;

  rv= MAV_ID_MATRIX;

  return rv;
}

void mav_gfxMatrixLoad(MAV_matrix imat)
{
}

void mav_gfxMatrixTranslate(MAV_vector t)
{
}

void mav_gfxMatrixScale(float x, float y, float z)
{
}

void mav_gfxPerspectiveSet(float ncp, float fcp, float fov, float aspect)
{
}

void mav_gfxOrthogonalSet(float left, float right, float top, float bottom, float nr, float fr)
{
}

void mav_gfxPolygonBegin(void)
{
}

void mav_gfxPolygonEnd(void)
{
}

void mav_gfxTrianglesBegin(void)
{
}

void mav_gfxTrianglesEnd(void)
{
}

void mav_gfxVertex(MAV_vector v)
{
}

void mav_gfxNormal(MAV_vector v)
{
}

void mav_gfxLightSet(MAV_light info)
{
}

void mav_gfxLightUse(MAV_light info)
{
}

void mav_gfxLightPos(MAV_light info)
{
}

void mav_gfxTextureEnv1Set(int v)
{
}

void mav_gfxTextureEnv2Set(int v)
{
}

void mav_gfxColouringModeUse(MAV_palette *p, int mode)
{
}

int mav_gfxVisualInfoGet(int *r, int *g, int *b, int *a, int *d, int *db, int *ar, int *ag, int *ab, int *aa, int *sb, int *msb)
{
  return 0;
}

void mav_gfxStripQBegin(void)
{
}

void mav_gfxStripQEnd(void)
{
}

void mav_gfxStripTBegin(void)
{
}

void mav_gfxStripTEnd(void)
{
}

void mav_gfxTexCoord(MAV_texCoord t)
{
}

void mav_gfxLineClosedBegin(void)
{
}

void mav_gfxLineClosedEnd(void)
{
}

void mav_gfxLineBegin(void)
{
}

void mav_gfxLineEnd(void)
{
}

void mav_gfxMeshTBegin(void)
{
}

void mav_gfxMeshTEnd(void)
{
}

void mav_gfxColourSet(MAV_colour col)
{
}

void mav_gfxColourUse(MAV_colour col)
{
}

void mav_gfxMaterialSet(MAV_material mat)
{
}

void mav_gfxMaterialUse(MAV_material mat)
{
}

void mav_gfxTextureSet(MAV_texture *tex, MAV_texEnvFn pTexEnv)
{
}


void mav_gfxTextureUse(MAV_texture tex, MAV_texEnvFn pTexEnv)
{
}

void mav_gfxLightingModelSet(MAV_lightingModel info)
{
}

void mav_gfxLightingModelUse(MAV_lightingModel info)
{
}

void mav_gfxBufferReadSet(int buf)
{
}

void mav_gfxPixelRead(int x, int y, int width, int height, uint32_t *mem)
{
}

void mav_gfxPixelReadUByte(int x, int y, int width, int height, unsigned char *mem)
{
}

void mav_gfxPixelDraw(int w, int h, float *v)
{
}

void mav_gfxNormalizeSet(int i)
{
}

void mav_gfxBackfaceCullSet(int i)
{
}

int mav_gfxBackfaceCullGet()
{
  int rv= MAV_FALSE;
  
  return (int) rv;
}

void mav_gfxDepthTestSet(int i)
{
}

void mav_gfxDepthMaskSet(int i)
{
}

void mav_gfxViewPortSet(int x, int y, int width, int height)
{
}

void mav_gfxRasterPosSet(MAV_vector v)
{
}

void mav_gfxRasterPos2DSet(float x, float y)
{
}

void mav_gfxLineWidthSet(float lw)
{
}

float mav_gfxLineWidthGet(void)
{
  float rv=0;

  return rv;
}

void mav_gfxLineStippleSet(int factor, unsigned short pattern)
{
}

void mav_gfxBlendSet(int v)
{
}

void mav_gfxAccumSet(int mode, float val)
{
}

int mav_gfxListsNew(int range)
{
  return -1;
}

void mav_gfxListNew(int list, int mode)
{
}

void mav_gfxListEnd(void)
{
}

void mav_gfxListExec(int list)
{
}

void mav_gfxListsExec(int n, int *lists)
{
}

void mav_gfxListsDelete(int list, int range)
{
}
