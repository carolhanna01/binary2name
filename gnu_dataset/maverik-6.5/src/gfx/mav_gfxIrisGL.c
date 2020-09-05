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

#include <gl/gl.h>

float mavlib_bk[4];

int mav_opt_trackMatrix= MAV_FALSE;
int mav_opt_texComps= 4;

int mavlib_matrixmode= MAV_MODELVIEW;
int mavlib_voodoo= MAV_FALSE;

char *mav_gfx_vendor= NULL;
char *mav_gfx_renderer= NULL;
char *mav_gfx_version= NULL;



/* Option for tracking matrix transformations */

void mavlib_trackMatrix(void)
{
  mav_win_current->viewMat= mav_gfxMatrixGet();

  if (mav_opt_trackMatrix==MAV_PROJANDVIEW) mav_win_current->pdvMat= mav_matrixMult(mav_win_current->projMat, mav_win_current->viewMat);
}



/* These routines are no more than wrappers to IrisGL */

void mav_gfxClipPlaneSet(int id, MAV_clipPlane cp)
{
  float params[4];

  params[0]= cp.norm.x;
  params[1]= cp.norm.y;
  params[2]= cp.norm.z;
  params[3]= cp.d;

  clipplane(id, CP_DEFINE, params);
}

void mav_gfxClipPlanesSet(MAV_clipPlanes *cp)
{
  int index;

  for (index=0; index<cp->num; index ++) mav_gfxClipPlaneSet(index, cp->planes[index]);
}

void mav_gfxClipPlaneEnable(int id)
{
  float dummy_params[4];

  clipplane(id, CP_ON, dummy_params);
}

void mav_gfxClipPlaneDisable(int id)
{
  float dummy_params[4];

  clipplane(id, CP_OFF, dummy_params);
}

void mav_gfxBackgroundColourSet(float r, float g, float b)
{
  mavlib_bk[0]= r;
  mavlib_bk[1]= g;
  mavlib_bk[2]= b;
  mavlib_bk[3]= 0.0;
}

void mav_gfxClearC(void)
{
  c3f(mavlib_bk);
  clear();
}

void mav_gfxClearZ(void)
{
  zclear();
}

void mav_gfxClearA(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxClearA not implemented, ignoring.\n");
}

void mav_gfxClearCZ(void)
{
  c3f(mavlib_bk);
  clear();
  zclear();
}

void mav_gfxPolygonModeSet(int i)
{
  if (i==MAV_POLYGON_LINE)
  {
    polymode(PYM_LINE);
  }
  else
  {
    polymode(PYM_FILL);
  }
}

void mav_gfxMultiSampleSet(int i)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxMultiSampleSet not implemented, ignoring.\n");
}

void mav_gfxFinish(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxFinish not implemented, ignoring.\n");
}

void mav_gfxFlush(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxFlush option not implemented, ignoring.\n");
}

void mav_gfxMatrixPush(void)
{
  pushmatrix();
}

void mav_gfxMatrixPop(void)
{
  popmatrix();
  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

MAV_matrix mavlib_matrixTranspose(MAV_matrix mat)
{
  MAV_matrix rv;
  int i,j;
  
  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      rv.mat[i][j]= mat.mat[j][i];
    }
  }

  return rv;
}

void mav_gfxMatrixMult(MAV_matrix im)
{
  MAV_matrix m= mavlib_matrixTranspose(im);
#ifdef MAV_IRIX5
  multmatrix(m.mat);
#endif
#ifdef MAV_IRIX6
  multmatrix((const float (*)[4]) m.mat);
#endif
  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixMode(int mode)
{
  if (mode==MAV_PROJECTION) mmode(MPROJECTION);
  if (mode==MAV_MODELVIEW) mmode(MVIEWING);

  mavlib_matrixmode= mode;
}

MAV_matrix mav_gfxMatrixGet(void)
{
  MAV_matrix rv;

  getmatrix(rv.mat);

  return mavlib_matrixTranspose(rv);
}

void mav_gfxMatrixLoad(MAV_matrix imat)
{
  MAV_matrix mat= mavlib_matrixTranspose(imat);
#ifdef MAV_IRIX5
  loadmatrix(mat.mat);
#endif
#ifdef MAV_IRIX6
  loadmatrix((const float (*)[4]) mat.mat);
#endif
  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixTranslate(MAV_vector t)
{
  translate(t.x, t.y, t.z);
  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixScale(float x, float y, float z)
{
  scale(x, y, z);
  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxPerspectiveSet(float ncp, float fcp, float fov, float aspect)
{
  perspective(fov*10, aspect, ncp, fcp);
}

void mav_gfxOrthogonalSet(float left, float right, float top, float bottom, float nr, float fr)
{
  ortho(left, right, top, bottom, nr, fr);
}

void mav_gfxPolygonBegin(void)
{
  bgnpolygon();
}

void mav_gfxPolygonEnd(void)
{
  endpolygon();
}

void mav_gfxTrianglesBegin(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxTrianglesBegin not implemented, ignoring.\n");
}

void mav_gfxTrianglesEnd(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxTrianglesEnd not implemented, ignoring.\n");
}

void mav_gfxVertex(MAV_vector v)
{
  v3f((float *) &v);
}

void mav_gfxNormal(MAV_vector v)
{
  n3f((float *) &v);
}

void mav_gfxLightSet(MAV_light info)
{
  float ldef[9];

  ldef[0]= AMBIENT;
  ldef[1]= info.ambient[0];
  ldef[2]= info.ambient[1];
  ldef[3]= info.ambient[2];
  ldef[4]= LCOLOR;
  ldef[5]= info.diffuse[0];
  ldef[6]= info.diffuse[1];
  ldef[7]= info.diffuse[2];
  ldef[8]= LMNULL;

  lmdef(DEFLIGHT, info.id+1, 0, ldef);
}

void mav_gfxLightUse(MAV_light info)
{
  int whichlight=-1;

  if (info.index==0) whichlight=LIGHT0;
  if (info.index==1) whichlight=LIGHT1;
  if (info.index==2) whichlight=LIGHT2;
  if (info.index==3) whichlight=LIGHT3;
  if (info.index==4) whichlight=LIGHT4;

  lmbind(whichlight, 0);

  if (info.defined) lmbind(whichlight, info.id+1);
}

void mav_gfxLightPos(MAV_light info)
{
  if (info.defined) {
    float ldef[6];
    
    ldef[0]= POSITION;
    ldef[1]= info.pos.x;
    ldef[2]= info.pos.y;
    ldef[3]= info.pos.z;
    ldef[4]= 1.0;
    ldef[5]= LMNULL;

    lmdef(DEFLIGHT, info.id+1, 0, ldef);
  }
}

void mav_gfxTextureEnv1Set(int v)
{
  if (v!=1) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxTextureEnv1Set not implemented, ignoring.\n");
  }
}

void mav_gfxTextureEnv2Set(int v)
{
  if (v!=1) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxTextureEnv2Set not implemented, ignoring.\n");
  }
}

void mav_gfxColouringModeUse(MAV_palette *p, int mode)
{
  if (mode==MAV_COLOUR) 
  {
    lmbind(LMODEL, 0);
    tevbind(TV_ENV0, 0);
  } 
  else if (mode==MAV_MATERIAL) 
  {
    lmbind(LMODEL, p->lm.id+1);
    tevbind(TV_ENV0, 0);
  }
  else if (mode==MAV_TEXTURE) 
  {
    lmbind(LMODEL, 0);
    tevbind(TV_ENV0, 2);
  }
  else if (mode==MAV_BLENDED_TEXTURE) 
  {
    lmbind(LMODEL, p->lm.id+1);
    tevbind(TV_ENV0, 1);
  }
  else if (mode==MAV_LIT_TEXTURE) 
  {
    lmbind(LMODEL, p->lm.id+1);
    tevbind(TV_ENV0, 2);
  }
}

int mav_gfxVisualInfoGet(int *r, int *g, int *b, int *a, int *d, int *db, int *ar, int *ag, int *ab, int *aa, int *sb, int *msb)
{
  *r=getgconfig(GC_BITS_RED);
  *g=getgconfig(GC_BITS_GREEN);
  *b=getgconfig(GC_BITS_BLUE); 
  *a=0;
  *d= abs(getgconfig(GC_BITS_ZBUFFER));
  *db= getgconfig(GC_DOUBLE);
  *ar=0;
  *ag=0;
  *ab=0;
  *aa=0;
  *sb=0;
  *msb=0;

  return 1;
}

void mav_gfxStripQBegin(void)
{
  bgnqstrip();
}

void mav_gfxStripQEnd(void)
{
  endqstrip();
}

void mav_gfxStripTBegin(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxStripTBegin not implemented, ignoring.\n");
}

void mav_gfxStripTEnd(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxStripTEnd not implemented, ignoring.\n");
}

void mav_gfxTexCoord(MAV_texCoord t)
{
  t2f((float *) &t);
}

void mav_gfxLineClosedBegin(void)
{
  bgnclosedline();
}

void mav_gfxLineClosedEnd(void)
{
  endclosedline();
}

void mav_gfxLineBegin(void)
{
  bgnline();
}

void mav_gfxLineEnd(void)
{
  endline();
}

void mav_gfxMeshTBegin(void)
{
  bgntmesh();
}

void mav_gfxMeshTEnd(void)
{
  endtmesh();
}

void mav_gfxColourSet(MAV_colour col)
{
}

void mav_gfxColourUse(MAV_colour col)
{
  c4f(col.colour);
}

void mav_gfxMaterialSet(MAV_material mat)
{
  float mat_def[21];

  mat_def[0] = AMBIENT;
  mat_def[1] = mat.ambient[0];
  mat_def[2] = mat.ambient[1];
  mat_def[3] = mat.ambient[2];
  mat_def[4] = DIFFUSE;
  mat_def[5] = mat.diffuse[0];
  mat_def[6] = mat.diffuse[1];
  mat_def[7] = mat.diffuse[2];
  mat_def[8] = SPECULAR;
  mat_def[9] = mat.specular[0];
  mat_def[10] = mat.specular[1];
  mat_def[11] = mat.specular[2];
  mat_def[12] = EMISSION;
  mat_def[13] = mat.emission[0];
  mat_def[14] = mat.emission[1];
  mat_def[15] = mat.emission[2];
  mat_def[16] = ALPHA;
  mat_def[17] = mat.diffuse[3];
  mat_def[18] = SHININESS;
  mat_def[19] = mat.shine;
  mat_def[20] = LMNULL;

  lmdef(DEFMATERIAL, mat.id+1, 0, mat_def);
}

void mav_gfxMaterialUse(MAV_material mat)
{
  lmbind(MATERIAL, mat.id+1);
}

#define L2 0.301029995

void mav_gfxTextureSet(MAV_texture *tex, MAV_texEnvFn pTexEnv)
{
  float texprops[]= {TX_MINFILTER, TX_BILINEAR, TX_NULL, TX_MAGFILTER, TX_BILINEAR, TX_NULL, TX_WRAP, TX_REPEAT, TX_NULL};
  texdef2d(tex->id+1, 4, tex->width, tex->height, tex->mem, 0, texprops);
}

void mav_gfxTextureUse(MAV_texture tex, MAV_texEnvFn pTexEnv)
{
  texbind(TX_TEXTURE_0, tex.id+1);

  /* call correct texture environment function */
  if (tex.texEnv) 
  {
    (*(tex.texEnv))(&tex);
  }
  else
  {
    if (pTexEnv) (*pTexEnv)(&tex);
  }
}

void mav_gfxLightingModelSet(MAV_lightingModel info)
{
  float lm_def[7];

  lm_def[0]= AMBIENT;
  lm_def[1]= info.ambient[0];
  lm_def[2]= info.ambient[1];
  lm_def[3]= info.ambient[2];
  lm_def[4]= LOCALVIEWER;
  lm_def[5]= info.localviewer;
  lm_def[6]= LMNULL;

  lmdef(DEFLMODEL, info.id+1, 0, lm_def);
}

void mav_gfxLightingModelUse(MAV_lightingModel info)
{
  lmbind(LMODEL, 0);

  if (info.defined) {
    lmbind(LMODEL, info.id+1);
  }
}

void mav_gfxBufferReadSet(int buf)
{
  if (buf==MAV_FRONT)
  {
    readsource(SRC_FRONT);
  }
  else
  {
    readsource(SRC_BACK);
  }
}

void mav_gfxPixelRead(int x, int y, int width, int height, unsigned long *mem)
{
  lrectread(x, y, width+x-1, height+y-1, mem);
}

void mav_gfxPixelReadUByte(int x, int y, int width, int height, unsigned char *mem)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxPixelReadUByte not implemented, ignoring.\n");
}

void mav_gfxPixelDraw(int w, int h, float *v)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxPixelDraw not implemented, ignoring.\n");
}

void mav_gfxNormalizeSet(int i)
{
  if (i) 
  {
    nmode(NAUTO);
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxNormalizeSet not implemented, ignoring.\n");
  }
}

void mav_gfxBackfaceCullSet(int i)
{
  if (i) 
  {
    backface(TRUE);
  }
  else
  {
    backface(FALSE);
  }
}

int mav_gfxBackfaceCullGet()
{
  int rv= MAV_FALSE;
  
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxBackfaceCullGet not implemented, ignoring.\n");
  
  return (int) rv;
}

void mav_gfxDepthTestSet(int i)
{
  if (i) 
  {
    zbuffer(TRUE);
  }
  else
  {
    zbuffer(FALSE);
  }
}

void mav_gfxDepthMaskSet(int i)
{
  if (i) 
  {
    zwritemask(0xffffffff);
  }
  else
  {
    zwritemask(0);
  }
}

void mav_gfxViewPortSet(int x, int y, int width, int height)
{
  viewport(x,x+width-1,y,y+height-1);
}

void mav_gfxRasterPosSet(MAV_vector v)
{
  cmov2(v.x,v.y);
}

void mav_gfxRasterPos2DSet(float x, float y)
{
  cmov2(x,y);
}

void mav_gfxLineWidthSet(float lw)
{
  linewidthf(lw);
}

float mav_gfxLineWidthGet(void)
{
  float rv=0;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxLineWidthGet not implemented, ignoring.\n");

  return rv;
}

void mav_gfxLineStippleSet(int factor, unsigned short pattern)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxLineStippleSet not implemented, ignoring.\n");
}

void mav_gfxFogSet(int type, float data1, float data2, float r, float g, float b)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxFogSet not implemented, ignoring.\n");
}

void mav_gfxBlendSet(int v)
{
  switch (v) {
  case MAV_BLEND_OFF:
    blendfunction(BF_ONE, BF_ZERO);
    break;

  case MAV_BLEND_1:
    blendfunction(BF_SA, BF_MSA);
    break;
  }    
}

void mav_gfxAccumSet(int mode, float val)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxAccumSet not implemented, ignoring.\n");
}

int mav_gfxListsNew(int range)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxListsNew not implemented, ignoring.\n");

  return -1;
}

void mav_gfxListNew(int list, int mode)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxListNew not implemented, ignoring.\n");
}

void mav_gfxListEnd(void)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxListEnd not implemented, ignoring.\n");
}

void mav_gfxListExec(int list)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxListExec not implemented, ignoring.\n");
}

void mav_gfxListsExec(int n, int *lists)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxListsExec not implemented, ignoring.\n");
}

void mav_gfxListsDelete(int list, int range)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_gfxListsDelete not implemented, ignoring.\n");
}
