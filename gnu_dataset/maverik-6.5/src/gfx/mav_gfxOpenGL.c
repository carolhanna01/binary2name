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

#ifdef WIN32
#include <windows.h>
#endif
#include "GL/glu.h"
#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
GLuint *mavlib_bindTextureIndex;
#endif

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



/* These routines are no more than wrappers to OpenGL */

void mav_gfxClipPlaneSet(int id, MAV_clipPlane cp)
{
  GLdouble params[4];

  params[0]= cp.norm.x;
  params[1]= cp.norm.y;
  params[2]= cp.norm.z;
  params[3]= cp.d;

  glClipPlane(GL_CLIP_PLANE0 + id, params);
}

void mav_gfxClipPlanesSet(MAV_clipPlanes *cp)
{
  int i;

  for (i=0; i<cp->num; i++) mav_gfxClipPlaneSet(i, cp->planes[i]);
}

void mav_gfxClipPlaneEnable(int id)
{
  glEnable(GL_CLIP_PLANE0 + id);
}

void mav_gfxClipPlaneDisable(int id)
{
  glDisable(GL_CLIP_PLANE0 + id);
}

void mav_gfxBackgroundColourSet(float r, float g, float b)
{
  glClearColor(r,g,b,0.0);
  glClearAccum(r,g,b,0.0);
}

void mav_gfxClearC(void)
{
  glClear(GL_COLOR_BUFFER_BIT);
}

void mav_gfxClearZ(void)
{
  glClear(GL_DEPTH_BUFFER_BIT);
}

void mav_gfxClearA(void)
{
  glClear(GL_ACCUM_BUFFER_BIT);
}

void mav_gfxClearCZ(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void mav_gfxPolygonModeSet(int i)
{
  if (i==MAV_POLYGON_LINE)
  {
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  }
  else
  {
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  }
}

void mav_gfxMultiSampleSet(int i)
{
#if defined(GL_SGIS_multisample) && defined(MAV_IRIX6)
  if (mav_opt_multiSample) 
  {
    if (i==MAV_TRUE)
    {
      glEnable(GL_MULTISAMPLE_SGIS);
    }
    else
    {
      glDisable(GL_MULTISAMPLE_SGIS);
    }
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Multisample option not selected, ignoring.\n");
  }
#else
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Multisample option not available, ignoring.\n");
#endif
}

void mav_gfxFinish(void)
{
  glFinish();
}

void mav_gfxFlush(void)
{
  glFlush();
}

void mav_gfxMatrixPush(void)
{
  glPushMatrix();
}

void mav_gfxMatrixPop(void)
{
  glPopMatrix();
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

  glMultMatrixf((float *) m.mat);

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixMode(int mode)
{
  if (mode==MAV_PROJECTION) glMatrixMode(GL_PROJECTION);
  if (mode==MAV_MODELVIEW) glMatrixMode(GL_MODELVIEW);  

  mavlib_matrixmode= mode;
}

MAV_matrix mav_gfxMatrixGet(void)
{
  MAV_matrix rv;

  if (mavlib_matrixmode==MAV_PROJECTION) glGetFloatv(GL_PROJECTION_MATRIX, (float *) rv.mat);
  if (mavlib_matrixmode==MAV_MODELVIEW) glGetFloatv(GL_MODELVIEW_MATRIX, (float *) rv.mat);

  return mavlib_matrixTranspose(rv);
}

void mav_gfxMatrixLoad(MAV_matrix imat)
{
  MAV_matrix mat= mavlib_matrixTranspose(imat);

  glLoadMatrixf((float *) mat.mat);

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixTranslate(MAV_vector t)
{
  glTranslatef(t.x, t.y, t.z);

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixScale(float x, float y, float z)
{
  glScalef(x, y, z);

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxPerspectiveSet(float ncp, float fcp, float fov, float aspect)
{
  gluPerspective(fov, aspect, ncp, fcp);
}

void mav_gfxOrthogonalSet(float left, float right, float top, float bottom, float nr, float fr)
{
  glOrtho(left, right, top, bottom, nr, fr);
}

void mav_gfxPolygonBegin(void)
{
  glBegin(GL_POLYGON);
}

void mav_gfxPolygonEnd(void)
{
  glEnd();
}

void mav_gfxTrianglesBegin(void)
{
  glBegin(GL_TRIANGLES);
}

void mav_gfxTrianglesEnd(void)
{
  glEnd();
}

void mav_gfxVertex(MAV_vector v)
{
  glVertex3fv((float *) &v);
}

void mav_gfxNormal(MAV_vector v)
{
  glNormal3fv((float *) &v);
}

void mav_gfxLightSet(MAV_light info)
{
}

void mav_gfxLightUse(MAV_light info)
{
  int whichlight=-1;

  if (info.index==0) whichlight=GL_LIGHT0;
  if (info.index==1) whichlight=GL_LIGHT1;
  if (info.index==2) whichlight=GL_LIGHT2;
  if (info.index==3) whichlight=GL_LIGHT3;
  if (info.index==4) whichlight=GL_LIGHT4;

  glDisable(whichlight);

  if (info.defined) {
    glEnable(whichlight);
    glLightfv(whichlight, GL_AMBIENT, info.ambient);
    glLightfv(whichlight, GL_DIFFUSE, info.diffuse);
    glLightfv(whichlight, GL_SPECULAR, info.specular);
  }
}

void mav_gfxLightPos(MAV_light info)
{
  int whichlight=-1;
     
  if (info.index == 0) whichlight=GL_LIGHT0;
  if (info.index == 1) whichlight=GL_LIGHT1;
  if (info.index == 2) whichlight=GL_LIGHT2;
  if (info.index == 3) whichlight=GL_LIGHT3;
  if (info.index == 4) whichlight=GL_LIGHT4;

  if (info.defined) {
    float pv[4];

    pv[0]=info.pos.x;
    pv[1]=info.pos.y;
    pv[2]=info.pos.z;
    pv[3]=1.0;
    
    if (info.positioning==MAV_LIGHT_RELATIVE) 
    {
      /* Position is relative to eye */
      glPushMatrix(); 
      glLoadIdentity();
      glLightfv(whichlight, GL_POSITION, pv);
      glPopMatrix();
    }
    else
    {
      /* Position is in world coords */
      glLightfv(whichlight, GL_POSITION, pv);
    }
  }
}

void mav_gfxTextureEnv1Set(int v)
{
  switch (v) {
    case 1:
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      break;
    case 2:
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      break;
    case 3:
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      break;
    case 4:
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      break;
  }
}

void mav_gfxTextureEnv2Set(int v)
{
  switch (v) {
    case 1:
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      break;
    case 2:
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
      break;
  }
}

void mav_gfxColouringModeUse(MAV_palette *p, int mode)
{
  if (mode==MAV_COLOUR) 
  {
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
  } 
  else if (mode==MAV_MATERIAL) 
  {
    glEnable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
  }
  else if (mode==MAV_TEXTURE) 
  {
    glDisable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
  }
  else if (mode==MAV_BLENDED_TEXTURE) 
  {
    glEnable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
  }
  else if (mode==MAV_LIT_TEXTURE) 
  {
    glEnable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
  }
}

int mav_gfxVisualInfoGet(int *r, int *g, int *b, int *a, int *d, int *db, int *ar, int *ag, int *ab, int *aa, int *sb, int *msb)
{
  glGetIntegerv(GL_RED_BITS, (GLint *) r);
  glGetIntegerv(GL_GREEN_BITS, (GLint *) g);
  glGetIntegerv(GL_BLUE_BITS, (GLint *) b);
  glGetIntegerv(GL_ALPHA_BITS, (GLint *) a);
  glGetIntegerv(GL_DEPTH_BITS, (GLint *) d);
  glGetIntegerv(GL_DOUBLEBUFFER, (GLint *) db);
  glGetIntegerv(GL_ACCUM_RED_BITS, (GLint *) ar);
  glGetIntegerv(GL_ACCUM_GREEN_BITS, (GLint *) ag);
  glGetIntegerv(GL_ACCUM_BLUE_BITS, (GLint *) ab);
  glGetIntegerv(GL_ACCUM_ALPHA_BITS, (GLint *) aa);
  glGetIntegerv(GL_STENCIL_BITS, (GLint *) sb);
#if defined(GL_SGIS_multisample) && defined(MAV_IRIX6)
  glGetIntegerv(GL_SAMPLES_SGIS, (GLint *) msb);
#else
  *msb=0;
#endif

  return 1;
}

void mav_gfxStripQBegin(void)
{
  glBegin(GL_QUAD_STRIP);
}

void mav_gfxStripQEnd(void)
{
  glEnd();
}

void mav_gfxStripTBegin(void)
{
  glBegin(GL_TRIANGLE_STRIP);
}

void mav_gfxStripTEnd(void)
{
  glEnd();
}

void mav_gfxTexCoord(MAV_texCoord t)
{
  glTexCoord2fv((float *) &t);
}

void mav_gfxLineClosedBegin(void)
{
  glBegin(GL_LINE_LOOP);
}

void mav_gfxLineClosedEnd(void)
{
  glEnd();
}

void mav_gfxLineBegin(void)
{
  glBegin(GL_LINE_STRIP);
}

void mav_gfxLineEnd(void)
{
  glEnd();
}

void mav_gfxMeshTBegin(void)
{
  glBegin(GL_TRIANGLE_STRIP);
}

void mav_gfxMeshTEnd(void)
{
  glEnd();
}

void mav_gfxColourSet(MAV_colour col)
{
}

void mav_gfxColourUse(MAV_colour col)
{
  glColor4fv(col.colour);
}

void mav_gfxMaterialSet(MAV_material mat)
{
}

void mav_gfxMaterialUse(MAV_material mat)
{
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat.ambient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat.diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat.specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat.emission);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, mat.shine);
}

#define L2 0.301029995

void mav_gfxTextureSet(MAV_texture *tex, MAV_texEnvFn pTexEnv)
{
  if (tex->mipmapped) { /* build mipmaps if applicable */
    int width, height, i;

    width= tex->width;
    height= tex->height;

    if (width>height)
    {
      tex->nmaps= log10(width)/L2;
    }
    else
    {
      tex->nmaps= log10(height)/L2;
    }
    
    if (tex->nmaps!=0) {
      tex->xsize= (int *) mav_malloc(tex->nmaps*sizeof(int));
      tex->ysize= (int *) mav_malloc(tex->nmaps*sizeof(int));
      /*tex->mipmap= (unsigned long **) mav_malloc(tex->nmaps*sizeof(unsigned long *));*/
	  tex->mipmap= (uint32_t **) mav_malloc(tex->nmaps*sizeof(uint32_t *));
    }

    for (i=0; i<tex->nmaps; i++) {
      if (width>=2) width/=2;
      if (height>=2) height/=2;

      tex->xsize[i]= width;
      tex->ysize[i]= height;
      /*tex->mipmap[i]= (unsigned long *) mav_malloc(width*height*sizeof(unsigned long));*/
	  tex->mipmap[i]= (uint32_t *) mav_malloc(width*height*sizeof(uint32_t));
#ifdef WIN32
      gluScaleImage(GL_RGBA, tex->width, tex->height, GL_UNSIGNED_BYTE, tex->mem, width, height, GL_UNSIGNED_BYTE, tex->mipmap[i]);
#else
#ifdef MAV_LINUX
      gluScaleImage(GL_RGBA, tex->width, tex->height, GL_UNSIGNED_BYTE, tex->mem, width, height, GL_UNSIGNED_BYTE, tex->mipmap[i]); /* ABGR_EXT doesnt work for some reason */
#else
#ifdef GL_EXT_abgr
      gluScaleImage(GL_ABGR_EXT, tex->width, tex->height, GL_UNSIGNED_BYTE, tex->mem, width, height, GL_UNSIGNED_BYTE, tex->mipmap[i]);
#else
      fprintf(stderr, "Warning: no ABGR texture extension, ignoring.\n");
#endif
#endif
#endif
    }
  }

#ifndef MAV_SUNOS4
#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
  if (mav_opt_bindTextures) {
#ifdef GL_VERSION_1_1
    glBindTexture(GL_TEXTURE_2D, mavlib_bindTextureIndex[tex->id+1]);
#else
    glBindTextureEXT(GL_TEXTURE_2D, mavlib_bindTextureIndex[tex->id+1]);
#endif
    glEnable(GL_TEXTURE_2D);

    mav_opt_bindTextures=MAV_FALSE;
    mav_gfxTextureUse(*tex, pTexEnv);
    mav_opt_bindTextures=MAV_TRUE;

    mav_surfaceParamsUndefine();
  }
#endif
#endif
}


void mav_gfxTextureUse(MAV_texture tex, MAV_texEnvFn pTexEnv)
{
#ifndef MAV_SUNOS4
#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
  if (mav_opt_bindTextures) 
  {
#ifdef GL_VERSION_1_1
    glBindTexture(GL_TEXTURE_2D, mavlib_bindTextureIndex[tex.id+1]);
#else
    glBindTextureEXT(GL_TEXTURE_2D, mavlib_bindTextureIndex[tex.id+1]);
#endif
  }
  else
#endif
#endif
  {
    int i;
#ifdef WIN32
    glTexImage2D(GL_TEXTURE_2D, 0, mav_opt_texComps, tex.width, tex.height, 0, GL_RGBA, GL_UNSIGNED_BYTE, tex.mem);
    if (tex.mipmapped) for (i=0; i<tex.nmaps; i++) glTexImage2D(GL_TEXTURE_2D, i+1, mav_opt_texComps, tex.xsize[i], tex.ysize[i], 0, GL_RGBA, GL_UNSIGNED_BYTE, tex.mipmap[i]);
#else
#ifdef GL_EXT_abgr
    glTexImage2D(GL_TEXTURE_2D, 0, mav_opt_texComps, tex.width, tex.height, 0, GL_ABGR_EXT, GL_UNSIGNED_BYTE, tex.mem);
    if (tex.mipmapped) for (i=0; i<tex.nmaps; i++) glTexImage2D(GL_TEXTURE_2D, i+1, mav_opt_texComps, tex.xsize[i], tex.ysize[i], 0, GL_ABGR_EXT, GL_UNSIGNED_BYTE, tex.mipmap[i]);
#else
    fprintf(stderr, "Warning: no ABGR texture extension, ignoring.\n");
#endif
#endif
  }

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
}

void mav_gfxLightingModelUse(MAV_lightingModel info)
{
  glDisable(GL_LIGHTING);

  if (info.defined) {
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, info.ambient);
    glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, info.localviewer);
    glEnable(GL_LIGHTING);
  }
}

void mav_gfxBufferReadSet(int buf)
{
  if (buf==MAV_FRONT)
  {
    glReadBuffer(GL_FRONT);
  }
  else
  {
    glReadBuffer(GL_BACK);
  }
}

/*void mav_gfxPixelRead(int x, int y, int width, int height, unsigned long *mem)*/
void mav_gfxPixelRead(int x, int y, int width, int height, uint32_t *mem)
{
  float *fmem= mav_malloc(width*height*3*sizeof(float));
  int i,j=0;

  glReadPixels(x, y, width, height, GL_RGB, GL_FLOAT, fmem);

  for (i=0; i<width*height; i++) {
#ifdef MAV_LINUX
    mem[i]= (((int) (fmem[j]*255.0))<<24) + (((int) (fmem[j+1]*255.0))<<16) + (((int) (fmem[j+2]*255.0))<<8) + 255;
#else
    mem[i]= (255<<24) + (((int) (fmem[j+2]*255.0))<<16) + (((int) (fmem[j+1]*255.0))<<8) + ((int) (fmem[j]*255.0));
#endif
    j+=3;
  }

  mav_free(fmem);
}

void mav_gfxPixelReadUByte(int x, int y, int width, int height, unsigned char *mem)
{
  glReadPixels(x, y, width, height, GL_RGB, GL_UNSIGNED_BYTE, mem);
}

void mav_gfxPixelDraw(int w, int h, float *v)
{
  glDrawPixels(w, h, GL_RGB, GL_FLOAT, v);
}

void mav_gfxNormalizeSet(int i)
{
  if (i) 
  {
    glEnable(GL_NORMALIZE);
  }
  else
  {
    glDisable(GL_NORMALIZE);
  }
}

void mav_gfxBackfaceCullSet(int i)
{
  if (i) 
  {
    glEnable(GL_CULL_FACE);
  }
  else
  {
    glDisable(GL_CULL_FACE);
  }
}

int mav_gfxBackfaceCullGet()
{
  GLint rv= MAV_FALSE;

  glGetIntegerv(GL_CULL_FACE, &rv);
  
  return (int) rv;
}

void mav_gfxDepthTestSet(int i)
{
  if (i) 
  {
    glEnable(GL_DEPTH_TEST);
  }
  else
  {
    glDisable(GL_DEPTH_TEST);
  }
}

void mav_gfxDepthMaskSet(int i)
{
  if (i)
  {
    glDepthMask(1);
  }
  else
  {
    glDepthMask(0);
  }
}

void mav_gfxViewPortSet(int x, int y, int width, int height)
{
  glViewport(x,y,width,height);
}

void mav_gfxRasterPosSet(MAV_vector v)
{
  glRasterPos3f(v.x,v.y,v.z);
}

void mav_gfxRasterPos2DSet(float x, float y)
{
  glRasterPos2f(x,y);
}

void mav_gfxLineWidthSet(float lw)
{
  glLineWidth(lw);
}

float mav_gfxLineWidthGet(void)
{
  float rv=0;

  glGetFloatv(GL_LINE_WIDTH, &rv);

  return rv;
}

void mav_gfxLineStippleSet(int factor, unsigned short pattern)
{
  if (factor>0)
  {
    glLineStipple(factor, pattern);
    glEnable(GL_LINE_STIPPLE);
  }
  else
  {
    glDisable(GL_LINE_STIPPLE);
  }
}

void mav_gfxFogSet(int type, float data1, float data2, float r, float g, float b)
{
  float col[4];
  
  col[0]=r;
  col[1]=g;
  col[2]=b;
  col[3]=0;

  if (type==MAV_FOG_EXP2)
  {
    glEnable(GL_FOG);
    glFogi(GL_FOG_MODE, GL_EXP2);
    glFogf(GL_FOG_DENSITY, data1);
    glFogfv(GL_FOG_COLOR, col);
  }
  else if (type==MAV_FOG_EXP)
  {
    glEnable(GL_FOG);
    glFogi(GL_FOG_MODE, GL_EXP);
    glFogf(GL_FOG_DENSITY, data1);
    glFogfv(GL_FOG_COLOR, col);
  }
  else if (type==MAV_FOG_LINEAR)
  {
    glEnable(GL_FOG);
    glFogi(GL_FOG_MODE, GL_LINEAR);
    glFogf(GL_FOG_START, data1);
    glFogf(GL_FOG_END, data2);
    glFogfv(GL_FOG_COLOR, col);
  }
  else if (type==MAV_FOG_NONE)
  {
    glDisable(GL_FOG);
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Bad value when setting fog, ignoring.\n");
  }
}

void mav_gfxBlendSet(int v)
{
  switch (v) {
  case MAV_BLEND_OFF:
    glDisable(GL_BLEND);
    break;

  case MAV_BLEND_1:
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    break;
  }    
}

void mav_gfxAccumSet(int mode, float val)
{
  switch (mode) {
  case MAV_ACCUM_ACCUM:
    glAccum(GL_ACCUM, val);
    break;

  case MAV_ACCUM_LOAD:
    glAccum(GL_LOAD, val);
    break;

  case MAV_ACCUM_RETURN:
    glAccum(GL_RETURN, val);
    break;

  case MAV_ACCUM_ADD:
    glAccum(GL_ADD, val);
    break;

  case MAV_ACCUM_MULT:
    glAccum(GL_MULT, val);
    break;
  }
}

int mav_gfxListsNew(int range)
{
  return glGenLists(range);
}

void mav_gfxListNew(int list, int mode)
{
  switch (mode) {
  case MAV_DLISTS_COMPILE:
    glNewList(list, GL_COMPILE);
    break;

  case MAV_DLISTS_COMPILE_AND_EXECUTE:
    glNewList(list, GL_COMPILE_AND_EXECUTE);
    break;
  }
}

void mav_gfxListEnd(void)
{
  glEndList();
}

void mav_gfxListExec(int list)
{
  glCallList(list);
  mav_surfaceParamsUndefine();
}

void mav_gfxListsExec(int n, int *lists)
{
  glCallLists(n, GL_INT, lists);
  mav_surfaceParamsUndefine();
}

void mav_gfxListsDelete(int list, int range)
{
  glDeleteLists(list, range);
}
