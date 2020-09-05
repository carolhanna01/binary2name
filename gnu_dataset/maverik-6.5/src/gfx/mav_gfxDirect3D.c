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


#include "maverik.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <windows.h>
#include <d3d8.h>

#include "mavlib_d3d.h"

/* define some custom vertex types */

#define VERT_ARRAY_SIZE 10000

#define MAVLIB_PN 0
#define MAVLIB_PNT 1
#define MAVLIB_PC 2
#define MAVLIB_PT 3
#define MAVLIB_PCT 4

/* position/normal */
typedef struct
{
  float x;
  float y;
  float z;
  float nx;
  float ny;
  float nz;
} MAVLIB_PNVERTEX;

#define MAVLIB_PNVERTEXFLAGS (D3DFVF_XYZ|D3DFVF_NORMAL)

/* position/normal/texture */
typedef struct
{
  float x;
  float y;
  float z;
  float nx;
  float ny;
  float nz;
  float tu;
  float tv;
} MAVLIB_PNTVERTEX;

#define MAVLIB_PNTVERTEXFLAGS (D3DFVF_XYZ|D3DFVF_NORMAL|D3DFVF_TEX1)

/* position/colour */
typedef struct
{
  float x;
  float y;
  float z;
  DWORD color;
} MAVLIB_PCVERTEX;

#define MAVLIB_PCVERTEXFLAGS (D3DFVF_XYZ|D3DFVF_DIFFUSE)

/* position/texture */
typedef struct
{
  float x;
  float y;
  float z;
  float tu;
  float tv;
} MAVLIB_PTVERTEX;

#define MAVLIB_PTVERTEXFLAGS (D3DFVF_XYZ|D3DFVF_TEX1)

/* vertex */
char mavlib_vertList[VERT_ARRAY_SIZE];

/* current normal */
MAV_vector mavlib_lastNorm;

/* current texture coordinate */
MAV_texCoord mavlib_lastTex;

/* current colour */
DWORD mavlib_gfxColour= 0;

/* vertex mode */
int mavlib_vertMode= MAVLIB_PN;
int mavlib_vertSize= sizeof (MAVLIB_PNVERTEX);
int mavlib_vertMax= VERT_ARRAY_SIZE/sizeof (MAVLIB_PNVERTEX);

/* test threshold for 1 bit alpha textures */
#define MAVLIB_ALPHA_THRESHOLD 128

/* pointers to original textures */
MAV_texture *mavlib_mavTextures[GFX_MAX_TEXTURE];

/* loaded textures */
LPDIRECT3DTEXTURE8 mavlib_textures[GFX_MAX_TEXTURE];

/* references for primitive vertex creation */
int mavlib_vert= 0;
int mavlib_mode= 0;

/* options */
int mav_opt_trackMatrix= MAV_FALSE;
int mav_opt_texComps= 4;

int mavlib_matrixmode= MAV_MODELVIEW;
int mavlib_voodoo= MAV_FALSE;

char *mav_gfx_vendor= NULL;
char *mav_gfx_renderer= NULL;
char *mav_gfx_version= NULL;

/* current read pixel buffer */
int mavlib_gfx_readBuffer= 0;

/* background colour */
DWORD mavlib_bk= 0;

/* matrix (modelview or projection) */
int mavlib_matMode= 0;

/* matrices */
MAV_matrix mavlib_worldMat;
MAV_matrix mavlib_projMat;
D3DMATRIX mavlib_D3DWorld;
D3DMATRIX mavlib_D3DProj;

/* current set texture */
int mavlib_texSet= -1;

/* render states - set with default starting states */
int mavlib_gfxPolyMode= MAV_POLYGON_FILL;
int mavlib_gfxNormalize= 1;
int mavlib_gfxBackfaceCull= 0;
int mavlib_gfxDepthTest= 1;
int mavlib_gfxDepthMask= 0;
int mavlib_gfxBlend= MAV_BLEND_OFF;
int mavlib_gfxFog= MAV_FOG_NONE;
float mavlib_gfxFogData1= 1.0;
float mavlib_gfxFogData2= 1.0;
float mavlib_gfxFogR= 0;
float mavlib_gfxFogG= 0;
float mavlib_gfxFogB= 0;
DWORD mavlib_gfxAmb= 0;
DWORD mavlib_gfxLighting= FALSE;
MAV_list *mavlib_gfxLights= NULL;

void mavlib_gfxRenderStateSet(void)
{
  int i;

/* set ctrl-F1 function */
  mav_ctrlF[1]= mavlib_gfxFullscreen;
  mav_ctrlF_desc[1]= "Ctrl-F1 toggle between full screen and in-window rendering";


  mav_gfxPolygonModeSet(mavlib_gfxPolyMode);
  mav_gfxNormalizeSet(mavlib_gfxNormalize);
  mav_gfxBackfaceCullSet(mavlib_gfxBackfaceCull);
  mav_gfxDepthTestSet(mavlib_gfxDepthTest);
  mav_gfxDepthMaskSet(mavlib_gfxDepthMask);
  mav_gfxBlendSet(mavlib_gfxBlend);
  mav_gfxFogSet(mavlib_gfxFog, mavlib_gfxFogData1, mavlib_gfxFogData2,
	mavlib_gfxFogR, mavlib_gfxFogG, mavlib_gfxFogB);

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_AMBIENT, mavlib_gfxAmb),
	"failed to set ambient light");
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_SPECULARENABLE, mavlib_gfxLighting),
	"failed to set specular light");
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_LIGHTING, mavlib_gfxLighting),
	"failed to set lighting");

/* recreate lights */
  if (mavlib_gfxLights)
  {
    MAV_light *light;

    mav_listPointerPush(mavlib_gfxLights);
    mav_listPointerReset(mavlib_gfxLights);
    while (mav_listItemNext(mavlib_gfxLights, (void **) &light))
      mav_gfxLightUse(*light);
    mav_listPointerPop(mavlib_gfxLights);
  }

/* reload textures */
  for (i=0; i<GFX_MAX_TEXTURE; i++)
  {
    if (mavlib_mavTextures[i])
      mav_gfxTextureSet(mavlib_mavTextures[i], NULL);
  }
}

/* function to get a D3DCOLORVALUE from a 4 float array */
D3DCOLORVALUE mavlib_convertMavColour(float col[4])
{
  D3DCOLORVALUE rv;

  rv.r= col[0];
  rv.g= col[1];
  rv.b= col[2];
  rv.a= col[3];

  return rv;
}

/* Option for tracking matrix transformations */

void mavlib_trackMatrix(void)
{
  mav_win_current->viewMat= mav_gfxMatrixGet();

  if (mav_opt_trackMatrix==MAV_PROJANDVIEW) mav_win_current->pdvMat= mav_matrixMult(mav_win_current->projMat, mav_win_current->viewMat);
}

/* Matrix conversion - D3D is row majored */
MAV_matrix mavlib_convertD3DMatrix(D3DMATRIX d3dmat)
{
  MAV_matrix rv;
  int i, j;

  for (i=0; i<4; i++)
    for (j=0; j<4; j++)
      rv.mat[i][j]= d3dmat.m[j][i];

  return rv;
}

D3DMATRIX mavlib_convertMavMatrix(MAV_matrix mavmat)
{
  D3DMATRIX rv;
  int i, j;

  for (i=0; i<4; i++)
    for (j=0; j<4; j++)
      rv.m[i][j]= mavmat.mat[j][i];

  return rv;
}

/* current clip planes */
int mavlib_clips= 0;

void mav_gfxClipPlaneSet(int id, MAV_clipPlane cp)
{
  float cplane[4];
  MAV_vector pt;
  MAV_vector norm2;

/* need to convert plane to view coordinates */
  norm2.x= mav_vectorDotProduct(cp.norm, mav_win_current->right);
  norm2.y= mav_vectorDotProduct(cp.norm, mav_win_current->up);
  norm2.z= -1.0 * mav_vectorDotProduct(cp.norm, mav_win_current->view);

/* get a (world) point on plane */
  pt.x= -cp.d/cp.norm.x;
  pt.y= 0;
  pt.z= 0;

/* transform into view space */
  pt= mav_vectorMult4x4(pt, mavlib_worldMat);

  cplane[0]= -norm2.x;
  cplane[1]= -norm2.y;
  cplane[2]= -norm2.z;
  cplane[3]= mav_vectorDotProduct(pt, norm2);

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetClipPlane(mavlib_D3DDevice, 1<<id,
	cplane), "failed to set clip plane");
}

void mav_gfxClipPlanesSet(MAV_clipPlanes *cp)
{
  int index;

  for (index= 0; index < cp->num; index ++)
    mav_gfxClipPlaneSet(index, cp->planes[index]);
}

void mav_gfxClipPlaneEnable(int id)
{
/* mavlib_clips needs to have all current clip planes */
  mavlib_clips |= 1<<id;
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
		D3DRS_CLIPPLANEENABLE, mavlib_clips),
		"failed to enable clip plane");
}

void mav_gfxClipPlaneDisable(int id)
{
  if (mavlib_clips & (1<<id))
  {
    mavlib_clips -= 1<<id;
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
		D3DRS_CLIPPLANEENABLE, mavlib_clips),
		"failed to disable clip plane");
  }
}

void mav_gfxBackgroundColourSet(float r, float g, float b)
{
/* store for use in mav_gfxClearC and mav_gfxClearCZ */
  mavlib_bk= (((int)(r * 255.0))<<16) + (((int)(g*255.0))<<8) +
		(((int)(b*255.0)));
}

void mav_gfxClearC(void)
{
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->Clear(mavlib_D3DDevice, 0, NULL,
		D3DCLEAR_TARGET, mavlib_bk, 1.0, 0),
		"failed to clear back buffer");
}

void mav_gfxClearZ(void)
{
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->Clear(mavlib_D3DDevice, 0, NULL,
		D3DCLEAR_ZBUFFER, mavlib_bk, 1.0, 0),
		"failed to clear Z buffer");
}

void mav_gfxClearA(void)
{
}

void mav_gfxClearCZ(void)
{
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->Clear(mavlib_D3DDevice, 0, NULL,
		D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER, mavlib_bk, 1.0, 0),
		"failed to clear buffers");
}

void mav_gfxPolygonModeSet(int i)
{
  if (i==MAV_POLYGON_LINE)
  {
/* wireframe */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FILLMODE, D3DFILL_WIREFRAME),
	"failed to set wireframe mode");
  }
  else
  {
/* solid */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FILLMODE, D3DFILL_SOLID),
	"failed to set solid mode");
  }

  mavlib_gfxPolyMode= i;
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
  if (mavlib_matMode==MAV_PROJECTION)
  {
/* projection matrix */
    mav_matrixStackPush(mavlib_projMat);
  }
  else
  {
/* world matrix */
    mav_matrixStackPush(mavlib_worldMat);
  }
}

void mav_gfxMatrixPop(void)
{
  D3DMATRIX d3dmat;
  MAV_matrix popmat;

/* get matrix from stack */
  popmat= mav_matrixStackGet();
  mav_matrixStackPop();
  d3dmat= mavlib_convertMavMatrix(popmat);

  if (mavlib_matMode==MAV_PROJECTION)
  {
/* projection matrix */
    mavlib_projMat= popmat;
    mavlib_D3DProj= d3dmat;
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_PROJECTION, &d3dmat),
	"Error: failed to pop projection matrix");
  }
  else
  {
/* world matrix */
    mavlib_worldMat= popmat;
    mavlib_D3DWorld= d3dmat;
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_WORLD, &d3dmat),
	"failed to pop modelview matrix");
  }

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

MAV_matrix mavlib_matrixTranspose(MAV_matrix mat)
{
/* not used */
  MAV_matrix rv;
  int i,j;
  
  for (i=0; i<4; i++)
  {
    for (j=0; j<4; j++)
    {
      rv.mat[i][j]= mat.mat[j][i];
    }
  }

  return rv;
}

void mav_gfxMatrixMult(MAV_matrix mat)
{
  if (mavlib_matMode==MAV_PROJECTION)
  {
/* projection matrix */
    mavlib_projMat= mav_matrixMult(mavlib_projMat, mat);
    mavlib_D3DProj= mavlib_convertMavMatrix(mavlib_projMat);

/* use Set rather than Multiply so we don't have to multiply */
/* the matrix twice */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
		D3DTS_PROJECTION, &mavlib_D3DProj),
		"failed to multiply projection matrix");
  }
  else
  {
/* world matrix */
    mavlib_worldMat= mav_matrixMult(mavlib_worldMat, mat);
    mavlib_D3DWorld= mavlib_convertMavMatrix(mavlib_worldMat);

/* use Set */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
		D3DTS_WORLD, &mavlib_D3DWorld),
		"failed to multiply modelview matrix");
  }

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixMode(int mode)
{
/* MAV_PROJECTION or MAV_MODELVIEW */
  mavlib_matMode= mode;
}

MAV_matrix mav_gfxMatrixGet(void)
{
  MAV_matrix mav_mat;

  if (mavlib_matMode==MAV_PROJECTION)
  {
/* projection */
    mav_mat= mavlib_projMat;
  }
  else
  {
/* world - should be up to date in mavlib_worldMat */
    mav_mat= mavlib_worldMat;
  }

  return mav_mat;
}

void mav_gfxMatrixLoad(MAV_matrix mat)
{
  D3DMATRIX d3dmat;

  if (mavlib_matMode==MAV_PROJECTION)
  {
/* projection */
    mavlib_projMat= mat;
    mavlib_D3DProj= mavlib_convertMavMatrix(mat);
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
		D3DTS_PROJECTION, &mavlib_D3DProj),
		"failed to load projection matrix");
  }
  else
  {
/* world */
    MAV_matrix id_mat;

    mavlib_worldMat= mat;
    mavlib_D3DWorld= mavlib_convertMavMatrix(mat);
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
		D3DTS_WORLD, &mavlib_D3DWorld),
		"failed to load modelview matrix");

/* reset view matrix to identity (in case it has been fiddled with) */
    id_mat= MAV_ID_MATRIX;
    d3dmat= mavlib_convertMavMatrix(id_mat);
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
		D3DTS_VIEW, &d3dmat),
		"failed to load view matrix");
  }

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixTranslate(MAV_vector t)
{
  MAV_matrix mat;

/* compute translation matrix */
  mat= MAV_ID_MATRIX;

  mat.mat[0][3]= t.x;
  mat.mat[1][3]= t.y;
  mat.mat[2][3]= t.z;

  if (mavlib_matMode==MAV_PROJECTION)
  {
/* projection */
    mavlib_projMat= mav_matrixMult(mavlib_projMat, mat);
    mavlib_D3DProj= mavlib_convertMavMatrix(mavlib_projMat);
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_PROJECTION, &mavlib_D3DProj),
	"failed to translate projection matrix");
  }
  else
  {
/* world */
    mavlib_worldMat= mav_matrixMult(mavlib_worldMat, mat);
    mavlib_D3DWorld= mavlib_convertMavMatrix(mavlib_worldMat);
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_WORLD, &mavlib_D3DWorld),
	"failed to translate modelview matrix");
  }

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxMatrixScale(float x, float y, float z)
{
  MAV_matrix mat;

/* compute scale matrix */
  mat= MAV_ID_MATRIX;

  mat.mat[0][0]= x;
  mat.mat[1][1]= y;
  mat.mat[2][2]= z;

  if (mavlib_matMode==MAV_PROJECTION)
  {
/* projection */
    mavlib_projMat= mav_matrixMult(mavlib_projMat, mat);
    mavlib_D3DProj= mavlib_convertMavMatrix(mavlib_projMat);
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_PROJECTION, &mavlib_D3DProj),
	"Error: failed to scale projection matrix");
  }
  else
  {
/* world */
    mavlib_worldMat= mav_matrixMult(mat, mavlib_worldMat);
    mavlib_D3DWorld= mavlib_convertMavMatrix(mavlib_worldMat);
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_WORLD, &mavlib_D3DWorld),
	"failed to scale modelview matrix");
  }

  if (mav_opt_trackMatrix) mavlib_trackMatrix();
}

void mav_gfxPerspectiveSet(float ncp, float fcp, float fov, float aspect)
{
/* create a perspective transformation matrix that swaps from right */
/* to left handed co-ordinate systems */
  float c,s,Q;

  c= (float) cos(MAV_PI*fov/360.0);
  s= (float) sin(MAV_PI*fov/360.0);

  Q= s/(1.0-ncp/fcp);

  mavlib_D3DProj.m[0][0]= c/(aspect*Q*ncp);
  mavlib_D3DProj.m[0][1]= 0;
  mavlib_D3DProj.m[0][2]= 0;
  mavlib_D3DProj.m[0][3]= 0;
  mavlib_D3DProj.m[1][0]= 0;
  mavlib_D3DProj.m[1][1]= c/(Q*ncp);
  mavlib_D3DProj.m[1][2]= 0;
  mavlib_D3DProj.m[1][3]= 0;
  mavlib_D3DProj.m[2][0]= 0;
  mavlib_D3DProj.m[2][1]= 0;
  mavlib_D3DProj.m[2][2]= -1/ncp;
  mavlib_D3DProj.m[2][3]= -s/(Q*ncp);
  mavlib_D3DProj.m[3][0]= 0;
  mavlib_D3DProj.m[3][1]= 0;
  mavlib_D3DProj.m[3][2]= -1;
  mavlib_D3DProj.m[3][3]= 0;

  mavlib_projMat= mavlib_convertD3DMatrix(mavlib_D3DProj);

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
		D3DTS_PROJECTION, &mavlib_D3DProj),
		"failed to set perspective");
}

void mav_gfxOrthogonalSet(float left, float right, float top, float bottom, float nr, float fr)
{
  MAV_matrix mat;

/* compute orthogonal matrix */
  mat= MAV_ID_MATRIX;

  mat.mat[0][0]= 2.0/(right-left);
  mat.mat[1][1]= 2.0/(bottom-top);
  mat.mat[2][2]= 1.0/(nr-fr);
  mat.mat[0][3]= -1.0-2.0*left/(right-left);
  mat.mat[1][3]= -1.0-2.0*top/(bottom-top);
  mat.mat[2][3]= nr/(nr-fr);

  mavlib_projMat= mat;

  mavlib_D3DProj= mavlib_convertMavMatrix(mat);

/* glOrtho multiples rather than replaces projection matrix so do the same */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->MultiplyTransform(mavlib_D3DDevice,
		D3DTS_PROJECTION, &mavlib_D3DProj),
		"failed to set orthogonal projection");
}

void mav_gfxPolygonBegin(void)
{
/* reset vertex reference variables */
  mavlib_vert= 0;
  mavlib_mode= 1;
}

void mav_gfxPolygonEnd(void)
{
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->DrawPrimitiveUP(mavlib_D3DDevice,
	D3DPT_TRIANGLEFAN, mavlib_vert-2, mavlib_vertList,
	mavlib_vertSize),
	"failed to draw polygon");
  mavlib_mode= 0;
}

void mav_gfxTrianglesBegin(void)
{
/* reset vertex reference variables */
  mavlib_mode= 1;
  mavlib_vert= 0;
}

void mav_gfxTrianglesEnd(void)
{
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->DrawPrimitiveUP(mavlib_D3DDevice,
	D3DPT_TRIANGLELIST, mavlib_vert/3,  mavlib_vertList,
	mavlib_vertSize),
	"failed to draw triangles");

  mavlib_mode= 0;
}

void mav_gfxVertex(MAV_vector v)
{
  if (mavlib_mode!=0)
  {
/* in a valid Begin-End scope */
    switch (mavlib_vertMode)
    {
      case MAVLIB_PN:
/* position/normal vertex */
	{
	  MAVLIB_PNVERTEX *pnv= (MAVLIB_PNVERTEX *) mavlib_vertList;

	  pnv[mavlib_vert].x= v.x;
	  pnv[mavlib_vert].y= v.y;
	  pnv[mavlib_vert].z= v.z;

	  pnv[mavlib_vert].nx= mavlib_lastNorm.x;
	  pnv[mavlib_vert].ny= mavlib_lastNorm.y;
	  pnv[mavlib_vert].nz= mavlib_lastNorm.z;
	}
	break;
      case MAVLIB_PNT:
/* position/normal/texture */
	{
	  MAVLIB_PNTVERTEX *pntv= (MAVLIB_PNTVERTEX *) mavlib_vertList;

	  pntv[mavlib_vert].x= v.x;
	  pntv[mavlib_vert].y= v.y;
	  pntv[mavlib_vert].z= v.z;

	  pntv[mavlib_vert].nx= mavlib_lastNorm.x;
	  pntv[mavlib_vert].ny= mavlib_lastNorm.y;
	  pntv[mavlib_vert].nz= mavlib_lastNorm.z;

	  pntv[mavlib_vert].tu= mavlib_lastTex.s;
	  pntv[mavlib_vert].tv= mavlib_lastTex.t;
	}
	break;
      case MAVLIB_PC:
/* position/colour */
	{
	  MAVLIB_PCVERTEX *pcv= (MAVLIB_PCVERTEX *) mavlib_vertList;

	  pcv[mavlib_vert].x= v.x;
	  pcv[mavlib_vert].y= v.y;
	  pcv[mavlib_vert].z= v.z;

	  pcv[mavlib_vert].color= mavlib_gfxColour;
	}
	break;
      case MAVLIB_PT:
/* position/texture */
	{
	  MAVLIB_PTVERTEX *ptv= (MAVLIB_PTVERTEX *) mavlib_vertList;

	  ptv[mavlib_vert].x= v.x;
	  ptv[mavlib_vert].y= v.y;
	  ptv[mavlib_vert].z= v.z;

	  ptv[mavlib_vert].tu= mavlib_lastTex.s;
	  ptv[mavlib_vert].tv= mavlib_lastTex.t;
	}
	break;
      default:
	fprintf(stderr, "Warning: unknown vertex type\n");
    }

/* point to next vertex */
    if (mavlib_vert < mavlib_vertMax)
    {
      mavlib_vert ++;
    }
    else
    {
      fprintf(stderr, "Warning: mav_gfxVertex: vertex buffer overflow\n");
    }
  }
}

void mav_gfxNormal(MAV_vector v)
{
/* remember normal for vertex operations */
  mavlib_lastNorm= v;
}

void mav_gfxLightSet(MAV_light info)
{
}

void mav_gfxLightUse(MAV_light info)
{
  if (info.defined)
  {
    D3DLIGHT8 d3dlight;
    MAV_light *list_light;
    MAV_light *the_light= NULL;

/* create gfx list if needed */
    if (!mavlib_gfxLights)
      mavlib_gfxLights= mav_listNew();

/* add or update this light */

/* find light in list (need to push pointer in case of nested list */
/* searches) */
    mav_listPointerPush(mavlib_gfxLights);
    mav_listPointerReset(mavlib_gfxLights);
    while (mav_listItemNext(mavlib_gfxLights, (void **) &list_light))
    {
      if (list_light->id==info.id) the_light= list_light;
    }
    mav_listPointerPop(mavlib_gfxLights);

    if (!the_light)
    {
      the_light= (MAV_light *) mav_malloc(sizeof(MAV_light));
      mav_listItemAdd(mavlib_gfxLights, (void *) the_light);
    }

    *the_light= info;

    ZeroMemory(&d3dlight, sizeof(d3dlight));
    d3dlight.Type= D3DLIGHT_DIRECTIONAL;
    d3dlight.Diffuse= mavlib_convertMavColour(info.diffuse);
    d3dlight.Specular= mavlib_convertMavColour(info.specular);
    d3dlight.Ambient= mavlib_convertMavColour(info.ambient);

/* simulate position with a directional light */
    d3dlight.Direction.x= -info.pos.x;
    d3dlight.Direction.y= -info.pos.y;
    d3dlight.Direction.z= -info.pos.z;

    MAVD3DWARN(mavlib_D3DDevice->lpVtbl->SetLight(mavlib_D3DDevice,
	info.index, &d3dlight),
	"failed to set light");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->LightEnable(mavlib_D3DDevice,
	info.index, TRUE),
	"failed to enable light");
  }
}

void mav_gfxLightPos(MAV_light info)
{
  if (info.defined)
  {
    D3DLIGHT8 d3dlight;
    MAV_light *list_light;

/* update gfx light list */
    if (mavlib_gfxLights)
    {
      mav_listPointerPush(mavlib_gfxLights);
      mav_listPointerReset(mavlib_gfxLights);
      while (mav_listItemNext(mavlib_gfxLights, (void **) &list_light))
      {
	if (list_light->index==info.index)
	  *list_light= info;
      }
      mav_listPointerPop(mavlib_gfxLights);
    }

/* get light info */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetLight(mavlib_D3DDevice,
	info.index, &d3dlight),
	"failed to get light");

/* update light direction */
    d3dlight.Direction.x= -info.pos.x;
    d3dlight.Direction.y= -info.pos.y;
    d3dlight.Direction.z= -info.pos.z;

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetLight(mavlib_D3DDevice,
	info.index, &d3dlight),
	"failed to set light");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->LightEnable(mavlib_D3DDevice,
	info.index, TRUE),
	"failed to enable light");
  }
}

void mav_gfxTextureEnv1Set(int v)
{
  switch (v)
  {
    case 1:
/* bilinear interpolation, wrap mode */
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MINFILTER, D3DTEXF_LINEAR),
		"failed to set texture min filter");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MAGFILTER, D3DTEXF_LINEAR),
		"failed to set texture mag filter");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSU, D3DTADDRESS_WRAP),
		"failed to set U texture wrap");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSV, D3DTADDRESS_WRAP),
		"failed to set V texture wrap");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MIPFILTER, D3DTEXF_NONE),
		"failed to reset texture mip filter");
      break;
    case 2:
/* bilinear interpolation, clamp mode */
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MINFILTER, D3DTEXF_LINEAR),
		"failed to set texture min filter");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MAGFILTER, D3DTEXF_LINEAR),
		"failed to set texture mag filter");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSU, D3DTADDRESS_CLAMP),
		"failed to set U texture clamp");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSV, D3DTADDRESS_CLAMP),
		"failed to set V texture clamp");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MIPFILTER, D3DTEXF_NONE),
		"failed to reset texture mip filter");
      break;
    case 3:
/* trilinear (mipmapped) interpolation, wrap mode */
/* (mip filters override min and mag filters) */
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MIPFILTER, D3DTEXF_LINEAR),
		"failed to set texture mip filter");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSU, D3DTADDRESS_WRAP),
		"failed to set U texture wrap");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSV, D3DTADDRESS_WRAP),
		"failed to set V texture wrap");
      break;
    case 4:
/* trilinear (mipmapped) interpolation, clamp mode */
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_MIPFILTER, D3DTEXF_LINEAR),
		"failed to set texture mip filter");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSU, D3DTADDRESS_CLAMP),
		"failed to set U texture clamp");
      MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
		mavlib_D3DDevice, 0, D3DTSS_ADDRESSV, D3DTADDRESS_CLAMP),
		"failed to set V texture clamp");
      break;
  }
}

void mav_gfxTextureEnv2Set(int v)
{
/* this is deal with in mav_gfxColouringModeUse */
#if 0
  switch (v)
  {
    case 1:
modulate tex
    case 2:
decal tex
  }
#endif
}

void mav_gfxColouringModeUse(MAV_palette *p, int mode)
{
  if (mode==MAV_COLOUR) 
  {
/* colour (no textures or lighting) */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetVertexShader(mavlib_D3DDevice,
	MAVLIB_PCVERTEXFLAGS),
	"failed to set colour vertex shader");

/* remember which vertex list to use */
    mavlib_vertMode= MAVLIB_PC;
    mavlib_vertSize= sizeof(MAVLIB_PCVERTEX);
    mavlib_vertMax= VERT_ARRAY_SIZE/mavlib_vertSize;

/* disable lighting */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_LIGHTING, FALSE),
	"failed to reset lighting");

/* disable texturing */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLOROP, D3DTOP_DISABLE),
	"failed to disable texture colour op");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAOP, D3DTOP_DISABLE),
	"failed to disable texture alpha op");
  } 
  else if (mode==MAV_MATERIAL) 
  {
/* lighting */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetVertexShader(mavlib_D3DDevice,
	MAVLIB_PNVERTEXFLAGS),
	"failed to set vertex shader");

    mavlib_vertMode= MAVLIB_PN;
    mavlib_vertSize= sizeof(MAVLIB_PNVERTEX);
    mavlib_vertMax= VERT_ARRAY_SIZE/mavlib_vertSize;

/* enable lighting */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_LIGHTING, TRUE),
	"failed to set lighting");

/* disable textures */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLOROP, D3DTOP_DISABLE),
	"failed to disable texture colour op");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAOP, D3DTOP_DISABLE),
	"failed to disable texture alpha op");
  }
  else if (mode==MAV_TEXTURE) 
  {
/* textures */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetVertexShader(mavlib_D3DDevice,
	MAVLIB_PTVERTEXFLAGS),
	"failed to set colour vertex shader");

/* use coloured vertex list */
    mavlib_vertMode= MAVLIB_PT;
    mavlib_vertSize= sizeof(MAVLIB_PTVERTEX);
    mavlib_vertMax= VERT_ARRAY_SIZE/mavlib_vertSize;

/* disable lighting */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_LIGHTING, FALSE),
	"failed to reset lighting");

/* enable textures */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLOROP, D3DTOP_SELECTARG1),
	"failed to set texture colour op (select arg1)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1),
	"failed to set texture alpha op (select arg1)");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLORARG1, D3DTA_TEXTURE),
	"failed to set texture colour arg1 (texture)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE),
	"failed to set texture alpha arg1 (texture)");
  }
  else if (mode==MAV_BLENDED_TEXTURE) 
  {
/* texture alpha blended with material */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetVertexShader(mavlib_D3DDevice,
	MAVLIB_PNTVERTEXFLAGS),
	"failed to set vertex shader");

    mavlib_vertMode= MAVLIB_PNT;
    mavlib_vertSize= sizeof(MAVLIB_PNTVERTEX);
    mavlib_vertMax= VERT_ARRAY_SIZE/mavlib_vertSize;

/* enable lighting */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_LIGHTING, TRUE),
	"failed to set lighting");

/* enable texturing */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLORARG1, D3DTA_TEXTURE),
	"failed to set texture colour arg1 (texture)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE),
	"failed to set texture alpha arg1 (texture)");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE),
	"failed to set texture colour arg2 (diffuse)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE),
	"failed to set texture alpha arg2 (diffuse)");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLOROP, D3DTOP_BLENDTEXTUREALPHA),
	"failed to set texture colour op (blend texture alpha)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAOP, D3DTOP_BLENDTEXTUREALPHA),
	"failed to set texture alpha op (blend texture alpha)");
  }
  else if (mode==MAV_LIT_TEXTURE) 
  {
/* texture with lights */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetVertexShader(mavlib_D3DDevice,
	MAVLIB_PNTVERTEXFLAGS),
	"failed to set vertex shader");

    mavlib_vertMode= MAVLIB_PNT;
    mavlib_vertSize= sizeof(MAVLIB_PNTVERTEX);
    mavlib_vertMax= VERT_ARRAY_SIZE/mavlib_vertSize;

/* enable lighting */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_LIGHTING, TRUE),
	"failed to set lighting");

/* enable texture with diffuse light */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLORARG1, D3DTA_TEXTURE),
	"failed to set texture colour arg1 (texture)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE),
	"failed to set texture alpha arg1 (texture)");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE),
	"failed to set texture colour arg2 (diffuse)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE),
	"failed to set texture alpha arg2 (diffuse)");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_COLOROP, D3DTOP_MODULATE),
	"failed to set texture colour op (modulate)");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTextureStageState(
	mavlib_D3DDevice, 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE),
	"failed to set texture alpha op (modulate)");
  }
}

int mav_gfxVisualInfoGet(int *r, int *g, int *b, int *a, int *d, int *db, int *ar, int *ag, int *ab, int *aa, int *sb, int *msb)
{
  return 0;
}

void mav_gfxStripQBegin(void)
{
  mav_gfxStripTBegin();
}

void mav_gfxStripQEnd(void)
{
  mav_gfxStripTEnd();
}

void mav_gfxStripTBegin(void)
{
/* reset vertex reference variables */
  mavlib_mode= 1;
  mavlib_vert= 0;
}

void mav_gfxStripTEnd(void)
{
  if (mavlib_vert>2) MAVD3DERR(mavlib_D3DDevice->lpVtbl->DrawPrimitiveUP(mavlib_D3DDevice,
	D3DPT_TRIANGLESTRIP, mavlib_vert-2, mavlib_vertList,
	mavlib_vertSize),
	"failed to draw T strip");
  mavlib_mode= 0;
}

void mav_gfxTexCoord(MAV_texCoord t)
{
/* remember for vertex operations */
  mavlib_lastTex= t;
}

void mav_gfxLineClosedBegin(void)
{
/* reset vertex reference variables */
  mavlib_mode= 1;
  mavlib_vert= 0;
}

void mav_gfxLineClosedEnd(void)
{
  if (mavlib_vert < mavlib_vertMax)
  {
/* copy first vertex */
    memcpy(mavlib_vertList + mavlib_vert*mavlib_vertSize, mavlib_vertList,
	mavlib_vertSize);
    mavlib_vert ++;
  }
  else
  {
    fprintf(stderr, "Warning: mav_gfxLineClosedEnd: vertex buffer overflow\n");
  }

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->DrawPrimitiveUP(mavlib_D3DDevice,
	D3DPT_LINESTRIP, mavlib_vert-1, mavlib_vertList,
	mavlib_vertSize),
	"failed to draw closed line");
  mavlib_mode= 0;
}

void mav_gfxLineBegin(void)
{
/* reset vertex reference variables */
  mavlib_mode= 1;
  mavlib_vert= 0;
}

void mav_gfxLineEnd(void)
{
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->DrawPrimitiveUP(mavlib_D3DDevice,
	D3DPT_LINESTRIP, mavlib_vert-1, mavlib_vertList,
	mavlib_vertSize),
	"failed to draw line");
  mavlib_mode= 0;
}

void mav_gfxMeshTBegin(void)
{
/* StripT does all necessary variable updates */
  mav_gfxStripTBegin();
}

void mav_gfxMeshTEnd(void)
{
  mav_gfxStripTEnd();
}


void mav_gfxColourSet(MAV_colour col)
{
}

void mav_gfxColourUse(MAV_colour col)
{
/* current colour */
  mavlib_gfxColour= D3DCOLOR_ARGB((int)(col.colour[3]*255.0),
		(int)(col.colour[0]*255.0),
		(int)(col.colour[1]*255.0), (int)(col.colour[2]*255.0));

/* set colour for 2D text */
  mavlib_col_r= col.colour[0]*255;
  mavlib_col_g= col.colour[1]*255;
  mavlib_col_b= col.colour[2]*255;
}

void mav_gfxMaterialSet(MAV_material mat)
{
}

void mav_gfxMaterialUse(MAV_material mat)
{
  D3DMATERIAL8 d3dmat;

  ZeroMemory(&d3dmat, sizeof(d3dmat));

  d3dmat.Ambient= mavlib_convertMavColour(mat.ambient);
  d3dmat.Diffuse= mavlib_convertMavColour(mat.diffuse);
  d3dmat.Specular= mavlib_convertMavColour(mat.specular);
  d3dmat.Emissive= mavlib_convertMavColour(mat.emission);

  d3dmat.Power= mat.shine;

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetMaterial(mavlib_D3DDevice,
	&d3dmat),
	"failed to set material");
}

void mavlib_loadTextureImage(MAV_texture *tex, LPDIRECT3DSURFACE8 texsurf, int level, D3DFORMAT texformat)
{
  LPDIRECT3DSURFACE8 fromsurf;
  int width, height;
  D3DLOCKED_RECT surf;
  DWORD rmask, gmask, bmask, amask;
  DWORD *dwpix;
  unsigned char *ptr;
  unsigned char *memptr;
  int i, yp, xp;
  int pix_step;
  int single_alpha= 0;
  DWORD alpha_bits;

  switch (texformat)
  {
    case D3DFMT_X8R8G8B8:
	rmask= 255<<16;
	gmask= 255<<8;
	bmask= 255;
	amask= 0;
	pix_step= 4;
	break;
    case D3DFMT_X1R5G5B5:
	rmask= 31<<10;
	gmask= 31<<5;
	bmask= 31;
	amask= 0;
	pix_step= 2;
	break;
    case D3DFMT_R5G6B5:
	rmask= 31<<11;
	gmask= 63<<5;
	bmask= 31;
	amask= 0;
	pix_step= 2;
	break;
    case D3DFMT_A8R8G8B8:
	rmask= 255<<16;
	gmask= 255<<8;
	bmask= 255;
	amask= 255<<24;
	pix_step= 4;
	break;
    case D3DFMT_A1R5G5B5:
	rmask= 31<<10;
	gmask= 31<<5;
	bmask= 31;
	pix_step= 2;
	amask= 1<<15;
	single_alpha= 1;
	break;
    case D3DFMT_A4R4G4B4:
	rmask= 15<<8;
	gmask= 15<<4;
	bmask= 15;
	amask= 15<<12;
	pix_step= 2;
	break;
    default:
	fprintf(stderr, "Error: unknown texture format\n");
	exit(1);
  }

  if (!level)
  {
    width= tex->width;
    height= tex->height;
  }
  else
  {
    width= tex->xsize[level-1];
    height= tex->ysize[level-1];
  }

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateImageSurface(mavlib_D3DDevice,
	width, height, texformat, &fromsurf),
	"failed to create image surface");

  MAVD3DERR(fromsurf->lpVtbl->LockRect(fromsurf,
	&surf, NULL, 0),
	"failed to lock image surface");

/* blank the texture surface */
  ZeroMemory(surf.pBits, surf.Pitch*height);

/* i is the (byte) index into the Maverik image data */
  i= 0;

/* check where to copy from */
  if (level==0)
  {
    memptr= (char *) tex->mem;
  }
  else
  {
    memptr= (char *) tex->mipmap[level-1];
  }

/* copy the image data */
  for (yp=0; yp<height; yp++)
  {
    ptr= (char *) surf.pBits;

    ptr += yp * surf.Pitch;

    for (xp=0; xp<width; xp++)
    {
/* get a DWORD pointer into the surface data */
      dwpix= (DWORD *) ptr;

      if (single_alpha)
      {
/* single alpha bit - use arbitrary test for alpha */
	if (memptr[i+3]>MAVLIB_ALPHA_THRESHOLD)
	  alpha_bits= amask;
	else
	  alpha_bits= 0;
      }
      else
      {
/* multiple alpha bits */
	alpha_bits= (DWORD) (amask * ((float)memptr[i+3]/255.0)) & amask;
      }

/* get RGBA (use + because this DWORD might be part of another pixel) */
      *dwpix += ((DWORD)(rmask * ((float)memptr[i]/255.0)) & rmask) +
		((DWORD)(gmask * ((float)memptr[i+1]/255.0)) & gmask) +
		((DWORD)(bmask * ((float)memptr[i+2]/255.0)) & bmask) +
		alpha_bits;
/* alpha_bits will be zero for a non-alpha texture surface */

/* move to next pixel in Maverik image ... */
      i += 4;
/* ... and in texture surface */
      ptr += pix_step;
    }
  }

  MAVD3DERR(fromsurf->lpVtbl->UnlockRect(fromsurf),
	"failed to unlock image surface");

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CopyRects(mavlib_D3DDevice,
	fromsurf, NULL, 0, texsurf, NULL),
	"failed to copy texture rect");

/* clean up */
  fromsurf->lpVtbl->Release(fromsurf);
}

#define L2 0.301029995

void mav_gfxTextureSet(MAV_texture *tex, MAV_texEnvFn pTexEnv)
{
  D3DFORMAT texformat;
  LPDIRECT3DSURFACE8 texsurf;
  int width;
  int height;
  int x,y;
  int i, j;
  int thistex;
  unsigned char *fromtex;
  unsigned char *totex;

/* remember this MAV_texture */
  mavlib_mavTextures[tex->id]= tex;

/* delete previous texture */
  if (mavlib_textures[tex->id])
  {
    mavlib_textures[tex->id]->lpVtbl->Release(mavlib_textures[tex->id]);

    if (tex->nmaps>0)
    {
/* free mipmap data */
      for (i=0; i<tex->nmaps; i++)
	mav_free(tex->mipmap[i]);
      mav_free(tex->xsize);
      mav_free(tex->ysize);
      mav_free(tex->mipmap);
      tex->nmaps= 0;
    }
  }

  if (tex->mipmapped)
  {
/* compute (maximum) number of levels */
    if (tex->height>tex->width)
    {
      tex->nmaps= log10(tex->width)/L2;
    }
    else
    {
      tex->nmaps= log10(tex->height)/L2;
    }
  }
  else
  {
/* no mipmaps */
    tex->nmaps= 0;
  }

/* choose a suitable pixel format */
/* if D3D device has no alpha textures RGBA will be the same as RGB */
  if (tex->transparent)
    texformat= mavlib_texRGBA;
  else
    texformat= mavlib_texRGB;

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateTexture(mavlib_D3DDevice,
	tex->width, tex->height, tex->nmaps+1, 0,
	texformat, D3DPOOL_DEFAULT, &mavlib_textures[tex->id]),
	"failed to create texture surface");

/* get texture surface */
  MAVD3DERR(mavlib_textures[tex->id]->lpVtbl->GetSurfaceLevel(
	mavlib_textures[tex->id], 0, &texsurf),
	"failed to get texture surface");

/* copy main texture image onto texture surface */
  mavlib_loadTextureImage(tex, texsurf, 0, texformat);

/* release texture surface */
  texsurf->lpVtbl->Release(texsurf);

/* create mipmap levels */
  if (tex->nmaps>0)
  {
    tex->xsize= (int *) mav_malloc(tex->nmaps*sizeof(int));
    tex->ysize= (int *) mav_malloc(tex->nmaps*sizeof(int));
    tex->mipmap= (unsigned long **) mav_malloc(tex->nmaps *
			sizeof(unsigned long *));

    width= tex->width;
    height= tex->height;

/* get handle to the first mipmap level */
    MAVD3DERR(mavlib_textures[tex->id]->lpVtbl->GetSurfaceLevel(
	mavlib_textures[tex->id], 1, &texsurf),
	"failed to get mipmap surface");

    for (i=0; i<tex->nmaps; i++)
    {
      if (width>=2) width/=2;
      if (height>=2) height/=2;

      tex->xsize[i]= width;
      tex->ysize[i]= height;

      tex->mipmap[i]= (unsigned long *) mav_malloc(width*height*
			sizeof(unsigned long));

/* top level mipmap copies from main texture image, other level mipmaps */
/* copy from the mipmap one level above */
      if (i==0)
      {
        fromtex= (unsigned char *) tex->mem;
      }
      else
      {
	fromtex= (unsigned char *) tex->mipmap[i-1];
      }

/* point to mipmap being copied to */
      totex= (unsigned char *) tex->mipmap[i];

/* generate (memory) mipmap */
      for (y=0; y<height; y++)
      {
        for (x= 0; x<width; x++)
	{
	    thistex= 16*y*width+8*x;
	  for (j=0; j<4; j++)
	  {
/* 4 pixels have equal weighting into new pixel */
	    totex[4*y*width+4*x+j]= (fromtex[thistex+j] +
			fromtex[thistex+4+j] +
			fromtex[thistex+8*width+j] +
			fromtex[thistex+8*width+4+j])/4;
	  }
        }
      }

/* load mipmap image into mipmap texture surface */
      mavlib_loadTextureImage(tex, texsurf, i+1, texformat);

/* free this level */
      texsurf->lpVtbl->Release(texsurf);

/* get next mipmap texture surface */
      if (i<tex->nmaps)
      {
        if (mavlib_textures[tex->id]->lpVtbl->GetSurfaceLevel(
		mavlib_textures[tex->id], i+2, &texsurf) != D3D_OK)
	{
/* surface wasn't created - adjust mipmap count */
/* hopefully this should leave no lost memory areas */
	  tex->nmaps= i;
	}
      }
    }
  }
}


void mav_gfxTextureUse(MAV_texture tex, MAV_texEnvFn pTexEnv)
{
  if (mavlib_textures[tex.id])
  {
/* set texture */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTexture(mavlib_D3DDevice,
		0, (LPDIRECT3DBASETEXTURE8) mavlib_textures[tex.id]),
		"failed to set texture");

    mavlib_texSet= tex.id;
  }
  else
  {
    mavlib_texSet= -1;
  }

/* set texture environment */
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
/* compute ambient light colour */
  mavlib_gfxAmb= (((int)(info.ambient[0] * 255.0))<<16) +
		(((int)(info.ambient[1]*255.0))<<8) +
		(((int)(info.ambient[2]*255.0)));

/* enable ambient light */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_AMBIENT, mavlib_gfxAmb),
	"failed to set ambient light");

/* enable specular light */
  mavlib_gfxLighting= TRUE;
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_SPECULARENABLE, TRUE),
	"failed to enable specular light");

/* enable lighting computations */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_LIGHTING, TRUE),
	"failed to enable lighting");
}

void mav_gfxBufferReadSet(int buf)
{
  mavlib_gfx_readBuffer= buf;
}

void mav_gfxPixelRead(int x, int y, int width, int height, unsigned long *mem)
{
  char *image;
  int i;
  int im_i;

/* get image data */
  image= mav_malloc(width*height*3);
  mav_gfxPixelReadUByte(x, y, width, height, image);

  im_i= 0;
/* copy (with 255 alpha) to mem */
  for (i=0; i<width*height; i++)
  {
    mem[i]= (image[im_i]<<24) + (image[im_i+1]<<16) + (image[im_i+2]<<8) + 255;
    im_i += 3;
  }

/* free image data */
  mav_free(image);
}

void mav_gfxPixelReadUByte(int x, int y, int width, int height, unsigned char *mem)
{
  LPDIRECT3DSURFACE8 buff;
  LPDIRECT3DSURFACE8 backbuff;
  D3DLOCKED_RECT lrect;
  int xp, yp;
  char *ptr;
  DWORD rmask, gmask, bmask;
  DWORD *dwpix;
  int i= 0;
  char r,g,b;
  int pix_step;

/* assume read from back buffer for now */

/* create a surface to read into - needs to be same size as back buffer */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateImageSurface(mavlib_D3DDevice,
	mav_win_current->width, mav_win_current->height,
	mavlib_buffFormat, &buff),
	"failed to create pixel buffer surface");

/* grab the back buffer */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetBackBuffer(mavlib_D3DDevice,
		0, D3DBACKBUFFER_TYPE_MONO, &backbuff),
	"failed to grab back buffer");

/* copy the pixels */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CopyRects(mavlib_D3DDevice,
		backbuff, NULL, 0, buff, NULL),
	"failed to copy pixels from buffer");

/* release the back buffer handle */
  backbuff->lpVtbl->Release(backbuff);

/* lock surface */
/* and a pointer to the surface data */
  MAVD3DERR(buff->lpVtbl->LockRect(buff, &lrect, NULL, D3DLOCK_READONLY),
	"failed to lock pixel buffer");

/* set colour bit masks */
  if (mavlib_buffFormat==D3DFMT_X1R5G5B5)
  {
    rmask= 31<<10;
    gmask= 31<<5;
    bmask= 31;
    pix_step= 2;
  }
  else if (mavlib_buffFormat==D3DFMT_R5G6B5)
  {
    rmask= 31<<11;
    gmask= 63<<5;
    bmask= 31;
    pix_step= 2;
  }
  else
  {
/* only other render target types are 32 bit */
    rmask= 255<<16;
    gmask= 255<<8;
    bmask= 255;
    pix_step= 4;
  }

/* copy pixels */
  for (yp=0; yp<height; yp++)
  {
    ptr= (char *) lrect.pBits;

    ptr += (mav_win_current->height-yp-y-1) * lrect.Pitch;

    ptr += x*pix_step;

    for (xp=0; xp<width; xp++)
    {
/* get RGB */
      dwpix= (DWORD *) ptr;

      r= (255*(rmask & *dwpix))/rmask;
      g= (255*(gmask & *dwpix))/gmask;
      b= (255*(bmask & *dwpix))/bmask;

      mem[i]= r;
      mem[i+1]= g;
      mem[i+2]= b;

/* move to next pixel */
      ptr += pix_step;
      i += 3;
    }
  }

  MAVD3DERR(buff->lpVtbl->UnlockRect(buff),
		"failed to unlock pixel buffer");

/* release surface */
  buff->lpVtbl->Release(buff);
}

void mav_gfxPixelDraw(int w, int h, float *v)
{
  LPDIRECT3DSURFACE8 buff;
  LPDIRECT3DSURFACE8 backbuff;
  D3DLOCKED_RECT lrect;
  int xp, yp;
  char *ptr;
  DWORD rmask, gmask, bmask;
  DWORD *dwpix;
  int i= 0;
  int pix_step;
  POINT copy_pt;

/* assume draw to back buffer for now */

/* create a surface to read from */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateImageSurface(mavlib_D3DDevice,
	w, h, mavlib_buffFormat, &buff),
	"failed to create pixel buffer surface");

/* lock surface */
/* and a pointer to the surface data */
  MAVD3DERR(buff->lpVtbl->LockRect(buff, &lrect, NULL, D3DLOCK_READONLY),
	"failed to lock pixel buffer");

/* set colour bit masks */
  if (mavlib_buffFormat==D3DFMT_X1R5G5B5)
  {
    rmask= 31<<10;
    gmask= 31<<5;
    bmask= 31;
    pix_step= 2;
  }
  else if (mavlib_buffFormat==D3DFMT_R5G6B5)
  {
    rmask= 31<<11;
    gmask= 63<<5;
    bmask= 31;
    pix_step= 2;
  }
  else
  {
/* only other render target types are 32 bit */
    rmask= 255<<16;
    gmask= 255<<8;
    bmask= 255;
    pix_step= 4;
  }

/* copy pixels */
  for (yp=h-1; yp>=0; yp--)
  {
    ptr= (char *) lrect.pBits;

    ptr += yp * lrect.Pitch;

    for (xp=0; xp<w; xp++)
    {
/* get RGB */
      dwpix= (DWORD *) ptr;

      *dwpix += ((DWORD)(rmask*v[i]) & rmask) +
		((DWORD)(gmask*v[i+1]) & gmask) +
		((DWORD)(bmask*v[i+2]) & bmask);

/* move to next pixel */
      ptr += pix_step;
      i += 3;
    }
  }

  MAVD3DERR(buff->lpVtbl->UnlockRect(buff),
		"failed to unlock pixel buffer");

/* grab the back buffer */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetBackBuffer(mavlib_D3DDevice,
		0, D3DBACKBUFFER_TYPE_MONO, &backbuff),
	"failed to grab back buffer");

/* copy the pixels */
  copy_pt.x= mavlib_2d_x;
  copy_pt.y= mavlib_2d_y-h;

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CopyRects(mavlib_D3DDevice,
		buff, NULL, 0, backbuff, &copy_pt),
	"failed to copy pixels to buffer");

/* release the back buffer handle */
  backbuff->lpVtbl->Release(backbuff);

/* release surface */
  buff->lpVtbl->Release(buff);
}

void mav_gfxNormalizeSet(int i)
{
  if (i) 
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_NORMALIZENORMALS, TRUE),
	"failed to set normalize");
  }
  else
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_NORMALIZENORMALS, FALSE),
	"failed to reset normalize");
  }

  mavlib_gfxNormalize= i;
}

void mav_gfxBackfaceCullSet(int i)
{
  if (i) 
  {
/* need clockwise vertex culling */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_CULLMODE, D3DCULL_CW),
	"failed to set cull state");
  }
  else
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_CULLMODE, D3DCULL_NONE),
	"failed to reset cull state");
  }

  mavlib_gfxBackfaceCull= i;
}

int mav_gfxBackfaceCullGet()
{
  int rv= MAV_FALSE;
  DWORD cull_state;

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetRenderState(mavlib_D3DDevice,
	D3DRS_CULLMODE, &cull_state),
	"failed to get cull state");

  if (cull_state!=D3DCULL_NONE) rv= MAV_TRUE;

  return (int) rv;
}

void mav_gfxDepthTestSet(int i)
{
  if (i) 
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
		D3DRS_ZENABLE, D3DZB_TRUE),
		"couldn't enable Z buffer testing");
  }
  else
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
		D3DRS_ZENABLE, D3DZB_FALSE),
		"couldn't disable Z buffer testing");
  }

  mavlib_gfxDepthTest= i;
}

void mav_gfxDepthMaskSet(int i)
{
/* for D3D ZWRITEENABLE is the opposite of depth masking */

  if (i) 
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
		D3DRS_ZWRITEENABLE, FALSE),
		"couldn't enable Z buffer masking");
  }
  else
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
		D3DRS_ZWRITEENABLE, TRUE),
		"couldn't disable Z buffer masking");
  }

  mavlib_gfxDepthMask= i;
}

void mav_gfxViewPortSet(int x, int y, int width, int height)
{
  D3DVIEWPORT8 vp;

/* clear display */
  mav_gfxClearC();

  vp.X= x;

/* D3D y=0 is top of screen, need to swap to bottom for opengl */
/* behaviour */
  vp.Y= mav_win_current->height - y - height;
  vp.Width= width;
  vp.Height= height;
  vp.MinZ= 0.0;
  vp.MaxZ= 1.0;

/* if mav_win_current->height is not accurate you may get a warning */
  MAVD3DWARN(mavlib_D3DDevice->lpVtbl->SetViewport(mavlib_D3DDevice,
		&vp),
		"failed to set viewport (gfx command)");
}

void mav_gfxRasterPosSet(MAV_vector v)
{
/* transpose into screen space */
  v= mav_vectorMult4x4(v, mavlib_worldMat);
  v= mav_vectorMult4x4(v, mavlib_projMat);

/* save x and y pixel values */
  mavlib_2d_x= 0.5*(v.x+1.0) * (float)mav_win_current->width;
  mavlib_2d_y= 0.5*(1.0-v.y) * (float)mav_win_current->height;
}

void mav_gfxRasterPos2DSet(float x, float y)
{
  MAV_vector vt;

/* make a vector at z=0 */
  vt.x= x;
  vt.y= y;
  vt.z= 0;

/* transpose into screen space */
  vt= mav_vectorMult4x4(vt, mavlib_worldMat);
  vt= mav_vectorMult4x4(vt, mavlib_projMat);

/* save x and y pixel values */
  mavlib_2d_x= 0.5*(vt.x+1.0) * (float)mav_win_current->width;
  mavlib_2d_y= 0.5*(1.0-vt.y) * (float)mav_win_current->height;
}

void mav_gfxLineWidthSet(float lw)
{
}

float mav_gfxLineWidthGet(void)
{
  float rv=1;

  return rv;
}

void mav_gfxLineStippleSet(int factor, unsigned short pattern)
{
  D3DLINEPATTERN lp;
  DWORD *thing;

  if (factor>0)
  {
    lp.wRepeatFactor= factor;
    lp.wLinePattern= pattern;
  }
  else
  {
    lp.wRepeatFactor= 1;
    lp.wLinePattern= 0xffff;
  }

/* I'm sure there's a better way of doing this cast, but what the hey */
  thing= (DWORD *) &lp;

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
		D3DRS_LINEPATTERN, *thing),
		"failed to set line stipple");
}

void mav_gfxBlendSet(int v)
{
  switch (v)
  {
  case MAV_BLEND_OFF:
/* alpha blending off */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_ALPHABLENDENABLE, FALSE),
	"failed to disable alpha blending");
    break;

  case MAV_BLEND_1:
/* set alpha blending to alpha, 1-alpha */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_ALPHABLENDENABLE, TRUE),
	"failed to enable alpha blending");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_SRCBLEND, D3DBLEND_SRCALPHA),
	"failed to set alpha src mode");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA),
	"failed to set alpha dest mode");
    break;
  }

  mavlib_gfxBlend= v;
}

void mav_gfxAccumSet(int mode, float val)
{
  switch (mode)
  {
  case MAV_ACCUM_ACCUM:

  case MAV_ACCUM_LOAD:
    break;

  case MAV_ACCUM_RETURN:
    break;

  case MAV_ACCUM_ADD:
    break;

  case MAV_ACCUM_MULT:
    break;
  }
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

void mav_gfxFogSet(int type, float data1, float data2, float r, float g, float b)
{
  D3DCOLOR col;

/* store value */
  mavlib_gfxFog= type;
  mavlib_gfxFogData1= data1;
  mavlib_gfxFogData2= data2;
  mavlib_gfxFogR= r;
  mavlib_gfxFogG= g;
  mavlib_gfxFogB= b;

  col= D3DCOLOR_ARGB(255, (int)(r*255.0), (int)(g*255.0), (int)(b*255.0));

/* the float to DWORD casts look a bit nasty, but that's what */
/* it says to do in the manual... */
  if (type==MAV_FOG_EXP2)
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGENABLE, TRUE),
	"failed to enable fog");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGVERTEXMODE, D3DFOG_EXP2),
	"failed to set fog mode");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGCOLOR, col),
	"failed to set fog colour");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGDENSITY, *((DWORD *) (&data1))),
	"failed to set fog density");
  }
  else if (type==MAV_FOG_EXP)
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGENABLE, TRUE),
	"failed to enable fog");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGVERTEXMODE, D3DFOG_EXP),
	"failed to set fog mode");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGCOLOR, col),
	"failed to set fog colour");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGDENSITY, *((DWORD *) (&data1))),
	"failed to set fog density");
  }
  else if (type==MAV_FOG_LINEAR)
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGENABLE, TRUE),
	"failed to enable fog");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGVERTEXMODE, D3DFOG_LINEAR),
	"failed to set fog mode");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGCOLOR, col),
	"failed to set fog colour");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGSTART, *((DWORD *) (&data1))),
	"failed to set fog start");
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGEND, *((DWORD *) (&data2))),
	"failed to set fog end");
  }
  else if (type==MAV_FOG_NONE)
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderState(mavlib_D3DDevice,
	D3DRS_FOGENABLE, FALSE),
	"failed to disable fog");
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Bad value when setting fog, ignoring.\n");
  }
}
