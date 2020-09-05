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


/* VRML97 parser uses the free CyberVRML97 for C++ library */
/* by Satoshi Konno (http://www.cyber.koganei.tokyo.jp/top/index.html) */



#include "maverik.h"
#include "mav_vrml97.h"
#include "SceneGraph.h"
#include <stdio.h>
#include <math.h>

#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
extern "C" char *strdup(const char *);
#endif

#ifdef MAV_IRIX5
/* Dont have these defined on our Irix 5 version of CC */
bool false=0;
bool true=!false;
#endif

int mav_opt_VRML97CleanUp= MAV_TRUE;
int mav_opt_VRML97HBBThreshold= 0;

MAV_SMS *mavlib_nodeObj;
int mavlib_nodeObjCount;
int mavlib_matScaleWarn;
int mavlib_vrml97BoxCount;
int mavlib_vrml97CylinderCount;
int mavlib_vrml97ConeCount;
int mavlib_vrml97SphereCount;
int mavlib_vrml97IFSCount;
int mavlib_vrml97IFSFaceCount;
int mavlib_vrml97IFSVertCount;
int mavlib_vrml97ILSCount;
int mavlib_vrml97ILSLineCount;
int mavlib_vrml97ILSVertCount;
int mavlib_vrml97InlineCount;

/* Casts to convert generic node into a specfic type. Note that is*Node retuns true if it is */
/* an instance node which refers to the queried note type. So, these casts need to either */
/* cast the supplied node, or the node it references. */

Node *mavlib_getReferenceNode(Node *n)
{
  Node *rv= n->getReferenceNode();
  while (rv->getReferenceNode()) rv= rv->getReferenceNode();
  return rv;
}

ShapeNode *mavlib_castToShapeNode(Node *n)
{
  ShapeNode *rv= (ShapeNode *) n;
  if (n->isInstanceNode()) rv= (ShapeNode *) mavlib_getReferenceNode(n);
  return rv;
}

TransformNode *mavlib_castToTransformNode(Node *n)
{
  TransformNode *rv= (TransformNode *) n;
  if (n->isInstanceNode()) rv= (TransformNode *) mavlib_getReferenceNode(n);
  return rv;
}

InlineNode *mavlib_castToInlineNode(Node *n)
{
  InlineNode *rv= (InlineNode *) n;
  if (n->isInstanceNode()) rv= (InlineNode *) mavlib_getReferenceNode(n);
  return rv;
} 

SphereNode *mavlib_castToSphereNode(Node *n)
{
  SphereNode *rv= (SphereNode *) n;
  if (n->isInstanceNode()) rv= (SphereNode *) mavlib_getReferenceNode(n);
  return rv;
}

BoxNode *mavlib_castToBoxNode(Node *n)
{
  BoxNode *rv= (BoxNode *) n;
  if (n->isInstanceNode()) rv= (BoxNode *) mavlib_getReferenceNode(n);
  return rv;
}

CylinderNode *mavlib_castToCylinderNode(Node *n)
{
  CylinderNode *rv= (CylinderNode *) n;
  if (n->isInstanceNode()) rv= (CylinderNode *) mavlib_getReferenceNode(n);
  return rv;
}

ConeNode *mavlib_castToConeNode(Node *n)
{
  ConeNode *rv= (ConeNode *) n;
  if (n->isInstanceNode()) rv= (ConeNode *) mavlib_getReferenceNode(n);
  return rv;
}

IndexedFaceSetNode *mavlib_castToIndexedFaceSetNode(Node *n)
{
  IndexedFaceSetNode *rv= (IndexedFaceSetNode *) n;
  if (n->isInstanceNode()) rv= (IndexedFaceSetNode *) mavlib_getReferenceNode(n);
  return rv;
}

IndexedLineSetNode *mavlib_castToIndexedLineSetNode(Node *n)
{
  IndexedLineSetNode *rv= (IndexedLineSetNode *) n;
  if (n->isInstanceNode()) rv= (IndexedLineSetNode *) mavlib_getReferenceNode(n);
  return rv;
}



/* Routine to generate a box */

MAV_object *mavlib_dealWithBoxNode(BoxNode *n, MAV_surfaceParams *sp, MAV_matrix m)
{
  MAV_box *b;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr,"\rBuilding objects... Object %i - box                                        ", mavlib_nodeObjCount);

  if (fabs(m.mat[0][0]-m.mat[1][1])>0.01 || fabs(m.mat[0][0]-m.mat[2][2])>0.01) mavlib_matScaleWarn++;

  b= (MAV_box *) mav_malloc(sizeof(MAV_box));
  b->size.x= n->getX();
  b->size.y= n->getY();
  b->size.z= n->getZ();
  b->matrix= m;
  b->sp= sp;

  mavlib_vrml97BoxCount++;

  return mav_objectNew(mav_class_box, b);
}



/* Routine to generate a sphere */

MAV_object *mavlib_dealWithSphereNode(SphereNode *n, MAV_surfaceParams *sp, MAV_matrix m)
{
  MAV_sphere *s;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr,"\rBuilding objects... Object %i - sphere                                     ", mavlib_nodeObjCount);

  if (fabs(m.mat[0][0]-m.mat[1][1])>0.01 || fabs(m.mat[0][0]-m.mat[2][2])>0.01) mavlib_matScaleWarn++;

  s= (MAV_sphere *) mav_malloc(sizeof(MAV_sphere));
  s->radius= n->getRadius();
  s->nverts= 10;
  s->nchips= 10;
  s->matrix= m;
  s->sp= sp;

  mavlib_vrml97SphereCount++;

  return mav_objectNew(mav_class_sphere, s);
}



/* Routine to generate a cylinder */

MAV_object *mavlib_dealWithCylinderNode(CylinderNode *n, MAV_surfaceParams *sp, MAV_matrix m)
{
  MAV_cylinder *cyl;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr,"\rBuilding objects... Object %i - cylinder                                   ", mavlib_nodeObjCount);

  if (fabs(m.mat[0][0]-m.mat[1][1])>0.01 || fabs(m.mat[0][0]-m.mat[2][2])>0.01) mavlib_matScaleWarn++;

  cyl= (MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  cyl->radius= n->getRadius();
  cyl->height= n->getHeight();
  cyl->nverts= 10;
  cyl->endcap= MAV_TRUE;
  cyl->matrix= mav_matrixMult(m, mav_matrixSet(0,-90,0,0,0,0));
  cyl->sp= sp;  

  mavlib_vrml97CylinderCount++;

  return mav_objectNew(mav_class_cylinder, cyl);
}



/* Routine to generate a cone */

MAV_object *mavlib_dealWithConeNode(ConeNode *n, MAV_surfaceParams *sp, MAV_matrix m)
{
  MAV_cone *cone;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr,"\rBuilding objects... Object %i - cone                                       ", mavlib_nodeObjCount);

  if (fabs(m.mat[0][0]-m.mat[1][1])>0.01 || fabs(m.mat[0][0]-m.mat[2][2])>0.01) mavlib_matScaleWarn++;

  cone= (MAV_cone *) mav_malloc(sizeof(MAV_cone));
  cone->rt= 0.0;
  cone->rb= n->getBottomRadius();
  cone->height= n->getHeight();
  cone->nverts= 10;
  cone->endcap= MAV_TRUE;
  cone->matrix= mav_matrixMult(m, mav_matrixSet(0,-90,0,0,0,0));
  cone->sp= sp;  

  mavlib_vrml97ConeCount++;

  return mav_objectNew(mav_class_cone, cone);
}



/* Routine to generate an indexed face set (a facet) */

MAV_object *mavlib_dealWithIndexedFaceSetNode(IndexedFaceSetNode *n, MAV_surfaceParams *sp, MAV_matrix m, MAV_matrix texTran)
{ 
  int i, j, idx, idxbk, nf=0, nv=0;
  CoordinateNode *pts= NULL; 
  TextureCoordinateNode *texc= NULL;
  NormalNode *norms= NULL;
  MAV_facet *facet;
  float v[3], vt[2];
  MAV_vector v1, v2, tvec;
  MAV_object *o;
  MAV_BB bb;
  int n_getCCW= n->getCCW();
  int n_getNTexCoordIndexes= n->getNTexCoordIndexes();
  int n_getNormalPerVertex= n->getNormalPerVertex();
  int n_getNNormalIndexes= n->getNNormalIndexes();
  float n_getCreaseAngle= n->getCreaseAngle();
  int n_getNCoordIndexes= n->getNCoordIndexes();
  int pts_getNPoints= 0;
  int norms_getNVectors= 0;
  int texc_getNPoints= 0;

  /* Get the coordinates node */
  pts= n->getCoordinateNodes();
  if (pts) pts_getNPoints= pts->getNPoints();

  /* Get the normal node */
  norms= n->getNormalNodes();
  if (norms) norms_getNVectors= norms->getNVectors();

  /* Get the texture coordinate node */
  texc= n->getTextureCoordinateNodes();
  if (texc) texc_getNPoints= texc->getNPoints();

  if (!pts) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: can not find the indexed face set coordinate node, ignoring\n");
    return NULL;
  }

  /* Ensure that its convex */
  if (!n->getConvex()) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: indexed face set is concave, ignoring\n");
    return NULL;
  }

  /* Misc traps */
  if (n->getColorNodes()) if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: colour not yet supported, ignoring\n");

  /* Work with a local copy of the coord indexes for efficiency */
  if (n_getNCoordIndexes==0) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: indexed face set contains no indices, ignoring\n");
    return NULL;
  }    
  int *coordIdx= new int[n_getNCoordIndexes];

  /* Copy indexes and count number of faces and vertices */
  for (i=0; i<n_getNCoordIndexes; i++) {
    coordIdx[i]= n->getCoordIndexNext();
    if (coordIdx[i]==-1)
    {
      nf++;
    }
    else
    {
      nv++;
    }
  }

  /* Last face may be implicit rather than have a -1 */
  if (coordIdx[n_getNCoordIndexes-1]!=-1) nf++;

  /* Work with a local copy of the points for efficiency */
  if (pts_getNPoints==0) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: indexed face set contains no points, ignoring\n");
    return NULL;
  }
  float *ptsIdx= new float[pts_getNPoints*3];
  for (i=0; i<pts_getNPoints; i++) pts->getPointNext(&ptsIdx[i*3]);

  /* Work with a local copy of normals for efficiency */
  float *normIdx= NULL;
  if (norms) {
    normIdx= new float[norms_getNVectors*3];
    for (i=0; i<norms_getNVectors; i++) norms->getVectorNext(&normIdx[i*3]);
  }

  /* Work with a local copy of texture coordinates for efficiency */
  float *texcIdx= NULL;
  if (texc) {
    texcIdx= new float[texc_getNPoints*2];
    for (i=0; i<texc_getNPoints; i++) texc->getPointNext(&texcIdx[i*2]);
  }

  /* Create facet to represent the indexed face set */
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\rBuilding objects... Object %i - facet with %i faces and %i vertices      ", mavlib_nodeObjCount, nf, nv); 
  mavlib_vrml97IFSCount++;
  mavlib_vrml97IFSFaceCount+= nf;
  mavlib_vrml97IFSVertCount+= nv;

  if (fabs(m.mat[0][0]-m.mat[1][1])>0.01 || fabs(m.mat[0][0]-m.mat[2][2])>0.01) mavlib_matScaleWarn++;

  facet= (MAV_facet *) mav_malloc(sizeof(MAV_facet));
  facet->npolys= nf;
  facet->np= (int *) mav_malloc(facet->npolys*sizeof(int));
  facet->norm= (MAV_vector **) mav_malloc(facet->npolys*sizeof(MAV_vector *));
  facet->vert= (MAV_vector **) mav_malloc(facet->npolys*sizeof(MAV_vector *));
  facet->tex= (MAV_texCoord **) mav_malloc(facet->npolys*sizeof(MAV_texCoord *));
  facet->sp= (MAV_surfaceParams **) mav_malloc(facet->npolys*sizeof(MAV_surfaceParams *));
  facet->matrix= m;

  MAV_vector *perfacenormal= new MAV_vector[facet->npolys];

  idx=0;
  for (i=0; i<facet->npolys; i++) {

    /* Count number of vertices in this face */
    nv=0;
    idxbk= idx;
    while (!(idx==n_getNCoordIndexes || coordIdx[idx]==-1)) {
      nv++;
      idx++;
    }

    if (nv>=3)
    {
      /* Malloc of room for the vertices */
      facet->np[i]= nv;
      facet->norm[i]= (MAV_vector *) mav_malloc(facet->np[i]*sizeof(MAV_vector));
      facet->vert[i]= (MAV_vector *) mav_malloc(facet->np[i]*sizeof(MAV_vector));
      facet->tex[i]= (MAV_texCoord *) mav_malloc(facet->np[i]*sizeof(MAV_texCoord));
      facet->sp[i]= sp;

      /* Store the vertices */
      nv=0;
      idx= idxbk;
      while (!(idx==n_getNCoordIndexes || coordIdx[idx]==-1)) {
	
	/* Ensure that the index is valid */
	if (coordIdx[idx] >= pts_getNPoints) 
	{
	  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: coordinate index too big, setting to max\n");
	  v[0]= ptsIdx[(pts_getNPoints-1)*3+0];
	  v[1]= ptsIdx[(pts_getNPoints-1)*3+1];
	  v[2]= ptsIdx[(pts_getNPoints-1)*3+2];
	}
	else
	{
	  v[0]= ptsIdx[coordIdx[idx]*3+0];
	  v[1]= ptsIdx[coordIdx[idx]*3+1];
	  v[2]= ptsIdx[coordIdx[idx]*3+2];
	}

	/* Account for vertex ordering */
	if (n_getCCW)
	{
	  facet->vert[i][nv].x= v[0];
	  facet->vert[i][nv].y= v[1];
	  facet->vert[i][nv].z= v[2];    
	}
	else
	{
	  facet->vert[i][facet->np[i]-1-nv].x= v[0];
	  facet->vert[i][facet->np[i]-1-nv].y= v[1];
	  facet->vert[i][facet->np[i]-1-nv].z= v[2];    
	}

	nv++;
	idx++;
      }

      /* Store the texture coordinates */
      if (texc) { // Texture coordinates supplied
	nv=0;
	idx= idxbk;
	while (!(idx==n_getNCoordIndexes || coordIdx[idx]==-1)) {
	
	  if (n_getNTexCoordIndexes==0) // Implicit ordering (same as coordIndex)
	  {	
	    /* Ensure that the index is valid */
	    if (coordIdx[idx] >= texc_getNPoints)
	    {
	      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: coordinate index too big, setting to max\n");
	      vt[0]= texcIdx[(texc_getNPoints-1)*2+0];
	      vt[1]= texcIdx[(texc_getNPoints-1)*2+1];
	    }
	    else
	    {
	      vt[0]= texcIdx[coordIdx[idx]*2+0];
	      vt[1]= texcIdx[coordIdx[idx]*2+1];
	    }
	  }
	  else // Explicit ordering (suppled in texCoordIndex)
	  {
	    /* Ensure that the index is valid */
	    int ntexidx= n->getTexCoordIndex(idx);
	    if (ntexidx >= texc_getNPoints)
	    {
	      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: texture coordinate index too big, setting to max\n");
	      vt[0]= texcIdx[(texc_getNPoints-1)*2+0];
	      vt[1]= texcIdx[(texc_getNPoints-1)*2+1];
	    }
	    else
	    {
	      vt[0]= texcIdx[ntexidx*2+0];
	      vt[1]= texcIdx[ntexidx*2+1];
	    }
	  }

	  /* Account for texture transform matrix */
	  tvec= mav_vectorMult(mav_vectorSet(vt[0], vt[1], 0), texTran);
	  vt[0]= tvec.x;
	  vt[1]= tvec.y;

	  /* Account for vertex ordering */
	  if (n_getCCW)
	  {
	    facet->tex[i][nv].s= vt[0];
	    facet->tex[i][nv].t= vt[1];
	  }
	  else
	  {
	    facet->tex[i][facet->np[i]-1-nv].s= vt[0];
	    facet->tex[i][facet->np[i]-1-nv].t= vt[1];
	  }
	  
	  nv++;
	  idx++;
	}
      }

      /* Store the normals */
      if (norms)  // Normals supplied
      {
	if (n_getNormalPerVertex) // Defined per-vertex
	{
	  nv=0;
	  idx= idxbk;
	  while (!(idx==n_getNCoordIndexes || coordIdx[idx]==-1)) {
	
	    if (n_getNNormalIndexes==0) // Implicit ordering (same as coordIndex)
	    {	
	      /* Ensure that the index is valid */
	      if (coordIdx[idx] >= norms_getNVectors)
	      {
		if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: coordinate index too big, setting to max\n");
		v[0]= normIdx[(norms_getNVectors-1)*3+0];
		v[1]= normIdx[(norms_getNVectors-1)*3+1];
		v[2]= normIdx[(norms_getNVectors-1)*3+2];
	      }
	      else
	      {
		v[0]= normIdx[coordIdx[idx]*3+0];
		v[1]= normIdx[coordIdx[idx]*3+1];
		v[2]= normIdx[coordIdx[idx]*3+2];
	      }
	    }
	    else // Explicit ordering (suppled in normalIndex)
	    {
	      /* Ensure that the index is valid */
	      int nidx= n->getNormalIndex(idx);
	      if (nidx >= norms_getNVectors)
	      {
		if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: normal index too big, setting to max\n");
		v[0]= normIdx[(norms_getNVectors-1)*3+0];
		v[1]= normIdx[(norms_getNVectors-1)*3+1];
		v[2]= normIdx[(norms_getNVectors-1)*3+2];
	      }
	      else
	      {
		v[0]= normIdx[nidx*3+0];
		v[1]= normIdx[nidx*3+1];
		v[2]= normIdx[nidx*3+2];
	      }
	    }

	    /* Account for vertex ordering */
	    if (n_getCCW)
	    {
	      facet->norm[i][nv].x= v[0];
	      facet->norm[i][nv].y= v[1];
	      facet->norm[i][nv].z= v[2];    
	    }
	    else
	    {
	      facet->norm[i][facet->np[i]-1-nv].x= v[0];
	      facet->norm[i][facet->np[i]-1-nv].y= v[1];
	      facet->norm[i][facet->np[i]-1-nv].z= v[2];    
	    }

	    nv++;
	    idx++;
	  }
	}
	else // Defined per-face
	{
	  if (n_getNNormalIndexes==0) // Implicit ordering
	  {
	    /* Get normal - ensure index is valid */
	    if (i >= norms_getNVectors) 
	    {
	      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: more faces than supplied normals, setting to max\n");
	      v[0]= normIdx[(norms_getNVectors-1)*3+0];
	      v[1]= normIdx[(norms_getNVectors-1)*3+1];
	      v[2]= normIdx[(norms_getNVectors-1)*3+2];
	    }
	    else
	    {
	      v[0]= normIdx[i*3+0];
	      v[1]= normIdx[i*3+1];
	      v[2]= normIdx[i*3+2];
	    }

	    /* Set to all vertices in face */
	    for (nv=0; nv<facet->np[i]; nv++) {
	      facet->norm[i][nv].x= v[0];
	      facet->norm[i][nv].y= v[1];
	      facet->norm[i][nv].z= v[2];
	    }
	  }
	  else // Explicit ordering
	  {
	    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: indexed per-face normals not supported yet, ignoring\n");
	  }
	}
      }
      else // Normals need to be generated
      {
	/* For now calculate a per-face normal and store in tmp array */
	j=0;
	do {
	  /* Find 2 non-parallel edge vectors */
	  if (j==facet->np[i]-2)
	  {
	    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: can't find surface normal, ignoring\n");
	    facet->np[i]=0;
	    v1.x= 1;
	    v1.y= 0;
	    v1.z= 0;
	    v2.x= 0;
	    v2.y= 1;
	    v2.z= 0;
	  }
	  else
	  {
	    v1= mav_vectorNormalise(mav_vectorSub(facet->vert[i][j+2],facet->vert[i][j+1]));
	    v2= mav_vectorNormalise(mav_vectorSub(facet->vert[i][j],facet->vert[i][j+1]));
	  }
	  j++;
	} while (fabs(mav_vectorDotProduct(v1, v2))==1.0);

	/* Cross product them to get normal and store */
	perfacenormal[i]= mav_vectorNormalise(mav_vectorCrossProduct(v1,v2));
      }
    }    
    else
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: face with less than 3 vertices, ignoring\n");
      facet->np[i]=0;
    }

    idx++;
  }

  /* Smooth vertex normals */
  if (!norms) {

    /* Find the maximum number of faces any vertex is in */
    int *ptslist= new int[pts_getNPoints];
    for (i=0; i<pts_getNPoints; i++) ptslist[i]= 0;
    int maxfc=0;
    
    for (i=0; i<n_getNCoordIndexes; i++) {
      if (coordIdx[i]!=-1 && coordIdx[i]<pts_getNPoints) {
	ptslist[coordIdx[i]]++;
	if (ptslist[coordIdx[i]]>maxfc) maxfc= ptslist[coordIdx[i]];
      }
    }

    for (i=0; i<pts_getNPoints; i++) ptslist[i]= 0;

    /* For each point store a list of the facet faces and vertex numbers */
    int *theface= new int [pts_getNPoints*maxfc];
    int *thevertex= new int[pts_getNPoints*maxfc];

    nf=0;
    nv=0;
    for (i=0; i<n_getNCoordIndexes; i++) 
    {
      if (coordIdx[i]==-1) 
      {
	nf++;
	nv=i+1;
      }
      else
      {
	if (facet->np[nf]) {
	  theface[coordIdx[i]*maxfc+ptslist[coordIdx[i]]]= nf;

	  if (n_getCCW)
	  {
	    thevertex[coordIdx[i]*maxfc+ptslist[coordIdx[i]]]= i-nv;
	  }
	  else
	  {
	    thevertex[coordIdx[i]*maxfc+ptslist[coordIdx[i]]]= facet->np[nf]-1-(i-nv);
	  }

	  ptslist[coordIdx[i]]++;
	}
      }
    }
    
    /* For each point defined */
    for (i=0; i<pts_getNPoints; i++) {

      /* Calculate the normal for each face the point appears in */
      for (nf=0; nf<ptslist[i]; nf++) {
	
	MAV_vector accnorm;
	accnorm.x=0;
	accnorm.y=0;
	accnorm.z=0;
	
	/* If angle between faces is less than crease angle, then update face normal */
	for (nv=0; nv<ptslist[i]; nv++) {
	  float aval= mav_vectorDotProduct(perfacenormal[theface[i*maxfc+nf]], perfacenormal[theface[i*maxfc+nv]]);
	  if (aval>1.0) aval=1.0;
	  if (acos(aval)<=n_getCreaseAngle+0.01) accnorm= mav_vectorAdd(accnorm, perfacenormal[theface[i*maxfc+nv]]);
	}

	facet->norm[theface[i*maxfc+nf]][thevertex[i*maxfc+nf]]= mav_vectorNormalise(accnorm);
      }
    }

    delete theface;
    delete thevertex;
    delete ptslist;
  }

  delete coordIdx;
  if (norms) delete normIdx;
  if (texc) delete texcIdx;
  delete ptsIdx;
  delete perfacenormal;

  o= mav_objectNew(mav_class_facet, facet);

  if (!texc && sp->mode>=MAV_TEXTURE) { // Need to automatically generate texture coordinates from BB
    facet->matrix= MAV_ID_MATRIX; // Local BB
    if (mav_callbackBBExec(mav_win_current, o, &bb))
    {
      float bbax[3], axv[2];
      int ax[2];
            
      /* Calculate size of BB */
      bbax[0]= bb.max.x-bb.min.x;
      bbax[1]= bb.max.y-bb.min.y;
      bbax[2]= bb.max.z-bb.min.z;

      /* Calculate which are the longest 2 dimensions */
      for (i=0; i<2; i++) { /* s,t */
	ax[i]= -1;
	axv[i]= -1.0;
	for (j=0; j<3; j++) { /* x,y,z */
	  if (axv[i]<bbax[j]) {
	    axv[i]=bbax[j];
	    ax[i]=j;
	  }
	}
	bbax[ax[i]]=-2.0;
      }
      
      /* Calculate texture coordinates */
      for (i=0; i<facet->npolys; i++) {
	for (j=0; j<facet->np[i]; j++) {

	  /* S */
	  switch (ax[0]) {
	  case 0: // X axis
	    facet->tex[i][j].s= (facet->vert[i][j].x-bb.min.x)/(bb.max.x-bb.min.x);
	    break;	    
	  case 1: // Y axis
	    facet->tex[i][j].s= (facet->vert[i][j].y-bb.min.y)/(bb.max.y-bb.min.y);
	    break;	   
	  case 2: // Z axis
	    facet->tex[i][j].s= (facet->vert[i][j].z-bb.min.z)/(bb.max.z-bb.min.z);
	    break;
	  default:
	    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: failed to calculate texture coordinates, ignoring\n");
	    break;
	  }

	  /* T */
	  switch (ax[1]) {
	  case 0: // X axis
	    facet->tex[i][j].t= (facet->vert[i][j].x-bb.min.x)/(bb.max.x-bb.min.x);
	    break;	    
	  case 1: // Y axis
	    facet->tex[i][j].t= (facet->vert[i][j].y-bb.min.y)/(bb.max.y-bb.min.y);
	    break;	   
	  case 2: // Z axis
	    facet->tex[i][j].t= (facet->vert[i][j].z-bb.min.z)/(bb.max.z-bb.min.z);
	    break;
	  default:
	    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: failed to calculate texture coordinates, ignoring\n");
	    break;
	  }	      
	}
      }
    }
    else
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: failed to calculate texture coordinates, ignoring\n");
    }

    facet->matrix= m; // Restore matrix
  }

  return o;
}



/* Routine to generate an indexed line set */

MAV_object *mavlib_dealWithIndexedLineSetNode(IndexedLineSetNode *n, MAV_surfaceParams *sp, MAV_matrix m)
{
  int i, idx, idxbk, np=0, nv=0;
  CoordinateNode *pts= NULL; 
  MAV_polyline *polyline;
  float v[3];
  MAV_object *o;
  int n_getNCoordIndexes= n->getNCoordIndexes();
  int pts_getNPoints= 0;

  /* Get the coordinates node */
  pts= n->getCoordinateNodes();
  if (pts) pts_getNPoints= pts->getNPoints();

  if (!pts) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: can not find the indexed line set coordinate node, ignoring\n");
    return NULL;
  }

  /* Misc traps */
  if (n->getColorNodes()) if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: colour not yet supported, ignoring\n");

  /* Work with a local copy of the coord indexes for efficiency */
  if (n_getNCoordIndexes==0) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: indexed line set contains no indices, ignoring\n");
    return NULL;
  }    
  int *coordIdx= new int[n_getNCoordIndexes];

  /* Copy indexes and count number of lines and vertices */
  for (i=0; i<n_getNCoordIndexes; i++) {
    coordIdx[i]= n->getCoordIndexNext();
    if (coordIdx[i]==-1)
    {
      np++;
    }
    else
    {
      nv++;
    }
  }

  /* Last line may be implicit rather than have a -1 */
  if (coordIdx[n_getNCoordIndexes-1]!=-1) np++;

  /* Work with a local copy of the points for efficiency */
  if (pts_getNPoints==0) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: indexed line set contains no points, ignoring\n");
    return NULL;
  }
  float *ptsIdx= new float[pts_getNPoints*3];
  for (i=0; i<pts_getNPoints; i++) pts->getPointNext(&ptsIdx[i*3]);

  /* Create polyline to represent the indexed line set */
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\rBuilding objects... Object %i - polyline with %i lines and %i vertices      ", mavlib_nodeObjCount, np, nv); 
  mavlib_vrml97ILSCount++;
  mavlib_vrml97ILSLineCount+= np;
  mavlib_vrml97ILSVertCount+= nv;

  if (fabs(m.mat[0][0]-m.mat[1][1])>0.01 || fabs(m.mat[0][0]-m.mat[2][2])>0.01) mavlib_matScaleWarn++;

  polyline = (MAV_polyline *) mav_malloc(sizeof(MAV_polyline));
  polyline->nlines = np;
  polyline->np= (int *) mav_malloc(polyline->nlines*sizeof(int));
  polyline->closed= (int *) mav_malloc(polyline->nlines*sizeof(int));
  polyline->vert= (MAV_vector **) mav_malloc(polyline->nlines*sizeof(MAV_vector *));
  polyline->sp= (MAV_surfaceParams **) mav_malloc(polyline->nlines*sizeof(MAV_surfaceParams *));
  polyline->matrix= m;

  idx=0;
  for (i=0; i<polyline->nlines; i++) {

    /* Count number of vertices in this line */
    nv=0;
    idxbk= idx;
    while (!(idx==n_getNCoordIndexes || coordIdx[idx]==-1)) {
      nv++;
      idx++;
    }

    polyline->closed[i] = MAV_FALSE;
    if (nv>=2)
    {
      /* Malloc of room for the vertices */
      polyline->np[i]= nv;
      polyline->vert[i]= (MAV_vector *) mav_malloc(polyline->np[i]*sizeof(MAV_vector));
      polyline->sp[i]= sp;

      /* Store the vertices */
      nv=0;
      idx= idxbk;
      while (!(idx==n_getNCoordIndexes || coordIdx[idx]==-1)) {
	
	/* Ensure that the index is valid */
	if (coordIdx[idx] >= pts_getNPoints) 
	{
	  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: coordinate index too big, setting to max\n");
	  v[0]= ptsIdx[(pts_getNPoints-1)*3+0];
	  v[1]= ptsIdx[(pts_getNPoints-1)*3+1];
	  v[2]= ptsIdx[(pts_getNPoints-1)*3+2];
	}
	else
	{
	  v[0]= ptsIdx[coordIdx[idx]*3+0];
	  v[1]= ptsIdx[coordIdx[idx]*3+1];
	  v[2]= ptsIdx[coordIdx[idx]*3+2];
	}

	polyline->vert[i][polyline->np[i]-1-nv].x= v[0];
	polyline->vert[i][polyline->np[i]-1-nv].y= v[1];
	polyline->vert[i][polyline->np[i]-1-nv].z= v[2];    

	nv++;
	idx++;
      }
    }    
    else
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: line with less than 2 vertices, ignoring\n");
      polyline->np[i]=0;
    }

    idx++;
  }

  delete coordIdx;
  delete ptsIdx;

  o= mav_objectNew(mav_class_polyline, polyline);

  return o;
}



/* Routine to build a material */

int mavlib_dealWithMaterialNode(MaterialNode *n)
{  
  float ambientInt, diffuse[3], spec[3], emis[3], shin, trans;
  int i;

  /* Get definition of material */
  ambientInt= n->getAmbientIntensity();
  n->getDiffuseColor(diffuse);
  n->getSpecularColor(spec);
  n->getEmissiveColor(emis);
  shin= n->getShininess();
  trans= n->getTransparency();

  /* Try to find a matching index */
  i= mav_paletteMaterialIndexMatchGet(mav_palette_default,
			   diffuse[0]*ambientInt, diffuse[1]*ambientInt, diffuse[2]*ambientInt, 1.0-trans, 
			   diffuse[0], diffuse[1], diffuse[2], 1.0-trans, spec[0], spec[1], spec[2], 1.0-trans, 
			   emis[0], emis[1], emis[2], 1.0-trans, shin*128.0);
  
  if (i<0) 
  {
    /* No joy, find empty material index to use */
    i= mav_paletteMaterialIndexEmptyGet(mav_palette_default);

    if (i>=0) 
    {
      /* Set this material */
      mav_paletteMaterialSet(mav_palette_default, i,
			   diffuse[0]*ambientInt, diffuse[1]*ambientInt, diffuse[2]*ambientInt, 1.0-trans, 
			   diffuse[0], diffuse[1], diffuse[2], 1.0-trans, spec[0], spec[1], spec[2], 1.0-trans, 
			   emis[0], emis[1], emis[2], 1.0-trans, shin*128.0);

    }
    else
    {
      i=1;
    }
  }

  return i; 
}



/* Routine to build a texture */

int mavlib_dealWithImageTextureNode(ImageTextureNode *n)
{  
  char *filename;
  int i=2;
  
  if (n->getNUrls()==1)
  {
    /* Get filename */
    filename= n->getUrl(0);

    /* Try to find a matching index */
    i= mav_paletteTextureIndexMatchGet(mav_palette_default, filename);

    if (i<0) 
    {
      /* No joy, find empty texture index to use */
      i= mav_paletteTextureIndexEmptyGet(mav_palette_default);

      if (i>=0) 
      {
	/* Set this texture */
	if (mav_paletteTextureSet(mav_palette_default, i, filename)==MAV_FALSE) i=2;
      }
      else
      {
	i=2;
      }
    }
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: %i texture image URLs - dont know how to handle anything other than one\n", n->getNUrls());
  }

  return i;
}



/* Routine to build a texture */

int mavlib_dealWithPixelTextureNode(PixelTextureNode *n)
{  
  int i=2, j, width, height, comp;
  unsigned char *c;
  int r=0, g=0, b=0, a=0;

  width= n->getImageNext();
  height= n->getImageNext();
  comp= n->getImageNext();

  if (comp==3 || comp==4)
  {
    if (width*height>0)
    {
      /* allocate memory and read texture */
      unsigned long *mem= (unsigned long *) mav_malloc(width*height*sizeof(unsigned long));

      for (j=0; j<width*height; j++) {
	mem[j]= n->getImageNext();
	
	if (comp==3)
	{
	  r= (mem[j] & 0xFF0000)>>16;
	  g= (mem[j] & 0x00FF00)>>8;
	  b= (mem[j] & 0x0000FF);
	  a= 255;
	}
	else if (comp==4)
	{
	  r= (mem[j] & 0xFF000000)>>24;
	  g= (mem[j] & 0x00FF0000)>>16;
	  b= (mem[j] & 0x0000FF00)>>8;
	  a= (mem[j] & 0x000000FF);
	}

	c= (unsigned char *) &mem[j];

#ifdef WIN32
	/* Textures have RGBA ordering */
	c[0]= r;
	c[1]= g;
	c[2]= b;
	c[3]= a;
#else
	/* Textures have ABGR ordering */
	c[0]= a;
	c[1]= b;
	c[2]= g;
	c[3]= r;
#endif
      }

      /* find empty texture index to use */
      i= mav_paletteTextureIndexEmptyGet(mav_palette_default);

      if (i>=0) 
      {
	/* Set this texture */
	if (mav_paletteTextureSetFromMem(mav_palette_default, i, width, height, mem)==MAV_FALSE) i=2;
      }
      else
      {
	i=2;
      }
    }
    else
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: pixel image has zero size\n");
    }
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: only understand 3 and 4 component pixel images\n");
  }

  return i;
}



/* Routine to deal with texture transforms */

MAV_matrix mavlib_dealWithTextureTransformNode(TextureTransformNode *n)
{
  MAV_matrix m, trans, cent, rot, scale, mcent;
  float c[2], rt, sc[2], tx[2];

  /* Get the transform definition */
  n->getCenter(c);
  rt= n->getRotation();
  n->getScale(sc);
  n->getTranslation(tx);

  /* Load up the matrices (See VRML97 texture transform node spec) */
  trans= mav_matrixSet(0,0,0, tx[0], tx[1], 0.0);
  cent= mav_matrixSet(0,0,0, c[0], c[1], 0.0);
  rot= mav_matrixQuaternionConvert(mav_quaternionSet(mav_vectorSet(0, 0, 1), MAV_RAD2DEG(rt)));
  scale= MAV_ID_MATRIX;
  scale.mat[0][0]= sc[0];
  scale.mat[1][1]= sc[1];
  mcent= mav_matrixSet(0,0,0, -c[0], -c[1], 0.0);

  m= mcent;
  m= mav_matrixMult(m, scale);
  m= mav_matrixMult(m, rot);
  m= mav_matrixMult(m, cent);
  m= mav_matrixMult(m, trans);

  return m;
}



/* Routine to deal with an appearance node */

MAV_surfaceParams *mavlib_dealWithAppearanceNode(AppearanceNode *n, MAV_matrix *texTran)
{
  MaterialNode *mn= n->getMaterialNodes();
  ImageTextureNode *tn= n->getImageTextureNodes();
  PixelTextureNode *pn= n->getPixelTextureNodes();
  TextureTransformNode *ttn= n->getTextureTransformNodes();
  MAV_surfaceParams *rv= mav_sp_default;
  int midx=-1, tidx=-1;

  /* Deal with material node if it exists */
  if (mn) midx= mavlib_dealWithMaterialNode(mn);

  /* Deal with image texture node if it exists */
  if (tn) tidx= mavlib_dealWithImageTextureNode(tn);

  /* Deal with pixel texture node if it exists */
  if (pn)  tidx= mavlib_dealWithPixelTextureNode(pn);

  /* Deal with texture transform node if it exists */
  if (ttn) *texTran= mavlib_dealWithTextureTransformNode(ttn);

  /* Create surface params */
  if (mn && (tn || pn)) 
  {
    rv= mav_surfaceParamsNew(MAV_LIT_TEXTURE, 0, midx, tidx);
  }
  else if (mn)
  {
    rv= mav_surfaceParamsNew(MAV_MATERIAL, 0, midx, 0);
  }
  else if (tn || pn)
  {
    rv= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, tidx);
  }

  return rv;
}



/* Routine to deal with a shape node (i.e. an appearance and a geometric object) */

MAV_object *mavlib_dealWithShapeNode(ShapeNode *n, MAV_matrix m)
{
  AppearanceNode *an= n->getAppearanceNodes();
  Node *gn= n->getGeometryNode();
  MAV_surfaceParams *sp= mav_sp_default;
  MAV_object *rv= NULL;
  MAV_matrix texTran;

  texTran= MAV_ID_MATRIX;

  /* Deal with appearance node if it exists */
  if (an)
  {
    sp= mavlib_dealWithAppearanceNode(an, &texTran);
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: no appearance node defined in shape - dont know if this is significant\n");
  }

  /* Deal with geometric shape */
  if (gn)
  {
    if (gn->isIndexedFaceSetNode()) 
    {
      rv= mavlib_dealWithIndexedFaceSetNode(mavlib_castToIndexedFaceSetNode(gn), sp, m, texTran);
    } 
    else if (gn->isIndexedLineSetNode()) 
    {
      rv= mavlib_dealWithIndexedLineSetNode(mavlib_castToIndexedLineSetNode(gn), sp, m);
    }
    else if (gn->isBoxNode()) 
    {
      rv= mavlib_dealWithBoxNode(mavlib_castToBoxNode(gn), sp, m);
    } 
    else if (gn->isSphereNode()) 
    {
      rv= mavlib_dealWithSphereNode(mavlib_castToSphereNode(gn), sp, m);
    } 
    else if (gn->isCylinderNode()) 
    {
      rv= mavlib_dealWithCylinderNode(mavlib_castToCylinderNode(gn), sp, m);
    } 
    else if (gn->isConeNode()) 
    {
      rv= mavlib_dealWithConeNode(mavlib_castToConeNode(gn), sp, m);
    } 
    else 
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: geometry node type %s not supported yet, ignoring\n", gn->getType());
    }
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: no geometry node defined in shape - dont know if this is significant\n");
  }

  return rv;
}



/* Routine to deal with a transform node (i.e. a matrix definition) */

MAV_matrix mavlib_dealWithTransformNode(TransformNode *n)
{
  MAV_matrix m, trans, cent, rot, scaleOrient, scale, mscaleOrient, mcent;
  float c[3], rt[4], sc[3], sco[4], tx[4];

  /* Get the transform definition */
  n->getCenter(c);
  n->getRotation(rt);
  n->getScale(sc);
  n->getScaleOrientation(sco);
  n->getTranslation(tx);

  /* Load up the matrices (See VRML97 transform node spec) */
  trans= mav_matrixSet(0,0,0, tx[0], tx[1], tx[2]);
  cent= mav_matrixSet(0,0,0, c[0], c[1], c[2]);
  rot= mav_matrixQuaternionConvert(mav_quaternionSet(mav_vectorSet(rt[0], rt[1], rt[2]), MAV_RAD2DEG(rt[3])));
  scaleOrient= mav_matrixQuaternionConvert(mav_quaternionSet(mav_vectorSet(sco[0], sco[1], sco[2]), MAV_RAD2DEG(sco[3])));
  scale= MAV_ID_MATRIX;
  scale.mat[0][0]= sc[0];
  scale.mat[1][1]= sc[1];
  scale.mat[2][2]= sc[2];
  mscaleOrient= mav_matrixInverse(scaleOrient);
  mcent= mav_matrixInverse(cent);

  /* Construct the overall matrix */
  m= trans;
  m= mav_matrixMult(m, cent);
  m= mav_matrixMult(m, rot);
  m= mav_matrixMult(m, scaleOrient);
  m= mav_matrixMult(m, scale);
  m= mav_matrixMult(m, mscaleOrient);
  m= mav_matrixMult(m, mcent);

  return m;
}



/* Routine to deal with an inline node (i.e. another vrml model) */
 
MAV_object *mavlib_dealWithInlineNode(InlineNode *n, MAV_matrix m)
{
  char *filename;
  MAV_object *rv= NULL;
 
  if (n->getNUrls()==1)
  {
    MAV_composite *c= (MAV_composite *) mav_malloc(sizeof(MAV_composite));
 
    /* Get filename */
    filename= n->getUrl(0);

    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\rBuilding objects... Object %i - inline with filename %s                  \n", mavlib_nodeObjCount, filename); 
 
    /* Take a backup of the globals */
    MAV_SMS *mavlibBK_nodeObj= mavlib_nodeObj;
    int mavlibBK_nodeObjCount= mavlib_nodeObjCount;
    int mavlibBK_matScaleWarn= mavlib_matScaleWarn;
    int mavlibBK_vrml97BoxCount= mavlib_vrml97BoxCount;
    int mavlibBK_vrml97CylinderCount= mavlib_vrml97CylinderCount;
    int mavlibBK_vrml97ConeCount= mavlib_vrml97ConeCount;
    int mavlibBK_vrml97SphereCount= mavlib_vrml97SphereCount;
    int mavlibBK_vrml97IFSCount= mavlib_vrml97IFSCount;
    int mavlibBK_vrml97IFSFaceCount= mavlib_vrml97IFSFaceCount;
    int mavlibBK_vrml97IFSVertCount= mavlib_vrml97IFSVertCount;
    int mavlibBK_vrml97ILSCount= mavlib_vrml97ILSCount;
    int mavlibBK_vrml97ILSLineCount= mavlib_vrml97ILSLineCount;
    int mavlibBK_vrml97ILSVertCount= mavlib_vrml97ILSVertCount;
    int mavlibBK_vrml97InlineCount= mavlib_vrml97InlineCount;

    if (mav_compositeRead(filename, c, m)) rv= mav_objectNew(mav_class_composite, c);

    /* Restore globals */
    mavlib_nodeObj= mavlibBK_nodeObj;
    mavlib_nodeObjCount= mavlibBK_nodeObjCount;
    mavlib_matScaleWarn= mavlibBK_matScaleWarn;
    mavlib_vrml97BoxCount= mavlibBK_vrml97BoxCount;
    mavlib_vrml97CylinderCount= mavlibBK_vrml97CylinderCount;
    mavlib_vrml97ConeCount= mavlibBK_vrml97ConeCount;
    mavlib_vrml97SphereCount= mavlibBK_vrml97SphereCount;
    mavlib_vrml97IFSCount= mavlibBK_vrml97IFSCount;
    mavlib_vrml97IFSFaceCount= mavlibBK_vrml97IFSFaceCount;
    mavlib_vrml97IFSVertCount= mavlibBK_vrml97IFSVertCount;
    mavlib_vrml97ILSCount= mavlibBK_vrml97ILSCount;
    mavlib_vrml97ILSLineCount= mavlibBK_vrml97ILSLineCount;
    mavlib_vrml97ILSVertCount= mavlibBK_vrml97ILSVertCount;
    mavlib_vrml97InlineCount= mavlibBK_vrml97InlineCount;

    if (rv) mavlib_vrml97InlineCount++;
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: %i inline node URLs - dont know how to handle anything other than one\n", n->getNUrls());
    rv= NULL;
  }
  
  return rv;
}



/* Routine to deal with a node */

MAV_object *mavlib_dealWithNode(Node *n, int depth, MAV_matrix matin, MAV_matrix *matout)
{
  MAV_object *o= NULL;
  MAV_matrix newm;

  //  printf("[depth %i] [name %s] [type %s] [mat %f %f %f %f\n", depth, n->getName(), n->getType());

  /* Output matrix is same as input matrix (may be changed lated by transform node) */
  *matout= matin;

  /* Only concerned with shape, tranform and inline nodes */
  if (n->isShapeNode()) 
  {
    o= mavlib_dealWithShapeNode(mavlib_castToShapeNode(n), matin);
  }
  else if (n->isTransformNode())
  {
    newm= mavlib_dealWithTransformNode(mavlib_castToTransformNode(n));
    *matout= mav_matrixMult(matin, newm);
  }
  else if (n->isInlineNode())
  {
    o= mavlib_dealWithInlineNode(mavlib_castToInlineNode(n), matin);
  }

  return o;
}



/* Routine to traverse and parse nodes and their children */

int mavlib_parseNode(Node *n, int depth, MAV_matrix m, int lod)
{
  MAV_object *o;
  MAV_matrix matout;
  int rv;

  while (n) {
    /* Deal with this node */
    o= mavlib_dealWithNode(n, depth, m, &matout);

    /* Store the object if it generated one */
    if (o) {
      mav_SMSObjectAdd(mavlib_nodeObj, o);
      mavlib_nodeObjCount++;

      if (lod) {
	if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\nWarning: Using only highest LOD level, ignoring nodes at other levels\n");
	return MAV_TRUE;
      }
    }

    /* Parse its children */
    rv= mavlib_parseNode(n->getChildNodes(), depth+1, matout, lod || n->isLodNode());

    /* Return up to LOD node if an object was present in the child and we're in a LOD node (or descendent) */
    if (lod && rv) return MAV_TRUE;

    /* Move onto next node */
    n= n->next();
  }

  return MAV_FALSE;
}



/* Routine to read a VRML97 file and generate a composite for it */
int mavlib_vrml97Inc;

void mavlib_VRML97LineCount(int nl, void *fn)
{  
  if (mav_opt_output==MAV_VERBOSE && !(nl%mavlib_vrml97Inc)) {
    fprintf(stderr, "\rParsing VRML97 file %s line %i", (char *) fn, nl);
    if (nl==mavlib_vrml97Inc*10) mavlib_vrml97Inc*=10;
  }
}

int mav_compositeReadVRML97(char *filename, MAV_composite *c, MAV_matrix m)
{
  SceneGraph *sceneGraph= new SceneGraph;
  MAV_SMSObj *smsobj;
  int otbk;

  /* Initialise composite */
  c->numobj=0;

  mavlib_nodeObj= mav_SMSObjListNew();
  mavlib_vrml97BoxCount=0;
  mavlib_vrml97CylinderCount=0;
  mavlib_vrml97ConeCount=0;
  mavlib_vrml97SphereCount=0;
  mavlib_vrml97IFSCount=0;
  mavlib_vrml97IFSFaceCount=0;
  mavlib_vrml97IFSVertCount=0;
  mavlib_vrml97ILSCount=0;
  mavlib_vrml97ILSLineCount=0;
  mavlib_vrml97ILSVertCount=0;
  mavlib_vrml97InlineCount=0;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Parsing VRML97 file %s... ", filename);

  /* Construct scene graph */
  mavlib_vrml97Inc=1;
  sceneGraph->load(filename, false, mavlib_VRML97LineCount, (void *) filename);

  if (!sceneGraph->isOK()) {
    if (mav_opt_output==MAV_VERBOSE && sceneGraph->getErrorLineNumber()>0) fprintf(stderr, "\nError reading VRML97 file: reason [%s], line %d [%s], token [%s]\n", sceneGraph->getErrorReason(), sceneGraph->getErrorLineNumber(), sceneGraph->getErrorLineString(), sceneGraph->getErrorToken());
    return MAV_FALSE;
  }

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "\rParsing VRML97 file %s...Parsed OK\nBuilding objects...", filename);

  /* create objects without table entries */
  otbk= mav_opt_objectTables;
  mav_opt_objectTables= MAV_FALSE;

  /* Parse nodes and build objects */
  mavlib_nodeObjCount= 0;
  mavlib_matScaleWarn= 0;
  mavlib_parseNode(sceneGraph->getNodes(), 0, m, 0);

  /* Print summary */
  if (mav_opt_output==MAV_VERBOSE) {
    fprintf(stderr, "\rBuilding objects... Built OK                                                  \nModel contains %i objects: \n", mavlib_nodeObjCount);
    if (mavlib_vrml97BoxCount) fprintf(stderr, " %i boxes\n", mavlib_vrml97BoxCount);
    if (mavlib_vrml97CylinderCount) fprintf(stderr, " %i cylinders\n", mavlib_vrml97CylinderCount);
    if (mavlib_vrml97ConeCount) fprintf(stderr, " %i cones\n", mavlib_vrml97ConeCount);
    if (mavlib_vrml97SphereCount) fprintf(stderr, " %i spheres\n", mavlib_vrml97SphereCount);
    if (mavlib_vrml97IFSCount) fprintf(stderr, " %i indexed face sets totaling %i faces and %i vertices\n", mavlib_vrml97IFSCount, mavlib_vrml97IFSFaceCount, mavlib_vrml97IFSVertCount);   
    if (mavlib_vrml97ILSCount) fprintf(stderr, " %i indexed line sets totaling %i lines and %i vertices\n", mavlib_vrml97ILSCount, mavlib_vrml97ILSLineCount, mavlib_vrml97ILSVertCount);   
    if (mavlib_vrml97InlineCount) fprintf(stderr, " %i inlined objects\n", mavlib_vrml97InlineCount);   
    if (mavlib_matScaleWarn) fprintf(stderr, "Warning: %i objects had non-uniformly scaled matrix, this may lead to intersection problems\n", mavlib_matScaleWarn);
  }

  /* restore object table value */
  mav_opt_objectTables= otbk;

  /* Make an SMS object to store the model */
  smsobj= (MAV_SMSObj *) mav_malloc(sizeof(MAV_SMSObj));
  smsobj->matrix= MAV_ID_MATRIX;

  if (mavlib_nodeObjCount<mav_opt_VRML97HBBThreshold)
  {
    /* Use an obj list SMS */
    smsobj->sms= mavlib_nodeObj;
  }
  else
  {
    /* Use a HBB SMS - construct from obj list SMS used in parsing */
    smsobj->sms= mav_SMSHBBNew();
    mav_HBBConstructFromSMS(smsobj->sms, mavlib_nodeObj);

    /* Delete SMS used in parsing, but not the objects it contains */
    mav_SMSDelete(mavlib_nodeObj, 0);
  }

  /* Make the SMS unselectable - selection must come via the MAV_SMSObj */
  mav_SMSSelectabilitySet(smsobj->sms, mav_win_all, MAV_FALSE);

  /* Build composite and add the SMS object */
  c->numobj= 1;
  c->obj= (MAV_object **) mav_malloc(c->numobj*sizeof(MAV_object *));
  c->obj[0]= mav_objectNew(mav_class_SMSObj, smsobj);
  if (mav_opt_compositeSetMatrix) c->matrix= MAV_ID_MATRIX;
  c->filename= strdup(filename);

  /* Calculate and store its BB */
  mav_compositeCalcBB(c);

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Finished parsing VRML97 file %s\n", filename);

  if (mav_opt_VRML97CleanUp) delete sceneGraph;

  return MAV_TRUE;
}



/* Routines to initialise the module */

char *mav_VRML97ModuleID(void)
{
  return "VRML97 (CyberVRML97)";
}

int mav_VRML97ModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_VRML97ModuleID);

  /* Add VRML97 to list of supported composite file formats */
  if (mav_compositeFormat[2].defined && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: file format 2 already reserved, overwriting\n"); 
  mav_compositeFormat[2].defined= MAV_TRUE;
  mav_compositeFormat[2].ext= ".wrl";
  mav_compositeFormat[2].fn= mav_compositeReadVRML97;

  return 1;
}
