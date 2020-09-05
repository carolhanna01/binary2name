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


#include "mavlib_objects.h"
#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif
#include <stdio.h>
#include <math.h>
#include <stdlib.h>



/* 
   Routines to parse a JIF file (now obsolete). The matrix of the individual objects 
   are pre-multipled by transform to allow the user to define the position, 
   orientation and scale of the composite.
*/

/* Routine to parse a matrix - common to all objects */

int mavlib_jif_warnflag=0;

void mavlib_jif_matscan(FILE *file, MAV_matrix *matrix, MAV_matrix transform)
{
  MAV_matrix tmp, ans;
  MAV_vector xvec, yvec, zvec;
  float xscale, yscale, zscale;
  int i, j;
  
/* Read matrix from file */
  
  fscanf(file, "%f %f %f %f", &tmp.mat[0][0], &tmp.mat[0][1], &tmp.mat[0][2], &tmp.mat[0][3]);
  fscanf(file, "%f %f %f %f", &tmp.mat[1][0], &tmp.mat[1][1], &tmp.mat[1][2], &tmp.mat[1][3]);
  fscanf(file, "%f %f %f %f", &tmp.mat[2][0], &tmp.mat[2][1], &tmp.mat[2][2], &tmp.mat[2][3]);
  fscanf(file, "%f %f %f %f", &tmp.mat[3][0], &tmp.mat[3][1], &tmp.mat[3][2], &tmp.mat[3][3]);

/* Warn if matrix is non-orthogonal and use BB for intersection test */

  xvec.x=tmp.mat[0][0];
  xvec.y=tmp.mat[1][0];
  xvec.z=tmp.mat[2][0];

  yvec.x=tmp.mat[0][1];
  yvec.y=tmp.mat[1][1];
  yvec.z=tmp.mat[2][1];

  zvec.x=tmp.mat[0][2];
  zvec.y=tmp.mat[1][2];
  zvec.z=tmp.mat[2][2];

  xscale=mav_vectorMag(xvec);
  yscale=mav_vectorMag(yvec);
  zscale=mav_vectorMag(zvec);

  if (!mavlib_jif_warnflag && (fabs(xscale-yscale)/xscale > 0.01 || fabs(xscale-zscale)/xscale > 0.01)) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Using bounding box for composite intersection test\n");
    mav_callbackIntersectSet(mav_win_all, mav_class_composite, NULL);
    mavlib_jif_warnflag=1;
  }

/* Pre-multiply objects matrix with the transformation matrix */

  ans=mav_matrixMult(transform, tmp);

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      matrix->mat[i][j]=ans.mat[i][j];
    }
  }
}

void mavlib_jif_matscannt(FILE *file, MAV_matrix *matrix)
{
  MAV_vector xvec, yvec, zvec;
  float xscale, yscale, zscale;
  
/* Read matrix from file */
  
  fscanf(file, "%f %f %f %f", &matrix->mat[0][0], &matrix->mat[0][1], &matrix->mat[0][2], &matrix->mat[0][3]);
  fscanf(file, "%f %f %f %f", &matrix->mat[1][0], &matrix->mat[1][1], &matrix->mat[1][2], &matrix->mat[1][3]);
  fscanf(file, "%f %f %f %f", &matrix->mat[2][0], &matrix->mat[2][1], &matrix->mat[2][2], &matrix->mat[2][3]);
  fscanf(file, "%f %f %f %f", &matrix->mat[3][0], &matrix->mat[3][1], &matrix->mat[3][2], &matrix->mat[3][3]);

/* Warn if matrix is non-orthogonal and use BB for intersection test */

  xvec.x=matrix->mat[0][0];
  xvec.y=matrix->mat[1][0];
  xvec.z=matrix->mat[2][0];

  yvec.x=matrix->mat[0][1];
  yvec.y=matrix->mat[1][1];
  yvec.z=matrix->mat[2][1];

  zvec.x=matrix->mat[0][2];
  zvec.y=matrix->mat[1][2];
  zvec.z=matrix->mat[2][2];

  xscale=mav_vectorMag(xvec);
  yscale=mav_vectorMag(yvec);
  zscale=mav_vectorMag(zvec);

  if (!mavlib_jif_warnflag && (fabs(xscale-yscale)/xscale > 0.01 || fabs(xscale-zscale)/xscale > 0.01)) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Using bounding box for composite intersection test\n");
    mav_callbackIntersectSet(mav_win_all, mav_class_composite, NULL);
    mavlib_jif_warnflag=1;
  }
}



/* Routine to find an unused place in the matlist of the active window */

int mavlib_jif_findplace(float *matdata)
{
  int i, j, flag;
  MAV_material mat;

  /* try to find an identical definition */

  for (i=0; i<mav_opt_maxMaterials; i++) {
    mat= mav_win_current->palette->matlist[i];
    if (mat.defined) {
      flag=1;
      for (j=0; j<3; j++) if (matdata[j]!=mat.ambient[j]) flag=0;
      for (j=0; j<3; j++) if (matdata[j+3]!=mat.diffuse[j]) flag=0;
      for (j=0; j<3; j++) if (matdata[j+6]!=mat.specular[j]) flag=0;
      if (matdata[9]!=mat.shine) flag=0;

      if (flag) return(i);
    }
  }

  for (i=0; i<mav_opt_maxMaterials; i++) {
    if (!mav_win_current->palette->matlist[i].defined) {
      mav_paletteMaterialSet(mav_palette_default, i, matdata[0], matdata[1], matdata[2], 1.0, matdata[3], matdata[4], matdata[5], 1.0, matdata[6], matdata[7], matdata[8], 1.0, 0.0, 0.0, 0.0, 0.0, matdata[9]);  
      return(i);
    }
  }

  return(-1);
}



/* Routine to find an unused place in the texlist of the active window */

int mavlib_jif_findtexplace(char *name)
{
  int i;
  
  for (i=1; i<mav_opt_maxTextures; i++) {
    if (!mav_win_current->palette->texlist[i].defined) {
      if (!mav_paletteTextureSet(mav_palette_default, i, name)) return(-2);
      return(i);
    }
  }

  return(-1);
}



/* Routine to read a JIF object from file */

int mav_compositeReadJIF(char *filename, MAV_composite *comp, MAV_matrix transform)
{
  FILE *file;
  float matdata[10];
  char tmp[100], name[100];
  int i, j, k, idata, no, nocols, nomats, notexs, *colindex, *texindex;
  char pname[100], *pptr;

  colindex= mav_malloc(mav_opt_maxMaterials*sizeof(int));
  texindex= mav_malloc(mav_opt_maxTextures*sizeof(int));

  comp->filename= strdup(filename);

/* Get the pathname */

  strcpy(pname, filename);
  pptr=strrchr(pname, '/');
  if (pptr) *(pptr+1)=0;

/* Open the file */

  file=fopen(filename, "r");
  if (file==NULL) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: can not open composite object file %s\n", filename);
    mav_free(colindex);
    mav_free(texindex);
    return(MAV_FALSE);
  }

/*
  Read in the colour definition.
*/

  fscanf(file, "%i", &nocols);

  if (nocols!=0) {
    fprintf(stderr, "Error: can not deal with colours\n");
    mav_free(colindex);
    mav_free(texindex);
    return(MAV_FALSE);
  }
  
/*
  Read in the material definition. For each material read, find an unused
  place for it in the matlist for the active window. Error if all materials
  are used. May want to be smart and check if the read in material has
  already been defined elsewhere.
*/

  fscanf(file, "%i", &nomats);
  
  for (i=0; i<nomats; i++) {
    fscanf(file, "%f %f %f %f %f %f %f %f %f %f", &matdata[0], &matdata[1], &matdata[2], &matdata[3], &matdata[4], &matdata[5], &matdata[6], &matdata[7], &matdata[8], &matdata[9]);
    colindex[i]=mavlib_jif_findplace(matdata);
    if (colindex[i] == -1) {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: maximum number of materials exceeded.\n");
      mav_free(colindex);
      mav_free(texindex);
      return(MAV_FALSE);
    }
  }

/*
  Read in the texture definition. For each texture read, find an unused
  place for it in the texlist for the active window. Error if all textures
  are used. May want to be smart and check if the read in texture has
  already been defined elsewhere. 
*/

  fscanf(file, "%i", &notexs);
  
  for (i=0; i<notexs; i++) {
    fscanf(file, "%s", tmp);

    /* Acount for optional path */

    if (pptr)
    {
      sprintf(name, "%s%s", pname, tmp);
    }
    else
    {
      strcpy(name, tmp);
    }

    texindex[i]=mavlib_jif_findtexplace(name);
    if (texindex[i] == -1) {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: maximum number of textures exceeded.\n");
      mav_free(colindex);
      mav_free(texindex);
      return(MAV_FALSE);
    }
    if (texindex[i]<0) {
      mav_free(colindex);
      mav_free(texindex);
      return(MAV_FALSE);
    }
  }

/* Read in objects */

  comp->numobj=0;
  fscanf(file, "%i", &no);
  comp->obj=mav_malloc(no*sizeof(MAV_object *));

  for (i=0; i<no; i++) {
    fscanf(file, "%s", name);

    if (!strcmp(name, "box")) {
      fprintf(stderr, "Warning: boxes are no longer supported in JIF\n");
    }

    if (!strcmp(name, "pyramid")) {
      fprintf(stderr, "Warning: pyramids are no longer supported in JIF\n");
    }

    if (!strcmp(name, "cylinder")) {
      MAV_cylinder *cyl;

      cyl=mav_malloc(sizeof(MAV_cylinder));

      fscanf(file, "%f", &cyl->radius);
      fscanf(file, "%f", &cyl->height);
      fscanf(file, "%i", &cyl->nverts);
      fscanf(file, "%i", &cyl->endcap);
      fscanf(file, "%i", &idata);      
      fscanf(file, "%i", &idata);
      cyl->sp->mode= MAV_MATERIAL;
      cyl->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &cyl->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_cylinder, (void *) cyl);
    }

    if (!strcmp(name, "cone")) {
      MAV_cone *cone;

      cone=mav_malloc(sizeof(MAV_cone));

      fscanf(file, "%f", &cone->rt);
      fscanf(file, "%f", &cone->rb);
      fscanf(file, "%f", &cone->height);
      fscanf(file, "%i", &cone->nverts);
      fscanf(file, "%i", &cone->endcap);
      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      cone->sp->mode= MAV_MATERIAL;
      cone->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &cone->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_cone, (void *) cone);
    }

    if (!strcmp(name, "sphere")) {
      MAV_sphere *sph;

      sph=mav_malloc(sizeof(MAV_sphere));

      fscanf(file, "%f", &sph->radius);
      fscanf(file, "%i", &sph->nverts);
      fscanf(file, "%i", &sph->nchips);
      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      sph->sp->mode=MAV_MATERIAL;
      sph->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &sph->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_sphere, (void *) sph);
    }

    if (!strcmp(name, "hsphere")) {
      MAV_hsphere *hsph;

      hsph=mav_malloc(sizeof(MAV_hsphere));

      fscanf(file, "%f", &hsph->radius);
      fscanf(file, "%i", &hsph->nverts);
      fscanf(file, "%i", &hsph->nchips);
      fscanf(file, "%i", &hsph->endcap);
      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      hsph->sp->mode=MAV_MATERIAL;
      hsph->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &hsph->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_hsphere, (void *) hsph);
    }

    if (!strcmp(name, "ellipse")) {
      MAV_ellipse *elip;

      elip=mav_malloc(sizeof(MAV_ellipse));

      fscanf(file, "%f", &elip->radius);
      fscanf(file, "%f", &elip->height);
      fscanf(file, "%i", &elip->nverts);
      fscanf(file, "%i", &elip->nchips);
      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      elip->sp->mode=MAV_MATERIAL;
      elip->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &elip->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_ellipse, (void *) elip);
    }

    if (!strcmp(name, "hellipse")) {
      MAV_hellipse *helip;

      helip=mav_malloc(sizeof(MAV_hellipse));

      fscanf(file, "%f", &helip->radius);
      fscanf(file, "%f", &helip->height);
      fscanf(file, "%i", &helip->nverts);
      fscanf(file, "%i", &helip->nchips);
      fscanf(file, "%i", &helip->endcap);
      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      helip->sp->mode=MAV_MATERIAL;
      helip->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &helip->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_hellipse, (void *) helip);
    }

    if (!strcmp(name, "ctorus")) {
      MAV_ctorus *ct;

      ct=mav_malloc(sizeof(MAV_ctorus));

      fscanf(file, "%f", &ct->rmajor);
      fscanf(file, "%f", &ct->rminor);
      fscanf(file, "%f", &ct->angle);
      fscanf(file, "%i", &ct->nverts);
      fscanf(file, "%i", &ct->nchips);
      fscanf(file, "%i", &ct->endcap);
      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      ct->sp->mode=MAV_MATERIAL;
      ct->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &ct->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_ctorus, (void *) ct);
    }

    if (!strcmp(name, "rtorus")) {
      MAV_rtorus *rt;

      rt=mav_malloc(sizeof(MAV_rtorus));

      fscanf(file, "%f", &rt->radius);
      fscanf(file, "%f", &rt->width);
      fscanf(file, "%f", &rt->height);
      fscanf(file, "%f", &rt->angle);
      fscanf(file, "%i", &rt->nchips);
      fscanf(file, "%i", &rt->endcap);
      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      rt->sp->mode=MAV_MATERIAL;
      rt->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &rt->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_rtorus, (void *) rt);
    }

    if (!strcmp(name, "polygon")) {
      MAV_polygon *apoly;

      apoly=mav_malloc(sizeof(MAV_polygon));

      fscanf(file, "%i", &apoly->np);
      fscanf(file, "%f %f %f", &apoly->norm.x, &apoly->norm.y, &apoly->norm.z);

      apoly->vert=mav_malloc(apoly->np*sizeof(MAV_vector));
      
      for (j=0; j<apoly->np; j++) {
	fscanf(file, "%f %f %f", &apoly->vert[j].x, &apoly->vert[j].y, &apoly->vert[j].z);
      }

      fscanf(file, "%i", &idata);
      fscanf(file, "%i", &idata);
      apoly->sp->mode=MAV_MATERIAL;
      apoly->sp->material=colindex[idata-1];
      mavlib_jif_matscan(file, &apoly->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_polygon, (void *) apoly);
    }

    if (!strcmp(name, "polygongrp")) {
      MAV_polygonGrp *polygrp;

      polygrp=mav_malloc(sizeof(MAV_polygonGrp));

      fscanf(file, "%i", &polygrp->npolys);

      polygrp->np=mav_malloc(polygrp->npolys*sizeof(int));
      polygrp->norm=mav_malloc(polygrp->npolys*sizeof(MAV_vector));
      polygrp->vert=mav_malloc(polygrp->npolys*sizeof(MAV_vector *));
      polygrp->sp=mav_malloc(polygrp->npolys*sizeof(MAV_surfaceParams));

      for (j=0; j<polygrp->npolys; j++) {
	fscanf(file, "%i", &polygrp->np[j]);
	fscanf(file, "%f %f %f", &polygrp->norm[j].x, &polygrp->norm[j].y, &polygrp->norm[j].z);

	polygrp->vert[j]=mav_malloc(polygrp->np[j]*sizeof(MAV_vector));
      
	for (k=0; k<polygrp->np[j]; k++) {
	  fscanf(file, "%f %f %f", &polygrp->vert[j][k].x, &polygrp->vert[j][k].y, &polygrp->vert[j][k].z);
	}

	fscanf(file, "%i", &idata);
	fscanf(file, "%i", &idata);
	polygrp->sp[j]->mode=MAV_MATERIAL;
	polygrp->sp[j]->material=colindex[idata-1];
      }

      mavlib_jif_matscan(file, &polygrp->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_polygonGrp, (void *) polygrp);
    }

    if (!strcmp(name, "facet")) {
      MAV_facet *facet;

      facet=mav_malloc(sizeof(MAV_facet));

      fscanf(file, "%i", &facet->npolys);
      
      facet->np=mav_malloc(facet->npolys*sizeof(int));
      facet->norm=mav_malloc(facet->npolys*sizeof(MAV_vector *));
      facet->vert=mav_malloc(facet->npolys*sizeof(MAV_vector *));
      facet->sp=mav_malloc(facet->npolys*sizeof(MAV_surfaceParams));
      
      for (j=0; j<facet->npolys; j++) {
	fscanf(file, "%i", &facet->np[j]);
	facet->norm[j]=mav_malloc(facet->np[j]*sizeof(MAV_vector));
	facet->vert[j]=mav_malloc(facet->np[j]*sizeof(MAV_vector));
	
	for (k=0; k<facet->np[j]; k++) {
	  fscanf(file, "%f %f %f", &facet->norm[j][k].x, &facet->norm[j][k].y, &facet->norm[j][k].z);
	  fscanf(file, "%f %f %f", &facet->vert[j][k].x, &facet->vert[j][k].y, &facet->vert[j][k].z);
	}

	fscanf(file, "%i", &idata);
	fscanf(file, "%i", &idata);
	facet->sp[j]->mode=MAV_MATERIAL;
	facet->sp[j]->material=colindex[idata-1];
      }

      mavlib_jif_matscan(file, &facet->matrix, transform);
      comp->obj[comp->numobj++]=mav_objectNew(mav_class_facet, (void *) facet);
    }

    if (!strcmp(name, "textureobj")) {
      fprintf(stderr, "Warning: textureobjs are no longer supported in JIF\n");
    }

    if (!strcmp(name, "texturerect")) {
      fprintf(stderr, "Warning: texturerects are no longer supported in JIF\n");
    }

    if (!strcmp(name, "text")) {
      fprintf(stderr, "Warning: text is no longer supported in JIF\n");
    }

    if (!strcmp(name, "polyline")) {
      fprintf(stderr, "Warning: polylines are no longer supported in JIF\n");
    }

    if (!strcmp(name, "vradObject")) {
      fprintf(stderr, "Warning: vradObjects are no longer supported in JIF\n");
    }

    if (!strcmp(name, "composite")) {
      MAV_composite *comp2;
      MAV_matrix transform2;
      char filename2[100];
      
      comp2=mav_malloc(sizeof(MAV_composite));

      fscanf(file, "%s", tmp);
      mavlib_jif_matscannt(file, &transform2);
      mavlib_jif_matscan(file, &comp2->matrix, transform);

      /* Acount for optional path */

      if (pptr)
      {
	sprintf(filename2, "%s%s", pname, tmp);
      }
      else
      {
	strcpy(filename2, tmp);
      }

      if (!mav_compositeReadJIF(filename2, comp2, transform2)) 
      {
	mav_free(colindex);
	mav_free(texindex);
	return(MAV_FALSE); /* Failed read */
      }
      else
      {
	comp->obj[comp->numobj++]=mav_objectNew(mav_class_composite, (void *) comp2);
      }
    }
  }

  fclose(file);

/* Calculate local BB */

  mav_compositeCalcBB(comp);
  if (mav_opt_compositeSetMatrix) comp->matrix= MAV_ID_MATRIX;

  mav_free(colindex);
  mav_free(texindex);
  return (MAV_TRUE);
}

