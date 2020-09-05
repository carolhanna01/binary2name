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



/* Routines to support AC3D files */

int mavlib_ac3d_matNo; 
int *mavlib_ac3d_matLookUp;
int *mavlib_ac3d_colLookUp;
FILE *mavlib_ac3d_file;
int mavlib_ac3d_source;
char *mavlib_ac3d_str;
int mavlib_ac3d_strCount;
int mavlib_ac3d_strLen;
int mavlib_ac3d_nl;

typedef struct {
  int numverts;
  MAV_vector *verts;
  MAV_vector *vertnorm;
  int numsurfs;
  int *surfprops;
  int *matindex;
  int *refs;
  int **index;
  MAV_texCoord **tex;
  MAV_vector *surfnorm;
  char texture[100];
  float xrep;
  float yrep;
} MAVLIB_AC3DObj;

typedef struct {
  int surfs, lines;
} MAVLIB_obj_info;


/* Routine to print an AC3D error/warning message */

void mavlib_ac3d_fprintf(char *s)
{
  /* Do we need to print a new line */
  if (!mavlib_ac3d_nl) {
    fprintf(stderr, "\n");
    mavlib_ac3d_nl=1;
  }

  /* Print error/warning message */
  fprintf(stderr, s);
}



/* Routine to return an empty or matching material index */

int mavlib_ac3d_findMatPlace(float *dif, float *amb, float *emis, float *spec, float shin, float trans)
{
  int i;

  trans=1.0-trans;

  /* check if material has already been defined */
  for (i=0; i<mav_opt_maxMaterials; i++) {
    if (mav_palette_default->matlist[i].defined) {
      if (mav_palette_default->matlist[i].ambient[0]==amb[0] &&
	  mav_palette_default->matlist[i].ambient[1]==amb[1] &&
	  mav_palette_default->matlist[i].ambient[2]==amb[2] &&
	  mav_palette_default->matlist[i].ambient[3]==trans &&
	  mav_palette_default->matlist[i].diffuse[0]==dif[0] &&
	  mav_palette_default->matlist[i].diffuse[1]==dif[1] &&
	  mav_palette_default->matlist[i].diffuse[2]==dif[2] &&
	  mav_palette_default->matlist[i].diffuse[3]==trans &&
	  mav_palette_default->matlist[i].emission[0]==emis[0] &&
	  mav_palette_default->matlist[i].emission[1]==emis[1] &&
	  mav_palette_default->matlist[i].emission[2]==emis[2] &&
	  mav_palette_default->matlist[i].emission[3]==trans &&
	  mav_palette_default->matlist[i].specular[0]==spec[0] &&
	  mav_palette_default->matlist[i].specular[1]==spec[1] &&
	  mav_palette_default->matlist[i].specular[2]==spec[2] &&
	  mav_palette_default->matlist[i].specular[3]==trans &&
	  mav_palette_default->matlist[i].shine==shin) return i;
    }
  }
  
  /* Not found, so need to define it - first find an unused material */
  for (i=0; i<mav_opt_maxMaterials; i++) {
    if (!mav_palette_default->matlist[i].defined) {
      mav_paletteMaterialSet(mav_palette_default, i, amb[0], amb[1], amb[2], trans, dif[0], dif[1], dif[2], trans, spec[0], spec[1], spec[2], trans, emis[0], emis[1], emis[2], trans, shin);
      return i;
    }
  }
  
  /* cant find empty slot */
  if (mav_opt_output==MAV_VERBOSE) mavlib_ac3d_fprintf("Error: maximum number of materials exceeded.\n");
  return -1;
}



/* Routine to return an empty or matching colour index */

int mavlib_ac3d_findColPlace(float *amb, float trans)
{
  int i;

  trans=1.0-trans;

  /* check if colour has already been defined */
  for (i=0; i<mav_opt_maxColours; i++) {
    if (mav_palette_default->collist[i].defined) {
      if (mav_palette_default->collist[i].colour[0]==amb[0] &&
	  mav_palette_default->collist[i].colour[1]==amb[1] &&
	  mav_palette_default->collist[i].colour[2]==amb[2] &&
	  mav_palette_default->collist[i].colour[3]==trans) return i;
    }
  }

  /* Not found, so need to define it - first find an unused colour */
  for (i=0; i<mav_opt_maxColours; i++) {
    if (!mav_palette_default->collist[i].defined) {
      mav_paletteColourSet(mav_palette_default, i, amb[0], amb[1], amb[2], trans);
      return i;
    }
  }

  /* cant find empty slot */
  if (mav_opt_output==MAV_VERBOSE) mavlib_ac3d_fprintf("Error: maximum number of colours exceeded.\n");
  return -1;
}



/* Routine to return an empty or matching texture index */

int mavlib_ac3d_texLookUp2(char *nm)
{
  int i;

  /* check if texture has already been defined */
  for (i=0; i<mav_opt_maxTextures; i++) {
    if (mav_palette_default->texlist[i].defined) {
      if (!strcmp(mav_palette_default->texlist[i].filename, nm)) return i;
    }
  }

  /* Not found, so need to define it - first find an unused texture */
  for (i=0; i<mav_opt_maxTextures; i++) {
    if (!mav_palette_default->texlist[i].defined) {
      if (mav_paletteTextureSet(mav_palette_default, i, nm)==MAV_FALSE) return -1;
      return i;
    }
  }
  
  /* cant find empty slot */
  return -2;
}

char *mavlib_ac3d_origFile;

int mavlib_ac3d_texLookUp(char *nm)
{
  char path[500], fn2[500];
  int i, optbk, rv;

  /* Silent output */
  optbk= mav_opt_output;
  mav_opt_output= MAV_SILENT;

  if (strstr(nm, "/"))
  { 
    /* Texture filename contains path */
    rv= mavlib_ac3d_texLookUp2(nm);
  }
  else
  {
    /* Texture filename is relative */

    /* Find path of AC3D model filename */
    strcpy(path, mavlib_ac3d_origFile);
    for (i=strlen(path)-1; i>=0 && path[i]!='/'; i--);
    if (i==-1) 
    {
      path[0]='.';
      path[1]=0;
    }
    else if (i==0) 
    {
      path[0]='/';
      path[1]=0;
    }
    else
    {
      path[i]=0;
    }
    
    /* Account for AC3D model path when looking for texture */
    sprintf(fn2, "%s/%s", path, nm);
    rv= mavlib_ac3d_texLookUp2(fn2);

    if (rv==-1) {
      /* Failed, so try looking in textures sub-directory */
      sprintf(fn2, "%s/textures/%s", path, nm);
      rv= mavlib_ac3d_texLookUp2(fn2);
    }
  }

  /* Restore output */
  mav_opt_output= optbk;

  /* Print error messages as appropriate */
  if (rv==-1 && mav_opt_output==MAV_VERBOSE) {
    sprintf(path, "Warning: failed to read texture %s, ignoring.\n", nm);
    mavlib_ac3d_fprintf(path);
  }

  if (rv==-2 && mav_opt_output==MAV_VERBOSE) mavlib_ac3d_fprintf("Error: maximum number of textures exceeded.\n");

  return rv;
}



/* Routine to parse tokens from file or memory buffer */

void mavlib_ac3d_parseString(char *str)
{
  int i;

  if (mavlib_ac3d_source)
  {
    fscanf(mavlib_ac3d_file, "%s", str);

    /* account for quoted strings */
    if (str[0]=='"') {

      /* parse until matching quote */
      while (str[strlen(str)-1]!='"') {
	str[strlen(str)+1]=0;
	fscanf(mavlib_ac3d_file, "%c", &str[strlen(str)]);
      }

      /* remove quotes */
      for (i=1; i<strlen(str)-1; i++) str[i-1]=str[i];
      str[i-1]=0;
    }
  }
  else
  {    
    sscanf(&mavlib_ac3d_str[mavlib_ac3d_strCount], "%s", str);
    mavlib_ac3d_strCount+=(strlen(str)+1);

    /* account for quoted strings */
    if (str[0]=='"') {

      /* parse until matching quote */
      while (str[strlen(str)-1]!='"') {
	str[strlen(str)+1]=0;

	sscanf(&mavlib_ac3d_str[mavlib_ac3d_strCount], "%c", &str[strlen(str)]);
	mavlib_ac3d_strCount++;
      }

      /* remove quotes */
      for (i=1; i<strlen(str)-1; i++) str[i-1]=str[i];
      str[i-1]=0;
    }
  }
}

void mavlib_ac3d_parseFloat(float *f)
{
  if (mavlib_ac3d_source)
  {
    fscanf(mavlib_ac3d_file, "%f", f);
  }
  else
  {
    char str[100];

    sscanf(&mavlib_ac3d_str[mavlib_ac3d_strCount], "%s", str);
    mavlib_ac3d_strCount+=(strlen(str)+1);

    *f=atof(str);
  }
}

void mavlib_ac3d_parseInt(int *i)
{
  if (mavlib_ac3d_source)
  {
    fscanf(mavlib_ac3d_file, "%i", i);
  }
  else
  {
    char str[100];

    sscanf(&mavlib_ac3d_str[mavlib_ac3d_strCount], "%s", str);
    mavlib_ac3d_strCount+=(strlen(str)+1);

    *i=atoi(str);
  }
}

void mavlib_ac3d_parseDummy(void)
{
  if (mavlib_ac3d_source)
  {
    fscanf(mavlib_ac3d_file, "%*s");
  }
  else
  {
    char str[100];

    sscanf(&mavlib_ac3d_str[mavlib_ac3d_strCount], "%s", str);
    mavlib_ac3d_strCount+=(strlen(str)+1);
  }
}

int mavlib_ac3d_parseOpen(char *s)
{
  if (mavlib_ac3d_source) 
  {
    mavlib_ac3d_file=fopen(s, "r");

    if (!mavlib_ac3d_file) 
    {
      return MAV_FALSE;
    }
    else
    {
      return MAV_TRUE;
    }
  }
  else
  {
    mavlib_ac3d_str= s;
    mavlib_ac3d_strCount= 0;
    mavlib_ac3d_strLen= strlen(mavlib_ac3d_str);

    return MAV_TRUE;
  }
}

void mavlib_ac3d_parseClose(void)
{
  if (mavlib_ac3d_source) fclose(mavlib_ac3d_file);
}

int mavlib_ac3d_parseEOF(void)
{
  if (mavlib_ac3d_source) 
  {
    return feof(mavlib_ac3d_file);
  }
  else
  {
    if (mavlib_ac3d_strCount>=mavlib_ac3d_strLen)
    {
      return 1;
    }
    else
    {
      return 0;
    }
  }
}



/* Routine to parse a material */

int mavlib_ac3d_parseMaterial(void)
{
  char str[100];
  float dif[3], amb[3], emis[3], spec[3], shin, trans;
  
  /* read material name */
  mavlib_ac3d_parseString(str);

  /* read material colours */
  mavlib_ac3d_parseDummy();
  mavlib_ac3d_parseFloat(&dif[0]);
  mavlib_ac3d_parseFloat(&dif[1]);
  mavlib_ac3d_parseFloat(&dif[2]);

  mavlib_ac3d_parseDummy();
  mavlib_ac3d_parseFloat(&amb[0]);
  mavlib_ac3d_parseFloat(&amb[1]);
  mavlib_ac3d_parseFloat(&amb[2]);

  mavlib_ac3d_parseDummy();
  mavlib_ac3d_parseFloat(&emis[0]);
  mavlib_ac3d_parseFloat(&emis[1]);
  mavlib_ac3d_parseFloat(&emis[2]);

  mavlib_ac3d_parseDummy();
  mavlib_ac3d_parseFloat(&spec[0]);
  mavlib_ac3d_parseFloat(&spec[1]);
  mavlib_ac3d_parseFloat(&spec[2]);

  mavlib_ac3d_parseDummy();
  mavlib_ac3d_parseFloat(&shin);
  mavlib_ac3d_parseDummy();
  mavlib_ac3d_parseFloat(&trans);

  /* give material an index taking into account duplicates enteries */
  mavlib_ac3d_matLookUp[mavlib_ac3d_matNo]= mavlib_ac3d_findMatPlace(dif, amb, emis, spec, shin, trans);

  /* give material a colour index taking into account duplicates enteries */
  mavlib_ac3d_colLookUp[mavlib_ac3d_matNo]= mavlib_ac3d_findColPlace(dif, trans);

  /* Check if there was room for the material */
  if (mavlib_ac3d_matLookUp[mavlib_ac3d_matNo]==-1) return -1;
  if (mavlib_ac3d_colLookUp[mavlib_ac3d_matNo]==-1) return -1;

  mavlib_ac3d_matNo++;

  return 0;
}



/* Routine to parse an object */

int mavlib_ac3d_parseObject(MAV_composite *c, MAV_list *oi_list, MAV_matrix mat)
{
  MAVLIB_AC3DObj *obj= mav_malloc(sizeof(MAVLIB_AC3DObj));
  MAV_facet *fc= NULL;
  MAV_polyline *pl= NULL;
  MAV_matrix omat=MAV_ID_MATRIX;
  MAV_vector v1, v2;
  char str[100], dum[100];
  int i, j, k, nk, found, facets;
  MAVLIB_obj_info *cur_oi;

  mavlib_ac3d_matNo=0;

  /* initialise AC3D object */
  obj->numverts=0;
  obj->numsurfs=0;
  obj->texture[0]=0;
  obj->xrep=1.0;
  obj->yrep=1.0;

  /* initialise facet and polyline objects */
  mav_listItemNext(oi_list, (void **)&cur_oi);
  facets= cur_oi->surfs - cur_oi->lines;

  if (facets) {
    fc= mav_malloc(sizeof(MAV_facet));
    fc->npolys= 0;
    fc->np= mav_malloc(facets*sizeof(int));
    fc->norm= mav_malloc(facets*sizeof(MAV_vector *));
    fc->tex= mav_malloc(facets*sizeof(MAV_texCoord *));
    fc->vert= mav_malloc(facets*sizeof(MAV_vector *));
    fc->sp= mav_malloc(facets*sizeof(MAV_surfaceParams *));
    fc->matrix= MAV_ID_MATRIX;

    c->obj[c->numobj++]= mav_objectNew(mav_class_facet, fc);
  }

  if (cur_oi->lines) {
    pl= mav_malloc(sizeof(MAV_polyline));
    pl->nlines=0;
    pl->np= mav_malloc(cur_oi->lines*sizeof(int));
    pl->closed= mav_malloc(cur_oi->lines*sizeof(int));
    pl->vert= mav_malloc(cur_oi->lines*sizeof(MAV_vector *));
    pl->sp= mav_malloc(cur_oi->lines*sizeof(MAV_surfaceParams *));
    pl->matrix= MAV_ID_MATRIX;

    c->obj[c->numobj++]= mav_objectNew(mav_class_polyline, pl);
  }

  /* read object type */
  mavlib_ac3d_parseString(str);

  /* parse optional stuff until we reach the kids */  
  do {
    mavlib_ac3d_parseString(str);

    /* name */
    if (!strcmp(str, "name")) mavlib_ac3d_parseString(dum);

    /* data */
    if (!strcmp(str, "data")) mavlib_ac3d_parseString(dum);

    /* texture */
    if (!strcmp(str, "texture")) mavlib_ac3d_parseString(obj->texture);

    /* texture repeat */
    if (!strcmp(str, "texrep")) {
      mavlib_ac3d_parseFloat(&obj->xrep);
      mavlib_ac3d_parseFloat(&obj->yrep);
    }

    /* rotation */
    if (!strcmp(str, "rot")) {
      mavlib_ac3d_parseFloat(&omat.mat[0][0]);
      mavlib_ac3d_parseFloat(&omat.mat[1][0]);
      mavlib_ac3d_parseFloat(&omat.mat[2][0]);

      mavlib_ac3d_parseFloat(&omat.mat[0][1]);
      mavlib_ac3d_parseFloat(&omat.mat[1][1]);
      mavlib_ac3d_parseFloat(&omat.mat[2][1]);

      mavlib_ac3d_parseFloat(&omat.mat[0][2]);
      mavlib_ac3d_parseFloat(&omat.mat[1][2]);
      mavlib_ac3d_parseFloat(&omat.mat[2][2]);
    }

    /* location */
    if (!strcmp(str, "loc")) {
      mavlib_ac3d_parseFloat(&omat.mat[0][3]);
      mavlib_ac3d_parseFloat(&omat.mat[1][3]);
      mavlib_ac3d_parseFloat(&omat.mat[2][3]);
    }

    /* URL */
    if (!strcmp(str, "url")) mavlib_ac3d_parseString(dum);

    /* vertices */
    if (!strcmp(str, "numvert")) {
      /* read in vertices */
      mavlib_ac3d_parseInt(&obj->numverts);
      obj->verts= mav_malloc(obj->numverts*sizeof(MAV_vector));
      obj->vertnorm= mav_malloc(obj->numverts*sizeof(MAV_vector));
      for (i=0; i<obj->numverts; i++) {
	mavlib_ac3d_parseFloat(&obj->verts[i].x);
	mavlib_ac3d_parseFloat(&obj->verts[i].y);
	mavlib_ac3d_parseFloat(&obj->verts[i].z);
	obj->verts[i]= mav_vectorMult(obj->verts[i], mav_matrixMult(mat, omat));
      }
    }

    /* surface */
    if (!strcmp(str, "numsurf")) {

      /* read number of surfaces */
      mavlib_ac3d_parseInt(&obj->numsurfs);

      obj->surfprops= mav_malloc(obj->numsurfs*sizeof(int));
      obj->matindex= mav_malloc(obj->numsurfs*sizeof(int));
      obj->refs= mav_malloc(obj->numsurfs*sizeof(int));
      obj->index= mav_malloc(obj->numsurfs*sizeof(int *));
      obj->tex= mav_malloc(obj->numsurfs*sizeof(MAV_texCoord *));
      obj->surfnorm= mav_malloc(obj->numsurfs*sizeof(MAV_vector));

      for (i=0; i<obj->numsurfs; i++) {

	/* default surface props and material index */
	obj->surfprops[i]=0;
	obj->matindex[i]=0;

	/* parse optional stuff until we reach refs */
	do {
	  mavlib_ac3d_parseString(dum);
	  /* optional surface details */
	  if (!strcmp(dum, "SURF")) mavlib_ac3d_parseInt(&obj->surfprops[i]);
	  /* optional material index */
	  if (!strcmp(dum, "mat")) mavlib_ac3d_parseInt(&obj->matindex[i]);
	} while (strcmp(dum, "refs"));

	/* read in vertex index and tex coord of surface */
	mavlib_ac3d_parseInt(&obj->refs[i]);

	obj->index[i]= mav_malloc(obj->refs[i]*sizeof(int));
	obj->tex[i]= mav_malloc(obj->refs[i]*sizeof(MAV_texCoord));
	
	for (j=0; j<obj->refs[i]; j++) {
	  mavlib_ac3d_parseInt(&obj->index[i][j]);
	  mavlib_ac3d_parseFloat(&obj->tex[i][j].s);
	  mavlib_ac3d_parseFloat(&obj->tex[i][j].t);
	}

	/* calculate flat shaded surface normal for polygon */
	if ((obj->surfprops[i] & 0xF)==0) {
	  if (obj->refs[i]>2)
	  {
	    j=0;
	    do {
	      if (j==obj->refs[i]-2)
	      {
		if (mav_opt_output==MAV_VERBOSE) mavlib_ac3d_fprintf("Warning: can't find surface normal.\n");
		obj->refs[i]=0;
		v1.x= 1;
		v1.y= 0;
		v1.z= 0;
		v2.x= 0;
		v2.y= 1;
		v2.z= 0;
	      }
	      else
	      {
		v1= mav_vectorNormalize(mav_vectorSub(obj->verts[obj->index[i][j]], obj->verts[obj->index[i][j+1]]));
		v2= mav_vectorNormalize(mav_vectorSub(obj->verts[obj->index[i][j+2]], obj->verts[obj->index[i][j+1]]));	    
	      }
	      j++;
	    } while (fabs(mav_vectorDotProduct(v1, v2))==1.0);

	    obj->surfnorm[i]= mav_vectorNormalize(mav_vectorCrossProduct(v2, v1));
	  } 
	  else 
	  {
	    if (mav_opt_output==MAV_VERBOSE) mavlib_ac3d_fprintf("Warning: face with less than 3 sides.\n");
	    obj->refs[i]=0;
	    obj->surfnorm[i]= MAV_NULL_VECTOR;
	  }
	}
      }

      /* calculate vertex normals */
      for (i=0; i<obj->numverts; i++) {
	obj->vertnorm[i].x=0;
	obj->vertnorm[i].y=0;
	obj->vertnorm[i].z=0;
	found=0;

	for (j=0; j<obj->numsurfs; j++) {
	  for (k=0; k<obj->refs[j]; k++) {
	    if (obj->index[j][k]==i) {
	      obj->vertnorm[i]= mav_vectorAdd(obj->vertnorm[i], obj->surfnorm[j]);
	      found=1;
	    }
	  }
	}

	if (found) obj->vertnorm[i]= mav_vectorNormalize(obj->vertnorm[i]);
      }

      /* create new facet or polyline for each surface */
      for (i=0; i<obj->numsurfs; i++) {
	if (obj->refs[i]>0) {
	  if ((obj->surfprops[i] & 0xF)==0) 
	  {
	    fc->np[fc->npolys]= obj->refs[i];
	    fc->norm[fc->npolys]= mav_malloc(fc->np[fc->npolys]*sizeof(MAV_vector));
	    fc->tex[fc->npolys]= mav_malloc(fc->np[fc->npolys]*sizeof(MAV_texCoord));
	    fc->vert[fc->npolys]= mav_malloc(fc->np[fc->npolys]*sizeof(MAV_vector));
	  
	    /* textured or not */
	    if (obj->texture[0]) 
	    {
	      fc->sp[fc->npolys]= mav_surfaceParamsNew(MAV_LIT_TEXTURE, 0, mavlib_ac3d_matLookUp[obj->matindex[i]], mavlib_ac3d_texLookUp(obj->texture));
	      if (fc->sp[fc->npolys]->texture<0) return -1;
	    }
	    else
	    {
	      fc->sp[fc->npolys]= mav_surfaceParamsNew(MAV_MATERIAL, 0, mavlib_ac3d_matLookUp[obj->matindex[i]], 0);
	    }

	    for (j=0; j<fc->np[fc->npolys]; j++) {
	      /* smooth or flat shading */
	      if (obj->surfprops[i] & 0x10) 
	      {
		fc->norm[fc->npolys][j]= obj->vertnorm[obj->index[i][j]];
	      }
	      else
	      {
		fc->norm[fc->npolys][j]= obj->surfnorm[i];
	      }

	      fc->tex[fc->npolys][j]= obj->tex[i][j];
	      fc->tex[fc->npolys][j].s*= obj->xrep;
	      fc->tex[fc->npolys][j].t*= obj->yrep;
	      fc->vert[fc->npolys][j]= obj->verts[obj->index[i][j]];
	    }
	
	    fc->npolys++;
	  }
	  else
	  {
	    pl->np[pl->nlines]= obj->refs[i];
	    pl->vert[pl->nlines]= mav_malloc(pl->np[pl->nlines]*sizeof(MAV_vector));
	    if ((obj->surfprops[i] & 0xF)==1) 
	    {
	      pl->closed[pl->nlines]= MAV_TRUE;
	    }
	    else
	    {
	      pl->closed[pl->nlines]= MAV_FALSE;
	    }

	    for (j=0; j<pl->np[pl->nlines]; j++) pl->vert[pl->nlines][j]= obj->verts[obj->index[i][j]];

	    pl->sp[pl->nlines]= mav_surfaceParamsNew(MAV_COLOUR, mavlib_ac3d_colLookUp[obj->matindex[i]], 0, 0);
	    pl->nlines++;
	  }
	}
      }
    }
  } while (strcmp(str, "kids"));

  /* parse number of children */
  mavlib_ac3d_parseInt(&nk);
  for (i=0; i<nk; i++) {
    /* read object token */
    mavlib_ac3d_parseString(str);
    /* parse rest of object */
    if (mavlib_ac3d_parseObject(c, oi_list, mav_matrixMult(mat, omat))==-1) return -1;
  }

  if (obj->numverts) {
    mav_free(obj->verts);
    mav_free(obj->vertnorm);
  }

  if (obj->numsurfs) {
    for (i=0; i<obj->numsurfs; i++) {
      mav_free(obj->index[i]);
      mav_free(obj->tex[i]);
    }
    mav_free(obj->surfprops);
    mav_free(obj->matindex);
    mav_free(obj->refs);
    mav_free(obj->index);
    mav_free(obj->tex);
    mav_free(obj->surfnorm);
  }

  mav_free(obj);

  return 0;
}



/* Routine to read an AC3D object from file or memory */

int mavlib_compositeAC3DRead(char *filename, MAV_composite *c, MAV_matrix m) 
{
  /*
  MAV_facet *fc=NULL;
  MAV_polyline *pl=NULL;
  */
  char str[100];
  int ns, sp, nolines, nosurf= 0, otbk, noobjs= 0;
  MAV_list *oi_list;
  MAVLIB_obj_info *cur_oi= NULL;

  mavlib_ac3d_origFile= filename;
  mavlib_ac3d_nl=0;

  /* open file */
  if (!mavlib_ac3d_parseOpen(filename)) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: can not open AC3D file %s\n", filename);
    return MAV_FALSE;
  }
    
  /* read version string */
  mavlib_ac3d_parseString(str);
  if (!strstr(str, "AC3D")) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: file does not start with ACD3 preamble.\n");
    return MAV_FALSE;
  }

  /* parse the lot counting up number of surfaces, lines and objects*/

  oi_list= mav_listNew();

  while (!mavlib_ac3d_parseEOF()) {
    mavlib_ac3d_parseString(str);
    if (strcmp(str, "numsurf") == 0) {
      if (cur_oi) {
	mavlib_ac3d_parseInt(&ns);
	nosurf+=ns;
	cur_oi->surfs= ns;
      }
      else {
	if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: missing an OBJECT token.\n");
	return MAV_FALSE;
      }
    }
    if (strcmp(str, "SURF") == 0) {
      if (cur_oi) {
	mavlib_ac3d_parseInt(&sp);
	if ((sp & 0xF)) {
	  nolines++;
	  cur_oi->lines++;
	}
      }
      else {
	if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: missing an OBJECT token.\n");
	return MAV_FALSE;
      }
    }
    if (strcmp(str, "OBJECT") == 0) {
      mavlib_ac3d_parseString(str);
      /* start a new object */
      cur_oi= mav_malloc(sizeof(MAVLIB_obj_info));
      cur_oi->surfs= 0;
      cur_oi->lines= 0;
      mav_listItemAdd(oi_list, cur_oi);
    }
  }

  /* calc how many objects we need (2 for an AC3D object with both solid &
     outline surfaces) */

  mav_listPointerReset(oi_list);
  while (mav_listItemNext(oi_list, (void **)&cur_oi)) {
    if (cur_oi->surfs > cur_oi->lines) noobjs++;
    if (cur_oi->lines) noobjs++;
  }

  mav_listPointerReset(oi_list);

  if (mav_opt_output==MAV_VERBOSE) 
  {
    if (mavlib_ac3d_source)
    {
      fprintf(stderr, "Parsing AC3D file %s - %i object(s), %i surfaces...", filename, noobjs, nosurf);
    }
    else
    {
      fprintf(stderr, "Parsing AC3D buffer - %i object(s), %i surfaces...", noobjs, nosurf);
    }
  }


  /* make the composite (create objects without table entries) */
  otbk= mav_opt_objectTables;
  mav_opt_objectTables= MAV_FALSE;

  c->numobj= 0;

  /* malloc off room for that number of objects */
  c->obj= mav_malloc(noobjs*sizeof(MAV_object *));


  /* close it all down and open the file again */
  mavlib_ac3d_parseClose();
  if (!mavlib_ac3d_parseOpen(filename)) {
    if (mav_opt_output==MAV_VERBOSE) mavlib_ac3d_fprintf("Error: can not open the AC3D file the second time around.\n");
    return MAV_FALSE;
  }
    
  /* read version string */
  mavlib_ac3d_parseString(str);
  if (!strstr(str, "AC3D")) {
    if (mav_opt_output==MAV_VERBOSE) mavlib_ac3d_fprintf("Error: file does not start with ACD3 preamble the second time around.\n");
    return MAV_FALSE;
  }
  
  /* parse file */
  mavlib_ac3d_parseString(str);
  while (!mavlib_ac3d_parseEOF()) {
    if (!strcmp(str, "MATERIAL")) if (mavlib_ac3d_parseMaterial()==-1) return MAV_FALSE;
    if (!strcmp(str, "OBJECT")) if (mavlib_ac3d_parseObject(c, oi_list, m)==-1) return MAV_FALSE;
    mavlib_ac3d_parseString(str);
  }

  /* close file */
  mavlib_ac3d_parseClose();

  mav_opt_objectTables= otbk;
  
  /* calculate and store its BB */
  mav_compositeCalcBB(c);

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, " done.\n");
  if (mav_opt_compositeSetMatrix) c->matrix= MAV_ID_MATRIX;
  
  return MAV_TRUE;
}



/* Routine to read an AC3D object from file */

int mav_compositeReadAC3D(char *filename, MAV_composite *c, MAV_matrix m) 
{
  int rv;

  mavlib_ac3d_source=1;
  mavlib_ac3d_matLookUp= mav_malloc(mav_opt_maxMaterials*sizeof(int));
  mavlib_ac3d_colLookUp= mav_malloc(mav_opt_maxColours*sizeof(int));

  rv= mavlib_compositeAC3DRead(filename, c, m);
  c->filename= strdup(filename);

  mav_free(mavlib_ac3d_matLookUp);
  mav_free(mavlib_ac3d_colLookUp);

  return rv;
}



/* Routine to read an AC3D object from memory */

int mav_compositeReadAC3DBuf(char *buf, MAV_composite *c, MAV_matrix m) 
{
  int rv;
  
  mavlib_ac3d_source=0;
  mavlib_ac3d_matLookUp= mav_malloc(mav_opt_maxMaterials*sizeof(int));
  mavlib_ac3d_colLookUp= mav_malloc(mav_opt_maxColours*sizeof(int));

  rv= mavlib_compositeAC3DRead(buf, c, m);

  mav_free(mavlib_ac3d_matLookUp);
  mav_free(mavlib_ac3d_colLookUp);

  return rv;
}
