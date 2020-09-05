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
#include "mavlib_vrad.h"
#include <stdio.h>
#include <math.h>
#include <string.h>

MAV_class *mav_class_vrad;
int mav_opt_vradSetMatrix= MAV_TRUE;



int mav_vradDraw(MAV_object *o, MAV_drawInfo *di)
{
  MAV_vrad *vrad= (MAV_vrad *) mav_objectDataGet(o);
  MAV_drawInfo di2;

  /* store the current transformation matrix - then multiply it by the local transformation */
  mav_gfxMatrixPush();
  mav_gfxMatrixMult(vrad->matrix);

  /* transform the drawinfo */
  di2= mav_drawInfoTransFrame(*di, vrad->matrix);
  
  /* use emmissive colour */
  mav_gfxColouringModeUse(mav_palette_default, MAV_COLOUR);

  /* display SMS containing elements */
  mav_SMSDisplayUsingDrawInfo(mav_win_current, vrad->sms, di2);

  /* undefine surface params */
  mav_surfaceParamsUndefine();

  /* restore original transformation matrix */
  mav_gfxMatrixPop();

  return 1;
}



int mav_vradGetMatrix(MAV_object *o, MAV_matrix **mat)
{
  MAV_vrad *vrad= (MAV_vrad *) mav_objectDataGet(o);

  *mat= &vrad->matrix;

  return 1;
}



int mav_vradGetUserdef(MAV_object *o, void ***ud)
{
  MAV_vrad *vrad= (MAV_vrad *) mav_objectDataGet(o);

  *ud= &vrad->userdef;

  return 1;
}



/* Routines to parse a VRAD file */

int mavlib_vradReadInt(FILE *f)
{
  unsigned char c;
  int i;

  fread(&c, 1, 1, f);
  i=c;

  return i;
}

float mavlib_vradReadFloat(FILE *f)
{
  float v;

#ifdef MAV_LINUX
  char buf[4], buf2[4];
  fread(&buf, 1, 4, f);
  
  buf2[0]= buf[3];
  buf2[1]= buf[2];
  buf2[2]= buf[1];
  buf2[3]= buf[0];

  memcpy(&v, buf2, 4);
#else
#ifdef WIN32
  char buf[4], buf2[4];
  fread(&buf, 1, 4, f);
  
  buf2[0]= buf[3];
  buf2[1]= buf[2];
  buf2[2]= buf[1];
  buf2[3]= buf[0];

  memcpy(&v, buf2, 4);
#else
  fread(&v, sizeof(float), 1, f);
#endif
#endif
  
  return v;
}

int mav_vradRead(char *filename, MAV_vrad *vrad, MAV_matrix m)
{
  FILE *file;
  MAVLIB_vradElem *elem;
  MAV_SMS *tmpsms;
  int i=0, k, np;

  /* Open the file and parse the elements */
  file = fopen(filename, "rb");
  if (!file) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Cannot open vrad file %s\n", filename);
    return MAV_FALSE;
  }

  /* Create a temporary SMS for the elements */
  tmpsms= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());

  while (!feof(file)) {

    /* Create a new element */
    elem= (MAVLIB_vradElem *) mav_malloc(sizeof(MAVLIB_vradElem));

    elem->type= mavlib_vradReadInt(file);

    if (feof(file)) break;

    /* Number of vertices depends on type */
    switch (elem->type) {
      case 0 :
      case 1 :
        np=4;
        break;
      case 2 :
      case 3 :
      case 4 :
      case 5 :
        np=5;
        break;
      case 6 :
      case 7 :
      case 8 :
      case 9 :
      case 10 :
      case 11 :
        np=6;
        break;
      case 12 :
      case 13 :
      case 14 :
      case 15 :
        np=7;
        break;
      case 16 :
        np=8;
        break;
      default:
	if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Read a %i type in file %s, fail\n", elem->type, filename);
	return MAV_FALSE;
    }

    /* Store vertices, colours and BB for element */
    elem->verts = (MAVLIB_vradVert *) mav_malloc(np*sizeof(MAVLIB_vradVert));
    mav_BBCompInit(&elem->bb);

    for (k=0; k<np; k++) {
      int r,g,b;
      r= mavlib_vradReadInt(file);
      g= mavlib_vradReadInt(file);
      b= mavlib_vradReadInt(file);
      mavlib_vradReadInt(file);
      elem->verts[k].colour[0]=r/255.0;
      elem->verts[k].colour[1]=g/255.0;
      elem->verts[k].colour[2]=b/255.0;

      elem->verts[k].vertex.x= mavlib_vradReadFloat(file);
      elem->verts[k].vertex.y= mavlib_vradReadFloat(file);
      elem->verts[k].vertex.z= mavlib_vradReadFloat(file);
      elem->verts[k].vertex= mav_vectorMult(elem->verts[k].vertex, m);

      mav_BBCompPt(elem->verts[k].vertex, &elem->bb);
    }

    /* Add obj to temporary SMS */
    mav_SMSCallbackObjectAddExec(tmpsms, mav_objectNew(mavlib_vradElemClass, elem));

    i++;
    if (!(i%10000)) if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Read %i elements\n", i);
  }

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Read %i elements in all\n", i);
  fclose(file);

  /* Create an HBB from the temporary SMS */
  vrad->sms= mav_SMSNew(mav_SMSClass_HBB, mav_HBBNew());
  mav_SMSSelectabilitySet(vrad->sms, mav_win_all, MAV_FALSE);
  mav_HBBConstructFromSMS(vrad->sms, tmpsms);

  /* delete temporary SMS */
  mav_SMSDelete(tmpsms, MAV_FALSE);

  if (mav_opt_vradSetMatrix) vrad->matrix= MAV_ID_MATRIX;

  return MAV_TRUE;
}



void mav_vradGamma(MAV_vrad *vrad, float gam)
{
  MAV_object *o;
  MAVLIB_vradElem *e;
  int i, k, np;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Applying gamma...");

  mav_SMSCallbackPointerResetExec(vrad->sms);
  while (mav_SMSCallbackObjectNextExec(vrad->sms, &o)) {
    e= (MAVLIB_vradElem *) mav_objectDataGet(o);

    switch (e->type) {
      case 0 :
      case 1 :
        np=4;
        break;
      case 2 :
      case 3 :
      case 4 :
      case 5 :
        np=5;
        break;
      case 6 :
      case 7 :
      case 8 :
      case 9 :
      case 10 :
      case 11 :
        np=6;
        break;
      case 12 :
      case 13 :
      case 14 :
      case 15 :
        np=7;
        break;
      case 16 :
        np=8;
        break;
      default:
        np=-1;
        break;
    }
    
    for (i=0; i<np; i++) {
      for (k=0; k<3; k++) {
	e->verts[i].colour[k]= pow(e->verts[i].colour[k], gam);
      }
    }
  }

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "done.\n");
}

