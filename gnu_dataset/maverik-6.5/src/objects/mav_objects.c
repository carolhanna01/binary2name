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
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

int mav_opt_BBMethod= MAV_BB_ACCURATE;
int mav_opt_compositeSetMatrix= MAV_TRUE;
int mav_opt_splash= MAV_UNDEFINED;
int mav_opt_curveLOD= MAV_FALSE;
int mav_opt_vertsMin= 6;
int mav_opt_vertsMax= 20;
float mav_opt_curveFactor= 10000;
float mav_opt_drawNormals= MAV_UNDEFINED;
MAV_compositeFormat mav_compositeFormat[MAV_MAX_COMPOSITE_FORMATS];

extern MAV_vector mavlib_splashVert[];
extern MAV_vector mavlib_splashNorm[];



/* The splash */

void mavlib_updateSplash(MAV_facet *f, MAV_text *t, float time)
{
  float val;

  if (time < 1.0)
  {
    val= sin(MAV_PI_OVER_2*time);
    f->matrix= mav_matrixSetOld(360.0*val,270.0+90.0*val,180.0*(1.0-val),0.0,0.0,
                             8.0*(1.0-val));
    t->matrix= mav_matrixSet(0.0,0.0,0.0, 0.0,-2.0,6.0*(1.0-val));
    t->matrix= mav_matrixScaleSet(t->matrix, 0.575);
  }
  else
  {
    f->matrix= MAV_ID_MATRIX;
    t->matrix= mav_matrixSet(0.0,0.0,0.0, 0.0,-2.0,0.0);
    t->matrix= mav_matrixScaleSet(t->matrix, 0.575);
  }
}

int mavlib_splashKey(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->key=='i' && ke->movement==MAV_PRESSED) {
    fprintf(stderr, "\n\n");
    fprintf(stderr, "%s - Copyright (C) 1999-2002 Advanced Interfaces Group\n", mav_kernelID());
    fprintf(stderr, "Maverik comes with ABSOLUTELY NO WARRANTY.\n");
    fprintf(stderr, "This is free software, distributed under the terms of\n");
    fprintf(stderr, "the GNU General Public License. See the COPYING file in\n");
    fprintf(stderr, "the Maverik distribution for more information.\n");
    fprintf(stderr, "\n\n");
  }

  return 1;
}

void mavlib_splash(void)
{  
  MAV_facet f;
  MAV_text txt;
  MAV_timer t;
  MAV_object o1, o2;
  MAV_window *win, *wb;
  int i, j, splashCount=0;
  MAV_surfaceParams *sp;

  /* Make a copy of the stereo params, then set them to zero */
  float stpbk= mav_stp_default.offset;
  mav_stp_default.offset=0;

  txt.text= "(c) 1999-2002 Advanced Interfaces Group";
  txt.style= MAV_FILLED_FONT;
  txt.justify= MAV_CENTER_JUSTIFY;
  txt.sp= mav_sp_default;
  txt.matrix= mav_matrixSet(0.0,0.0,0.0, 0.0,-2.0,0.0);
  txt.matrix= mav_matrixScaleSet(txt.matrix, 0.575);

  sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 13, 0);
  f.npolys=704;
  f.np= (int *) mav_malloc(f.npolys*sizeof(int));
  f.norm= (MAV_vector **) mav_malloc(f.npolys*sizeof(MAV_vector *));
  f.vert= (MAV_vector **) mav_malloc(f.npolys*sizeof(MAV_vector *));
  f.sp= (MAV_surfaceParams **) mav_malloc(f.npolys*sizeof(MAV_surfaceParams *));
  
  for (i=0; i<f.npolys; i++) {
    f.np[i]=3;
    f.norm[i]= (MAV_vector *) mav_malloc(f.np[i]*sizeof(MAV_vector));
    f.vert[i]= (MAV_vector *) mav_malloc(f.np[i]*sizeof(MAV_vector));
    f.sp[i]= sp;
    
    for (j=0; j<f.np[i]; j++) {
      f.norm[i][j]= mavlib_splashNorm[splashCount];
      f.vert[i][j]= mavlib_splashVert[splashCount++];
    }
  }

  o1.the_class= mav_class_text;
  o1.the_data= &txt;

  o2.the_class= mav_class_facet;
  o2.the_data= &f;

  mav_windowBackgroundColourSet(mav_win_all, 0,0,0);
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, mavlib_splashKey);

  mav_timerStart(&t);
  do {
    mav_timerStop(&t);      
    mav_eventsCheck();
    mav_frameBegin();
    mavlib_updateSplash(&f, &txt, t.wall);

    mav_stringDisplayCentre(mav_win_all, mav_kernelID(), MAV_COLOUR_WHITE, 0, -0.6,-0.82);
    mav_stringDisplayCentre(mav_win_all, "email:  maverik@aig.cs.man.ac.uk", MAV_COLOUR_WHITE, 0, -0.6,-0.90);
    mav_stringDisplayCentre(mav_win_all, "www:  http://aig.cs.man.ac.uk", MAV_COLOUR_WHITE, 0, -0.6,-0.96);
    if (t.wall<1.75) mav_stringDisplayRight(mav_win_all, "Press \"i\" for more information", MAV_COLOUR_WHITE, 0, 0.9,-0.90);

    wb= mav_win_current;
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) {
      if (mav_win_current!=win) mav_windowSet(win);
      mav_textDraw(&o1, NULL);
      mav_facetDraw(&o2, NULL);
    }
    if (mav_win_current!=wb) mav_windowSet(wb);

    mav_frameEnd();
    /*printf("fps %f\n", mav_fps);*/
    /*} while (1);*/
  } while (t.wall<1.75);

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, NULL);
  mav_firstFrame= MAV_TRUE;

  /* free up memory */
  for (i=0; i<f.npolys; i++) {
    mav_free(f.norm[i]);
    mav_free(f.vert[i]);
  }
  mav_free(f.np);
  mav_free(f.norm);
  mav_free(f.vert);
  mav_free(f.sp);
  mav_free(sp);

  /* Restore stereo params */
  mav_stp_default.offset= stpbk;
}



/* Routine to toggle LOD */

void mavlib_cf4(MAV_window *w)
{
  mav_opt_curveLOD= !mav_opt_curveLOD;
  if (mav_opt_output==MAV_VERBOSE) {
    if (mav_opt_curveLOD) 
    {
      fprintf(stderr, "LOD on\n");
    }
    else
    {
      fprintf(stderr, "LOD off\n");
    }
  }
}



/* Routines for handling options */

void mavlib_objectsCmdLineParse(int argc, char *argv[])
{
  int cur_arg;
  int cur_char;
  char *arg_name;

  /* start at 1 to skip prog name */
  for (cur_arg=1; cur_arg < argc; cur_arg++)
  {
    /* skip args already parsed by another module */
    if (argv[cur_arg])
    {
      arg_name= mav_malloc(sizeof(char)*strlen(argv[cur_arg])+1);
      strcpy(arg_name, argv[cur_arg]);

      /* lowercase arg_name */
      for (cur_char=0; arg_name[cur_char]; cur_char++)
      {  
	arg_name[cur_char]= tolower(arg_name[cur_char]);
      }

      if (strcmp(arg_name, "-drawnormals") == 0)
      {
	mav_opt_drawNormals= atof(argv[cur_arg+1]);
	if (mav_opt_drawNormals==0) mav_opt_drawNormals= MAV_INFINITY;
	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }
      else if (strcmp(arg_name, "-nosplash") == 0)
      {
	mav_opt_splash= MAV_FALSE;
	argv[cur_arg]= NULL;
      }
      else if (strcmp(arg_name, "-splash") == 0)
      {
	mav_opt_splash= MAV_TRUE;
	argv[cur_arg]= NULL;
      }
      else if (strcmp(arg_name, "-mavhelp") == 0)
      {
	printf("  -[no]Splash\t\t\t\tDisplay splash screen\n");
	printf("  -drawNormals <length>\t\t\tDraw object normals using specified length\n");
      }

      mav_free(arg_name);
    }
  }      
}

void mavlib_objectsEnvVarsParse(void)
{
  char *ev;
  
  if ((ev= getenv("MAV_SPLASH")))
  {
    mav_opt_splash= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_DRAWNORMALS")))
  {
    if (strcmp(ev, "0") == 0) 
    {
      mav_opt_drawNormals= MAV_INFINITY;
    }
    else
    {
      mav_opt_drawNormals= atof(ev);
    }
  }
}

void mavlib_objectsConfigFileParse(FILE *cf_file)
{
  char line[200], opt[100], val[100];
  int cur_char;

  fseek(cf_file, 0, SEEK_SET);

  while (fgets(line, 200, cf_file))
  {
    if (sscanf(line, "%s %s", opt, val) == 2)
    {
      /* lowercase opt name */
      for (cur_char=0; opt[cur_char]; cur_char++)
      {  
	opt[cur_char]= tolower(opt[cur_char]);
      }

      /* config file options mustn't override application settings */

      if (strcmp(opt, "splash") == 0 && mav_opt_splash == MAV_UNDEFINED)
      {
	mav_opt_splash= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }
      else if (strcmp(opt, "drawnormals") == 0 && mav_opt_drawNormals == MAV_UNDEFINED)
      {
	if (strcmp(val, "0") == 0) 
	{
	  mav_opt_drawNormals= MAV_INFINITY;
	}
	else
	{
	  mav_opt_drawNormals= atof(val);
	}
      }
    }
  }
}

void mavlib_objectsOptionsDefaultSet(void)
{
  /* set any undefined options */

  if (mav_opt_splash == MAV_UNDEFINED) mav_opt_splash= MAV_TRUE;
  if (mav_opt_drawNormals == MAV_UNDEFINED) mav_opt_drawNormals= MAV_INFINITY;
}


/* Routines to initialise the common objects module */

char *mav_objectsModuleID(void)
{
  return "Common objects";
}

int mav_objectsModuleInit(void)
{
  int i;

  /* set options */
  if (mav_userconf) mavlib_objectsConfigFileParse(mav_userconf);
  mavlib_objectsEnvVarsParse();
  if (mav_argc) mavlib_objectsCmdLineParse(mav_argc, mav_argv);
  mavlib_objectsOptionsDefaultSet();

  /* Add the new module */

  mav_moduleNew(mav_objectsModuleID);

  /* Define classes for the objects */

  mav_class_box= mav_classNew();
  mav_class_pyramid= mav_classNew();
  mav_class_cylinder= mav_classNew();
  mav_class_cone= mav_classNew();
  mav_class_sphere= mav_classNew();
  mav_class_hsphere= mav_classNew();
  mav_class_ellipse= mav_classNew();
  mav_class_hellipse= mav_classNew();
  mav_class_ctorus= mav_classNew();
  mav_class_rtorus= mav_classNew();
  mav_class_polygon= mav_classNew();
  mav_class_polygonGrp= mav_classNew();
  mav_class_facet= mav_classNew();
  mav_class_rectangle= mav_classNew();
  mav_class_polyline= mav_classNew();
  mav_class_text= mav_classNew();
  mav_class_composite= mav_classNew();
  mav_class_SMSObj= mav_classNew();
  mav_class_teapot= mav_classNew();

  /* Set their draw callbacks */
  
  mav_callbackDrawSet(mav_win_all, mav_class_box, mav_boxDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_pyramid, mav_pyramidDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_cylinder, mav_cylinderDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_cone, mav_coneDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_sphere, mav_sphereDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_hsphere, mav_hsphereDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_ellipse, mav_ellipseDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_hellipse, mav_hellipseDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_ctorus, mav_ctorusDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_rtorus, mav_rtorusDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_polygon, mav_polygonDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_facet, mav_facetDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_rectangle, mav_rectangleDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_polyline, mav_polylineDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_text, mav_textDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_composite, mav_compositeDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_SMSObj, mav_SMSObjDraw);
  mav_callbackDrawSet(mav_win_all, mav_class_teapot, mav_teapotDraw);

  /* Set their bounding box callbacks */

  if (mav_opt_BBMethod==MAV_BB_FAST)
  {
    /* quick but overestimates method */
    mav_callbackBBSet(mav_win_all, mav_class_box, mav_boxBB);
    mav_callbackBBSet(mav_win_all, mav_class_pyramid, mav_pyramidBB);
    mav_callbackBBSet(mav_win_all, mav_class_cylinder, mav_cylinderBB);
    mav_callbackBBSet(mav_win_all, mav_class_cone, mav_coneBB);
    mav_callbackBBSet(mav_win_all, mav_class_sphere, mav_sphereBB);
    mav_callbackBBSet(mav_win_all, mav_class_hsphere, mav_hsphereBB);
    mav_callbackBBSet(mav_win_all, mav_class_ellipse, mav_ellipseBB);
    mav_callbackBBSet(mav_win_all, mav_class_hellipse, mav_hellipseBB);
    mav_callbackBBSet(mav_win_all, mav_class_ctorus, mav_ctorusBB);
    mav_callbackBBSet(mav_win_all, mav_class_rtorus, mav_rtorusBB);
    mav_callbackBBSet(mav_win_all, mav_class_polygon, mav_polygonBB);
    mav_callbackBBSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpBB);
    mav_callbackBBSet(mav_win_all, mav_class_facet, mav_facetBB);
    mav_callbackBBSet(mav_win_all, mav_class_polyline, mav_polylineBB);
  }
  else if (mav_opt_BBMethod==MAV_BB_ACCURATE)
  {
    /* slow but accurate method */
    mav_callbackBBSet(mav_win_all, mav_class_box, mav_boxBB2);
    mav_callbackBBSet(mav_win_all, mav_class_pyramid, mav_pyramidBB2);
    mav_callbackBBSet(mav_win_all, mav_class_cylinder, mav_cylinderBB2);
    mav_callbackBBSet(mav_win_all, mav_class_cone, mav_coneBB2);
    mav_callbackBBSet(mav_win_all, mav_class_sphere, mav_sphereBB2);
    mav_callbackBBSet(mav_win_all, mav_class_hsphere, mav_hsphereBB2);
    mav_callbackBBSet(mav_win_all, mav_class_ellipse, mav_ellipseBB2);
    mav_callbackBBSet(mav_win_all, mav_class_hellipse, mav_hellipseBB2);
    mav_callbackBBSet(mav_win_all, mav_class_ctorus, mav_ctorusBB2);
    mav_callbackBBSet(mav_win_all, mav_class_rtorus, mav_rtorusBB2);
    mav_callbackBBSet(mav_win_all, mav_class_polygon, mav_polygonBB2);
    mav_callbackBBSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpBB2);
    mav_callbackBBSet(mav_win_all, mav_class_facet, mav_facetBB2);
    mav_callbackBBSet(mav_win_all, mav_class_polyline, mav_polylineBB2);
  }
  mav_callbackBBSet(mav_win_all, mav_class_rectangle, mav_rectangleBB);
  mav_callbackBBSet(mav_win_all, mav_class_text, mav_textBB);
  mav_callbackBBSet(mav_win_all, mav_class_composite, mav_compositeBB);
  mav_callbackBBSet(mav_win_all, mav_class_SMSObj, mav_SMSObjBB);
  mav_callbackBBSet(mav_win_all, mav_class_teapot, mav_teapotBB);

  /* Set their intersection callbacks */

  mav_callbackIntersectSet(mav_win_all, mav_class_box, mav_boxIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_pyramid, mav_pyramidIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_cylinder, mav_cylinderIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_cone, mav_coneIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_sphere, mav_sphereIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_hsphere, mav_hsphereIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_ellipse, mav_ellipseIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_hellipse, mav_hellipseIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_ctorus, mav_ctorusIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_rtorus, mav_rtorusIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_polygon, mav_polygonIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_facet, mav_facetIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_rectangle, mav_rectangleIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_polyline, NULL);
  mav_callbackIntersectSet(mav_win_all, mav_class_text, NULL);
  mav_callbackIntersectSet(mav_win_all, mav_class_composite, mav_compositeIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_SMSObj, mav_SMSObjIntersect);
  mav_callbackIntersectSet(mav_win_all, mav_class_teapot, NULL);

  /* Set their identification callbacks */

  mav_callbackIDSet(mav_win_all, mav_class_box, mav_boxID);
  mav_callbackIDSet(mav_win_all, mav_class_pyramid, mav_pyramidID);
  mav_callbackIDSet(mav_win_all, mav_class_cylinder, mav_cylinderID);
  mav_callbackIDSet(mav_win_all, mav_class_cone, mav_coneID);
  mav_callbackIDSet(mav_win_all, mav_class_sphere, mav_sphereID);
  mav_callbackIDSet(mav_win_all, mav_class_hsphere, mav_hsphereID);
  mav_callbackIDSet(mav_win_all, mav_class_ellipse, mav_ellipseID);
  mav_callbackIDSet(mav_win_all, mav_class_hellipse, mav_hellipseID);
  mav_callbackIDSet(mav_win_all, mav_class_ctorus, mav_ctorusID);
  mav_callbackIDSet(mav_win_all, mav_class_rtorus, mav_rtorusID);
  mav_callbackIDSet(mav_win_all, mav_class_polygon, mav_polygonID);
  mav_callbackIDSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpID);
  mav_callbackIDSet(mav_win_all, mav_class_facet, mav_facetID);
  mav_callbackIDSet(mav_win_all, mav_class_rectangle, mav_rectangleID);
  mav_callbackIDSet(mav_win_all, mav_class_polyline, mav_polylineID);
  mav_callbackIDSet(mav_win_all, mav_class_text, mav_textID);
  mav_callbackIDSet(mav_win_all, mav_class_composite, mav_compositeID);
  mav_callbackIDSet(mav_win_all, mav_class_SMSObj, mav_SMSObjID);
  mav_callbackIDSet(mav_win_all, mav_class_teapot, mav_teapotID);

  /* Set their get userdef callbacks */

  mav_callbackGetUserdefSet(mav_win_all, mav_class_box, mav_boxGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_pyramid, mav_pyramidGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_cylinder, mav_cylinderGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_cone, mav_coneGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_sphere, mav_sphereGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_hsphere, mav_hsphereGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_ellipse, mav_ellipseGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_hellipse, mav_hellipseGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_ctorus, mav_ctorusGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_rtorus, mav_rtorusGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_polygon, mav_polygonGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_facet, mav_facetGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_rectangle, mav_rectangleGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_polyline, mav_polylineGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_text, mav_textGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_composite, mav_compositeGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_SMSObj, mav_SMSObjGetUserdef);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_teapot, mav_teapotGetUserdef);

  /* Set their get matrix callbacks */

  mav_callbackGetMatrixSet(mav_win_all, mav_class_box, mav_boxGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_pyramid, mav_pyramidGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_cylinder, mav_cylinderGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_cone, mav_coneGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_sphere, mav_sphereGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_hsphere, mav_hsphereGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_ellipse, mav_ellipseGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_hellipse, mav_hellipseGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_ctorus, mav_ctorusGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_rtorus, mav_rtorusGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_polygon, mav_polygonGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_facet, mav_facetGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_rectangle, mav_rectangleGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_polyline, mav_polylineGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_text, mav_textGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_composite, mav_compositeGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_SMSObj, mav_SMSObjGetMatrix);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_teapot, mav_teapotGetMatrix);

  /* Set their get surface params callbacks */

  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_box, mav_boxGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_pyramid, mav_pyramidGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_cylinder, mav_cylinderGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_cone, mav_coneGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_sphere, mav_sphereGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_hsphere, mav_hsphereGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_ellipse, mav_ellipseGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_hellipse, mav_hellipseGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_ctorus, mav_ctorusGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_rtorus, mav_rtorusGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_polygon, mav_polygonGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_facet, mav_facetGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_rectangle, mav_rectangleGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_polyline, mav_polylineGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_text, mav_textGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_composite, mav_compositeGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_SMSObj, mav_SMSObjGetSurfaceParams);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_teapot, mav_teapotGetSurfaceParams);

  /* Set their dump callbacks */

  mav_callbackDumpSet(mav_win_all, mav_class_box, mav_boxDump);
  mav_callbackDumpSet(mav_win_all, mav_class_pyramid, mav_pyramidDump);
  mav_callbackDumpSet(mav_win_all, mav_class_cylinder, mav_cylinderDump);
  mav_callbackDumpSet(mav_win_all, mav_class_cone, mav_coneDump);
  mav_callbackDumpSet(mav_win_all, mav_class_sphere, mav_sphereDump);
  mav_callbackDumpSet(mav_win_all, mav_class_hsphere, mav_hsphereDump);
  mav_callbackDumpSet(mav_win_all, mav_class_ellipse, mav_ellipseDump);
  mav_callbackDumpSet(mav_win_all, mav_class_hellipse, mav_hellipseDump);
  mav_callbackDumpSet(mav_win_all, mav_class_ctorus, mav_ctorusDump);
  mav_callbackDumpSet(mav_win_all, mav_class_rtorus, mav_rtorusDump);
  mav_callbackDumpSet(mav_win_all, mav_class_polygon, mav_polygonDump);
  mav_callbackDumpSet(mav_win_all, mav_class_polygonGrp, mav_polygonGrpDump);
  mav_callbackDumpSet(mav_win_all, mav_class_facet, mav_facetDump);
  mav_callbackDumpSet(mav_win_all, mav_class_rectangle, mav_rectangleDump);
  mav_callbackDumpSet(mav_win_all, mav_class_polyline, mav_polylineDump);
  mav_callbackDumpSet(mav_win_all, mav_class_text, mav_textDump);
  mav_callbackDumpSet(mav_win_all, mav_class_composite, mav_compositeDump);
  mav_callbackDumpSet(mav_win_all, mav_class_SMSObj, mav_SMSObjDump);
  mav_callbackDumpSet(mav_win_all, mav_class_teapot, mav_teapotDump);

  /* Initialise the supported composite file formats */
  for (i=0; i<MAV_MAX_COMPOSITE_FORMATS; i++) mav_compositeFormat[i].defined=MAV_FALSE;
  
  /* Add AC3D to list of supported composite file formats */
  mav_compositeFormat[0].defined= MAV_TRUE;
  mav_compositeFormat[0].ext= ".ac";
  mav_compositeFormat[0].fn= mav_compositeReadAC3D;

  /* Add JIF to list of supported composite file formats */
  mav_compositeFormat[1].defined= MAV_TRUE;
  mav_compositeFormat[1].ext= ".jif";
  mav_compositeFormat[1].fn= mav_compositeReadJIF;

  /* The splash */
  if (mav_opt_splash && mav_win_left) mavlib_splash();

  /* Default background colour */
  mav_windowBackgroundColourSet(mav_win_all, 0.0, 0.5, 1.0);

  mav_ctrlF[4]= mavlib_cf4;
  mav_ctrlF_desc[4]= "Ctrl-F4 toggle LOD";

  return 1;
}



/* Routine to intersect a line with a polygon */

int mav_linePolygonIntersection(MAV_polygon *apoly, MAV_line ln, MAV_objectIntersection *o)
{
  MAV_vector *new_vert, axis, norm;
  MAV_matrix rot;
  MAV_line rotln;
  float ca, sa, ang;
  int i, j, c=MAV_FALSE;

/* Malloc off space to rotate vertices so norm is parrallel to Z axis */

  new_vert=mav_malloc(apoly->np*sizeof(MAV_vector));

/* Find rotation so polygon's normal is aligned with Z axis */

  ca= apoly->norm.z;
  if (ca>=1.0)
  {
    rot= MAV_ID_MATRIX;
  }
  else if (ca<=-1.0)
  {
    rot= mav_matrixSet(0,180,0,0,0,0);
  }
  else
  {
    axis.x= apoly->norm.y;
    axis.y= -apoly->norm.x;
    axis.z= 0;

    ang= MAV_RAD2DEG(acos(ca));
    sa= mav_vectorMag(axis);
    if (sa<0.0) ang= 360-ang;

    rot= mav_matrixQuaternionConvert(mav_quaternionSet(mav_vectorNormalise(axis), ang));
  }

/* Rotate all the polygon pts to be Z axis aligned */

  for (i=0; i<apoly->np; i++) new_vert[i]= mav_vectorMult(apoly->vert[i], rot);

/* Rotate line by the same amount */

  rotln.pt= mav_vectorMult(ln.pt, rot);
  rotln.dir= mav_vectorMult(ln.dir, rot);

/* Check if line intersects with infite plane of the polygon */

  norm.x=0.0;
  norm.y=0.0;
  norm.z=1.0;

  if (mav_lineInfPlaneIntersection(new_vert[0], norm, rotln, o))
  {
    /* Check if intersection is within polygon - This bit taken from cga FAQ */

    for (i = 0, j = apoly->np-1; i < apoly->np; j = i++) {
      if ((((new_vert[i].y<=o->intpt.y) && (o->intpt.y<new_vert[j].y)) ||
           ((new_vert[j].y<=o->intpt.y) && (o->intpt.y<new_vert[i].y))) &&
          (o->intpt.x < (new_vert[j].x - new_vert[i].x) * (o->intpt.y - new_vert[i].y) / (new_vert[j].y - new_vert[i].y) + new_vert[i].x))
        
        c = !c;
    }
  }
  
  mav_free(new_vert);

  if (!c) {
    o->pt1=-100;
    o->pt2=-100;
  }

  return(c);
}
