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
#include "mav_hud.h"
#include <math.h>
#include <stdio.h>

MAV_class *mav_class_hud;



void mavlib_hud2Rect(MAV_hud *hud)
{
  MAV_vector cent;
  float xndc, yndc, width, height;

  if (mav_win_current->orthogonal && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: HUDs not supported with orthogonal projections\n");

  /* Orientate matrix to view */
  hud->matrix= MAV_ID_MATRIX;
  hud->matrix= mav_matrixXAxisSet(hud->matrix, mav_win_current->right);
  hud->matrix= mav_matrixYAxisSet(hud->matrix, mav_win_current->up);
  hud->matrix= mav_matrixZAxisSet(hud->matrix, mav_vectorScalar(mav_win_current->view, -1));

  /* Calculate center position of HUD in NDC */
  xndc= (hud->x+hud->w/2.0)/((float) mav_win_current->width)*2.0-1.0;
  yndc= (hud->y-hud->h/2.0)/((float) mav_win_current->height)*2.0-1.0;

  /* Calculate half width and height of screen in real units at near clip plane plus a bit */
  height= mav_win_current->ncp*hud->depthfac*tan(MAV_DEG2RAD(mav_win_current->fov*0.5));
  width= mav_win_current->aspect*height;

  /* Set position part of matrix */
  cent= mav_vectorAdd(mav_win_current->eye, mav_vectorScalar(mav_win_current->view, mav_win_current->ncp*hud->depthfac));
  cent= mav_vectorAdd(cent, mav_vectorScalar(mav_win_current->up, yndc*height));
  cent= mav_vectorAdd(cent, mav_vectorScalar(mav_win_current->right, xndc*width));
  hud->matrix= mav_matrixXYZSet(hud->matrix, cent);

  /* Calculate height and width of HUD */
  height*= hud->h/((float) mav_win_current->height)*2.0;
  width*= hud->w/((float) mav_win_current->width)*2.0;

  /* Make up a rectangle to represent the HUD */
  hud->rect.width= width;
  hud->rect.height= height;
  hud->rect.matrix= hud->matrix;
  hud->rect.sp= hud->sp;
  hud->rect.xtile= 1;
  hud->rect.ytile= 1;
}



int mav_hudDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_hud *hud= (MAV_hud*) mav_objectDataGet(obj);
  MAV_surfaceParams sp;
  float xndc, yndc;
  int i;

  /* Turn the HUD into a rectangle */
  mavlib_hud2Rect(hud);
  
  /* Render the rectangle */
  mav_rectangleDraw(hud->obj, di);
    
  /* Render text on HUD */
  if (hud->nolines) {
    
    /* Use an identity projection matrix */
    mav_gfxMatrixMode(MAV_PROJECTION);
    mav_gfxMatrixLoad(MAV_ID_MATRIX);    

    /* Use an identity view matrix */
    mav_gfxMatrixMode(MAV_MODELVIEW);
    mav_gfxMatrixPush();
    mav_gfxMatrixLoad(MAV_ID_MATRIX);
  
    /* Set the correct colouring */
    sp.mode= MAV_COLOUR;
    sp.colour= hud->textColour;
    sp.material=0;
    sp.texture=0;
    mav_surfaceParamsUse(&sp);

    for (i=0; i<hud->nolines; i++) {      

      if (hud->line[i]) {
	/* Calc position of test in NDC */
	xndc= (hud->x+hud->textXOffset)/((float) mav_win_current->width)*2.0-1.0;
	yndc= (hud->y-hud->textYOffset-(i*hud->textYStep))/((float) mav_win_current->height)*2.0-1.0;

	/* Render text without depth test */
	mav_gfxDepthTestSet(MAV_FALSE);
	mav_gfxRasterPos2DSet(xndc, yndc);
	mav_gfxWindowStringDisplay(hud->line[i], hud->textFont);
	mav_gfxDepthTestSet(MAV_TRUE);
      }
    }

    /* Restore original projection and view matrices */
    mav_gfxMatrixMode(MAV_PROJECTION);
    mav_gfxMatrixLoad(mav_win_current->projMat);
    
    mav_gfxMatrixMode(MAV_MODELVIEW);
    mav_gfxMatrixPop();
  }

  return 1;
}



int mav_hudBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_hud *hud= (MAV_hud *) mav_objectDataGet(obj);

  /* Turn the HUD into a rectangle */
  mavlib_hud2Rect(hud);

  /* Calc BB of the rectangle */
  return mav_rectangleBB(hud->obj, bb);
}



int mav_hudIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *oi)
{
  MAV_hud *hud= (MAV_hud *) mav_objectDataGet(obj);
  int rv; 

  /* Turn the HUD into a rectangle */
  mavlib_hud2Rect(hud);

  /* Intersect the rectangle */
  if (hud->selectable)
  {
     rv= mav_rectangleIntersect(hud->obj, ln, oi);
  }
  else
  {
     rv= 0;
  }

  return rv;
}



int mav_hudID(MAV_object *obj, char **id)
{
  *id="hud";

  return 1;
}



int mav_hudGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_hud *hud= (MAV_hud*) mav_objectDataGet(obj);

  *ud= &hud->userdef;

  return 1;
}



int mav_hudGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_hud *hud= (MAV_hud*) mav_objectDataGet(obj);

  *mat= &hud->matrix;

  return 1;
}



int mav_hudGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_hud *hud= (MAV_hud*) mav_objectDataGet(obj);

  *sp= &hud->sp;

  return 1;
}



int mav_hudDump(MAV_object *obj)
{
  MAV_hud *hud = (MAV_hud *) mav_objectDataGet(obj);
  int i;

  printf("*** Dumping object %p - a MAV_hud with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("nolines %i\n", hud->nolines);
  for (i=0; i<hud->nolines; i++) printf("line %i [%s]\n", i, hud->line[i]);
  printf("xywd %i %i %i %i\n", hud->x, hud->y, hud->w, hud->h);
  printf("depthfac %f\n", hud->depthfac);
  printf("text xoffset %i yoffset %i ystep %i\n", hud->textXOffset, hud->textYOffset, hud->textYStep);
  printf("text font %i colour %i\n", hud->textFont, hud->textColour);
  printf("selectable %i\n", hud->selectable);
  mav_surfaceParamsPrint("surface params ", *hud->sp);
  mav_matrixPrint("matrix (temporary)\n", hud->matrix);
  printf("userdef %p\n", hud->userdef);

  return 1;
}



MAV_hud *mav_hudNew(int nolines, int x, int y, int w, int h, MAV_surfaceParams *sp)
{
  MAV_hud *hud= (MAV_hud *) mav_malloc(sizeof(MAV_hud));
  int i;

  hud->nolines= nolines;  
  if (hud->nolines) {
    hud->line= (char **) mav_malloc(nolines*sizeof(char *));
    for (i=0; i<hud->nolines; i++) hud->line[i]= NULL;
  }
  
  hud->sp= sp;
  hud->x= x;
  hud->y= y;
  hud->w= w;
  hud->h= h;

  hud->depthfac= 1.001;
  hud->textXOffset= 0;
  hud->textYOffset= 14;
  hud->textYStep= 14;
  hud->textFont= 0;
  hud->textColour= MAV_COLOUR_BLACK;
  hud->selectable= 1;

  hud->obj= mav_objectNew(mav_class_rectangle, &hud->rect);

  hud->matrix= MAV_ID_MATRIX;

  return hud;
}



/* Routines to initialise the module */

char *mav_hudModuleID(void)
{
  return "HUD";
}

int mav_hudModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_hudModuleID);

  /* Create new object class and set callbacks */
  mav_class_hud= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_hud, mav_hudDraw);
  mav_callbackBBSet(mav_win_all, mav_class_hud, mav_hudBB);
  mav_callbackIntersectSet(mav_win_all, mav_class_hud, mav_hudIntersect);
  mav_callbackIDSet(mav_win_all, mav_class_hud, mav_hudID);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_hud, mav_hudGetUserdef);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_hud, mav_hudGetMatrix);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_hud, mav_hudGetSurfaceParams);
  mav_callbackDumpSet(mav_win_all, mav_class_hud, mav_hudDump);

  return 1;
}
