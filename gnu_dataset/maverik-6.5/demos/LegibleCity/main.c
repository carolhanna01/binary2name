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


#include "lcity.h"
#ifdef WIN32
#include <windows.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Classes for new object types */
MAV_class *mav_class_character;
MAV_class *mav_class_feature;
MAV_class *mav_class_vector;

/* BB of city */
MAV_BB city_bb;
MAV_SMS *city_build;
MAV_SMS *city_sms;

/* LOD parameters */
int apply_lod=1;
int apply_fog=1;
float fog_distance= 300.0;
float box_distance= 200.0;

/* Keyboard event callback */
int Keyboard_Function(MAV_object *obj, MAV_keyboardEvent *ev)
{
  int rv=0;

  if (ev->movement==MAV_PRESSED) {
    switch (ev->key) {
    case 'q': /* quit */
      exit(1);
      break;

    case 'h': /* help */
      fprintf(stderr, "\nh  this help page\n");
      fprintf(stderr, "-  decrease fog distance\n");
      fprintf(stderr, "=  increase fog distance\n");
      fprintf(stderr, "f  toggle fog\n");
      fprintf(stderr, "[  decrease box level of detail distance\n");
      fprintf(stderr, "]  increase box level of detail distance\n");
      fprintf(stderr, "l  toggle level of detail\n");
      fprintf(stderr, "left   mouse button  forward and yaw\n");
      fprintf(stderr, "middle mouse button  pitch and yaw\n");
      fprintf(stderr, "right  mouse button  right and up\n");
      fprintf(stderr, "cursor keys also navigate\n\n");
      break;

    case '-': /* decrease fog distance */
      fog_distance-=10.0;
      if (fog_distance < 1.0) fog_distance= 1.0;
      if (box_distance > fog_distance) box_distance= fog_distance;
      fprintf(stdout, "Fog distance= %f\n", fog_distance);
      mav_windowFogSet(mav_win_all, MAV_FOG_LINEAR, 1.0, fog_distance, -1,-1,-1);
      rv=1;
      break;

    case '=': /* increase fog distance */
      fog_distance+=10.0;
      fprintf(stdout, "Fog distance= %f\n", fog_distance);
      mav_windowFogSet(mav_win_all, MAV_FOG_LINEAR, 1.0, fog_distance, -1,-1,-1);
      rv=1;
      break;

    case '[': /* decrease BB distance */
      box_distance-=10.0;
      if (box_distance<0.0) box_distance= 0.0;
      fprintf(stdout, "Box distance= %f\n", box_distance);
      rv=1;
      break;
      
    case ']': /* increase BB distance */
      box_distance+=10.0;
      if (box_distance>fog_distance) fog_distance= box_distance;
      fprintf(stdout, "Box distance= %f\n", box_distance);
      rv=1;
      break;

    case 'f': /* toggle fog */
      apply_fog=!apply_fog;
      rv=1;
      
      if (apply_fog) 	
      {
	mav_windowFogSet(mav_win_all, MAV_FOG_LINEAR, 1.0, fog_distance, -1,-1,-1);
	fprintf(stderr, "enabled fog\n");
      }
      else
      {
	mav_windowFogSet(mav_win_all, MAV_FOG_NONE, 1.0, fog_distance, -1,-1,-1);
	fprintf(stderr, "disabled fog\n");
      }
      break;
      
    case 'l': /* toggle lod */
      apply_lod=!apply_lod;
      rv=1;
      break;
    }
  }

  return rv;
}



int main(int argc, char *argv[])
{
  MAV_viewParams vp;

  if (!(argc==1 || argc==2)) {
    printf("usage %s: [amsterdam|manhattan|karlsruhe]\n", argv[0]);
    exit(1);
  }

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* create character class */
  mav_class_character= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_character, mav_characterDraw);
  mav_callbackBBSet(mav_win_all, mav_class_character, mav_characterBBox);

  /* create feature class */
  mav_class_feature= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_feature, mav_featureDraw);
  mav_callbackBBSet(mav_win_all, mav_class_feature, mav_featureBBox);  

  /* create vector class */
  mav_class_vector= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_vector, mav_vectorDraw);
  mav_callbackBBSet(mav_win_all, mav_class_vector, mav_vectorBBox);  

  /* create 2 SMS's. One, an objList, to build the city, then construct
     the other, an HBB, from this (leads to a more efficient hierarchy)  */
  city_build= mav_SMSObjListNew();
  city_sms= mav_SMSHBBNew();

  fprintf(stderr, "building city (takes a minute, but it's worth it)...");
  
  /* parse input files */
  Make_Font();

  if (argc==1) 
  {
    Make_Streets("data/amsterdam.str", "data/amsterdam.lst");
  }
  else
  {
    if (!strcmp(argv[1], "amsterdam"))
    {
      Make_Streets("data/amsterdam.str", "data/amsterdam.lst");
    }
    else if (!strcmp(argv[1], "manhattan"))
    {
      Make_Streets("data/manhattan.str", "data/manhattan.lst");
    }
    else if (!strcmp(argv[1], "karlsruhe"))
    {
      Make_Streets("data/karlsruhe.str", "data/karlsruhe.lst");
    }
    else
    {
      printf("\nunknown city %s\n", argv[1]);
      printf("usage %s: [amsterdam|manhattan|karlsruhe]\n", argv[0]);
      exit(1);
    }
  }

  /* construct HBB from objList */
  mav_HBBConstructFromSMS(city_sms, city_build);

  /* delete objList (but not the objects it contains) as its no longer needed */
  mav_SMSDelete(city_build, MAV_FALSE);

  /* make SMS unselectable since we are not intersected in intersection */
  mav_SMSSelectabilitySet(city_sms, mav_win_all, MAV_FALSE);

  /* set window parameters */
  mav_windowPerspectiveSet(mav_win_all, 1.0, 10000, 50.0, ((float) mav_win_current->width)/mav_win_current->height); 
  mav_windowBackgroundColourSet(mav_win_all, 0.0, 0.0, 0.0);
  mav_windowBackfaceCullSet(mav_win_all, MAV_TRUE);

  /* set up mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_LEFT_BUTTON, mav_navigateYawFixedUp, 0.5, -0.001,
			      mav_navigateForwardsFixedUp, 0.5, 0.001);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_MIDDLE_BUTTON, mav_navigateYawFixedUp, 0.5, -0.001,
			      mav_navigatePitch, 0.5, 0.001);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_RIGHT_BUTTON, mav_navigateRightFixedUp, 0.5, 0.001,
			      mav_navigateUpFixedUp, 0.5, 0.001);

  /* set up keyboard navigation */

  mav_navigationKeyboard(mav_win_all, mav_navigationKeyboardDefault);
  mav_navigationKeyboardDefaultParams(mav_win_all, 100, 0.25, 0.001);

  /* define keyboard interaction */
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, Keyboard_Function);

  /* define lights */
  mav_paletteLightingModelSet(mav_palette_default, 0.4, 0.4, 0.4, 1.0, MAV_TRUE);
  mav_paletteLightSet(mav_palette_default, 1, 0.0, 0.0, 0.0, 0.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0);

  /* initial view point is above the middle of the city */
  vp.eye.x= 0.5*(city_bb.min.x+city_bb.max.x);
  vp.eye.y= 0.5*(city_bb.min.y+city_bb.max.y)+50.0;
  vp.eye.z= 0.5*(city_bb.min.z+city_bb.max.z);

  vp.view.x= 0.0;
  vp.view.y= 0.0;
  vp.view.z= 1.0;

  vp.up.x= 0;
  vp.up.y= 1;
  vp.up.z= 0;
  vp.fixed_up= vp.up;
  vp.mod= NULL;
  mav_windowViewParamsSet(mav_win_all, &vp);

  /* fog - a linear fade to background colour between 1.0 and fog_distance */
  mav_windowFogSet(mav_win_all, MAV_FOG_LINEAR, 1.0, fog_distance, -1,-1,-1);

  fprintf(stderr, "*** press h for controls ***\n");

  /* main loop */
  while (1) {

    /* check if we need to draw a frame */
    if (mav_eventsCheck() || mav_needFrameDraw) 
    {
      mav_frameBegin();
      mav_SMSDisplay(mav_win_all, city_sms);
      mav_frameEnd();
    }
    else
    {
      mav_sleep(0.01);
    }
  }
}

