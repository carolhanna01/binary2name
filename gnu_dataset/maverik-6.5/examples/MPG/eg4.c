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

/* eg4.c */
#include "maverik.h"
#include <stdio.h>
#include <stdlib.h>

MAV_surfaceParams *sp[4];

/* Define a rectangle */
void defRect(MAV_rectangle *r)
{
  r->width= 500.0; /* Size */
  r->height= 500.0;
  r->xtile= 3; /* Texture repeat tiling */
  r->ytile= 3;
  /* Orientation (RPY 0,-90,0) and position (XYZ 0,-2,0) */
  r->matrix= mav_matrixSet(0,-90,0, 0,-2,0); 

  /* Use decal texture with index 5 */
  r->sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, 5); 
}

/* Define a box */
void defBox(MAV_box *b)
{
  /* Random box size, position/orientation and set of surface params */
  b->size.x= mav_random()*30;
  b->size.y= mav_random()*30;
  b->size.z= mav_random()*30;
  b->matrix= mav_matrixSet(0,0,mav_random()*360, 
                           -200+mav_random()*400,0,-200+mav_random()*400);
  b->sp= sp[(int) (mav_random()*4)];
}

/* Define a cylinder */
void defCyl(MAV_cylinder *c)
{
  /* Random cylinder size, position/orientation and set of surface params */
  c->radius= mav_random()*20;
  c->height= mav_random()*20;
  c->endcap= 1;
  c->nverts= 10;
  c->matrix= mav_matrixSet(0,mav_random()*360,0, 
                           -200+mav_random()*400,0,-200+mav_random()*400);
  c->sp= sp[(int) (mav_random()*4)];
}

/* Define a composite object */
void defComp(MAV_composite *c)
{
  /* Read AC3D object from file */
  if (!mav_compositeReadAC3D("mavlogo.ac", c, MAV_ID_MATRIX)) {
    printf("failed to read mavlogo.ac\n");
    exit(1);
  }

  /* Fixed position and orientation */
  c->matrix= mav_matrixSet(0,0,0, 0,0.2,-15); 
}

/* Render a frame */
void drawFrame(MAV_SMS *sms)
{
  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();
    
  /* Display the SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms);

  /* Request end of the frame */
  mav_frameEnd();
}

int main(int argc, char *argv[])
{
  MAV_rectangle gp;
  MAV_SMS *objs;
  MAV_box box[10];
  MAV_cylinder cyl[10];
  MAV_composite comp;
  int i;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);
  
  /* Define a texture map from file, texture index 5 */
  mav_paletteTextureSet(mav_palette_default, 5, "marble_floor.ppm");

  /* Define a set of "surface parameters", i.e. the colour with */
  /* which an object is rendered */
  sp[0]= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0); /* Material index 1 */
  sp[1]= mav_surfaceParamsNew(MAV_MATERIAL, 0, 2, 0); /* Material index 2 */
  sp[2]= mav_surfaceParamsNew(MAV_MATERIAL, 0, 3, 0); /* Material index 3 */
  /* Texture 1 modulated with material 2 */
  sp[3]= mav_surfaceParamsNew(MAV_LIT_TEXTURE,  0, 2, 1); 

  /* Define a rectangle to act as the ground plane */
  defRect(&gp);

  /* Create an SMS for the objects and add the ground plane to it */
  objs= mav_SMSObjListNew();
  mav_SMSObjectAdd(objs, mav_objectNew(mav_class_rectangle, &gp));

  /* Create 10 boxes and cylinders */
  for (i=0; i<10; i++) {

    /* Define a box and a cylinder */
    defBox(&box[i]);
    defCyl(&cyl[i]);

    /* Add the box and cylinder to the objs SMS */
    mav_SMSObjectAdd(objs, mav_objectNew(mav_class_box, &box[i]));
    mav_SMSObjectAdd(objs, mav_objectNew(mav_class_cylinder, &cyl[i]));
  }

  /* Define a composite object and add it to objs SMS */  
  defComp(&comp);
  mav_SMSObjectAdd(objs, mav_objectNew(mav_class_composite, &comp));

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Rendering loop */
  while (1) drawFrame(objs);
}
