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


/* Includes for Maverik kernel and graphics module */

#include "mav_kernel.h"
#include "mav_gfx.h"

#include <stdio.h>



/* New callback to render objects */

MAV_callback *draw;

/* Wrapper to above allowing for tighter prototyping */

typedef int (*drawFn)(MAV_object *);

void mav_callbackDrawSet(MAV_window *w, MAV_class *c, drawFn fn)
{
  mav_callbackSet(draw, w, c, (MAV_callbackFn) fn);
}

int mav_callbackDrawExec(MAV_window *w, MAV_object *o)
{
  return (mav_callbackExec(draw, w, o, NULL, NULL));
}



/* Data structure of new object */

typedef struct {
  int colour;
  float size;
  MAV_vector pos;
} Box;

/* Callback routine to render new object */

int boxdraw(MAV_object *o)
{
  /* Get original object data from the MAV_object */

  Box *b= (Box *) mav_objectDataGet(o);
  MAV_vector v;
  MAV_surfaceParams sp;

  /* Render with a solid colour */

  sp.mode= MAV_COLOUR;
  sp.colour= b->colour;
  mav_surfaceParamsUse(&sp);
  
  /* Render a rectangle of side size at position pos */

  mav_gfxPolygonBegin();

  v.x= -b->size+b->pos.x;
  v.y= -b->size+b->pos.y;
  v.z= b->pos.z;
  mav_gfxVertex(v);

  v.x= b->size+b->pos.x;
  mav_gfxVertex(v);

  v.y= b->size+b->pos.y;
  mav_gfxVertex(v);

  v.x= -b->size+b->pos.x;
  mav_gfxVertex(v);

  mav_gfxPolygonEnd();

  return 1;
}



int main(int argc, char *argv[])
{
  MAV_class *boxclass;
  MAV_object *o, *o2;
  MAV_window *w1, *w2;
  MAV_viewParams vp;
  Box box, box2;

  /* Initialise the Maverik kernel */

  mav_initialise(&argc, argv);

  /* Open 2 windows */

  w1= mav_windowNew(100, 100, 200, 200, "win1", NULL);
  w2= mav_windowNew(400, 400, 500, 500, "win2", NULL);

  /* Define a callback to draw objects */

  draw= mav_callbackNew();
  
  /* Define a new class of objects */

  boxclass= mav_classNew();

  /* Set the draw callback for this class in all windows */

  mav_callbackDrawSet(mav_win_all, boxclass, boxdraw);

  /* Define an instance of the new object class */

  box.size=1;
  box.colour=1;
  box.pos.x=2;
  box.pos.y=0;
  box.pos.z=0;
  o= mav_objectNew(boxclass, &box);

  /* and another */

  box2.size=2;
  box2.colour=1;
  box2.pos.x=-3;
  box2.pos.y=2;
  box2.pos.z=0;
  o2= mav_objectNew(boxclass, &box2);

  /* Define the viewing parameters */

  vp.eye.x=0;
  vp.eye.y=0;
  vp.eye.z=10;

  vp.view.x=0;
  vp.view.y=0;
  vp.view.z=-1;

  vp.up.x=0;
  vp.up.y=1;
  vp.up.z=0;
  vp.fixed_up= vp.up;
  vp.mod= NULL;

  /* Set the view parameters to all windows */

  mav_windowViewParamsSet(mav_win_all, &vp);

  /* Define a colour */

  mav_paletteColourSet(mav_palette_default, 1, 0.6, 0.8, 0.5, 1.0);

  /* Main loop */

  while (1) {

    /* Check for, and act on, any events */
    mav_eventsCheck();

    /* Do what needs to be done at the start of a frame */
    mav_frameBegin();    
    
    /* Draw object in window 1 */
    mav_windowSet(w1);
    mav_callbackDrawExec(mav_win_current, o);
    mav_callbackDrawExec(mav_win_current, o2);

    /* Draw object in window 2 */
    mav_windowSet(w2);
    mav_callbackDrawExec(mav_win_current, o);
    mav_callbackDrawExec(mav_win_current, o2);

    /* Do what needs to be done at the end of a frame */
    mav_frameEnd();
  }
}
