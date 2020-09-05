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

void myModInit(void);



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



/* SMS data structure */

typedef struct {
  MAV_list *list;
} OurSMS;

/* Callbacks for SMS to add an object and to display */

MAV_SMSCallback *addobj, *dpyobj;

/* Wrapper to above display callback */

int mav_execSMSCallbackDpy(MAV_SMS *sms)
{
  return mav_SMSCallbackExec(dpyobj, sms, NULL, NULL, NULL, NULL);
}

/* Routine to add an object to the SMS */

int SMSAdd(MAV_SMS *sms, void *d1, void *d2, void *d3, void *d4)
{
  OurSMS *os= (OurSMS *) mav_SMSDataGet(sms);

  mav_listItemAdd(os->list, d1);

  return 1;
}

/* Routine to display an SMS */

int SMSDpy(MAV_SMS *sms, void *d1, void *d2, void *d3, void *d4)
{
  OurSMS *os= (OurSMS *) mav_SMSDataGet(sms);

  MAV_object *o;
  MAV_window *w;

  /* step through list of windows */

  mav_listPointerReset(mav_win_list);

  while (mav_listItemNext(mav_win_list, (void **) &w)) {

    /* set to this window */
    mav_windowSet(w);

    /* step through list calling display callback */

    mav_listPointerReset(os->list);
    while (mav_listItemNext(os->list, (void **) &o)) mav_callbackDrawExec(w, o);
  }

  return 1;
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

/* Alternate draw callback */

int boxdraw2(MAV_object *o)
{
  printf("in boxdraw2\n");
  return 1;
}



/* Data structure of another new object */

typedef struct {
  int colour;
  float size;
  MAV_matrix mat;
} Tri;

/* Callback routine to render new object */

int tridraw(MAV_object *o)
{
  /* Get original object data from the MAV_object */

  Tri *t= (Tri *) mav_objectDataGet(o);
  MAV_vector v;
  MAV_surfaceParams sp;

  /* Render with a solid colour */

  sp.mode= MAV_COLOUR;
  sp.colour= t->colour;
  mav_surfaceParamsUse(&sp);
  
  /* Save current transformation matrix - then multiply it by the local transformation  */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(t->mat);
  
  /* Render a 3-sided triangle of side size */

  mav_gfxPolygonBegin();
  v.x=0;
  v.y=0;
  v.z=0;
  mav_gfxVertex(v);

  v.x+=t->size;
  mav_gfxVertex(v);

  v.x-=t->size/2.0;
  v.y=t->size;
  mav_gfxVertex(v);

  v.x=0;
  v.y=0;
  mav_gfxVertex(v);
  mav_gfxPolygonEnd();

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;  
}


int main(int argc, char *argv[])
{
  MAV_class *boxclass, *triclass;
  MAV_object *o, *o2, *o3;
  MAV_window *w1, *w2, *w3;
  MAV_viewParams vp;
  Box box, box2;
  Tri tri;
  MAV_SMSClass *smsclass;
  MAV_SMS *sms;
  OurSMS os;

  /* Initialise the Maverik kernel */

  mav_initialise(&argc, argv);
  myModInit(); /* Could add to local .MavModules */

  /* Open 3 windows */

  w1= mav_windowNew(100, 100, 200, 200, "win1", NULL);
  w2= mav_windowNew(400, 400, 500, 500, "win2", NULL);
  w3= mav_windowNew(600,  50, 300, 300, "win3", NULL);

  /* Define a callback to draw objects */

  draw= mav_callbackNew();
  
  /* Define new classes of objects */

  boxclass= mav_classNew();
  triclass= mav_classNew();

  /* Set the draw callback for the classes in the windows */

  mav_callbackDrawSet(w1, boxclass, boxdraw);
  mav_callbackDrawSet(w2, boxclass, boxdraw2);
  mav_callbackDrawSet(w3, boxclass, boxdraw);

  mav_callbackDrawSet(mav_win_all, triclass, tridraw);

  /* Define instances of the new object classes */

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

  tri.size=2;
  tri.colour=2;
  tri.mat= mav_matrixSet(30,0,0, -3,-2,0);
  o3= mav_objectNew(triclass, &tri);

  /* Create a new class of SMS to manage the objects */

  smsclass= mav_SMSClassNew();

  /* Create callbacks to add an object and to display the SMS */

  addobj= mav_SMSCallbackNew();
  dpyobj= mav_SMSCallbackNew();

  /* Define these callbacks for the SMS  */

  mav_SMSCallbackSet(addobj, smsclass, SMSAdd);
  mav_SMSCallbackSet(dpyobj, smsclass, SMSDpy);

  /* Initialise the SMS */

  os.list= mav_listNew();

  /* Create an instance of the SMS */

  sms= mav_SMSNew(smsclass, &os);

  /* Call the add object callback to add the objects to the SMS */

  mav_SMSCallbackExec(addobj, sms, (void *) o, NULL, NULL, NULL);
  mav_SMSCallbackExec(addobj, sms, (void *) o2, NULL, NULL, NULL);
  mav_SMSCallbackExec(addobj, sms, (void *) o3, NULL, NULL, NULL);
	     
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

  /* Define solid colours */

  mav_paletteColourSet(mav_palette_default, 1, 0.6, 0.8, 0.5, 1.0);
  mav_paletteColourSet(mav_palette_default, 2, 0.2, 0.2, 0.8, 1.0);

  /* Main loop */

  while (1) {

    /* Check for, and act on, any events */
    mav_eventsCheck();

    /* Do what needs to be done at the start of a frame */
    mav_frameBegin();    
    
    /* Draw object in all windows */
    mav_execSMSCallbackDpy(sms);

    /* Do what needs to be done at the end of a frame */
    mav_frameEnd();
  }
}



/* New module routines */

/* Routine to poll device */

void pollDev(void)
{
  printf("polling\n");
}

/* Routine to calculate devices world position */

void calcDev(void)
{
  printf("calcing world pos\n");
}

/* Routine to check for devices events */

int eventDev(void)
{
  printf("checking and dealing with events\n");
  return 0;
}

/* Routine to identify this module */

char *myModID(void)
{
  return "myMod";
}

/* Routine to initialise the modules */

void myModInit(void)
{
  /* Add the new module */

  printf("init mymod\n");
  mav_moduleNew(myModID);

  /* Add a new device */

  mav_deviceNew(pollDev, calcDev, eventDev);
}
