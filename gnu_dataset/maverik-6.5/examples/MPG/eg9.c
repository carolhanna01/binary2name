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

/* eg9.c */
#include "maverik.h"
#include <stdio.h>

/* The data structure to represent the AppObject */
typedef struct {
  char *name;
} AppObject;

/* Define an AppObject */
void defAppObject(AppObject *a)
{
  a->name= "I'm an object";
}

/* Draw an AppObject */
int AppObjectDraw(MAV_object *o, MAV_drawInfo *di)
{
  /* Convert from generic Maverik object to the AppObject object */
  AppObject *a= (AppObject *) mav_objectDataGet (o);

  /* No code for drawing the object -- this would normally be graphics! */
  printf("Drawing AppObject whose name is %s\n", a->name);
  
  return MAV_TRUE;
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
  MAV_class *AppObjectClass;
  AppObject app_object;
  MAV_object *obj;
  MAV_SMS *sms;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Create a new Maverik class to represent the object */
  AppObjectClass= mav_classNew();

  /* Set the draw callback for the new class */
  mav_callbackDrawSet(mav_win_all, AppObjectClass, AppObjectDraw);

  /* Define an instance of the AppObject */
  defAppObject(&app_object);
  
  /* Register the AppObject as a Maverik object */
  obj= mav_objectNew(AppObjectClass, &app_object);

  /* Create an SMS and add the object to it */
  sms= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, obj);

  /* Rendering loop */
  while (1) drawFrame(sms);
}
